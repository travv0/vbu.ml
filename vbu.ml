open Args
open Base
open Cmdliner
open Printf
open Stdio
open Types
open Util
module Glob = Dune_glob.V1

let note verbose s =
  if verbose then ANSITerminal.(printf [ blue ] "Note: %s\n" s)

let warn s = ANSITerminal.(printf [ yellow ] "Warning: %s\n" s)
let err s = ANSITerminal.(printf [ red ] "Error: %s\n" s)

let warn_missing_groups config (groups : string list) =
  let warning_printed =
    List.fold
      ~f:(fun warning_printed group ->
        if not (List.exists ~f:String.(fun g -> g.name = group) config.groups)
        then (
          warn (sprintf "No group named `%s'" group);
          true)
        else warning_printed)
      ~init:false groups
  in

  if warning_printed then printf "\n"

let print_config_row label value new_value =
  printf "%s: %s%s\n" label value
  @@
  match new_value with
  | Some nv when String.equal value nv -> ""
  | Some nv -> sprintf " -> %s" nv
  | None -> ""

let print_group group new_name new_path new_glob =
  print_config_row "Name" group.name new_name;
  print_config_row "Path" group.path new_path;

  (match (group.glob, new_glob) with
  | _, Some new_glob ->
      print_config_row "Glob"
        (Option.value ~default:"" group.glob)
        (Some (Option.value ~default:"" new_glob))
  | Some _, None ->
      print_config_row "Glob" (Option.value ~default:"" group.glob) None
  | _ -> ());

  printf "\n"

let buffer_size = 8192
let buffer = Bytes.create buffer_size

let file_copy input_name output_name =
  let fd_in = Unix.(openfile input_name [ O_RDONLY ] 0) in
  let fd_out =
    Unix.(openfile output_name [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666)
  in
  let rec copy_loop () =
    match Unix.read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r ->
        Unix.write fd_out buffer 0 r |> ignore;
        copy_loop ()
  in
  copy_loop ();
  Unix.close fd_in;
  Unix.close fd_out;

  let stats = Unix.lstat input_name in
  let atime = stats.st_atime in
  let mtime = stats.st_mtime in
  Unix.utimes output_name atime mtime

let dir_exists path_name = file_exists path_name && is_directory path_name

let rec mkdir_p path perms =
  try Unix.mkdir path perms with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdir_p (dirname path) perms;
      Unix.mkdir path perms

let cleanup_backups config backup_path verbose =
  if config.num_to_keep > 0 then
    let glob = backup_path ^ ".bak.????_??_??_??_??_??" |> Glob.of_string in

    let all_files =
      readdir (dirname backup_path)
      |> Array.map ~f:(fun f -> dirname backup_path ^/ f)
    in

    let files =
      all_files
      |> Array.filter ~f:(Glob.test glob)
      |> Array.append [| backup_path |]
    in

    if Array.length files > config.num_to_keep then
      let sorted_files =
        files
        |> Array.to_list
        |> List.sort ~compare:(fun f1 f2 ->
               Float.compare (Unix.lstat f2).st_mtime (Unix.lstat f1).st_mtime)
      in

      let files_to_delete = List.drop sorted_files config.num_to_keep in

      files_to_delete
      |> List.iter ~f:(fun file ->
             note verbose (sprintf "Deleting %s" file);
             Unix.unlink file)

let format_filename_time t =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    Unix.gmtime t
  in
  sprintf "%04d_%02d_%02d_%02d_%02d_%02d" (tm_year + 1900) (tm_mon + 1) tm_mday
    tm_hour tm_min tm_sec

let format_month = function
  | 0 -> "January"
  | 1 -> "February"
  | 2 -> "March"
  | 3 -> "April"
  | 4 -> "May"
  | 5 -> "June"
  | 6 -> "July"
  | 7 -> "August"
  | 8 -> "September"
  | 9 -> "October"
  | 10 -> "November"
  | 11 -> "December"
  | m -> failwithf "format_month: bad month %d" m ()

let format_weekday = function
  | 0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | m -> failwithf "format_weekday: bad weekday %d" m ()

let format_date t =
  let { Unix.tm_year; tm_mon; tm_mday; tm_wday; _ } = Unix.localtime t in
  let month = format_month tm_mon in
  let weekday = format_weekday tm_wday in
  let year = tm_year + 1900 in
  sprintf "%s, %s %d, %d" weekday month tm_mday year

let format_time t =
  let { Unix.tm_hour; tm_min; tm_sec; _ } = Unix.localtime t in
  let hours =
    let hour = tm_hour % 12 in
    if hour = 0 then 12 else hour
  in
  let suffix = if tm_hour > 11 then "PM" else "AM" in
  sprintf "%d:%02d:%02d %s" hours tm_min tm_sec suffix

let rec backup_file
    config
    group
    base_path
    glob
    from_path
    to_path
    (verbose : bool) =
  try
    let glob_matches () =
      let glob = Option.value ~default:default_glob glob |> Glob.of_string in
      Glob.test glob (basename from_path)
    in

    let copy_and_cleanup () =
      mkdir_p (dirname to_path) 0o775;
      printf "%s ==>\n\t%s\n" from_path to_path;
      file_copy from_path to_path;
      cleanup_backups config to_path verbose;
      (1, [])
    in

    let backup_file' () =
      let from_info = Unix.lstat from_path in

      let from_is_link =
        match from_info.st_kind with Unix.S_LNK -> true | _ -> false
      in

      if from_is_link then (
        note verbose
          (sprintf
             "%s appears to be a link to somewhere else in the filesystem. \
              Skipping..."
             from_path);

        (0, []))
      else
        let from_mod_time = from_info.st_mtime in

        let to_mod_time =
          if file_exists to_path then Some (Unix.lstat to_path).st_mtime
          else None
        in

        match to_mod_time with
        | Some to_mod_time ->
            if Int.of_float from_mod_time <> Int.of_float to_mod_time then (
              Unix.rename to_path
                (to_path ^ ".bak." ^ format_filename_time to_mod_time);
              copy_and_cleanup ())
            else (0, [])
        | None -> copy_and_cleanup ()
    in

    if dir_exists from_path then
      backup_files config group base_path glob from_path to_path verbose
    else if glob_matches () then backup_file' ()
    else (0, [])
  with e ->
    let warning =
      sprintf "Unable to backup file %s for group %s:\n%s\n" to_path group
        (Exn.to_string e)
    in
    warn warning;
    (1, [ warning ])

and backup_files config group base_path glob from_path to_path verbose :
    int * string list =
  readdir from_path
  |> Array.fold
       ~f:(fun (c, es) path ->
         let file = basename path in

         let new_count, new_errs =
           backup_file config group base_path glob (from_path ^/ file)
             (to_path ^/ file) verbose
         in

         (c + new_count, es @ new_errs))
       ~init:(0, [])

let backup_group config group_name verbose =
  let start_time = Unix.gettimeofday () in

  let group =
    List.find ~f:String.(fun g -> g.name = group_name) config.groups
  in

  match group with
  | Some group ->
      if dir_exists group.path then (
        let backed_up_count, warnings =
          backup_files config group.name group.path group.glob group.path
            (config.path ^/ group_name)
            verbose
        in

        (if backed_up_count > 0 then
         let now = Unix.gettimeofday () in
         let warning_count = List.length warnings in

         printf
           "\nFinished backing up %d file%s%s for %s in %fs on %s at %s\n\n"
           backed_up_count
           (if backed_up_count = 1 then "" else "s")
           (if warning_count > 0 then
            sprintf " with %d warning%s" warning_count
              (if warning_count = 1 then "" else "s")
           else "")
           group_name
           Float.(now - start_time)
           (format_date now) (format_time now));
        warnings)
      else (
        warn (sprintf "Path set for %s doesn't exist: %s" group_name group.path);
        [])
  | None ->
      warn_missing_groups config [ group_name ];
      []

let rec backup config (group_names : string list) (loop : bool) (verbose : bool)
    =
  let group_names =
    match group_names with
    | [] -> List.map ~f:(fun g -> g.name) config.groups
    | gns -> gns
  in

  let warnings =
    group_names
    |> List.fold
         ~f:(fun acc group ->
           try
             let warnings = backup_group config group verbose in
             acc @ warnings
           with e ->
             err (sprintf "Error backing up %s: %s" group (Exn.to_string e));
             acc)
         ~init:[]
  in

  let warning_count = List.length warnings in

  if warning_count > 0 then (
    ANSITerminal.(
      printf [ yellow ] "\n%d warning%s occurred:" warning_count
        (if warning_count = 1 then "" else "s");

      if verbose then (
        printf [ yellow ] "\n\n";
        List.iter ~f:(printf [ yellow ] "%s\n") warnings)
      else
        printf [ yellow ]
          "\n\
           Pass --verbose flag to print all warnings after backup completes\n\n"));

  if loop then (
    Unix.sleep (config.frequency * 60);
    backup config group_names loop verbose)
  else None

let valid_group_name_chars : (char, _) Set.t =
  List.filter Char.all ~f:Char.is_alphanum @ [ '-'; '_' ]
  |> Set.of_list (module Char)

let is_valid_group_name =
  String.for_all ~f:(fun c ->
      Set.exists ~f:Char.(( = ) c) valid_group_name_chars)

let add config (group : string) (path : string) (glob : string option) :
    config option =
  if List.exists ~f:String.(fun g -> g.name = group) config.groups then (
    err (sprintf "Group with the name %s already exists" group);

    None)
  else if not (is_valid_group_name group) then (
    err
      (sprintf
         "Invalid characters in name `%s': only alphanumeric characters, \
          underscores, and hyphens are allowed"
         group);
    None)
  else
    let new_group = { name = group; path = realpath path; glob } in

    let new_groups =
      config.groups @ [ new_group ]
      |> List.sort ~compare:(fun g1 g2 -> String.compare g1.name g2.name)
    in

    printf "Group added successfully:\n\n";
    print_group new_group None None None;
    Some { config with groups = new_groups }

let list config =
  List.iter ~f:(fun g -> printf "%s\n" g.name) config.groups;
  None

let print_info config (group_names : string list) =
  if List.length group_names > 0 then warn_missing_groups config group_names;

  let groups =
    match group_names with
    | [] -> config.groups
    | gs ->
        List.filter
          ~f:(fun g -> List.exists ~f:String.(( = ) g.name) gs)
          config.groups
  in

  List.iter ~f:(fun g -> print_group g None None None) groups;
  None

let rec prompt_y_or_n prompt =
  printf "\n%s (y/N) " prompt;
  Out_channel.flush stdout;

  match
    In_channel.input_line ~fix_win_eol:true stdin |> Option.map ~f:String.strip
  with
  | Some "y" | Some "Y" -> true
  | Some "n" | Some "N" | Some "" | None -> false
  | Some s ->
      printf "Invalid input: entered `%s'.\n" s;
      prompt_y_or_n prompt

let remove config (groups : string list) (yes : bool) =
  warn_missing_groups config groups;

  let new_groups =
    List.filter_map config.groups ~f:(fun group ->
        if
          List.exists ~f:String.(( = ) group.name) groups
          && (yes
             || prompt_y_or_n
                  ("Are you sure you want to remove " ^ group.name ^ "?"))
        then (
          printf "Removed %s\n" group.name;
          None)
        else Some group)
  in

  Some { config with groups = new_groups }

let edit
    config
    (group_name : string)
    (name : string option)
    (path : string option)
    (glob : string option) =
  match (name, path, glob) with
  | None, None, None ->
      err "One or more of --name, --path, or --glob must be provided.";
      None
  | _ -> (
      let split_list =
        List.findi ~f:String.(fun _ g -> g.name = group_name) config.groups
        |> Option.map ~f:(fun (i, _) -> List.split_n config.groups i)
      in

      match split_list with
      | None ->
          warn_missing_groups config [ group_name ];
          None
      | Some (_, []) ->
          err "Couldn't find group in list";
          None
      | Some (front, group :: back) ->
          let new_name = Option.value ~default:group.name name in

          let new_glob =
            match glob with
            | Some "none" | Some "" -> Some None
            | None -> None
            | glob -> Some glob
          in

          let new_path = Option.value ~default:group.path path |> realpath in

          let edited_group =
            { name = new_name
            ; path = new_path
            ; glob = Option.value ~default:None new_glob
            }
          in

          if not (is_valid_group_name new_name) then (
            err
              (sprintf
                 "Invalid characters in name `%s': only alphanumeric \
                  characters, underscores, and hyphens are allowed"
                 new_name);

            None)
          else (
            print_group group (Some new_name) (Some new_path) new_glob;
            let backup_dir_exists = dir_exists (config.path ^/ group_name) in
            if Option.is_some name && backup_dir_exists then (
              warn "Group name changed, renaming backup directory...";
              Unix.rename (config.path ^/ group_name) (config.path ^/ new_name));

            Some { config with groups = front @ [ edited_group ] @ back }))

let print_config config new_backup_dir new_backup_freq new_backups_to_keep =
  print_config_row "Backup path" config.path new_backup_dir;
  print_config_row "Backup frequency (in minutes)"
    (Int.to_string config.frequency)
    (Option.map ~f:Int.to_string new_backup_freq);
  print_config_row "Number of backups to keep"
    (Int.to_string config.num_to_keep)
    (Option.map ~f:Int.to_string new_backups_to_keep);
  printf "\n"

let edit_config
    config
    (backup_dir : string option)
    (backup_freq : int option)
    (backups_to_keep : int option) =
  let new_backup_dir =
    Option.value ~default:config.path backup_dir |> realpath
  in

  let new_backup_freq = Option.value ~default:config.frequency backup_freq in

  let new_backups_to_keep =
    Option.value ~default:config.num_to_keep backups_to_keep
  in

  print_config config (Some new_backup_dir) (Some new_backup_freq)
    (Some new_backups_to_keep);

  match (backup_dir, backup_freq, backups_to_keep) with
  | None, None, None -> None
  | _ ->
      Some
        { config with
          path = new_backup_dir
        ; frequency = new_backup_freq
        ; num_to_keep = new_backups_to_keep
        }

let default_config =
  { path = Util.home_dir ^/ ".vbu-backups"
  ; frequency = 15
  ; num_to_keep = 20
  ; groups = []
  }

let save_default_config path =
  printf
    "Creating new config file at `%s'.\n\
     Use the `config' command to update default values, which are:\n\n\
     Backup path: %s\n\
     Backup frequency (in minutes): %d\n\
     Number of backups to keep: %d\n\n"
    path default_config.path default_config.frequency default_config.num_to_keep;

  let dir = dirname path in
  mkdir_p dir 0o775;
  Yojson.to_file path (Config.to_json default_config)

let load_config config_path =
  if not (file_exists config_path) then save_default_config config_path;

  try Yojson.Basic.from_file config_path |> Config.of_json
  with e ->
    warn
      (sprintf
         "Couldn't load config: %s\n\
          Attempting to save default config to '%s' after backing up existing \
          config.\n"
         (Exn.to_string e) config_path);

    if file_exists config_path then file_copy config_path (config_path ^ ".bak");

    save_default_config config_path;
    default_config

let load_config_t = Term.(const load_config $ Util.config_path)

let backup_t =
  Term.(
    const backup
    $ load_config_t
    $ BackupCmd.groups
    $ BackupCmd.loop
    $ BackupCmd.verbose)

let add_t =
  Term.(const add $ load_config_t $ AddCmd.group $ AddCmd.path $ AddCmd.glob)

let list_t = Term.(const list $ load_config_t)
let info_t = Term.(const print_info $ load_config_t $ InfoCmd.groups)

let remove_t =
  Term.(const remove $ load_config_t $ RemoveCmd.groups $ RemoveCmd.yes)

let edit_t =
  Term.(
    const edit
    $ load_config_t
    $ EditCmd.group
    $ EditCmd.name
    $ EditCmd.path
    $ EditCmd.glob)

let config_t =
  Term.(
    const edit_config
    $ load_config_t
    $ ConfigCmd.path
    $ ConfigCmd.frequency
    $ ConfigCmd.keep)

let vbu_info = Term.info "vbu" ~version:"v1.0.0"
let vbu_t = Term.(ret (const (Fn.const (`Help (`Pager, None))) $ const 0))

let () =
  let config_path =
    Term.eval_peek_opts Util.config_path
    |> fst
    |> Option.value ~default:Util.default_config_path
  in
  let result =
    Term.eval_choice (vbu_t, vbu_info)
      [ (backup_t, BackupCmd.info)
      ; (add_t, AddCmd.info)
      ; (list_t, ListCmd.info)
      ; (info_t, InfoCmd.info)
      ; (remove_t, RemoveCmd.info)
      ; (edit_t, EditCmd.info)
      ; (config_t, ConfigCmd.info)
      ]
  in
  match result with
  | `Ok (Some new_config) ->
      mkdir_p (dirname config_path) 0o775;

      Yojson.Basic.to_file config_path (Config.to_json new_config)
  | r -> Term.exit r
