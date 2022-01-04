open Args
open Base
open Cmdliner
open Printf
open Stdio
open Types
open Types.Vbu
open Util.Console
open Util.DateTime
open Util.FileSystem
module Glob = Dune_glob.V1

let warn_missing_groups (groups : string list) =
  let* config = ask in
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
  return @@ if warning_printed then printf "\n"

let cleanup_backups backup_path verbose =
  let* config = ask in
  whenm (config.num_to_keep > 0) (fun () ->
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

      whenm
        (Array.length files > config.num_to_keep)
        (fun () ->
          let sorted_files =
            files
            |> Array.to_list
            |> List.sort ~compare:(fun f1 f2 ->
                   Float.compare (Unix.lstat f2).st_mtime
                     (Unix.lstat f1).st_mtime)
          in

          let files_to_delete = List.drop sorted_files config.num_to_keep in

          files_to_delete
          |> List.iter ~f:(fun file ->
                 note verbose (sprintf "Deleting %s" file);
                 Unix.unlink file)
          |> return))

let rec backup_file group base_path glob from_path to_path (verbose : bool) =
  try
    let glob_matches () =
      Glob.test (Glob.of_string glob) (basename from_path)
    in

    let copy_and_cleanup () =
      mkdir_p (dirname to_path) 0o775;
      printf "%s ==>\n\t%s\n" from_path to_path;
      Out_channel.flush stdout;
      file_copy from_path to_path;
      let* () = cleanup_backups to_path verbose in
      return (1, [])
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

        return (0, []))
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
            else return (0, [])
        | None -> copy_and_cleanup ()
    in

    if dir_exists from_path then
      backup_files group base_path glob from_path to_path verbose
    else if glob_matches () then backup_file' ()
    else return (0, [])
  with e ->
    let warning =
      sprintf "Unable to backup file %s for group %s:\n%s\n" to_path group
        (Exn.to_string e)
    in
    warn warning;
    return (1, [ warning ])

and backup_files group base_path glob from_path to_path verbose =
  readdir from_path
  |> Array.to_list
  |> fold_list
       ~f:(fun (c, es) path ->
         let file = basename path in

         let* new_count, new_errs =
           backup_file group base_path glob (from_path ^/ file)
             (to_path ^/ file) verbose
         in

         return (c + new_count, es @ new_errs))
       ~init:(0, [])

let backup_group group_name verbose =
  let* config = ask in
  let start_time = Unix.gettimeofday () in

  let group =
    List.find ~f:String.(fun g -> g.name = group_name) config.groups
  in

  match group with
  | Some group ->
      if dir_exists group.path then (
        let* backed_up_count, warnings =
          backup_files group.name group.path group.glob group.path
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
        return warnings)
      else (
        warn (sprintf "Path set for %s doesn't exist: %s" group_name group.path);
        return [])
  | None ->
      let* () = warn_missing_groups [ group_name ] in
      return []

let rec backup (group_names : string list) (loop : bool) (verbose : bool) =
  let* config = ask in
  let group_names =
    match group_names with
    | [] -> List.map ~f:(fun g -> g.name) config.groups
    | gns -> gns
  in

  let* warnings =
    fold_list group_names
      ~f:(fun acc group ->
        try
          let* warnings = backup_group group verbose in
          return (acc @ warnings)
        with e ->
          err (sprintf "Error backing up %s: %s" group (Exn.to_string e));
          return acc)
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
    Out_channel.flush stdout;
    Unix.sleep (config.frequency * 60);
    backup group_names loop verbose)
  else return None

let valid_group_name_chars : (char, _) Set.t =
  List.filter Char.all ~f:Char.is_alphanum @ [ '-'; '_' ]
  |> Set.of_list (module Char)

let is_valid_group_name =
  String.for_all ~f:(fun c ->
      Set.exists ~f:Char.(( = ) c) valid_group_name_chars)

let add (group : string) (path : string) (glob : string) =
  let* config = ask in
  if List.exists ~f:String.(fun g -> g.name = group) config.groups then (
    err (sprintf "Group with the name %s already exists" group);

    return None)
  else if not (is_valid_group_name group) then (
    err
      (sprintf
         "Invalid characters in name `%s': only alphanumeric characters, \
          underscores, and hyphens are allowed"
         group);
    return None)
  else
    let new_glob =
      match glob with "none" | "" -> default_glob | glob -> glob
    in
    let new_group = { name = group; path = realpath path; glob = new_glob } in

    let new_groups =
      config.groups @ [ new_group ]
      |> List.sort ~compare:(fun g1 g2 -> String.compare g1.name g2.name)
    in

    printf "Group added successfully:\n\n";
    Group.print new_group;
    return @@ Some { config with groups = new_groups }

let list =
  let* config = ask in
  List.iter ~f:(fun g -> printf "%s\n" g.name) config.groups;
  return None

let print_info (group_names : string list) =
  let* config = ask in
  let* () =
    whenm
      (List.length group_names > 0)
      (fun () -> warn_missing_groups group_names)
  in

  let groups =
    match group_names with
    | [] -> config.groups
    | gs ->
        List.filter
          ~f:(fun g -> List.exists ~f:String.(( = ) g.name) gs)
          config.groups
  in

  List.iter ~f:Group.print groups;
  return None

let remove (groups : string list) (yes : bool) =
  let* config = ask in
  let* () = warn_missing_groups groups in

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

  return @@ Some { config with groups = new_groups }

let edit
    (group_name : string)
    (name : string option)
    (path : string option)
    (glob : string option) =
  let* config = ask in
  match (name, path, glob) with
  | None, None, None ->
      err "One or more of --name, --path, or --glob must be provided.";
      return None
  | _ -> (
      let split_list =
        List.findi ~f:String.(fun _ g -> g.name = group_name) config.groups
        |> Option.map ~f:(fun (i, _) -> List.split_n config.groups i)
      in

      match split_list with
      | None ->
          let* () = warn_missing_groups [ group_name ] in
          return None
      | Some (_, []) ->
          err "Couldn't find group in list";
          return None
      | Some (front, group :: back) ->
          let new_name = Option.value ~default:group.name name in

          let new_glob =
            match glob with
            | Some "none" | Some "" -> Some default_glob
            | glob -> glob
          in

          let new_path = Option.value ~default:group.path path |> realpath in

          let edited_group =
            { name = new_name
            ; path = new_path
            ; glob = new_glob |> Option.value ~default:group.glob
            }
          in

          if not (is_valid_group_name new_name) then (
            err
              (sprintf
                 "Invalid characters in name `%s': only alphanumeric \
                  characters, underscores, and hyphens are allowed"
                 new_name);

            return None)
          else (
            Group.print group ~new_name ~new_path ?new_glob;
            let backup_dir_exists = dir_exists (config.path ^/ group_name) in
            if Option.is_some name && backup_dir_exists then (
              warn "Group name changed, renaming backup directory...";
              Unix.rename (config.path ^/ group_name) (config.path ^/ new_name));

            return
            @@ Some { config with groups = front @ [ edited_group ] @ back }))

let edit_config
    (backup_dir : string option)
    (backup_freq : int option)
    (backups_to_keep : int option) =
  let* config = ask in
  let new_backup_dir =
    Option.map ~f:realpath backup_dir |> Option.value ~default:config.path
  in

  let new_backup_freq = Option.value ~default:config.frequency backup_freq in

  let new_backups_to_keep =
    Option.value ~default:config.num_to_keep backups_to_keep
  in

  Config.print config ~new_backup_dir ~new_backup_freq ~new_backups_to_keep;

  match (backup_dir, backup_freq, backups_to_keep) with
  | None, None, None -> return None
  | _ ->
      return
      @@ Some
           { config with
             path = new_backup_dir
           ; frequency = new_backup_freq
           ; num_to_keep = new_backups_to_keep
           }

let load_config_t = Term.(const Config.load $ config_path_t)

let backup_t =
  Term.(
    const run_reader
    $ (const backup $ BackupCmd.groups $ BackupCmd.loop $ BackupCmd.verbose)
    $ load_config_t)

let add_t =
  Term.(
    const run_reader
    $ (const add $ AddCmd.group $ AddCmd.path $ AddCmd.glob)
    $ load_config_t)

let list_t = Term.(const run_reader $ const list $ load_config_t)

let info_t =
  Term.(const run_reader $ (const print_info $ InfoCmd.groups) $ load_config_t)

let remove_t =
  Term.(
    const run_reader
    $ (const remove $ RemoveCmd.groups $ RemoveCmd.yes)
    $ load_config_t)

let edit_t =
  Term.(
    const run_reader
    $ (const edit $ EditCmd.group $ EditCmd.name $ EditCmd.path $ EditCmd.glob)
    $ load_config_t)

let config_t =
  Term.(
    const run_reader
    $ (const edit_config $ ConfigCmd.path $ ConfigCmd.frequency $ ConfigCmd.keep)
    $ load_config_t)

let vbu_info = Term.info "vbu" ~version:"v1.3.1"
let vbu_t = Term.(ret (const (Fn.const (`Help (`Pager, None))) $ const 0))

let () =
  let config_path =
    Term.eval_peek_opts config_path_t
    |> fst
    |> Option.value ~default:default_config_path
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
  | `Ok (Some new_config) -> Config.save new_config config_path
  | r -> Term.exit r
