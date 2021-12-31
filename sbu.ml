open Args
open Base
open Cmdliner
open Printf
open Stdio
open Types
module Glob = Dune_glob.V1

let dirname = Caml.Filename.dirname
let basename = Caml.Filename.basename
let ( ^/ ) = Caml.Filename.concat
let file_exists = Caml.Sys.file_exists
let is_directory = Caml.Sys.is_directory
let readdir = Caml.Sys.readdir
let defaultGlob = "*/*"
let note s = ANSITerminal.(printf [ blue ] "Note: %s\n" s)
let warn s = ANSITerminal.(printf [ yellow ] "Warning: %s\n" s)
let err s = ANSITerminal.(printf [ red ] "Error: %s\n" s)

let warnMissingGames config (games : string list) =
  let warningPrinted =
    List.fold
      ~f:(fun warningPrinted game ->
        if not (List.exists ~f:String.(fun g -> g.name = game) config.games)
        then (
          warn (sprintf "No game named `%s'" game);
          true)
        else warningPrinted)
      ~init:false games
  in

  if warningPrinted then printf "\n"

let printConfigRow label value newValue =
  printf "%s: %s%s\n" label value
  @@
  match newValue with
  | Some nv when String.equal value nv -> ""
  | Some nv -> sprintf " -> %s" nv
  | None -> ""

let printGame game newName newPath newGlob =
  printConfigRow "Name" game.name newName;
  printConfigRow "Save path" game.path newPath;

  (match (game.glob, newGlob) with
  | _, Some newGlob ->
      printConfigRow "Save glob"
        (Option.value ~default:"" game.glob)
        (Some (Option.value ~default:"" newGlob))
  | Some _, None ->
      printConfigRow "Save glob" (Option.value ~default:"" game.glob) None
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
  Unix.close fd_out

let dir_exists path_name = file_exists path_name && is_directory path_name

let rec mkdir_p path perms =
  try Unix.mkdir path perms with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      mkdir_p (dirname path) perms;
      Unix.mkdir path perms

let cleanupBackups config backupPath verbose =
  if config.numToKeep > 0 then
    let glob =
      backupPath
      ^ ".bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
      |> Glob.of_string
    in

    let allFiles = readdir (dirname backupPath) in

    let files =
      allFiles
      |> Array.filter ~f:(Glob.test glob)
      |> Array.append [| backupPath |]
    in

    if Array.length files > config.numToKeep then
      let sortedFiles =
        files
        |> Array.to_list
        |> List.sort ~compare:(fun f1 f2 ->
               Float.compare (Unix.lstat f2).st_mtime (Unix.lstat f1).st_mtime)
      in

      let filesToDelete = List.drop sortedFiles config.numToKeep in

      filesToDelete
      |> List.iter ~f:(fun file ->
             if verbose then note (sprintf "Deleting %s" file);
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
  let hours = if tm_hour = 0 then 12 else tm_hour % 12 in
  let suffix = if tm_hour > 11 then "PM" else "AM" in
  sprintf "%d:%02d:%02d %s" hours tm_min tm_sec suffix

let rec backupFile config game basePath glob fromPath toPath (verbose : bool) =
  try
    let globMatches () =
      let glob =
        basePath ^/ Option.value ~default:defaultGlob glob |> Glob.of_string
      in

      Glob.test glob fromPath
    in

    let copyAndCleanup () =
      mkdir_p (dirname toPath) 0o775;
      printf "%s ==>\n\t%s\n" fromPath toPath;
      file_copy fromPath toPath;
      cleanupBackups config toPath verbose;
      (1, [])
    in

    let backupFile' () =
      let fromInfo = Unix.lstat fromPath in

      let fromIsSymLink =
        match fromInfo.st_kind with Unix.S_LNK -> true | _ -> false
      in

      if fromIsSymLink then (
        if verbose then
          note
            (sprintf
               "%s appears to be a link to somewhere else in the filesystem. \
                Skipping..."
               fromPath);

        (0, []))
      else
        let fromModTime = fromInfo.st_mtime in

        let toModTime =
          if file_exists toPath then Some (Unix.lstat toPath).st_mtime else None
        in

        match toModTime with
        | Some toModTime ->
            if Float.(fromModTime <> toModTime) then (
              Unix.rename toPath
                (toPath ^ ".bak." ^ format_filename_time toModTime);
              copyAndCleanup ())
            else (0, [])
        | None -> copyAndCleanup ()
    in

    if dir_exists fromPath then
      backupFiles config game basePath glob fromPath toPath verbose
    else if globMatches () then backupFile' ()
    else (0, [])
  with e ->
    let warning =
      sprintf "Unable to backup file %s for game %s:\n%s\n" toPath game
        (Exn.to_string e)
    in
    warn warning;
    (1, [ warning ])

and backupFiles config game basePath glob fromPath toPath verbose :
    int * string list =
  readdir fromPath
  |> Array.fold
       ~f:(fun (c, es) path ->
         let file = basename path in

         let newCount, newErrs =
           backupFile config game basePath glob (fromPath ^/ file)
             (toPath ^/ file) verbose
         in

         (c + newCount, es @ newErrs))
       ~init:(0, [])

let backupGame config gameName verbose =
  let startTime = Unix.gettimeofday () in

  let game = List.find ~f:String.(fun g -> g.name = gameName) config.games in

  match game with
  | Some game ->
      if dir_exists game.path then (
        let backedUpCount, warnings =
          backupFiles config game.name game.path game.glob game.path
            (config.path ^/ gameName) verbose
        in

        (if backedUpCount > 0 then
         let now = Unix.gettimeofday () in
         let warningCount = List.length warnings in

         printf
           "\nFinished backing up %d file%s%s for %s in %fs on %s at %s\n\n"
           backedUpCount
           (if backedUpCount = 1 then "" else "s")
           (if warningCount > 0 then
            sprintf " with %d warning%s" warningCount
              (if warningCount = 1 then "" else "s")
           else "")
           gameName
           Float.(now - startTime)
           (format_date now) (format_time now));
        warnings)
      else (
        warn (sprintf "Path set for %s doesn't exist: %s" gameName game.path);
        [])
  | None ->
      warnMissingGames config [ gameName ];
      []

let rec backup config (gameNames : string list) (loop : bool) (verbose : bool) =
  let gameNames' =
    match gameNames with
    | [] -> List.map ~f:(fun g -> g.name) config.games
    | gns -> gns
  in

  let warnings =
    gameNames'
    |> List.fold
         ~f:(fun acc game ->
           try
             let warnings = backupGame config game verbose in
             acc @ warnings
           with e ->
             err (sprintf "Error backing up %s: %s" game (Exn.to_string e));
             acc)
         ~init:[]
  in

  let warningCount = List.length warnings in

  if warningCount > 0 then (
    ANSITerminal.(
      printf [ yellow ] "\n%d warning%s occurred:" warningCount
        (if warningCount = 1 then "" else "s");

      if verbose then (
        printf [ yellow ] "\n\n";
        List.iter ~f:(printf [ yellow ] "%s\n") warnings)
      else
        printf [ yellow ]
          "\n\
           Pass --verbose flag to print all warnings after backup completes\n\n"));

  if loop then (
    Unix.sleep (config.frequency * 60);
    backup config gameNames loop verbose)
  else None

let validGameNameChars : (char, _) Set.t =
  List.filter Char.all ~f:Char.is_alphanum @ [ '-'; '_' ]
  |> Set.of_list (module Char)

let isValidGameName =
  String.for_all ~f:(fun c -> Set.exists ~f:Char.(( = ) c) validGameNameChars)

let realpath path = try Unix.realpath path with _ -> path

let add config (game : string) (path : string) (glob : string option) :
    config option =
  if List.exists ~f:String.(fun g -> g.name = game) config.games then (
    err (sprintf "Game with the name %s already exists" game);

    None)
  else if not (isValidGameName game) then (
    err
      (sprintf
         "Invalid characters in name `%s': only alphanumeric characters, \
          underscores, and hyphens are allowed"
         game);
    None)
  else
    let newGame = { name = game; path = realpath path; glob } in

    let newGames =
      config.games @ [ newGame ]
      |> List.sort ~compare:(fun g1 g2 -> String.compare g1.name g2.name)
    in

    printf "Game added successfully:\n\n";
    printGame newGame None None None;
    Some { config with games = newGames }

let list config =
  List.iter ~f:(fun g -> printf "%s\n" g.name) config.games;
  None

let print_info config (gameNames : string list) =
  if List.length gameNames > 0 then warnMissingGames config gameNames;

  let games =
    match gameNames with
    | [] -> config.games
    | gs ->
        List.filter
          ~f:(fun g -> List.exists ~f:String.(( = ) g.name) gs)
          config.games
  in

  List.iter ~f:(fun g -> printGame g None None None) games;
  None

let rec promptYorN prompt =
  printf "\n%s (y/N) " prompt;
  Out_channel.flush stdout;

  match
    In_channel.input_line ~fix_win_eol:true stdin |> Option.map ~f:String.strip
  with
  | Some "y" | Some "Y" -> true
  | Some "n" | Some "N" | Some "" | None -> false
  | Some s ->
      printf "Invalid input: entered `%s'.\n" s;
      promptYorN prompt

let remove config (games : string list) (yes : bool) =
  warnMissingGames config games;

  let newGames =
    List.filter_map config.games ~f:(fun game ->
        if
          List.exists ~f:String.(( = ) game.name) games
          && (yes
             || promptYorN ("Are you sure you want to remove " ^ game.name ^ "?")
             )
        then (
          printf "Removed %s\n" game.name;
          None)
        else Some game)
  in

  Some { config with games = newGames }

let edit
    config
    (gameName : string)
    (newName : string option)
    (newPath : string option)
    (newGlob : string option) =
  match (newName, newPath, newGlob) with
  | None, None, None ->
      err "One or more of --name, --path, or --glob must be provided.";
      None
  | _ -> (
      let splitList =
        List.findi ~f:String.(fun _ g -> g.name = gameName) config.games
        |> Option.map ~f:(fun (i, _) -> List.split_n config.games i)
      in

      match splitList with
      | None ->
          warnMissingGames config [ gameName ];
          None
      | Some (_, []) ->
          err "Couldn't find game in list";
          None
      | Some (front, game :: back) ->
          let newName' = Option.value ~default:game.name newName in

          let newGlob' =
            match newGlob with
            | Some "none" | Some "" -> Some None
            | None -> None
            | glob -> Some glob
          in

          let newPath' = Option.value ~default:game.path newPath |> realpath in

          let editedGame =
            { name = newName'
            ; path = newPath'
            ; glob = Option.value ~default:None newGlob'
            }
          in

          if not (isValidGameName newName') then (
            err
              (sprintf
                 "Invalid characters in name `%s': only alphanumeric \
                  characters, underscores, and hyphens are allowed"
                 newName');

            None)
          else (
            printGame game (Some newName') (Some newPath') newGlob';

            let backupDirExists = dir_exists (config.path ^/ gameName) in

            if Option.is_some newName && backupDirExists then (
              note "Game name changed, renaming backup directory...";

              Unix.rename (config.path ^/ gameName) (config.path ^/ newName'));

            Some { config with games = front @ [ editedGame ] @ back }))

let printConfig config newBackupDir newBackupFreq newBackupsToKeep =
  printConfigRow "Backup path" config.path newBackupDir;
  printConfigRow "Backup frequency (in minutes)"
    (Int.to_string config.frequency)
    (Option.map ~f:Int.to_string newBackupFreq);
  printConfigRow "Number of backups to keep"
    (Int.to_string config.numToKeep)
    (Option.map ~f:Int.to_string newBackupsToKeep);
  printf "\n"

let editConfig
    config
    (backupDir : string option)
    (backupFreq : int option)
    (backupsToKeep : int option) =
  let newBackupDir = Option.value ~default:config.path backupDir |> realpath in

  let newBackupFreq = Option.value ~default:config.frequency backupFreq in

  let newBackupsToKeep = Option.value ~default:config.numToKeep backupsToKeep in

  printConfig config (Some newBackupDir) (Some newBackupFreq)
    (Some newBackupsToKeep);

  match (backupDir, backupFreq, backupsToKeep) with
  | None, None, None -> None
  | _ ->
      Some
        { config with
          path = newBackupDir
        ; frequency = newBackupFreq
        ; numToKeep = newBackupsToKeep
        }

let defaultConfig =
  { path = Args.Util.homeDir ^/ ".sbu-backups"
  ; frequency = 15
  ; numToKeep = 20
  ; games = []
  }

let saveDefaultConfig path =
  printf
    "Creating new config file at `%s'.\n\
     Use the `config' command to update default values, which are:\n\n\
     Backup path: %s\n\
     Backup frequency (in minutes): %d\n\
     Number of backups to keep: %d\n\n"
    path defaultConfig.path defaultConfig.frequency defaultConfig.numToKeep;

  let dir = dirname path in
  mkdir_p dir 0o775;
  Yojson.to_file path (Config.to_json defaultConfig)

let loadConfig configPath =
  if not (file_exists configPath) then saveDefaultConfig configPath;

  try Yojson.Basic.from_file configPath |> Config.of_json
  with e ->
    warn
      (sprintf
         "Couldn't load config: %s\n\
          Attempting to save default config to '%s' after backing up existing \
          config.\n"
         (Exn.to_string e) configPath);

    if file_exists configPath then file_copy configPath (configPath ^ ".bak");

    saveDefaultConfig configPath;
    defaultConfig

let load_config_t = Term.(const loadConfig $ Util.config_path)

let backup_t =
  Term.(
    const backup
    $ load_config_t
    $ BackupCmd.games
    $ BackupCmd.loop
    $ BackupCmd.verbose)

let add_t =
  Term.(const add $ load_config_t $ AddCmd.game $ AddCmd.path $ AddCmd.glob)

let list_t = Term.(const list $ load_config_t)
let info_t = Term.(const print_info $ load_config_t $ InfoCmd.games)

let remove_t =
  Term.(const remove $ load_config_t $ RemoveCmd.games $ RemoveCmd.yes)

let edit_t =
  Term.(
    const edit
    $ load_config_t
    $ EditCmd.game
    $ EditCmd.name
    $ EditCmd.path
    $ EditCmd.glob)

let config_t =
  Term.(
    const editConfig
    $ load_config_t
    $ ConfigCmd.path
    $ ConfigCmd.frequency
    $ ConfigCmd.keep)

let sbu_info = Term.info "sbu" ~version:"v1.0.0"
let sbu_t = Term.(ret (const (Fn.const (`Help (`Pager, None))) $ const 0))

let () =
  let config_path =
    Term.eval_peek_opts Util.config_path
    |> fst
    |> Option.value ~default:Util.defaultConfigPath
  in
  let result =
    Term.eval_choice (sbu_t, sbu_info)
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
