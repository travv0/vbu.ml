open Base
open Stdio
open Printf
open Types
module Glob = Dune_glob.V1

let defaultGlob = "**/*"
let note s = ANSITerminal.(printf [ blue ] "Note: %s\n" s)
let warn s = ANSITerminal.(printf [ yellow ] "Warning: %s\n" s)
let err s = ANSITerminal.(printf [ red ] "Error: %s\n" s)

let warnMissingGames config (games : string list) : unit app =
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

  match (game.glob, newGlob) with
  | _, Some newGlob ->
      printConfigRow "Save glob"
        (Option.value ~default:"" game.glob)
        (Some (Option.value ~default:"" newGlob))
  | Some _, None ->
      printConfigRow "Save glob" (Option.value ~default:"" game.glob) None
  | _ ->
      ();

      printf "\n"

let buffer_size = 8192
let buffer = Bytes.create buffer_size

let file_copy input_name output_name =
  let open Unix in
  let fd_in = openfile input_name [ O_RDONLY ] 0 in
  let fd_out = openfile output_name [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666 in
  let rec copy_loop () =
    match read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r ->
        ignore (write fd_out buffer 0 r);
        copy_loop ()
  in
  copy_loop ();
  close fd_in;
  close fd_out

let dir_exists path_name =
  Caml.Sys.file_exists path_name && Caml.Sys.is_directory path_name

let cleanupBackups config (backupPath : string) verbose : unit app =
  if config.numToKeep > 0 then
    let glob =
      backupPath
      ^ ".bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
      |> Glob.of_string
    in

    let allFiles = Caml.Sys.readdir (Caml.Filename.dirname backupPath) in

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

let format_time t =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    Unix.gmtime t
  in
  sprintf "%4d_%2d_%2d_%2d_%2d_%2d" (tm_year + 1900) (tm_mon + 1) tm_mday
    tm_hour tm_min tm_sec

let rec backupFile config game basePath glob fromPath toPath (verbose : bool) :
    (int * string list) app =
  try
    let globMatches () =
      let glob =
        Caml.Filename.concat basePath (Option.value ~default:defaultGlob glob)
        |> Glob.of_string
      in

      Glob.test glob fromPath
    in

    let copyAndCleanup () =
      Unix.mkdir (Caml.Filename.dirname toPath) 0o775 |> ignore;

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
          if Caml.Sys.file_exists toPath then Some (Unix.lstat toPath).st_mtime
          else None
        in

        match toModTime with
        | Some toModTime ->
            if Float.(fromModTime <> toModTime) then (
              Caml.Sys.rename toPath (toPath ^ ".bak." ^ format_time toModTime);
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
    (int * string list) app =
  Caml.Sys.readdir fromPath
  |> Array.fold
       ~f:(fun (c, es) path ->
         let file = Caml.Filename.basename path in

         let newCount, newErrs =
           backupFile config game basePath glob
             (Caml.Filename.concat fromPath file)
             (Caml.Filename.concat toPath file)
             verbose
         in

         (c + newCount, es @ newErrs))
       ~init:(0, [])

let backupGame config gameName verbose : string list app =
  let startTime = Unix.time () in

  let game = List.find ~f:String.(fun g -> g.name = gameName) config.games in

  match game with
  | Some game ->
      if dir_exists game.path then (
        let backedUpCount, warnings =
          backupFiles config game.name game.path game.glob game.path
            (Caml.Filename.concat config.path gameName)
            verbose
        in

        (if backedUpCount > 0 then
         let now = Unix.time () in
         let warningCount = List.length warnings in

         printf "\nFinished backing up %d file%s%s for %s in %fs"
           (* on %s at %s\n\n *)
           backedUpCount
           (if backedUpCount = 1 then "" else "s")
           (if warningCount > 0 then
            sprintf " with %d warning%s" warningCount
              (if warningCount = 1 then "" else "s")
           else "")
           gameName
           Float.(now - startTime));
        (*now
          (now.toLongTimeString ()));*)
        warnings)
      else (
        warn (sprintf "Path set for %s doesn't exist: %s" gameName game.path);
        [])
  | None ->
      warnMissingGames config [ gameName ];
      []

let rec backup
    config
    (gameNames : string list option)
    (loop : bool)
    (verbose : bool) : config option app =
  let gameNames' =
    match gameNames with
    | None -> List.map ~f:(fun g -> g.name) config.games
    | Some gns -> gns
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

let validGameNameChars : (char, Char.comparator_witness) Set.t =
  List.filter Char.all ~f:Char.is_alphanum @ [ '-'; '_' ]
  |> Set.of_list (module Char)

let isValidGameName =
  String.for_all ~f:(fun c -> Set.exists ~f:Char.(( = ) c) validGameNameChars)

let add config (game : string) (path : string) (glob : string option) :
    config option app =
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
    let newGame = { name = game; path = Unix.realpath path; glob } in

    let newGames =
      config.games @ [ newGame ]
      |> List.sort ~compare:(fun g1 g2 -> String.compare g1.name g2.name)
    in

    printf "Successfully added %s\n\n" game;
    printGame newGame None None None;
    Some { config with games = newGames }

let list config : config option app =
  List.iter ~f:(fun g -> printf "%s\n" g.name) config.games;
  None

let info config (gameNames : string list option) : config option app =
  (match gameNames with Some gns -> warnMissingGames config gns | None -> ());

  let games =
    match gameNames with
    | None -> config.games
    | Some gs ->
        List.filter
          ~f:(fun g -> List.exists ~f:String.(( = ) g.name) gs)
          config.games
  in

  List.iter ~f:(fun g -> printGame g None None None) games;
  None

let rec promptYorN prompt =
  Out_channel.flush stdout;
  printf "\n%s (y/n) " prompt;

  match
    In_channel.input_line ~fix_win_eol:true stdin |> Option.map ~f:String.strip
  with
  | Some "y" | Some "Y" -> true
  | Some "n" | Some "N" | None -> false
  | _ -> promptYorN prompt

let remove config (games : string list) (yes : bool) : config option app =
  warnMissingGames config games;

  let newGames =
    List.filter_map config.games ~f:(fun game ->
        if
          List.exists ~f:String.(( = ) game.name) games
          && (yes
             || promptYorN ("Are you sure you want to remove " ^ game.name ^ "?")
             )
        then (
          printf "Removed %s{game.name}\n" game.name;
          None)
        else Some game)
  in

  Some { config with games = newGames }

let edit
    config
    (gameName : string)
    (newName : string option)
    (newPath : string option)
    (newGlob : string option) : config option app =
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

          let newPath' =
            Option.value ~default:game.path newPath |> Unix.realpath
          in

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

            let backupDirExists =
              dir_exists (Caml.Filename.concat config.path gameName)
            in

            if Option.is_some newName && backupDirExists then (
              note "Game name changed, renaming backup directory...";

              Unix.rename
                (Caml.Filename.concat config.path gameName)
                (Caml.Filename.concat config.path newName'));

            Some { config with games = front @ [ editedGame ] @ back }))

let printConfig config newBackupDir newBackupFreq newBackupsToKeep : unit app =
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
    (backupsToKeep : int option) : config option app =
  let newBackupDir =
    Option.value ~default:config.path backupDir |> Unix.realpath
  in

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

let homeDir = Sys.getenv "HOME" |> Option.value ~default:"."

let defaultConfigPath =
  Caml.Filename.concat (Caml.Filename.concat homeDir ".sbu") "config.json"

let defaultConfig =
  { path = Caml.Filename.concat homeDir ".sbu-backups"
  ; frequency = 15
  ; numToKeep = 20
  ; games = []
  }
(*
   let saveDefaultConfig path =
       match Path.GetDirectoryName(path: string) with
       | "" -> ()
       | path -> Directory.CreateDirectory(path) |> ignore

       File.WriteAllText(path, JsonSerializer.Serialize(defaultconfig))

   let app (parseResults: ParseResults<_>) :(config option) app =

           let command = parseResults.GetSubCommand() in

           return!
               match command with
               | Backup sp -> backup (sp.TryGetResult BackupArgs.Games) (sp.Contains Loop) (sp.Contains Verbose)
               | Add sp -> add (sp.GetResult AddArgs.Game) (sp.GetResult AddArgs.Path) (sp.TryGetResult AddArgs.Glob)
               | List _ -> list ()
               | Info sp -> info (sp.TryGetResult InfoArgs.Games)
               | Remove sp -> remove (NonEmptyList.ofList (sp.GetResult Games)) (sp.Contains Yes)
               | Edit sp ->
                   edit (sp.GetResult Game) (sp.TryGetResult Name) (sp.TryGetResult EditArgs.Path) (sp.TryGetResult Glob)
               | Config sp -> editconfig (sp.TryGetResult Path) (sp.TryGetResult Frequency) (sp.TryGetResult Keep)
               | Config_Path _
               | Version -> failwithf $"non-command matched as command: %A{command}"


   let loadConfig configPath =
       if not (File.Exists(configPath)) then
           saveDefaultConfig configPath

       try
           configPath
           |> File.ReadAllText
           |> JsonSerializer.Deserialize<config>
       with e ->
           warn
               $"Couldn't load config: %s{e.Message}\nAttempting to save default config \
               to '%s{configPath}' after backing up existing config.\n";

           if File.Exists(configPath) then
               File.Copy(configPath, configPath + ".bak", true);

           saveDefaultConfig configPath;
           defaultconfig

   let runApp = Reader.run << app

   let () =
       try (
           let parser =
               ArgumentParser.Create<SbuArgs>(programName = AppDomain.CurrentDomain.FriendlyName) in

           let parseResults =
               parser.ParseCommandLine(inputs = argv, raiseOnUsage = true) in

           if parseResults.Contains Version then
               printf "sbu v1.2.3\n"
           else
               let configPath =
                   parseResults.TryGetResult config_Path
                   |> Option.value ~default:defaultconfigPath in

               let config = loadconfig configPath in
               let newConfig = runApp parseResults config in

               match newConfig with
               | None -> ()
               | Some c ->
                   Directory.CreateDirectory(Path.GetDirectoryName(configPath))
                   |> ignore

                   File.WriteAllText(configPath, JsonSerializer.Serialize(c)))
       with
       | :? ArguParseException as e -> printf $"%s{e.Message}\n"
       | e -> err e.Message *)
