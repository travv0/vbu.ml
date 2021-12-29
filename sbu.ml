open Base
open Printf

open Types

let defaultGlob = "**/*"

let note s = ANSITerminal.(printf [blue] "Note: %s" s)

let warn s = ANSITerminal.(printf [yellow] "Warning: %s" s)

let err s = ANSITerminal.(printf [red] "Error: %s" s)

let warnMissingGames config (games: string list) :(unit) app =
        let warningPrinted =
            List.fold
                (fun warningPrinted game ->
                    if not (List.exists ~f:String.(fun g -> g.name = game) config.games) then (
                        warn $"No game named `%s{game}'";
                        true)
                    else
                        warningPrinted)
                false
                games
                    in

        if warningPrinted then printf "\n"

let printconfigRow label value newValue =
    printf "%s: %s%s\n" label value
    @@ match newValue with
       | Some nv when value = nv -> ""
       | Some nv -> sprintf " -> %s" nv
       | None -> ""

let printGame game newName newPath newGlob =
    printconfigRow "Name" game.name newName;
    printconfigRow "Save path" game.path newPath;

    match (game.glob, newGlob) with
    | _, Some newGlob ->
        printconfigRow "Save glob" (Option.defaultValue "" game.glob) (Some(Option.defaultValue "" newGlob))
    | Some _, None -> printconfigRow "Save glob" (Option.defaultValue "" game.glob) None
    | _ -> ();

    printf "\n"

let cleanupBackups config (backupPath: string) verbose :(unit) app =
        if config.numToKeep > 0 then (
            let glob =
                Glob.Parse(
                    backupPath
                    + ".bak.[0-9][0-9][0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]"
                ) in

            let allFiles =
                Directory.EnumerateFiles(Path.GetDirectoryName(backupPath)) in

            let files =
                filter (fun f -> glob.isMatch(f: string)) allFiles
                |> plus (result backupPath) in

            if (length files > config.numToKeep) then (
                let sortedFiles =
                    sortByDescending File.getLastWriteTimeUtc files in

                let filesToDelete = skip config.numToKeep sortedFiles in

                filesToDelete |> List.iter ~f:(fun file -> 
                    if verbose then
                        note $"Deleting %s{file}";

                    File.Delete(file))))

let rec backupFile game basePath glob fromPath toPath verbose :(int * string list) app =
        try (
            let globMatches () =
                let glob =
                    Glob.Parse(Path.Join(basePath, Option.defaultValue defaultGlob glob)) in

                glob.isMatch(fromPath: string) in

            let copyAndCleanup () =
                    Directory.CreateDirectory(Path.GetDirectoryName(toPath: string))
                    |> ignore;

                    printf $"%s{fromPath} ==>\n\t%s{toPath}\n";
                    File.Copy(fromPath, toPath);
                    cleanupBackups toPath verbose;
                    return (1, empty) in

            let backupFile' () =
                    let fromInfo = FileInfo(fromPath) in

                    let fromIsReparsePoint =
                        fromInfo.attributes.hasFlag(FileAttributes.ReparsePoint) in

                    if fromIsReparsePoint then  (
                        if verbose then
                            note $"%s{fromPath} appears to be a link to somewhere else in the filesystem. Skipping...";

                        return (0, empty))
                    else (
                        let fromModTime = fromInfo.lastWriteTimeUtc in

                        let toModTime =
                            if File.exists(toPath) then
                                Some(File.getLastWriteTimeUtc(toPath))
                            else
                                None in

                        match toModTime with
                        | Some toModTime ->
                            if fromModTime <> toModTime then (
                                File.move(
                                    toPath,
                                    toPath
                                    + ".bak."
                                    + toModTime.toString("yyyy_MM_dd_HH_mm_ss")
                                );

                                return! copyAndCleanup ())
                            else
                                return (0, empty)
                        | None -> return! copyAndCleanup ())
                            in

            if Directory.exists(fromPath) then
                return! backupFiles game basePath glob fromPath toPath verbose
            else if globMatches () then
                return! backupFile' ()
            else
                return (0, empty))
        with e -> (
            let warning =
                sprintf "Unable to backup file %s for game %s:\n%s\n" toPath game e.message in

            warn warning;
            return (1, result warning))

and backupFiles game basePath glob fromPath toPath verbose :(int * string list) app =
    
        return!
            Directory.EnumerateFileSystemEntries(fromPath)
            |> toList
            |> List.foldM
                (fun (c, es) path ->
                    
                        let file = Path.GetFileName(path) in

                        let newCount, newErrs =
                            backupFile game basePath glob (Path.Join(fromPath, file)) (Path.Join(toPath, file)) verbose in

                        return (c + newCount, es ++ newErrs)
                    )
                (0, empty)
    

let backupGame config gameName verbose :(string list) app =
        let startTime = DateTime.Now in

        let game =
            tryFind ~f:String.(fun g -> g.name = gameName) config.games in

        match game with
        | Some game ->
            if Directory.Exists game.path then (
                let backedUpCount, warnings =
                    backupFiles game.name game.path game.glob game.path (Path.Join(config.path, gameName)) verbose in

                if (backedUpCount > 0) then (
                    let now = DateTime.Now in
                    let warningCount = length warnings in

                    printf
                        "\nFinished backing up %d file%s%s for %s in %fs on %s at %s\n\n"
                        backedUpCount
                        (if backedUpCount = 1 then "" else "s")
                        (if warningCount > 0 then
                             sprintf " with %d warning%s" warningCount (if warningCount = 1 then "" else "s")
                         else
                             "")
                        gameName
                        (now - startTime).totalSeconds
                        (now.toLongDateString())
                        (now.toLongTimeString()))

                return warnings)
            else (
                warn $"Path set for %s{gameName} doesn't exist: %s{game.path}"

                return empty)
        | None ->
            warnMissingGames [ gameName ]
            return empty

let rec backup config (gameNames: string list option) (loop: bool) (verbose: bool) :(config option) app =
        let gameNames' =
            match gameNames with
            | None -> map (fun g -> g.name) config.games
            | Some gns -> ofList gns in

        let warnings =
            gameNames'
            |> toList
            |> List.foldM
                (fun acc game ->
                    
                        try 
                            let warnings = backupGame game verbose in
                            return acc ++ warnings
                        with e -> (
                            err $"Error backing up %s{game}: %s{e.Message}";

                            return acc)
                    )
                empty in

        let warningCount = length warnings in

        if warningCount > 0 then
            withColor
                ConsoleColor.Yellow
                (fun () ->
                    printf "\n%d warning%s occurred:" warningCount (if warningCount = 1 then "" else "s");

                    if verbose then (
                        printf "\n\n";
                        iter (printf "%s\n") warnings)
                    else
                        printf "\nPass --verbose flag to print all warnings after backup completes\n\n");

        if loop then (
            Thread.Sleep(TimeSpan.FromMinutes(float config.frequency));
            return! backup gameNames loop verbose)
        else
            return None

let validGameNameChars : (char, Char.comparator_witness) Set.t =
    [ 'A'..'Z' ]
    @ [ 'a'..'z' ]
    @ [ '0'..'9' ]
    @ [ '-'; '_' ]
    |> ofSeq

let isValidGameName (name: string) =
    forall (fun c -> Set.contains c validGameNameChars) name

let absolutePath (path: string) =
    if path.length > 0 && path.[0] = '~' then 
        let home =
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) in

        Path.join(home, path.[1..]) |> Path.getFullPath
    else
        Path.getFullPath path

let add config (game: string) (path: string) (glob: string option) :(config option) app =
    
        if exists (fun g -> g.name = game) config.games then (
            err $"Game with the name %s{game} already exists";

            return None)
        else if not (isValidGameName game) then (
            err
                $"Invalid characters in name `%s{game}': only alphanumeric characters, underscores, and hyphens are allowed"

            return None)
        else (
            let newGame =
                { Name = game
                  Path = absolutePath path
                  Glob = glob } in

            let newGames =
                result newGame
                |> plus config.games
                |> sortBy (fun g -> g.name) in

            printf $"Successfully added %s{game}\n\n";
            printGame newGame None None None;
            return Some { config with Games = newGames })
    

let list config :(config option) app =
    
        iter (fun g -> printf $"%s{g.name}\n") config.games;
        return None
    

let info config (gameNames: string list option) :(config option) app =

        match gameNames with
        | Some gns -> warnMissingGames gns
        | None -> ();

        let games =
            match gameNames with
            | None -> config.games
            | Some gs -> filter (fun g -> exists ((=) g.name) gs) config.games in

        iter (fun g -> printGame g None None None) games;
        return None
    

let rec promptYorN prompt =
    stdout.Flush();
    printf $"\n%s{prompt} (y/n) ";

    match Console.ReadLine().Trim().ToLower() with
    | "y" -> true
    | "n" -> false
    | _ -> promptYorN prompt

let remove config (games: string NonEmptyList) (yes: bool) :(config option) app =
    
        warnMissingGames games;

        let newGames =
            [| for game in config.games do
                   if exists ((=) game.name) games
                      && (yes
                          || promptYorN (
                              "Are you sure you want to remove "
                              + game.name
                              + "?"
                          )) then
                       printf $"Removed %s{game.name}\n"
                   else
                       yield game |] in

        return Some { config with Games = newGames }
    

let edit
                    config
    (gameName: string)
    (newName: string option)
    (newPath: string option)
    (newGlob: string option)
    :(config option) app =
        match (newName, newPath, newGlob) with
        | None, None, None ->
            err "One or more of --name, --path, or --glob must be provided.";
            return None
        | _ ->
            let splitList =
                tryFindIndex (fun g -> g.name = gameName) config.games
                |> map (fun i -> toList config.games |> List.splitAt i) in

            match splitList with
            | None ->
                warnMissingGames [ gameName ];
                return None
            | Some (_, []) ->
                err "Couldn't find game in list";
                return None
            | Some (front, game :: back) ->
                let newName' = Option.defaultValue game.name newName in

                let newGlob' =
                    match newGlob with
                    | Some "none"
                    | Some "" -> Some None
                    | None -> None
                    | glob -> Some glob in

                let newPath' =
                    Option.defaultValue game.path newPath
                    |> absolutePath in

                let editedGame =
                    { game with
                          Name = newName'
                          Path = newPath'
                          Glob = Option.defaultValue None newGlob' } in

                if not (isValidGameName newName') then (
                    err
                        $"Invalid characters in name `%s{newName'}': only alphanumeric characters, underscores, and hyphens are allowed";

                    return None)
                else (
                    printGame game (Some newName') (Some newPath') newGlob';

                    let backupDirExists =
                        Directory.Exists(Path.Join(config.path, gameName)) in

                    if (Option.isSome newName && backupDirExists) then (
                        note "Game name changed, renaming backup directory...";

                        Directory.Move(Path.Join(config.path, gameName), Path.Join(config.path, newName')));

                    return
                        Some
                            { config with
                                  Games = front ++ result editedGame ++ back |> ofSeq })
    

let printconfig config newBackupDir newBackupFreq newBackupsToKeep :(unit) app =
        printconfigRow "Backup path" config.path newBackupDir;
        printconfigRow "Backup frequency (in minutes)" (string config.frequency) (string <!> newBackupFreq);
        printconfigRow "Number of backups to keep" (string config.numToKeep) (string <!> newBackupsToKeep);
        printf "\n"
    

let editconfig config (backupDir: string option) (backupFreq: int option) (backupsToKeep: int option) :(config option) app =
        let newBackupDir =
            Option.defaultValue config.path backupDir
            |> absolutePath in

        let newBackupFreq =
            Option.defaultValue config.frequency backupFreq in

        let newBackupsToKeep =
            Option.defaultValue config.numToKeep backupsToKeep in

        printconfig (Some newBackupDir) (Some newBackupFreq) (Some newBackupsToKeep);

        match (backupDir, backupFreq, backupsToKeep) with
        | None, None, None -> return None
        | _ ->
            return
                Some
                    { config with
                          Path = newBackupDir
                          Frequency = newBackupFreq
                          NumToKeep = newBackupsToKeep }
    

let defaultconfigPath =
    Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs", "config.json")

let defaultconfig =
    { path = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".sbufs-backups")
      frequency = 15
      numToKeep = 20
      games = empty }

let saveDefaultconfig path =
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
            | config sp -> editconfig (sp.TryGetResult Path) (sp.TryGetResult Frequency) (sp.TryGetResult Keep)
            | config_Path _
            | Version -> failwithf $"non-command matched as command: %A{command}"
    

let loadconfig configPath =
    if not (File.Exists(configPath)) then
        saveDefaultconfig configPath

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

        saveDefaultconfig configPath;
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
                |> Option.defaultValue defaultconfigPath in

            let config = loadconfig configPath in
            let newconfig = runApp parseResults config in

            match newconfig with
            | None -> ()
            | Some c ->
                Directory.CreateDirectory(Path.GetDirectoryName(configPath))
                |> ignore

                File.WriteAllText(configPath, JsonSerializer.Serialize(c)))
    with
    | :? ArguParseException as e -> printf $"%s{e.Message}\n"
    | e -> err e.Message
