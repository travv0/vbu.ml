open Base
open Cmdliner

module BackupCmd = struct
  let info = Term.info "backup" ~doc:"Backup your game saves."

  let games =
    let doc =
      "List of games to back up.  If not provided, will back up all games."
    in
    Arg.(value & pos_all string [] & info [] ~docv:"GAMES" ~doc)

  let loop =
    let doc =
      "Keep running, backing up games at the interval specified in your config \
       file."
    in
    Arg.(value & flag & info [ "l"; "loop" ] ~docv:"LOOP" ~doc)

  let verbose =
    let doc = "Print verbose output." in
    Arg.(value & flag & info [ "v"; "verbose" ] ~docv:"VERBOSE" ~doc)
end

module AddCmd = struct
  let info = Term.info "add" ~doc:"Add games to backup."

  let game =
    let doc = "Name of game to add." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"GAME" ~doc)

  let path =
    let doc = "Path to added game's save files." in
    Arg.(
      required
      & opt (some string) None
      & info [ "p"; "path" ] ~docv:"SAVE_PATH" ~doc)

  let glob =
    let doc =
      "Save file glob for added game's save files. Only files matching this \
       pattern will be backed up. The default is **/* which will recursively \
       back up all saves in SAVE_PATH."
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "g"; "glob" ] ~docv:"SAVE_GLOB" ~doc)
end

module ListCmd = struct
  let info = Term.info "list" ~doc:"List names of games that can be backed up."
end

module InfoCmd = struct
  let info = Term.info "info" ~doc:"List info for games."

  let games =
    let doc =
      "List of games to display info for. If not provided, will display info \
       for all games."
    in
    Arg.(value & pos_all string [] & info [] ~docv:"GAMES" ~doc)
end

module RemoveCmd = struct
  let info = Term.info "remove" ~doc:"Remove games from backup."

  let games =
    let doc = "List of games to remove." in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"GAMES" ~doc)

  let yes =
    let doc = "Remove all without confirmation prompts." in
    Arg.(value & flag & info [ "y"; "yes" ] ~docv:"YES" ~doc)
end

module EditCmd = struct
  let info = Term.info "edit" ~doc:"Edit game info."

  let game =
    let doc = "Name of game to edit." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"GAME" ~doc)

  let name =
    let doc =
      "Set game name to NEW_NAME. This will also update the directory name in \
       your backup directory."
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "n"; "name" ] ~docv:"NEW_NAME" ~doc)

  let path =
    let doc = "Set game's save path to NEW_SAVE_PATH." in
    Arg.(
      value
      & opt (some string) None
      & info [ "p"; "path" ] ~docv:"NEW_SAVE_PATH" ~doc)

  let glob =
    let doc =
      "Set game's save file glob to NEW_SAVE_GLOB. Setting this to an empty \
       string or \"none\" implies the glob **/* which will recursively back up \
       all files."
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "g"; "glob" ] ~docv:"NEW_SAVE_GLOB" ~doc)
end

module ConfigCmd = struct
  let info = Term.info "config" ~doc:"Manage sbu configuration."

  let path =
    let doc = "Set path to directory in which to back up saves." in
    Arg.(
      value
      & opt (some string) None
      & info [ "p"; "path" ] ~docv:"BACKUP_PATH" ~doc)

  let frequency =
    let doc = "Set frequency in minutes to backup saves when looping." in
    Arg.(
      value
      & opt (some int) None
      & info [ "f"; "frequency" ] ~docv:"BACKUP_FREQUENCY" ~doc)

  let keep =
    let doc = "Set how many copies of each backed-up file to keep." in
    Arg.(
      value
      & opt (some int) None
      & info [ "k"; "keep" ] ~docv:"BACKUPS_TO_KEEP" ~doc)
end

module Util = struct
  let home_dir = Sys.getenv "HOME" |> Option.value ~default:"."

  let default_config_path =
    Caml.Filename.concat (Caml.Filename.concat home_dir ".sbu") "config.json"

  let config_path =
    let doc = "Path to configuration file." in
    Arg.(
      value
      & opt string default_config_path
      & info [ "config" ] ~docv:"CONFIG_PATH" ~doc)
end
