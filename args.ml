open Base
open Cmdliner
open Printf
open Util.FileSystem

let config_path_t =
  let doc = "Path to configuration file." in
  Arg.(
    value
    & opt string default_config_path
    & info [ "config" ] ~docv:"CONFIG_PATH" ~doc)

module BackupCmd = struct
  let info = Term.info "backup" ~doc:"Backup your files."

  let groups =
    let doc =
      "List of groups to back up.  If not provided, will back up all groups."
    in
    Arg.(value & pos_all string [] & info [] ~docv:"GROUPS" ~doc)

  let loop =
    let doc =
      "Keep running, backing up files at the interval specified in your config."
    in
    Arg.(value & flag & info [ "l"; "loop" ] ~docv:"LOOP" ~doc)

  let verbose =
    let doc = "Print verbose output." in
    Arg.(value & flag & info [ "v"; "verbose" ] ~docv:"VERBOSE" ~doc)
end

module AddCmd = struct
  let info = Term.info "add" ~doc:"Add groups to backup."

  let group =
    let doc = "Name of group to add." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"GROUP" ~doc)

  let path =
    let doc = "Path to added group's files." in
    Arg.(
      required & opt (some string) None & info [ "p"; "path" ] ~docv:"PATH" ~doc)

  let glob =
    let doc =
      "File glob for added group's files. Only files matching this pattern \
       will be backed up. By default, all files in PATH will be recursively \
       backed up."
    in
    Arg.(
      value & opt string default_glob & info [ "g"; "glob" ] ~docv:"GLOB" ~doc)
end

module ListCmd = struct
  let info = Term.info "list" ~doc:"List names of groups that can be backed up."
end

module InfoCmd = struct
  let info = Term.info "info" ~doc:"List info for groups."

  let groups =
    let doc =
      "List of groups to display info for. If not provided, will display info \
       for all groups."
    in
    Arg.(value & pos_all string [] & info [] ~docv:"GROUPS" ~doc)
end

module RemoveCmd = struct
  let info = Term.info "remove" ~doc:"Remove groups from backup."

  let groups =
    let doc = "List of groups to remove." in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"GROUPS" ~doc)

  let yes =
    let doc = "Remove all without confirmation prompts." in
    Arg.(value & flag & info [ "y"; "yes" ] ~docv:"YES" ~doc)
end

module EditCmd = struct
  let info = Term.info "edit" ~doc:"Edit group info."

  let group =
    let doc = "Name of group to edit." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"GROUP" ~doc)

  let name =
    let doc =
      "Set group name to NEW_NAME. This will also update the directory name in \
       your backup directory."
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "n"; "name" ] ~docv:"NEW_NAME" ~doc)

  let path =
    let doc = "Set group's path to NEW_PATH." in
    Arg.(
      value
      & opt (some string) None
      & info [ "p"; "path" ] ~docv:"NEW_PATH" ~doc)

  let glob =
    let doc =
      sprintf
        "Set group's file glob to NEW_GLOB. Setting this to an empty string or \
         \"none\" implies the glob %s which will recursively back up all \
         files."
        default_glob
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "g"; "glob" ] ~docv:"NEW_GLOB" ~doc)
end

module ConfigCmd = struct
  let info = Term.info "config" ~doc:"Manage vbu configuration."

  let path =
    let doc = "Set path to directory in which to back up files." in
    Arg.(
      value
      & opt (some string) None
      & info [ "p"; "path" ] ~docv:"BACKUP_PATH" ~doc)

  let frequency =
    let doc = "Set frequency in minutes to backup files when looping." in
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
