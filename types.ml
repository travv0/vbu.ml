open Base
open Printf
open Stdio
open Util.Console
open Util.FileSystem

let print_config_row ~value ?new_value label =
  printf "%s: %s%s\n" label value
  @@
  match new_value with
  | Some nv when String.equal value nv -> ""
  | Some nv -> sprintf " -> %s" nv
  | None -> ""

type group = { name : string; path : string; glob : string option }

module Group = struct
  let to_json { name; path; glob } =
    `Assoc
      [ ("name", `String name)
      ; ("path", `String path)
      ; ( "glob"
        , glob
          |> Option.map ~f:(fun a -> `String a)
          |> Option.value ~default:`Null )
      ]

  let of_json json =
    let open Yojson.Basic.Util in
    { name = json |> member "name" |> to_string
    ; path = json |> member "path" |> to_string
    ; glob = json |> member "glob" |> to_string_option
    }

  let print ?new_name ?new_path ?new_glob group =
    print_config_row "Name" ~value:group.name ?new_value:new_name;
    print_config_row "Path" ~value:group.path ?new_value:new_path;

    (match (group.glob, new_glob) with
    | _, Some new_glob ->
        print_config_row "Glob"
          ~value:(Option.value ~default:"" group.glob)
          ~new_value:(Option.value ~default:"" new_glob)
    | Some _, None ->
        print_config_row "Glob" ~value:(Option.value ~default:"" group.glob)
    | _ -> ());

    printf "\n"
end

type config =
  { path : string; frequency : int; num_to_keep : int; groups : group list }

module Config = struct
  let default =
    { path = home_dir ^/ ".vbu-backups"
    ; frequency = 15
    ; num_to_keep = 20
    ; groups = []
    }

  let to_json { path; frequency; num_to_keep; groups } =
    `Assoc
      [ ("path", `String path)
      ; ("frequency", `Int frequency)
      ; ("num_to_keep", `Int num_to_keep)
      ; ("groups", `List (List.map groups ~f:Group.to_json))
      ]

  let of_json json =
    let open Yojson.Basic.Util in
    { path = json |> member "path" |> to_string
    ; frequency = json |> member "frequency" |> to_int
    ; num_to_keep = json |> member "num_to_keep" |> to_int
    ; groups = json |> member "groups" |> to_list |> List.map ~f:Group.of_json
    }

  let print ?new_backup_dir ?new_backup_freq ?new_backups_to_keep config =
    print_config_row "Backup path" ~value:config.path ?new_value:new_backup_dir;
    print_config_row "Backup frequency (in minutes)"
      ~value:(Int.to_string config.frequency)
      ?new_value:(Option.map ~f:Int.to_string new_backup_freq);
    print_config_row "Number of backups to keep"
      ~value:(Int.to_string config.num_to_keep)
      ?new_value:(Option.map ~f:Int.to_string new_backups_to_keep);
    printf "\n"

  let save_default path =
    printf
      "Creating new config file at `%s'.\n\
       Use the `config' command to update default values, which are:\n\n\
       Backup path: %s\n\
       Backup frequency (in minutes): %d\n\
       Number of backups to keep: %d\n\n"
      path default.path default.frequency default.num_to_keep;

    mkdir_p (dirname path) 0o775;
    Yojson.to_file path (to_json default)

  let load path =
    if not (file_exists path) then save_default path;

    try Yojson.Basic.from_file path |> of_json
    with e ->
      warn
        (sprintf
           "Couldn't load config: %s\n\
            Attempting to save default config to '%s' after backing up \
            existing config.\n"
           (Exn.to_string e) path);

      if file_exists path then file_copy path (path ^ ".bak");

      save_default path;
      default

  let save config path =
    mkdir_p (dirname path) 0o775;
    Yojson.Basic.to_file path (to_json config)
end
