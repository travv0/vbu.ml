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

module Group = struct
  type t = { name : string; path : string; glob : string }

  let to_json { name; path; glob } =
    `Assoc
      [ ("name", `String name); ("path", `String path); ("glob", `String glob) ]

  let of_json json =
    let open Yojson.Basic.Util in
    { name = json |> member "name" |> to_string
    ; path = json |> member "path" |> to_string
    ; glob =
        json
        |> member "glob"
        |> to_string_option
        |> Option.value ~default:default_glob
    }

  let print ?new_name ?new_path ?new_glob group =
    print_config_row "Name" ~value:group.name ?new_value:new_name;
    print_config_row "Path" ~value:group.path ?new_value:new_path;

    (match (String.equal group.glob default_glob, new_glob) with
    | _, Some new_glob ->
        print_config_row "Glob" ~value:group.glob ~new_value:new_glob
    | false, None -> print_config_row "Glob" ~value:group.glob
    | _ -> ());

    printf "\n"

  let valid_name_chars : (char, _) Set.t =
    List.filter Char.all ~f:Char.is_alphanum @ [ '-'; '_' ]
    |> Set.of_list (module Char)

  let is_valid_name =
    String.for_all ~f:(fun c -> Set.exists ~f:Char.(( = ) c) valid_name_chars)
end

module Config = struct
  type t =
    { path : string; frequency : int; num_to_keep : int; groups : Group.t list }

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

module RunConfig = struct
  type t = { config : Config.t; verbose : bool }
end

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module MonadOps (M : Monad) = struct
  let ( >>= ) = M.bind
  let ( let* ) = M.bind

  module List = struct
    let rec foldm l ~init ~f =
      match l with
      | [] -> M.return init
      | x :: xs -> f init x >>= fun a -> foldm ~f ~init:a xs

    include List
  end

  module Array = struct
    let foldm a ~init ~f =
      let r = ref @@ M.return init in
      for i = 0 to Array.length a - 1 do
        r := !r >>= fun x -> f x (Array.unsafe_get a i)
      done;
      !r

    include Array
  end

  let whenm c f = if c then f () else M.return ()
end

module Reader (R : sig
  type env
end) =
struct
  module T = struct
    type 'a t = Reader of (R.env -> 'a)

    let run (Reader f) = f
    let ask = Reader (fun x -> x)
    let return x = Reader (fun _ -> x)
    let bind (Reader f) g = Reader (fun x -> run (g (f x)) x)
  end

  include T
  include MonadOps (T)
end

module Vbu = Reader (struct
  type env = RunConfig.t
end)
