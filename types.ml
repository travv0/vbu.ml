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
  [@@deriving yojson { exn = true }]

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
  [@@deriving yojson { exn = true }]

  let default =
    { path = home_dir ^/ ".vbu-backups"
    ; frequency = 15
    ; num_to_keep = 20
    ; groups = []
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
    Yojson.Safe.to_file path (to_yojson default)

  let load path =
    if not (file_exists path) then save_default path;

    try Yojson.Safe.from_file path |> of_yojson_exn
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
    Yojson.Safe.to_file path (to_yojson config)
end

module RunConfig = struct
  type t = { config : Config.t; verbose : bool }
end

module MonadOps (M : Monad.S) = struct
  open M

  module List = struct
    let rec fold_m l ~init ~f =
      match l with
      | [] -> M.return init
      | x :: xs -> f init x >>= fun a -> fold_m ~f ~init:a xs

    include List
  end

  module Array = struct
    let fold_m a ~init ~f =
      let r = ref @@ M.return init in
      for i = 0 to Array.length a - 1 do
        r := !r >>= fun x -> f x (Array.unsafe_get a i)
      done;
      !r

    include Array
  end

  let when_m c f = if c then f () else M.return ()
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
    let bind (Reader f) ~f:g = Reader (fun x -> run (g (f x)) x)
    let map = `Define_using_bind
  end

  include T

  module M = struct
    type 'a t = 'a T.t

    include Monad.Make (T)
  end

  include M
  include MonadOps (M)
end

module Vbu = Reader (struct
  type env = RunConfig.t
end)
