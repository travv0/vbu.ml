open Base

let home_dir = Sys.getenv "HOME" |> Option.value ~default:"."

let default_config_path =
  Caml.Filename.concat (Caml.Filename.concat home_dir ".vbu") "config.json"

let dirname = Caml.Filename.dirname
let basename = Caml.Filename.basename
let ( ^/ ) = Caml.Filename.concat
let file_exists = Caml.Sys.file_exists
let is_directory = Caml.Sys.is_directory
let realpath path = try Unix.realpath path with _ -> path
let readdir = Caml.Sys.readdir
let default_glob = "**"
