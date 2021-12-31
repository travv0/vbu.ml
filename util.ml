open Base
open Printf
open Stdio

module Console = struct
  let note verbose s =
    if verbose then ANSITerminal.(printf [ blue ] "Note: %s\n" s)

  let warn s = ANSITerminal.(printf [ yellow ] "Warning: %s\n" s)
  let err s = ANSITerminal.(printf [ red ] "Error: %s\n" s)

  let rec prompt_y_or_n prompt =
    printf "\n%s (y/N) " prompt;
    Out_channel.flush stdout;

    match
      In_channel.input_line ~fix_win_eol:true stdin
      |> Option.map ~f:String.strip
    with
    | Some "y" | Some "Y" -> true
    | Some "n" | Some "N" | Some "" | None -> false
    | Some s ->
        printf "Invalid input: entered `%s'.\n" s;
        prompt_y_or_n prompt
end

module FileSystem = struct
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

  let rec mkdir_p path perms =
    try Unix.mkdir path perms with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        mkdir_p (dirname path) perms;
        Unix.mkdir path perms

  let dir_exists path_name = file_exists path_name && is_directory path_name

  module FileCopy = struct
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
      Unix.close fd_out;

      let stats = Unix.lstat input_name in
      let atime = stats.st_atime in
      let mtime = stats.st_mtime in
      Unix.utimes output_name atime mtime
  end

  include FileCopy
end

module DateTime = struct
  let format_filename_time t =
    let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
      Unix.gmtime t
    in
    sprintf "%04d_%02d_%02d_%02d_%02d_%02d" (tm_year + 1900) (tm_mon + 1)
      tm_mday tm_hour tm_min tm_sec

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
    let hours =
      let hour = tm_hour % 12 in
      if hour = 0 then 12 else hour
    in
    let suffix = if tm_hour > 11 then "PM" else "AM" in
    sprintf "%d:%02d:%02d %s" hours tm_min tm_sec suffix
end
