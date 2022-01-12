module Console : sig
  val note : bool -> string -> unit
  val warn : string -> unit
  val err : string -> unit
  val prompt_y_or_n : string -> bool
end

module FileSystem : sig
  val home_dir : string
  val default_config_path : string
  val dirname : string -> string
  val basename : string -> string
  val ( ^/ ) : string -> string -> string
  val file_exists : string -> bool
  val is_directory : string -> bool
  val realpath : string -> string
  val readdir : string -> string array
  val default_glob : string
  val mkdir_p : string -> int -> unit
  val dir_exists : string -> bool
  val file_copy : ?overwrite:bool -> string -> string -> unit
  val file_move : ?overwrite:bool -> string -> string -> unit
end

module DateTime : sig
  val format_filename_time : float -> string
  val format_date : float -> string
  val format_time : float -> string
end
