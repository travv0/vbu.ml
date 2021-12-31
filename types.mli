type group = { name : string; path : string; glob : string option }

module Group : sig
  val print :
       ?new_name:string
    -> ?new_path:string
    -> ?new_glob:string option
    -> group
    -> unit
end

type config =
  { path : string; frequency : int; num_to_keep : int; groups : group list }

module Config : sig
  val print :
       ?new_backup_dir:string
    -> ?new_backup_freq:int
    -> ?new_backups_to_keep:int
    -> config
    -> unit

  val load : string -> config
  val save : config -> string -> unit
end
