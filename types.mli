type group = { name : string; path : string; glob : string }

module Group : sig
  val print :
    ?new_name:string -> ?new_path:string -> ?new_glob:string -> group -> unit
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

module Vbu : sig
  type 'a t

  val run : 'a t -> config -> 'a
  val ask : config t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val fold_list : f:('a -> 'b -> 'a t) -> init:'a -> 'b list -> 'a t
  val whenm : bool -> (unit -> unit t) -> unit t
end
