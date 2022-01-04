type group = { name : string; path : string; glob : string }

module Group : sig
  val print :
    ?new_name:string -> ?new_path:string -> ?new_glob:string -> group -> unit

  val is_valid_name : string -> bool
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

  module List : sig
    val foldm :
      'a list -> init:'accum -> f:('accum -> 'a -> 'accum t) -> 'accum t

    include module type of Base.List
  end

  module Array : sig
    val foldm :
      'a array -> init:'accum -> f:('accum -> 'a -> 'accum t) -> 'accum t

    include module type of Base.Array
  end

  val whenm : bool -> (unit -> unit t) -> unit t
end
