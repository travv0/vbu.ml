open Base

module Group : sig
  type t = { name : string; path : string; glob : string }

  val print :
    ?new_name:string -> ?new_path:string -> ?new_glob:string -> t -> unit

  val is_valid_name : string -> bool
end

module Config : sig
  type t =
    { path : string; frequency : int; num_to_keep : int; groups : Group.t list }

  val print :
       ?new_backup_dir:string
    -> ?new_backup_freq:int
    -> ?new_backups_to_keep:int
    -> t
    -> unit

  val load : string -> t
  val save : t -> string -> unit
end

module RunConfig : sig
  type t = { config : Config.t; verbose : bool }
end

module Vbu : sig
  type 'a t

  val run : 'a t -> RunConfig.t -> 'a
  val ask : RunConfig.t t
  val when_m : bool -> (unit -> unit t) -> unit t

  module List : sig
    val fold_m :
      'a list -> init:'accum -> f:('accum -> 'a -> 'accum t) -> 'accum t

    include module type of List
  end

  module Array : sig
    val fold_m :
      'a array -> init:'accum -> f:('accum -> 'a -> 'accum t) -> 'accum t

    include module type of Array
  end

  include Monad.S with type 'a t := 'a t
end
