module type T = sig
  type t

  val implementation : string

  val gettimeofday : unit -> t

  val usleep : t -> unit

  val of_float : float -> t

  val to_float : t -> float

  val ( |+| ) : t -> t -> t

  val ( |-| ) : t -> t -> t

  val ( |*| ) : t -> t -> t

  val ( |<| ) : t -> t -> bool

  val ( |<=| ) : t -> t -> bool
end

val implementation : (module T) ref
