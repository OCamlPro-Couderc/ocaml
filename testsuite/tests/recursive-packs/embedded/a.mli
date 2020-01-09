type t = B of B.t | A

val v_b : B.t -> t

val print : Format.formatter -> t -> unit
