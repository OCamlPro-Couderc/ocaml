type t = B of M.B.t | A of M.A.t | N

val v_a : M.A.t -> t
val v_b : M.B.t -> t

val print : Format.formatter -> t -> unit
