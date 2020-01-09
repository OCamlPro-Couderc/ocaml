type t = A of A.t | N of N.t | B

val v_a : A.t -> t

val v_n : N.t -> t

val print : Format.formatter -> t -> unit
