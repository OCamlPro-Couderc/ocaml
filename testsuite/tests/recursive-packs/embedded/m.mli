module rec A : sig
  type t = B of B.t | A

  val v_b : B.t -> t

  val print : Format.formatter -> t -> unit
end

and B : sig
  type t = A of A.t | N of N.t | B

  val v_a : A.t -> t

  val v_n : N.t -> t

  val print : Format.formatter -> t -> unit
end
