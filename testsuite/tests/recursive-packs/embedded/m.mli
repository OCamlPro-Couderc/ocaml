module A : sig
  type t = B of M.B.t | A

  val v_b : M.B.t -> t

  val print : Format.formatter -> t -> unit
end

module B : sig
  type t = A of A.t | N of N.t | B

  val v_a : A.t -> t

  val v_n : N.t -> t

  val print : Format.formatter -> t -> unit
end
