type t = Leaf of string | Node of Aset.t
val compare: t -> t -> int
