type t = Leaf of string | Node of Aset.t
let compare t1 t2 =
  match (t1, t2) with
  | (Leaf s1, Leaf s2) -> Stdlib.compare s1 s2
  | (Leaf _, Node _) -> 1
  | (Node _, Leaf _) -> -1
  | (Node n1, Node n2) -> Aset.compare n1 n2
