
type t = A of A.t | N of N.t | B

let v_a a = A a

let v_n n = N n

let print fmt = function
    A a -> Format.fprintf fmt "[B]%a" A.print a
  | N n -> Format.fprintf fmt "[B]%a" N.print n
  | B -> Format.fprintf fmt "B"
