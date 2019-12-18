type t = B of M.B.t | A of M.A.t | N

let v_a a = A a
let v_b b = B b

let print fmt = function
    B b -> Format.fprintf fmt "[N]%a" M.B.print b
  | A a -> Format.fprintf fmt "[N]%a" M.A.print a
  | N -> Format.fprintf fmt "N"
