
type t = B of M.B.t | A

let v_b v = B v

let print fmt = function
    B b -> Format.fprintf fmt "[A]%a" M.B.print b
  | A -> Format.fprintf fmt "A"
