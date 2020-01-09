
type t = B of B.t | A

let v_b v = B v

let print fmt = function
    B b -> Format.fprintf fmt "[A]%a" B.print b
  | A -> Format.fprintf fmt "A"
