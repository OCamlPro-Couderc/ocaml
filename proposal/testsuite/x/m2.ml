in namespace X
with X.(M1)

type t = { value_m2 : M1.t; str : string }

let to_string v =
  Format.sprintf "{ vm2: %s; str: %s }" (M1.to_string v.value_m2) v.str
