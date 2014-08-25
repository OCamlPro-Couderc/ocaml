in namespace Y
with X.(M1 as Mod)

type t = Mod.t

let create = Mod.create

let value = 42 ++ 42

let print_message () =
  print_endline "From Mod@Y"
