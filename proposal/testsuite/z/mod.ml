in namespace Z
with Y.(Mod as Y_Mod)

include Y_Mod

let print_message () =
  print_endline "From Mod@Z"
