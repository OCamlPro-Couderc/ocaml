in namespace Z
with (Mod as Y_Mod) of Y;;

include Y_Mod

let print_message () =
  print_endline "From Mod@Z"
