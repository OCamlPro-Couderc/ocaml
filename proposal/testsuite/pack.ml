with (P) of P1
and (P as P2) of P2;;

let () =
  P.Mod.print P2.Mod.value;
  print_endline "";
  P2.Mod.print P.Mod.value;
  print_endline ""
