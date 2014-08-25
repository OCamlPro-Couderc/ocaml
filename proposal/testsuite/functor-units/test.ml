with X.(Mod; Builder as BX)
and Y.(Mod as ModY; Builder as BY)

let _ =
  let res = Mod.X.create 42 in
  let res' = ModY.X.create 21 in
  Mod.print res;
  ModY.print res';
  print_endline "Using builder";
  let vx = (BX.create_value 42) in
  let vy = (BY.create_value 42) in
  print_endline "Value created";
  Mod.print vx;
  ModY.print vy;
  print_endline "Using print_int";
  Mod.print_int 42;
  ModY.print_int 21
