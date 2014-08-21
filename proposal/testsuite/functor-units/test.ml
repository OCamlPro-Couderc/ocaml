with (Mod) of X
and (Mod as ModY) of Y

let _ =
  let res = Mod.X.create 42 in
  let res' = ModY.X.create 21 in
  Mod.print res;
  ModY.print res'
