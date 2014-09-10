with X.(M1, _)
and Y.(Mod)
(* Mod@Y rebinds M1@X *)
and Z.(Mod as ZMod)
(* Z : include Mod@Y *)

open M1

let () =
  let v = create 42 in
  let v2 = Mod.create 42 in
  let v3 = ZMod.create 42 in
  assert (v = v2);
  assert (v2 = v3);
  assert (v = v3);
  let str = "Hello world!" in
  let m2_val = M2.to_string M2.{ value_m2 = v; str } in
  let m2_val' = M2.to_string M2. { value_m2 = v2; str } in
  let m2_val'' = M2.to_string M2. { value_m2 = v3; str } in
  assert (m2_val = m2_val');
  assert (m2_val' = m2_val'');
  assert (m2_val = m2_val'');
  Format.printf "value1: %s\nvalue2: %s\nvalue3: %s@."
    m2_val m2_val' m2_val''
