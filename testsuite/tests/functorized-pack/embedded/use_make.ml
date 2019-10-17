module M = Make(struct let v = 5 end)

let _ =
  let m1_v = M.M.M1.v in
  Printf.printf "M.M.M1.v: %d\n%!" m1_v;
  let m2_v = M.M.M2.v in
  Printf.printf "M.M.M2.v: %d\n%!" m2_v;
  let n1_v = M.N.N1.v in
  Printf.printf "M.N.N1.v: %d\n%!" n1_v;
  let n2_v = M.N.N2.v in
  Printf.printf "M.N.N2.v: %d\n%!" n2_v;
