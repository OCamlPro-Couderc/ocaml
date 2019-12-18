let a = P.M.A.A
let ba = P.M.B.v_a a
let aba = P.M.A.v_b ba
let naba = P.N.v_a aba

let () =
  Format.printf "Result: %a \n%!" P.N.print naba
