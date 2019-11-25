open P

let s1 = Aset.add (A.Leaf "s1") Aset.empty

let n1 = A.Node s1

let s2 = Aset.add n1 Aset.empty
let s2' = Aset.add n1 Aset.empty

let n2 = A.Node s2
let n2' = A.Node s2'

let () =
  Printf.printf "compare n2 n2' = %d\n%!" (A.compare n2 n2')
