in namespace X

type t = Int of int

let create i = Int i
let to_string (Int i) = string_of_int i
let eval = function Int i -> i
