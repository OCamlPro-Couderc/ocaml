in namespace Y

type t = { value : int }

let create i = { value = i }
let to_string v = string_of_int v.value
