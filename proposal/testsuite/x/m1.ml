in namespace X;;

type t = { value : int }

let create v = { value = v }

let to_string v = string_of_int v.value
