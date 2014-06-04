
type 'a array = 'a List.t

let print_array () =
  let open List in
  print_list ();
  print_endline "From array"
