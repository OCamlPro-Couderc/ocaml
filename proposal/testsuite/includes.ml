with Y.(Included as M)

include M

let _ =
  let v = create 24 in
  Format.printf "%d %d@." v value
