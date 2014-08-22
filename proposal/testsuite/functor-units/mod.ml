module X = Intf

let print t = Format.printf "Value: %s@." (X.to_string t)

let print_int i = Builder.create_value i |> print
