module X = Intf

let print t = Format.printf "Value: %s@." (X.to_string t)
