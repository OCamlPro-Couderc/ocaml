let select_arch arch : (module Backend_parameter.S) =
  match arch with
    "amd64" -> (module Amd64)
  | _ -> assert false

module Arch = (val (select_arch Config.architecture))

module Instance = Backend_maker(Amd64)
