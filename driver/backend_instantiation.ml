let select_arch arch : (module Backend_parameter.S) =
  match arch with
    "amd64" -> (module Amd64)
  | "arm" -> (module Arm)
  | "arm64" -> (module Arm64)
  | "i386" -> (module I386)
  | _ -> assert false

module Arch = (val (select_arch Config.architecture))

module Instance = Backend_maker(Amd64)
