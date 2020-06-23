module type Backend = functor () -> Backend_parameter.S

let select_arch arch : (module Backend) =
  match arch with
    "amd64" -> (module Amd64)
  | "arm" -> (module Arm)
  | "arm64" -> (module Arm64)
  | "i386" -> (module I386)
  | "power" -> (module Power)
  | "s390x" -> (module S390x)
  | _ -> assert false

module Current_arch = (val (select_arch Config.architecture))
module Arch = Current_arch()

module Instance = Backend_maker(Arch)
