module type S = sig

  module Arch : Arch_type.T

  class reload_generic : object
    method reload_operation :
      Mach_type.Make(Arch).operation -> Reg.t array -> Reg.t array -> Reg.t array * Reg.t array
    method reload_test : Mach_type.Make(Arch).test -> Reg.t array -> Reg.t array
    (* Can be overridden to reflect instructions that can operate
       directly on stack locations *)
    method makereg : Reg.t -> Reg.t
    (* Can be overridden to avoid creating new registers of some class
       (i.e. if all "registers" of that class are actually on stack) *)
    method fundecl : Mach_type.Make(Arch).fundecl -> int array -> Mach_type.Make(Arch).fundecl * bool
    (* The entry point *)
  end
end
