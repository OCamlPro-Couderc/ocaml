module type S = sig

  module Arch : Arch_type.T

  class virtual scheduler_generic : object
    (* Can be overridden by processor description *)
    method virtual oper_issue_cycles : Mach_type.Make(Arch).operation -> int
    (* Number of cycles needed to issue the given operation *)
    method virtual oper_latency : Mach_type.Make(Arch).operation -> int
    (* Number of cycles needed to complete the given operation *)
    method reload_retaddr_issue_cycles : int
    (* Number of cycles needed to issue a Lreloadretaddr operation *)
    method reload_retaddr_latency : int
    (* Number of cycles needed to complete a Lreloadretaddr operation *)
    method oper_in_basic_block : Mach_type.Make(Arch).operation -> bool
    (* Says whether the given operation terminates a basic block *)
    method is_store : Mach_type.Make(Arch).operation -> bool
    (* Says whether the given operation is a memory store *)
    method is_load : Mach_type.Make(Arch).operation -> bool
    (* Says whether the given operation is a memory load *)
    method is_checkbound : Mach_type.Make(Arch).operation -> bool
    (* Says whether the given operation is a checkbound *)
    (* Entry point *)
    method schedule_fundecl : Linear_type.Make(Arch).fundecl -> Linear_type.Make(Arch).fundecl
  end

end
