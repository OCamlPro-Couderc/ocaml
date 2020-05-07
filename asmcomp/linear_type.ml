
(* Transformation of Mach code into a list of pseudo-instructions. *)

module type T = sig

  module Arch : Arch_type.T

  type label = Cmm.label

  type instruction =
    { mutable desc: instruction_desc;
      mutable next: instruction;
      arg: Reg.t array;
      res: Reg.t array;
      dbg: Debuginfo.t;
      live: Reg.Set.t }

  and instruction_desc =
    | Lprologue
    | Lend
    | Lop of Mach_type.Make(Arch).operation
    | Lreloadretaddr
    | Lreturn
    | Llabel of label
    | Lbranch of label
    | Lcondbranch of Mach_type.Make(Arch).test * label
    | Lcondbranch3 of label option * label option * label option
    | Lswitch of label array
    | Lentertrap
    | Ladjust_trap_depth of { delta_traps : int; }
    | Lpushtrap of { lbl_handler : label; }
    | Lpoptrap
    | Lraise of Lambda.raise_kind

  type fundecl =
    { fun_name: Backend_sym.t;
      fun_body: instruction;
      fun_fast: bool;
      fun_dbg : Debuginfo.t;
      fun_spacetime_shape : Mach_type.Make(Arch).spacetime_shape option;
      fun_tailrec_entry_point_label : label;
      fun_contains_calls: bool;
      fun_num_stack_slots: int array;
      fun_frame_required: bool;
      fun_prologue_required: bool;
    }

end

module Make(Arch: Arch_type.T) : T with module Arch := Arch = struct

  type label = Cmm.label

  type instruction =
    { mutable desc: instruction_desc;
      mutable next: instruction;
      arg: Reg.t array;
      res: Reg.t array;
      dbg: Debuginfo.t;
      live: Reg.Set.t }

  and instruction_desc =
    | Lprologue
    | Lend
    | Lop of Mach_type.Make(Arch).operation
    | Lreloadretaddr
    | Lreturn
    | Llabel of label
    | Lbranch of label
    | Lcondbranch of Mach_type.Make(Arch).test * label
    | Lcondbranch3 of label option * label option * label option
    | Lswitch of label array
    | Lentertrap
    | Ladjust_trap_depth of { delta_traps : int; }
    | Lpushtrap of { lbl_handler : label; }
    | Lpoptrap
    | Lraise of Lambda.raise_kind

  type fundecl =
    { fun_name: Backend_sym.t;
      fun_body: instruction;
      fun_fast: bool;
      fun_dbg : Debuginfo.t;
      fun_spacetime_shape : Mach_type.Make(Arch).spacetime_shape option;
      fun_tailrec_entry_point_label : label;
      fun_contains_calls: bool;
      fun_num_stack_slots: int array;
      fun_frame_required: bool;
      fun_prologue_required: bool;
    }

end

module type S = sig

  module Arch : Arch_type.T

  include module type of Make(Arch)

  val has_fallthrough :  instruction_desc -> bool
  val end_instr: instruction
  val instr_cons:
    instruction_desc -> Reg.t array -> Reg.t array -> instruction -> instruction
  val invert_test: Mach_type.Make(Arch).test -> Mach_type.Make(Arch).test

end
