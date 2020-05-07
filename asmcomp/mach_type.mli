(** Representation of machine code by sequences of pseudoinstructions *)
(** N.B. Backends vary in their treatment of call gc and checkbound
    points.  If the positioning of any labels associated with these is
    important for some new feature in the compiler, the relevant backends'
    behaviour should be checked. *)

module type T = sig

  module Arch : Arch_type.T

  type label = Cmm.label

  type integer_comparison =
      Isigned of Cmm.integer_comparison
    | Iunsigned of Cmm.integer_comparison

  type integer_operation =
      Iadd | Isub | Imul | Imulh | Idiv | Imod
    | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
    | Icomp of integer_comparison
    | Icheckbound of { label_after_error : label option;
                       spacetime_index : int; }

  type float_comparison = Cmm.float_comparison

  type test =
      Itruetest
    | Ifalsetest
    | Iinttest of integer_comparison
    | Iinttest_imm of integer_comparison * int
    | Ifloattest of float_comparison
    | Ioddtest
    | Ieventest

  type operation =
      Imove
    | Ispill
    | Ireload
    | Iconst_int of nativeint
    | Iconst_float of int64
    | Iconst_symbol of string
    | Icall_ind of { label_after : label; }
    | Icall_imm of { func : string; label_after : label; }
    | Itailcall_ind of { label_after : label; }
    | Itailcall_imm of { func : string; label_after : label; }
    | Iextcall of { func : string; alloc : bool; label_after : label; }
    | Istackoffset of int
    | Iload of Cmm.memory_chunk * Arch.addressing_mode
    | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
    | Ialloc of { bytes : int; label_after_call_gc : label option;
                  dbginfo : Debuginfo.alloc_dbginfo; spacetime_index : int; }
    | Iintop of integer_operation
    | Iintop_imm of integer_operation * int
    | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
    | Ifloatofint | Iintoffloat
    | Ispecific of Arch.specific_operation
    | Iname_for_debugger of { ident : Backend_var.t; which_parameter : int option;
                              provenance : unit option; is_assignment : bool; }

  type instruction =
    { desc: instruction_desc;
      next: instruction;
      arg: Reg.t array;
      res: Reg.t array;
      dbg: Debuginfo.t;
      mutable live: Reg.Set.t;
      mutable available_before: Reg_availability_set.t;
      mutable available_across: Reg_availability_set.t option;
    }

  and instruction_desc =
      Iend
    | Iop of operation
    | Ireturn
    | Iifthenelse of test * instruction * instruction
    | Iswitch of int array * instruction array
    | Icatch of Cmm.rec_flag * (int * instruction) list * instruction
    | Iexit of int
    | Itrywith of instruction * instruction
    | Iraise of Lambda.raise_kind

  type spacetime_part_of_shape =
    | Direct_call_point of { callee : string; }
    | Indirect_call_point
    | Allocation_point

  type spacetime_shape = (spacetime_part_of_shape * Cmm.label) list

  type fundecl =
    { fun_name: string;
      fun_args: Reg.t array;
      fun_body: instruction;
      fun_codegen_options : Cmm.codegen_option list;
      fun_dbg : Debuginfo.t;
      fun_spacetime_shape : spacetime_shape option;
      fun_num_stack_slots: int array;
      fun_contains_calls: bool;
    }

end

module Make(Arch: Arch_type.T) : T with module Arch := Arch

module type S = sig

  module Arch : Arch_type.T

  include module type of Make(Arch)

  val dummy_instr: instruction
  val end_instr: unit -> instruction
  val instr_cons:
    instruction_desc -> Reg.t array -> Reg.t array -> instruction ->
    instruction
  val instr_cons_debug:
    instruction_desc -> Reg.t array -> Reg.t array -> Debuginfo.t ->
    instruction -> instruction
  val instr_iter: (instruction -> unit) -> instruction -> unit

  val spacetime_node_hole_pointer_is_live_before : instruction -> bool

  val operation_can_raise : operation -> bool

end
