module Arch : sig

  val command_line_options : (string * Arg.spec * string) list

  type addressing_mode

  type specific_operation

  val size_int: int
  val size_float: int

  val size_addr: int

  val division_crashes_on_overflow: bool

  val big_endian: bool
  val allow_unaligned_access: bool

  val print_addressing:
    (Format.formatter -> Reg.t -> unit) ->
    addressing_mode -> Format.formatter -> Reg.t array -> unit

  val print_specific_operation:
    (Format.formatter -> Reg.t -> unit) ->
    specific_operation -> Format.formatter -> Reg.t array -> unit

  val offset_addressing : addressing_mode -> int -> addressing_mode

  val identity_addressing : addressing_mode

  val spacetime_node_hole_pointer_is_live_before: specific_operation -> bool

end

module Proc : sig
  (* Processor descriptions *)

  (* Instruction selection *)
  val word_addressed: bool

  (* Registers available for register allocation *)
  val num_register_classes: int
  val register_class: Reg.t -> int
  val num_available_registers: int array
  val first_available_register: int array
  val register_name: int -> string
  val phys_reg: int -> Reg.t
  val rotate_registers: bool

  (* Calling conventions *)
  val loc_arguments: Reg.t array -> Reg.t array * int
  val loc_results: Reg.t array -> Reg.t array
  val loc_parameters: Reg.t array -> Reg.t array
  (* For argument number [n] split across multiple registers, the target-specific
     implementation of [loc_external_arguments] must return [regs] such that
     [regs.(n).(0)] is to hold the part of the value at the lowest address.
     (All that matters for the input to [loc_external_arguments] is the pattern
     of lengths and register types of the various supplied arrays.) *)
  val loc_external_arguments: Reg.t array array -> Reg.t array array * int
  val loc_external_results: Reg.t array -> Reg.t array
  val loc_exn_bucket: Reg.t
  val loc_spacetime_node_hole: Reg.t

  (* The maximum number of arguments of an OCaml to OCaml function call for
     which it is guaranteed there will be no arguments passed on the stack.
     (Above this limit, tail call optimization may be disabled.)
     N.B. The values for this parameter in the backends currently assume
     that no unboxed floats are passed using the OCaml calling conventions.
  *)
  val max_arguments_for_tailcalls : int

  (* Maximal register pressures for pre-spilling *)
  val safe_register_pressure: Mach_type.Make(Arch).operation -> int
  val max_register_pressure: Mach_type.Make(Arch).operation -> int array

  (* Registers destroyed by operations *)
  val destroyed_at_oper: Mach_type.Make(Arch).instruction_desc -> Reg.t array
  val destroyed_at_raise: Reg.t array
  val destroyed_at_reloadretaddr : Reg.t array

  (* Volatile registers: those that change value when read *)
  val regs_are_volatile: Reg.t array -> bool

  (* Pure operations *)
  val op_is_pure: Mach_type.Make(Arch).operation -> bool

  (* Info for laying out the stack frame *)
  val frame_required : Mach_type.Make(Arch).fundecl -> bool

  (* Function prologues *)
  val prologue_required : Mach_type.Make(Arch).fundecl -> bool

  (** For a given register class, the DWARF register numbering for that class.
      Given an allocated register with location [Reg n] and class [reg_class], the
      returned array contains the corresponding DWARF register number at index
      [n - first_available_register.(reg_class)]. *)
  val dwarf_register_numbers : reg_class:int -> int array

  (** The DWARF register number corresponding to the stack pointer. *)
  val stack_ptr_dwarf_register_number : int

  (* Calling the assembler *)
  val assemble_file: string -> string -> int

  (* Called before translating a fundecl. *)
  val init : unit -> unit

end

module CSEspec : sig

  val class_of_operation: Arch.specific_operation -> CSEgen.op_class option

  val is_cheap_operation: Arch.specific_operation -> bool option

end

module Selection : sig
  (* Selection of pseudo-instructions, assignment of pseudo-registers,
     sequentialization. *)

  val fundecl: Cmm.fundecl -> Mach_type.Make(Arch).fundecl

end

module Reload : sig
  (* Insert load/stores for pseudoregs that got assigned to stack locations. *)

  val fundecl:
    Mach_type.Make(Arch).fundecl -> int array ->
    Mach_type.Make(Arch).fundecl * bool

end

module Scheduling : sig
  (* Instruction scheduling *)

  val fundecl: Linear_type.Make(Arch).fundecl -> Linear_type.Make(Arch).fundecl

end

module Emit : sig
  (* Generation of assembly code *)

  val fundecl: Linear_type.Make(Arch).fundecl -> unit
  val data: Cmm.data_item list -> unit
  val begin_assembly: Backend_compilation_unit.t -> unit
  val end_assembly: Backend_compilation_unit.t -> unit

end
