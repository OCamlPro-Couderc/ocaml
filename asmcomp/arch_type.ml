module type T = sig

  type specific_operation
  type addressing_mode

end

module type S = sig

  include T

  val command_line_options : (string * Arg.spec * string) list

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
