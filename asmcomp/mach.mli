(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include module type of Mach_type.Make(Arch)

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
