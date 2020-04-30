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

open Arch_specific
include Mach_type.Make(Arch_specific.Arch)

let rec dummy_instr =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let end_instr () =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let instr_cons_debug d a r dbg n =
  { desc = d; next = n; arg = a; res = r; dbg = dbg; live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let rec instr_iter f i =
  match i.desc with
    Iend -> ()
  | _ ->
      f i;
      match i.desc with
        Iend -> ()
      | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) -> ()
      | Iifthenelse(_tst, ifso, ifnot) ->
          instr_iter f ifso; instr_iter f ifnot; instr_iter f i.next
      | Iswitch(_index, cases) ->
          for i = 0 to Array.length cases - 1 do
            instr_iter f cases.(i)
          done;
          instr_iter f i.next
      | Icatch(_, handlers, body) ->
          instr_iter f body;
          List.iter (fun (_n, handler) -> instr_iter f handler) handlers;
          instr_iter f i.next
      | Iexit _ -> ()
      | Itrywith(body, handler) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iraise _ -> ()
      | _ ->
          instr_iter f i.next

let spacetime_node_hole_pointer_is_live_before insn =
  match insn.desc with
  | Iop op ->
    begin match op with
    | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _ -> true
    | Iextcall { alloc; } -> alloc
    | Ialloc _ ->
      (* Allocations are special: the call to [caml_call_gc] requires some
         instrumentation code immediately prior, but this is not inserted until
         the emitter (since the call is not visible prior to that in any IR).
         As such, none of the Mach / Linearize analyses will ever see that
         we use the node hole pointer for these, and we do not need to say
         that it is live at such points. *)
      false
    | Iintop op | Iintop_imm (op, _) ->
      begin match op with
      | Icheckbound _
        (* [Icheckbound] doesn't need to return [true] for the same reason as
           [Ialloc]. *)
      | Iadd | Isub | Imul | Imulh | Idiv | Imod
      | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
      | Icomp _ -> false
      end
    | Ispecific specific_op ->
      Arch.spacetime_node_hole_pointer_is_live_before specific_op
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _
    | Iconst_symbol _ | Istackoffset _ | Iload _ | Istore _
    | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
    | Ifloatofint | Iintoffloat
    | Iname_for_debugger _ -> false
    end
  | Iend | Ireturn | Iifthenelse _ | Iswitch _ | Icatch _
  | Iexit _ | Itrywith _ | Iraise _ -> false

let operation_can_raise op =
  match op with
  | Icall_ind _ | Icall_imm _ | Iextcall _
  | Iintop (Icheckbound _) | Iintop_imm (Icheckbound _, _)
  | Ialloc _ -> true
  | _ -> false
