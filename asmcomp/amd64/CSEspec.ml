(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CSE for the AMD64 *)

open Arch
open CSEgen

let class_of_operation spec =
  match spec with
  | Ilea _ | Isextend32 | Izextend32 -> Some Op_pure
  | Istore_int(_, _, is_asg) -> Some (Op_store is_asg)
  | Ioffset_loc(_, _) -> Some (Op_store true)
  | Ifloatarithmem _ | Ifloatsqrtf _ -> Some Op_load
  | Ibswap _ | Isqrtf -> None

let is_cheap_operation _ = None
