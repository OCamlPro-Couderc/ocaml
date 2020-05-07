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

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

include Selector.S with module Arch := Arch_specific.Arch

val env_add
   : Backend_var.With_provenance.t
  -> Reg.t array
  -> environment
  -> environment

val env_find : Backend_var.t -> environment -> Reg.t array

val size_expr : environment -> Cmm.expression -> int

val reset : unit -> unit
