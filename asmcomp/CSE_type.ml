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

(* Classification of operations *)

type op_class =
  | Op_pure           (* pure arithmetic, produce one or several result *)
  | Op_checkbound     (* checkbound-style: no result, can raise an exn *)
  | Op_load           (* memory load *)
  | Op_store of bool  (* memory store, false = init, true = assign *)
  | Op_other   (* anything else that does not allocate nor store in memory *)

module type S = sig

  module Arch : Arch_type.T

  class cse_generic : object
    (* The following methods can be overridden to handle processor-specific
       operations. *)

    method class_of_operation: Mach_type.Make(Arch).operation -> op_class

    method is_cheap_operation: Mach_type.Make(Arch).operation -> bool
    (* Operations that are so cheap that it isn't worth factoring them. *)

    (* The following method is the entry point and should not be overridden *)
    method fundecl: Mach_type.Make(Arch).fundecl -> Mach_type.Make(Arch).fundecl

  end

end
