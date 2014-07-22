(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Module dependencies. *)

module StringLid : Set.S with type elt = string * Longident.t option

val free_structure_names : StringLid.t ref

(* In case a module cannot be found in the loadpath, it allows to retrieve it
   into a namespace whose modules were added by a wildcard *)
val possible_wildcard : Longident.t list ref

val add_use_file : StringLid.t -> Parsetree.toplevel_phrase list -> unit

val add_signature : StringLid.t -> Parsetree.signature -> unit

val add_interface : StringLid.t -> Parsetree.interface -> unit

val add_implementation : StringLid.t -> Parsetree.structure -> unit
