(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open Lambda

val transl_implementation: string -> structure * module_coercion -> lambda
val transl_store_phrases: string -> structure -> int * lambda
val transl_store_implementation:
      string -> structure * module_coercion -> int * lambda
val transl_toplevel_definition: structure -> lambda
val transl_package:
      Ident.t option list -> Ident.t -> module_coercion -> lambda
val transl_store_package:
      Ident.t option list -> Ident.t -> module_coercion -> int * lambda
(* let transl_applied_unit funit target_id instantiation *)
val transl_applied_unit: Ident.t -> Ident.t ->
  (Ident.t * Ident.t) list -> module_coercion list -> lambda
val transl_store_apply: int -> Ident.t -> Ident.t ->
  (Ident.t * Ident.t) list -> module_coercion list -> lambda

val toplevel_name: Ident.t -> string
val nat_toplevel_name: Ident.t -> Ident.t * int

val primitive_declarations: Primitive.description list ref

type error =
  Circular_dependency of Ident.t

exception Error of Location.t * error

val report_error: Format.formatter -> error -> unit

val reset: unit -> unit
