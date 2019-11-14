(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Compile_common

let tool_name = "ocamlc"

let with_info =
  Compile_common.with_info ~native:false ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

let with_infos ~source_files ~output_prefixes ~dump_ext k =
  (* Temporary hack *)
  List.map2 (fun source_file output_prefix ->
      safe_with_info ~native:false ~tool_name ~source_file ~output_prefix ~dump_ext
        (fun info -> info))
    source_files output_prefixes
  |> k

let rec_interfaces ~source_files ~output_prefixes =
  with_infos ~source_files ~output_prefixes ~dump_ext:"cmi" @@ fun infos ->
  Compile_common.rec_interfaces infos

(** Bytecode compilation backend for .ml files. *)

let to_bytecode i (typedtree, coercion) =
  (typedtree, coercion)
  |> Profile.(record transl)
    (Translmod.transl_implementation
       (Compilation_unit.Name.to_string i.module_name))
  |> Profile.(record ~accumulate:true generate)
    (fun { Lambda.code = lambda; required_globals; recursive } ->
       lambda
       |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
       |> Simplif.simplify_lambda
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
       |> Bytegen.compile_implementation i.module_name
       |> print_if i.ppf_dump Clflags.dump_instr Printinstr.instrlist
       |> fun bytecode -> bytecode, required_globals, recursive
    )

let emit_bytecode i (bytecode, required_globals, recursive) =
  let cmofile = cmo i in
  let oc = open_out_bin cmofile in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file cmofile)
    (fun () ->
       bytecode
       |> Profile.(record ~accumulate:true generate)
         (Emitcode.to_file oc i.module_name cmofile ~required_globals recursive)
    )

let implementation ~source_file ~output_prefix =
  let backend info typed =
    let bytecode = to_bytecode info typed in
    emit_bytecode info bytecode
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" @@ fun info ->
  Compile_common.implementation info ~backend
