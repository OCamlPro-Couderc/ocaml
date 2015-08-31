(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The batch compiler *)

open Misc
open Format
open Typedtree
open Compenv

(* Compile a .mli file *)

(* Keep in sync with the copy in optcompile.ml *)

let tool_name = "ocamlc"

let interface ppf sourcefile outputprefix =
  if !Clflags.ns_debug then
    Format.printf "Compile.interface@.";
  Compmisc.init_path false;
  (* let outputprefix = *)
  (*   if !Clflags.root <> "" then Filename.basename outputprefix *)
  (*   else outputprefix in *)
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  let interf = Pparse.parse_interface ~tool_name ppf sourcefile in
  (* let Parsetree.Pinterf (_, ast) = interf in *)
  if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface interf;
  if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.interface interf;
  let tsg, ns = Typemod.type_interface modulename initial_env interf in
  if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
  let sg = tsg.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env initial_env (fun () ->
        fprintf std_formatter "%a@."
          Printtyp.signature (Typemod.simplify_signature sg));
  ignore (Includemod.signatures initial_env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  if not !Clflags.print_types then begin
    let sg = Env.save_signature ns sg modulename (outputprefix ^ ".cmi") in
    Typemod.save_signature modulename tsg outputprefix sourcefile
      initial_env sg ;
  end

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

(* Pierrick: The opening of the objfile is done directly in emitcode. It is
   necessary since we need the file to be parsed in order to write the file
   in the correct directory according to the namespace it belongs. *)

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let outputprefix =
    if !Clflags.root <> "" then Filename.basename outputprefix
    else outputprefix in
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  try
    let (typedtree, coercion) =
      Pparse.parse_implementation ~tool_name ppf sourcefile
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.implementation
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
        Printtyped.implementation_with_coercion
    in
    if !Clflags.print_types then begin
      Warnings.check_fatal ();
      Stypes.dump (Some (outputprefix ^ ".annot"))
    end else begin
      let bytecode =
        (typedtree, coercion)
        ++ Translmod.transl_implementation modulename
        ++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
        ++ Simplif.simplify_lambda
        ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
        ++ Bytegen.compile_implementation modulename
        ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
      in
      let objfile = outputprefix ^ ".cmo" in
      try
        bytecode
        ++ Emitcode.to_file objfile modulename;
        Warnings.check_fatal ();
        Stypes.dump (Some (outputprefix ^ ".annot"))
      with x ->
        remove_file objfile;
        raise x
    end
  with x ->
    Stypes.dump (Some (outputprefix ^ ".annot"));
    raise x

let c_file name =
  Location.input_name := name;
  if Ccomp.compile_file name <> 0 then exit 2
