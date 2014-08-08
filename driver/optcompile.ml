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
open Config
open Format
open Typedtree
open Compenv

(* Compile a .mli file *)

(* Keep in sync with the copy in compile.ml *)

let interface ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  (* let ast = Pparse.parse_interface ppf sourcefile in *)
  let interf = Pparse.parse_interface ppf sourcefile in
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
let (+++) (x, y) f = (x, f y)

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path true;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  Compilenv.reset ?packname:!Clflags.for_package modulename;

  (* let outputprefix = *)
  (*   if !Clflags.root <> "" then Filename.basename outputprefix *)
  (*   else outputprefix in *)
  let cmxfile = outputprefix ^ ".cmx" in
  let objfile = outputprefix ^ ext_obj in
  let comp ast =
    if !Clflags.print_types
    then
      ast
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.implementation
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
          Printtyped.implementation_with_coercion
      ++ (fun _ -> ())
    else begin
      ast
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.implementation
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
          Printtyped.implementation_with_coercion
      ++ Translmod.transl_store_implementation modulename
      +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
      +++ Simplif.simplify_lambda
      +++ print_if ppf Clflags.dump_lambda Printlambda.lambda
      ++ Asmgen.compile_implementation outputprefix ppf;
      Compilenv.save_unit_info cmxfile;
    end;
    Warnings.check_fatal ();
    Stypes.dump (Some (outputprefix ^ ".annot"))
  in
  try comp (Pparse.parse_implementation ppf sourcefile)
  with x ->
    let dir = Filename.concat !Clflags.root @@
    Env.longident_to_filepath (Env.get_namespace_unit()) in
    Stypes.dump (Some (outputprefix ^ ".annot"));
    remove_file (Filename.concat dir objfile);
    remove_file (Filename.concat dir cmxfile);
    raise x

let c_file name =
  if Ccomp.compile_file name <> 0 then exit 2
