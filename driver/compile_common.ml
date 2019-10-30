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

open Misc
open Compenv

type info = {
  source_file : string;
  module_name : Compilation_unit.Name.t;
  output_prefix : string;
  for_pack_prefix : Compilation_unit.Prefix.t;
  env : Env.t option;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}

let cmx i = i.output_prefix ^ ".cmx"
let obj i = i.output_prefix ^ Config.ext_obj
let cmo i = i.output_prefix ^ ".cmo"
let annot i = i.output_prefix ^ ".annot"

let init_env module_name for_pack_prefix =
  Compmisc.init_path ();
  Persistent_env.Current_unit.set ~prefix:for_pack_prefix module_name;
  Compmisc.initial_env ()

let raw_with_info ~gen_env ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  let module_name = module_of_filename source_file output_prefix in
  let for_pack_prefix =
    Compilation_unit.Prefix.parse_for_pack !Clflags.for_package in
  let env =
    if gen_env then Some (init_env module_name for_pack_prefix)
    else None in
  let dump_file = String.concat "." [output_prefix; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file @@ fun ppf_dump ->
  k {
    module_name;
    output_prefix;
    for_pack_prefix;
    env;
    source_file;
    ppf_dump;
    tool_name;
    native;
  }

let safe_with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  raw_with_info
    ~gen_env:false ~native ~tool_name ~source_file ~output_prefix ~dump_ext k

let with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  raw_with_info
    ~gen_env:true ~native ~tool_name ~source_file ~output_prefix ~dump_ext k

(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.signature

let typecheck_intf info ast =
  Profile.(record_call typing) @@ fun () ->
  let env = match info.env with Some env -> env | None -> assert false in
  let tsg =
    ast
    |> Typemod.type_interface env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature info.source_file)
          sg);
  ignore (Includemod.signatures env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tsg

let typecheck_rec_intf infos asts =
  Profile.(record_call typing) @@ fun () ->
  (* There is no reason that the initial environment is different for each mli *)
  let ppf_dump, prefix = match infos with
      [] -> assert false
    | i :: _ -> i.ppf_dump, i.for_pack_prefix
  in
  let env = init_env "*recmod*" prefix in
  let tsgs, recenv =
    asts
    |> List.map2 (fun info ast ->
        let unit =
          Compilation_unit.create
            ~for_pack_prefix:info.for_pack_prefix info.module_name in
        unit, ast, Location.in_file info.source_file) infos
    |> Typemod.type_rec_interfaces env
    |> fun (tsgs, recenv) ->
    List.map2 (fun tsg info ->
        print_if ppf_dump Clflags.dump_typedtree Printtyped.interface tsg, info)
      tsgs infos,
    recenv
  in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false recenv (fun () ->
        List.fold_left (fun fmt (tsg, info) ->
            let sg = tsg.Typedtree.sig_type in
            Format.fprintf fmt  "%a@."
              (Printtyp.printed_signature info.source_file)
              sg;
            fmt)
          Format.std_formatter
          tsgs
      |> ignore);
  List.iter (fun (tsg, _) ->
      let sg = tsg.Typedtree.sig_type in
      ignore (Includemod.signatures recenv sg sg)) tsgs;
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  fst @@ List.split tsgs, recenv

let emit_signature info ast tsg =
  let sg =
    let alerts = Builtin_attributes.alerts_of_sig ast in
    Env.save_signature ~alerts tsg.Typedtree.sig_type
      info.module_name (info.output_prefix ^ ".cmi")
  in
  let env = match info.env with Some env -> env | None -> assert false in
  Typemod.save_signature info.module_name tsg
    info.output_prefix info.source_file env sg

let interface info =
  Profile.record_call info.source_file @@ fun () ->
  let ast = parse_intf info in
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let tsg = typecheck_intf info ast in
    if not !Clflags.print_types then begin
      emit_signature info ast tsg
    end
  end

let rec_interfaces infos =
  Profile.record_call "rec_interfaces" @@ fun () ->
  let asts = List.map parse_intf infos in
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let tsgs, recenv = typecheck_rec_intf infos asts in
    if not !Clflags.print_types then begin
      Misc.iter3 (fun info ->
          emit_signature { info with env = Some recenv })
        infos asts tsgs
    end
  end

(** Frontend for a .ml file *)

let parse_impl i =
  Pparse.parse_implementation ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.structure

let typecheck_impl i parsetree =
  let env = match i.env with Some env -> env | None -> assert false in
  let always () = Stypes.dump (Some (annot i)) in
  Misc.try_finally ~always (fun () ->
    parsetree
    |> Profile.(record typing)
      (Typemod.type_implementation
         i.source_file i.output_prefix i.module_name env)
    |> print_if i.ppf_dump Clflags.dump_typedtree
      Printtyped.implementation_with_coercion
  )

let implementation info ~backend =
  Profile.record_call info.source_file @@ fun () ->
  let exceptionally () =
    let sufs = if info.native then [ cmx; obj ] else [ cmo ] in
    List.iter (fun suf -> remove_file (suf info)) sufs;
  in
  Misc.try_finally ?always:None ~exceptionally (fun () ->
    let parsed = parse_impl info in
    if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
      let typed = typecheck_impl info parsed in
      if Clflags.(should_stop_after Compiler_pass.Typing) then () else begin
        backend info typed
      end;
    end;
    Warnings.check_fatal ();
  )
