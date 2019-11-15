(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*         Mehdi Dogguy, PPS laboratory, University Paris Diderot         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2010 Mehdi Dogguy                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Dump info on .cmi, .cmo, .cmx, .cma, .cmxa, .cmxs files
   and on bytecode executables. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Printf
open Misc
open Cmo_format

module CU = Compilation_unit
module LI = Cmxa_format.Library_info
module DH = Cmxs_format.Dynheader_info
module DU = Cmxs_format.Dynunit_info
module UI = Cmx_format.Unit_info
module UIL = Cmx_format.Unit_info_link_time

module Int = Numbers.Int

(* Command line options to prevent printing approximation,
   function code and CRC
 *)
let no_approx = ref false
let no_code = ref false
let no_crc = ref false
let full_path = ref false

module Magic_number = Misc.Magic_number

let input_stringlist ic len =
  let get_string_list sect len =
    let rec fold s e acc =
      if e != len then
        if sect.[e] = '\000' then
          fold (e+1) (e+1) (String.sub sect s (e-s) :: acc)
        else fold s (e+1) acc
      else acc
    in fold 0 0 []
  in
  let sect = really_input_string ic len in
  get_string_list sect len

let dummy_crc = String.make 32 '-'
let null_crc = String.make 32 '0'

let string_of_crc crc = if !no_crc then null_crc else Digest.to_hex crc

let print_name_crc (unit, crco) =
  let crc =
    match crco with
      None -> dummy_crc
    | Some crc -> string_of_crc crc
  in
  if not !full_path then
    printf "\t%s\t%s\n" crc CU.(Name.to_string (name unit))
  else
    printf "\t%s\t%s\n" crc
      (Format.asprintf "%a" CU.print_full_path unit)

let print_line name =
  printf "\t%s\n" name

let print_required_global id =
  printf "\t%s\n" (Ident.name id)

let print_list print_elt ppf elts =
  List.iter (fun elt -> fprintf ppf "%a ;\n\t" print_elt elt) elts

let rec print_shape ppf = function
    Lambda.Function -> fprintf ppf "Function"
  | Lambda.Lazy -> fprintf ppf "Lazy"
  | Lambda.Class -> fprintf ppf "Class"
  | Lambda.Module l ->
      fprintf ppf "Module [%a]"
        (print_list print_shape) l

let print_shape_result ppf = function
    Ok shape -> fprintf ppf "Shape: %a" print_shape shape
  | Error _ -> fprintf ppf "Shape: Error"

let print_free_var ppf id = fprintf ppf "%s" (Ident.name id)

let print_rec_infos infos =
  printf "Recursive: %a\n"
    (fun ppf infos ->
       match infos with
         None -> fprintf ppf "NO"
       | Some (shape, fvs) ->
           fprintf ppf "%a\nFreevars:%a\n"
             print_shape_result shape
             (print_list print_free_var) fvs) infos

let print_cmo_infos cu =
  printf "Unit name: %s\n" (CU.Name.to_string cu.cu_name);
  print_string "Interfaces imported:\n";
  List.iter print_name_crc cu.cu_imports;
  print_string "Required globals:\n";
  List.iter print_required_global cu.cu_required_globals;
  printf "Uses unsafe features: ";
  (match cu.cu_primitives with
    | [] -> printf "no\n"
    | l  ->
        printf "YES\n";
        printf "Primitives declared in this module:\n";
        List.iter print_line l);
  printf "Force link: %s\n" (if cu.cu_force_link then "YES" else "no");
  print_rec_infos cu.cu_rec_infos

let print_spaced_string s =
  printf " %s" s

let print_cma_infos (lib : Cmo_format.library) =
  printf "Force custom: %s\n" (if lib.lib_custom then "YES" else "no");
  printf "Extra C object files:";
  (* PR#4949: print in linking order *)
  List.iter print_spaced_string (List.rev lib.lib_ccobjs);
  printf "\nExtra C options:";
  List.iter print_spaced_string (List.rev lib.lib_ccopts);
  printf "\n";
  print_string "Extra dynamically-loaded libraries:";
  List.iter print_spaced_string (List.rev lib.lib_dllibs);
  printf "\n";
  List.iter print_cmo_infos lib.lib_units

let print_pers_flags =
  let open Cmi_format in
  function
  | Rectypes -> printf " -rectypes"
  | Alerts _ -> ()
  | Opaque -> printf " -opaque"
  | Unsafe_string -> printf " -unsafe-string"
  | Pack modnames -> printf " -for-pack %s" (CU.Prefix.to_string modnames)
  | Recursive intfs ->
      printf " -recursive %s"
        (Format.asprintf "%a"
           (Format.pp_print_list
              ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
              Compilation_unit.Name.print) intfs)

let print_cmi_infos name crcs flags =
  printf "Unit name: %s\n" (CU.Name.to_string name);
  printf "Interfaces imported:\n";
  List.iter print_name_crc crcs;
  printf "Compilation flags:\n";
  List.iter print_pers_flags flags;
  printf "\n"


let print_cmt_infos cmt =
  let open Cmt_format in
  printf "Cmt unit name: %s\n" (CU.Name.to_string cmt.cmt_modname);
  print_string "Cmt interfaces imported:\n";
  List.iter print_name_crc cmt.cmt_imports;
  printf "Source file: %s\n"
         (match cmt.cmt_sourcefile with None -> "(none)" | Some f -> f);
  printf "Compilation flags:";
  Array.iter print_spaced_string cmt.cmt_args;
  printf "\nLoad path:";
  List.iter print_spaced_string cmt.cmt_loadpath;
  printf "\n";
  printf "cmt interface digest: %s\n"
    (match cmt.cmt_interface_digest with
     | None -> ""
     | Some crc -> string_of_crc crc)

let print_name_crc_native print_name (name, crco) =
  let crc =
    match crco with
      None -> dummy_crc
    | Some crc -> string_of_crc crc
  in
  printf "\t%s\t%s\n" crc (Format.asprintf "%a" print_name name)

let print_general_infos name crc defines cmi cmx =
  printf "Name: %s\n" (Format.asprintf "%a" CU.Name.print (CU.name name));
  printf "CRC of implementation: %s\n" (string_of_crc crc);
  printf "Globals defined:\n";
  let defines =
    List.map (fun comp_unit ->
        CU.Name.to_string (CU.name comp_unit))
      defines
  in
  List.iter print_line defines;
  printf "Interfaces imported:\n";
  let cmi = CU.Map.bindings cmi in
  List.iter (print_name_crc_native CU.print) cmi;
  printf "Implementations imported:\n";
  let cmx = CU.Map.bindings cmx in
  List.iter (print_name_crc_native CU.print) cmx

let print_global_table table =
  printf "Globals defined:\n";
  Symtable.iter_global_map
    (fun id _ -> print_line (Ident.name id))
    table

let print_cmx_infos (ui, ui_link, crc) =
  print_general_infos (UI.unit ui) crc (UI.defines ui)
    (UI.imports_cmi ui) (UI.imports_cmx ui);
  begin match UI.export_info ui with
  | Closure approx ->
    if not !no_approx then begin
      printf "Clambda approximation:\n";
      Format.fprintf Format.std_formatter "  %a@." Printclambda.approx approx
    end else
      Format.printf "Clambda unit@.";
  | Flambda export ->
    if not !no_approx || not !no_code then
      printf "Flambda export information:\n"
    else
      printf "Flambda unit\n";
    if not !no_approx then begin
      let cu = UI.unit ui in
      Persistent_env.Current_unit.set_unit cu;
      let root_symbols =
        List.map (fun comp_unit -> Symbol.for_module_block comp_unit)
          (UI.defines ui)
      in
      Format.printf "approximations@ %a@.@."
        Export_info.print_approx (export, root_symbols)
    end;
    if not !no_code then
      Format.printf "functions@ %a@.@."
        Export_info.print_functions export
  end;
  let pr_funs _ fns =
    List.iter (fun arity -> printf " %d" arity) fns in
  printf "Currying functions:%a\n" pr_funs
    (Int.Set.elements (UIL.curry_fun ui_link));
  printf "Apply functions:%a\n" pr_funs
    (Int.Set.elements (UIL.apply_fun ui_link));
  printf "Send functions:%a\n" pr_funs
    (Int.Set.elements (UIL.send_fun ui_link));
  printf "Force link: %s\n" (if (UIL.force_link ui_link) then "YES" else "no")

let print_cmxa_infos lib =
  printf "Extra C object files:";
  List.iter print_spaced_string (List.rev (LI.ccobjs lib));
  printf "\nExtra C options:";
  List.iter print_spaced_string (List.rev (LI.ccopts lib));
  printf "\n";
  List.iter print_cmx_infos (LI.units lib)

let print_cmxs_infos header =
  List.iter
    (fun ui ->
       print_general_infos
         (DU.unit ui)
         (DU.crc ui)
         (DU.defines ui)
         (DU.imports_cmi ui)
         (DU.imports_cmx ui))
    (DH.units header)

let p_title title = printf "%s:\n" title

let p_section title = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print_name_crc l

let p_list title print = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print l

let dump_byte ic =
  Bytesections.read_toc ic;
  let toc = Bytesections.toc () in
  let toc = List.sort compare toc in
  List.iter
    (fun (section, _) ->
       try
         let len = Bytesections.seek_section ic section in
         if len > 0 then match section with
           | "CRCS" ->
               p_section
                 "Imported units"
                 (input_value ic : Compilation_unit.crcs)
           | "DLLS" ->
               p_list
                 "Used DLLs"
                 print_line
                 (input_stringlist ic len)
           | "DLPT" ->
               p_list
                 "Additional DLL paths"
                 print_line
                 (input_stringlist ic len)
           | "PRIM" ->
               p_list
                 "Primitives used"
                 print_line
                 (input_stringlist ic len)
           | "SYMB" ->
               print_global_table (input_value ic)
           | _ -> ()
       with _ -> ()
    )
    toc

let find_dyn_offset filename =
  let helper = Filename.concat Config.standard_library "objinfo_helper" in
  let tempfile = Filename.temp_file "objinfo" ".out" in
  match
    Fun.protect
      ~finally:(fun () -> remove_file tempfile)
      (fun () ->
         let rc = Sys.command (sprintf "%s %s > %s"
                                 (Filename.quote helper)
                                 (Filename.quote filename)
                                 tempfile) in
         if rc <> 0 then failwith "cannot read";
         let tc = Scanf.Scanning.from_file tempfile in
         Fun.protect
           ~finally:(fun () -> Scanf.Scanning.close_in tc)
           (fun () ->
              Scanf.bscanf tc "%Ld" (fun x -> x)))
  with
    | offset -> Some offset
    | exception (Failure _ | Sys_error _) -> None

let exit_err msg = print_endline msg; exit 2
let exit_errf fmt = Printf.ksprintf exit_err fmt

let exit_magic_msg msg =
  exit_errf
     "Wrong magic number:\n\
      this tool only supports object files produced by compiler version\n\
      \t%s\n\
      %s"
    Sys.ocaml_version msg

let exit_magic_error ~expected_kind err =
  exit_magic_msg Magic_number.(match err with
    | Parse_error err -> explain_parse_error expected_kind err
    | Unexpected_error err -> explain_unexpected_error err)

(* assume that 'ic' is already positioned at the right place
   depending on the format (usually right after the magic number,
   but Exec and Cmxs differ) *)
let dump_obj_by_kind filename ic obj_kind =
  let open Magic_number in
  match obj_kind with
    | Cmo ->
       let cu_pos = input_binary_int ic in
       seek_in ic cu_pos;
       let cu = (input_value ic : compilation_unit) in
       close_in ic;
       print_cmo_infos cu
    | Cma ->
       let toc_pos = input_binary_int ic in
       seek_in ic toc_pos;
       let toc = (input_value ic : library) in
       close_in ic;
       print_cma_infos toc
    | Cmi | Cmt ->
       close_in ic;
       let cmi, cmt = Cmt_format.read filename in
       begin match cmi with
         | None -> ()
         | Some cmi ->
             print_cmi_infos
               cmi.Cmi_format.cmi_name
               cmi.Cmi_format.cmi_crcs
               cmi.Cmi_format.cmi_flags
       end;
       begin match cmt with
         | None -> ()
         | Some cmt -> print_cmt_infos cmt
       end
    | Cmx _config ->
        let ui, ui_link, crc = Cmx_format.load ~filename in
        print_cmx_infos (ui, ui_link, crc)
    | Cmxa _config ->
       let li = Cmxa_format.load ~filename in
       close_in ic;
       print_cmxa_infos li
    | Exec ->
       (* no assumptions on [ic] position,
          [dump_byte] will seek at the right place *)
       dump_byte ic;
       close_in ic
    | Cmxs ->
       (* we assume we are at the offset of the dynamic information,
          as returned by [find_dyn_offset]. *)
       let header = (input_value ic : DH.t) in
       close_in ic;
       print_cmxs_infos header;
    | Ast_impl | Ast_intf ->
       exit_errf "The object file type %S \
                  is currently unsupported by this tool."
         (human_name_of_kind obj_kind)

let dump_obj filename =
  let open Magic_number in
  let dump_standard ic =
    match read_current_info ~expected_kind:None ic with
      | Error ((Unexpected_error _) as err) ->
         exit_magic_error ~expected_kind:None err
      | Ok { kind; version = _ } ->
         dump_obj_by_kind filename ic kind;
         Ok ()
      | Error (Parse_error head_error) ->
         Error head_error
  and dump_exec ic =
    let pos_trailer = in_channel_length ic - Magic_number.magic_length in
    let _ = seek_in ic pos_trailer in
    let expected_kind = Some Exec in
    match read_current_info ~expected_kind ic with
      | Error ((Unexpected_error _) as err) ->
         exit_magic_error ~expected_kind err
      | Ok _ ->
         dump_obj_by_kind filename ic Exec;
         Ok ()
      | Error (Parse_error _)  ->
         Error ()
  and dump_cmxs ic =
    flush stdout;
    match find_dyn_offset filename with
      | None ->
         exit_errf "Unable to read info on %s %s."
           (human_name_of_kind Cmxs) filename
      | Some offset ->
         LargeFile.seek_in ic offset;
         let header = (input_value ic : DH.t) in
         let expected_kind = Some Cmxs in
         match parse (DH.magic header) with
           | Error err ->
              exit_magic_error ~expected_kind (Parse_error err)
           | Ok info ->
         match check_current Cmxs info with
           | Error err ->
              exit_magic_error ~expected_kind (Unexpected_error err)
           | Ok () ->
         LargeFile.seek_in ic offset;
         dump_obj_by_kind filename ic Cmxs;
         ()
  in
  printf "File %s\n" filename;
  let ic = open_in_bin filename in
  match dump_standard ic with
    | Ok () -> ()
    | Error head_error ->
  match dump_exec ic with
    | Ok () -> ()
    | Error () ->
  if Filename.check_suffix filename ".cmxs"
  then dump_cmxs ic
  else exit_magic_error ~expected_kind:None (Parse_error head_error)

let arg_list = [
  "-full-path", Arg.Set full_path, "";
  "-no-approx", Arg.Set no_approx,
    " Do not print module approximation information";
  "-no-code", Arg.Set no_code,
    " Do not print code from exported flambda functions";
  "-null-crc", Arg.Set no_crc, " Print a null CRC for imported interfaces";
  "-args", Arg.Expand Arg.read_arg,
     "<file> Read additional newline separated command line arguments \n\
     \      from <file>";
  "-args0", Arg.Expand Arg.read_arg0,
     "<file> Read additional NUL separated command line arguments from \n\
     \      <file>";
]
let arg_usage =
   Printf.sprintf "%s [OPTIONS] FILES : give information on files" Sys.argv.(0)

let main() =
  Arg.parse_expand arg_list dump_obj arg_usage;
  exit 0

let _ = main ()
