(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Misc

module CU = Compilation_unit
module UI = Cmx_format.Unit_info

type error =
  | Illegal_renaming of {
      name_in_cmx : CU.Name.t;
      file : string;
      desired_name : CU.Name.t;
    }
  | Forward_reference of string * CU.Name.t
  | Wrong_for_pack of string * CU.Prefix.t
  | Linking_error
  | File_not_found of string
  | Cannot_pack_recursive_interface of string
  | Incompatible_recursive_flags of CU.Name.t * bool

exception Error of error

(* Read the unit information from a .cmx file. *)

type pack_member_kind =
  | PM_intf
  | PM_impl of UI.t * Cmx_format.Unit_info_link_time.t

type pack_member =
  { pm_file: filepath;
    pm_name: CU.Name.t;
    pm_kind: pack_member_kind;
  }

let check_member_recursive_info info =
  match UI.recursive info with
    None when !Clflags.make_recursive_package ->
      raise (Error (Incompatible_recursive_flags
                      (CU.name (UI.unit info), false)))
  | Some (_, _, _)
    when not !Clflags.make_recursive_package ->
      raise (Error (Incompatible_recursive_flags
                      (CU.name (UI.unit info), true)))
  | _ -> ()

let read_member_info pack_path file =
  let name =
    CU.Name.of_string (String.capitalize_ascii (
      Filename.basename (chop_extensions file)))
  in
  let kind =
    if Filename.check_suffix file ".cmi" then begin
      if !Clflags.make_recursive_package then
        raise (Error (Cannot_pack_recursive_interface file));
      PM_intf
    end
    else begin
      let info, link_info, crc = Cmx_format.load ~filename:file in
      let name_in_cmx = CU.name (UI.unit info) in
      if not (CU.Name.equal name_in_cmx name) then begin
        raise (Error (Illegal_renaming {
          name_in_cmx;
          file;
          desired_name = name;
        }))
      end;
      let cmx_file_for_pack_prefix = CU.for_pack_prefix (UI.unit info) in
      if not (CU.Prefix.equal cmx_file_for_pack_prefix pack_path)
      then begin
        raise (Error (Wrong_for_pack (file, pack_path)))
      end;
      Asmlink.check_consistency file info crc;
      Compilation_state.cache_unit_info info;
      check_member_recursive_info info;
      PM_impl (info, link_info)
    end
  in
  { pm_file = file; pm_name = name; pm_kind = kind }

(* Check absence of forward references *)

let check_units members =
  let rec check forbidden = function
    [] -> ()
  | mb :: tl ->
      begin match mb.pm_kind with
      | PM_intf -> ()
      | PM_impl (info, _link_info) ->
          CU.Map.iter
            (fun unit _crc ->
              let name = CU.name unit in
              if CU.Name.Set.mem name forbidden
              then raise(Error(Forward_reference(mb.pm_file, name))))
            (UI.imports_cmx info)
      end;
      check (CU.Name.Set.remove mb.pm_name forbidden) tl
  in
  let forbidden =
    CU.Name.Set.of_list (List.map (fun mb -> mb.pm_name) members)
  in
  check forbidden members

(* Make the .o file for the package *)

let make_package_object
    ~ppf_dump prefix members recursive_dependencies
    targetobj targetname coercion ~backend  =
  Profile.record_call
    (Printf.sprintf "pack(%s)" (CU.Name.to_string targetname)) (fun () ->
    let objtemp =
      if !Clflags.keep_asm_file
      then Filename.remove_extension targetobj ^ ".pack" ^ Config.ext_obj
      else
        (* Put the full name of the module in the temporary file name
           to avoid collisions with MSVC's link /lib in case of successive
           packs *)
        let comp_unit = Persistent_env.Current_unit.get_exn () in
        let symbol = Symbol.for_module_block comp_unit in
        let name = Symbol.name_for_backend symbol in
        Filename.temp_file name Config.ext_obj
    in
    let components =
      List.map
        (fun m ->
          match m.pm_kind with
          | PM_intf -> Lambda.PM_intf
          | PM_impl (ui, _) ->
              let member_cu =
                CU.create ~for_pack_prefix:prefix m.pm_name in
              let member_recursive =
                match UI.recursive ui with
                  None -> None
                | Some (s, fvs, for_recursive_pack) ->
                    let fvs' =
                      List.fold_left (fun acc elt -> Ident.Set.add elt acc)
                        Ident.Set.empty fvs in
                    Some (s, fvs', for_recursive_pack)
              in
              Lambda.PM_impl { member_cu; member_recursive;
                               member_recursive_dependencies =
                                 UI.recursive_dependencies ui })
        members
    in
    let targetname_as_string = CU.Name.to_string targetname in
    let module_ident = Ident.create_persistent targetname_as_string in
    let prefixname = Filename.remove_extension objtemp in
    let program, middle_end =
      if Config.flambda then
        let program =
          Translmod.transl_package_flambda
            components module_ident recursive_dependencies coercion
        in
        program, Flambda_middle_end.lambda_to_clambda
      else
        let program =
          Translmod.transl_store_package
            components module_ident recursive_dependencies coercion
        in
        program, Closure_middle_end.lambda_to_clambda
    in
    let program =
      Lambda.{ program with code = Simplif.simplify_lambda program.code } in
    if !Clflags.dump_lambda then
      Format.fprintf ppf_dump "%a@." Printlambda.lambda program.code;
    Asmgen.compile_implementation ~backend
      ~filename:(CU.Name.to_string targetname)
      ~prefixname
      ~middle_end
      ~ppf_dump
      program;
    let objfiles =
      List.map
        (fun m -> Filename.remove_extension m.pm_file ^ Config.ext_obj)
        (List.filter (fun m -> m.pm_kind <> PM_intf) members)
    in
    let ok =
      Ccomp.call_linker Ccomp.Partial targetobj (objtemp :: objfiles) ""
    in
    remove_file objtemp;
    if not ok then raise(Error Linking_error);
    program
  )

(* Make the .cmx file for the package *)

let build_package_cmx members cmxfile program recursive_dependencies =
  let module UI = Cmx_format.Unit_info in
  let unit_names_in_pack =
    CU.Name.Set.of_list (List.map (fun m -> m.pm_name) members)
  in
  let units, unit_link_infos =
    List.split (
      List.fold_right (fun m accu ->
          match m.pm_kind with
          | PM_intf -> accu
          | PM_impl (info, link_info) -> (info, link_info) :: accu)
        members [])
  in
  let recursive =
    match program.Lambda.recursive with
      None -> None
    | Some (shape, fvs, for_recursive_pack) ->
        Some (shape, Ident.Set.elements fvs, for_recursive_pack)
  in
  (* Recursive pack can generate a call to !CamlinternalMod *)
  Ident.Set.iter (fun id ->
      let _, name = CU.Prefix.extract_prefix (Ident.name id) in
      if CU.Name.Set.mem name unit_names_in_pack then ()
      else
        Compilation_state.require_global id)
    program.Lambda.required_globals;
  let compilation_state = Compilation_state.Snapshot.create () in
  let current_unit = Persistent_env.Current_unit.get_exn () in
  let current_unit_name = CU.name current_unit in
  let current_unit_crc = Env.crc_of_unit current_unit_name in
  let imports_cmi =
    let imports_from_components =
      CU.Map.filter (fun cu _crc ->
          not (CU.Name.Set.mem (CU.name cu) unit_names_in_pack))
        (Asmlink.extract_crc_interfaces ())
    in
    (* Recursive pack can generate a call to !CamlinternalMod *)
    let imports_from_recursive_pack =
      Ident.Set.fold (fun id imports ->
          let cu = CU.of_raw_string (Ident.name id) in
          if CU.Name.Set.mem (CU.name cu) unit_names_in_pack then imports
          else
            let crc = Env.crc_of_unit (CU.name cu) in
            CU.Map.add cu (Some crc) imports)
        program.Lambda.required_globals
        imports_from_components
    in
    CU.Map.add current_unit (Some current_unit_crc) imports_from_recursive_pack
  in
  let imports_cmx =
    let imports_from_components =
      CU.Map.filter (fun imported_unit _crc ->
          let name = CU.name imported_unit in
          not (CU.Name.Set.mem name unit_names_in_pack))
        (Asmlink.extract_crc_implementations ())
    in
    CU.Map.union
      (fun _ comps _imports -> Some comps)
      imports_from_components compilation_state.imports_cmx
  in
  let defines =
    (List.flatten (List.map UI.defines units)) @ [current_unit]
  in
  let export_info : UI.export_info =
    match compilation_state.export_info with
    | Closure approx -> Closure approx
    | Flambda export_info ->
      let export_info =
        List.fold_left (fun acc info ->
            match UI.export_info info with
            | Flambda export_info -> Export_info.merge acc export_info
            | Closure _ ->
              Misc.fatal_errorf "%a contains Closure approximations yet \
                  Flambda export info was found"
                CU.print (UI.unit info))
          export_info
          units
      in
      Flambda export_info
  in
  let pkg_infos =
    UI.create ~unit:current_unit ~defines ~imports_cmi ~imports_cmx
      ~recursive
      ~recursive_dependencies
      ~export_info
  in
  let pkg_link_infos = Cmx_format.Unit_info_link_time.join unit_link_infos in
  Cmx_format.save pkg_infos pkg_link_infos ~filename:cmxfile

(* Make the .cmx and the .o for the package *)

let package_object_files ~ppf_dump files targetcmx
                         targetobj targetname coercion ~backend =
  let packagename =
    match !Clflags.for_package with
    | None -> CU.Name.to_string targetname
    | Some p -> p ^ "." ^ CU.Name.to_string targetname in
  let pack_path = CU.Prefix.parse_for_pack (Some packagename) in
  let recursive_dependencies =
    List.filter (fun cu ->
        not (CU.Prefix.equal pack_path (CU.for_pack_prefix cu)))
      (Env.imports_from_recursive_pack ())
  in
  let members = map_left_right (read_member_info pack_path) files in
  check_units members;
  let program =
    make_package_object
      ~ppf_dump pack_path members recursive_dependencies
      targetobj targetname coercion ~backend
  in
  build_package_cmx members targetcmx program recursive_dependencies

(* The entry point *)

let package_files ~ppf_dump initial_env files targetcmx ~backend =
  let files =
    List.map
      (fun f ->
        try Load_path.find f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extensions targetcmx in
  let targetcmi = prefix ^ ".cmi" in
  let targetobj = Filename.remove_extension targetcmx ^ Config.ext_obj in
  let targetname =
    CU.Name.of_string (String.capitalize_ascii(Filename.basename prefix)) in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmx;
  (* Set the name of the current compunit *)
  let for_pack_prefix =
      CU.Prefix.parse_for_pack !Clflags.for_package in
  let comp_unit = CU.create ~for_pack_prefix targetname in
  Persistent_env.Current_unit.set_unit comp_unit;
    Persistent_env.Current_unit.set_recursive_prefixes
      (List.map (fun s -> Compilation_unit.Prefix.parse_for_pack (Some s))
         !Clflags.recursive_packages);
  Compilation_state.reset comp_unit;
  Misc.try_finally (fun () ->
      let coercion =
        Typemod.package_units initial_env files targetcmi targetname in
      package_object_files ~ppf_dump files targetcmx targetobj targetname
        coercion ~backend
    )
    ~exceptionally:(fun () -> remove_file targetcmx; remove_file targetobj)

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_renaming { name_in_cmx; file; desired_name; } ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %a was expected"
        Location.print_filename file CU.Name.print name_in_cmx
        CU.Name.print desired_name
  | Forward_reference(file, cu_name) ->
      fprintf ppf "Forward reference to %a in file %a" CU.Name.print cu_name
        Location.print_filename file
  | Wrong_for_pack(file, path) ->
      fprintf ppf "File %a@ was not compiled with the `-for-pack %a' option"
        Location.print_filename file
        Compilation_unit.Prefix.print path
  | File_not_found file ->
      fprintf ppf "File %s not found" file
  | Linking_error ->
      fprintf ppf "Error during partial linking"
  | Cannot_pack_recursive_interface file ->
      fprintf ppf "Cannot pack %s into a recursive pack, only implementations \
                   are accepted." file
  | Incompatible_recursive_flags (name, for_recursive_pack) ->
      let compiled_for =
        if for_recursive_pack then "recursive" else "classic" in
      let flag = if for_recursive_pack then "-pack" else "-recursive-pack" in
      fprintf ppf "Compilation unit %a is compiled for a %s pack \
                   This pack must be compiled with the `%s` option."
        Compilation_unit.Name.print name compiled_for flag

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
