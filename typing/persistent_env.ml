(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Xavier Leroy, projet Gallium, INRIA Rocquencourt                     *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Persistent structure descriptions *)

open Misc
open Cmi_format

module Consistbl = Consistbl.Make (Compunit)

let add_delayed_check_forward = ref (fun _ -> assert false)

type error =
  | Illegal_renaming of Compunit.Name.t * Compunit.Name.t * filepath
  | Inconsistent_import of Compunit.Name.t * filepath * filepath
  | Need_recursive_types of Compunit.Name.t
  | Depend_on_unsafe_string_unit of Compunit.Name.t
  | Inconsistent_package_declaration of
      { imported_unit: Compunit.Name.t; filename: filepath;
        prefix: Compunit.Prefix.t; current_pack: Compunit.Prefix.t }
  | Inconsistent_package_import of filepath * Compunit.Name.t

exception Error of error
let error err = raise (Error err)

(* The compilation unit currently compiled.
   "" if outside a compilation unit. *)
module Current_unit : sig
  val get : unit -> Compunit.t
  val set : ?prefix:Compunit.Prefix.t -> Compunit.Name.t -> unit
  val is : Compunit.Name.t -> bool
  val is_name_of : Ident.t -> bool
end = struct
  let current_unit =
    ref (Compunit.create "")
  let get () =
    !current_unit
  let set ?prefix name =
    let prefix =
      match prefix with
        Some p -> p
      | None -> Compunit.Prefix.parse_for_pack !Clflags.for_package
    in
    current_unit := Compunit.create ~for_pack_prefix:prefix name

  let is name =
    Compunit.Name.equal (Compunit.name !current_unit) name

  let is_name_of id =
    is (Ident.name id)
end

module Persistent_signature = struct
  type t =
    { filename : string;
      cmi : Cmi_format.cmi_infos }

  let load = ref (fun ~unit_name ->
      match Load_path.find_uncap (unit_name ^ ".cmi") with
      | filename -> Some { filename; cmi = read_cmi filename }
      | exception Not_found -> None)
end

type can_load_cmis =
  | Can_load_cmis
  | Cannot_load_cmis of EnvLazy.log

type pers_struct = {
  ps_name: Compunit.Name.t;
  ps_crcs: Compunit.crcs;
  ps_filename: string;
  ps_flags: pers_flags list;
}

module String = Misc.Stdlib.String

(* If a .cmi file is missing (or invalid), we
   store it as Missing in the cache. *)
type 'a pers_struct_info =
  | Missing
  | Found of pers_struct * 'a

type 'a t = {
  persistent_structures : (string, 'a pers_struct_info) Hashtbl.t;
  imported_units: Compunit.Set.t ref;
  imported_opaque_units: String.Set.t ref;
  imported_packed_units: Compunit.Prefix.t String.Map.t ref;
  crc_units: Consistbl.t;
  can_load_cmis: can_load_cmis ref;
}

let empty () = {
  persistent_structures = Hashtbl.create 17;
  imported_units = ref Compunit.Set.empty;
  imported_opaque_units = ref String.Set.empty;
  imported_packed_units = ref String.Map.empty;
  crc_units = Consistbl.create ();
  can_load_cmis = ref Can_load_cmis;
}

let clear penv =
  let {
    persistent_structures;
    imported_units;
    imported_opaque_units;
    imported_packed_units;
    crc_units;
    can_load_cmis;
  } = penv in
  Hashtbl.clear persistent_structures;
  imported_units := Compunit.Set.empty;
  imported_opaque_units := String.Set.empty;
  imported_packed_units := String.Map.empty;
  Consistbl.clear crc_units;
  can_load_cmis := Can_load_cmis;
  ()

let clear_missing {persistent_structures; _} =
  let missing_entries =
    Hashtbl.fold
      (fun name r acc -> if r = Missing then name :: acc else acc)
      persistent_structures []
  in
  List.iter (Hashtbl.remove persistent_structures) missing_entries

let add_import {imported_units; _} unit =
  imported_units :=
    Compunit.Set.add unit !imported_units

let add_imported_opaque {imported_opaque_units; _} s =
  imported_opaque_units := String.Set.add s !imported_opaque_units

let add_imported_packed {imported_packed_units; _} s prefix =
  imported_packed_units := String.Map.add s prefix !imported_packed_units

let find_in_cache {persistent_structures; _} s =
  match Hashtbl.find persistent_structures s with
  | exception Not_found -> None
  | Missing -> None
  | Found (_ps, pm) -> Some pm

let import_crcs penv ~source crcs =
  let {crc_units; _} = penv in
  let import_crc (unit, crco) =
    match crco with
    | None -> ()
    | Some crc ->
        add_import penv unit;
        Consistbl.check crc_units unit crc source
  in List.iter import_crc crcs

let check_consistency penv ps =
  try import_crcs penv ~source:ps.ps_filename ps.ps_crcs
  with Consistbl.Inconsistency(unit, source, auth) ->
    error (Inconsistent_import(Compunit.name unit, auth, source))

let can_load_cmis penv =
  !(penv.can_load_cmis)
let set_can_load_cmis penv setting =
  penv.can_load_cmis := setting

let without_cmis penv f x =
  let log = EnvLazy.log () in
  let res =
    Misc.(protect_refs
            [R (penv.can_load_cmis, Cannot_load_cmis log)]
            (fun () -> f x))
  in
  EnvLazy.backtrack log;
  res

let fold {persistent_structures; _} f x =
  Hashtbl.fold (fun modname pso x -> match pso with
      | Missing -> x
      | Found (_, pm) -> f modname pm x)
    persistent_structures x

let prefix_of_pers_struct ps =
    match List.find_opt (function Pack _ -> true | _ -> false) ps.ps_flags with
      Some (Pack p) -> p
    | _ -> []

(* Reading persistent structures from .cmi files *)

let save_pers_struct penv crc ps pm =
  let {persistent_structures; crc_units; _} = penv in
  let modname = ps.ps_name in
  Hashtbl.add persistent_structures modname (Found (ps, pm));
  List.iter
    (function
        | Rectypes -> ()
        | Alerts _ -> ()
        | Unsafe_string -> ()
        | Pack prefix -> add_imported_packed penv modname prefix
        | Opaque -> add_imported_opaque penv modname)
    ps.ps_flags;
  let for_pack_prefix = prefix_of_pers_struct ps in
  let unit = Compunit.create ~for_pack_prefix modname in
  Consistbl.set crc_units unit crc ps.ps_filename;
  add_import penv unit

let check_pack_compatibility current_prefix imported_prefix =
  Misc.Stdlib.List.is_prefix
    ~equal:(=)
    imported_prefix
    ~of_:current_prefix

let check_pack_import current_prefix imported_prefix imported_unit =
  not (Misc.Stdlib.List.is_prefix
         ~equal:(=)
         (imported_prefix @ [imported_unit])
         ~of_:current_prefix)

let acknowledge_pers_struct penv check modname pers_sig pm =
  let { Persistent_signature.filename; cmi } = pers_sig in
  let name = cmi.cmi_name in
  let crcs = cmi.cmi_crcs in
  let flags = cmi.cmi_flags in
  let ps = { ps_name = name;
             ps_crcs = crcs;
             ps_filename = filename;
             ps_flags = flags;
           } in
  if ps.ps_name <> modname then
    error (Illegal_renaming(modname, ps.ps_name, filename));
  List.iter
    (function
      | Rectypes ->
          if not !Clflags.recursive_types then
            error (Need_recursive_types(ps.ps_name))
      | Unsafe_string ->
          if Config.safe_string then
            error (Depend_on_unsafe_string_unit(ps.ps_name));
      | Alerts _ -> ()
      | Pack p ->
          (* Current for-pack prefix should be stored somewhere to avoid
             computing it using `split_on_char` each time *)
          let curr_prefix = Compunit.prefix (Current_unit.get ()) in
          if not (check_pack_compatibility curr_prefix p)
          && not !Clflags.make_package then
            error (Inconsistent_package_declaration
                     {filename; imported_unit = name; prefix = p;
                      current_pack = curr_prefix});
          if not (check_pack_import curr_prefix p ps.ps_name) then
            error (Inconsistent_package_import
                     (filename,  Compunit.Prefix.to_string p ^ "." ^ modname));
          add_imported_packed penv modname p
      | Opaque ->
          add_imported_opaque penv modname)
    ps.ps_flags;
  if check then check_consistency penv ps;
  let {persistent_structures; _} = penv in
  Hashtbl.add persistent_structures modname (Found (ps, pm));
  ps

let read_pers_struct penv val_of_pers_sig check modname filename =
  let cmi = read_cmi filename in
  let pers_sig = { Persistent_signature.filename; cmi } in
  let pm = val_of_pers_sig pers_sig in
  let ps = acknowledge_pers_struct penv check modname pers_sig pm in
  let for_pack_prefix = prefix_of_pers_struct ps in
  let unit = Compunit.create ~for_pack_prefix modname in
  add_import penv unit;
  (ps, pm)

let find_pers_struct penv val_of_pers_sig check name =
  let {persistent_structures; _} = penv in
  if name = "*predef*" then raise Not_found;
  match Hashtbl.find persistent_structures name with
  | Found (ps, pm) -> (ps, pm)
  | Missing -> raise Not_found
  | exception Not_found ->
    match can_load_cmis penv with
    | Cannot_load_cmis _ -> raise Not_found
    | Can_load_cmis ->
        let psig =
          match !Persistent_signature.load ~unit_name:name with
          | Some psig -> psig
          | None ->
            Hashtbl.add persistent_structures name Missing;
            raise Not_found
        in
        let pm = val_of_pers_sig psig in
        let ps = acknowledge_pers_struct penv check name psig pm in
        let for_pack_prefix = prefix_of_pers_struct ps in
        let unit = Compunit.create ~for_pack_prefix name in
        add_import penv unit;
        (ps, pm)

(* Emits a warning if there is no valid cmi for name *)
let check_pers_struct penv f ~loc name =
  try
    ignore (find_pers_struct penv f false name)
  with
  | Not_found ->
      let warn = Warnings.No_cmi_file(name, None) in
        Location.prerr_warning loc warn
  | Cmi_format.Error err ->
      let msg = Format.asprintf "%a" Cmi_format.report_error err in
      let warn = Warnings.No_cmi_file(name, Some msg) in
        Location.prerr_warning loc warn
  | Error err ->
      let msg =
        match err with
        | Illegal_renaming(name, ps_name, filename) ->
            Format.asprintf
              " %a@ contains the compiled interface for @ \
               %s when %s was expected"
              Location.print_filename filename ps_name name
        | Inconsistent_import _ -> assert false
        | Need_recursive_types name ->
            Format.sprintf
              "%s uses recursive types"
              name
        | Depend_on_unsafe_string_unit name ->
            Printf.sprintf "%s uses -unsafe-string"
              name
        | Inconsistent_package_declaration {filename; prefix; _} ->
            Printf.sprintf
              "%s is compiled for package %s"
              filename (String.concat "." prefix)
        | Inconsistent_package_import(intf_filename, _) ->
            Printf.sprintf
              "%s corresponds to the current unit's package"
              intf_filename
      in
      let warn = Warnings.No_cmi_file(name, Some msg) in
        Location.prerr_warning loc warn

let read penv f modname filename =
  snd (read_pers_struct penv f true modname filename)

let find penv f name =
  snd (find_pers_struct penv f true name)

let check penv f ~loc name =
  let {persistent_structures; _} = penv in
  if not (Hashtbl.mem persistent_structures name) then begin
    (* PR#6843: record the weak dependency ([add_import]) regardless of
       whether the check succeeds, to help make builds more
       deterministic. *)
    add_import penv (Compunit.create name);
    if (Warnings.is_active (Warnings.No_cmi_file("", None))) then
      !add_delayed_check_forward
        (fun () -> check_pers_struct penv f ~loc name)
  end

let crc_of_unit penv f name =
  let (ps, _pm) = find_pers_struct penv f true name in
  let crco =
    try
      List.find (fun (unit, _) ->
          Compunit.Name.equal (Compunit.name unit) name) ps.ps_crcs
    |> snd
    with Not_found ->
      assert false
  in
    match crco with
      None -> assert false
    | Some crc -> crc

let imports {imported_units; crc_units; _} =
  Consistbl.extract (Compunit.Set.elements !imported_units) crc_units

let looked_up {persistent_structures; _} modname =
  Hashtbl.mem persistent_structures modname

let packed {imported_packed_units; _} =
  String.Map.bindings !imported_packed_units

let is_imported {imported_units; _} u =
  Compunit.Set.mem u !imported_units

let is_imported_opaque {imported_opaque_units; _} s =
  String.Set.mem s !imported_opaque_units

let is_imported_packed {imported_packed_units; _} s =
  String.Map.mem s !imported_packed_units

let make_cmi penv modname sign alerts =
  let flags =
    List.concat [
      if !Clflags.recursive_types then [Cmi_format.Rectypes] else [];
      if !Clflags.opaque then [Cmi_format.Opaque] else [];
      (if !Clflags.unsafe_string then [Cmi_format.Unsafe_string] else []);
      (match Compunit.prefix (Current_unit.get ()) with
         [] -> []
       | prefix -> [Cmi_format.Pack prefix] );
      [Alerts alerts];
    ]
  in
  let crcs = imports penv in
  {
    cmi_name = modname;
    cmi_sign = sign;
    cmi_crcs = crcs;
    cmi_flags = flags
  }

let save_cmi penv psig pm =
  let { Persistent_signature.filename; cmi } = psig in
  Misc.try_finally (fun () ->
      let {
        cmi_name = modname;
        cmi_sign = _;
        cmi_crcs = imports;
        cmi_flags = flags;
      } = cmi in
      let crc =
        output_to_file_via_temporary (* see MPR#7472, MPR#4991 *)
          ~mode: [Open_binary] filename
          (fun temp_filename oc -> output_cmi temp_filename oc cmi) in
      (* Enter signature in persistent table so that imports()
         will also return its crc *)
      let prefix =
        match List.find_opt (function Pack _ -> true | _ -> false) flags with
          Some (Pack p) -> p
        | _ -> []
      in
      let unit = Compunit.create ~for_pack_prefix:prefix modname in
      let ps =
        { ps_name = modname;
          ps_crcs = (unit, Some crc) :: imports;
          ps_filename = filename;
          ps_flags = flags;
        } in
      save_pers_struct penv crc ps pm
    )
    ~exceptionally:(fun () -> remove_file filename)

let report_error ppf =
  let open Format in
  let print_prefix ppf prefix =
    match prefix with
    | [] -> pp_print_string ppf "no `-for-pack' prefix"
    | _ ->
        fprintf ppf "a `-for-pack' prefix of [%a]"
          Compunit.Prefix.print prefix
  in
  function
  | Illegal_renaming(modname, ps_name, filename) -> fprintf ppf
      "Wrong file naming: %a@ contains the compiled interface for @ \
       %s when %s was expected"
      Location.print_filename filename ps_name modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %s@]"
      Location.print_filename source1 Location.print_filename source2 name
  | Need_recursive_types(import) ->
      fprintf ppf
        "@[<hov>Invalid import of %s, which uses recursive types.@ %s@]"
        import "The compilation flag -rectypes is required"
  | Depend_on_unsafe_string_unit(import) ->
      fprintf ppf
        "@[<hov>Invalid import of %s, compiled with -unsafe-string.@ %s@]"
        import "This compiler has been configured in strict \
                                  safe-string mode (-force-safe-string)"
  | Inconsistent_package_declaration
      { imported_unit; filename; prefix; current_pack } ->
      fprintf ppf "%s contains the description for a unit [%s] with@ \
                   %a; this cannot be used because@ "
        filename imported_unit
        print_prefix prefix;
      begin match current_pack with
      | [] ->
          fprintf ppf "the current unit is being compiled \
                              without a `-for-pack' prefix"
      | _ ->
          fprintf ppf "the current unit has %a"
            print_prefix current_pack
      end
  | Inconsistent_package_import(intf_filename, intf_fullname) ->
      fprintf ppf
        "@[<hov>The interface %s@ corresponds to the current unit's package %s.@]"
        intf_filename intf_fullname

let () =
  Location.register_error_of_exn
    (function
      | Error err ->
          Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
