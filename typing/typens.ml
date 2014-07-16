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

(* Functions used to type and elaborate namespace *)

open Longident
open Parsetree
open Location
open Misc

type namespaces = namespace_item list

and namespace_item = namespace_item_desc loc

and namespace_item_desc =
  | Ns of string * namespaces
  | Mod of string * string * bool (* name to use * path to find it/original name
                                       * opened
                                  *)
  | Shadowed of string
  | Wildcard

let print_namespace ns =
  let rec print indent m =
    match m.txt with
    | Shadowed m -> Printf.sprintf "%s- %s as _" indent m
    | Mod (m, n, b) -> Printf.sprintf "%s- %s as %s (opened: %b)" indent m n b
    | Ns (n, sub) ->
        let indent' = Printf.sprintf "%s  " indent in
        let sub = List.map (print indent') sub in
        Printf.sprintf "%s| %s:\n%s" indent n (String.concat "\n" sub)
    | Wildcard -> Printf.sprintf "%s- ..." indent
  in
  List.fold_left (fun acc ns ->
      Printf.sprintf "%s\n%s" acc @@ print "" ns) "" ns

let is_namespace ns item =
  match item.txt with
  | Ns (n, sub) -> n = ns
  | _ -> false

let mod_of_cstr loc = function
  | Cstr_mod s -> mkloc (Mod (s, s, false)) loc
  | Cstr_alias (s, al) -> mkloc (Mod (al, s, false)) loc
  | Cstr_shadow (s) -> mkloc (Shadowed s) loc
  | Cstr_wildcard -> mkloc Wildcard loc

let find_all_compunits loc path =
  let rec scan_loadpath acc = function
      [] -> acc
    | dir :: tl ->
        let fullpath = Filename.concat dir path in
        if !Clflags.ns_debug then
          Format.printf "Looking for %s@." fullpath;
        if Sys.file_exists fullpath then
          let files = Sys.readdir fullpath in
          let acc = Array.fold_left (fun acc f ->
              let fullname = Filename.concat fullpath f in
              if !Clflags.ns_debug then
                  Format.printf "Found %s; is_dir: %b; is_cmi: %b@."
                    fullname
                    (Sys.is_directory fullname)
                    (Filename.check_suffix f ".cmi");
              if not (Sys.is_directory fullname)
              && Filename.check_suffix f ".cmi" then
                let m = Filename.chop_extension f |> String.capitalize in
                let res = mkloc (Mod (m, m, m = "Pervasives")) loc in
                if not (List.exists
                          (fun item -> match item.txt with
                              Mod (_, s, _) -> s = m
                            | _ -> false)
                          acc) then
                  res :: acc
                else acc
              else acc) acc files
          in
          scan_loadpath acc tl
        else scan_loadpath acc tl
  in
  scan_loadpath [] !Config.load_path

let rec remove_duplicates l1 = function
    [] -> l1
  | ({ txt = Mod (_, m, _); _ } as mo):: tl ->
      if List.exists
          (fun item -> match item.txt with
               Mod (_, s, _) -> s = m
             | _ -> false)
          l1 then
        remove_duplicates l1 tl
      else
        remove_duplicates (mo :: l1) tl
  | _ :: tl -> assert false

let expand_wildcard loc ns (l: namespaces) =
  let path = Env.longident_to_filepath ns in
  let expand l =
    find_all_compunits loc path
    |> remove_duplicates l
    |> List.filter (fun x ->
        if x.txt = Wildcard && not !Clflags.transparent_modules then
          Location.prerr_warning
            x.loc
            (Warnings.Wildcard_usage (Env.namespace_name ns));
        x.txt <> Wildcard)
  in
  if List.exists (fun item -> item.txt = Wildcard) l then
    begin
      expand l
    end
  else l

let add_pervasives orig mods =
  if List.exists (fun m ->
      match m.txt with
        Mod (_, m, _) | Shadowed m -> true
      | _ -> false) mods
  then mods
  else
    try
      ignore (find_in_path_uncap ~subdir:(Env.longident_to_filepath orig)
                !Config.load_path "pervasives.cmi");
      let p = "Pervasives" in
      (mknoloc (Mod (p, p, true))) :: mods
    with Not_found -> mods

let remove_shadowed mods =
  let rec step l acc =
    match l with
      [] -> acc
    | { txt = Shadowed s; _ } :: tl ->
        List.filter (fun item ->
            match item.txt with
              Mod (_, m, _) ->
                s <> m
            | Shadowed m ->
                s <> m (* removes itself *)
            | _ -> true) acc
        |> step tl
    | _ :: tl -> step tl acc
  in
  step mods mods

let add_constraints loc env (orig: Longident.t) cstrs =
  let ns = flatten orig in
  let rec step env ns =
    match env, ns with
    | content, [] ->
        List.fold_left (fun acc c ->
            mod_of_cstr (c.imp_cstr_loc) (c.imp_cstr_desc) :: acc)
          content cstrs
        |> add_pervasives (Some orig)
        |> expand_wildcard loc (Some orig)
        |> remove_shadowed
    | l, ns :: path ->
        let l = if List.exists (is_namespace ns) l then l
          else (mkloc (Ns (ns, [])) loc) :: l in
        List.map (fun item ->
            match item with
            | { txt = Ns (n, sub); loc } when n = ns ->
                mkloc (Ns (n, step sub path)) loc
            | any -> any) l in
  step env ns

let mk_nsenv imports =
  List.fold_left (fun env item ->
      add_constraints item.imp_loc env item.imp_namespace item.imp_cstr)
    [] imports

let string_of_longident l = String.concat "." (flatten l)

let check_namespace_availability ns loc check_ns_names =
  check_ns_names (mkloc (string_of_longident ns) loc)

let check_import_constraints cstr =
  List.fold_left (fun tree cstr -> ()) () cstr

(*
   First step of elaboration: when looking for List in Std, look directly for
   Std_list
   Once it is working, looking for list.cmi/cmo in std directory instead
   Finally, extend compilation units to add the possibility to link two cu with
   the same name: i.e. add a namespace information into it
*)

let mk_prefixed ns m =
  let flatten = function
    | None -> []
    | Some l -> Longident.flatten l in
  String.capitalize @@
  String.concat "_" @@
  List.map (String.uncapitalize) @@ flatten ns @ [String.uncapitalize m]

let update_ns ns sub =
  match ns with
    None -> Some (Lident sub)
  | Some ns -> Some (Ldot (ns, sub))

let addloc loc = function
    None -> None
  | Some x -> Some (mkloc x loc)

(* let fold_hierarchy f acc item = *)
(*   match item.txt with *)
(*   | Ns (n, sub) -> *)
(*       let acc = f acc item in *)
(*       List.fold_left f acc sub *)
(*   | _ -> f acc item *)

(* Original function, that creates "namespaces structure" *)
let elaborate_import h =
  let rec compute (ns: Longident.t option) nsloc item =
    match item.txt with
    | Mod (al, m, b) ->
        let desc = Pmod_ident
            (mkloc (Lident m) item.loc, addloc nsloc ns) in
        let md =
          {
            pmod_desc = desc;
            pmod_loc = item.loc;
            pmod_attributes = [];
          } in
        let desc = if b then [(m, ns)] else [] in
        Pstr_module {
          pmb_name = mkloc al item.loc;
          pmb_attributes = [];
          pmb_expr = md;
          pmb_loc = item.loc;
        }, desc
    | Ns (n, sub) ->
        let ns = update_ns ns n in
        let str, opened = List.fold_left (fun (descs, opened_acc) r ->
            let desc, opened = compute ns item.loc r in
            { pstr_desc = desc; pstr_loc = item.loc} :: descs, opened @ opened_acc)
            ([], []) sub in
        let md =
          {
            pmod_desc = Pmod_structure str;
            pmod_loc = item.loc;
            pmod_attributes = [];
          } in
        Pstr_module {
          pmb_name = mkloc n item.loc;
          pmb_attributes = [];
          pmb_expr = md;
          pmb_loc = item.loc;
        }, opened
    | _ -> assert false
  in
  let desc, opened = compute None (Location.none) h in
  {
    pstr_desc = desc;
    pstr_loc = Location.none;
  }, opened


let elaborate_open' (id, ns) =
  let lid = match ns with
      Some ns -> Ldot(ns, id)
    | None -> Lident (id) (* impossible case *)
  in
  let open_desc =
    {
      popen_lid = mknoloc lid;
      popen_override = Asttypes.Fresh;
      popen_loc = Location.none;
      popen_attributes = [];
    }
  in
  {
    pstr_desc = Pstr_open open_desc;
    pstr_loc = Location.none;
  }

(** Impl *)

let mk_open id ns =
  let lid = match ns with
      Some ns -> Ldot(ns, id)
    | None -> Lident (id) (* impossible case *)
  in
  let open_desc =
    {
      popen_lid = mknoloc lid;
      popen_override = Asttypes.Fresh;
      popen_loc = Location.none;
      popen_attributes = [];
    }
  in
  {
    pstr_desc = Pstr_open open_desc;
    pstr_loc = Location.none;
  }

let mk_mod ns item =
  match item.txt with
    Mod (al, m, _) ->
      let desc = Pmod_ident
          (mkloc (Lident m) item.loc, addloc item.loc ns) in
      let md =
        {
          pmod_desc = desc;
          pmod_loc = item.loc;
          pmod_attributes = [];
        } in
      let str = Pstr_module {
          pmb_name = mkloc al item.loc;
          pmb_attributes = [];
          pmb_expr = md;
          pmb_loc = item.loc;
      } in
      {
        pstr_desc = str;
        pstr_loc = item.loc;
      }
  | _ -> assert false

let mod_to_parsetree acc ns item =
  match item.txt with
    Mod (al, m ,b) ->
      let acc = mk_mod ns item :: acc in
      if b then mk_open m ns :: acc
      else acc
  | _ -> assert false

let import_to_parsetree h =
  let rec f ns acc item =
    match item.txt with
      Mod _ -> mod_to_parsetree acc ns item
    | Ns (n, sub) ->
        let ns = update_ns ns n in
        List.fold_left (f ns) acc sub
    | _ -> assert false
  in
  f None [] h
  |> List.rev

(** Sig *)

let mk_open_sig id ns =
  let lid = match ns with
      Some ns -> Ldot(ns, id)
    | None -> Lident (id) (* impossible case *)
  in
  let open_desc =
    {
      popen_lid = mknoloc lid;
      popen_override = Asttypes.Fresh;
      popen_loc = Location.none;
      popen_attributes = [];
    }
  in
  {
    psig_desc = Psig_open open_desc;
    psig_loc = Location.none;
  }

let mk_mod_sig ns item =
  match item.txt with
    Mod (al, m, _) ->
      let desc = Pmty_alias
          (mkloc (Lident m) item.loc, addloc item.loc ns) in
      let mty =
        {
          pmty_desc = desc;
          pmty_loc = item.loc;
          pmty_attributes = [];
        } in
      let sg = Psig_module {
          pmd_name = mkloc al item.loc;
          pmd_attributes = [];
          pmd_type = mty;
          pmd_loc = item.loc;
      } in {
        psig_desc = sg;
        psig_loc = item.loc;
      }
  | _ -> assert false

let mod_to_parsetree_sig acc ns item =
  match item.txt with
    Mod (al, m ,b) ->
      let acc = mk_mod_sig ns item :: acc in
      if b then mk_open_sig m ns :: acc
      else acc
  | _ -> assert false

let import_to_parsetree_sig h =
  let rec f ns acc item =
    match item.txt with
      Mod _ -> mod_to_parsetree_sig acc ns item
    | Ns (n, sub) ->
        let ns = update_ns ns n in
        List.fold_left (f ns) acc sub
    | _ -> assert false
  in
  f None [] h
  |> List.rev


(** *)

let elaborate_sig_open (id, ns) =
  let lid = match ns with
      Some ns -> Ldot(ns, id)
    | None -> Lident (id) (* impossible case *)
  in
  let open_desc =
    {
      popen_lid = mknoloc lid;
      popen_override = Asttypes.Fresh;
      popen_loc = Location.none;
      popen_attributes = [];
    }
  in
  {
    psig_desc = Psig_open open_desc;
    psig_loc = Location.none;
  }

let elaborate_interface h =
  let rec compute ns nsloc item =
    match item.txt with
    | Mod (al, m, b) ->
        let desc = Pmty_alias (mkloc (Lident m) item.loc, addloc nsloc ns) in
        let mty =
          {
            pmty_desc = desc;
            pmty_loc = item.loc;
            pmty_attributes = [];
          } in
        let opened = if b then [(m, ns)] else [] in
        Psig_module {
          pmd_name = mkloc al item.loc;
          pmd_attributes = [];
          pmd_type = mty;
          pmd_loc = item.loc;
        }, opened
    | Ns (n, sub) ->
        let ns = update_ns ns n in
        let sg, opened = List.fold_left (fun (descs, opened_acc) r ->
            let desc, opened = compute ns item.loc r in
            { psig_desc = desc; psig_loc = item.loc} :: descs, opened @ opened_acc)
            ([], []) sub in
        let mty =
          {
            pmty_desc = Pmty_signature sg;
            pmty_loc = item.loc;
            pmty_attributes = [];
          } in
        Psig_module {
          pmd_name = mkloc n item.loc;
          pmd_attributes = [];
          pmd_type = mty;
          pmd_loc = item.loc;
        }, opened
    | _ -> assert false
  in
  let desc, opened = compute None (Location.none) h in
  {
    psig_desc = desc;
    psig_loc = Location.none;
  }, opened

let verify_import i check_ns_names =
  check_namespace_availability i.imp_namespace i.imp_loc check_ns_names;
  let _constraints = check_import_constraints i.imp_cstr in
  ()

(* Experiment to add modules directly in the environment without using aliases *)
(* let modl = *)
(*   type_module ~alias:true true funct_body *)
(*     (anchor_submodule name.txt anchor) env smodl in *)
(* let md = *)
(*   { md_type = enrich_module_type anchor name.txt modl.mod_type env; *)
(*     md_attributes = attrs; *)
(*     md_loc = pmb_loc; *)
(*   } *)
(* in *)
(* let (id, newenv) = Env.enter_module_declaration name.txt md env in *)

let add_module_from_namespace env alias module_name ns =
  let path = Env.lookup_module ~load:true ~pers:true ns (Lident module_name) env in
  if !Clflags.ns_debug then
    Format.printf "Path received for %s %@ %s: %s@." module_name
      (Env.namespace_name ns) (Path.complete_name path);
  (* let decl = Env.find_module ns path env in *)
  let id, env = Env.enter_module ~arg:false ~from_header:true alias
      (Types.Mty_alias (path, ns)) env in
  Env.add_required_global id;
  env
  (* if !Clflags.ns_debug then *)
  (*   Format.printf "Adding %s as an alias for %s" alias (Path.name path); *)
  (* Env.add_module ~arg:false (Ident.create alias) (Types.Mty_ident path) env *)

module StringSet = Set.Make(String)

let add_modules hierarchy env =
  let module_names = ref StringSet.empty in
  let rec fold ns env item =
    match item.txt with
    | Mod (al, m, opened) ->
        if StringSet.mem al !module_names then
          failwith "Module with the same name already imported";
        module_names := StringSet.add al !module_names;
        let new_env = add_module_from_namespace env al m ns in
        (* if opened then *)
        (*   snd @@ *)
        (*   Typemod.type_open_ Asttypes.Override new_env item.loc *)
        (*     (mkloc (Lident al) item.loc) *)
        (* else *) new_env
    | Ns (n, sub) ->
        let ns = update_ns ns n in
        List.fold_left (fold ns) env sub
    | _ -> assert false
  in
  List.fold_left (fold None) env hierarchy

let compute_prelude_no_alias prl env =
  let ns =
    match prl.prl_ns with
      None -> None
    | Some nd -> Some nd.ns_name
  in
  Env.set_namespace_unit ns;
  let hierarchy = mk_nsenv prl.prl_imports in
  add_modules hierarchy env, ns

let compute_interface_prelude prl =
  let ns =
    match prl.prl_ns with
      None -> None
    | Some nd -> Some nd.ns_name
  in
  Env.set_namespace_unit ns;
  let hierarchy = mk_nsenv prl.prl_imports in
  let ast =
    if not !Clflags.namespace_struct then
      List.map import_to_parsetree_sig hierarchy
      |> List.flatten
    else
      let imports, opened = List.split @@ List.map elaborate_interface hierarchy in
      imports @ (List.map elaborate_sig_open (List.flatten opened))
  in
  if ! Clflags.ns_debug then
    Format.printf "Resulting ast:\n%a@." Pprintast.signature ast;
  ast, ns

let compute_prelude prl =
  let ns =
      match prl.prl_ns with
      | None -> None
      | Some nd -> Some nd.ns_name
  in
  Env.set_namespace_unit ns;
  let hierarchy = mk_nsenv prl.prl_imports in
  if !Clflags.ns_debug then
    Format.printf "Resulting hierarchy of namespaces:\n%s@."
    @@ print_namespace hierarchy;
  let ast =
    if not !Clflags.namespace_struct then
      List.map import_to_parsetree hierarchy
      |> List.flatten
    else
      let imports, opened = List.split @@ List.map elaborate_import hierarchy in
      imports @ (List.map elaborate_open' (List.flatten opened))
  in
  if ! Clflags.ns_debug then
    Format.printf "Resulting ast:\n%a@." Pprintast.structure ast;
  ast, ns
