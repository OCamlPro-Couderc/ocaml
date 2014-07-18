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
open Types

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
    | Mod (al, m, b) -> Printf.sprintf "%s- %s as %s (opened: %b)" indent m al b
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


let string_of_longident l = String.concat "." (flatten l)

let check_namespace_availability ns loc check_ns_names =
  check_ns_names (mkloc (string_of_longident ns) loc)

let check_import_constraints cstr =
  List.fold_left (fun tree cstr -> ()) () cstr

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
        Mod (_, m, _) | Shadowed m -> m = "Pervasives"
      | _ -> false) mods
  then mods
  else
    try
      ignore (find_in_path_uncap ~subdir:(Env.longident_to_filepath orig)
                !Config.load_path "pervasives.cmi");
      let p = "Pervasives" in
      let al = p ^ "@" ^ (Env.namespace_name orig) in
      (mknoloc (Mod (al, p, true))) :: mods
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

let opened_closures loc ns mods =
  let step acc item =
    match item.txt with
      Mod (al, m, op) ->
        if op then
          (fun env ->
             if !Clflags.ns_debug then
               Format.printf "Opening auto of %s@." al;
             snd @@
             !Typecore.type_open Asttypes.Override env item.loc
               (mkloc (Lident al) item.loc)) :: acc
        else acc
    | _ -> acc
  in
  mods, List.fold_left step [] @@ List.rev mods


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
        |> opened_closures loc (Some orig)
    | l, ns :: path ->
        let l = if List.exists (is_namespace ns) l then l
          else (mkloc (Ns (ns, [])) loc) :: l in
        List.fold_left
          (fun (acc, op) item ->
             match item with
             | { txt = Ns (n, sub); loc } when n = ns ->
                 let sub, to_op = step sub path in
                 mkloc (Ns (n, sub)) loc :: acc, to_op @ op
             | any -> any :: acc, op)
          ([], []) @@ List.rev l in
  step env ns

let mk_nsenv imports =
  List.fold_left (fun (env, to_op) item ->
      add_constraints item.imp_loc env item.imp_namespace item.imp_cstr)
    ([], []) @@ List.rev imports

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

(* Original functions, that creates "namespaces structure" *)

let mk_import h =
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


let mk_open_structure (id, ns) =
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
  let lid = (* match ns with *)
    (*   Some ns -> Ldot(ns, id) *)
    (* | None ->  *)Lident (id) (* impossible case *)
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

let mk_sig_open id ns =
  let lid = (* match ns with *)
    (*   Some ns -> Ldot(ns, id) *)
    (* | None ->  *)Lident (id) (* impossible case *)
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
      if b then mk_sig_open m ns :: acc
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

let mk_sig_open_structure (id, ns) =
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

let mk_interface h =
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

(** Elaboration case, where the imports are transformed into module aliases *)

let compute_interface_prelude prl =
  let ns =
    match prl.prl_ns with
      None -> None
    | Some nd -> Some nd.ns_name
  in
  Env.set_namespace_unit ns;
  let hierarchy, _ = mk_nsenv prl.prl_imports in
  let ast =
    if not !Clflags.namespace_struct then
      List.map import_to_parsetree_sig hierarchy
      |> List.flatten
    else
      let imports, opened = List.split @@ List.map mk_interface hierarchy in
      imports @ (List.map mk_sig_open_structure (List.flatten opened))
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
  let hierarchy, _ = mk_nsenv prl.prl_imports in
  if !Clflags.ns_debug then
    Format.printf "Resulting hierarchy of namespaces:\n%s@."
    @@ print_namespace hierarchy;
  let ast =
    if not !Clflags.namespace_struct then
      List.map import_to_parsetree hierarchy
      |> List.flatten
    else
      let imports, opened = List.split @@ List.map mk_import hierarchy in
      imports @ (List.map mk_open_structure (List.flatten opened))
  in
  if ! Clflags.ns_debug then
    Format.printf "Resulting ast:\n%a@." Pprintast.structure ast;
  ast, ns


(* Experiment to add modules directly in the environment without using aliases *)

let paths = Hashtbl.create 17

let rec alias_of_path p =
  match p with
    Path.Pident id ->
      begin
        try Path.Pident (Hashtbl.find paths p)
        with Not_found -> p
      end
  | Path.Pdot (p, s, i) -> Path.Pdot (alias_of_path p, s, i)
  | Path.Papply (p1, p2) -> Path.Papply (alias_of_path p1, alias_of_path p2)


let add_module_from_namespace env alias module_name ns =
  let path =
    Env.lookup_module ~load:false ~pers:true ns (Lident module_name) env in
  if !Clflags.ns_debug then
    Format.printf "Path received for %s %@ %s: %s@." module_name
      (Env.namespace_name ns) (Path.complete_name path);
  let id, env = Env.enter_module ~arg:false ~from_header:true alias
      (Types.Mty_alias (path, ns)) env in
  Env.add_required_global id;
  Hashtbl.add paths path id;
  env

module StringMap = Map.Make(String)

let module_names = ref StringMap.empty

let add_modules imports env =
  let rec fold ns env item =
    let str_ns = Env.namespace_name ns in
    match item.txt with
    | Mod (al, m, _) ->
        begin
          try let ns' = StringMap.find al !module_names in
            Location.prerr_warning
              item.loc
              (Warnings.Shadowed_import (al, ns'));
          with Not_found -> ()
        end;
        module_names := StringMap.add al str_ns !module_names;
        add_module_from_namespace env al m ns
    | Ns (n, sub) ->
        let ns = update_ns ns n in
        List.fold_left (fold ns) env sub
    | _ -> assert false
  in
  List.fold_left (fun (env, opened) item ->
      let h, to_op =
        add_constraints item.imp_loc [] item.imp_namespace item.imp_cstr in
      List.fold_left (fold None) env h, to_op @ opened)
    (env, []) imports

let compute_import env import =
  let env, opened = add_modules [import] env in
  List.fold_left (|>) env opened

let compute_prelude_no_alias prl env : Env.t * Longident.t option =
  let ns =
    match prl.prl_ns with
      None -> None
    | Some nd -> Some nd.ns_name
  in
  Env.set_namespace_unit ns;
  if !Clflags.ns_debug then
    (let hierarchy, _ = mk_nsenv prl.prl_imports in
    Format.printf "Resulting hierarchy of namespaces:\n%s@."
    @@ print_namespace hierarchy);
  (* let env, opened = add_modules prl.prl_imports env in *)
  List.fold_left compute_import env prl.prl_imports, ns

(** Functions to restore the alias in the infered signature.
    Only used when outputing the sig with the -i option of the compiler.
*)

let rec update_type_desc tdesc =
  match tdesc with
      Tvar _ -> tdesc
    | Tarrow (l, te1, te2, comm) ->
        Tarrow (l, update_type_expr te1, update_type_expr te2, comm)
    | Ttuple texprs -> Ttuple (List.map update_type_expr texprs)
    | Tconstr (p, texprs, am) ->
        let am = ref @@ update_abbrev_memo !am in
        Tconstr (alias_of_path p, List.map update_type_expr texprs, am)
    | Tobject (texpr, op) ->
        let op = match !op with (* creates a new ref, does not change the
                                  semantics since it is only call in case of -i *)
            None -> ref None
          | Some (p, texprs) ->
              ref (Some (alias_of_path p, List.map update_type_expr texprs)) in
        Tobject (update_type_expr texpr, op)
    | Tfield (s, fk, te1, te2) ->
        Tfield (s, fk, update_type_expr te1, update_type_expr te2)
    | Tnil -> Tnil
    | Tlink te -> Tlink (update_type_expr te)
    | Tsubst te -> Tsubst (update_type_expr te)
    | Tvariant row -> Tvariant (update_row_desc row)
    | Tunivar _ -> tdesc
    | Tpoly (te, tes) -> Tpoly (update_type_expr te, List.map update_type_expr tes)
    | Tpackage (p, lids, tes) ->
        Tpackage (alias_of_path p, lids, List.map update_type_expr tes)

and update_type_expr texpr =
  { texpr with desc = update_type_desc texpr.desc }

and update_label_decl label =
  { label with ld_type = update_type_expr label.ld_type }

and update_cons_decl cons_decl =
  { cons_decl with cd_args = List.map update_type_expr cons_decl.cd_args }

and update_abbrev_memo am =
  match am with
  | Mnil -> am
  | Mcons (pfl, p, te1, te2, abm) ->
      let abm = update_abbrev_memo am in
      let p = alias_of_path p in
      let te1 = update_type_expr te1 in
      let te2 = update_type_expr te2 in
      Mcons (pfl, p, te1, te2, abm)
  | Mlink abm ->
      Mlink (ref @@ update_abbrev_memo !abm)

and update_type_kind kind =
  match kind with
    Type_abstract | Type_open -> kind
  | Type_record (ldl, rec_repr) ->
      Type_record (List.map update_label_decl ldl, rec_repr)
  | Type_variant cons_decls -> Type_variant (List.map update_cons_decl cons_decls)

and update_row_desc rd =
  { rd with row_fields = List.map
                (fun (l, rf) -> l, update_row_field rf) rd.row_fields;
            row_more = update_type_expr rd.row_more;
            row_name = match rd.row_name with
              | None -> None
              | Some (p, tel) ->
                  Some (alias_of_path p, List.map update_type_expr tel); }

and update_row_field rf =
  match rf with
    Rpresent (Some te) -> Rpresent (Some (update_type_expr te))
  | Rpresent _ | Rabsent -> rf
  | Reither (b1, tel, b2, rf_op) ->
      let tel = List.map update_type_expr tel in
      let rf_op = match !rf_op with
        | None -> ref None
        | Some (rf) -> ref @@ Some (update_row_field rf) in
      Reither (b1, tel, b2, rf_op)

and update_meths m =
  Meths.map (fun (id, ty_expr) -> id, update_type_expr ty_expr) m

and update_vars v =
  Vars.map (fun (id, fl, vfl, te) -> id, fl, vfl, update_type_expr te) v

and update_value_kind kind =
  match kind with
    Val_reg | Val_prim _ | Val_ivar _ | Val_anc _ | Val_unbound -> kind
  | Val_self (meth, vars, str, texpr) ->
      Val_self (ref @@ update_meths !meth,
                ref @@ update_vars !vars,
                str,
                update_type_expr texpr)

and update_ext_constr ec =
  { ec with ext_type_path = alias_of_path ec.ext_type_path;
            ext_type_params = List.map update_type_expr ec.ext_type_params;
            ext_args = List.map update_type_expr ec.ext_args;
            ext_ret_type = match ec.ext_ret_type with
                None -> None
              | Some te -> Some (update_type_expr te); }

(* Update of classes types *)

and update_class_type cty =
  match cty with
  | Cty_constr (p, tel, cty) ->
      let p = alias_of_path p in
      let tel = List.map update_type_expr tel in
      let cty = update_class_type cty in
      Cty_constr (p, tel, cty)
  | Cty_signature sg -> Cty_signature (update_class_sig sg)
  | Cty_arrow (l, te, cty) ->
      Cty_arrow (l, update_type_expr te, update_class_type cty)

and update_class_sig csg =
  { csg with csig_self = update_type_expr csg.csig_self;
             csig_vars = Vars.map (fun (mfl, vfl, te) ->
                 (mfl, vfl, update_type_expr te)) csg.csig_vars;
             csig_inher = List.map (fun (p, tel) ->
                 alias_of_path p, List.map update_type_expr tel) csg.csig_inher; }

and update_class_decl cd =
  { cd with cty_params = List.map update_type_expr cd.cty_params;
            cty_path = alias_of_path cd.cty_path;
            cty_new = (match cd.cty_new with
                None -> None
              | Some te -> Some (update_type_expr te)); }

and update_class_ty_decl ctd =
  { ctd with clty_params = List.map update_type_expr ctd.clty_params;
             clty_type = update_class_type ctd.clty_type;
             clty_path = alias_of_path ctd.clty_path; }


let realias_signature sg =
  let update_item item =
    match item with
    | Sig_value (id, vd) ->
        let vty = update_type_expr vd.val_type in
        let kind = update_value_kind vd.val_kind in
        Sig_value (id, { vd with val_type = vty; val_kind = kind })
    | Sig_type (id, td, r) ->
        let ty_par = List.map update_type_expr td.type_params in
        let manif = match td.type_manifest with
            None -> None
          | Some texpr -> Some (update_type_expr texpr) in
        let kind = update_type_kind td.type_kind in
        let td = { td with type_params = ty_par;
                           type_manifest = manif;
                           type_kind = kind } in
        Sig_type (id, td, r)
    | Sig_typext (id, ec, ext_stat) ->
        let ec = update_ext_constr ec in
        Sig_typext (id, ec, ext_stat)
    | Sig_module (id, md, r) ->
        let mdt =
          match md.md_type with
          | Mty_alias (p, ns) ->
              Mty_alias (alias_of_path p, ns)
          | _ -> md.md_type
        in
        Sig_module (id, { md with md_type = mdt }, r)
    | Sig_modtype (id, mtd) ->
        let mdty =
          match mtd.mtd_type with
          | Some (Mty_alias (p, ns)) ->
              Some (Mty_alias (alias_of_path p, ns))
          | _ -> mtd.mtd_type
        in
        Sig_modtype (id, { mtd with mtd_type = mdty })
    | Sig_class (id, cd, r) -> Sig_class (id, update_class_decl cd, r)
    | Sig_class_type (id, ctd, r) -> item
  in
  List.map update_item sg
