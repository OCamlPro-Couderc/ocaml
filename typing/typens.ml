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
open Header_utils

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


(** *)


let verify_import i check_ns_names =
  check_namespace_availability i.imp_namespace i.imp_loc check_ns_names;
  let _constraints = check_import_constraints i.imp_cstr in
  ()

(* Add modules directly in the environment without using aliases *)

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
  (* let module_name = match ns with *)
  (*     None -> Lident module_name *)
  (*   | Some ns -> *)
  (*       if !Clflags.longnames then Ldot (ns, module_name) *)
  (*       else Lident module_name in *)
  let path =
    Env.lookup_module ~load:false ~pers:true ns (Lident module_name) env in
  if !Clflags.ns_debug then
    Format.printf "Path received for %s %@ %s: %s@."
      ((* Longident.string_of_longident  *)module_name)
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

let compute_header_no_alias hdr env : Env.t * Longident.t option =
  let ns =
    match hdr.hd_ns with
      None -> None
    | Some nd -> Some nd.ns_name
  in
  Env.set_namespace_unit ns;
  if !Clflags.ns_debug then
    (let hierarchy, _ = mk_nsenv hdr.hd_imports in
    Format.printf "Resulting hierarchy of namespaces:\n%s@."
    @@ print_namespace hierarchy);
  (* let env, opened = add_modules prl.prl_imports env in *)
  List.fold_left compute_import env hdr.hd_imports, ns

(** Experimental: imports elaborated into structures *)

let extract_ns_and_hierarchy hdr =
  let ns = match hdr.hd_ns with
      None -> None
    | Some nd -> Some nd.ns_name in
  let hierarchy, _ = mk_nsenv hdr.hd_imports in
  ns, hierarchy

let header_as_structure hdr =
  let ns, h = extract_ns_and_hierarchy hdr in
  Env.set_namespace_unit ns;
  import_structure h, ns

let header_as_signature hdr =
  let ns, h = extract_ns_and_hierarchy hdr in
  Env.set_namespace_unit ns;
  import_signature h, ns

(** Functions to restore the alias in the infered signature.
    Only used when outputing the sig with the -i option of the compiler or when
    printing in the toploop.
*)

let realias_signature sg =
  let subst = Hashtbl.fold (fun p id acc ->
      match p with
        Path.Pident i -> Subst.add_module i (Path.Pident id) acc
      | _ -> assert false) paths Subst.identity in
  Subst.signature subst sg

let realias_type_expr expr =
  let subst = Hashtbl.fold (fun p id acc ->
      match p with
        Path.Pident i -> Subst.add_module i (Path.Pident id) acc
      | _ -> assert false) paths Subst.identity in
  Subst.type_expr subst expr
