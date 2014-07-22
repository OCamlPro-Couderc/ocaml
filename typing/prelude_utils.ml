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

open Location
open Parsetree
open Longident

(* The representation of the environment *)

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

let addloc loc = function
    None -> None
  | Some x -> Some (mkloc x loc)

let string_of_longident l = String.concat "." (flatten l)

let longident_to_filepath = function
    | None -> ""
    | Some lid ->
        String.concat Filename.dir_sep
        @@ List.map String.uncapitalize
        @@ Longident.flatten lid

let namespace_name = function
    None -> "ROOT"
  | Some ns -> string_of_longident ns


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


(* Elaboration functions *)

(* Simple elaboration *)
(* implementation part *)

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


let simple_structure prl =
  List.map (fun item ->
      (List.map (fun cstr ->
          mod_of_cstr cstr.imp_cstr_loc cstr.imp_cstr_desc) item.imp_cstr,
       item.imp_namespace))
    prl.prl_imports
  |> List.map (fun (mods, ns) -> List.map (mk_mod (Some ns)) mods)
  |> List.flatten


(* Signature part *)

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

let mod_to_parsetree_sig acc ns item =
  match item.txt with
    Mod (al, m ,b) ->
      let acc = mk_mod_sig ns item :: acc in
      if b then mk_sig_open m ns :: acc
      else acc
  | _ -> assert false

let simple_signature prl =
  List.map (fun item ->
      (List.map (fun cstr ->
          mod_of_cstr cstr.imp_cstr_loc cstr.imp_cstr_desc) item.imp_cstr,
       item.imp_namespace))
    prl.prl_imports
  |> List.map (fun (mods, ns) -> List.map (mk_mod_sig (Some ns)) mods)
  |> List.flatten

(* Plain structures *)
(* Implementation part *)

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

(* Signature part *)

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
