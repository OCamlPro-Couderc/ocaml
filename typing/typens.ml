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
open Typedtree
open Asttypes

type namespaces = namespace_env list

and namespace_env =
  | Ns of string Asttypes.loc * namespace_env list
  | Mod of string Asttypes.loc * string (* name to use * path to find it/original name *)
  | Shadowed of string Asttypes.loc
  | Wildcard

let print_namespace ns =
  let rec print indent = function
    | Shadowed m -> Format.sprintf "%s- %s as _" indent m.txt
    | Mod (m, n) -> Format.sprintf "%s- %s as %s" indent m.txt n
    | Ns (n, sub) ->
        let indent' = Format.sprintf "%s  " indent in
        let sub = List.map (print indent') sub in
        Format.sprintf "%s| %s:\n%s" indent n.txt (String.concat "\n" sub)
    | Wildcard -> Format.sprintf "%s- ..." indent
  in
  List.fold_left (fun acc ns -> Format.sprintf "%s\n%s" acc @@ print "" ns) "" ns

let is_namespace ns = function
  | Ns (n, sub) -> n.txt = ns
  | _ -> false

let mod_of_cstr loc = function
  | Cstr_mod s -> Mod (mkloc s loc, s)
  | Cstr_alias (s, al) -> Mod (mkloc al loc, s)
  | Cstr_shadow (s) -> Shadowed (mkloc s loc)
  | Cstr_wildcard -> Wildcard

let find_all_compunits path =
  let rec scan_loadpath acc = function
      [] -> acc
    | dir :: tl ->
        let fullpath = Filename.concat dir path in
        if !Clflags.ns_debug then
          Format.printf "Looking for %s@." fullpath;
        if Sys.file_exists fullpath then
          let files = Sys.readdir fullpath in
          let acc = Array.fold_left (fun acc f ->
              if !Clflags.ns_debug then
                  Format.printf "Found %s@." f;
              let fullname = Filename.concat fullpath f in
              if not (Sys.is_directory fullname) && Filename.check_suffix ".cmi" f then
                let m = Filename.chop_extension f |> String.capitalize in
                let res = Mod (mknoloc m, m) in
                if not (List.exists
                          (function Mod (_, s) -> s = m | _ -> false) acc) then
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
  | (Mod (_, m) as mo) :: tl -> if List.exists
      (function Mod (_, s) -> s = m | _ -> false) l1 then
        remove_duplicates l1 tl
      else
        remove_duplicates (mo :: l1) tl
  | _ :: tl -> assert false

let expand_wildcard ns l =
  let path = Env.longident_to_filepath ns in
  let expand l =
    find_all_compunits path
    |> remove_duplicates l
    |> List.filter (fun x -> x <> Wildcard)
  in
  if List.mem Wildcard l then
    expand l
  else l

let remove_shadowed mods =
  let rec step l acc =
    match l with
      Shadowed s :: tl ->
        List.filter (function
              Mod (_, m) -> s.txt <> m
            | Shadowed m -> s.txt <> m.txt (* technically shadowing two times a
                                            module shouldn't be accepted *)
            | _ -> true) acc
        |> step tl
    | _ -> acc
  in
  step mods mods

let add_constraints loc env (orig: Longident.t) cstrs =
  let ns = flatten orig in
  let rec step env ns =
    match env, ns with
    | content, [] ->
        List.fold_left (fun acc c ->
            mod_of_cstr (c.imp_cstr_loc) (c.imp_cstr_desc) :: acc) content cstrs
        |> expand_wildcard (Some orig)
        (* |> remove_shadowed *)
    | l, ns :: path ->
        let l = if List.exists (is_namespace ns) l then l
          else Ns (mkloc ns loc, []) :: l in
        List.map (function
            | Ns (n, sub) when n.txt = ns -> Ns (n, step sub path)
            | any -> any) l in
  step env ns

let mk_nsenv imports =
  List.fold_left (fun env item ->
      add_constraints item.imp_loc env item.imp_namespace item.imp_cstr) [] imports

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
    None -> Some (Lident sub.txt)
  | Some ns -> Some (Ldot (ns, sub.txt))

let addnoloc = function
    None -> None
  | Some x -> Some (mknoloc x)

let elaborate_import h =
  let rec compute ns = function
    | Mod (al, m) ->
        let md =
          {
            pmod_desc = Pmod_ident (mknoloc (Lident m), addnoloc ns);
            pmod_loc = Location.none;
            pmod_attributes = [];
          } in
        Pstr_module {
          pmb_name = al;
          pmb_attributes = [];
          pmb_expr = md;
          pmb_loc = Location.none;
        }
    | Ns (n, sub) ->
        let ns = update_ns ns n in
        let str = List.map (fun r ->
            { pstr_desc = compute ns r; pstr_loc = Location.none}) sub in
        let md =
          {
            pmod_desc = Pmod_structure str;
            pmod_loc = Location.none;
            pmod_attributes = [];
          } in
        Pstr_module {
          pmb_name = n;
          pmb_attributes = [];
          pmb_expr = md;
          pmb_loc = Location.none;
        }
    | _ -> assert false
  in
  {
    pstr_desc = compute None h;
    pstr_loc = Location.none;
  }

let verify_import i check_ns_names =
  check_namespace_availability i.imp_namespace i.imp_loc check_ns_names;
  let _constraints = check_import_constraints i.imp_cstr in
  ()


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
  List.map elaborate_import hierarchy, ns
