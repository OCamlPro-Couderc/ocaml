(* Functions used to type and elaborate namespace *)

(* open Asttypes *)
(* open Types *)
open Longident
(* open Path *)
open Parsetree
open Typedtree

let cu_ns = ref None

type namespaces = namespace_env list

and namespace_env =
  | Ns of string * namespace_env list
  | Mod of string * string (* name to use * path to find it *)
  | Shadowed of string

let add_mod : namespaces -> Longident.t -> Parsetree.constraint_desc ->
  namespaces = fun env ns (cstr: Parsetree.constraint_desc) ->
  let ns = flatten ns in
  let rec step tree ns =
    match tree, ns with
    | _, [] ->
        let res =
          match cstr with
          | Cstr_mod s -> Mod (s, s)
          | Cstr_alias (s, al) -> Mod (al, s)
          | Cstr_shadow (s) -> Shadowed s
          | Cstr_wildcard -> failwith "Wildcard"
        in
        res, true
    | Ns (n, sub), ns :: tl ->
        if n = ns then
          let sub, found =
            List.fold_left
              (fun (tree, found) n ->
                 if found then (n :: tree), found
                 else
                   let res, found = (step n tl) in
                   (res :: tree), found)
              ([], false) sub in
          Ns (n, sub), found
        else Ns (n, sub), false
    | _,_ -> assert false
  in
  let env = List.fold_left (fun acc n ->
      let res, _ = step n ns in
      res :: acc) [] env in
  env

let add_constraints env ns cstrs =
  List.fold_left (fun env (cstr: Parsetree.import_constraint_item) ->
      add_mod env ns cstr.cstr_type) env cstrs

let mk_nsenv =
  List.fold_left (fun env item ->
      add_constraints env item.imp_namespace item.imp_cstr) []

(* let mk_nsenv imports = *)
(*   List.fold_left (add_constraints) [] imports *)

let string_of_longident l = String.concat "." (flatten l)

let check_namespace_availability ns loc check_ns_names =
  check_ns_names (mkloc (string_of_longident ns) loc)

let check_import_constraints cstr =
  List.fold_left (fun tree cstr -> ()) () cstr

let elaborate_imports tree =
  ()

let verify_import i check_ns_names =
  check_namespace_availability i.imp_namespace i.imp_loc check_ns_names;
  let _constraints = check_import_constraints i.imp_cstr in
  ()

let compute_prelude prl check_ns_names =
  cu_ns :=
    begin
      match prl.prl_ns with
      | None -> None
      | Some nd -> Some nd.ns_name
    end;
  let _ = mk_nsenv prl.prl_imports in
  List.iter (fun i -> verify_import i check_ns_names) prl.prl_imports
