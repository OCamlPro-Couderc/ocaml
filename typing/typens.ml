(* Functions used to type and elaborate namespace *)

open Longident
open Parsetree
open Typedtree

let cu_ns = ref None

type namespaces = namespace_env list

and namespace_env =
  | Ns of string * namespace_env list
  | Mod of string * string (* name to use * path to find it/original name *)
  | Shadowed of string

let print_namespace ns =
  let rec print indent = function
    | Shadowed m -> Format.sprintf "%s- %s as _" indent m
    | Mod (m, n) -> Format.sprintf "%s- %s as %s" indent m n
    | Ns (n, sub) ->
        let indent' = Format.sprintf "%s  " indent in
        let sub = List.map (print indent') sub in
        Format.sprintf "%s| %s:\n%s" indent n (String.concat "\n" sub)
  in
  List.fold_left (fun acc ns -> Format.sprintf "%s\n%s" acc @@ print "" ns) "" ns

let is_namespace ns = function
  | Ns (n, sub) -> n = ns
  | _ -> false

let mod_of_cstr = function
  | Cstr_mod s -> Mod (s, s)
  | Cstr_alias (s, al) -> Mod (al, s)
  | Cstr_shadow (s) -> Shadowed s
  | Cstr_wildcard -> failwith "Wildcard: not yet implemented"

let add_constraints env ns cstrs =
  let ns = flatten ns in
  let rec step env ns =
    match env, ns with
    | content, [] ->
        List.fold_left (fun acc c ->
            mod_of_cstr (c.imp_cstr_desc) :: acc) content cstrs
    | l, ns :: path ->
        let l = if List.exists (is_namespace ns) l then l
          else Ns (ns, []) :: l in
        List.map (function
            | Ns (n, sub) when n = ns -> Ns (n, step sub path)
            | any -> any) l in
  step env ns

let mk_nsenv imports =
  List.fold_left (fun env item ->
      add_constraints env item.imp_namespace item.imp_cstr) [] imports

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
  let hierarchy = mk_nsenv prl.prl_imports in
  Format.printf "Resulting namespace hierarchy:\n%s@." @@ print_namespace hierarchy;
  List.iter (fun i -> verify_import i check_ns_names) prl.prl_imports
