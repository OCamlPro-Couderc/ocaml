
type namespaces = namespace_item list

and namespace_item = namespace_item_desc Location.loc

and namespace_item_desc =
  | Ns of string * namespaces
  | Mod of string * string * bool (* name to use * path to find it/original name
                                       * opened
                                  *)
  | Shadowed of string
  | Wildcard

val is_namespace: string -> namespace_item -> bool

val mod_of_cstr: Location.t -> Parsetree.constraint_desc -> namespace_item

val update_ns: Longident.t option -> string -> Longident.t option

val print_namespace: namespaces -> string

(* Ocamldep specific functions: creates an AST for the analysis *)

val simple_structure: Parsetree.prelude -> Parsetree.structure

val simple_signature: Parsetree.prelude -> Parsetree.signature
