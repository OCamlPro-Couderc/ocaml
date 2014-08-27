
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

(** Returns the path where the cmis/cmos in the namespace given should be looked for *)
val longident_to_filepath: Longident.t option -> string

(** Returns the namespace name, ROOT otherwise *)
val namespace_name: Longident.t option -> string

(* Ocamldep specific functions: creates an AST for the analysis *)

val simple_structure: Parsetree.header -> Parsetree.structure * Longident.t list

val simple_signature: Parsetree.header -> Parsetree.signature * Longident.t list


(** Experimental: imports elaborated into structures *)

val import_structure: namespaces -> Parsetree.structure

val import_signature: namespaces -> Parsetree.signature
