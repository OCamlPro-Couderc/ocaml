
(** Representation of the environment builded by the prelude *)
type namespaces = namespace_item list

and namespace_item = namespace_item_desc Location.loc

and namespace_item_desc =
  | Ns of string * namespaces
  | Mod of string * string * bool (* name/alias to use * original name * opened *)
  | Shadowed of string
  | Wildcard

(** Builds the environment, using the prelude given. Exported to allow programs
  to use it to compute dependencies and build such an environment *)
val mk_nsenv: Parsetree.imports -> namespaces

val elaborate_import: namespace_item ->
  Parsetree.structure_item * (string * Longident.t option) list

val elaborate_interface: namespace_item ->
  Parsetree.signature_item * (string * Longident.t option) list

val compute_interface_prelude: Parsetree.prelude -> Parsetree.signature * Longident.t option

(** Checks the prelude, takes a function that add the namespaces into the
    modules names *)
val compute_prelude: Parsetree.prelude -> Parsetree.structure * Longident.t option
