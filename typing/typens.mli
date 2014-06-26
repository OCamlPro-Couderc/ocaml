
(** Representation of the environment builded by the prelude *)
type namespaces = namespace_env list

and namespace_env =
  | Ns of string Asttypes.loc * namespace_env list
  | Mod of string Asttypes.loc * string (* name to use * path to find it *)
  | Shadowed of string Asttypes.loc
  | Wildcard

(** Builds the environment, using the prelude given. Exported to allow programs
  to use it to compute dependencies and build such an environment *)
val mk_nsenv: Parsetree.imports -> namespaces

val elaborate_import: namespace_env -> Parsetree.structure_item

val elaborate_interface: namespace_env -> Parsetree.signature_item

val compute_interface_prelude: Parsetree.prelude -> Parsetree.signature * Longident.t option

(** Checks the prelude, takes a function that add the namespaces into the
    modules names *)
val compute_prelude: Parsetree.prelude -> Parsetree.structure * Longident.t option
