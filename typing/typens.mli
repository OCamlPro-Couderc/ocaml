
(** The namespace of the currently compiled unit
    Only temporary, should move in proper place (Env certainly)
*)
val cu_ns: Longident.t option ref

(** Representation of the environment builded by the prelude *)
type namespaces = namespace_env list

and namespace_env =
  | Ns of string * namespace_env list
  | Mod of string * string (* name to use * path to find it *)
  | Shadowed of string

(** Builds the environment, using the prelude given. Exported to allow programs
  to use it to compute dependencies and build such an environment *)
val mk_nsenv: Parsetree.imports -> namespaces

(** Checks the prelude, takes a function that add the namespaces into the
    modules names *)
val compute_prelude: Parsetree.prelude -> (string Asttypes.loc -> unit) -> unit
