
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
val mk_nsenv: Parsetree.imports -> namespaces * (Env.t -> Env.t) list

(** Adds the import constraint given to the environment and opens the Pervasive
  module if necessary *)
val compute_import: Env.t -> Parsetree.import_item -> Env.t

(** Takes the prelude (the header) and adds all its imports in the environment
  (which is basically the initial_env *)
val compute_prelude_no_alias: Parsetree.prelude -> Env.t -> Env.t * Longident.t option

(** Readds the aliases to the signature, since unaliased names for imported
    modules cannot be used by the parser *)
val realias_signature: Types.signature -> Types.signature


val mk_import: namespace_item ->
  Parsetree.structure_item * (string * Longident.t option) list

val mk_interface: namespace_item ->
  Parsetree.signature_item * (string * Longident.t option) list

val compute_interface_prelude: Parsetree.prelude -> Parsetree.signature * Longident.t option

(** Checks the prelude, takes a function that add the namespaces into the
    modules names *)
val compute_prelude: Parsetree.prelude -> Parsetree.structure * Longident.t option
