open Prelude_utils

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

val realias_type_expr: Types.type_expr -> Types.type_expr
