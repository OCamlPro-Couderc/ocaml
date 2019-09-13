type error =
  Invalid_character of char

exception Error of error

module Name : sig

  type t = string

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit

end

module Prefix : sig

  type component = Name.t

  type t = component list

  val equal_component : component -> component -> bool

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val parse_for_pack : string option -> t
  (** [parse_for_pack p] returns the list of nested packed modules from a
      `-for-pack` argument.*)

  val extract_prefix : string -> t * Name.t
  (** [extract_prefix id] returns the prefix of an identifier and its basename,
      as if it was generated with `-for-pack` *)

  val print: Format.formatter -> t -> unit

  val to_string: t -> string

end

type t

val create : ?for_pack_prefix:Prefix.t -> Name.t -> t

val name : t -> Name.t

val prefix : t -> Prefix.t

val full_path : t -> string

val of_raw_string : string -> t

type crcs = (t * Digest.t option) list

include Identifiable.S with type t := t
