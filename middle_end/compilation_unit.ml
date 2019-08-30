(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare

module String = Misc.Stdlib.String

module Name = struct
  type t = string

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = String.compare
    let equal = String.equal
    let hash = Hashtbl.hash

    let print = String.print
    let output chan t = print (Format.formatter_of_out_channel chan) t
  end)

  let isupper chr =
    Stdlib.(=) (Char.uppercase_ascii chr) chr

  let of_string str =
    if String.length str < 1 || not (isupper (String.get str 0)) then begin
      Misc.fatal_errorf "Bad compilation unit name: %s" str
    end;
    str

  let to_string t = t
end

module Prefix = struct

  include Misc.Prefix

  let of_prefix p = p

  let to_prefix p = p

end

type t = {
  for_pack_prefix : Prefix.t;
  basename : Name.t;
  hash : int;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare
        ({ basename = basename1; for_pack_prefix = for_pack_prefix1; hash = hash1; _} as t1)
        ({ basename = basename2; for_pack_prefix = for_pack_prefix2; hash = hash2; _} as t2)
        =
    if t1 == t2 then 0
    else
      let c = Stdlib.compare hash1 hash2 in
      if c <> 0 then c
      else
        let c = String.compare basename1 basename2 in
        if c <> 0 then c
        else
          (* With identifiers now prefixed by their pack, this case should
             always return 0 *)
          Prefix.compare for_pack_prefix1 for_pack_prefix2

  let equal x y =
    if x == y then true
    else compare x y = 0

  let print ppf { for_pack_prefix; hash = _; basename } =
    match for_pack_prefix with
    | [] ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(id@ %s)@])@]"
        basename
    | for_pack_prefix ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>(for_pack_prefix@ %a)@]@;\
          @[<hov 1>(basename@ %s)@])@]"
        Prefix.print for_pack_prefix
        basename

  let output oc t =
    print (Format.formatter_of_out_channel oc) t

  let hash t = t.hash
end)

let print_name ppf t =
  Format.pp_print_string ppf t.basename

let create ?(for_pack_prefix = []) basename =
  { for_pack_prefix;
    basename;
    hash = Hashtbl.hash (basename, for_pack_prefix);
  }

let of_persistent_ident id =
  let for_pack_prefix, basename = Prefix.extract_prefix (Ident.name id) in
  create ~for_pack_prefix basename


let none = create (Name.of_string "*none*")

let name t = Name.of_string t.basename

let is_packed t =
  match t.for_pack_prefix with
  | [] -> false
  | _::_ -> true

let for_pack_prefix t = t.for_pack_prefix

let for_pack_prefix_as_string t =
  match for_pack_prefix t with
  | [] -> None
  | prefix -> Some (Prefix.to_string prefix)

let full_path t = t.for_pack_prefix @ [name t]

let full_path_as_string t =
  Prefix.to_string (full_path t)

let current = ref None

let is_current_exn arg =
  match !current with
  | None -> Misc.fatal_error "Current compilation unit is not set"
  | Some cur -> equal cur arg

let set_current t = current := Some t

let get_current () = !current

let get_current_exn () =
  match !current with
  | Some current -> current
  | None -> Misc.fatal_error "Current compilation unit is not set"

let get_current_id_exn () =
  let curr = get_current_exn () in
  Ident.create_persistent ~prefix:curr.for_pack_prefix curr.basename

let path t =
  Path.Pident (Ident.create_persistent (Name.to_string (name t)))
