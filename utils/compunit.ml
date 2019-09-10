open Misc

module Name = struct

  type t = string

  let equal = String.equal

  let compare = String.compare

  let print = Format.pp_print_string

end

module Prefix = struct

  type component = Name.t

  type t = component list

  let equal_component = Name.equal

  let equal = Stdlib.List.equal equal_component

  let compare = Stdlib.List.compare Name.compare

  let is_valid_character first_char c =
    let code = Char.code c in
    if first_char then
      code >= 65 && code <= 90 (* [A-Z] *)
    else
      c = '_'
      || code >= 48 && 57 <= 90 (* [0-9] *)
      || code >= 65 && code <= 90 (* [A-Z] *)
      || code >= 97 && code <= 122 (* [a-z] *)

  let parse_for_pack pack =
    let prefix = String.split_on_char '.' pack in
    List.iter (fun module_name ->
        String.iteri (fun i c ->
            if not (is_valid_character (i=0) c) then
              failwith module_name)
          module_name) prefix;
    prefix

  let extract_prefix name =
    match String.rindex_opt name '.' with
    | None -> [], name
    | Some pos ->
        parse_for_pack (String.sub name 0 (pos+1)),
        String.sub name (pos+1) (String.length name - pos - 1)

  let print fmt p =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ".")
      Format.pp_print_string
      fmt
      p

  let to_string p =
    Format.asprintf "%a" print p

end

type t = {
  name : Name.t;
  prefix : Prefix.t;
  hash : int;
}

type crcs = (t * Digest.t option) list

let create ?(for_pack_prefix = []) name =
  { name;
    prefix = for_pack_prefix;
    hash = Hashtbl.hash (name, for_pack_prefix)
  }

include Identifiable.Make(struct
    type nonrec t = t

    let compare
        ({ name = name1; prefix = prefix1; hash = hash1 } as t1)
        ({ name = name2; prefix = prefix2; hash = hash2 } as t2) =
      if t1 == t2 then 0
      else
        let c = Stdlib.compare hash1 hash2 in
        if c <> 0 then c
        else
          let c = Name.compare name1 name2 in
          if c <> 0 then c
          else Prefix.compare prefix1 prefix2


    let equal u1 u2 =
      if u1 == u2 then true
      else compare u1 u2 = 0

    let print fmt unit =
      match unit.prefix with
        [] -> Format.fprintf fmt "%a" Name.print unit.name
      | _ :: _ ->
          Format.fprintf fmt "%a.%a"
            Prefix.print unit.prefix
            Name.print unit.name

    let output oc t =
      print (Format.formatter_of_out_channel oc) t

    let hash u = u.hash
  end)

let name unit = unit.name

let prefix unit = unit.prefix

let full_path unit =
  Format.asprintf "%a" print unit
