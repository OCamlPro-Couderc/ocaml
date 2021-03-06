(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: Address Damien's comments on this file from the
   Asm_directives GPR *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  section : Asm_section.t;
  object_file : Object_file.t;
  name : string;
  never_add_prefix : bool;
}

let create backend_sym =
  let section : Asm_section.t =
    match Backend_sym.kind backend_sym with
    | Text -> Text
    | Data -> Data
  in
  let object_file = Backend_sym.object_file backend_sym in
  let name = Backend_sym.name_for_asm_symbol backend_sym in
  if String.length name <= 0 then begin
    Misc.fatal_errorf "[Backend_sym] returned an empty name for %a"
      Backend_sym.print backend_sym
  end;
  { section;
    object_file;
    name;
    never_add_prefix = false;
  }

let of_external_name ?no_prefix section object_file name =
  if String.length name <= 0 then begin
    Misc.fatal_error "[Asm_symbol.of_external_name] with empty symbol name"
  end;
  let never_add_prefix = Option.is_some no_prefix in
  { section;
    object_file;
    name;
    never_add_prefix;
  }

let add_prefix t section object_file ~prefix =
  { section;
    object_file;
    name = prefix ^ t.name;
    never_add_prefix = t.never_add_prefix;
  }

let section t = t.section

let object_file t = t.object_file

let symbol_prefix t =
  if t.never_add_prefix then ""
  else
    match Target_system.architecture () with
    | IA32 ->
      begin match Target_system.system () with
      | Linux
      | FreeBSD
      | NetBSD
      | OpenBSD
      | Generic_BSD
      | Solaris
      | BeOS
      | GNU
      | Dragonfly -> ""
      | Windows Cygwin
      | Windows MinGW
      | Windows Native
      | Unknown
      | MacOS_like -> "_"
      end
    | X86_64 ->
      begin match Target_system.system () with
      | Linux
      | Windows Cygwin
      | Windows MinGW
      | FreeBSD
      | NetBSD
      | OpenBSD
      | Generic_BSD
      | Solaris
      | BeOS
      | GNU
      | Dragonfly
      | Windows Native
      | Unknown -> ""
      | MacOS_like -> "_"
      end
    | ARM
    | AArch64 -> "$"
    | POWER
    | Z -> "."

let escape name =
  let spec = ref false in
  for i = 0 to String.length name - 1 do
    match String.unsafe_get name i with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ()
    | _ -> spec := true;
  done;
  if not !spec then begin
    name
  end else begin
    let b = Buffer.create (String.length name + 10) in
    String.iter
      (function
        | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c -> Buffer.add_char b c
        | c -> Printf.bprintf b "$%02x" (Char.code c)
      )
      name;
    Buffer.contents b
  end

let to_escaped_string ?(reloc = "") t =
  (* The prefix and relocation must not be escaped! *)
  (symbol_prefix t) ^ (escape t.name) ^ reloc

let to_string ?without_prefix t =
  let symbol_prefix =
    match without_prefix with
    | None -> symbol_prefix t
    | Some () -> ""
  in
  symbol_prefix ^ t.name

let is_generic_function t =
  List.exists (fun prefix -> Misc.Stdlib.String.is_prefix t.name ~prefix)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

include Identifiable.Make (struct
  type nonrec t = t

  let compare { section = section1; object_file = object_file1;
                name = name1; never_add_prefix = never_add_prefix1;
              }
              { section = section2; object_file = object_file2;
                name = name2; never_add_prefix = never_add_prefix2;
              } =
    let c = Asm_section.compare section1 section2 in
    if c <> 0 then c
    else
      let c = Object_file.compare object_file1 object_file2 in
      if c <> 0 then c
      else
        let c = String.compare name1 name2 in
        if c <> 0 then c
        else
          Stdlib.compare never_add_prefix1 never_add_prefix2

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash { section; object_file; name; never_add_prefix; } =
    Hashtbl.hash (
      Asm_section.hash section,
      Object_file.hash object_file,
      name,
      never_add_prefix)

  let print ppf t = Format.pp_print_string ppf (to_string t)

  let output chan t = print (Format.formatter_of_out_channel chan) t
end)

module Names = struct
  let runtime = Object_file.runtime_and_external_libs

  let caml_young_ptr =
    of_external_name Data runtime "caml_young_ptr"

  let caml_young_limit =
    of_external_name Data runtime "caml_young_limit"

  let caml_exception_pointer =
    of_external_name Data runtime "caml_exception_pointer"

  let caml_negf_mask =
    of_external_name Eight_byte_literals Object_file.current_compilation_unit
      "caml_negf_mask"

  let caml_absf_mask =
    of_external_name Eight_byte_literals Object_file.current_compilation_unit
      "caml_absf_mask"

  let caml_call_gc =
    of_external_name Text runtime "caml_call_gc"

  let caml_c_call =
    of_external_name Text runtime "caml_c_call"

  let caml_alloc1 =
    of_external_name Text runtime "caml_alloc1"

  let caml_alloc2 =
    of_external_name Text runtime "caml_alloc2"

  let caml_alloc3 =
    of_external_name Text runtime "caml_alloc3"

  let caml_allocN =
    of_external_name Text runtime "caml_allocN"

  let caml_ml_array_bound_error =
    of_external_name Text runtime "caml_ml_array_bound_error"

  let caml_raise_exn =
    of_external_name Text runtime "caml_raise_exn"
end
