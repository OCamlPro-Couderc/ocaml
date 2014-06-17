(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Consistency tables: for checking consistency of module CRCs *)

type t = ((string * string option), Digest.t * string) Hashtbl.t

let create () = Hashtbl.create 13

let clear = Hashtbl.clear

exception Inconsistency of string * string * string

exception Not_available of string

let check tbl name ns crc source =
  try
    let (old_crc, old_source) = Hashtbl.find tbl (name, ns) in
    if crc <> old_crc then raise(Inconsistency(name, source, old_source))
  with Not_found ->
    Hashtbl.add tbl (name, ns) (crc, source)

let check_noadd tbl name ns crc source =
  try
    let (old_crc, old_source) = Hashtbl.find tbl (name, ns) in
    if crc <> old_crc then raise(Inconsistency(name, source, old_source))
  with Not_found ->
    raise (Not_available name)

let set tbl name ns crc source = Hashtbl.add tbl (name, ns) (crc, source)

let source tbl name ns = snd (Hashtbl.find tbl (name, ns))

let extract l tbl =
  List.fold_left
    (fun assc (name, ns) ->
     try
       ignore (List.find (fun (name', ns', _) -> name' = name && ns' = ns) assc);
       assc
     with Not_found ->
       try
         let (crc, _) = Hashtbl.find tbl (name, ns) in
           (name, ns, Some crc) :: assc
       with Not_found ->
         (name, ns, None) :: assc)
    [] l

let filter p tbl =
  let to_remove = ref [] in
  Hashtbl.iter
    (fun (name, ns) (crc, auth) ->
      if not (p (name, ns)) then to_remove := (name, ns) :: !to_remove)
    tbl;
  List.iter
    (fun (name, ns) ->
       while Hashtbl.mem tbl (name, ns) do Hashtbl.remove tbl (name, ns) done)
    !to_remove
