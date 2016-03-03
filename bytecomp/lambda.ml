(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Misc
open Path
open Asttypes
open Types

type compile_time_constant =
  | Big_endian
  | Word_size
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin

type loc_kind =
  | Loc_FILE
  | Loc_LINE
  | Loc_MODULE
  | Loc_LOC
  | Loc_POS

type primitive =
    Pidentity
  | Pignore
  | Prevapply of Location.t
  | Pdirapply of Location.t
  | Ploc of loc_kind
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int
  | Psetfloatfield of int
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray of array_kind
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of boxed_integer
  | Pmodbint of boxed_integer
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pstring_set_16 of bool
  | Pstring_set_32 of bool
  | Pstring_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer
  (* Integer to external pointer *)
  | Pint_as_pointer

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt | Variable

type meth_kind = Self | Public | Cached

type shared_code = (int * int) list

type lambda_expr =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda * lambda list * Location.t
  | Lfunction of function_kind * Ident.t list * lambda
  | Llet of let_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list
  | Lswitch of lambda * lambda_switch
  | Lstringswitch of lambda * (string * switch_case_extra * lambda) list * lambda option
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * Ident.t list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * Location.t
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda

and lambda =
  { lb_expr: lambda_expr;
    lb_tt_type: Types.typedtree_type option;
    lb_from: string option;
  }
  
and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * switch_case_extra * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * switch_case_extra * lambda) list;     (* Tag block cases *)
    sw_failaction : lambda option}      (* Action to take if failure *)

and switch_case_extra =
  { pattern_type: type_expr option;
    pattern_from: string option;
  }
and lambda_event =
  { lev_loc: Location.t;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.summary }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function

let const_unit = Const_pointer 0

let lambda_unit =
  { lb_expr = Lconst const_unit;
    lb_tt_type = Some (Val Predef.type_unit);
    lb_from = None;
  }

let mk_lambda ?ty ?from l =
  { lb_expr = l; lb_tt_type = ty; lb_from = from }

let as_int = mk_lambda ~ty:(Val Predef.type_int)
let as_char = mk_lambda ~ty:(Val Predef.type_char)
let as_string = mk_lambda ~ty:(Val Predef.type_string)
let as_bytes = mk_lambda ~ty:(Val Predef.type_bytes)
let as_float = mk_lambda ~ty:(Val Predef.type_float)
let as_bool = mk_lambda ~ty:(Val Predef.type_bool)
let as_unit = mk_lambda ~ty:(Val Predef.type_unit)
let as_exn = mk_lambda ~ty:(Val Predef.type_exn)
let as_array ?from ty l = mk_lambda ~ty:(Val (Predef.type_array ty)) ?from l
let as_list ?from ty l = mk_lambda ~ty:(Val (Predef.type_list ty)) ?from l
let as_option ?from ty l = mk_lambda ~ty:(Val (Predef.type_option ty)) ?from l
let as_nativeint = mk_lambda ~ty:(Val Predef.type_nativeint)
let as_int32 = mk_lambda ~ty:(Val Predef.type_int32)
let as_int64 = mk_lambda ~ty:(Val Predef.type_int64)
let as_lazy_t ?from ty l = mk_lambda ~ty:(Val (Predef.type_lazy_t ty)) ?from l

let as_arg ?from arg l = { arg with lb_expr = l; lb_from = from }
let as_constr_arg ?from arg extract l =
  let ty = match arg.lb_tt_type with
      Some (Val ({ desc = Tconstr (p, tys, _) })) ->
        (try Some (Val (extract p tys)) with _ -> None)
    | Some (Val ty) ->
        begin
          match Btype.repr ty with
            { desc = Tconstr (p, tys, _) } ->
              (try Some (Val (extract p tys)) with _ -> None)
          | _ -> None
        end 
    | _ -> None in
  { lb_expr = l; lb_from = from; lb_tt_type = ty }

let as_constr_arg1 ?from arg extract l =
  as_constr_arg ?from arg (fun p tys ->
      match tys with ty :: _ -> extract p ty | _ -> raise Not_found) l

let as_constr_arg2 ?from arg extract l =
  as_constr_arg ?from arg (fun p tys ->
      match tys with [ty1; ty2] -> extract p ty1 ty2 | _ -> raise Not_found) l

let as_constr_arg3 ?from arg extract l =
  as_constr_arg ?from arg (fun p tys ->
      match tys with [ty1; ty2; ty3] -> extract p ty1 ty2 ty3
                   | _ -> raise Not_found) l

let as_constr_arg4 ?from arg extract l =
  as_constr_arg ?from arg (fun p tys ->
      match tys with [ty1; ty2; ty3; ty4] -> extract p ty1 ty2 ty3 ty4
                   | _ -> raise Not_found) l

let as_tuple_arg ?from arg pos l =
  let ty = match arg.lb_tt_type with
      Some (Val { desc = Ttuple tys }) ->
        (try Some (Val (List.nth tys pos)) with Not_found -> None)
    | _ -> None in
  { lb_expr = l; lb_from = from; lb_tt_type = ty }


let mk_switch_extr ?ty ?from () =
  { pattern_type = ty; pattern_from = from; }

let mk_switch_extr_as ?from l =
  let ty =
    match l.lb_tt_type with
      Some (Val ty) -> Some ty
    | _ -> None in
  mk_switch_extr ?ty ?from ()

let mk_lambda_as_extr extr ?from l =
  let from =
    match extr.pattern_from, from with
      None, None -> None
    | Some s, None | None, Some s -> Some s
    | Some extr, Some from -> Some (Printf.sprintf "%s+%s" extr from)
  in
  let ty =
    match extr.pattern_type with
      None -> None | Some ty -> Some (Val ty) in
  { lb_expr = l;
    lb_tt_type = ty;
    lb_from = from;
  }

(* Build sharing keys *)
(*
   Those keys are later compared with Pervasives.compare.
   For that reason, they should not include cycles.
*)

exception Not_simple

let max_raw = 32

let make_key e =
  let count = ref 0   (* Used for controling size *)
  and make_key = Ident.make_key_generator () in
  (* make_key is used for normalizing let-bound variables *)
  let rec tr_rec env e = 
    let e = { e with  lb_tt_type = None; lb_from = None } in
    let mk = mk_lambda ?ty:None ~from:"make_key" in
    incr count ;
    if !count > max_raw then raise Not_simple ; (* Too big ! *)
    match e.lb_expr with
    | Lvar id ->
      begin
        try Ident.find_same id env
        with Not_found -> e
      end
    | Lconst  (Const_base (Const_string _)|Const_float_array _) ->
        (* Mutable constants are not shared *)
        raise Not_simple
    | Lconst _ -> e
    | Lapply (e,es,loc) ->
        { e with
          lb_expr = Lapply (tr_rec env e,tr_recs env es,Location.none) }
    | Llet (Alias,x,ex,e) -> (* Ignore aliases -> substitute *)
        let ex = tr_rec env ex in
        tr_rec (Ident.add x ex env) e
    | Llet (str,x,ex,e) ->
     (* Because of side effects, keep other lets with normalized names *)
        let ex = tr_rec env ex in
        let y = make_key x in
        { e with
          lb_expr =
            Llet (str,y,ex,tr_rec
                    (Ident.add x (mk @@ Lvar y) env) e) }
    | Lprim (p,es) ->
        { e with
          lb_expr = Lprim (p,tr_recs env es) }
    | Lswitch (e,sw) ->
        { e with
          lb_expr = Lswitch (tr_rec env e,tr_sw env sw) }
    | Lstringswitch (e,sw,d) ->
       { e with
         lb_expr = Lstringswitch
           (tr_rec env e,
            List.map (fun (s,extr,e) -> s,extr,tr_rec env e) sw,
            tr_opt env d) }
    | Lstaticraise (i,es) ->
        { e with
          lb_expr = Lstaticraise (i,tr_recs env es) }
    | Lstaticcatch (e1,xs,e2) ->
        { e with
          lb_expr = Lstaticcatch (tr_rec env e1,xs,tr_rec env e2) }
    | Ltrywith (e1,x,e2) ->
        { e with
          lb_expr = Ltrywith (tr_rec env e1,x,tr_rec env e2) }
    | Lifthenelse (cond,ifso,ifnot) ->
        { e with
          lb_expr = Lifthenelse (tr_rec env cond,tr_rec env ifso,tr_rec env ifnot) }
    | Lsequence (e1,e2) ->
        { e with
          lb_expr = Lsequence (tr_rec env e1,tr_rec env e2) }
    | Lassign (x,e) ->
        { e with
          lb_expr = Lassign (x,tr_rec env e) }
    | Lsend (m,e1,e2,es,loc) ->
        { e with
          lb_expr = Lsend (m,tr_rec env e1,tr_rec env e2,tr_recs env es,Location.none) }
    | Lifused (id,e) ->
        { e with
          lb_expr = Lifused (id,tr_rec env e) }
    | Lletrec _|Lfunction _
    | Lfor _ | Lwhile _
(* Beware: (PR#6412) the event argument to Levent
   may include cyclic structure of type Type.typexpr *)
    | Levent _  ->
        raise Not_simple

  and tr_recs env es = List.map (tr_rec env) es

  and tr_sw env sw =
    { sw with
      sw_consts = List.map (fun (i, extr, e) -> i, extr, tr_rec env e) sw.sw_consts ;
      sw_blocks = List.map (fun (i, extr, e) -> i, extr, tr_rec env e) sw.sw_blocks ;
      sw_failaction = tr_opt env sw.sw_failaction ; }

  and tr_opt env = function
    | None -> None
    | Some e -> Some (tr_rec env e) in

  try
    Some (tr_rec Ident.empty e)
  with Not_simple -> None

(***************)

let name_lambda strict arg fn =
  match arg.lb_expr with
    Lvar id -> fn id
  | _ -> let id = Ident.create "let" in
      { arg with lb_expr = Llet(strict, id, arg, fn id) }

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | ({ lb_expr = Lvar id } as arg) :: rem ->
      name_list (arg :: names) rem
  | arg :: rem ->
      let id = Ident.create "let" in
      let name_list_rem =  name_list ({arg with lb_expr = Lvar id} :: names) rem in
      as_arg ~from:"name_lambda_list" name_list_rem @@ 
      Llet(Strict, id, arg, name_list_rem) in
  name_list [] args


let iter_opt f = function
  | None -> ()
  | Some e -> f e

let iter f l =
  match l.lb_expr with
    Lvar _
  | Lconst _ -> ()
  | Lapply(fn, args, _) ->
      f fn; List.iter f args
  | Lfunction(kind, params, body) ->
      f body
  | Llet(str, id, arg, body) ->
      f arg; f body
  | Lletrec(decl, body) ->
      f body;
      List.iter (fun (id, exp) -> f exp) decl
  | Lprim(p, args) ->
      List.iter f args
  | Lswitch(arg, sw) ->
      f arg;
      List.iter (fun (key, extr, case) -> f case) sw.sw_consts;
      List.iter (fun (key, extr, case) -> f case) sw.sw_blocks;
      iter_opt f sw.sw_failaction
  | Lstringswitch (arg,cases,default) ->
      f arg ;
      List.iter (fun (_,_,act) -> f act) cases ;
      iter_opt f default
  | Lstaticraise (_,args) ->
      List.iter f args
  | Lstaticcatch(e1, (_,vars), e2) ->
      f e1; f e2
  | Ltrywith(e1, exn, e2) ->
      f e1; f e2
  | Lifthenelse(e1, e2, e3) ->
      f e1; f e2; f e3
  | Lsequence(e1, e2) ->
      f e1; f e2
  | Lwhile(e1, e2) ->
      f e1; f e2
  | Lfor(v, e1, e2, dir, e3) ->
      f e1; f e2; f e3
  | Lassign(id, e) ->
      f e
  | Lsend (k, met, obj, args, _) ->
      List.iter f (met::obj::args)
  | Levent (lam, evt) ->
      f lam
  | Lifused (v, e) ->
      f e


module IdentSet =
  Set.Make(struct
    type t = Ident.t
    let compare = compare
  end)

let free_ids get l =
  let fv = ref IdentSet.empty in
  let rec free l =
    iter free l;
    fv := List.fold_right IdentSet.add (get l) !fv;
    match l.lb_expr with
      Lfunction(kind, params, body) ->
        List.iter (fun param -> fv := IdentSet.remove param !fv) params
    | Llet(str, id, arg, body) ->
        fv := IdentSet.remove id !fv
    | Lletrec(decl, body) ->
        List.iter (fun (id, exp) -> fv := IdentSet.remove id !fv) decl
    | Lstaticcatch(e1, (_,vars), e2) ->
        List.iter (fun id -> fv := IdentSet.remove id !fv) vars
    | Ltrywith(e1, exn, e2) ->
        fv := IdentSet.remove exn !fv
    | Lfor(v, e1, e2, dir, e3) ->
        fv := IdentSet.remove v !fv
    | Lassign(id, e) ->
        fv := IdentSet.add id !fv
    | Lvar _ | Lconst _ | Lapply _
    | Lprim _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
    | Lifthenelse _ | Lsequence _ | Lwhile _
    | Lsend _ | Levent _ | Lifused _ -> ()
  in free l; !fv

let free_variables l =
  free_ids (function { lb_expr = Lvar id } -> [id] | _ -> []) l

let free_methods l =
  free_ids
    (function
        { lb_expr = Lsend(Self, { lb_expr = Lvar meth }, obj, _, _) } -> [meth]
      | _ -> []) l

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count ;
  !raise_count

let negative_raise_count = ref 0

let next_negative_raise_count () =
  decr negative_raise_count ;
  !negative_raise_count

(* Anticipated staticraise, for guards *)
let staticfail =
  mk_lambda ?ty:None ~from:"staticfail" @@ Lstaticraise (0,[])

let rec is_guarded l =
  match l.lb_expr with
  | Lifthenelse( cond, body, { lb_expr = Lstaticraise (0,[])}) -> true
  | Llet(str, id, lam, body) -> is_guarded body
  | Levent(lam, ev) -> is_guarded lam
  | _ -> false

let rec patch_guarded patch l =
  match l.lb_expr with
  | Lifthenelse (cond, body, { lb_expr = Lstaticraise (0,[])}) ->
      { l with lb_expr = Lifthenelse (cond, body, patch) }
  | Llet(str, id, lam, body) ->
      { l with lb_expr = Llet (str, id, lam, patch_guarded patch body) }
  | Levent(lam, ev) ->
      { l with lb_expr = Levent (patch_guarded patch lam, ev) }
  | _ -> fatal_error "Lambda.patch_guarded"

(* Translate an access path *)

let rec transl_normal_path ?ty env = function
    Pident id ->
      if Ident.global id then
        mk_lambda ?ty ~from:"transl_normal_path" @@
        Lprim(Pgetglobal id, [])
      else mk_lambda ?ty ~from:"transl_normal_path" @@ Lvar id
  | Pdot(p, s, pos) ->
      let ty' =
        try Some (Mod (Env.find_module p env).md_type)
        with Not_found -> None in
      mk_lambda ?ty  ~from:"transl_normal_path" @@
      Lprim(Pfield pos, [transl_normal_path ?ty:ty' env p])
  | Papply(p1, p2) ->
      fatal_error "Lambda.transl_path"

(* Translation of value identifiers *)

let transl_path ?(loc=Location.none) ?ty env path =
  transl_normal_path env ?ty (Env.normalize_path (Some loc) env path)

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem ->
      let lam = fn x in
      let lam' = make_sequence fn rem in
      as_arg ~from:"make_sequence" lam' @@ Lsequence(lam, lam')

(* Apply a substitution to a lambda-term.
   Assumes that the bound variables of the lambda-term do not
   belong to the domain of the substitution.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst_lambda s lam =
  let rec subst l =
    match l.lb_expr with
    Lvar id ->
      begin try Ident.find_same id s with Not_found -> l end
  | Lconst sc -> l
  | Lapply(fn, args, loc) ->
      { l with lb_expr = Lapply(subst fn, List.map subst args, loc) }
  | Lfunction(kind, params, body) ->
      { l with lb_expr = Lfunction(kind, params, subst body) }
  | Llet(str, id, arg, body) ->
      { l with lb_expr = Llet(str, id, subst arg, subst body) }
  | Lletrec(decl, body) ->
      { l with lb_expr = Lletrec(List.map subst_decl decl, subst body) }
  | Lprim(p, args) ->
      { l with lb_expr = Lprim(p, List.map subst args) }
  | Lswitch(arg, sw) ->
      { l with
        lb_expr =
          Lswitch(subst arg,
                  {sw with sw_consts = List.map subst_case sw.sw_consts;
                           sw_blocks = List.map subst_case sw.sw_blocks;
                           sw_failaction = subst_opt  sw.sw_failaction; }) }
  | Lstringswitch (arg,cases,default) ->
      { l with lb_expr =
                 Lstringswitch
                   (subst arg,List.map subst_strcase cases,subst_opt default) }
  | Lstaticraise (i,args) ->
      { l with lb_expr = Lstaticraise (i, List.map subst args) }
  | Lstaticcatch(e1, io, e2) ->
      { l with lb_expr = Lstaticcatch(subst e1, io, subst e2) }
  | Ltrywith(e1, exn, e2) ->
      { l with lb_expr = Ltrywith(subst e1, exn, subst e2) }
  | Lifthenelse(e1, e2, e3) ->
      { l with lb_expr = Lifthenelse(subst e1, subst e2, subst e3) }
  | Lsequence(e1, e2) ->
      { l with lb_expr = Lsequence(subst e1, subst e2) }
  | Lwhile(e1, e2) ->
      { l with lb_expr = Lwhile(subst e1, subst e2) }
  | Lfor(v, e1, e2, dir, e3) ->
      { l with lb_expr = Lfor(v, subst e1, subst e2, dir, subst e3) }
  | Lassign(id, e) ->
      { l with lb_expr = Lassign(id, subst e) }
  | Lsend (k, met, obj, args, loc) ->
      { l with
        lb_expr = Lsend (k, subst met, subst obj, List.map subst args, loc) }
  | Levent (lam, evt) ->
      { l with lb_expr = Levent (subst lam, evt) }
  | Lifused (v, e) ->
      { l with lb_expr = Lifused (v, subst e) }
  and subst_decl (id, exp) = (id, subst exp)
  and subst_case (key, extr, case) = (key, extr, subst case)
  and subst_strcase (key, extr, case) = (key, extr, subst case)
  and subst_opt = function
    | None -> None
    | Some e -> Some (subst e)
  in subst lam


(* To let-bind expressions to variables *)

let bind str var exp body =
  match exp.lb_expr with
    Lvar var' when Ident.same var var' -> body
  | _ -> as_arg ~from:"bind" body @@ Llet(str, var, exp, body)

and commute_comparison = function
| Ceq -> Ceq| Cneq -> Cneq
| Clt -> Cgt | Cle -> Cge
| Cgt -> Clt | Cge -> Cle

and negate_comparison = function
| Ceq -> Cneq| Cneq -> Ceq
| Clt -> Cge | Cle -> Cgt
| Cgt -> Cle | Cge -> Clt

let raise_kind = function
  | Raise_regular -> "raise"
  | Raise_reraise -> "reraise"
  | Raise_notrace -> "raise_notrace"

let loc_type =
  Val (Btype.newgenty (Ttuple [Predef.type_string; Predef.type_int; Predef.type_int]))

let lam_of_loc kind loc =
  let loc_start = loc.Location.loc_start in
  let (file, lnum, cnum) = Location.get_pos_info loc_start in
  let enum = loc.Location.loc_end.Lexing.pos_cnum -
      loc_start.Lexing.pos_cnum + cnum in
  match kind with
  | Loc_POS ->
      mk_lambda ~ty:loc_type ~from:"lam_of_loc" @@
      Lconst (Const_block (0, [
          Const_immstring file;
          Const_base (Const_int lnum);
          Const_base (Const_int cnum);
          Const_base (Const_int enum);
        ]))
  | Loc_FILE ->
      as_string ~from:"lam_of_loc" @@ Lconst (Const_immstring file)
  | Loc_MODULE ->
    let filename = Filename.basename file in
    let name = Env.get_unit_name () in
    let module_name = if name = "" then "//"^filename^"//" else name in
    as_string ~from:"lam_of_loc" @@ Lconst (Const_immstring module_name)
  | Loc_LOC ->
      let loc = Printf.sprintf "File %S, line %d, characters %d-%d"
          file lnum cnum enum in
      as_string ~from:"lam_of_loc" @@ Lconst (Const_immstring loc)
  | Loc_LINE ->
      as_int ~from:"lam_of_loc" @@ Lconst (Const_base (Const_int lnum))

let reset () =
  raise_count := 0
