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

(* Elimination of useless Llet(Alias) bindings.
   Also transform let-bound references into variables. *)

open Asttypes
open Lambda

let simplif_annot = ref false
let update_from from (annot:string) =
  if !simplif_annot then
    match from with
      None -> Some (Printf.sprintf "#unknown(+%s)" annot)
    | Some s -> Some (Printf.sprintf "%s(+%s)" s annot)
  else
    from

(* To transform let-bound references into variables *)

exception Real_reference

let rec eliminate_ref id lam =
  let mk l = { lam with
               lb_expr = l;
               lb_from = update_from lam.lb_from "eliminate_ref"}
  and mk_u l =
    { lb_expr = l;
      lb_tt_type = None;
      lb_from = update_from lam.lb_from "eliminate_ref" } in
  match lam.lb_expr with
    Lvar v ->
      if Ident.same v id then raise Real_reference else lam
  | Lconst cst -> lam
  | Lapply(e1, el, loc) ->
      mk @@ Lapply(eliminate_ref id e1, List.map (eliminate_ref id) el, loc)
  | Lfunction(kind, params, body) ->
      if IdentSet.mem id (free_variables lam)
      then raise Real_reference
      else lam
  | Llet(str, v, e1, e2) ->
      mk @@ Llet(str, v, eliminate_ref id e1, eliminate_ref id e2)
  | Lletrec(idel, e2) ->
      mk @@ Lletrec(List.map (fun (v, e) -> (v, eliminate_ref id e)) idel,
              eliminate_ref id e2)
  | Lprim(Pfield 0, [{ lb_expr = Lvar v }]) when Ident.same v id ->
      mk @@ Lvar id
  | Lprim(Psetfield(0, _), [{ lb_expr = Lvar v }; e]) when Ident.same v id ->
      mk @@ Lassign(id, eliminate_ref id e)
  | Lprim(Poffsetref delta, [{ lb_expr = Lvar v }]) when Ident.same v id ->
      mk @@ Lassign(id, mk_u @@ Lprim(Poffsetint delta, [mk_u @@ Lvar id]))
  | Lprim(p, el) ->
      mk @@ Lprim(p, List.map (eliminate_ref id) el)
  | Lswitch(e, sw) ->
      mk @@ Lswitch(eliminate_ref id e,
        {sw_numconsts = sw.sw_numconsts;
         sw_consts =
            List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_consts;
         sw_numblocks = sw.sw_numblocks;
         sw_blocks =
            List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_blocks;
         sw_failaction =
            Misc.may_map (eliminate_ref id) sw.sw_failaction; })
  | Lstringswitch(e, sw, default) ->
      mk @@ Lstringswitch
        (eliminate_ref id e,
         List.map (fun (s, e) -> (s, eliminate_ref id e)) sw,
         Misc.may_map (eliminate_ref id) default)
  | Lstaticraise (i,args) ->
      mk @@ Lstaticraise (i,List.map (eliminate_ref id) args)
  | Lstaticcatch(e1, i, e2) ->
      mk @@ Lstaticcatch(eliminate_ref id e1, i, eliminate_ref id e2)
  | Ltrywith(e1, v, e2) ->
      mk @@ Ltrywith(eliminate_ref id e1, v, eliminate_ref id e2)
  | Lifthenelse(e1, e2, e3) ->
      mk @@
      Lifthenelse(eliminate_ref id e1,
                  eliminate_ref id e2,
                  eliminate_ref id e3)
  | Lsequence(e1, e2) ->
      mk @@ Lsequence(eliminate_ref id e1, eliminate_ref id e2)
  | Lwhile(e1, e2) ->
      mk @@ Lwhile(eliminate_ref id e1, eliminate_ref id e2)
  | Lfor(v, e1, e2, dir, e3) ->
      mk @@
      Lfor(v, eliminate_ref id e1, eliminate_ref id e2,
           dir, eliminate_ref id e3)
  | Lassign(v, e) ->
      mk @@
      Lassign(v, eliminate_ref id e)
  | Lsend(k, m, o, el, loc) ->
      mk @@
      Lsend(k, eliminate_ref id m, eliminate_ref id o,
            List.map (eliminate_ref id) el, loc)
  | Levent(l, ev) ->
      mk @@
      Levent(eliminate_ref id l, ev)
  | Lifused(v, e) ->
      mk @@
      Lifused(v, eliminate_ref id e)

(* Simplification of exits *)

let simplify_exits lam =

  (* Count occurrences of (exit n ...) statements *)
  let exits = Hashtbl.create 17 in

  let count_exit i =
    try
      !(Hashtbl.find exits i)
    with
    | Not_found -> 0

  and incr_exit i =
    try
      incr (Hashtbl.find exits i)
    with
    | Not_found -> Hashtbl.add exits i (ref 1) in

  let rec count l =
    match l.lb_expr with
  | (Lvar _| Lconst _) -> ()
  | Lapply(l1, ll, _) -> count l1; List.iter count ll
  | Lfunction(kind, params, l) -> count l
  | Llet(str, v, l1, l2) ->
      count l2; count l1
  | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count l) bindings;
      count body
  | Lprim(p, ll) -> List.iter count ll
  | Lswitch(l, sw) ->
      count_default sw ;
      count l;
      List.iter (fun (_, l) -> count l) sw.sw_consts;
      List.iter (fun (_, l) -> count l) sw.sw_blocks
  | Lstringswitch(l, sw, d) ->
      count l;
      List.iter (fun (_, l) -> count l) sw;
      begin match  d with
      | None -> ()
      | Some d -> match sw with
        | []|[_] -> count d
        | _ -> count d; count d (* default will get replicated *)
      end
  | Lstaticraise (i,ls) -> incr_exit i ; List.iter count ls
  | Lstaticcatch (l1,(i,[]), {lb_expr = Lstaticraise (j,[])}) ->
      (* i will be replaced by j in l1, so each occurence of i in l1
         increases j's ref count *)
      count l1 ;
      let ic = count_exit i in
      begin try
        let r = Hashtbl.find exits j in r := !r + ic
      with
      | Not_found ->
          Hashtbl.add exits j (ref ic)
      end
  | Lstaticcatch(l1, (i,_), l2) ->
      count l1;
      (* If l1 does not contain (exit i),
         l2 will be removed, so don't count its exits *)
      if count_exit i > 0 then
        count l2
  | Ltrywith(l1, v, l2) -> count l1; count l2
  | Lifthenelse(l1, l2, l3) -> count l1; count l2; count l3
  | Lsequence(l1, l2) -> count l1; count l2
  | Lwhile(l1, l2) -> count l1; count l2
  | Lfor(_, l1, l2, dir, l3) -> count l1; count l2; count l3
  | Lassign(v, l) -> count l
  | Lsend(k, m, o, ll, _) -> List.iter count (m::o::ll)
  | Levent(l, _) -> count l
  | Lifused(v, l) -> count l

  and count_default sw = match sw.sw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then begin (* default action will occur twice in native code *)
        count al ; count al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
        count al
      end
  in
  count lam;

  (*
     Second pass simplify  ``catch body with (i ...) handler''
      - if (exit i ...) does not occur in body, suppress catch
      - if (exit i ...) occurs exactly once in body,
        substitute it with handler
      - If handler is a single variable, replace (exit i ..) with it
   Note:
    In ``catch body with (i x1 .. xn) handler''
     Substituted expression is
      let y1 = x1 and ... yn = xn in
      handler[x1 <- y1 ; ... ; xn <- yn]
     For the sake of preserving the uniqueness  of bound variables.
     (No alpha conversion of ``handler'' is presently needed, since
     substitution of several ``(exit i ...)''
     occurs only when ``handler'' is a variable.)
  *)

  let subst = Hashtbl.create 17 in

  let rec simplif l =
    let mk l' =
      { l with lb_expr = l'; lb_from = update_from l.lb_from "simplify_exits" } in
    match l.lb_expr with
  | (Lvar _|Lconst _) -> l
  | Lapply(l1, ll, loc) -> mk @@ Lapply(simplif l1, List.map simplif ll, loc)
  | Lfunction(kind, params, l) -> mk @@ Lfunction(kind, params, simplif l)
  | Llet(kind, v, l1, l2) -> mk @@ Llet(kind, v, simplif l1, simplif l2)
  | Lletrec(bindings, body) ->
      mk @@
      Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
  | Lprim(p, ll) -> begin
    let ll = List.map simplif ll in
    match p, ll with
        (* Simplify %revapply, for n-ary functions with n > 1 *)
      | Prevapply loc, [x; {lb_expr = Lapply(f, args, _)}]
      | Prevapply loc, [x; {lb_expr = Levent ({lb_expr = Lapply(f, args, _)},_)}] ->
        mk @@ Lapply(f, args@[x], loc)
      | Prevapply loc, [x; f] -> mk @@ Lapply(f, [x], loc)

        (* Simplify %apply, for n-ary functions with n > 1 *)
      | Pdirapply loc, [{lb_expr = Lapply(f, args, _)}; x]
      | Pdirapply loc, [{lb_expr = Levent ({lb_expr = Lapply(f, args, _)},_)}; x] ->
        mk @@ Lapply(f, args@[x], loc)
      | Pdirapply loc, [f; x] -> mk @@ Lapply(f, [x], loc)

      | _ -> mk @@ Lprim(p, ll)
     end
  | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = Misc.may_map simplif sw.sw_failaction in
      mk @@
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
  | Lstringswitch(l,sw,d) ->
      mk @@
      Lstringswitch
        (simplif l,List.map (fun (s,l) -> s,simplif l) sw,
         Misc.may_map simplif d)
  | Lstaticraise (i,[]) ->
      begin try
        let _,handler =  Hashtbl.find subst i in
        handler
      with
      | Not_found -> l
      end
  | Lstaticraise (i,ls) ->
      let ls = List.map simplif ls in
      begin try
        let xs,handler =  Hashtbl.find subst i in
        let ys = List.map Ident.rename xs in
        let env =
          List.fold_right2
            (fun x y t -> Ident.add x (mk_lambda @@ Lvar y) t)
            xs ys Ident.empty in
        List.fold_right2
          (fun y l r ->
             { r with lb_expr = Llet (Alias, y, l, r) })
          ys ls (Lambda.subst_lambda env handler)
      with
      | Not_found -> mk @@ Lstaticraise (i,ls)
      end
  | Lstaticcatch (l1,(i,[]),({lb_expr = Lstaticraise (j,[])} as l2)) ->
      Hashtbl.add subst i ([],simplif l2) ;
      simplif l1
  | Lstaticcatch (l1,(i,xs),l2) ->
      begin match count_exit i with
      | 0 -> simplif l1
      | 1 when i >= 0 ->
          Hashtbl.add subst i (xs,simplif l2) ;
          simplif l1
      | _ ->
          mk @@ Lstaticcatch (simplif l1, (i,xs), simplif l2)
      end
  | Ltrywith(l1, v, l2) -> mk @@ Ltrywith(simplif l1, v, simplif l2)
  | Lifthenelse(l1, l2, l3) -> mk @@ Lifthenelse(simplif l1, simplif l2, simplif l3)
  | Lsequence(l1, l2) -> mk @@ Lsequence(simplif l1, simplif l2)
  | Lwhile(l1, l2) -> mk @@ Lwhile(simplif l1, simplif l2)
  | Lfor(v, l1, l2, dir, l3) ->
      mk @@ Lfor(v, simplif l1, simplif l2, dir, simplif l3)
  | Lassign(v, l) -> mk @@ Lassign(v, simplif l)
  | Lsend(k, m, o, ll, loc) ->
      mk @@ Lsend(k, simplif m, simplif o, List.map simplif ll, loc)
  | Levent(l, ev) -> mk @@ Levent(simplif l, ev)
  | Lifused(v, l) -> mk @@ Lifused (v,simplif l)
  in
  simplif lam

(* Compile-time beta-reduction of functions immediately applied:
      Lapply(Lfunction(Curried, params, body), args, loc) ->
        let paramN = argN in ... let param1 = arg1 in body
      Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock(args))], loc) ->
        let paramN = argN in ... let param1 = arg1 in body
   Assumes |args| = |params|.
*)

let beta_reduce params body args =
  List.fold_left2 (fun l param arg ->
      { l with lb_expr = Llet(Strict, param, arg, l);
               lb_from = update_from l.lb_from "beta_reduce"; })
    body params args

(* Simplification of lets *)

let simplify_lets lam =

  (* Disable optimisations for bytecode compilation with -g flag *)
  let optimize = !Clflags.native_code || not !Clflags.debug in

  (* First pass: count the occurrences of all let-bound identifiers *)

  let occ = (Hashtbl.create 83: (Ident.t, int ref) Hashtbl.t) in
  (* The global table [occ] associates to each let-bound identifier
     the number of its uses (as a reference):
     - 0 if never used
     - 1 if used exactly once in and not under a lambda or within a loop
     - > 1 if used several times or under a lambda or within a loop.
     The local table [bv] associates to each locally-let-bound variable
     its reference count, as above.  [bv] is enriched at let bindings
     but emptied when crossing lambdas and loops. *)

  (* Current use count of a variable. *)
  let count_var v =
    try
      !(Hashtbl.find occ v)
    with Not_found ->
      0

  (* Entering a [let].  Returns updated [bv]. *)
  and bind_var bv v =
    let r = ref 0 in
    Hashtbl.add occ v r;
    Tbl.add v r bv

  (* Record a use of a variable *)
  and use_var bv v n =
    try
      let r = Tbl.find v bv in r := !r + n
    with Not_found ->
      (* v is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
    try
      let r = Hashtbl.find occ v in r := !r + 2
    with Not_found ->
      (* Not a let-bound variable, ignore *)
      () in

  let rec count bv l =
    match l.lb_expr with
  | Lconst cst -> ()
  | Lvar v ->
      use_var bv v 1
  | Lapply({ lb_expr = Lfunction(Curried, params, body) }, args, _)
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
  | Lapply({ lb_expr = Lfunction(Tupled, params, body)},
           [{ lb_expr = Lprim(Pmakeblock _, args)}], _)
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
  | Lapply(l1, ll, _) ->
      count bv l1; List.iter (count bv) ll
  | Lfunction(kind, params, l) ->
      count Tbl.empty l
  | Llet(str, v, { lb_expr = Lvar w}, l2) when optimize ->
      (* v will be replaced by w in l2, so each occurrence of v in l2
         increases w's refcount *)
      count (bind_var bv v) l2;
      use_var bv w (count_var v)
  | Llet(str, v, l1, l2) ->
      count (bind_var bv v) l2;
      (* If v is unused, l1 will be removed, so don't count its variables *)
      if str = Strict || count_var v > 0 then count bv l1
  | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count bv l) bindings;
      count bv body
  | Lprim(p, ll) -> List.iter (count bv) ll
  | Lswitch(l, sw) ->
      count_default bv sw ;
      count bv l;
      List.iter (fun (_, l) -> count bv l) sw.sw_consts;
      List.iter (fun (_, l) -> count bv l) sw.sw_blocks
  | Lstringswitch(l, sw, d) ->
      count bv l ;
      List.iter (fun (_, l) -> count bv l) sw ;
      begin match d with
      | Some d ->
          begin match sw with
          | []|[_] -> count bv d
          | _ -> count bv d ; count bv d
          end
      | None -> ()
      end
  | Lstaticraise (i,ls) -> List.iter (count bv) ls
  | Lstaticcatch(l1, (i,_), l2) -> count bv l1; count bv l2
  | Ltrywith(l1, v, l2) -> count bv l1; count bv l2
  | Lifthenelse(l1, l2, l3) -> count bv l1; count bv l2; count bv l3
  | Lsequence(l1, l2) -> count bv l1; count bv l2
  | Lwhile(l1, l2) -> count Tbl.empty l1; count Tbl.empty l2
  | Lfor(_, l1, l2, dir, l3) -> count bv l1; count bv l2; count Tbl.empty l3
  | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcount *)
      count bv l
  | Lsend(_, m, o, ll, _) -> List.iter (count bv) (m::o::ll)
  | Levent(l, _) -> count bv l
  | Lifused(v, l) ->
      if count_var v > 0 then count bv l

  and count_default bv sw = match sw.sw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then begin (* default action will occur twice in native code *)
        count bv al ; count bv al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
        count bv al
      end
  in
  count Tbl.empty lam;

  (* Second pass: remove Lalias bindings of unused variables,
     and substitute the bindings of variables used exactly once. *)

  let subst = Hashtbl.create 83 in

(* This (small)  optimisation is always legal, it may uncover some
   tail call later on. *)

  let mklet (kind,v,e1,e2) = match e2.lb_expr with
  | Lvar w when optimize && Ident.same v w -> e1
  | _ ->
      { e2 with lb_expr = Llet (kind,v,e1,e2);
                lb_from = update_from e2.lb_from "simplif_lets"; }  in


  let rec simplif l =
    let mk l' =
      { l with lb_expr = l'; lb_from = update_from l.lb_from "simplif_lets"; } in
    match l.lb_expr with
    Lvar v ->
      begin try
        Hashtbl.find subst v
      with Not_found ->
        l
      end
  | Lconst cst -> l
  | Lapply({ lb_expr = Lfunction(Curried, params, body)}, args, _)
    when optimize && List.length params = List.length args ->
      simplif (beta_reduce params body args)
  | Lapply({ lb_expr = Lfunction(Tupled, params, body)},
           [{ lb_expr = Lprim(Pmakeblock _, args)}], _)
    when optimize && List.length params = List.length args ->
      simplif (beta_reduce params body args)
  | Lapply(l1, ll, loc) -> mk @@ Lapply(simplif l1, List.map simplif ll, loc)
  | Lfunction(kind, params, l) -> mk @@ Lfunction(kind, params, simplif l)
  | Llet(str, v, ({ lb_expr = Lvar w } as lvar), l2) when optimize ->
      Hashtbl.add subst v (simplif lvar);
      simplif l2
  | Llet(Strict, v,
         ({lb_expr = Lprim(Pmakeblock(0, Mutable), [linit])} as lprim),
         lbody)
    when optimize ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin try
       mklet (Variable, v, slinit, eliminate_ref v slbody)
      with Real_reference ->
        mklet(Strict, v,
              { lprim with lb_expr = Lprim(Pmakeblock(0, Mutable), [slinit])},
              slbody)
      end
  | Llet(Alias, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | 1 when optimize -> Hashtbl.add subst v (simplif l1); simplif l2
      | n -> mk @@ Llet(Alias, v, simplif l1, simplif l2)
      end
  | Llet(StrictOpt, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | n -> mklet(Alias, v, simplif l1, simplif l2)
      end
  | Llet(kind, v, l1, l2) -> mklet(kind, v, simplif l1, simplif l2)
  | Lletrec(bindings, body) ->
      mk @@ Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
  | Lprim(p, ll) -> mk @@ Lprim(p, List.map simplif ll)
  | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = Misc.may_map simplif sw.sw_failaction in
      mk @@
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
  | Lstringswitch (l,sw,d) ->
      mk @@
      Lstringswitch
        (simplif l,List.map (fun (s,l) -> s,simplif l) sw,
         Misc.may_map simplif d)
  | Lstaticraise (i,ls) ->
      mk @@ Lstaticraise (i, List.map simplif ls)
  | Lstaticcatch(l1, (i,args), l2) ->
      mk @@ Lstaticcatch (simplif l1, (i,args), simplif l2)
  | Ltrywith(l1, v, l2) -> mk @@ Ltrywith(simplif l1, v, simplif l2)
  | Lifthenelse(l1, l2, l3) -> mk @@ Lifthenelse(simplif l1, simplif l2, simplif l3)
  | Lsequence({ lb_expr = Lifused(v, l1) }, l2) ->
      if count_var v > 0
      then mk @@ Lsequence(simplif l1, simplif l2)
      else simplif l2
  | Lsequence(l1, l2) -> mk @@ Lsequence(simplif l1, simplif l2)
  | Lwhile(l1, l2) -> mk @@ Lwhile(simplif l1, simplif l2)
  | Lfor(v, l1, l2, dir, l3) ->
      mk @@ Lfor(v, simplif l1, simplif l2, dir, simplif l3)
  | Lassign(v, l) -> mk @@ Lassign(v, simplif l)
  | Lsend(k, m, o, ll, loc) ->
      mk @@ Lsend(k, simplif m, simplif o, List.map simplif ll, loc)
  | Levent(l, ev) -> mk @@ Levent(simplif l, ev)
  | Lifused(v, l) ->
      if count_var v > 0 then simplif l else lambda_unit
  in
  simplif lam

(* Tail call info in annotation files *)

let is_tail_native_heuristic : (int -> bool) ref =
  ref (fun n -> true)

let rec emit_tail_infos is_tail lambda =
  let call_kind args =
    if is_tail
    && ((not !Clflags.native_code)
        || (!is_tail_native_heuristic (List.length args)))
   then Annot.Tail
   else Annot.Stack in
  match lambda.lb_expr with
  | Lvar _ -> ()
  | Lconst _ -> ()
  | Lapply (func, l, loc) ->
      list_emit_tail_infos false l;
      Stypes.record (Stypes.An_call (loc, call_kind l))
  | Lfunction (_, _, lam) ->
      emit_tail_infos true lam
  | Llet (_, _, lam, body) ->
      emit_tail_infos false lam;
      emit_tail_infos is_tail body
  | Lletrec (bindings, body) ->
      List.iter (fun (_, lam) -> emit_tail_infos false lam) bindings;
      emit_tail_infos is_tail body
  | Lprim (Pidentity, [arg]) ->
      emit_tail_infos is_tail arg
  | Lprim (Psequand, [arg1; arg2])
  | Lprim (Psequor, [arg1; arg2]) ->
      emit_tail_infos false arg1;
      emit_tail_infos is_tail arg2
  | Lprim (_, l) ->
      list_emit_tail_infos false l
  | Lswitch (lam, sw) ->
      emit_tail_infos false lam;
      list_emit_tail_infos_fun snd is_tail sw.sw_consts;
      list_emit_tail_infos_fun snd is_tail sw.sw_blocks;
      Misc.may  (emit_tail_infos is_tail) sw.sw_failaction
  | Lstringswitch (lam, sw, d) ->
      emit_tail_infos false lam;
      List.iter
        (fun (_,lam) ->  emit_tail_infos is_tail lam)
        sw ;
      Misc.may (emit_tail_infos is_tail) d
  | Lstaticraise (_, l) ->
      list_emit_tail_infos false l
  | Lstaticcatch (body, _, handler) ->
      emit_tail_infos is_tail body;
      emit_tail_infos is_tail handler
  | Ltrywith (body, _, handler) ->
      emit_tail_infos false body;
      emit_tail_infos is_tail handler
  | Lifthenelse (cond, ifso, ifno) ->
      emit_tail_infos false cond;
      emit_tail_infos is_tail ifso;
      emit_tail_infos is_tail ifno
  | Lsequence (lam1, lam2) ->
      emit_tail_infos false lam1;
      emit_tail_infos is_tail lam2
  | Lwhile (cond, body) ->
      emit_tail_infos false cond;
      emit_tail_infos false body
  | Lfor (_, low, high, _, body) ->
      emit_tail_infos false low;
      emit_tail_infos false high;
      emit_tail_infos false body
  | Lassign (_, lam) ->
      emit_tail_infos false lam
  | Lsend (_, meth, obj, args, loc) ->
      emit_tail_infos false meth;
      emit_tail_infos false obj;
      list_emit_tail_infos false args;
      Stypes.record (Stypes.An_call (loc, call_kind (obj :: args)))
  | Levent (lam, _) ->
      emit_tail_infos is_tail lam
  | Lifused (_, lam) ->
      emit_tail_infos is_tail lam
and list_emit_tail_infos_fun f is_tail =
  List.iter (fun x -> emit_tail_infos is_tail (f x))
and list_emit_tail_infos is_tail =
  List.iter (emit_tail_infos is_tail)

(* The entry point:
   simplification + emission of tailcall annotations, if needed. *)

let simplify_lambda lam =
  let res = simplify_lets (simplify_exits lam) in
  if !Clflags.annotations then emit_tail_infos true res;
  res
