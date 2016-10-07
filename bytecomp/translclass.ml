(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types
open Typedtree
open Lambda
open Translobj
open Translcore

(* XXX Rajouter des evenements... | Add more events... *)

type error = Illegal_class_expr | Tags of label * label

exception Error of Location.t * error

let lfunction params body =
  if params = [] then body else
  match body.lb_desc with
  | Lfunction {kind = Curried; params = params'; body = body'; attr; loc} ->
      mk_lambda @@
      Lfunction {kind = Curried; params = params @ params'; body = body'; attr;
                 loc}
  |  _ ->
      mk_lambda @@
      Lfunction {kind = Curried; params;
                 body;
                 attr = default_function_attribute;
                 loc = Location.none}

let lapply ap =
  match ap.ap_func.lb_desc with
    Lapply ap' ->
      mk_lambda @@
      Lapply {ap with ap_func = ap'.ap_func; ap_args = ap'.ap_args @ ap.ap_args}
  | _ ->
      mk_lambda @@
      Lapply ap

let mkappl (func, args) =
  mk_lambda @@
  Lapply {ap_should_be_tailcall=false;
          ap_loc=Location.none;
          ap_func=func;
          ap_args=args;
          ap_inlined=Default_inline;
          ap_specialised=Default_specialise};;

let lsequence l1 l2 =
  if l2 = lambda_unit then l1 else mk_lambda @@ Lsequence(l1, l2)

let lfield v i =
  mk_lambda @@ Lprim(Pfield i, [mk_lambda @@ Lvar v], Location.none)

let transl_label l = share (Const_immstring l)

let transl_meth_list lst =
  if lst = [] then mk_lambda @@ Lconst (Const_pointer 0) else
  share (Const_block
            (0, List.map (fun lab -> Const_immstring lab) lst))

let set_inst_var obj id expr =
  let kind =
    match Typeopt.maybe_pointer expr with
    | Pointer -> Paddrarray
    | Immediate -> Pintarray
  in
  mk_lambda @@
  Lprim(Parraysetu kind,
        [mk_lambda @@ Lvar obj;
         mk_lambda @@ Lvar id;
         transl_exp expr], Location.none)

let transl_val tbl create name =
  mkappl (oo_prim (if create then "new_variable" else "get_variable"),
          [mk_lambda @@ Lvar tbl; transl_label name])

let transl_vals tbl create strict vals rem =
  List.fold_right
    (fun (name, id) rem ->
       mk_lambda @@
      Llet(strict, Pgenval, id, transl_val tbl create name, rem))
    vals rem

let meths_super tbl meths inh_meths =
  List.fold_right
    (fun (nm, id) rem ->
       try
         (nm, id,
          mkappl(oo_prim "get_method",
                 [mk_lambda @@ Lvar tbl;
                  mk_lambda @@ Lvar (Meths.find nm meths)]))
         :: rem
       with Not_found -> rem)
    inh_meths []

let bind_super tbl (vals, meths) cl_init =
  transl_vals tbl false StrictOpt vals
    (List.fold_right (fun (_nm, id, def) rem ->
         mk_lambda @@ Llet(StrictOpt, Pgenval, id, def, rem))
       meths cl_init)

let create_object cl obj init =
  let obj' = Ident.create "self" in
  let (inh_init, obj_init, has_init) = init obj' in
  if obj_init = lambda_unit then
    (inh_init,
     mkappl (oo_prim (if has_init then "create_object_and_run_initializers"
                      else"create_object_opt"),
             [obj; mk_lambda @@ Lvar cl]))
  else begin
    (inh_init,
     mk_lambda @@
     Llet(Strict, Pgenval, obj',
          mkappl (oo_prim "create_object_opt", [obj; mk_lambda @@ Lvar cl]),
          mk_lambda @@
          Lsequence(obj_init,
                   if not has_init then mk_lambda @@ Lvar obj' else
                   mkappl (oo_prim "run_initializers_opt",
                           [obj; mk_lambda @@ Lvar obj';
                            mk_lambda @@ Lvar cl]))))
  end

let name_pattern default p =
  match p.pat_desc with
  | Tpat_var (id, _) -> id
  | Tpat_alias(_, id, _) -> id
  | _ -> Ident.create default

let normalize_cl_path cl path =
  Env.normalize_path (Some cl.cl_loc) cl.cl_env path

let rec build_object_init cl_table (obj:lambda) params inh_init obj_init cl =
  match cl.cl_desc with
    Tcl_ident ( path, _, _) ->
      let obj_init = Ident.create "obj_init" in
      let envs, inh_init = inh_init in
      let env =
        match envs with None -> []
        | Some envs ->
            [mk_lambda @@ Lprim(Pfield (List.length inh_init + 1),
                                [mk_lambda @@ Lvar envs],
                                Location.none)]
      in
      ((envs, (obj_init, normalize_cl_path cl path)
        ::inh_init),
       mkappl(mk_lambda @@ Lvar obj_init, env @ [obj]))
  | Tcl_structure str ->
      create_object cl_table obj (fun obj ->
        let (inh_init, obj_init, has_init) =
          List.fold_right
            (fun field (inh_init, obj_init, has_init) ->
               match field.cf_desc with
                 Tcf_inherit (_, cl, _, _, _) ->
                   let (inh_init, obj_init') =
                     build_object_init
                       cl_table (mk_lambda @@ Lvar obj) [] inh_init
                       (fun _ -> lambda_unit) cl
                   in
                   (inh_init, lsequence obj_init' obj_init, true)
               | Tcf_val (_, _, id, Tcfk_concrete (_, exp), _) ->
                   (inh_init, lsequence (set_inst_var obj id exp) obj_init,
                    has_init)
               | Tcf_method _ | Tcf_val _ | Tcf_constraint _ | Tcf_attribute _->
                   (inh_init, obj_init, has_init)
               | Tcf_initializer _ ->
                   (inh_init, obj_init, true)
            )
            str.cstr_fields
            (inh_init, obj_init obj, false)
        in
        (inh_init,
         List.fold_right
           (fun (id, expr) rem ->
              lsequence (mk_lambda @@ Lifused (id, set_inst_var obj id expr)) rem)
           params obj_init,
         has_init))
  | Tcl_fun (_, pat, vals, cl, partial) ->
      let vals = List.map (fun (id, _, e) -> id,e) vals in
      let (inh_init, obj_init) =
        build_object_init cl_table obj (vals @ params) inh_init obj_init cl
      in
      (inh_init,
       let build params rem =
         let param = name_pattern "param" pat in
         mk_lambda @@
         Lfunction {kind = Curried; params = param::params;
                    attr = default_function_attribute;
                    loc = pat.pat_loc;
                    body = Matching.for_function
                        pat.pat_loc None (mk_lambda @@ Lvar param)
                        [pat, rem] partial}
       in
       begin match obj_init with
         { lb_desc = Lfunction {kind = Curried; params; body = rem}} ->
           build params rem
       | rem -> build [] rem
       end)
  | Tcl_apply (cl, oexprs) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj params inh_init obj_init cl
      in
      (inh_init, transl_apply None obj_init oexprs Location.none)
  | Tcl_let (rec_flag, defs, vals, cl) ->
      let vals = List.map (fun (id, _, e) -> id,e) vals in
      let (inh_init, obj_init) =
        build_object_init cl_table obj (vals @ params) inh_init obj_init cl
      in
      (inh_init, Translcore.transl_let rec_flag defs obj_init)
  | Tcl_constraint (cl, _, _vals, _pub_meths, _concr_meths) ->
      build_object_init cl_table obj params inh_init obj_init cl

let rec build_object_init_0 cl_table params cl copy_env subst_env top ids =
  match cl.cl_desc with
    Tcl_let (_rec_flag, _defs, vals, cl) ->
      let vals = List.map (fun (id, _, e) -> id,e) vals in
      build_object_init_0 cl_table (vals@params) cl copy_env subst_env top ids
  | _ ->
      let self = Ident.create "self" in
      let env = Ident.create "env" in
      let obj =
        if ids = [] then lambda_unit else mk_lambda @@ Lvar self in
      let envs = if top then None else Some env in
      let ((_,inh_init), obj_init) =
        build_object_init cl_table obj params (envs,[]) copy_env cl in
      let obj_init =
        if ids = [] then obj_init else lfunction [self] obj_init in
      (inh_init, lfunction [env] (subst_env env inh_init obj_init))


let bind_method tbl lab id cl_init =
  mk_lambda @@
  Llet(Strict, Pgenval, id, mkappl (oo_prim "get_method_label",
                           [mk_lambda @@ Lvar tbl; transl_label lab]),
       cl_init)

let bind_methods tbl meths vals cl_init =
  let methl = Meths.fold (fun lab id tl -> (lab,id) :: tl) meths [] in
  let len = List.length methl and nvals = List.length vals in
  if len < 2 && nvals = 0 then Meths.fold (bind_method tbl) meths cl_init else
  if len = 0 && nvals < 2 then transl_vals tbl true Strict vals cl_init else
  let ids = Ident.create "ids" in
  let i = ref (len + nvals) in
  let getter, names =
    if nvals = 0 then "get_method_labels", [] else
    "new_methods_variables", [transl_meth_list (List.map fst vals)]
  in
  mk_lambda @@
  Llet(Strict, Pgenval, ids,
       mkappl (oo_prim getter,
               [mk_lambda @@ Lvar tbl;
                transl_meth_list (List.map fst methl)] @ names),
       List.fold_right
         (fun (_lab,id) lam ->
            decr i; mk_lambda @@ Llet(StrictOpt, Pgenval, id,
                                      lfield ids !i, lam))
         (methl @ vals) cl_init)

let output_methods tbl methods lam =
  match methods with
    [] -> lam
  | [lab; code] ->
      lsequence (mkappl(oo_prim "set_method",
                        [mk_lambda @@ Lvar tbl; lab; code])) lam
  | _ ->
      lsequence (mkappl(oo_prim "set_methods",
                        [mk_lambda @@ Lvar tbl;
                         mk_lambda @@ Lprim(Pmakeblock(0,Immutable,None),
                               methods, Location.none)]))
        lam

let rec ignore_cstrs cl =
  match cl.cl_desc with
    Tcl_constraint (cl, _, _, _, _) -> ignore_cstrs cl
  | Tcl_apply (cl, _) -> ignore_cstrs cl
  | _ -> cl

let rec index a = function
    [] -> raise Not_found
  | b :: l ->
      if b = a then 0 else 1 + index a l

let bind_id_as_val (id, _, _) = ("", id)

let rec build_class_init cla cstr super inh_init cl_init msubst top cl =
  match cl.cl_desc with
    Tcl_ident ( path, _, _) ->
      begin match inh_init with
        (obj_init, _path')::inh_init ->
          let lpath =
            transl_path ~loc:cl.cl_loc cl.cl_env (Some (Class cl.cl_type)) path in
          (inh_init,
           mk_lambda @@
           Llet (Strict, Pgenval, obj_init,
                 mkappl(mk_lambda @@
                        Lprim(Pfield 1, [lpath], Location.none),
                        (mk_lambda @@ Lvar cla) ::
                        if top then [mk_lambda @@
                                     Lprim(Pfield 3, [lpath], Location.none)]
                        else []),
                 bind_super cla super cl_init))
      | _ ->
          assert false
      end
  | Tcl_structure str ->
      let cl_init = bind_super cla super cl_init in
      let (inh_init, cl_init, methods, values) =
        List.fold_right
          (fun field (inh_init, cl_init, methods, values) ->
            match field.cf_desc with
              Tcf_inherit (_, cl, _, vals, meths) ->
                let cl_init = output_methods cla methods cl_init in
                let inh_init, cl_init =
                  build_class_init cla false
                    (vals, meths_super cla str.cstr_meths meths)
                    inh_init cl_init msubst top cl in
                (inh_init, cl_init, [], values)
            | Tcf_val (name, _, id, _, over) ->
                let values =
                  if over then values else (name.txt, id) :: values
                in
                (inh_init, cl_init, methods, values)
            | Tcf_method (_, _, Tcfk_virtual _)
            | Tcf_constraint _
              ->
                (inh_init, cl_init, methods, values)
            | Tcf_method (name, _, Tcfk_concrete (_, exp)) ->
                let met_code = msubst true (transl_exp exp) in
                let met_code =
                  if !Clflags.native_code && List.length met_code = 1 then
                    (* Force correct naming of method for profiles *)
                    let met = Ident.create ("method_" ^ name.txt) in
                    [mk_lambda @@
                     Llet(Strict, Pgenval, met, List.hd met_code,
                          mk_lambda @@ Lvar met)]
                  else met_code
                in
                (inh_init, cl_init,
                 (mk_lambda @@ Lvar(Meths.find name.txt str.cstr_meths)) ::
                              met_code @ methods,
                 values)
            | Tcf_initializer exp ->
                (inh_init,
                 mk_lambda @@
                 Lsequence(mkappl (oo_prim "add_initializer",
                                   (mk_lambda @@ Lvar cla) ::
                                   msubst false (transl_exp exp)),
                           cl_init),
                 methods, values)
            | Tcf_attribute _ ->
                (inh_init, cl_init, methods, values))
          str.cstr_fields
          (inh_init, cl_init, [], [])
      in
      let cl_init = output_methods cla methods cl_init in
      (inh_init, bind_methods cla str.cstr_meths values cl_init)
  | Tcl_fun (_, _pat, vals, cl, _) ->
      let (inh_init, cl_init) =
        build_class_init cla cstr super inh_init cl_init msubst top cl
      in
      let vals = List.map bind_id_as_val vals in
      (inh_init, transl_vals cla true StrictOpt vals cl_init)
  | Tcl_apply (cl, _exprs) ->
      build_class_init cla cstr super inh_init cl_init msubst top cl
  | Tcl_let (_rec_flag, _defs, vals, cl) ->
      let (inh_init, cl_init) =
        build_class_init cla cstr super inh_init cl_init msubst top cl
      in
      let vals = List.map bind_id_as_val vals in
      (inh_init, transl_vals cla true StrictOpt vals cl_init)
  | Tcl_constraint (cl, _, vals, meths, concr_meths) ->
      let virt_meths =
        List.filter (fun lab -> not (Concr.mem lab concr_meths)) meths in
      let concr_meths = Concr.elements concr_meths in
      let narrow_args =
        [mk_lambda @@ Lvar cla;
         transl_meth_list vals;
         transl_meth_list virt_meths;
         transl_meth_list concr_meths] in
      let cl = ignore_cstrs cl in
      begin match cl.cl_desc, inh_init with
        Tcl_ident (path, _, _), (obj_init, path')::inh_init ->
          assert (Path.same (normalize_cl_path cl path) path');
          let lpath = transl_normal_path path' in
          let inh = Ident.create "inh"
          and ofs = List.length vals + 1
          and valids, methids = super in
          let cl_init =
            List.fold_left
              (fun init (nm, id, _) ->
                 mk_lambda @@
                Llet(StrictOpt, Pgenval, id,
                     lfield inh (index nm concr_meths + ofs),
                     init))
              cl_init methids in
          let cl_init =
            List.fold_left
              (fun init (nm, id) ->
                 mk_lambda @@
                 Llet(StrictOpt, Pgenval, id,
                     lfield inh (index nm vals + 1), init))
              cl_init valids in
          (inh_init,
           mk_lambda @@
           Llet (Strict, Pgenval, inh,
                 mkappl(oo_prim "inherits",
                        narrow_args @
                        [lpath;
                         mk_lambda @@
                         Lconst(Const_pointer(if top then 1 else 0))]),
                 mk_lambda @@
                 Llet(StrictOpt, Pgenval, obj_init, lfield inh 0, cl_init)))
      | _ ->
          let core cl_init =
            build_class_init cla true super inh_init cl_init msubst top cl
          in
          if cstr then core cl_init else
          let (inh_init, cl_init) =
            core (mk_lambda @@
                  Lsequence (mkappl (oo_prim "widen",
                                     [mk_lambda @@ Lvar cla]), cl_init))
          in
          (inh_init,
           mk_lambda @@
           Lsequence(mkappl (oo_prim "narrow", narrow_args),
                     cl_init))
      end

let rec build_class_lets cl ids =
  match cl.cl_desc with
    Tcl_let (rec_flag, defs, _vals, cl') ->
      let env, wrap = build_class_lets cl' [] in
      (env, fun x ->
        let lam = Translcore.transl_let rec_flag defs (wrap x) in
        (* Check recursion in toplevel let-definitions *)
        if ids = [] || Translcore.check_recursive_lambda ids lam then lam
        else raise(Error(cl.cl_loc, Illegal_class_expr)))
  | _ ->
      (cl.cl_env, fun x -> x)

let rec get_class_meths cl =
  match cl.cl_desc with
    Tcl_structure cl ->
      Meths.fold (fun _ -> IdentSet.add) cl.cstr_meths IdentSet.empty
  | Tcl_ident _ -> IdentSet.empty
  | Tcl_fun (_, _, _, cl, _)
  | Tcl_let (_, _, _, cl)
  | Tcl_apply (cl, _)
  | Tcl_constraint (cl, _, _, _, _) -> get_class_meths cl

(*
   XXX Il devrait etre peu couteux d'ecrire des classes :
   |   Writing classes should be cheap
     class c x y = d e f
*)
let rec transl_class_rebind obj_init cl vf =
  match cl.cl_desc with
    Tcl_ident (path, _, _) ->
      if vf = Concrete then begin
        try if (Env.find_class path cl.cl_env).cty_new = None then raise Exit
        with Not_found -> raise Exit
      end;
      (normalize_cl_path cl path, obj_init)
  | Tcl_fun (_, pat, _, cl, partial) ->
      let path, obj_init = transl_class_rebind obj_init cl vf in
      let build params rem =
        let param = name_pattern "param" pat in
        mk_lambda @@
        Lfunction {kind = Curried; params = param::params;
                   attr = default_function_attribute;
                   loc = pat.pat_loc;
                   body = Matching.for_function
                       pat.pat_loc None
                       (mk_lambda @@ Lvar param) [pat, rem] partial}
      in
      (path,
       match obj_init with
         { lb_desc = Lfunction {kind = Curried; params; body}} ->
           build params body
       | rem -> build [] rem)
  | Tcl_apply (cl, oexprs) ->
      let path, obj_init = transl_class_rebind obj_init cl vf in
      (path, transl_apply None obj_init oexprs Location.none)
  | Tcl_let (rec_flag, defs, _vals, cl) ->
      let path, obj_init = transl_class_rebind obj_init cl vf in
      (path, Translcore.transl_let rec_flag defs obj_init)
  | Tcl_structure _ -> raise Exit
  | Tcl_constraint (cl', _, _, _, _) ->
      let path, obj_init = transl_class_rebind obj_init cl' vf in
      let rec check_constraint = function
          Cty_constr(path', _, _) when Path.same path path' -> ()
        | Cty_arrow (_, _, cty) -> check_constraint cty
        | _ -> raise Exit
      in
      check_constraint cl.cl_type;
      (path, obj_init)

let rec transl_class_rebind_0 self obj_init cl vf =
  match cl.cl_desc with
    Tcl_let (rec_flag, defs, _vals, cl) ->
      let path, obj_init = transl_class_rebind_0 self obj_init cl vf in
      (path, Translcore.transl_let rec_flag defs obj_init)
  | _ ->
      let path, obj_init = transl_class_rebind obj_init cl vf in
      (path, lfunction [self] obj_init)

let transl_class_rebind ids cl vf =
  try
    let obj_init = Ident.create "obj_init"
    and self = Ident.create "self" in
    let obj_init0 =
      lapply {ap_should_be_tailcall=false;
              ap_loc=Location.none;
              ap_func=mk_lambda @@ Lvar obj_init;
              ap_args=[mk_lambda @@ Lvar self];
              ap_inlined=Default_inline;
              ap_specialised=Default_specialise}
    in
    let path, obj_init' = transl_class_rebind_0 self obj_init0 cl vf in
    if not (Translcore.check_recursive_lambda ids obj_init') then
      raise(Error(cl.cl_loc, Illegal_class_expr));
    let id = (obj_init' = lfunction [self] obj_init0) in
    if id then transl_normal_path path else

    let cla = Ident.create "class"
    and new_init = Ident.create "new_init"
    and env_init = Ident.create "env_init"
    and table = Ident.create "table"
    and envs = Ident.create "envs" in
    mk_lambda @@
    Llet(
    Strict, Pgenval, new_init, lfunction [obj_init] obj_init',
    mk_lambda @@ Llet(
    Alias, Pgenval, cla, transl_normal_path path,
    mk_lambda @@
    Lprim(Pmakeblock(0, Immutable, None),
          [mkappl(mk_lambda @@ Lvar new_init, [lfield cla 0]);
           lfunction [table]
             (mk_lambda @@
              Llet(Strict, Pgenval, env_init,
                   mkappl(lfield cla 1, [mk_lambda @@ Lvar table]),
                   lfunction [envs]
                     (mkappl(mk_lambda @@ Lvar new_init,
                             [mkappl(mk_lambda @@ Lvar env_init,
                                     [mk_lambda @@ Lvar envs])]))));
           lfield cla 2;
           lfield cla 3],
          Location.none)))
  with Exit ->
    lambda_unit

(* Rewrite a closure using builtins. Improves native code size. *)

let rec module_path l =
  match l.lb_desc with
    Lvar id ->
      let s = Ident.name id in s <> "" && s.[0] >= 'A' && s.[0] <= 'Z'
  | Lprim(Pfield _, [p], _)    -> module_path p
  | Lprim(Pgetglobal _, [], _) -> true
  | _                          -> false

let const_path local l =
  match l.lb_desc with
    Lvar id -> not (List.mem id local)
  | Lconst _ -> true
  | Lfunction {kind = Curried; body} ->
      let fv = free_variables body in
      List.for_all (fun x -> not (IdentSet.mem x fv)) local
  | _ -> module_path l

let rec builtin_meths self env env2 body =
  let const_path = const_path (env::self) in
  let conv l =
    match l.lb_desc with
    (* Lvar s when List.mem s self ->  "_self", [] *)
    | _ when const_path l -> "const", [l]
    | Lprim(Parrayrefu _, [{ lb_desc = Lvar s };
                           { lb_desc = Lvar n }], _) when List.mem s self ->
        "var", [mk_lambda @@ Lvar n]
    | Lprim(Pfield n, [{ lb_desc = Lvar e }], _) when Ident.same e env ->
        "env", [mk_lambda @@ Lvar env2;
                mk_lambda @@ Lconst(Const_pointer n)]
    | Lsend(Self, met, { lb_desc = Lvar s }, [], _) when List.mem s self ->
        "meth", [met]
    | _ -> raise Not_found
  in
  match body.lb_desc with
  | Llet(_str, _k, s', {lb_desc = Lvar s}, body) when List.mem s self ->
      builtin_meths (s'::self) env env2 body
  | Lapply{ap_func = f; ap_args = [arg]} when const_path f ->
      let s, args = conv arg in ("app_"^s, f :: args)
  | Lapply{ap_func = f; ap_args = [arg; p]} when const_path f && const_path p ->
      let s, args = conv arg in
      ("app_"^s^"_const", f :: args @ [p])
  | Lapply{ap_func = f; ap_args = [p; arg]} when const_path f && const_path p ->
      let s, args = conv arg in
      ("app_const_"^s, f :: p :: args)
  | Lsend(Self, { lb_desc = Lvar n },
          { lb_desc = Lvar s}, [arg], _) when List.mem s self ->
      let s, args = conv arg in
      ("meth_app_"^s, (mk_lambda @@ Lvar n) :: args)
  | Lsend(Self, met, { lb_desc = Lvar s }, [], _) when List.mem s self ->
      ("get_meth", [met])
  | Lsend(Public, met, arg, [], _) ->
      let s, args = conv arg in
      ("send_"^s, met :: args)
  | Lsend(Cached, met, arg, [_;_], _) ->
      let s, args = conv arg in
      ("send_"^s, met :: args)
  | Lfunction {kind = Curried; params = [x]; body} ->
      let rec enter self l =
        match l.lb_desc with
        | Lprim(Parraysetu _,
                [{ lb_desc = Lvar s };
                 { lb_desc = Lvar n };
                 { lb_desc = Lvar x'}], _)
          when Ident.same x x' && List.mem s self ->
            ("set_var", [mk_lambda @@ Lvar n])
        | Llet(_str, _k, s', { lb_desc = Lvar s }, body) when List.mem s self ->
            enter (s'::self) body
        | _ -> raise Not_found
      in enter self body
  | Lfunction _ -> raise Not_found
  | _ ->
      let s, args = conv body in ("get_"^s, args)

module M = struct
  open CamlinternalOO
  let builtin_meths self env env2 body =
    let builtin, args = builtin_meths self env env2 body in
    (* if not arr then [mkappl(oo_prim builtin, args)] else *)
    let tag = match builtin with
      "get_const" -> GetConst
    | "get_var"   -> GetVar
    | "get_env"   -> GetEnv
    | "get_meth"  -> GetMeth
    | "set_var"   -> SetVar
    | "app_const" -> AppConst
    | "app_var"   -> AppVar
    | "app_env"   -> AppEnv
    | "app_meth"  -> AppMeth
    | "app_const_const" -> AppConstConst
    | "app_const_var"   -> AppConstVar
    | "app_const_env"   -> AppConstEnv
    | "app_const_meth"  -> AppConstMeth
    | "app_var_const"   -> AppVarConst
    | "app_env_const"   -> AppEnvConst
    | "app_meth_const"  -> AppMethConst
    | "meth_app_const"  -> MethAppConst
    | "meth_app_var"    -> MethAppVar
    | "meth_app_env"    -> MethAppEnv
    | "meth_app_meth"   -> MethAppMeth
    | "send_const" -> SendConst
    | "send_var"   -> SendVar
    | "send_env"   -> SendEnv
    | "send_meth"  -> SendMeth
    | _ -> assert false
    in (mk_lambda @@ Lconst(Const_pointer(Obj.magic tag))) :: args
end
open M


(*
   Class translation.
   Three subcases:
    * reapplication of a known class -> transl_class_rebind
    * class without local dependencies -> direct translation
    * with local dependencies -> generate a stubs tree,
      with a node for every local classes inherited
   A class is a 4-tuple:
    (obj_init, class_init, env_init, env)
    obj_init: creation function (unit -> obj)
    class_init: inheritance function (table -> env_init)
      (one by source code)
    env_init: parameterisation by the local environment
      (env -> params -> obj_init)
      (one for each combination of inherited class_init )
    env: environnement local
   If ids=0 (immediate object), then only env_init is conserved.
*)

(*
let prerr_ids msg ids =
  let names = List.map Ident.unique_toplevel_name ids in
  prerr_endline (String.concat " " (msg :: names))
*)

let transl_class ids cl_id pub_meths cl vflag =
  (* First check if it is not only a rebind *)
  let rebind = transl_class_rebind ids cl vflag in
  if rebind <> lambda_unit then rebind else

  (* Prepare for heavy environment handling *)
  let tables = Ident.create (Ident.name cl_id ^ "_tables") in
  let (top_env, req) = oo_add_class tables in
  let top = not req in
  let cl_env, llets = build_class_lets cl ids in
  let new_ids = if top then [] else Env.diff top_env cl_env in
  let env2 = Ident.create "env" in
  let meth_ids = get_class_meths cl in
  let subst env lam i0 new_ids' =
    let fv = free_variables lam in
    (* prerr_ids "cl_id =" [cl_id]; prerr_ids "fv =" (IdentSet.elements fv); *)
    let fv = List.fold_right IdentSet.remove !new_ids' fv in
    (* We need to handle method ids specially, as they do not appear
       in the typing environment (PR#3576, PR#4560) *)
    (* very hacky: we add and remove free method ids on the fly,
       depending on the visit order... *)
    method_ids :=
      IdentSet.diff (IdentSet.union (free_methods lam) !method_ids) meth_ids;
    (* prerr_ids "meth_ids =" (IdentSet.elements meth_ids);
       prerr_ids "method_ids =" (IdentSet.elements !method_ids); *)
    let new_ids = List.fold_right IdentSet.add new_ids !method_ids in
    let fv = IdentSet.inter fv new_ids in
    new_ids' := !new_ids' @ IdentSet.elements fv;
    (* prerr_ids "new_ids' =" !new_ids'; *)
    let i = ref (i0-1) in
    List.fold_left
      (fun subst id ->
        incr i; Ident.add id (lfield env !i)  subst)
      Ident.empty !new_ids'
  in
  let new_ids_meths = ref [] in
  let msubst arr l =
    match l.lb_desc with
      Lfunction {kind = Curried; params = self :: args; body} ->
        let env = Ident.create "env" in
        let body' =
          if new_ids = [] then body else
          subst_lambda (subst env body 0 new_ids_meths) body in
        begin try
          (* Doesn't seem to improve size for bytecode *)
          (* if not !Clflags.native_code then raise Not_found; *)
          if not arr || !Clflags.debug then raise Not_found;
          builtin_meths [self] env env2 (lfunction args body')
        with Not_found ->
          [lfunction (self :: args)
             (if not (IdentSet.mem env (free_variables body')) then body' else
              mk_lambda @@
              Llet(Alias, Pgenval, env,
                   mk_lambda @@
                   Lprim(Parrayrefu Paddrarray,
                         [mk_lambda @@ Lvar self; mk_lambda @@ Lvar env2],
                         Location.none),
                   body'))]
        end
      | _ -> assert false
  in
  let new_ids_init = ref [] in
  let env1 = Ident.create "env" and env1' = Ident.create "env'" in
  let copy_env self =
    if top then lambda_unit else
      mk_lambda @@
      Lifused(env2, mk_lambda @@
              Lprim(Parraysetu Paddrarray,
                    [mk_lambda @@ Lvar self;
                     mk_lambda @@ Lvar env2;
                     mk_lambda @@ Lvar env1'],
                    Location.none))
  and subst_env envs l lam =
    if top then lam else
    (* must be called only once! *)
    let lam = subst_lambda (subst env1 lam 1 new_ids_init) lam in
    mk_lambda @@  
    Llet(Alias, Pgenval, env1,
         (if l = [] then mk_lambda @@ Lvar envs else lfield envs 0),
    mk_lambda @@
    Llet(Alias, Pgenval, env1',
         (if !new_ids_init = [] then mk_lambda @@ Lvar env1 else lfield env1 0),
         lam))
  in

  (* Now we start compiling the class *)
  let cla = Ident.create "class" in
  let (inh_init, obj_init) =
    build_object_init_0 cla [] cl copy_env subst_env top ids in
  let inh_init' = List.rev inh_init in
  let (inh_init', cl_init) =
    build_class_init cla true ([],[]) inh_init' obj_init msubst top cl
  in
  assert (inh_init' = []);
  let table = Ident.create "table"
  and class_init = Ident.create (Ident.name cl_id ^ "_init")
  and env_init = Ident.create "env_init"
  and obj_init = Ident.create "obj_init" in
  let pub_meths =
    List.sort
      (fun s s' -> compare (Btype.hash_variant s) (Btype.hash_variant s'))
      pub_meths in
  let tags = List.map Btype.hash_variant pub_meths in
  let rev_map = List.combine tags pub_meths in
  List.iter2
    (fun tag name ->
      let name' = List.assoc tag rev_map in
      if name' <> name then raise(Error(cl.cl_loc, Tags(name, name'))))
    tags pub_meths;
  let ltable table lam =
    mk_lambda @@
    Llet(Strict, Pgenval, table,
         mkappl (oo_prim "create_table", [transl_meth_list pub_meths]), lam)
  and ldirect obj_init =
    mk_lambda @@
    Llet(Strict, Pgenval, obj_init, cl_init,
         mk_lambda @@
         Lsequence(mkappl (oo_prim "init_class", [mk_lambda @@ Lvar cla]),
                   mkappl (mk_lambda @@ Lvar obj_init, [lambda_unit])))
  in
  (* Simplest case: an object defined at toplevel (ids=[]) *)
  if top && ids = [] then llets (ltable cla (ldirect obj_init)) else

  let concrete = (vflag = Concrete)
  and lclass lam =
    let cl_init =
      llets (mk_lambda @@ Lfunction{kind = Curried;
                                   attr = default_function_attribute;
                                   loc = Location.none;
                                    params = [cla]; body = cl_init}) in
    mk_lambda @@
    Llet(Strict, Pgenval, class_init, cl_init, lam (free_variables cl_init))
  and lbody fv =
    if List.for_all (fun id -> not (IdentSet.mem id fv)) ids then
      mkappl (oo_prim "make_class",[transl_meth_list pub_meths;
                                    mk_lambda @@ Lvar class_init])
    else
      ltable table (
      mk_lambda @@
      Llet(
        Strict, Pgenval, env_init,
        mkappl (mk_lambda @@ Lvar class_init, [mk_lambda @@ Lvar table]),
        mk_lambda @@
      Lsequence(
        mkappl (oo_prim "init_class", [mk_lambda @@ Lvar table]),
        mk_lambda @@
      Lprim(Pmakeblock(0, Immutable, None),
            [mkappl (mk_lambda @@ Lvar env_init, [lambda_unit]);
             mk_lambda @@ Lvar class_init;
             mk_lambda @@ Lvar env_init; lambda_unit],
            Location.none))))
  and lbody_virt lenvs =
    mk_lambda @@
    Lprim(Pmakeblock(0, Immutable, None),
          [lambda_unit; mk_lambda @@
                        Lfunction{kind = Curried;
                                  attr = default_function_attribute;
                                  loc = Location.none;
                                  params = [cla]; body = cl_init};
           lambda_unit; lenvs],
         Location.none)
  in
  (* Still easy: a class defined at toplevel *)
  if top && concrete then lclass lbody else
  if top then llets (lbody_virt lambda_unit) else

  (* Now for the hard stuff: prepare for table cacheing *)
  let envs = Ident.create "envs"
  and cached = Ident.create "cached" in
  let lenvs =
    if !new_ids_meths = [] && !new_ids_init = [] && inh_init = []
    then lambda_unit
    else mk_lambda @@ Lvar envs in
  let lenv =
    let menv =
      if !new_ids_meths = [] then lambda_unit else
      mk_lambda @@
      Lprim(Pmakeblock(0, Immutable, None),
            List.map (fun id -> mk_lambda @@ Lvar id) !new_ids_meths,
            Location.none) in
    if !new_ids_init = [] then menv else
    mk_lambda @@
    Lprim(Pmakeblock(0, Immutable, None),
          menv :: List.map (fun id -> mk_lambda @@ Lvar id) !new_ids_init,
          Location.none)
  and linh_envs =
    List.map
      (fun (_, p) ->
         mk_lambda @@ Lprim(Pfield 3, [transl_normal_path p], Location.none))
      (List.rev inh_init)
  in
  let make_envs lam =
    mk_lambda @@
    Llet(StrictOpt, Pgenval, envs,
         (if linh_envs = [] then lenv else
         mk_lambda @@
         Lprim(Pmakeblock(0, Immutable, None),
               lenv :: linh_envs, Location.none)),
         lam)
  and def_ids cla lam =
    mk_lambda @@
    Llet(StrictOpt, Pgenval, env2,
         mkappl (oo_prim "new_variable",
                 [mk_lambda @@ Lvar cla; transl_label ""]),
         lam)
  in
  let inh_paths =
    List.filter
      (fun (_,path) -> List.mem (Path.head path) new_ids) inh_init
  in
  let inh_keys =
    List.map (fun (_,p) -> mk_lambda @@
               Lprim(Pfield 1, [transl_normal_path p],
                     Location.none))
      inh_paths
  in
  let lclass lam =
    mk_lambda @@
    Llet(Strict, Pgenval, class_init,
         mk_lambda @@
         Lfunction{kind = Curried; params = [cla];
                   attr = default_function_attribute;
                   loc = Location.none;
                   body = def_ids cla cl_init}, lam)
  and lcache lam =
    if inh_keys = [] then
      mk_lambda @@ Llet(Alias, Pgenval, cached, mk_lambda @@ Lvar tables, lam)
    else
    mk_lambda @@
    Llet(Strict, Pgenval, cached,
         mkappl (oo_prim "lookup_tables",
                 [mk_lambda @@ Lvar tables;
                  mk_lambda @@ Lprim(Pmakeblock(0, Immutable, None),
                                     inh_keys, Location.none)]),
         lam)
  and lset cached i lam =
    mk_lambda @@
    Lprim(Psetfield(i, Pointer, Assignment),
          [mk_lambda @@ Lvar cached; lam], Location.none)
  in
  let ldirect () =
    ltable cla
      (mk_lambda @@
       Llet(Strict, Pgenval, env_init, def_ids cla cl_init,
            mk_lambda @@
            Lsequence(mkappl (oo_prim "init_class", [mk_lambda @@ Lvar cla]),
                      lset cached 0 (mk_lambda @@ Lvar env_init))))
  and lclass_virt () =
    lset cached 0 (mk_lambda @@
                   Lfunction{kind = Curried; attr = default_function_attribute;
                             loc = Location.none;
                             params = [cla]; body = def_ids cla cl_init})
  in
  llets (
  lcache (
  mk_lambda @@    
  Lsequence(
  mk_lambda @@    
  Lifthenelse(lfield cached 0, lambda_unit,
              if ids = [] then ldirect () else
              if not concrete then lclass_virt () else
              lclass (
              mkappl (oo_prim "make_class_store",
                      [transl_meth_list pub_meths;
                       mk_lambda @@ Lvar class_init;
                       mk_lambda @@ Lvar cached]))),
  make_envs (
  if ids = [] then mkappl (lfield cached 0, [lenvs]) else
  mk_lambda @@    
  Lprim(Pmakeblock(0, Immutable, None),
        (if concrete then
          [mkappl (lfield cached 0, [lenvs]);
           lfield cached 1;
           lfield cached 0;
           lenvs]
        else [lambda_unit; lfield cached 0; lambda_unit; lenvs]),
        Location.none
       )))))

(* Wrapper for class compilation *)
(*
    let cl_id = ci.ci_id_class in
(* TODO: cl_id is used somewhere else as typesharp ? *)
  let _arity = List.length ci.ci_params in
  let pub_meths = m in
  let cl = ci.ci_expr in
  let vflag = vf in
*)

let transl_class ids id pub_meths cl vf =
  oo_wrap cl.cl_env false (transl_class ids id pub_meths cl) vf

let () =
  transl_object := (fun id meths cl -> transl_class [] id meths cl Concrete)

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_class_expr ->
      fprintf ppf "This kind of recursive class expression is not allowed"
  | Tags (lab1, lab2) ->
      fprintf ppf "Method labels `%s' and `%s' are incompatible.@ %s"
        lab1 lab2 "Change one of them."

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
