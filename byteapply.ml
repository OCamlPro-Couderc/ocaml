(* let apply_functor_unit ppf funit applied = *)
(*   let funit_cmi = (Filename.chop_extension funit) ^ ".cmi" in *)
(*   let cmi = Cmi_format.read_cmi funit_cmi in *)
(*   let instance = List.map2 *)
(*       (fun arg app -> *)
(*          if (\* app compatible with arg *\)... then *)
(*            (arg, app) *)
(*          else failwith "Inconsistent argument") in *)
(*   let lambda = Translmod.transl_applied_unit cmi instance in *)
(*   () *)
open Asttypes
open Lambda
open Longident

let mod_prim name =
  try
    transl_normal_path
      (fst (Env.lookup_value (Ldot (Lident "CamlinternalMod", name))
                             Env.empty))
  with Not_found -> Lvar (Ident.create_persistent "CamlinternalMod!")
    (* fatal_error ("Primitive " ^ name ^ " not found.") *)


let transl_applied_unit funit instantiation =
  Format.printf "In transl_applied_unit@.";
  let funit_id = Ident.create_persistent funit
      (* ~ns:(Longident.optstring cmi.Cmi_format.namespace) funit.cmi_name *) in
  let funit_body =
    Lprim(Pfield 0, [Lprim(Pgetglobal funit_id, [])]) in
  let env0_id = Ident.create "env0" in
  let body : Ident.t -> lambda = fun id ->
    Lapply(funit_body, [Lvar id], Location.none) in

  let application  =
    List.fold_left
      (fun prev (arg, app) ->
         let env_id = Ident.create "env" in
         let arg_id = Lconst (Const_base (Const_string (arg, None))) in
         let app_id = Ident.create (Ident.name app) in
         (fun id ->
            Llet (Strict,
                  app_id, Lprim (Pgetglobal app, []),
                  Llet (Strict, env_id,
                        Lapply(mod_prim "add_functor_arg",
                               [arg_id; Lvar app_id; Lvar id],
                               Location.none),
                        (prev env_id))))) body instantiation in
  let lambda_res = application env0_id in
  Format.printf "Lambda result = @.%a@." Printlambda.lambda lambda_res

let _ =
  transl_applied_unit "functorized" [
    "X", Ident.create_persistent "AppX";
  "Y", Ident.create_persistent "AppY";
  "Z", Ident.create_persistent "AppZ";
  "A", Ident.create_persistent "AppA";

  ]
