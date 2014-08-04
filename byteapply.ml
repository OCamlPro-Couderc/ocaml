open Asttypes
open Lambda
open Longident
open Cmi_format

let build_target oc targetname ns lambda =
  let instrs =
    Bytegen.compile_implementation target_name lam in
  let rel = Emitcode.emit oc instrs in

let applied_object_file ppf cmi targetfile targetname optns =
  let ns = match optns with
      None -> assert false
    | Some s -> Some (parse s) in
  Env.set_namespace_unit ns;
  let instance = List.map2
      (fun (arg, _) app ->
         if (* app compatible with arg *) true then
           (arg, app)
         else failwith "Inconsistent argument") cmi.cmi_functor_args !Clflags.applied in
  let lambda = Translmod.transl_applied_unit cmi instance in
  let target_id = Ident.create_persistent
      ~ns:(Longident.optstring ns) targetname in
  let lam = Lprim(Psetglobal target_id, [lambda]) in
  if !Clflags.dump_lambda then
    Format.printf "%a@." Printlambda.lambda lam;
  let instrs =
    Bytegen.compile_implementation targetname lam in
  Emitcode.to_file targetfile targetname instrs

let apply_functor_unit ppf funit intial_env =
  let funit_cmi = (Filename.chop_extension funit) ^ ".cmi" in
  let cmi = Cmi_format.read_cmi funit_cmi in
  let targetname = String.capitalize (Filename.basename prefix) in
  let targetfile = Filename.basename funit in
  applied_object_file ppf cmi targetfile !Clflags.ns
