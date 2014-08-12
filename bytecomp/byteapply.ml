open Lambda
open Longident
open Cmi_format


let applied_object_file ppf cmi targetfile targetname optns env =
  let ns = match optns with
      None -> assert false
    | Some s -> Some (parse s) in
  Env.set_namespace_unit ns;
  let parts = ref @@ List.filter (fun (name, _) -> name <> targetname) cmi.cmi_functor_parts in
  let instance = List.map2
      (fun (arg, _) app ->
         let arg_id =
           Ident.create_persistent ~ns:(optstring cmi.cmi_namespace) arg in
         Ident.make_functor_arg arg_id;
         Ident.make_functor_part arg_id;
         parts := List.filter (fun (name, _) -> name <> arg) !parts;
         (arg_id,
          Ident.create_persistent ~ns:(optstring ns) app))
      cmi.cmi_functor_args
      (List.rev !Clflags.applied) in
  let parts =
    List.map (fun (modname, _) ->
        let arg_id =
          Ident.create_persistent ~ns:(optstring cmi.cmi_namespace) modname in
        Ident.make_functor_part arg_id;
        ((arg_id, Env.crc_of_unit modname cmi.cmi_namespace),
         Ident.create_persistent ~ns:(optstring ns) modname))
      !parts in
  let coercion, application =
    Typemod.applied_unit env instance parts cmi ns targetname in
  let target_id = Ident.create_persistent
      ~ns:(Longident.optstring ns) targetname in
  let funit_id = Ident.create_persistent
      ~ns:(Longident.optstring cmi.cmi_namespace) cmi.cmi_name in
  (* let path = Env.lookup_module *)
  (*     ~pers:true ~load:true cmi.Cmi_format.cmi_namespace *)
  (*     (Lident cmi.Cmi_format.cmi_name) env in *)
  (* let funit_id' = match path with *)
  (*     Path.Pident id -> id *)
  (*   | Path.Pdot _ | Path.Papply _ -> assert false (\* because it is persistent *\) in *)
  (* Format.printf "Funit_id': %s@." (Ident.unique_name funit_id'); *)
  let instance = List.fold_left (fun acc ((part, crc), applied) ->
      (part, applied) :: acc) instance parts in
  let lambda = Translmod.transl_applied_unit funit_id target_id instance coercion in
  let lam = Lprim(Psetglobal target_id, [lambda]) in
  if !Clflags.dump_lambda then
    Format.printf "%a@." Printlambda.lambda lam;
  let instrs =
    Bytegen.compile_implementation targetname lam in
  (* let application = *)
  (*   Some (cmi.cmi_name, cmi.cmi_namespace) in *)
  Emitcode.to_file ~application targetfile targetname instrs

let apply_functor_unit ppf funit initial_env =
  let prefix = Filename.chop_extension funit in
  let funit_cmi = prefix ^ ".cmi" in
  let cmi = Cmi_format.read_cmi funit_cmi in
  let targetname = String.capitalize (Filename.basename prefix) in
  let targetfile = Filename.basename funit in
  applied_object_file ppf cmi targetfile targetname !Clflags.ns initial_env
