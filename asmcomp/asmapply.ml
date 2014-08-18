open Longident
open Cmi_format
(* open Misc *)
open Compenv
open Format


let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x
let (+++) (x, y) f = (x, f y)

(* Needs a LOT of refactoring, to do once it is working *)

let applied_object_file ppf cmi prefix targetname optns env =
  let ns = match optns with
      None -> assert false
    | Some s -> Some (Longident.parse s) in
  Env.set_namespace_unit ns;
  let parts = ref @@
    List.filter (fun (modname, _) -> modname <> targetname) cmi.cmi_functor_parts in
  (* Format.printf "%s@." @@ String.concat "; " @@ *)
  (* List.map (fun (modname, _) -> *)
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
        let ns_app = if modname = cmi.cmi_name then cmi.cmi_namespace else ns in
        let arg_id =
          Ident.create_persistent ~ns:(optstring cmi.cmi_namespace) modname in
        Ident.make_functor_part arg_id;
        let app_id = Ident.create_persistent ~ns:(optstring ns_app) modname in
        ((arg_id, Env.crc_of_unit modname cmi.cmi_namespace), app_id))
      !parts in

  let coercion, application =
    Typemod.applied_unit env instance parts cmi ns targetname in

  let target_id = Ident.create_persistent
      ~ns:(Longident.optstring ns) targetname in
  let funit_id = Ident.create_persistent
      ~ns:(Longident.optstring cmi.cmi_namespace) cmi.cmi_name in
  (* Ident.make_functor_part funit_id; *)
  let instance = List.fold_left (fun acc ((part, crc), applied) ->
      (part, applied) :: acc) instance parts in

  let cmxfile = prefix ^ ".cmx" in
  (* let objfile = prefix ^ Config.ext_obj in *)

  let open Lambda in
  let str = Lprim (Psetfield(0, false),
                   [Lprim(Pgetglobal target_id, []);
                    Translmod.transl_store_apply 0 funit_id target_id instance coercion]) in
  (1, str)
  +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
  +++ Simplif.simplify_lambda
  +++ print_if ppf Clflags.dump_lambda Printlambda.lambda
  ++ Asmgen.compile_implementation prefix ppf;
  Compilenv.save_unit_info ~application cmxfile


let apply_functor_unit ppf funit initial_env =
  (* Compmisc.init_path true; *)
  let prefix = Filename.chop_extension funit in
  let funit_cmi = prefix ^ ".cmi" in
  let cmi = Cmi_format.read_cmi funit_cmi in
  let targetname = String.capitalize (Filename.basename prefix) in
  (* let targetfile = Filename.basename funit in *)
  let modulename = module_of_filename ppf funit prefix in
  Env.set_unit_name modulename;
  (* let env = Compmisc.initial_env() in *)
  Compilenv.reset ?packname:!Clflags.for_package modulename;

  applied_object_file ppf cmi prefix targetname !Clflags.ns initial_env
