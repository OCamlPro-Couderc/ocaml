(* TEST
files="m.mli n.mli a.mli b.mli a.ml b.ml n.ml use_p.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/use_p.byte"
** ocamlc.byte
flags = "-for-pack P -pack-is-recursive P -recursive"
all_modules = "m.mli n.mli"
*** ocamlc.byte
flags = "-for-pack P.M -pack-is-recursive P -pack-is-recursive P.M -recursive"
all_modules = "a.mli b.mli"
**** ocamlc.byte
flags = "-for-pack P.M -pack-is-recursive P -pack-is-recursive P.M"
module = "a.ml"
***** ocamlc.byte
flags = "-for-pack P.M -pack-is-recursive P -pack-is-recursive P.M"
module = "b.ml"
****** ocamlc.byte
flags = "-for-pack P -recursive-pack -pack-is-recursive P"
module = ""
program = "m.cmo"
all_modules = "a.cmo b.cmo"
******* ocamlc.byte
flags = "-for-pack P -pack-is-recursive P"
module = "n.ml"
******** ocamlc.byte
flags = "-recursive-pack"
module = ""
program = "p.cmo"
all_modules = "m.cmo n.cmo"
********* ocamlc.byte
module = "use_p.ml"
flags = ""
********** ocamlc.byte
module = ""
program = "use_p.byte"
flags = ""
all_modules = "p.cmo use_p.cmo"
*********** run
************ check-program-output
reference = "${test_source_directory}/use_p.reference"

* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/use_p.asm"
** ocamlopt.byte
flags = "-for-pack P -pack-is-recursive P -recursive"
all_modules = "m.mli n.mli"
*** ocamlopt.byte
flags = "-for-pack P.M -pack-is-recursive P -pack-is-recursive P.M -recursive"
all_modules = "a.mli b.mli"
**** ocamlopt.byte
flags = "-for-pack P.M -pack-is-recursive P -pack-is-recursive P.M"
module = "a.ml"
***** ocamlopt.byte
flags = "-for-pack P.M -pack-is-recursive P -pack-is-recursive P.M"
module = "b.ml"
****** ocamlopt.byte
flags = "-for-pack P -recursive-pack -pack-is-recursive P"
module = ""
program = "m.cmx"
all_modules = "a.cmx b.cmx"
******* ocamlopt.byte
flags = "-for-pack P -pack-is-recursive P"
module = "n.ml"
******** ocamlopt.byte
flags = "-recursive-pack"
module = ""
program = "p.cmx"
all_modules = "m.cmx n.cmx"
********* ocamlopt.byte
module = "use_p.ml"
flags = ""
********** ocamlopt.byte
module = ""
program = "use_p.asm"
flags = ""
all_modules = "p.cmx use_p.cmx"
*********** run
************ check-program-output
reference = "${test_source_directory}/use_p.reference"
*)