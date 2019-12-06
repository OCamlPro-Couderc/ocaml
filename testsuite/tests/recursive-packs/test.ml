(* TEST
files="a.mli aset.mli a.ml aset.ml use_aset.ml"
* setup-ocamlc.byte-build-env
program = "${test_build_directory}/use_make.byte"
** ocamlc.byte
flags = "-for-recursive-pack P -recursive"
all_modules = "a.mli aset.mli"
*** ocamlc.byte
flags = "-for-recursive-pack P -recursive"
module = "a.ml"
**** ocamlc.byte
flags = "-for-recursive-pack P -recursive"
module = "aset.ml"
***** ocamlc.byte
flags = "-recursive-pack"
module = ""
program = "p.cmo"
all_modules = "a.cmo aset.cmo"
****** ocamlc.byte
module = "use_aset.ml"
flags = ""
******* ocamlc.byte
module = ""
program = "use_aset.byte"
flags = ""
all_modules = "p.cmo use_aset.cmo"
******** run
********* check-program-output
reference = "${test_source_directory}/use_aset.reference"

* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/use_make.asm"
** ocamlopt.byte
flags = "-for-recursive-pack P -recursive"
all_modules = "a.mli aset.mli"
*** ocamlopt.byte
flags = "-for-recursive-pack P -recursive"
module = "a.ml"
**** ocamlopt.byte
flags = "-for-recursive-pack P -recursive"
module = "aset.ml"
***** ocamlopt.byte
flags = "-recursive-pack"
module = ""
program = "p.cmx"
all_modules = "a.cmx aset.cmx"
****** ocamlopt.byte
module = "use_aset.ml"
flags = ""
******* ocamlopt.byte
module = ""
program = "use_aset.asm"
flags = ""
all_modules = "p.cmx use_aset.cmx"
******** run
********* check-program-output
reference = "${test_source_directory}/use_aset.reference"
*)
