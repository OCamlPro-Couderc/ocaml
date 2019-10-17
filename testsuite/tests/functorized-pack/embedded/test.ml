(* TEST

files="param.mli m1.ml m2.ml n1.ml n2.ml use_make.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/use_make.byte"
** ocamlc.byte
flags = "-parameter-of Make"
module = "param.mli"
*** ocamlc.byte
flags = "-for-pack Make(Param).M"
module = "m1.ml"
**** ocamlc.byte
flags = "-for-pack Make(Param).M"
module = "m2.ml"
***** ocamlc.byte
flags = "-pack -for-pack Make(Param)"
module = ""
program = "m.cmo"
all_modules = "m1.cmo m2.cmo"
****** ocamlc.byte
flags = "-for-pack Make(Param).N"
module = "n1.ml"
******* ocamlc.byte
flags = "-for-pack Make(Param).N"
module = "n2.ml"
******** ocamlc.byte
flags = "-pack -for-pack Make(Param)"
module = ""
program = "n.cmo"
all_modules = "n1.cmo n2.cmo"
********* ocamlc.byte
flags = "-parameter Param -pack"
module = ""
program = "make.cmo"
all_modules = "m.cmo n.cmo"
********** ocamlc.byte
module = "use_make.ml"
flags = ""
*********** ocamlc.byte
module = ""
program = "use_make.byte"
flags = ""
all_modules = "make.cmo use_make.cmo"
************ run
************* check-program-output
reference = "${test_source_directory}/use_make.reference"

*)
