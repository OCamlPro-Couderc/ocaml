all:
	ocamlc -c intf.mli
	ocamlc -c -functor intf.mli mod.ml
	ocamlc -c x/res.ml
	ocamlc -c -apply x/res.cmi mod.cmo
	ocamlc -o res.byte mod.cmo x/res.cmo x/mod.cmo test.ml

opt:
	ocamlopt -c intf.mli
	ocamlopt -c -functor intf.mli mod.ml
	ocamlopt -c x/res.ml
	ocamlopt -c -apply x/res.cmi mod.cmx
	ocamlopt -o res.opt mod.cmx x/res.cmx x/mod.cmx test.ml

clean:
	rm -f *.cm* */*.cm* *.opt *.byte *.o */*.o *~
