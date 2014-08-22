CMOS=  builder.cmo mod.cmo
XCMOS = x/res.cmo x/builder.cmo x/mod.cmo
YCMOS = y/arg.cmo y/builder.cmo y/mod.cmo

CMXS=$(CMOS:.cmo=.cmx)
XCMXS=$(XCMOS:.cmo=.cmx)
YCMXS=$(YCMOS:.cmo=.cmx)

all:
	ocamlc -c intf.mli
	ocamlc -c -functor intf.cmi builder.ml
	ocamlc -c -functor intf.cmi mod.ml
	ocamlc -c x/res.ml
	ocamlc -c -apply x/res.cmi builder.cmo
	ocamlc -c -apply x/res.cmi mod.cmo
	ocamlc -c y/arg.ml
	ocamlc -c -apply y/arg.cmi builder.cmo
	ocamlc -c -apply y/arg.cmi mod.cmo
	ocamlc -o res.byte $(CMOS) $(XCMOS) $(YCMOS) test.ml

opt:
	ocamlopt -c -g intf.mli
	ocamlopt -c -g -functor intf.cmi -dlambda builder.ml
	ocamlopt -c -g -functor intf.cmi -dlambda mod.ml
	ocamlopt -c -g x/res.ml
	ocamlopt -c -g -apply x/res.cmi -dlambda builder.cmx
	ocamlopt -c -g -apply x/res.cmi -dlambda mod.cmx
	ocamlopt -c -g y/arg.ml
	ocamlopt -c -g -apply y/arg.cmi builder.cmx
	ocamlopt -c -g -apply y/arg.cmi mod.cmx
	ocamlopt -o res.opt -g $(CMXS) $(XCMXS) $(YCMXS) test.ml

clean:
	rm -f *.cm* */*.cm* *.opt *.byte *.o */*.o *~
