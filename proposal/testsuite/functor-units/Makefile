CMOS=  builder.cmo mod.cmo
XCMOS = x/res.cmo x/builder.cmo x/mod.cmo
YCMOS = y/arg.cmo y/builder.cmo y/mod.cmo

CMXS=$(CMOS:.cmo=.cmx)
XCMXS=$(XCMOS:.cmo=.cmx)
YCMXS=$(YCMOS:.cmo=.cmx)

STDLIB = ../../../stdlib
CAMLC = ../../../ocamlc.opt -I $(STDLIB)
CAMLOPT = ../../../ocamlopt.opt -S -I $(STDLIB)
CAMLRUN = ../../../byterun/ocamlrun
FLAGS =	-g -no-alias-deps

all:
	$(CAMLC) -c intf.mli
	$(CAMLC) -c -functor intf.cmi builder.ml
	$(CAMLC) -c -functor intf.cmi mod.ml
	$(CAMLC) -c x/res.ml
	$(CAMLC) -c -apply x/res.cmi builder.cmo
	$(CAMLC) -c -apply x/res.cmi mod.cmo
	$(CAMLC) -c y/arg.ml
	$(CAMLC) -c -apply y/arg.cmi builder.cmo
	$(CAMLC) -c -apply y/arg.cmi mod.cmo
	$(CAMLC) -o res.byte $(CMOS) $(XCMOS) $(YCMOS) test.ml
	$(CAMLRUN) ./res.byte

opt:
	$(CAMLOPT)  -c -g intf.mli
	$(CAMLOPT)  -c -g -functor intf.cmi -dlambda builder.ml
	$(CAMLOPT)  -c -g -functor intf.cmi -dlambda mod.ml
	$(CAMLOPT)  -c -g x/res.ml
	$(CAMLOPT)  -c -g -apply x/res.cmi -dlambda builder.cmx
	$(CAMLOPT)  -c -g -apply x/res.cmi -dlambda mod.cmx
	$(CAMLOPT)  -c -g y/arg.ml
	$(CAMLOPT)  -c -g -apply y/arg.cmi builder.cmx
	$(CAMLOPT)  -c -g -apply y/arg.cmi mod.cmx
	$(CAMLOPT)  -o res.opt -g $(CMXS) $(XCMXS) $(YCMXS) test.ml
	./res.opt

clean:
	rm -f *.cm* */*.cm* *.opt *.byte *.o */*.o *~
