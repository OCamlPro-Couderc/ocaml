
X_INTF = m1.mli
X_IMPL = pervasives.ml m1.ml m2.ml
X_IMPL_SRC = $(addprefix x/, $(X_IMPL))
X_INTF_SRC = $(addprefix x/, $(X_INTF))
X_CMIS = $(X_INTF_SRC:.mli=.cmi)
X_CMOS = $(X_IMPL_SRC:.ml=.cmo)
X_CMXS = $(X_CMOS:.cmo=.cmx)

X_OBJS = $(X_CMIS) $(X_CMOS) $(X_CMXS)

Y_INTF = 
Y_IMPL = mod.ml included.ml linked.ml
Y_IMPL_SRC = $(addprefix y/, $(Y_IMPL))
Y_INTF_SRC = $(addprefix y/, $(Y_INTF))
Y_CMIS = $(Y_INTF_SRC:.mli=.cmi)
Y_CMOS = $(Y_IMPL_SRC:.ml=.cmo)
Y_CMXS = $(Y_CMOS:.cmo=.cmx)

Y_OBJS = $(Y_CMIS) $(Y_CMOS) $(Y_CMXS)

Z_INTF = 
Z_IMPL = mod.ml linked.ml
Z_IMPL_SRC = $(addprefix z/, $(Z_IMPL))
Z_INTF_SRC = $(addprefix z/, $(Z_INTF))
Z_CMIS = $(Z_INTF_SRC:.mli=.cmi)
Z_CMOS = $(Z_IMPL_SRC:.ml=.cmo)
Z_CMXS = $(Z_CMOS:.cmo=.cmx)

Z_OBJS = $(Z_CMIS) $(Z_CMOS) $(Z_CMXS)

FUN_INTF = intf.mli
FUN_IMPL = mod.ml
FUN_IMPL_SRC = $(addprefix functor/, $(FUN_IMPL))
FUN_INTF_SRC = $(addprefix functor/, $(FUN_INTF))
FUN_CMIS = $(FUN_INTF_SRC:.mli=.cmi)
FUN_CMOS = $(FUN_IMPL_SRC:.ml=.cmo)
FUN_CMXS = $(FUN_CMOS:.cmo=.cmx)

FUN_OBJS = $(FUN_CMIS) $(FUN_CMOS) $(FUN_CMXS)

# Packs modules

P1_IMPL = mod.ml
P1_IMPL_SRC = $(addprefix p1/, $(P1_IMPL))
P1_CMOS = $(P1_IMPL_SRC:.ml=.cmo)
P1_CMXS = $(P1_CMOS:.cmo=.cmx)

P1_OBJS = $(P1_CMIS) $(P1_CMOS) $(P1_CMXS)

P2_IMPL = mod.ml
P2_IMPL_SRC = $(addprefix p2/, $(P2_IMPL))
P2_CMOS = $(P2_IMPL_SRC:.ml=.cmo)
P2_CMXS = $(P2_CMOS:.cmo=.cmx)

P2_OBJS = $(P2_CMIS) $(P2_CMOS) $(P2_CMXS)

CMIS = $(X_CMIS) $(Y_CMIS) $(Z_CMIS) $(FUN_CMIS)
CMOS = $(X_CMOS) $(Y_CMOS) $(Z_CMOS) $(FUN_CMOS) $(P1_CMOS) $(P2_CMOS)
CMXS = $(X_CMXS) $(Y_CMXS) $(Z_CMXS) $(FUN_CMXS) $(P1_CMXS) $(P2_CMXS)
RCMIS = $(CMOS:.cmo=.cmi) # for cleaning rule
OBJS = $(CMXS:.cmx=.o)
ASMS = $(OBJS:.o=.s)

EXEC = main functor link includes pack
BYTE_EXEC = $(addsuffix .byte,$(EXEC))
NAT_EXEC = $(addsuffix .nat, $(EXEC))
SRC_EXEC = $(addsuffix .ml, $(EXEC))

SRCS = $(X_IMPL_SRC) $(X_INTF_SRC) $(Y_IMPL_SRC) $(Y_INTF_SRC) \
	$(Z_IMPL_SRC) $(Z_INTF_SRC) $(FUN_IMPL_SRC) $(FUN_INTF_SRC) \
	$(P1_IMPL_SRC) $(P1_INTF_SRC) $(P2_IMPL_SRC) $(P2_INTF_SRC) \
	$(SRC_EXEC)

STDLIB = ../../stdlib
CAMLC = ../../ocamlc.opt -I $(STDLIB)
CAMLOPT = ../../ocamlopt.opt -S -I $(STDLIB)
CAMLRUN = ../../byterun/ocamlrun
FLAGS =	-g -no-alias-deps

%.cmo: %.ml
	$(CAMLC) $(FLAGS) -c $<

%.cmi: %.mli
	$(CAMLC) $(FLAGS) -c $<

%.cmx: %.ml
	$(CAMLOPT) $(FLAGS) -c $<

all: test-byte test-opt

byte: $(CMIS) $(BYTE_EXEC)

opt: $(CMIS) $(NAT_EXEC)

test-byte: byte
	@for exec in $(BYTE_EXEC); do \
		echo "Executing $$exec"; \
		$(CAMLRUN) ./$$exec; \
	done

test-opt: opt
	@for exec in $(NAT_EXEC); do \
		echo "Executing $$exec"; \
		./$$exec; \
	done

main.byte: x.cma y.cma z.cma main.cmo
	$(CAMLC) $(FLAGS) -o $@ $^

main.nat: x.cmxa y.cmxa z.cmxa main.cmx
	$(CAMLOPT) $(FLAGS) -o $@ $^

link.byte: y.cma z.cma link.ml
	$(CAMLC) $(FLAGS) -o $@ $^

link.nat: y.cmxa z.cmxa link.ml
	$(CAMLOPT) $(FLAGS) -o $@ $^

functor.byte: x.cma functor.cma fun.ml
	$(CAMLC) $(FLAGS) -o $@ $^

functor.nat: x.cmxa functor.cmxa fun.ml
	$(CAMLOPT) $(FLAGS) -o $@ $^

includes.byte: y.cma includes.ml
	$(CAMLC) $(FLAGS) -o $@ $^

includes.nat: y.cmxa includes.ml
	$(CAMLOPT) $(FLAGS) -o $@ $^

# multi.byte: x.cma y.cma z.cma multi.ml
# 	$(CAMLC) $(FLAGS) -w +52 -o $@ $^

# multi.nat: x.cmxa y.cmxa z.cmxa multi.ml
# 	$(CAMLOPT) $(FLAGS) -w +52 -o $@ $^

# Cmas/Cmxa

x.cma: $(X_CMOS)
	$(CAMLC) -a -o $@ $^

x.cmxa: $(X_CMXS)
	$(CAMLOPT) -a -o $@ $^

y.cma: $(Y_CMOS)
	$(CAMLC) -a -o $@ $^

y.cmxa: $(Y_CMXS)
	$(CAMLOPT) -a -o $@ $^

z.cma: $(Z_CMOS)
	$(CAMLC) -a -o $@ $^

z.cmxa: $(Z_CMXS)
	$(CAMLOPT) -a -o $@ $^

functor.cma: $(FUN_CMOS)
	$(CAMLC) -a -o $@ $^

functor.cmxa: $(FUN_CMXS)
	$(CAMLOPT) -a -o $@ $^

# Specific rule for the compilation of pervasives

x/pervasives.cmo: x/pervasives.ml
	$(CAMLC) $(FLAGS) -nopervasives -c $<

x/pervasives.cmx: x/pervasives.ml
	$(CAMLOPT) $(FLAGS) -nopervasives -c $<

# Specific rules for the packs

pack.byte: p1/p.cmo p2/p.cmo pack.ml
	$(CAMLC) $(FLAGS) -o $@ $^

pack.nat: p1/p.cmx p2/p.cmx pack.ml
	$(CAMLOPT) $(FLAGS) -o $@ $^

p1/p.cmo: $(P1_CMOS)
	$(CAMLC) $(FLAGS) -o $@ -pack $^

p1/p.cmx: $(P1_CMXS)
	$(CAMLOPT) $(FLAGS) -o $@ -pack $^

p2/p.cmo: $(P2_CMOS)
	$(CAMLC) $(FLAGS) -o $@ -pack $^

p2/p.cmx: $(P2_CMXS)
	$(CAMLOPT) $(FLAGS) -o $@ -pack $^

p1/%.cmo: p1/%.ml
	$(CAMLC) $(FLAGS) -c -for-pack P $<

p1/%.cmx: p1/%.ml
	$(CAMLOPT) $(FLAGS) -c -for-pack P $<

p2/%.cmo: p2/%.ml
	$(CAMLC) $(FLAGS) -c -for-pack P $<

p2/%.cmx: p2/%.ml
	$(CAMLOPT) $(FLAGS) -c -for-pack P $<

depend:
	ocamldep $(SRCS) > .depend

clean:
	rm -f $(CMIS) $(CMOS) $(CMXS) $(BYTE_EXEC) $(NAT_EXEC) $(OBJS) $(ASMS)
	rm -f $(RCMIS) p1/p.* p2/p.*
	rm -f *~ *.o */*.o *.s *.cm[ioxa] *.cmxa

-include .depend
