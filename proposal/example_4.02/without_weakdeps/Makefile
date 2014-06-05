
# Module behaving as namespace
NAMESPACE= ns.ml
NOBJ= $(NAMESPACE:.ml=.cmo)
NOBJX= $(NOBJ:.cmo=.cmx)
STD= $(NOBJ:.cmo=.cma)
STDX= $(STD:.cma=.cmxa)
NS= $(basename $(NAMESPACE))
PREFIX= $(NS)_
NSMOD= $(shell echo $(NS) | sed 's/^\(.\)/\U\1/')

# For pack
NSPACK= $(NS)_pack.cmo

# Modules from the namespace (the library)
LIB= ns_a.ml ns_b.ml ns_c.ml
POBJS= $(LIB:.ml=.cmo)
# POBJS= $(addprefix $(PREFIX), $(OBJS))
POBJSX= $(POBJS:.cmo=.cmx)

# Our main program
SRC= main.ml
SOBJ= $(SRC:.ml=.cmo)
SOBJX= $(SOBJ:.cmo=.cmx)
EXEC= prog

CAMLC= ocamlc
CAMLOPT= ocamlopt
CAMLDEP= ocamldep
NAD= -no-alias-deps
FLAGS= -o $@


all: $(EXEC) $(EXEC).opt

stats: $(EXEC) $(EXEC).opt
	@echo "Size of executables generated without -no-alias-deps:";
	@$(foreach f, $^, stat --printf="%n: %s bytes.\n" $(f);)

$(EXEC): $(STD) $(SOBJ)
	$(CAMLC) -o $@ $^

$(EXEC).opt: $(STDX) $(SOBJX)
	$(CAMLOPT) -o $@ $^

$(EXEC)_pack: $(NSPACK) main_pack.cmo
	$(CAMLC) -o $@ $^

$(STD): $(PCMIS) $(POBJS) $(NOBJ) 
	$(CAMLC) -a -o $@ $(POBJS) $(NOBJ)

$(STDX): $(PCMIS) $(POBJSX) $(NOBJX)
	$(CAMLOPT) -a -o $@ $(POBJSX) $(NOBJX)

# $(NSPACK): $(CMIS) $(OBJS)
# 	$(CAMLC) -pack -o $@ $(OBJS)

# $(NS).cmo: $(NS).ml
# 	$(CAMLC) -c $(NAD) $<

# $(NS).cmi: $(NS).mli
# 	$(CAMLC) -c $(NAD) $<

# $(NS).cmx: $(NS).ml
# 	$(CAMLOPT) -c $(NAD) $<

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmo: %.ml
	$(CAMLC) -c $<

%.cmx: %.ml
	$(CAMLOPT) -c $<

# $(PREFIX)%.cmi: %.mli
# 	$(CAMLC) -c $(FLAGS) $<

# $(PREFIX)%.cmo: %.ml
# 	$(CAMLC) -c $(FLAGS) $<

# $(PREFIX)%.cmx: %.ml
# 	$(CAMLOPT) -c $(FLAGS) $<

clean :
	rm -f *.cm[ioxta] *.cmti *.cmxa depend *.[oa] *~
	rm -f $(EXEC) $(EXEC).opt
	rm -f sed.script

depend: $(NAMESPACE) $(LIB) $(SRC)
	$(CAMLDEP) $^ > depend

-include depend
