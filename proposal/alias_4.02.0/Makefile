
# Module behaving as namespace
NAMESPACE= std.ml
NOBJ= $(NAMESPACE:.ml=.cmo)
NOBJX= $(NOBJ:.cmo=.cmx)
STD= $(NOBJ:.cmo=.cma)
STDX= $(STD:.cma=.cmxa)
NS= $(basename $(NAMESPACE))
PREFIX= $(NS)_
NSMOD= $(shell echo $(NS) | sed 's/^\(.\)/\U\1/')

# Modules from the namespace (the library)
LIB= list.ml array.ml
LIB_MLI= list.mli array.mli
OBJS= $(LIB:.ml=.cmo)
POBJS= $(addprefix $(PREFIX), $(OBJS))
POBJSX= $(POBJS:.cmo=.cmx)
CMIS= $(addprefix $(PREFIX), $(LIB_MLI:.mli=.cmi))

# Our main program
SRC= main.ml
SOBJ= $(SRC:.ml=.cmo)
SOBJX= $(SOBJ:.cmo=.cmx)
EXEC= prog

CAMLC= ocamlc
CAMLOPT= ocamlopt
CAMLDEP= ocamldep
NAD= -no-alias-deps
FLAGS= $(NAD) -open $(NSMOD) -o $@

.PHONY: depend

all: $(EXEC) $(EXEC).opt

$(EXEC): $(STD) $(SOBJ)
	$(CAMLC) -o $@ $^

$(EXEC).opt: $(STDX) $(SOBJX)
	$(CAMLOPT) -o $@ $^

$(STD): $(NOBJ) $(CMIS) $(POBJS)
	$(CAMLC) -a -o $@ $(NOBJS) $(POBJS)

$(STDX): $(NOBJX) $(CMIS) $(POBJSX)
	$(CAMLOPT) -a -o $@ $(NOBJX) $(POBJSX)

%.cmi: %.mli
	$(CAMLC) -c $(NAD) $<

%.cmo: %.ml
	$(CAMLC) -c $(NAD) $<

%.cmx: %.ml
	$(CAMLOPT) -c $(NAD) $<

$(PREFIX)%.cmi: %.mli
	$(CAMLC) -c $(FLAGS) $<

$(PREFIX)%.cmo: %.ml
	$(CAMLC) -c $(FLAGS) $<

$(PREFIX)%.cmx: %.ml
	$(CAMLOPT) -c $(FLAGS) $<

clean :
	rm -f *.cm[ioxta] *.cmti depend *.o *~
	rm -f $(EXEC)

depend: $(NAMESPACE) $(LIB) $(SRC)
	$(CAMLDEP) $(NAMESPACE) > depend
	rm -f sed.script
	$(foreach f, $(basename $(OBJS)), echo 's/$(f)/$(PREFIX)$(f)/g' >> sed.script;)
	$(CAMLDEP) $(LIB) $(SRC) | sed -f sed.script >> depend

-include depend
