
# Module behaving as namespace
NAMESPACE= std.ml
NOBJ= $(NAMESPACE:.ml=.cmo)
STD= $(NOBJ:.cmo=.cma)
NS= $(basename $(NAMESPACE))
PREFIX= $(NS)_
NSMOD= $(shell echo $(NS) | sed 's/^\(.\)/\U\1/')

# Modules from the namespace (the library)
LIB= list.ml array.ml
OBJS= $(LIB:.ml=.cmo)
POBJS= $(addprefix $(PREFIX), $(OBJS))
LINKED= $(NOBJA)


# Our main program
SRC= main.ml
SOBJ= $(SRC:.ml=.cmo)
EXEC= prog

CAMLC= ocamlc
CAMLDEP= ocamldep
NAD= -no-alias-deps
FLAGS= $(NAD) -open $(NSMOD) -o $@

.PHONY: depend

all: depend $(EXEC)
	$(NSM)

$(EXEC): $(STD) $(SOBJ)
	$(CAMLC) -o $@ $^

$(STD): $(NOBJ) $(POBJS)
	$(CAMLC) -a -o $@ $^

%.cmo: %.ml
	$(CAMLC) -c $(NAD) $<

$(PREFIX)%.cmo: %.ml
	$(CAMLC) -c $(FLAGS) $<

clean :
	rm -f *.cm[ioxta] *.cmti depend *.o *~
	rm -f $(EXEC)

depend: $(NAMESPACE) $(LIB) $(SRC)
	$(CAMLDEP) $(NAMESPACE) > depend
	rm -f sed.script
	$(foreach f, $(OBJS), echo 's/$(f)/$(PREFIX)$(f)/' >> sed.script;)
	$(CAMLDEP) $(LIB) $(SRC) | sed -f sed.script >> depend

-include depend
