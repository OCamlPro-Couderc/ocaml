
# Module behaving as namespace
NAMESPACE= std.ml
NOBJ= $(NAMESPACE:.ml=.cmo)
NOBJA= $(NOBJ:.cmo=.cma)
PREFIX= $(basename $(NAMESPACE))_
NSMOD= Std

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

all: $(EXEC)

$(EXEC): $(NOBJA) $(SOBJ)
	$(CAMLC) -o $@ $^

$(NOBJA): $(NOBJ) $(POBJS)
	$(CAMLC) -a -o $@ $^

%.cmo: %.ml
	$(CAMLC) -c $(NAD) $<

$(PREFIX)%.cmo: %.ml
	$(CAMLC) -c $(FLAGS) $<

clean :
	rm -f *.cm[ioxt] *.cmti depend *.o *~
	rm -f $(EXEC)

# depend: $(NAMESPACE) $(LIB) $(SRC)
# 	$(CAMLDEP) $(NAMESPACE) $(LIB) $(SRC) > depend

# -include depend
