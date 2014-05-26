
# Module behaving as namespace
NAMESPACE= std.ml
NOBJ= $(NAMESPACE:.ml=.cmo)
NS= $(basename $(NAMESPACE))
PREFIX= $(NS)_
NSMOD= Std

# Modules from the namespace (the library)
LIB= list.ml array.ml
OBJS= $(LIB:.ml=.cmo)
POBJS= $(addprefix $(PREFIX), $(OBJS))
LINKED= $(POBJS) $(NOBJ)

# Our main program
SRC= main.ml
SOBJ= $(SRC:.ml=.cmo)
EXEC= prog

CAMLC= ocamlc
CAMLDEP= ocamldep
NAD= -no-alias-deps
FLAGS= $(NAD) -open $(NSMOD) -o $@

all: $(NOBJ) $(POBJS) $(EXEC)

$(EXEC): $(SOBJ)
	ocamlc -o $@ $(LINKED) $^

%.cmo: %.ml
	ocamlc -c $(NAD) $^

$(PREFIX)%.cmo: %.ml
	ocamlc -c $(FLAGS) $<

clean :
	rm -f *.cm[ioxt] *.cmti depend *.o *~
	rm -f $(EXEC)

# depend: $(NAMESPACE) $(LIB) $(SRC)
# 	$(CAMLDEP) $(NAMESPACE) $(LIB) $(SRC) > depend

# -include depend
