NAD= -no-alias-deps

LIB= list.ml array.ml
OBJS_LIB= $(LIB:.ml=.cmo)
FLAGS= $(NAD) -open Std -o $@
LINKED= std_list.cmo std_array.cmo std.cmo


all: std.cmo std_list.cmo std_array.cmo main

main: main.ml
	ocamlc -o res $(LINKED) $^

std.cmo: std.ml
	ocamlc -c $(NAD) $^

std_list.cmo: list.ml
	ocamlc -c $(FLAGS) -c $<

std_array.cmo: array.ml
	ocamlc -c $(FLAGS) -c $<

clean :
	rm -f *.cm[ioxt] *.cmti depend *.o *~
	rm -f $(SRC_MLL:.mll=.ml) $(SRC_MLY:.mly=.ml) $(SRC_MLY:.mly=.mli)
	rm -f $(EXEC)
	rm -f $(EXEC).opt

depend: $(SMLIY)
	$(CAMLDEP) $(SMLIY) $(SMLIY:.mly:.mli) > depend

-include depend
