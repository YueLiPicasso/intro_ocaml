TOPFILE = Mylib.cma
PACKINGOBJ = Mylib.cmo
AOBJ = Mylib__A.cmo
BOBJ =  Mylib__B.cmo
PACKINGSRC =  Mylib.ml
AINTERF = Mylib__A.mli
BINTERF = Mylib__B.mli
ASOURCE = Mylib__A.ml
BSOURCE = Mylib__B.ml
PACKINGDEPS = $(AINTERF) $(BINTERF) $(PACKINGSRC)
LIBOBJS =  $(PACKINGOBJ) $(AOBJ) $(BOBJ)
OCAMLC = ocamlc -c -no-alias-deps -open Mylib
CLEAN = $(wildcard *.cmo *.cmi *.cma)

.DEFAULT_GOAL := show

.PHONY: clean show




$(TOPFILE) : $(LIBOBJS)
	ocamlc -a -o $(TOPFILE) $(LIBOBJS)

$(AOBJ) : $(ASOURCE) $(PACKINGOBJ)
	$(OCAMLC) $(ASOURCE) 

$(BOBJ) : $(BSOURCE) $(PACKINGOBJ)
	$(OCAMLC) $(BSOURCE) 

$(PACKINGOBJ) : $(PACKINGDEPS)
	ocamlc -c -no-alias-deps $(PACKINGDEPS)

clean :
	rm -f $(CLEAN)

file_list != find . -name '*.ml'
foo := foo
bar = bar

undefine foo

show :
	@echo $(file_list)
	$(info printed by info)
	$(info $(origin foo))
	$(info $(origin bar))
	$(info $(flavor foo))
	$(info $(flavor bar))

