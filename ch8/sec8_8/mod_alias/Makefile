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


.PHONY: clean

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
