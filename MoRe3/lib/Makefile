MODCMO = $(addsuffix .cmo, $(MODTOPO))
MODCMI = $(addsuffix .cmi, $(MODTOPO))
OCAMLC_C = ocamlc -c -rectypes
OCAMLC_P = ocamlc -pack -o $(LIB) -rectypes

$(LIB) : $(MODCMO) $(MODCMI)
	 $(OCAMLC_P) $(MODCMO)

%.cmi : %.mli
	$(OCAMLC_C) $<

%.cmo : %.ml %.cmi
	$(OCAMLC_C) $<

.PHONY : clean

clean :
	rm -rf *.cm*  *~




