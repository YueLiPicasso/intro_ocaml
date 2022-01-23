ADD_PATH = -I $(LIB_PATH)
MODCMO = $(addsuffix .cmo, $(TYPE))
MODCMI = $(addsuffix .cmi, $(TYPE))
OCAMLC_C = ocamlc -c -rectypes $(ADD_PATH) 
OCAMLC_PACK = ocamlc -pack -rectypes -o $(TYP)

$(TYP) : $(MODCMO) $(MODCMI)
	$(OCAMLC_PACK) $(MODCMO)

%.cmi : %.mli
	$(OCAMLC_C) $<

%.cmo : %.ml %.cmi
	$(OCAMLC_C) $<

.PHONY : clean

clean :
	rm -rf *.cm*  *~


