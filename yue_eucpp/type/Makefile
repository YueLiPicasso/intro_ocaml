ADD_PATH = -I $(LIB_PATH)
MODCMO = $(addsuffix .cmo, $(TYPE))
OCAMLC_C = ocamlc -c $(ADD_PATH) -rectypes
OCAMLC_PACK = ocamlc -pack -o $(TYP) -rectypes

all : $(TYP)

$(TYP) : $(MODCMO)
	$(OCAMLC_PACK) $(MODCMO)

%.cmi : %.mli
	$(OCAMLC_C) $<

%.cmo : %.ml %.cmi
	$(OCAMLC_C) $<

.PHONY : clean

clean :
	rm -rf *.cm*  *~


