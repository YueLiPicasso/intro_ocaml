MODCMO = $(addsuffix .cmo, $(MODTOPO))
OCAMLC_C = ocamlc -c

all : $(LIB)

$(LIB) : $(MODCMO)
	ocamlc -pack -o $(LIB) $(MODCMO)

%.cmi : %.mli
	$(OCAMLC_C) $<

%.cmo : %.ml %.cmi
	$(OCAMLC_C) $<

.PHONY : clean

clean :
	rm -rf *.cm*  *~




