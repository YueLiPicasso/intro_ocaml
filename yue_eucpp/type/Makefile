ADD_PATH = -I $(LIB_PATH)

MODSIG = $(addsuffix .mli, $(TYPE))
MODIMP = $(addsuffix .ml,  $(TYPE))
MODCMI = $(addsuffix .cmi, $(TYPE))
MODCMO = $(addsuffix .cmo, $(TYPE))

all : $(TYP)

$(TYP) : $(MODCMO)
	ocamlc -pack -o $(TYP) $(MODCMO)

$(MODCMI) &: $(MODSIG) 
	ocamlc -c $(ADD_PATH) $(MODSIG)

$(MODCMO) &: $(MODCMI) $(MODIMP)
	ocamlc -c $(ADD_PATH) $(MODIMP)

.PHONY : clean

clean :
	rm -rf *.cm*  *~
