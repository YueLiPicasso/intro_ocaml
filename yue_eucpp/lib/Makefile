MODSIG = $(addsuffix .mli, $(MODTOPO))
MODIMP = $(addsuffix .ml,  $(MODTOPO))
MODCMI = $(addsuffix .cmi, $(MODTOPO))
MODCMO = $(addsuffix .cmo, $(MODTOPO))

all : $(LIB)

# pack the modules

$(LIB) : $(MODCMO)
	ocamlc -pack -o $(LIB) $(MODCMO)

# compile implementation

$(MODCMO) &: $(MODCMI) $(MODIMP)
	ocamlc -c $(MODIMP)

# compile interface

$(MODCMI) &: $(MODSIG)
	ocamlc -c $(MODSIG)

.PHONY : clean

clean :
	rm -rf *.cm*  *~




