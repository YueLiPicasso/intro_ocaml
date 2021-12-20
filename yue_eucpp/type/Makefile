TYPE = option
MODSIG = $(addsuffix .mli, $(TYPE))
MODIMP = $(addsuffix .ml,  $(TYPE))
MODCMI = $(addsuffix .cmi, $(TYPE))
MODCMO = $(addsuffix .cmo, $(TYPE))

all : $(MODCMO)

.PHONY : lib clean

lib :
	$(MAKE) -C $(LIB_PATH)

$(MODCMI) &: lib $(MODSIG) 
	ocamlc -c -I $(LIB_PATH) $(MODSIG)

$(MODCMO) &: $(MODCMI) $(MODIMP)
	ocamlc -c -I $(LIB_PATH) $(MODIMP)

clean :
	rm -rf *.cm*  *~
