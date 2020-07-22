TOPFILE = mining
SOURCES = mining.ml
MODS = LRational.ml
OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
OUTB = $(TOPFILE).byte
OUTO = $(TOPFILE).opt
# -dsource --- dump a text *after* camlp5 extension
CAMLP5 = -syntax camlp5o -package ocanren.syntax,GT.syntax.all 
PXFLAGS = $(CAMLP5)
INCLUDES = -I LRational
# add flag -i to BFLAGS to show types only
BFLAGS = -rectypes -g $(INCLUDES)
OFLAGS = $(BFLAGS)
CLEAN = $(wildcard *.cmi *.cmo *.cmx *.annot *.o *.opt *.byte *~ *depend)

.PHONY : all clean

all:.depend $(OUTO)

.depend: $(SOURCES)
	$(OCAMLDEP) $(INCLUDES) $(PXFLAGS) *.ml > .depend

$(OUTO): $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(OUTO) $(OFLAGS)  $(LIBS:.cma=.cmxa) -linkpkg -package GT,ocanren $(MODS:.ml=.cmx) $(SOURCES:.ml=.cmx) 

$(OUTB): $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $(OUTB) $(BFLAGS) $(LIBS) -linkpkg -package GT,ocanren $(MODS:.ml=.cmo) $(SOURCES:.ml=.cmo) 

clean:
	rm -Rf $(CLEAN)

-include .depend
# generic rules

###############
%.cmi: %.mli
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.cmo: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.o: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

