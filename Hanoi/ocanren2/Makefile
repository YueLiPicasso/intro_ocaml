TOPFILE = Hanoio
OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
SOURCES = Hanoio.ml
# -dsource --- dump a text *after* camlp5 extension
CAMLP5 = -syntax camlp5o -package ocanren.syntax,GT.syntax.all 
PXFLAGS = $(CAMLP5)
BFLAGS = -rectypes -g
OFLAGS = $(BFLAGS)

all:.depend $(TOPFILE)

.depend: $(SOURCES)
	$(OCAMLDEP) $(PXFLAGS) *.ml > .depend

$(TOPFILE): $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(TOPFILE) $(OFLAGS) $(LIBS:.cma=.cmxa) -linkpkg -package GT,ocanren $(SOURCES:.ml=.cmx) 

$(TOPFILE).byte: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $(TOPFILE).byte $(BFLAGS) $(LIBS) -linkpkg -package GT,ocanren $(SOURCES:.ml=.cmo) 

clean:
	rm -Rf *.cmi *.cmo *.cmx *.annot *.o *.opt *.byte *~ .depend

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

