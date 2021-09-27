TOPFILE = test
INTERFA = Core.mli
SOURCES = Core.ml test.ml
OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
OUTB = $(TOPFILE).byte
OUTO = $(TOPFILE).opt
# add flag -i to BFLAGS to show types only
BFLAGS = -rectypes 
OFLAGS = $(BFLAGS)
CLEAN = $(wildcard *.cmi *.cmo *.cmx *.annot *.o *.opt *.byte *~ *depend)

.PHONY : all clean

all: $(OUTO)

$(OUTO): $(INTERFA:.mli=.cmi) $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(OUTO) $(OFLAGS) $(SOURCES:.ml=.cmx) 

$(OUTB): $(INTERFA:.mli=.cmi) $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $(OUTB) $(BFLAGS) $(SOURCES:.ml=.cmo) 

clean:
	rm -Rf $(CLEAN)

# generic rules

###############
%.cmi: %.mli
	$(OCAMLC) -c $(BFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml
	$(OCAMLC) -c $(BFLAGS)  $<

%.cmo: %.ml
	$(OCAMLC) -c $(BFLAGS)  $<

%.o: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OFLAGS)  $<

