# sources

TOPFILE = plus
CSOURCES = exC.c
MSOURCES = exOCAML.ml

# compilers

OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt

# targets

OUTB = $(TOPFILE).byte
OUTO = $(TOPFILE).opt

# path of OCaml-C interface
# /home/yuelipicasso/.opam/4.07.1+fp+flambda/lib/ocaml

SPATH = /home/yuelipicasso/.opam/4.11.1/lib/ocaml

CLEAN = $(wildcard *.cmi *.cmo *.cmx *.o *.opt *.byte *~)

.PHONY : all clean

all: $(OUTO) $(OUTB)

$(OUTB): $(CSOURCES:.c=.o) $(MSOURCES:.ml=.cmo)
	$(OCAMLC) -custom -o $(OUTB) $(CSOURCES:.c=.o) $(MSOURCES:.ml=.cmo)

$(OUTO): $(CSOURCES:.c=.o) $(MSOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(OUTO) $(CSOURCES:.c=.o) $(MSOURCES:.ml=.cmx)

clean:
	rm -Rf $(CLEAN)

# generic rules

###############
%.cmi: %.mli
	$(OCAMLC) -c $(BFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c  $<

%.o: %.ml
	$(OCAMLOPT) -c $<

%.o: %.c
	$(OCAMLOPT) -cc gcc $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

