# topological order of modules
MODTOPO = anchor.mli var.mli term.mli core.mli            
MODIMP  = $(MODTOPO:.mli=.ml)
TEST=test.byte
LIB=moc.cma

dtest :
	dune build test

# make lib 
# make

all : 
	ocamlc -o $(TEST) $(LIB) $(TEST:.byte=.ml) 

# compile interface

interf 	: 
	ocamlc -c $(MODTOPO)

# compile implementation

lib : interf
	ocamlc -a -o $(LIB) $(MODIMP) 

# pattern rules for compiling ocaml sources 

%.cmi : %.mli
	ocamlc -c $<

%.cmi : %.ml
	ocamlc -c $<


%.cmo : %.ml %.cmi
	ocamlc -c $<

clean :
	rm -rf *.cm*  *~  *.byte
