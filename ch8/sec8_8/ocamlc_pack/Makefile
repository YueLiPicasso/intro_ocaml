CLEAN = $(wildcard *.cmo *.cmi)

Mylib.cmo : A.cmo B.cmo
	ocamlc -pack A.cmo B.cmo -o Mylib.cmo

A.cmo : A.ml
	ocamlc -for-pack Mylib -c A.ml

B.cmo : B.ml
	ocamlc -for-pack Mylib -c B.ml

clean : 
	rm -f $(CLEAN)
