

Mylib.cmo : A.cmo B.cmo
	ocamlc -pack A.cmo B.cmo -o Mylib.cmo

A.cmo : A.ml
	ocamlc -c A.ml

B.cmo : B.ml
	ocamlc -c B.ml

clean : 
	rm *.cmo *.cmi
