Mylib.cma : Mylib.cmo mods
	ocamlc -a -o Mylib.cma Mylib*.cmo

mods : Mylib__A.ml Mylib__B.ml Mylib__A.mli Mylib__B.mli
	ocamlc -c -no-alias-deps -open Mylib Mylib__*.mli Mylib__*.ml

Mylib.cmo : Mylib.ml
	ocamlc -c -no-alias-deps Mylib.ml

clean :
	rm *.cmo *.cmi *.cma
