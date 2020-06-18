src = Hanoi.ml
tgt = Hanoi.out
cln = $(tgt) Hanoi.cmo Hanoi.cmi

Hanoi.out: $(src)
	ocamlc -o $(tgt) $(src)

clean :
	rm $(cln)
