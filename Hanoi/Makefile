# variables
src = Hanoi.ml
srco = Hanoio.ml
byte = Hanoi.out
native = Hanoi.native
cln = *.native *.out *.cmo *.cmi *.cmx *.o

# a bytecode executable is made by defauly
.DEFAULT_GOAL : $(byte)

# phony targets for make
.PHONY : clean bytecode nativecode 

bytecode : $(byte)
nativecode : $(native)
clean :
	rm $(cln)

# rules for making bytecode / native-code executables
$(byte) : $(src)
	ocamlc -o $(byte) $(src)
$(native) : $(src)
	ocamlopt -o $(native) $(src)

