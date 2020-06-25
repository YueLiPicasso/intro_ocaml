# variables
src = Hanoi.ml
byte = Hanoi.out
native = Hanoi.native
cln = $(wildcard *.native *.out *.cmo *.cmi *.cmx *.o)

# a bytecode executable is made by defauly
.DEFAULT_GOAL : $(byte)

# phony targets for make
.PHONY : clean bytecode nativecode 

bytecode : $(byte)
nativecode : $(native)
clean :
	@echo Cleaning the working directory ...
	rm -f $(cln)
	@echo Done

# rules for making bytecode / native-code executables
$(byte) : $(src)
	@echo Creating the bytecode executable ...
	ocamlc -o $(byte) $(src)
	@echo Done
$(native) : $(src)
	@echo Creating the native code executable ...
	ocamlopt -o $(native) $(src)
	@echo Done

