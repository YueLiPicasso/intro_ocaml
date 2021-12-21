ADD_PATH = -I $(LIB_PATH) -I $(TYP_PATH)

all :
	ocamlc -o $(EXE) $(ADD_PATH) $(LIB) $(TYP) $(TEST).ml

.PHONY : clean

clean :
	rm -rf *.cm*  *~  *.$(BYTE_EXT)
