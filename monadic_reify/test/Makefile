ADD_PATH = -I $(LIB_PATH) -I $(TYP_PATH)
OCAMLC_LINK = ocamlc -rectypes -o $(EXE) $(ADD_PATH) $(LIB) $(TYP)

all :
	$(OCAMLC_LINK) $(TEST).ml

.PHONY : clean

clean :
	rm -rf *.cm*  *~  *.$(BYTE_EXT)
