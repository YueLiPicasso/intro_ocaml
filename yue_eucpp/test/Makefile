TEST = test

all : lib
	ocamlc -o $(TEST).$(BYTE_EXT) -I $(LIB_PATH) $(LIB) $(TEST).ml

.PHONY : lib clean

lib :
	$(MAKE) -C $(LIB_PATH)

clean :
	rm -rf *.cm*  *~  *.$(BYTE_EXT)
