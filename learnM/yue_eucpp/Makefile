export LIBDIR = lib
export LIB = eucpp.cmo
export BYTE_EXT = byte
export ROOTDIR = $(CURDIR)

TSTDIR = test

all : lib test

.PHONY : lib test

lib :
	$(MAKE) -C $(LIBDIR)

test :
	$(MAKE) -C $(TSTDIR)

clean :
	$(MAKE) -C $(TSTDIR) clean
	$(MAKE) -C $(LIBDIR) clean


