export LIBDIR = lib
export LIB = eucpp.cmo
export BYTE_EXT = byte
export ROOTDIR = $(CURDIR)
export LIB_PATH = $(ROOTDIR)/$(LIBDIR)
TYPDIR = type
TSTDIR = test
TGTS = lib type test

all : $(TGTS) 

.PHONY : $(TGTS)

lib :
	$(MAKE) -C $(LIBDIR)

type : lib
	$(MAKE) -C $(TYPDIR)

test : lib
	$(MAKE) -C $(TSTDIR)

clean :
	$(MAKE) -C $(TSTDIR) clean
	$(MAKE) -C $(TYPDIR) clean
	$(MAKE) -C $(LIBDIR) clean


