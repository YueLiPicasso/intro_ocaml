# topological order of core library modules
# list of custom types
export MODTOPO = anchor var term core
export TYPE = option list

# root directory, and relative dirs of:
# core library 
# custom types 
# test file  
export ROOTDIR = $(CURDIR)
export LIBDIR = lib
export TYPDIR = type
TSTDIR = test

# absolute dirs of:
# core library 
# custome types 
export LIB_PATH = $(ROOTDIR)/$(LIBDIR)
export TYP_PATH = $(ROOTDIR)/$(TYPDIR)

# names of module packages
export LIB = more.cmo
export TYP = type.cmo

# extension of bytecode
# the basename of the test file     
# the full name of the test executable
export BYTE_EXT = byte
export TEST = test
export EXE = $(TEST).$(BYTE_EXT)

# targets in the right order
TGTS = lib type test

.PHONY : $(TGTS) clean runtest

# make
# make runtest

all : $(TGTS) 


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

runtest :
	./$(TSTDIR)/$(EXE)

