# v3.0.5
# v3.1.2
# v3.1.4
# v3.2.1
# v3.3.1
# v3.4.2
# v4.0.0
# v4.1.2
# v4.3.0

CC = gcc

CGNSDIR = ../..

#Compile CGNS with --enable-scope for CGNS < v3.3.1
#Use HDF5 1.8 for versions < 3.4.0

#For CGNS_VERSION >= v3.1.0
include $(CGNSDIR)/make.defs
#For CGNS_VERSION > v3.1.3
include $(CGNSDIR)/cgnsBuild.defs

#For CGNS_VERSION < v3.1.0
#include $(CGNSDIR)/make.system
#include $(CGNSDIR)/make.$(SYSTEM)

CGNSLIB = $(CGNSDIR)/$(LIBCGNS)

COPTS  = $(CFLAGS) -I$(CGNSDIR) -I..
LDLIBS = $(CGNSLIB) $(BUILDLIBS)

all : gen_file$(EXE)

gen_file$(EXE) : gen_file.c $(CGNSLIB)
	$(CC) $(COPTS) $(CEOUT)$@ gen_file.c $(LDLIBS) $(CLIBS)

clean :
	-$(RM) gen_file$(EXE)
