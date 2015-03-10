#---------- linux/unix

# location for the CGNS includes and required libraries
# note: if using HDF5 it is looking for libhdf5.[a][so]
# you may need to point to its location
CGNSdir     = /home/bruce/cgns-install
HDF5dir     = /home/bruce/hdf5-install
CGNSinclude = $(CGNSdir)/include
CGNSlibs    = $(CGNSdir)/lib/libcgns.a -L$(HDF5dir) -lhdf5 -lm
# you could also use your build directory as in:
#CGNSdir     = /home/bruce/cgnslib_3.1/src
#CGNSinclude = $(CGNSdir)
#CGNSlibs    = $(CGNSdir)/LINUX/libcgns.a -lhdf5 -lm

# Fortran compiler and options
COMPILE = gfortran -Wall -I$(CGNSinclude)

# if you are using CGNS version 3.1 and have built as 64-bit,
# then you wiil need to set this option to whatever your Fortran compiler
# uses to automatically promote integer values to integer*8
I8OPT = -fdefault-integer-8

# how to name output executable and extension
OUT = -o
EXE =
RM  = /bin/rm -f

#---------- Windows
# NOTE: you may have to add include 'cgnswin_f.h' to all the Fortran code
#       in order to prevent the compiler complaining about the
#       variable argument routines (i.e cg_goto_f)

#CGNSdir = c:\cgns-install
#HDF5dir = c:\hdf5-1.8.5

#CGNSinclude = $(CGNSdir)\include
#CGNSlibs    = $(CGNSdir)\lib\cgns.lib $(HDF5dir)\dll\hdf5dll.lib

#COMPILE = ifort -I$(CGNSinclude)

#OUT = /exe:
#EXE = .exe
#RM  = del /q

#----------

ALL = \
	read_bcpnts_str$(EXE) \
	read_bcpnts_unst$(EXE) \
	read_bc_str$(EXE) \
	read_con2zn_genrl_str$(EXE) \
	read_con2zn_str$(EXE) \
	read_convergence$(EXE) \
	read_descriptor$(EXE) \
	read_dimensional$(EXE) \
	read_flowcentrind_str$(EXE) \
	read_flowcent_str$(EXE) \
	read_floweqn_str$(EXE) \
	read_flowvert_str$(EXE) \
	read_flowvert_unst$(EXE) \
	read_grid2zn_str$(EXE) \
	read_grid_str$(EXE) \
	read_grid_unst$(EXE) \
	read_nondimensional$(EXE) \
	read_timevert_str$(EXE) \
	write_bcpnts_str$(EXE) \
	write_bcpnts_unst$(EXE) \
	write_bc_str$(EXE) \
	write_con2zn_genrl_str$(EXE) \
	write_con2zn_str$(EXE) \
	write_convergence$(EXE) \
	write_descriptor$(EXE) \
	write_dimensional$(EXE) \
	write_flowcentrind_str$(EXE) \
	write_flowcent_str$(EXE) \
	write_floweqn_str$(EXE) \
	write_flowvert_str$(EXE) \
	write_flowvert_unst$(EXE) \
	write_grid2zn_str$(EXE) \
	write_grid_str$(EXE) \
	write_grid_unst$(EXE) \
	write_nondimensional$(EXE) \
	write_timevert_str$(EXE)

TESTS = \
	bcpnts_str \
	bcpnts_unst \
	bc_str \
	con2zn_genrl_str \
	con2zn_str \
	convergence \
	descriptor \
	dimensional \
	flowcentrind_str \
	flowcent_str \
	floweqn_str \
	flowvert_str \
	flowvert_unst \
	grid2zn_str \
	grid_str \
	grid_unst \
	nondimensional \
	timevert_str

#----------

all : $(ALL)
test : $(TESTS)
clean :
	-$(RM) $(ALL) grid.cgns

#----------

read_bcpnts_str$(EXE) : read_bcpnts_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_bcpnts_unst$(EXE) : read_bcpnts_unst.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_bc_str$(EXE) : read_bc_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_con2zn_genrl_str$(EXE) : read_con2zn_genrl_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_con2zn_str$(EXE) : read_con2zn_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_convergence$(EXE) : read_convergence.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_descriptor$(EXE) : read_descriptor.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_dimensional$(EXE) : read_dimensional.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_flowcentrind_str$(EXE) : read_flowcentrind_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_flowcent_str$(EXE) : read_flowcent_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_floweqn_str$(EXE) : read_floweqn_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_flowvert_str$(EXE) : read_flowvert_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_flowvert_unst$(EXE) : read_flowvert_unst.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_grid2zn_str$(EXE) : read_grid2zn_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_grid_str$(EXE) : read_grid_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_grid_unst$(EXE) : read_grid_unst.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_nondimensional$(EXE) : read_nondimensional.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

read_timevert_str$(EXE) : read_timevert_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_bcpnts_str$(EXE) : write_bcpnts_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_bcpnts_unst$(EXE) : write_bcpnts_unst.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_bc_str$(EXE) : write_bc_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_con2zn_genrl_str$(EXE) : write_con2zn_genrl_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_con2zn_str$(EXE) : write_con2zn_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_convergence$(EXE) : write_convergence.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_descriptor$(EXE) : write_descriptor.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_dimensional$(EXE) : write_dimensional.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_flowcentrind_str$(EXE) : write_flowcentrind_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_flowcent_str$(EXE) : write_flowcent_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_floweqn_str$(EXE) : write_floweqn_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_flowvert_str$(EXE) : write_flowvert_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_flowvert_unst$(EXE) : write_flowvert_unst.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_grid2zn_str$(EXE) : write_grid2zn_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_grid_str$(EXE) : write_grid_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_grid_unst$(EXE) : write_grid_unst.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_nondimensional$(EXE) : write_nondimensional.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

write_timevert_str$(EXE) : write_timevert_str.f
	$(COMPILE) $(I8OPT) $(OUT)$@ $< $(CGNSlibs)

#----------

bcpnts_str : write_grid_str write_bcpnts_str read_bcpnts_str
	@echo "********** Testing bcpnts_str"
	write_grid_str
	write_bcpnts_str
	read_bcpnts_str

bcpnts_unst : write_grid_unst write_bcpnts_unst read_bcpnts_unst
	@echo "********** Testing bcpnts_unst"
	write_grid_unst
	write_bcpnts_unst
	read_bcpnts_unst

bc_str : write_grid_str write_bc_str read_bc_str
	@echo "********** Testing bc_str"
	write_grid_str
	write_bc_str
	read_bc_str

con2zn_genrl_str : write_grid2zn_str write_con2zn_genrl_str read_con2zn_genrl_str
	@echo "********** Testing con2zn_genrl_str"
	write_grid2zn_str
	write_con2zn_genrl_str
	read_con2zn_genrl_str

con2zn_str : write_grid2zn_str write_con2zn_str read_con2zn_str
	@echo "********** Testing con2zn_str"
	write_grid2zn_str
	write_con2zn_str
	read_con2zn_str

convergence : write_grid_str write_convergence read_convergence
	@echo "********** Testing convergence"
	write_grid_str
	write_convergence
	read_convergence

descriptor : write_grid_str write_descriptor read_descriptor
	@echo "********** Testing descriptor"
	write_grid_str
	write_descriptor
	read_descriptor

dimensional : write_grid_str write_flowcent_str write_dimensional read_dimensional
	@echo "********** Testing dimensional"
	write_grid_str
	write_flowcent_str
	write_dimensional
	read_dimensional

flowcentrind_str : write_grid_str write_flowcentrind_str read_flowcentrind_str
	@echo "********** Testing flowcentrind_str"
	write_grid_str
	write_flowcentrind_str
	read_flowcentrind_str

flowcent_str : write_grid_str write_flowcent_str read_flowcent_str
	@echo "********** Testing flowcent_str"
	write_grid_str
	write_flowcent_str
	read_flowcent_str

floweqn_str : write_grid_str write_floweqn_str read_floweqn_str
	@echo "********** Testing floweqn_str"
	write_grid_str
	write_floweqn_str
	read_floweqn_str

flowvert_str : write_grid_str write_flowvert_str read_flowvert_str
	@echo "********** Testing flowvert_str"
	write_grid_str
	write_flowvert_str
	read_flowvert_str

flowvert_unst : write_grid_unst write_flowvert_unst read_flowvert_unst
	@echo "********** Testing flowvert_unst"
	write_grid_unst
	write_flowvert_unst
	read_flowvert_unst

grid2zn_str : write_grid2zn_str read_grid2zn_str
	@echo "********** Testing grid2zn_str"
	write_grid2zn_str
	read_grid2zn_str

grid_str : write_grid_str read_grid_str
	@echo "********** Testing grid_str"
	write_grid_str
	read_grid_str

grid_unst : write_grid_unst read_grid_unst
	@echo "********** Testing grid_unst"
	write_grid_unst
	read_grid_unst

nondimensional : write_grid_str write_nondimensional read_nondimensional
	@echo "********** Testing nondimensional"
	write_grid_str
	write_nondimensional
	read_nondimensional

timevert_str : write_grid_str write_timevert_str read_timevert_str
	@echo "********** Testing timevert_str"
	write_grid_str
	write_timevert_str
	read_timevert_str

