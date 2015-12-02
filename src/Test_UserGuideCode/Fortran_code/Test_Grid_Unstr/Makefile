include ../make.defs

WRITE1  = $(OUTDIR)/write_grid_unst$(EXE)
WRITE2  = $(OUTDIR)/write_flowvert_unst$(EXE)
WRITE3  = $(OUTDIR)/write_dimensional$(EXE)
WRITE4  = $(OUTDIR)/write_descriptor$(EXE)
WRITE5  = $(OUTDIR)/write_convergence$(EXE)
WRITE6  = $(OUTDIR)/write_bcpnts_unst$(EXE)
READ1   = $(OUTDIR)/read_grid_unst$(EXE)
READ2   = $(OUTDIR)/read_flowvert_unst$(EXE)
READ3   = $(OUTDIR)/read_dimensional$(EXE)
READ4   = $(OUTDIR)/read_descriptor$(EXE)
READ5   = $(OUTDIR)/read_convergence$(EXE)
READ6   = $(OUTDIR)/read_bcpnts_unst$(EXE)

all    : write read
write  : $(OUTDIR) $(WRITE1) $(WRITE2) $(WRITE3) $(WRITE4) $(WRITE5) $(WRITE6)
read   : $(OUTDIR) $(READ1) $(READ2) $(READ3) $(READ4) $(READ5) $(READ6)

test  : write read
	-@$(RM) grid.cgns
	$(WRITE1)
	$(READ1) > $(OUTDIR)/output1
	-$(DIFF) OUTPUT1 $(OUTDIR)/output1
	$(WRITE2)
	$(READ2) > $(OUTDIR)/output2
	-$(DIFF) OUTPUT2 $(OUTDIR)/output2
	$(WRITE3)
	$(READ3) > $(OUTDIR)/output3
	-$(DIFF) OUTPUT3 $(OUTDIR)/output3
	$(WRITE4)
	$(READ4) > $(OUTDIR)/output4
	-$(DIFF) OUTPUT4 $(OUTDIR)/output4
	$(WRITE5)
	$(READ5) > $(OUTDIR)/output5
	-$(DIFF) OUTPUT5 $(OUTDIR)/output5
	$(WRITE6)
	$(READ6) > $(OUTDIR)/output6
	-$(DIFF) OUTPUT6 $(OUTDIR)/output6

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

$(WRITE1) : ../write_grid_unst.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_grid_unst.F90 $(LIBS)

$(WRITE2) : ../write_flowvert_unst.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_flowvert_unst.F90 $(LIBS)

$(WRITE3) : ../write_dimensional.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_dimensional.F90 $(LIBS)

$(WRITE4) : ../write_descriptor.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_descriptor.F90 $(LIBS)

$(WRITE5) : ../write_convergence.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_convergence.F90 $(LIBS)

$(WRITE6) : ../write_bcpnts_unst.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_bcpnts_unst.F90 $(LIBS)

$(READ1) : ../read_grid_unst.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_grid_unst.F90 $(LIBS)

$(READ2) : ../read_flowvert_unst.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_flowvert_unst.F90 $(LIBS)

$(READ3) : ../read_dimensional.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_dimensional.F90 $(LIBS)

$(READ4) : ../read_descriptor.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_descriptor.F90 $(LIBS)

$(READ5) : ../read_convergence.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_convergence.F90 $(LIBS)

$(READ6) : ../read_bcpnts_unst.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_bcpnts_unst.F90 $(LIBS)

clean :
	-$(RM) grid.cgns
	-$(RMDIR) $(OUTDIR)

