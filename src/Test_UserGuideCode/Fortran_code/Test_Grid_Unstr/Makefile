include ../make.defs

WRITE1  = $(OUTDIR)/write_grid_unst$(EXE)
WRITE2  = $(OUTDIR)/write_flowvert_unst$(EXE)
WRITE3  = $(OUTDIR)/write_bcpnts_unst$(EXE)
READ1   = $(OUTDIR)/read_grid_unst$(EXE)
READ2   = $(OUTDIR)/read_flowvert_unst$(EXE)
READ3   = $(OUTDIR)/read_bcpnts_unst$(EXE)

all    : write read
write  : $(OUTDIR) $(WRITE1) $(WRITE2) $(WRITE3)
read   : $(OUTDIR) $(READ1) $(READ2) $(READ3)

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

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

$(WRITE1) : ../write_grid_unst.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_grid_unst.f $(LIBS)

$(WRITE2) : ../write_flowvert_unst.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_flowvert_unst.f $(LIBS)

$(WRITE3) : ../write_bcpnts_unst.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_bcpnts_unst.f $(LIBS)

$(READ1) : ../read_grid_unst.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_grid_unst.f $(LIBS)

$(READ2) : ../read_flowvert_unst.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_flowvert_unst.f $(LIBS)

$(READ3) : ../read_bcpnts_unst.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_bcpnts_unst.f $(LIBS)

clean :
	-$(RM) grid.cgns
	-$(RMDIR) $(OUTDIR)

