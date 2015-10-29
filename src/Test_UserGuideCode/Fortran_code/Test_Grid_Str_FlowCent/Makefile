include ../make.defs

WRITE1 = $(OUTDIR)/write_grid_str$(EXE)
WRITE2 = $(OUTDIR)/write_flowcent_str$(EXE)
WRITE3 = $(OUTDIR)/write_bc_str$(EXE)
READ1  = $(OUTDIR)/read_grid_str$(EXE)
READ2  = $(OUTDIR)/read_flowcent_str$(EXE)
READ3  = $(OUTDIR)/read_bc_str$(EXE)

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

$(WRITE1) : ../write_grid_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_grid_str.f $(LIBS)

$(WRITE2) : ../write_flowcent_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_flowcent_str.f $(LIBS)

$(WRITE3) : ../write_bc_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_bc_str.f $(LIBS)

$(READ1) : ../read_grid_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_grid_str.f $(LIBS)

$(READ2) : ../read_flowcent_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_flowcent_str.f $(LIBS)

$(READ3) : ../read_bc_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_bc_str.f $(LIBS)

clean :
	-$(RM) grid.cgns
	-$(RMDIR) $(OUTDIR)

