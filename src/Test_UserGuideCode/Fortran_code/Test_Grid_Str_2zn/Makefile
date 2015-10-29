include ../make.defs

WRITE1 = $(OUTDIR)/write_grid2zn_str$(EXE)
WRITE2 = $(OUTDIR)/write_con2zn_str$(EXE)
READ1  = $(OUTDIR)/read_grid2zn_str$(EXE)
READ2  = $(OUTDIR)/read_con2zn_str$(EXE)

all    : write read
write  : $(OUTDIR) $(WRITE1) $(WRITE2)
read   : $(OUTDIR) $(READ1) $(READ2)

test  : write read
	-@$(RM) grid.cgns
	$(WRITE1)
	$(READ1) > $(OUTDIR)/output1
	-$(DIFF) OUTPUT1 $(OUTDIR)/output1
	$(WRITE2)
	$(READ2) > $(OUTDIR)/output2
	-$(DIFF) OUTPUT2 $(OUTDIR)/output2

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

$(WRITE1) : ../write_grid2zn_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_grid2zn_str.f $(LIBS)

$(WRITE2) : ../write_con2zn_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_con2zn_str.f $(LIBS)

$(READ1) : ../read_grid2zn_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_grid2zn_str.f $(LIBS)

$(READ2) : ../read_con2zn_str.f
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_con2zn_str.f $(LIBS)

clean :
	-$(RM) grid.cgns
	-$(RMDIR) $(OUTDIR)

