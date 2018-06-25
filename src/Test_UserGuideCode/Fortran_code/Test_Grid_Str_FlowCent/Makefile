include ../make.defs

WRITE1 = write_grid_str$(EXE)
WRITE2 = write_flowcent_str$(EXE)
WRITE3 = write_bc_str$(EXE)
READ1  = read_grid_str$(EXE)
READ2  = read_flowcent_str$(EXE)
READ3  = read_bc_str$(EXE)

WRITE_PROGS = $(WRITE1) $(WRITE2) $(WRITE3)
READ_PROGS = $(READ1) $(READ2) $(READ3)

all    : write read
write  : $(OUTDIR) $(WRITE_PROGS)
read   : $(OUTDIR) $(READ_PROGS)

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

$(WRITE1) : ../write_grid_str.F90
	$(F77) $(FOPTS) $(FEOUT) $@ ../write_grid_str.F90 $(LIBS)

$(WRITE2) : ../write_flowcent_str.F90
	$(F77) $(FOPTS) $(FEOUT) $@ ../write_flowcent_str.F90 $(LIBS)

$(WRITE3) : ../write_bc_str.F90
	$(F77) $(FOPTS) $(FEOUT) $@ ../write_bc_str.F90 $(LIBS)

$(READ1) : ../read_grid_str.F90
	$(F77) $(FOPTS) $(FEOUT) $@ ../read_grid_str.F90 $(LIBS)

$(READ2) : ../read_flowcent_str.F90
	$(F77) $(FOPTS) $(FEOUT) $@ ../read_flowcent_str.F90 $(LIBS)

$(READ3) : ../read_bc_str.F90
	$(F77) $(FOPTS) $(FEOUT) $@ ../read_bc_str.F90 $(LIBS)

clean :
	-$(RM) grid.cgns $(WRITE_PROGS) $(READ_PROGS)
	-$(RMDIR) $(OUTDIR)

