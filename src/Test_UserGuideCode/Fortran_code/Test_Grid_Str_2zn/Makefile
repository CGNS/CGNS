include ../make.defs

WRITE1 = write_grid2zn_str$(EXE)
WRITE2 = write_con2zn_str$(EXE)
READ1  = read_grid2zn_str$(EXE)
READ2  = read_con2zn_str$(EXE)


WRITE_PROGS = $(WRITE1) $(WRITE2)
READ_PROGS = $(READ1) $(READ2)

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

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

$(WRITE1) : ../write_grid2zn_str.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_grid2zn_str.F90 $(LIBS)

$(WRITE2) : ../write_con2zn_str.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../write_con2zn_str.F90 $(LIBS)

$(READ1) : ../read_grid2zn_str.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_grid2zn_str.F90 $(LIBS)

$(READ2) : ../read_con2zn_str.F90
	$(F77) $(FOPTS) $(FEOUT)$@ ../read_con2zn_str.F90 $(LIBS)

clean :
	-$(RM) grid.cgns $(WRITE_PROGS) $(READ_PROGS)
	-$(RMDIR) $(OUTDIR)

