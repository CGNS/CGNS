include ../make.defs

SETUP   = write_grid_ho_unst$(EXE)
WRITE1  = write_flowcell_ho_unst$(EXE)
READ1   = read_flowcell_ho_unst$(EXE)

WRITE_PROGS = $(SETUP) $(WRITE1)
READ_PROGS = $(READ1)

all    : write read
write  : $(OUTDIR) $(WRITE_PROGS)
read   : $(OUTDIR) $(READ_PROGS)

test  : write read
	-@$(RM) grid_ho_c.cgns
	$(SETUP)
	$(WRITE1)
	$(READ1) > $(OUTDIR)/output1
	-$(DIFF) OUTPUT1 $(OUTDIR)/output1

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

$(SETUP) : ../write_grid_ho_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_grid_ho_unst.c $(LIBS)

$(WRITE1) : ../write_flowcell_ho_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_flowcell_ho_unst.c $(LIBS)

$(READ1) : ../read_flowcell_ho_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_flowcell_ho_unst.c $(LIBS)

clean :
	-$(RM) grid_ho_c.cgns $(WRITE_PROGS) $(READ_PROGS)
	-$(RMDIR) $(OUTDIR)
