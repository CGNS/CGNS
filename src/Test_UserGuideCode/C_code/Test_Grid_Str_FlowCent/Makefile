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

$(WRITE1) : ../write_grid_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_grid_str.c $(LIBS)

$(WRITE2) : ../write_flowcent_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_flowcent_str.c $(LIBS)

$(WRITE3) : ../write_bc_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_bc_str.c $(LIBS)

$(READ1) : ../read_grid_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_grid_str.c $(LIBS)

$(READ2) : ../read_flowcent_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_flowcent_str.c $(LIBS)

$(READ3) : ../read_bc_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_bc_str.c $(LIBS)

clean :
	-$(RM) grid.cgns $(WRITE_PROGS) $(READ_PROGS)
	-$(RMDIR) $(OUTDIR)

