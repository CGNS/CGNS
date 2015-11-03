include ../make.defs

WRITE1 = $(OUTDIR)/write_grid_str$(EXE)
WRITE2 = $(OUTDIR)/write_flowcentrind_str$(EXE)
READ1  = $(OUTDIR)/read_grid_str$(EXE)
READ2  = $(OUTDIR)/read_flowcentrind_str$(EXE)

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

$(WRITE1) : ../write_grid_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_grid_str.c $(LIBS)

$(WRITE2) : ../write_flowcentrind_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_flowcentrind_str.c $(LIBS)

$(READ1) : ../read_grid_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_grid_str.c $(LIBS)

$(READ2) : ../read_flowcentrind_str.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_flowcentrind_str.c $(LIBS)

clean :
	-$(RM) grid.cgns
	-$(RMDIR) $(OUTDIR)

