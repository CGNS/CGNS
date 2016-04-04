include ../make.defs

WRITE1  = write_grid_unst$(EXE)
WRITE2  = write_flowvert_unst$(EXE)
WRITE3  = write_dimensional$(EXE)
WRITE4  = write_descriptor$(EXE)
WRITE5  = write_convergence$(EXE)
WRITE6  = write_bcpnts_unst$(EXE)
READ1   = read_grid_unst$(EXE)
READ2   = read_flowvert_unst$(EXE)
READ3   = read_dimensional$(EXE)
READ4   = read_descriptor$(EXE)
READ5   = read_convergence$(EXE)
READ6   = read_bcpnts_unst$(EXE)

WRITE_PROGS = $(WRITE1) $(WRITE2) $(WRITE3) $(WRITE4) $(WRITE5) $(WRITE6)
READ_PROGS = $(READ1) $(READ2) $(READ3) $(READ4) $(READ5) $(READ6)

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

$(WRITE1) : ../write_grid_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_grid_unst.c $(LIBS)

$(WRITE2) : ../write_flowvert_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_flowvert_unst.c $(LIBS)

$(WRITE3) : ../write_dimensional.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_dimensional.c $(LIBS)

$(WRITE4) : ../write_descriptor.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_descriptor.c $(LIBS)

$(WRITE5) : ../write_convergence.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_convergence.c $(LIBS)

$(WRITE6) : ../write_bcpnts_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../write_bcpnts_unst.c $(LIBS)

$(READ1) : ../read_grid_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_grid_unst.c $(LIBS)

$(READ2) : ../read_flowvert_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_flowvert_unst.c $(LIBS)

$(READ3) : ../read_dimensional.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_dimensional.c $(LIBS)

$(READ4) : ../read_descriptor.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_descriptor.c $(LIBS)

$(READ5) : ../read_convergence.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_convergence.c $(LIBS)

$(READ6) : ../read_bcpnts_unst.c
	$(CC) $(COPTS) $(FEOUT)$@ ../read_bcpnts_unst.c $(LIBS)

clean :
	-$(RM) grid.cgns $(WRITE_PROGS) $(READ_PROGS)
	-$(RMDIR) $(OUTDIR)

