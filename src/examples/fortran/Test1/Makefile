include ../make.defs

WRITE  = cgwrite$(EXE)
READ   = cgread$(EXE)
MODIFY = cgmodify$(EXE)

all    : write read modify
write  : $(OUTDIR) $(WRITE)
read   : $(OUTDIR) $(READ)
modify : $(OUTDIR) $(MODIFY)

test  : write read
	-@$(RM) Test_V2
	$(WRITE)
	$(READ) > $(OUTDIR)/output
	-$(DIFF) OUTPUT $(OUTDIR)/output

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

$(WRITE) : cgwrite.F
	$(F77) $(FOPTS) $(FEOUT)$@ cgwrite.F $(LIBS)

$(READ) : cgread.F
	$(F77) $(FOPTS) $(FEOUT)$@ cgread.F $(LIBS)

$(MODIFY) : cgmodify.F
	$(F77) $(FOPTS) $(FEOUT)$@ cgmodify.F $(LIBS)

clean :
	-$(RM) Test_V2 $(WRITE) $(READ) $(MODIFY)
	-$(RMDIR) $(OUTDIR)

