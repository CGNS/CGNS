include ../make.defs

WRITE  = $(OUTDIR)/cgwrite$(EXE)
READ   = $(OUTDIR)/cgread$(EXE)
MODIFY = $(OUTDIR)/cgmodify$(EXE)
DELETE = $(OUTDIR)/cgdelete$(EXE)

all    : write read modify delete
write  : $(OUTDIR) $(WRITE)
read   : $(OUTDIR) $(READ)
modify : $(OUTDIR) $(MODIFY)
delete : $(OUTDIR) $(DELETE)

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

$(DELETE) : cgdelete.F
	$(F77) $(FOPTS) $(FEOUT)$@ cgdelete.F $(LIBS)

clean :
	-$(RM) Test_V2
	-$(RMDIR) $(OUTDIR)

