include ../make.defs

WRITE = cgwrite$(EXE)
READ  = cgread$(EXE)

all   : write read
write : $(OUTDIR) $(WRITE)
read  : $(OUTDIR) $(READ)

test  : write read
	-@$(RM) Test_V2
	$(WRITE)
	$(READ) > $(OUTDIR)/output
	-$(DIFF) OUTPUT $(OUTDIR)/output

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

$(WRITE) : cgwrite.F90
	$(F77) $(FOPTS) $(FEOUT) $@ cgwrite.F90 $(LIBS)

$(READ) : cgread.F90
	$(F77) $(FOPTS) $(FEOUT) $@ cgread.F90 $(LIBS)

clean :
	-$(RM) Test_V2 $(WRITE) $(READ)
	-$(RMDIR) $(OUTDIR)

