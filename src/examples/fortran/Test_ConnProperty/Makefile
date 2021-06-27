include ../make.defs

WRITE  = cgwrite$(EXE)
READ   = cgread$(EXE)
MODIFY = gmodify$(EXE)
DELETE = cgdelete$(EXE)

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

$(WRITE) : cgwrite.F90
	$(F77) $(FOPTS) $(FEOUT) $@ cgwrite.F90 $(LIBS)

$(READ) : cgread.F90
	$(F77) $(FOPTS) $(FEOUT) $@ cgread.F90 $(LIBS)

$(MODIFY) : cgmodify.F90
	$(F77) $(FOPTS) $(FEOUT) $@ cgmodify.F90 $(LIBS)

$(DELETE) : cgdelete.F90
	$(F77) $(FOPTS) $(FEOUT) $@ cgdelete.F90 $(LIBS)

clean :
	-$(RM) Test_V2 $(WRITE) $(READ) $(MODIFY) $(DELETE)
	-$(RMDIR) $(OUTDIR)

