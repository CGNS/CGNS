include ../make.defs

CGIOTEST = $(OUTDIR)/cgiotest$(EXE)

$(CGIOTEST) : $(OUTDIR) cgiotest.F
	$(F77) $(FOPTS) $(FEOUT)$@ cgiotest.F $(LIBS)

test : $(CGIOTEST)
	-@$(RM) adf_file_*.adf
	$(CGIOTEST) > $(OUTDIR)/output
	-$(DIFF) OUTPUT $(OUTDIR)/output

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

clean :
	-$(RM) $(OUTDIR)/*
	-$(RMDIR) $(OUTDIR)
	-$(RM) adf_file_*.adf

