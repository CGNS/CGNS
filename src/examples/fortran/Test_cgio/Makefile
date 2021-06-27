include ../make.defs

CGIOTEST = cgiotest$(EXE)

$(CGIOTEST) : $(OUTDIR) cgiotest.F90
	$(F77) $(FOPTS) $(FEOUT) $@ cgiotest.F90 $(LIBS)

test : $(CGIOTEST)
	-@$(RM) adf_file_*.adf
	$(CGIOTEST) > $(OUTDIR)/output
	-$(DIFF) OUTPUT $(OUTDIR)/output

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

clean :
	-$(RM) $(OUTDIR)/* $(CGIOTEST)
	-$(RMDIR) $(OUTDIR)
	-$(RM) adf_file_*.adf

