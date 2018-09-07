include ../make.defs

CGIOTEST = cgiotest$(EXE)

$(CGIOTEST) : $(OUTDIR) cgiotest.F
	$(F77) $(FOPTS) $(FEOUT) $@ cgiotest.F $(LIBS)

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

