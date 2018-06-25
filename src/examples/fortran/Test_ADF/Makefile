include ../make.defs

ADFTEST = adftest$(EXE)

$(ADFTEST) : $(OUTDIR) adftest.F
	$(F77) $(FOPTS) $(FEOUT) $@ adftest.F $(LIBS)
	-$(STRIP) $@

test : $(ADFTEST)
	-@$(RM) adf_file_*.adf
	$(ADFTEST) > $(OUTDIR)/output
	-$(DIFF) OUTPUT $(OUTDIR)/output

$(OUTDIR) :
	-@$(MKDIR) $(OUTDIR)

clean :
	-$(RM) $(OUTDIR)/* $(ADFTEST)
	-$(RMDIR) $(OUTDIR)
	-$(RM) adf_file_*.adf

