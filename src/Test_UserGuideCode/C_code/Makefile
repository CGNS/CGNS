
DIRS = \
	Test_Grid_Str \
	Test_Grid_Unstr \
	Test_Grid_Str_Timeacc \
	Test_Grid_Str_FlowCent \
	Test_Grid_Str_FlowCentRind \
	Test_Grid_Str_2zn \
	Test_Grid_Str_2zngenrl

#----------

all :
	@status=0;for d in $(DIRS) ; do \
	  cd $$d && $(MAKE); \
	  status=`expr $$status + $$?`; \
	  cd ..; \
	done;\
	if [ $$status -ne 0 ]; then \
		exit $$status; \
	fi

test : $(TESTS)
	@status=0;for d in $(DIRS) ; do \
	  cd $$d && $(MAKE); \
	  status=`expr $$status + $$?`; \
	  cd ..; \
	done;\
	if [ $$status -ne 0 ]; then \
		exit $$status; \
	fi;\
	./test.sh; \
	exit $$?;

clean : $(CLEAN)
	-@for d in $(DIRS) ; do \
	  cd $$d && $(MAKE) clean; \
	  cd ..; \
	done;

