
DIRS = \
	Test_Grid_Str \
	Test_Grid_Unstr

#----------

all :
	@status=0 ;for d in $(DIRS) ; do \
	  cd $$d && make; \
	  ((status = status + $$?)); \
	  cd ..; \
	done;\
	if [ $$status -ne 0 ]; then \
		exit $$status; \
	fi

test : $(TESTS)
	@for d in $(DIRS) ; do \
	  cd $$d && make; \
	  ((status = status + $$?)); \
	  cd ..; \
	done;\
	if [ $$status -ne 0 ]; then \
		exit $$status; \
	fi
	test.sh
	exit $$?;

clean : $(CLEAN)
	-@for d in $(DIRS) ; do \
	  cd $$d && make clean; \
	  cd ..; \
	done;

