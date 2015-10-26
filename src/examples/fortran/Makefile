
DIRS = \
	Test1 \
	Test_1to1ConnProp \
	Test_Axisym \
	Test_BCProperty \
	Test_cgio \
	Test_chemistry \
	Test_ConnProperty \
	Test_EM \
	Test_Family \
	Test_Gravity \
	Test_Links \
	Test_mixed_elements \
	Test_mixed_grid \
	Test_motion \
	Test_PartRdWr \
	Test_RotatingCoordinates \
	Test_UD_BCData \
	Test_UserDefinedData

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

