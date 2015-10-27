#! /bin/sh
NO_COLOR="\033[0m"
OK_COLOR="\033[32;01m"
ERROR_COLOR="\033[31;01m"

echoresults() {
    if test $* -ne 0
    then
        echo -e "$ERROR_COLOR *** FAILED *** $NO_COLOR"
        cat results.txt
    else
        echo -e "$OK_COLOR passed $NO_COLOR"
    fi
}

DIRS="Test1 \
	Test_1to1ConnProp \
	Test_Axisym \
	Test_BCProperty \
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
	Test_UserDefinedData"

#	-@for d in $(DIRS) ; do \
#	  echo "********** testing $$d"; \
#	  cd $$d && make; \
#	  cd ..; \
#	done; \

for dir in $DIRS
do
    cd $dir
    make
    cd ..
done
return_val=0
echo ""
echo "=== running tests ==="; \
for dir in $DIRS
do
    printf "%-40s" "Testing $dir..."
    cd $dir/build
    cgwrite
    cgread > output
    diff -I 'Library Version used for file creation*' -I 'DonorDatatype' -I 'datatype=' output ../OUTPUT &> results.txt
    status=$?
    echoresults $status
    return_val=`expr $status + $return_val`
    cd ../..
done

###############################
# Special cases
###############################

dir=Test_cgio
printf "%-40s" "Testing $dir..."
cd $dir/build
cgiotest > output
diff -I 'Library Version used for file creation*' output ../OUTPUT &> results.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`
cd ../..

###############################

echo "=== finished ==="
echo "$return_val tests failed"
exit $return_val
