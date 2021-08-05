#! /bin/bash
NO_COLOR="\033[0m"
OK_COLOR="\033[32;01m"
ERROR_COLOR="\033[31;01m"

echoresults() {
    if test $status -ne 0
    then
        printf "$ERROR_COLOR *** FAILED *** $NO_COLOR \n"
        status = 1
    else
        printf "$OK_COLOR PASSED $NO_COLOR \n"
    fi
}

TIMING_AVAIL=$(/usr/bin/time -a -o CGNS_timing.txt -f "%e" pwd > /dev/null; echo $?)

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

return_val=0
rm -f CGNS_timing.txt

echo ""
echo "=== running tests ==="; \
for dir in $DIRS;do
    printf "%-40s \n" "Testing $dir..."
    cd $dir
    printf "   Program: cgwrite "
    if [ "$TIMING_AVAIL" = "0" ]; then
        /usr/bin/time -a -o ../CGNS_timing.txt -f "$dir.cgwrite %e" ./cgwrite >/dev/null 2>&1
    else
        ./cgwrite >/dev/null 2>&1
    fi
    status=$?
    echoresults
    return_val=`expr $status + $return_val`

    printf "   Program: cgread "
    if [ $TIMING_AVAIL = "0" ]; then
        /usr/bin/time -a -o ../CGNS_timing.txt -f "$dir.cgread %e" ./cgread > build/output
    else
        ./cgread > build/output
    fi
    diff <( sed '/Library/ d' build/output | sed '/DonorDatatype/ d' | sed '/datatype=/ d') <( sed '/Library/ d' ./OUTPUT | sed '/DonorDatatype/ d' | sed '/datatype=/ d') > build/results.txt
#   diff -I 'Library Version used for file creation*' -I 'DonorDatatype' -I 'datatype=' output ./OUTPUT > results.txt
    status=$?
    echoresults $status
    return_val=`expr $status + $return_val`
    cd ..
done

###############################
# Special cases
###############################

dir=Test_cgio
printf "%-40s \n" "Testing $dir..."
cd $dir
printf "   Program: cgiotest "
if [ $TIMING_AVAIL = "0" ]; then
    /usr/bin/time -a -o ../CGNS_timing.txt -f "$dir.cgiotest %e" ./cgiotest > build/output
else
    ./cgiotest > build/output
fi
diff <( sed '/Library/ d' build/output) <( sed '/Library/ d' ./OUTPUT) > build/results.txt
#diff -I 'Library Version used for file creation*' output ./OUTPUT > results.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`
cd ..

###############################

echo "=== finished ==="
if test $return_val != 0; then
  printf "$ERROR_COLOR"
else
  printf "$OK_COLOR"
fi
printf "$return_val tests failed $NO_COLOR \n"
exit $return_val
