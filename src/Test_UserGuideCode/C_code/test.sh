#! /bin/bash
NO_COLOR="\033[0m"
OK_COLOR="\033[32;01m"
ERROR_COLOR="\033[31;01m"

echoresults() {
    if test $* -ne 0
    then
        printf "$ERROR_COLOR *** FAILED *** $NO_COLOR \n"
    else
        printf "$OK_COLOR passed $NO_COLOR \n"
    fi
}

DIRS=" \
	Test_Grid_Str"

return_val=0
###############################
# Special cases
###############################

dir=Test_Grid_Str
printf "%-40s" "Testing $dir..."
cd $dir/build

./write_grid_str
./read_grid_str > output1
diff <( sed '/Library/ d' output1) <( sed '/Library/ d' ../OUTPUT1) > results1.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_flowvert_str
./read_flowvert_str > output2
diff <( sed '/Library/ d' output2) <( sed '/Library/ d' ../OUTPUT2) > results2.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_nondimensional
./read_nondimensional > output3
diff <( sed '/Library/ d' output3) <( sed '/Library/ d' ../OUTPUT3) > results3.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_descriptor
./read_descriptor > output4
diff <( sed '/Library/ d' output4) <( sed '/Library/ d' ../OUTPUT4) > results4.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_convergence
./read_convergence > output5
diff <( sed '/Library/ d' output5) <( sed '/Library/ d' ../OUTPUT5) > results5.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_floweqn_str
./read_floweqn_str > output6
diff <( sed '/Library/ d' output6) <( sed '/Library/ d' ../OUTPUT6) > results6.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_bcpnts_str
./read_bcpnts_str > output7
diff <( sed '/Library/ d' output7) <( sed '/Library/ d' ../OUTPUT7) > results7.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

cd ../..

###############################

dir=Test_Grid_Unstr
printf "%-40s" "Testing $dir..."
cd $dir/build

./write_grid_unst
./read_grid_unst > output1
diff <( sed '/Library/ d' output1) <( sed '/Library/ d' ../OUTPUT1) > results1.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_flowvert_unst
./read_flowvert_unst > output2
diff <( sed '/Library/ d' output2) <( sed '/Library/ d' ../OUTPUT2) > results2.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_dimensional
./read_dimensional > output3
diff <( sed '/Library/ d' output3) <( sed '/Library/ d' ../OUTPUT3) > results3.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_descriptor
./read_descriptor > output4
diff <( sed '/Library/ d' output4) <( sed '/Library/ d' ../OUTPUT4) > results4.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_convergence
./read_convergence > output5
diff <( sed '/Library/ d' output5) <( sed '/Library/ d' ../OUTPUT5) > results5.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_bcpnts_unst
./read_bcpnts_unst > output6
diff <( sed '/Library/ d' output6) <( sed '/Library/ d' ../OUTPUT6) > results6.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

cd ../..

###############################

dir=Test_Grid_Str_Timeacc
printf "%-40s" "Testing $dir..."
cd $dir/build

./write_grid_str
./read_grid_str > output1
diff <( sed '/Library/ d' output1) <( sed '/Library/ d' ../OUTPUT1) > results1.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_timevert_str
./read_timevert_str > output2
diff <( sed '/Library/ d' output2) <( sed '/Library/ d' ../OUTPUT2) > results2.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

cd ../..

###############################

dir=Test_Grid_Str_FlowCent
printf "%-40s" "Testing $dir..."
cd $dir/build

./write_grid_str
./read_grid_str > output1
diff <( sed '/Library/ d' output1) <( sed '/Library/ d' ../OUTPUT1) > results1.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_flowcent_str
./read_flowcent_str > output2
diff <( sed '/Library/ d' output2) <( sed '/Library/ d' ../OUTPUT2) > results2.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_bc_str
./read_bc_str > output3
diff <( sed '/Library/ d' output3) <( sed '/Library/ d' ../OUTPUT3) > results3.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

cd ../..

###############################

dir=Test_Grid_Str_FlowCentRind
printf "%-40s" "Testing $dir..."
cd $dir/build

./write_grid_str
./read_grid_str > output1
diff <( sed '/Library/ d' output1) <( sed '/Library/ d' ../OUTPUT1) > results1.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_flowcentrind_str
./read_flowcentrind_str > output2
diff <( sed '/Library/ d' output2) <( sed '/Library/ d' ../OUTPUT2) > results2.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

cd ../..

###############################

dir=Test_Grid_Str_2zn
printf "%-40s" "Testing $dir..."
cd $dir/build

./write_grid2zn_str
./read_grid2zn_str > output1
diff <( sed '/Library/ d' output1) <( sed '/Library/ d' ../OUTPUT1) > results1.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_con2zn_str
./read_con2zn_str > output2
diff <( sed '/Library/ d' output2) <( sed '/Library/ d' ../OUTPUT2) > results2.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

cd ../..

###############################

dir=Test_Grid_Str_2zngenrl
printf "%-40s" "Testing $dir..."
cd $dir/build

./write_grid2zn_str
./read_grid2zn_str > output1
diff <( sed '/Library/ d' output1) <( sed '/Library/ d' ../OUTPUT1) > results1.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

./write_con2zn_genrl_str
./read_con2zn_genrl_str > output2
diff <( sed '/Library/ d' output2) <( sed '/Library/ d' ../OUTPUT2) > results2.txt
status=$?
echoresults $status
return_val=`expr $status + $return_val`

cd ../..

###############################

echo "=== finished ==="
echo "$return_val tests failed"
exit $return_val
