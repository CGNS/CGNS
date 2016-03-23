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

## declare an array of tests
declare -a w_arr=("write_grid_str" "write_flowvert_str" "write_nondimensional" "write_descriptor" \
	"write_convergence" "write_floweqn_str" "write_bcpnts_str")
declare -a r_arr=("read_grid_str" "read_flowvert_str" "read_nondimensional" "read_descriptor" \
	"read_convergence" "read_floweqn_str" "read_bcpnts_str")

dir=Test_Grid_Str
printf "%-40s" "Testing $dir..."
cd $dir

# loop through tests
size_arr=${#w_arr[@]} #Number of elements in the array
for i in $(seq 1 $size_arr);do
   ./${w_arr[$i-1]}
   ./${r_arr[$i-1]} > build/output$i
   diff <( sed '/Library/ d' build/output$i) <( sed '/Library/ d' ./OUTPUT$i) > build/results$i.txt
   status=$?
   echoresults $status
   return_val=`expr $status + $return_val`
done

cd ..

###############################

## declare an array of tests
declare -a w_arr=("write_grid_unst" "write_flowvert_unst" "write_dimensional" "write_descriptor" "write_convergence" "write_bcpnts_unst")
declare -a r_arr=("read_grid_unst" "read_flowvert_unst" "read_dimensional" "read_descriptor" "read_convergence" "read_bcpnts_unst")

dir=Test_Grid_Unstr
printf "%-40s" "Testing $dir..."
cd $dir

# loop through tests
size_arr=${#w_arr[@]} #Number of elements in the array
for i in $(seq 1 $size_arr);do
   ./${w_arr[$i-1]}
   ./${r_arr[$i-1]} > build/output$i
   diff <( sed '/Library/ d' build/output$i) <( sed '/Library/ d' ./OUTPUT$i) > build/results$i.txt
   status=$?
   echoresults $status
   return_val=`expr $status + $return_val`
done

cd ..

###############################

## declare an array of tests
declare -a w_arr=("write_grid_str" "write_timevert_str")
declare -a r_arr=("read_grid_str" "read_timevert_str")

dir=Test_Grid_Str_Timeacc
printf "%-40s" "Testing $dir..."
cd $dir

# loop through tests
size_arr=${#w_arr[@]} #Number of elements in the array
for i in $(seq 1 $size_arr);do
   ./${w_arr[$i-1]}
   ./${r_arr[$i-1]} > build/output$i
   diff <( sed '/Library/ d' build/output$i) <( sed '/Library/ d' ./OUTPUT$i) > build/results$i.txt
   status=$?
   echoresults $status
   return_val=`expr $status + $return_val`
done

cd ..

###############################

## declare an array of tests
declare -a w_arr=("write_grid_str" "write_flowcent_str" "write_bc_str")
declare -a r_arr=("read_grid_str" "read_flowcent_str" "read_bc_str")

dir=Test_Grid_Str_FlowCent
printf "%-40s" "Testing $dir..."
cd $dir

# loop through tests
size_arr=${#w_arr[@]} #Number of elements in the array
for i in $(seq 1 $size_arr);do
   ./${w_arr[$i-1]}
   ./${r_arr[$i-1]} > build/output$i
   diff <( sed '/Library/ d' build/output$i) <( sed '/Library/ d' ./OUTPUT$i) > build/results$i.txt
   status=$?
   echoresults $status
   return_val=`expr $status + $return_val`
done

cd ..

###############################

## declare an array of tests
declare -a w_arr=("write_grid_str" "write_flowcentrind_str")
declare -a r_arr=("read_grid_str" "read_flowcentrind_str")

dir=Test_Grid_Str_FlowCentRind
printf "%-40s" "Testing $dir..."
cd $dir

# loop through tests
size_arr=${#w_arr[@]} #Number of elements in the array
for i in $(seq 1 $size_arr);do
   ./${w_arr[$i-1]}
   ./${r_arr[$i-1]} > build/output$i
   diff <( sed '/Library/ d' build/output$i) <( sed '/Library/ d' ./OUTPUT$i) > build/results$i.txt
   status=$?
   echoresults $status
   return_val=`expr $status + $return_val`
done

cd ..

###############################

## declare an array of tests
declare -a w_arr=("write_grid2zn_str" "write_con2zn_str")
declare -a r_arr=("read_grid2zn_str" "read_con2zn_str")

dir=Test_Grid_Str_2zn
printf "%-40s" "Testing $dir..."
cd $dir

# loop through tests
size_arr=${#w_arr[@]} #Number of elements in the array
for i in $(seq 1 $size_arr);do
   ./${w_arr[$i-1]}
   ./${r_arr[$i-1]} > build/output$i
   diff <( sed '/Library/ d' build/output$i) <( sed '/Library/ d' ./OUTPUT$i) > build/results$i.txt
   status=$?
   echoresults $status
   return_val=`expr $status + $return_val`
done

cd ..

###############################

## declare an array of tests
declare -a w_arr=("write_grid2zn_str" "write_con2zn_genrl_str")
declare -a r_arr=("read_grid2zn_str" "read_con2zn_genrl_str")

dir=Test_Grid_Str_2zngenrl
printf "%-40s" "Testing $dir..."
cd $dir

# loop through tests
size_arr=${#w_arr[@]} #Number of elements in the array
for i in $(seq 1 $size_arr);do
   ./${w_arr[$i-1]}
   ./${r_arr[$i-1]} > build/output$i
   diff <( sed '/Library/ d' build/output$i) <( sed '/Library/ d' ./OUTPUT$i) > build/results$i.txt
   status=$?
   echoresults $status
   return_val=`expr $status + $return_val`
done

cd ..

###############################

echo "=== finished ==="
echo "$return_val tests failed"
exit $return_val
