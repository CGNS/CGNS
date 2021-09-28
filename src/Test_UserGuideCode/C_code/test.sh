#! /bin/bash
NO_COLOR="\033[0m"
OK_COLOR="\033[32;01m"
ERROR_COLOR="\033[31;01m"

echoresults() {
    padlimit=40
    pad=$(printf '%*s' "$padlimit")
    pad=${pad// /.}
    padlength=40
    printf ' %*.*s' 0 $((padlength - ${#x} )) "$pad"
    if test $status -ne 0
    then
	printf " [$(ERROR_COLOR)FAILED$(NO_COLOR)]\n"
        status=1
    else
	printf " [${OK_COLOR}PASSED${NO_COLOR}]"
        printf "%+12s\n" "$itime"
    fi
}

TIMING_AVAIL=$(/usr/bin/time -a -o CGNS_timing.txt -f "%e" pwd > /dev/null; echo $?)

run_tests() {
    printf "%-40s \n" "Testing $dir..."
    cd "$dir"
    # loop through tests
    size_arr=${#w_arr[@]} #Number of elements in the array
    itime=""
    for i in $(seq 1 $size_arr);do
        x="   Program: ${w_arr[$i-1]}"
        printf "$x"
        if [ "$TIMING_AVAIL" = "0" ]; then
            /usr/bin/time -a -o ../CGNS_timing.txt -f "$dir.${w_arr[$i-1]} %e" "./${w_arr[$i-1]}" >/dev/null 2>&1
            itime=$(tail -n1 ../CGNS_timing.txt |  awk  '{print $2}' | sed -e 's/$/ sec/')
        else
            "./${w_arr[$i-1]}" >/dev/null 2>&1
        fi
        status=$?
        echoresults
        return_val=$((status + return_val))

        x="   Program: ${r_arr[$i-1]}"
        printf "$x"
        if [ "$TIMING_AVAIL" = "0" ]; then
            /usr/bin/time -a -o ../CGNS_timing.txt -f "$dir.${r_arr[$i-1]} %e" "./${r_arr[$i-1]}" > "build/output$i"
            itime=$(tail -n1 ../CGNS_timing.txt |  awk  '{print $2}' | sed -e 's/$/ sec/')
        else
            "./${r_arr[$i-1]}" > build/output$i
        fi
        diff <( sed '/Library/ d' "build/output$i") <( sed '/Library/ d' "./OUTPUT$i") > "build/results$i.txt"
        status=$?
        echoresults
        return_val=$((status + return_val))
    done
    cd ..
}

return_val=0
rm -f CGNS_timing.txt

###############################
# Special cases
###############################

## declare an array of tests
declare -a w_arr=("write_grid_str" "write_flowvert_str" "write_nondimensional" "write_descriptor" \
	"write_convergence" "write_floweqn_str" "write_bcpnts_str")
declare -a r_arr=("read_grid_str" "read_flowvert_str" "read_nondimensional" "read_descriptor" \
	"read_convergence" "read_floweqn_str" "read_bcpnts_str")

dir=Test_Grid_Str

run_tests

###############################

## declare an array of tests
declare -a w_arr=("write_grid_unst" "write_flowvert_unst" "write_dimensional" "write_descriptor" "write_convergence" "write_bcpnts_unst")
declare -a r_arr=("read_grid_unst" "read_flowvert_unst" "read_dimensional" "read_descriptor" "read_convergence" "read_bcpnts_unst")

dir=Test_Grid_Unstr

run_tests

###############################

## declare an array of tests
declare -a w_arr=("write_grid_str" "write_timevert_str")
declare -a r_arr=("read_grid_str" "read_timevert_str")

dir=Test_Grid_Str_Timeacc

run_tests

###############################

## declare an array of tests
declare -a w_arr=("write_grid_str" "write_flowcent_str" "write_bc_str")
declare -a r_arr=("read_grid_str" "read_flowcent_str" "read_bc_str")

dir=Test_Grid_Str_FlowCent

run_tests

###############################

## declare an array of tests
declare -a w_arr=("write_grid_str" "write_flowcentrind_str")
declare -a r_arr=("read_grid_str" "read_flowcentrind_str")

dir=Test_Grid_Str_FlowCentRind

run_tests

###############################

## declare an array of tests
declare -a w_arr=("write_grid2zn_str" "write_con2zn_str")
declare -a r_arr=("read_grid2zn_str" "read_con2zn_str")

dir=Test_Grid_Str_2zn

run_tests

###############################

## declare an array of tests
declare -a w_arr=("write_grid2zn_str" "write_con2zn_genrl_str")
declare -a r_arr=("read_grid2zn_str" "read_con2zn_genrl_str")

dir=Test_Grid_Str_2zngenrl

run_tests

###############################

echo "=== finished ==="
if test $return_val != 0; then
  printf "$ERROR_COLOR"
else
  printf "$OK_COLOR"
fi
printf "$return_val tests failed $NO_COLOR \n"
exit $return_val
