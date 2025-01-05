#!/bin/bash

# SPDX-FileCopyrightText: 2020 Intel Corporation
#
# SPDX-License-Identifier: MIT

LANGUAGE=$1
SAMPLES_TAG=$2

git clone --depth 1 --branch "$SAMPLES_TAG" https://github.com/oneapi-src/oneAPI-samples.git

#shellcheck disable=SC2010
LATEST_VERSION=$(ls -1 /opt/intel/oneapi/compiler/ | grep -v latest | sort | tail -1)
# shellcheck source=/dev/null
source /opt/intel/oneapi/compiler/"$LATEST_VERSION"/env/vars.sh
sycl-ls

case $LANGUAGE in
c++)
  cd oneAPI-samples/DirectProgramming/C++/CompilerInfrastructure/Intrinsics
  make && make run && make clean && make CC='icx -msse3' && make run
  ;;
fortran)
  cd oneAPI-samples/DirectProgramming/Fortran/CombinationalLogic/openmp-primes
  make && make run && make clean && make FC=ifx && make run
  ;;
dpc++)
  cd oneAPI-samples/DirectProgramming/C++SYCL/DenseLinearAlgebra/simple-add
  mkdir build && cd build && cmake .. && make cpu-gpu
  # Sample has additional HW prerequisites. Please check sample Readme for details. Uncomment the following if the prerequisites are met.
  # mkdir build && cd build && cmake .. && make cpu-gpu &&  ./simple-add-buffers

esac
