#!/bin/sh
# 1.8 branch
git clone https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git --branch hdf5_1_8 --single-branch hdf5_1_8
cd hdf5_1_8
./configure --disable-fortran --disable-hl --prefix=$HOME/hdf5 && make > result.txt 2>&1 && make install
