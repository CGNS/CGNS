#!/bin/sh

# Set odd/even days of the week
day=$(( $(date +%d) % 2 ))

if [ $day -eq 0 ]; then #even day tests, HDF5 1.8 branch

HDF5_VER="hdf5_1_8"

else #odd day tests, HDF5 develop branch

HDF5_VER="develop"

fi

git clone https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git --branch $HDF5_VER --single-branch $HDF5_VER

cd $HDF5_VER

if [ ! -f configure ]; then
  whereis libtoolize
  which libtool
  which libtoolize
  export LIBTOOL=`which libtool`
  export LIBTOOLIZE=`which libtoolize`
  ./autogen.sh
fi

./configure --enable-parallel --disable-fortran --disable-hl --without-szip --without-zlib  --prefix=$HOME/hdf5 && make > result.txt 2>&1 && make install
