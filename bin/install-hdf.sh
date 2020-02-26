#!/bin/sh

# Set odd/even days of the week
day=$(( $(date +%d) % 2 ))

if [ $day -eq 0 ]; then #even day tests, HDF5 1.8 branch

HDF5_VER="hdf5_1_8"
OPTS="--disable-tools --disable-tests"

else #odd day tests, HDF5 develop branch

HDF5_VER="develop"
OPTS="--disable-tools --disable-tests"

fi

git clone https://bitbucket.hdfgroup.org/scm/hdffv/hdf5.git --branch $HDF5_VER --single-branch $HDF5_VER

cd $HDF5_VER

if [ ! -f configure ]; then
  export LIBTOOL=`which libtool`
  export LIBTOOLIZE=`which libtoolize`
  sudo ln -s $LIBTOOLIZE /libtoolize
  ./autogen.sh
fi

if [ $TRAVIS_OS_NAME = "linux" ]; then
    OPTS="$OPTS --enable-parallel"
fi

#./configure $OPTS --disable-fortran --disable-hl --without-szlib --without-zlib --prefix=$HOME/hdf5 && make > result.txt 2>&1 && make install

./configure $OPTS --disable-fortran --disable-hl --without-szlib --without-zlib --prefix=$HOME/hdf5
make
make install


