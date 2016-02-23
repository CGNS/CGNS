#!/bin/sh
set -e
#git clone -b develop --single-branch https://github.com/CGNS/CGNS.git
#cd CGNS/src
cd src
if [ $TRAVIS_OS_NAME = "osx" ]; then
  export FC=gfortran-4.9
  export F77=gfortran-4.9
  export CC=gcc-4.9
else
  export FC=gfortran
  export F77=gfortran
  export CC=gcc
  export FLIBS="-Wl,--no-as-needed -ldl -lz"
  export LIBS="-Wl,--no-as-needed -ldl -lz"
fi

./configure \
--prefix=$PWD/cgns_build \
--with-hdf5=$HOME/hdf5 \
--with-fortran \
--enable-lfs \
--enable-64bit \
--disable-shared \
--enable-debug \
--with-zlib \
--disable-cgnstools \
--disable-x \
--enable-64bit




