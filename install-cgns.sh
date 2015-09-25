#!/bin/sh
git clone -b develop --single-branch https://github.com/CGNS/CGNS.git
cd CGNS/src
export FC=gfortran
export F77=gfortran
export CC=gcc
export FLIBS="-Wl,--no-as-needed -ldl -lz"
export LIBS="-Wl,--no-as-needed -ldl -lz"
./configure \
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
make
if [ "$?" -ne "0" ];then
  echo "**FAILED** IN MAKE"
  exit 1
fi
make test
if [ "$?" -ne "0" ];then
  echo "**FAILED** IN MAKE TEST"
  exit 1
fi




