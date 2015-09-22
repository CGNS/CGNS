#!/bin/sh
git clone https://github.com/CGNS/CGNS.git
cd CGNS/src
git checkout develop
export FC=gfortran
export F77=gfortran
export CC=gcc
./configure \
--with-hdf5=$HOME/build/CGNS/CGNS/hdf5_1_8/hdf5 \
--enable-lfs \
--enable-64bit \
--disable-shared \
--enable-debug \
--with-zlib \
--disable-cgnstools \
--disable-x \
--enable-64bit

#--with-fortran \



