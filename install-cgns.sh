#!/bin/csh
git clone https://github.com/CGNS/CGNS.git
cd CGNS/src
git checkout develop
setenv HDF5 $HOME/trunk/hdf5
setenv FC gfortran
setenv CC gcc
./configure \
--with-fortran \
--with-hdf5=$HDF5 \
--enable-lfs \
--enable-64bit \
--disable-shared \
--enable-debug \
--with-zlib \
--disable-cgnstools \
--disable-x \
--enable-64bit


