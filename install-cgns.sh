#!/bin/sh
git clone https://github.com/CGNS/CGNS.git
cd CGNS/src
git checkout develop
./configure \
--with-fortran \
--with-hdf5=$HOME/hdf5_1_8/hdf5 \
--enable-lfs \
--enable-64bit \
--disable-shared \
--enable-debug \
--with-zlib \
--disable-cgnstools \
--disable-x \
--enable-64bit FC=gfortran CC=gcc



