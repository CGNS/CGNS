#!/bin/sh
git clone https://github.com/CGNS/CGNS.git
cd CGNS/src
git checkout develop
export FC=gfortran
export F77=gfortran
export CC=gcc
export FLIBS="-Wl,--no-as-needed -ldl -lz"
export LDFLAGS="-Wl,--no-as-needed -ldl"
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
make test




