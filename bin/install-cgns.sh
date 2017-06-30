#!/bin/sh
#
# Builds CGNS for travis CI. 
#
set -e
#git clone -b develop --single-branch https://github.com/CGNS/CGNS.git
#cd CGNS/src
cd src
if [ $TRAVIS_OS_NAME = "linux" ]; then
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
--enable-64bit




