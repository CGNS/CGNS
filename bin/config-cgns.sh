#!/bin/sh
#
# Configure CGNS for travis CI. 
#
set -e
cd src
OPTS=""
if [ $TRAVIS_OS_NAME = "linux" ]; then
  export FLIBS="-Wl,--no-as-needed -ldl"
  export LIBS="-Wl,--no-as-needed -ldl"
  OPTS="--enable-parallel --enable-cgnstools --with-tcl=/usr/lib/x86_64-linux-gnu --with-tk=/usr/lib/x86_64-linux-gnu"
else
  OPTS="--disable-cgnstools"
fi

./configure \
--prefix=$PWD/cgns_build $OPTS \
--with-hdf5=$HOME/hdf5 \
--with-fortran \
--enable-lfs \
--enable-64bit \
--disable-shared \
--enable-debug \
--enable-64bit




