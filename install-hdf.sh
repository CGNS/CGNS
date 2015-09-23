#!/bin/sh

#trunk
#svn co https://svn.hdfgroup.uiuc.edu/hdf5/trunk
#cd trunk && ./autogen.sh ./configure --disable-fortran --disable-hl CC=gcc && make && make install

# 1.8 branch
svn co https://svn.hdfgroup.uiuc.edu/hdf5/branches/hdf5_1_8
cd hdf5_1_8 && ./configure --disable-fortran --disable-hl --prefix=$HOME/hdf5 CC=gcc && make >hdfbuild.out && make install
