#!/bin/csh
svn co https://svn.hdfgroup.uiuc.edu/hdf5/trunk
setenv CC gcc
cd trunk && ./autogen.sh ./configure --disable-fortran --disable-hl && make && make install
