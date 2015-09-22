#!/bin/sh
svn co https://svn.hdfgroup.uiuc.edu/hdf5/trunk
cd trunk && ./autogen.sh ./configure --disable-fortran --disable-hl CC=gcc && make && make install
