#!/bin/tcsh
#
# This script uses cmake to build cgns, parallel build,
# it should be executed some place other then in the
# source directory.

#  Note:
#  Does szip support

# set to where hdf5 is installed.
set HDF5=/mnt/scr1/trunk/hdf5
# set to where the mpi compilers are
set MPI=/mnt/hdf/packages/mpich/3.1.3_xl15.1.0/ppc64/bin
# set to where the source is located
set CGNS=/mnt/scr1/HDF5_Parallel

cmake \
  -D CMAKE_C_COMPILER:PATH=$MPI/mpicc \
  -D CMAKE_Fortran_COMPILER:PATH=$MPI/mpif90 \
 $CGNS

ccmake  \
  -D CGNS_BUILD_SHARED:BOOL=OFF \
  -D CGNS_USE_SHARED:BOOL=OFF \
  -D HDF5_NEED_ZLIB:BOOL=ON \
  -D CMAKE_EXE_LINKER_FLAGS:STRING="" \
  -D CMAKE_STATIC_LINKER_FLAGS:STRING="" \
  -D MPIEXEC:STRING=$MPI/mpiexec \
  -D MPI_C_COMPILER:STRING=$MPI/mpicc \
  -D MPI_Fortran_COMPILER:STRING=$MPI/mpif90 \
  -D CGNS_ENABLE_HDF5:BOOL=ON \
  -D CGNS_ENABLE_TESTS:BOOL=ON \
  -D CGNS_BUILD_CGNSTOOLS:BOOL=OFF \
  -D HDF5_LIBRARY_DIR:PATH=$HDF5/lib \
  -D HDF5_LIBRARY:STRING=$HDF5/lib/libhdf5.a \
  -D HDF5_INCLUDE_DIR:PATH=$HDF5/include \
  -D HDF5_INCLUDE_PATH:PATH=$HDF5/include \
  -D HDF5_NEED_MPI:BOOL=ON \
  -D CGNS_ENABLE_FORTRAN:BOOL=ON \
  -D CGNS_ENABLE_64BIT:BOOL=ON \
  -D CMAKE_C_COMPILER:PATH=$MPI/mpicc \
  -D CMAKE_Fortran_COMPILER:PATH=$MPI/mpif90 \
  -D CGNS_ENABLE_PARALLEL:BOOL=ON \
  -D CMAKE_INSTALL_PREFIX:PATH="./" \
  .

# then do:
# make
# make install
