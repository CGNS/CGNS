#!/bin/bash

NO_COLOR="\033[0m"
OK_COLOR="\033[32;01m"
WARN_COLOR="\033[33;01m"
ERROR_COLOR="\033[31;01m"
NOTE_COLOR="\033[36;01m"

#
# Configure CGNS. 
#
set -e
SRC_DIR=$PWD

BUILD=$1
OPTS=$2
HDF5_DIR=$HOME/hdf5
OPTS_CMAKE=""
if [[ "$OPTS" == *"with-fortran"* ]]; then
   OPTS_CMAKE="$OPTS_CMAKE -D CGNS_ENABLE_FORTRAN:BOOL=ON"
else
   OPTS_CMAKE="$OPTS_CMAKE -D CGNS_ENABLE_FORTRAN:BOOL=OFF"
fi
if [[ "$OPTS" == *"with-hdf5"* ]]; then
   OPTS_CMAKE="$OPTS_CMAKE -D CMAKE_PREFIX_PATH=$HDF5_DIR -D CGNS_ENABLE_HDF5:BOOL=ON -D HDF5_NEED_ZLIB:BOOL=OFF -D HDF5_NEED_SZIP:BOOL=OFF"
else
   OPTS_CMAKE="$OPTS_CMAKE -D CGNS_ENABLE_HDF5:BOOL=OFF"
fi
if [[ "$OPTS" == *"enable-cgnstools"* ]]; then
   OPTS="$OPTS --with-tcl=/usr/lib/x86_64-linux-gnu --with-tk=/usr/lib/x86_64-linux-gnu"
   OPTS_CMAKE="$OPTS_CMAKE -D CGNS_BUILD_CGNSTOOLS:BOOL=ON"
else
   OPTS_CMAKE="$OPTS_CMAKE -D CGNS_BUILD_CGNSTOOLS:BOOL=OFF"
fi

if [[ "$OS_NAME" = "linux" ]]; then
  export FLIBS="-Wl,--no-as-needed -ldl"
  export LIBS="-Wl,--no-as-needed -ldl"
# See note in src/ptests/pcgns_ftest.F90 concerning CGNS-109 and Github Actions
  export FCFLAGS="-D DISABLE_CGNS109"

# Do a different test set-up than the autotools build
  OPTS_CMAKE="$OPTS_CMAKE -D CGNS_ENABLE_LEGACY:BOOL=ON -D CGNS_ENABLE_64BIT:BOOL=OFF"
fi

if [[ "$BUILD" == "cmake" ]]; then

##########################
# CMAKE CONFIG
##########################

printf "%b\n" "$NOTE_COLOR"
printf "%b" "$NOTE_COLOR" "       ________  ______    __ __ ______\n"
printf "%b" "$NOTE_COLOR" "      / ____/  |/  /   |  / //_// ____/\n"
printf "%b" "$NOTE_COLOR" "     / /   / /|_/ / /| | / ,<  / __/   \n"
printf "%b" "$NOTE_COLOR" "    / /___/ /  / / ___ |/ /| |/ /___   \n"
printf "%b" "$NOTE_COLOR" "    \____/_/  /_/_/  |_/_/ |_/_____/   \n\n"
printf "%b\n" "$NO_COLOR"

cd $SRC_DIR
mkdir cbuild
cd cbuild

cmake \
    ${OPTS_CMAKE} \
    -D CMAKE_C_COMPILER:PATH=$CC \
    -D CMAKE_C_FLAGS:STRING="$CFLAGS" \
    -D CMAKE_BUILD_TYPE:STRING="Debug" \
    -D CMAKE_STATIC_LINKER_FLAGS:STRING="" \
    -D CGNS_ENABLE_TESTS:BOOL=ON \
    -D CGNS_ENABLE_LFS:BOOL=ON \
    -D CMAKE_INSTALL_PREFIX:PATH="./" \
    -D CMAKE_EXE_LINKER_FLAGS:STRING="$CMAKE_EXE_LINKER_FLAGS" \
    ..

else

##########################
# AUTOTOOLS CONFIG
##########################

printf "%b" "$NOTE_COLOR" "       ___   __  ____________  __________  ____  __   _____\n"
printf "%b" "$NOTE_COLOR" "      /   | / / / /_  __/ __ \/_  __/ __ \/ __ \/ /  / ___/\n"
printf "%b" "$NOTE_COLOR" "     / /| |/ / / / / / / / / / / / / / / / / / / /   \__ \ \n"
printf "%b" "$NOTE_COLOR" "    / ___ / /_/ / / / / /_/ / / / / /_/ / /_/ / /______/ / \n"
printf "%b" "$NOTE_COLOR" "   /_/  |_\____/ /_/  \____/ /_/  \____/\____/_____/____/  \n\n"
printf "%b\n" "$NO_COLOR"

cd src
echo "AUTOTOOLS DIR $PWD"

./configure $OPTS \
--prefix=$PWD/cgns_build \
--with-hdf5=$HOME/hdf5 \
--enable-lfs \
--enable-64bit \
--disable-shared \
--enable-debug

fi