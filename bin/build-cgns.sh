#!/bin/sh
#
# Builds and tests CGNS for travis CI.
#
set -e
cd src
make
make install
cd tests
make
make test
if [ $TRAVIS_OS_NAME = "linux" ]; then
  cd ../ptests
  make
  make test
fi
cd ../examples/fortran
make
make test
cd ../../Test_UserGuideCode/Fortran_code
make
make test
cd ../C_code
make
make test
