#!/bin/sh
#
# Builds and tests CGNS for travis CI.
#
cd src
make
make install
cd tests
make
make test
cd ../examples/fortran
make
make test
cd ../../Test_UserGuideCode/Fortran_code
make
make test
cd ../C_code
make
make test
