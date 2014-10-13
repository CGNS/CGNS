This example assumes all the needed packages are in ${HOME}/packages and the all the scripts 
are placed in ${HOME}/packages.

NOTE (1): If the system already has HDF5 installed then you can use it instead, as long as it was build with --enable-parallel.

(1) Building zlib from source:
  
   Download and extract the zlib source: http://www.zlib.net/

   (a) cd into the top level zlib source directory.
   (b) modify and run the script: ../build_zlib

(2) Building hdf5 from source:

   (a) From the top level of the hdf5 library, change the ${HOME}/packages to 
    where zlib was installed in STEP 1. 

../build_hdf5 --without-pthread --disable-shared --enable-parallel --enable-production \
--enable-fortran --enable-fortran2003 \
--disable-stream-vfd --disable-direct-vfd \
--with-zlib=${HOME}/packages/zlib-1.2.8/lib --prefix=${HOME}/packages/phdf5-trunk

where prefix is set for where the hdf5 library will get installed. There should 
be no need to modify in the script.

(3) Building cgns from source:

   (a) cd into the cgns/src directory
   (b) modify and run: <pathto>/build_cgns
   (c) make
   (d) To make the tests: 
			  cd ptests
			  make

(4) IMPORTANT PARAMETERS FOR GOOD PERFORMANCE

    (i) The environment variable BGLOCKLESSMPIO_F_TYPE=0x47504653 should be set.

        * Set using qsub --env BGLOCKLESSMPIO_F_TYPE=0x47504653

    (ii) does CGNS_FILETYPE need to be set ????


