2.2 Building on SGI (Lustre)

1. Building zlib from source: Download and extract the zlib source: http://www.zlib.net/

   (a) cd into the top level zlib source directory. 

   (b) modify and run the script: ../build_zlib

2. Building hdf5 from source

   (a) From the top level of the hdf5 library, change the ${HOME}/packages to where zlib was installed in STEP 1. 
    
   (b) ../build_hdf5

3. Building cgns from source:

   (a) cd into the cgns/src directory 

   (b) modify and run: <pathto>/build_cgns 

   (c) make

   (d) To make the tests: cd ptests; make;make tests

4. IMPORTANT PARAMETERS FOR GOOD PERFORMANCE

   (a) The Lustre parameters have not been fully tested.

   (b) On Pleiades, lfs setstripe -c 64 -s 0 /nobackupp8/<dir>, has shown good performance.