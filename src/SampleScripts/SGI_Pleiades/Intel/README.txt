2.2 Building on SGI (Lustre)

0. Module Enviromnment:
    1) comp-intel/2015.0.090   2) mpi-intel/5.0.1.035
  (mpi-sgi/mpt.2.11r13 generates strange error messages at run time)


1. Building zlib from source: Download and extract the zlib source: http://www.zlib.net/

   (a) cd into the top level zlib source directory. 

   (b) modify and run the script: ../build_zlib

   (c) Note: if you get errors in steps 2 or 3 below because it cannot find the zlib.h or zlib.a
       files, it may be necessary to do the following:
          cd $HOME/packages/zlib-1.2.8
          ln -s lib/include/zlib.h .
          ln -s lib/lib/libz.a . 

2. Building hdf5 from source (version 1.8.14)

   (a) From the top level of the hdf5 library, change the ${HOME}/packages to where zlib was installed in STEP 1. 
    
   (b) ../build_hdf5

3. Building cgns from source:

   Note before compiling: CGNS expects the compilers to be mpicc and mpif90. Therefore, create a directory 'bin' in your home directory
   and add symbolic links to the mpi/intel wrappers:

      mpicc -> /nasa/intel/impi/5.0.1.035/bin64/mpiicc*
      mpif90 -> /nasa/intel/impi/5.0.1.035/bin64/mpiifort* 
   
   The script will point to these symbolic links.

   (a) cd into the cgns/src directory 

   (b) modify and run: <pathto>/build_cgns  (need to point to your own bin directory as noted above)
 
   (c) make

   (d) To make the tests: cd ptests; make

4. Running:

   (a) tests need to be submitted as a job; see, e.g.: run_cgns.sh

5. IMPORTANT PARAMETERS FOR GOOD PERFORMANCE

   (a) The Lustre parameters have not been fully tested.

   (b) It is best to create a directory with the lustre file parameters, for example:

       On Pleiades, cd to /nobackupp8/<username>:

          mkdir 2M
	  lfs setstripe -c 64 -s 2m 2M

        Then run in subdirectory 2M, using example submit script run_cgns.sh (appropriately modified)

	This will use a count of 64 and a strip size of 2M, which has shown good performance.


