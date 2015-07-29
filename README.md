# CGNS

## About

The CFD General Notation System (CGNS) provides a standard for recording and recovering computer data associated with the numerical solution of fluid dynamics equations.

## Installation

### Installation Instructions using cmake

1. Install HDF5 on your system

    (a) HDF5 can use the standard GNU autotools, so './configure',
    'make', 'sudo make install' should install HDF5 without
    problems on most systems.

2. Unpack the tar ball containing the source code into some
  directory.

3. Create a new director in which to build the library.

4. Use cmake to initialize the build tree.

   user@hostname:build_path$ cmake /path/to/cgns/sources/


5. Use ccmake to edit the control variables as needed.

    user@hostname:build_path$ ccmake .


    (a) The paths to the HDF5 libraries and header files must be set in 'HDF5_LIBRARY_DIR' and 'HDF5_INCLUDE_DIR' respectively.
    
### Installation Instructions using make

1. Install HDF5 on your system

    (a) HDF5 can use the standard GNU autotools, so './configure', 'make', 'sudo make install' should install HDF5 without problems on most systems.
    
2.  Typically the standard ./configure, make, make install will suffice.  

3. Sample scripts for building parrallel CGNS can be found in src/SampleScripts.
 
      
## Usage

## Development
CGNS uses the branching/release model as summarized at:

http://nvie.com/posts/a-successful-git-branching-model/
  

![image](https://github.com/CGNS/cgns.github.io/blob/master/git-model.png)