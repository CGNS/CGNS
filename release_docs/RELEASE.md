# INTRODUCTION

This document describes the difference between CGNS 4.4.0 and 
CGNS 4.5.0 and contains information on known problems in
CGNS 4.5.0.

Links to the CGNS current released source code can be found at:
<a href="https://cgns.github.io/current/download.html#supportdownload" rel="nofollow">Download</a>
User MLL documentation for the current release can be found at:
<a href="https://cgns.github.io/standard/MLL/CGNS_MLL.html#standardmll" rel="nofollow">MLL Documentation</a>
For more general information, see the CGNS home page:
<a href="http://cgns.org" rel="nofollow">CGNS.org</a>

## CONTENTS
- New Features
- Bug Fixes since CGNS 4.4.0
- Known Problems

# New Features

> [!NOTE]
> This release includes new Doxygen descriptions of the MLL APIs, which are now utilized in the updated documentation at cgns.org.

> [!IMPORTANT]
> All new Fortran APIs introduced in this release that expect a character string are required to pass a scalar character string (i.e.,` CHARACTER(LEN=*) `) and not an array of character strings (i.e., `CHARACTER(LEN=*), DIMENSION(*)`).

Configuration:
--------------------
N/A

Library:
---------------
* New CPEX0046 Particle extension, the inclusion of particle data in CGNS.
  - For Details, see: https://cgns.org/_downloads/74b6fa06432163b5730964340e3084bc/CPEX0046-v2-rev1.pdf
  - Numerous new MLL APIs, both parallel and serial, were added. See the MLL documentation 
    at cgns.org for more details.

> [!NOTE]
> We appreciate Convergent Science's contributions to CPEX 0046 and its implementation.

* Added Fortran support for `cg_configure_f` to allow a callback function, such as in the case of CG_CONFIG_ERROR.

Parallel Library:
------------------------

* Added new PointList Functions:  `cgp_parent_data_write(_f)`, `cgp_ptlist_write_data(_f)` and
  `cgp_ptlist_read_data(_f)`, PR #730, Issue #728.
   - Parallel writing/reading of any PointSet is possible via a `cg_goto` statement rather than
     just pointsets associated with `BC_t` nodes.

* Added poly elements read/parent elements read-write APIs: 
   - `cgp_poly_section_write(_f)`, `cgp_poly_elements_write_data(_f)`
   -  `cgp_poly_elements_read_data_elements`, `cgp_poly_elements_read_data_offsets(_f)`,`cgp_parentelements_read_data(_f)`


Fortran Library:
-----------------------
See section _Parallel Library_.

Tools:
-------------
N/A

# Bug Fixes since CGNS 4.4.0 release

Configuration:
--------------------
* Fixed compilation issues due to missing _POSIX_SOURCE define, Issue #810.

* Add cgns-config.cmake file.
  - Also change hdf5 target names to match those from find_package(HDF5)

* Fix for HDF5 1.14.3 and onward with the NAG compiler.
  - IEEE standard arithmetic enables software to raise exceptions such as overflow,
  division by zero, and other illegal operations without interrupting or halting
  the program flow. The HDF5 C library intentionally performs these exceptions.
  Therefore, the "-ieee=full" nagfor switch is necessary when compiling a program
  to avoid stopping on an exception.

* Improve cmake packaging
   - Set an installed rpath that handles install location differently from "lib"
     for multiple arch support, for instance.

* Update cmake requirement
   - Update cmake requirement to 3.20
   - Fix conversion issue with vs2022 compiler
   - Handle F2C string on ARM64
   - Hidden parameters should be handled through a size_t according to  https://gcc.gnu.org/onlinedocs/gcc-8.2.0/gfortran/Argument-passing-conventions.html

* Remove the deprecated build system on Windows (i.e., .bat); CMake is the current development focus.

* Bumped the Autotools prerequisite to 2.71, and fixed the check to allow for HDF5 version 2.0.0

Library:
--------------
* [CGNS-284] Fixed  cgi_warning
  -  modify return status when the bounding box is not found.
  -  No more cgi_warning shown see [CGNS-284]
  -  removed outdated documentation

* Do not force Elements_t node to be contiguous
  - Only data arrays are forced to be contiguous in the Elements_t node

* fixed uninitialized warnings

* Fix issue https://github.com/CGNS/CGNS/issues/754 (https://github.com/CGNS/CGNS/pull/756)
  - use C99 types int64_t
  - define cgsize_t as a intxx_t type
  - provide PRIdCGSIZE
  - follow hdf5 changes

* Corrected int types for MPI_Allgather  in ptests
   - Fixed cgsize_t type for MPI _Allgather, in tests, depending on how CGNS was built.

* Misc windows fixes 
   - Make cmakelist more robust to Multi-configuration generators
   - Improve Win32 ctest support
   - Remove redefinition of MACRO,  the MACRO is already defined in cgnslib.h

* use size_t casting to prevent allocation failure
  - the cast to size_t is done before multiplying with size_of
  - size_of now returns a size_t
  - This prevents capacity reduction in 32-bit mode when cgsize_t is an int.

* Remove access() calls in the CGNS library
   - Remove ACCESS calls due to possible race condition

* Address miscellaneous CodeQL issues and warnings.

Fortran:
---------------
* made Fortran Test_Family  example output valid for cgnscheck

Tools:
-------------
* backport gcc14 fedora patch for tkogl
* [cgnscheck] check multiple grid coordinates nodes & null pointer

</code></pre></div>
# Known Problems

************ FORTRAN ************

* A gfortran bug in version 10.2 broke Fortran mapping and caused cg_goto_f 
 to segfault. All other versions of gfortran are suitable.
 (ref. CGNS-246, GNU BUG 100149)

* A bug in gfortran (all versions) causes cg_configure_f to fail,
  GNU BUG 99982. Other Fortran compilers are OK.

* Building CGNS with parallel enabled, with Fortran enabled, and as a shared library on Windows
   is not working. Some C parallel tests fail on Windows with the Intel Compiler.

************ FORTRAN END ********

************ CGNSVIEW ************

* cgnsview for OSX is not viewing properly, and cgnsview under Windows 
  may fail to compile due to tcl/tk incompatibility. 

************ CGNSVIEW END ********

* For other issues, see https://github.com/CGNS/CGNS/issues
</code></pre></div>

# Supported Platforms

The following platforms are supported and have been tested for this release.
They are built with autotools unless specified otherwise.

> [!NOTE] 
> CGNS is not testing on any 32-bit Linux systems

|Platform | Configuration |
|-- | -- |
| Centos7 <br> #1 SMP x86_64 GNU/Linux<br>      | GNU C (gcc), Fortran (gfortran), C++ (g++) <br>compilers: <br> Version 4.8.5 20150623 (Red Hat 4.8.5-4) <br> Version 4.9.3, Version 7.2.0, Version 8.3.0, <br> Version 9.1.0, Version 10.2.0 <br> |
|                                               |  Intel(R) C (icc), C++ (icpc), Fortran (icc) <br> compilers: <br>   Version 17.0.0.098 Build 20160721 |
|                                               |  GNU C (gcc) and C++ (g++) 4.8.5 compilers<br>&nbsp;&nbsp;&nbsp;with NAG Fortran Compiler Release 7.1(Hanzomon)|
|                                               | Intel(R) C (icc) and C++ (icpc) 17.0.0.098|
|                                               | GNU C (gcc) and C++ (g++) 4.8.5 compilers with NAG Fortran Compiler Release 7.1(Hanzomon) |
|                                               | Intel(R) C (icc) and C++ (icpc) 17.0.0.098 compilers with NAG Fortran Compiler Release 7.1(Hanzomon)|
|                                               | MPICH 3.1.4 compiled with GCC 4.9.3 |
|                                               | MPICH 3.3 compiled with GCC 7.2.0 |
|                                               | OpenMPI 3.1.3 compiled with GCC 7.2.0, and 4.1.2 compiled with GCC 9.1.0 |
|                                               | NVIDIA nvc, nvfortran and nvc++ version 22.5-0 (autotools and cmake) |
| Ubuntu 24.04 <br> #1 SMP x86_64 GNU/Linux<br> | GNU C (gcc), Fortran (gfortran), C++ (g++) <br>compilers: 13.2 |
|                                               | OpenMPI 4.1.6 |

> [!NOTE]
> CGNS is not testing on any 32-bit Windows systems

|Platform         | Configuration                                   |
|---------------- | ----------------------------------------------- |
|  Windows 11 x64 | Visual Studio 2019 w/ Intel OneAPI 2025 (CMake) |
|                 | Visual Studio 2019 w/ MSMPI 10.1 (CMake)        |

> [!NOTE]
> CGNS is tested with the two latest macOS versions that are available
> on github runners. As new major macOS versions become available, CGNS
> will discontinue support for the older version and add the new latest
> version to its list of compatible systems, along with the previous version.


|Platform                   | Configuration                             |
|-------------------------- | ----------------------------------------- |
|macOS Sonoma 14.7.2 64-bit | Apple LLVM<br> gfortran GNU Fortran (GCC) |
|macOS Sequoia 15.2 64-bit  | Apple LLVM<br> gfortran GNU Fortran (GCC) |

</code></pre></div>
# Tested Configuration Features Summary
<div class="snippet-clipboard-content position-relative overflow-auto"><pre><code>In the table below
      y   = tested
      n   = not tested in this release
      x   = not working in this release
</code></pre></div>

Platform | C | C[1] | Fortran | Fortran [1]
-- | -- | -- | -- | --
Windows 10 | n | n | n | n
Windows 10 x64 | y | n | n | n
Windows 10 Cygwin | n | n | x | n
Mac OS X  Sonoma 64-bit | y | n | y | n
Mac OS X Sequoia 64-bit | y | n | y | n
CentOS 7.2 Linux 3.10.0 x86_64 PGI | y | n | y | n
CentOS 7.2 Linux 3.10.0 x86_64 GNU | y | y | y | y
CentOS 7.2 Linux 3.10.0 x86_64 Intel | y | y | y | y
Linux 2.6.32-573.18.1.el6.ppc64 | y | n | y | n

<p>[1] Parallel

# Acknowledgements
 * Thank you, Mickael Philit, for contributing to updating the documentation and resolving various issues in the CGNS library for this release. 
 * We appreciate all the CGNS users who reported issues and submitted pull requests.
 * A special thanks to Sandia National Laboratories for their support of the work done by The HDF Group for this release.

