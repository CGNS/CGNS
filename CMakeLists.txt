cmake_minimum_required(VERSION 2.6)
if(COMMAND cmake_policy)
	cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build Type, should be Release")
project("cgns")

# Allow for building a package
set(CPACK_PACKAGE_VERSION "3.0.4")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "cgns")
set(CPACK_GENERATOR "STGZ;TGZ;ZIP")
set(CPACK_SOURCE_GENERATOR "STGZ;TGZ;ZIP")
include(CPack)

enable_testing()

CONFIGURE_FILE(
	"${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
	"${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
	IMMEDIATE @ONLY)

ADD_CUSTOM_TARGET(uninstall
	"${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake")

###########
# Fortran #
###########

# Allow fortran support to be toggled
set(ENABLE_FORTRAN "OFF" CACHE BOOL "Enable or disable the use of Fortran")

# If we are using fortran, enable it and configure the C-Fortran interface
# It would be nice to automatically detect this setting
if(ENABLE_FORTRAN)
	enable_language("Fortran")
	message(STATUS "Fortran Enabled")

	set(FORTRAN_NAMING_HELP 
		"Configures how to link the Fortran components into the C library.
	
		Options are:
	
		LOWERCASE
		LOWERCASE_
		LOWERCASE__
		UPPERCASE
		UPPERRCASE_
		UPPERRCASE__
	
		LOWERCASE_ is known to work with gfortran.  If this setting does not work with your compiler, try the others until one does.
		")

	set(LIST  "LOWERCASE" "LOWERCASE_" "LOWERCASE__" "UPPERCASE" "UPPERCASE_" "UPPERCASE__")
	foreach(nc ${LIST})
		try_compile(NAMING_TEST_RESULT
			"${CMAKE_BINARY_DIR}/fortran_test"
			"${CMAKE_SOURCE_DIR}/fortran_test"
			projectName ${nc})
		if(${NAMING_TEST_RESULT} STREQUAL "TRUE")
			set(FORTRAN_NAMING "${nc}" CACHE STRING ${FORTRAN_NAMING_HELP})
			message(STATUS "Trying Fortran naming convention ${FORTRAN_NAMING}: success")
			message(STATUS "Setting fortran naming convention to ${FORTRAN_NAMING}")
			#add_definitions(-D${FORTRAN_NAMING})
			break()
		else(${NAMING_TEST_RESULT} STREQUAL "TRUE")
			message(STATUS "Trying Fortran naming convention ${nc}: failure")
		endif(${NAMING_TEST_RESULT} STREQUAL "TRUE")
	endforeach(nc ${LIST})
	if(NOT ${FORTRAN_NAMING})
		set(FORTRAN_NAMING "LOWERCASE" CACHE STRING ${FORTRAN_NAMING_HELP})
	endif(NOT ${FORTRAN_NAMING})
endif(ENABLE_FORTRAN)

########
# HDF5 #
########

# Setup the variables for HDF5
set(HDF5_INCLUDE_DIR "" CACHE PATH "Directory containing HDF5 headers")
set(HDF5_LIBRARY_DIR "" CACHE PATH "Directory containing HDF5 library")

set(HDF5_NEED_ZLIB "OFF" CACHE BOOL "Does the HDF5 library require linking to zlib?")
if(HDF5_NEED_ZLIB)
	set(ZLIB_INCLUDE_DIR "" CACHE PATH "Directory containing zlib headers")
	set(ZLIB_LIBRARY_DIR "" CACHE PATH "Directory containing zlib library")
endif(HDF5_NEED_ZLIB)

set(HDF5_NEED_SZIP "OFF" CACHE BOOL "Does the HDF5 library require linking to zlib?")
if(HDF5_NEED_SZIP)
	set(SZIP_INCLUDE_DIR "" CACHE PATH "Directory containing szip headers")
	set(SZIP_LIBRARY_DIR "" CACHE PATH "Directory containing szip library")
endif(HDF5_NEED_SZIP)

set(HDF5_NEED_MPI "OFF" CACHE BOOL "Does the HDF5 library require linking to mpi? (Only true if using parallel HDF5)")
if(HDF5_NEED_MPI)
	set(MPI_INCLUDE_DIR "" CACHE PATH "Directory containing MPI headers")
	set(MPI_LIBRARY_DIR "" CACHE PATH "Directory containing MPI library")
endif(HDF5_NEED_MPI)

####################
# RPATH Management #
####################

# use, i.e. don't skip the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE) 

# the RPATH to be used when installing
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

########
# CGNS #
########

# Include the src directory
add_subdirectory(src)