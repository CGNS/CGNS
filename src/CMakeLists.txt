#add_definitions(-DUSE_CGIO)
# Use HDF5 in the library
add_definitions(-DBUILD_HDF5)

##########
# Global #
##########

# Include the local directory
include_directories(.)
# Includes from HDF5
include_directories(${HDF5_INCLUDE_DIR})
if(HDF5_NEED_MPI)
	include_directories(${MPI_INCLUDE_DIR})
endif(HDF5_NEED_MPI)

###########
# Library #
###########

# All the C files needed by the cgns library
set(cgns_C_FILES
	cgns_error.c
	cgns_internals.c
	cgns_io.c
	cgnslib.c
	adf/ADF_interface.c
	adf/ADF_internals.c
	adf/adf_cond.c
	adfh/ADFH.c)

set_property(SOURCE adfh/ADFH.c adf/ADF_internals.c
	PROPERTY COMPILE_DEFINITIONS USE_CGIO)

# All the Fortran files need by the cgns library (if enabled)
set(cgns_F_FILES
	#adfh/ADFH_ftoc.c
	cg_ftoc.c
	cgio_ftoc.c)

# Only build those files that are needed
if (ENABLE_FORTRAN)
	set_property(SOURCE ${cgns_F_FILES}
		PROPERTY COMPILE_DEFINITIONS ${FORTRAN_NAMING})
	set(cgns_FILES ${cgns_C_FILES} ${cgns_F_FILES})
else (ENABLE_FORTRAN)
	set(cgns_FILES ${cgns_C_FILES})	
endif (ENABLE_FORTRAN)

set(CGNS_BUILD_SHARED "ON" CACHE BOOL "Build a shared version of the library")

# Build a static version of the library
add_library(cgns_static STATIC ${cgns_FILES})
# Build a shared version of the library
if(CGNS_BUILD_SHARED)
	add_library(cgns_shared SHARED ${cgns_FILES})
endif(CGNS_BUILD_SHARED)

# Change the output name of the library to be libcgns
set_target_properties(cgns_static PROPERTIES OUTPUT_NAME cgns)
set_target_properties(cgns_static PROPERTIES CLEAN_DIRECT_OUTPUT 1)
if(CGNS_BUILD_SHARED)
	set_target_properties(cgns_shared PROPERTIES OUTPUT_NAME cgns)
	set_target_properties(cgns_shared PROPERTIES CLEAN_DIRECT_OUTPUT 1)
endif(CGNS_BUILD_SHARED)

# Set the version numbers
set_target_properties(cgns_static PROPERTIES VERSION "3.0")
set_target_properties(cgns_static PROPERTIES VERSION "3.0")
set_target_properties(cgns_static PROPERTIES SOVERSION "3.0")

if(CGNS_BUILD_SHARED)
	set_target_properties(cgns_shared PROPERTIES VERSION "3.0")
	set_target_properties(cgns_shared PROPERTIES VERSION "3.0")
	set_target_properties(cgns_shared PROPERTIES SOVERSION "3.0")
endif(CGNS_BUILD_SHARED)

# Set the install path of the static library
install(TARGETS cgns_static ARCHIVE DESTINATION lib)
# Set the install path of the shared library
if(CGNS_BUILD_SHARED)
	install(TARGETS cgns_shared LIBRARY DESTINATION lib)
endif(CGNS_BUILD_SHARED)

# Set the install path of the header files
install(FILES
	cgnslib.h
	cgnslib_f.h
	cgns_io.h
	cgnswin_f.h
	DESTINATION include)

#########
# Tools #
#########

add_subdirectory(tools)

#########
# Tests #
#########

add_subdirectory(tests)

##############
# CGNS Tools #
##############


set(BUILD_CGNSTOOLS "OFF" CACHE BOOL "Build the CGNSTools package")
if(BUILD_CGNSTOOLS)
	add_subdirectory(cgnstools)
endif(BUILD_CGNSTOOLS)
