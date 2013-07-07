
##########
# Global #
##########

if (WIN32)
  add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif (WIN32)

# Include the local directory
include_directories(.)
if (CGNS_ENABLE_HDF5)
  add_definitions(-DBUILD_HDF5)
  if (WIN32)
    if (HDF5_LIBRARY)
      file(STRINGS ${HDF5_LIBRARY} HDF5isDLL REGEX "__imp__H5close")
      if (HDF5isDLL)
        # this is only needed when using hdf5 dll libs
        add_definitions(-DWIN32 -D_HDF5USEDLL_)
      endif (HDF5isDLL)
    endif (HDF5_LIBRARY)
  endif (WIN32)
  # Includes from HDF5
  if (HDF5_INCLUDE_PATH)
    include_directories(${HDF5_INCLUDE_PATH})
  endif (HDF5_INCLUDE_PATH)
  if(HDF5_NEED_MPI AND MPI_INC)
    include_directories(${MPI_INC})
  endif(HDF5_NEED_MPI AND MPI_INC)
  if (CGNS_ENABLE_PARALLEL)
    add_definitions(-DBUILD_PARALLEL)
  endif (CGNS_ENABLE_PARALLEL)
endif (CGNS_ENABLE_HDF5)

################################
# create configed header files #
################################

if (${SIZE_OF_LONG} GREATER 4)
  set(CGLONGT long)
else (${SIZE_OF_LONG} GREATER 4)
  set(CGLONGT "long long")
endif (${SIZE_OF_LONG} GREATER 4)

set(BUILDLEGACY 0)
set(BUILD64BIT 0)
set(BUILDSCOPE 0)
set(BUILDFORTRAN 0)

if (CGNS_ENABLE_LEGACY)
  set(BUILDLEGACY 1)
  set(CGFSIZET "")
else (CGNS_ENABLE_LEGACY)
  if (CGNS_ENABLE_64BIT)
    set(BUILD64BIT 1)
    set(CGFSIZET *8)
  else (CGNS_ENABLE_64BIT)
    set(CGFSIZET *4)
  endif (CGNS_ENABLE_64BIT)
endif (CGNS_ENABLE_LEGACY)
if (CGNS_ENABLE_SCOPING)
  set(BUILDSCOPE 1)
endif (CGNS_ENABLE_SCOPING)
if (CGNS_ENABLE_FORTRAN)
  set(BUILDFORTRAN 1)
endif (CGNS_ENABLE_FORTRAN)

set(SYSCFLAGS "")
set(CFGFLAGS "")
set(LIBS "")

file(TO_NATIVE_PATH ${CMAKE_INSTALL_PREFIX}/lib LIBDIR)
file(TO_NATIVE_PATH ${CMAKE_INSTALL_PREFIX}/include INCLUDEDIR)
file(TO_NATIVE_PATH ${CMAKE_C_COMPILER} CC)
if (WIN32)
  set(CGNSLIB cgns.lib)
else (WIN32)
  set(CGNSLIB libcgns.a)
  set(LIBS -lm)
endif (WIN32)

string(COMPARE EQUAL ${CMAKE_BUILD_TYPE} Debug IS_DEBUG)
if (IS_DEBUG)
  set(BUILDDEBUG 1)
  set(CFLAGS ${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_DEBUG})
  set(LDFLAGS ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_DEBUG})
else (IS_DEBUG)
  set(BUILDDEBUG 0)
  set(CFLAGS ${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE})
  set(LDFLAGS ${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_RELEASE})
endif (IS_DEBUG)

set(BUILDHDF5 0)
set(H5NEEDMPI 0)
set(H5NEEDSZIP 0)
set(H5NEEDZLIB 0)
set(BUILDPARALLEL 0)
set(HDF5INC "")
set(HDF5LIB "")
set(SZIPLIB "")
set(ZLIBLIB "")
set(MPIINC "")
set(MPILIBS "")

macro(native_paths INPUT_PATH_VARIABLE RESULT_VARIABLE)
  set(${RESULT_VARIABLE} "")
  foreach(PATH ${${INPUT_PATH_VARIABLE}})
    file(TO_NATIVE_PATH ${PATH} NATIVE_PATH)
    list(APPEND ${RESULT_VARIABLE} ${NATIVE_PATH})
  endforeach(PATH ${${INPUT_PATH_VARIABLE}})
endmacro(native_paths)

if (CGNS_ENABLE_HDF5)
  set(BUILDHDF5 1)
  if (HDF5_INCLUDE_PATH)
    native_paths(HDF5_INCLUDE_PATH HDF5INC)
  endif (HDF5_INCLUDE_PATH)
  if (HDF5_LIBRARY)
    native_paths(HDF5_LIBRARY HDF5LIB)
  endif (HDF5_LIBRARY)
  if (HDF5_NEED_SZIP)
    set(H5NEEDSZIP 1)
    if (SZIP_LIBRARY)
      native_paths(SZIP_LIBRARY SZIPLIB)
    endif (SZIP_LIBRARY)
  endif (HDF5_NEED_SZIP)
  if (HDF5_NEED_ZLIB)
    set(H5NEEDZLIB 1)
    if (ZLIB_LIBRARY)
      native_paths(${ZLIB_LIBRARY} ZLIBLIB)
    endif (ZLIB_LIBRARY)
  endif (HDF5_NEED_ZLIB)
  if (HDF5_NEED_MPI)
    set(H5NEEDMPI 1)
    if (MPI_INC)
      native_paths(MPI_INC MPIINC)
    endif (MPI_INC)
    if (MPI_LIBS)
      native_paths(MPI_LIBS MPILIBS)
    endif (MPI_LIBS)
    if (CGNS_ENABLE_PARALLEL)
      set(BUILDPARALLEL 1)
    endif (CGNS_ENABLE_PARALLEL)
  endif (HDF5_NEED_MPI)
endif (CGNS_ENABLE_HDF5)

#create these in build directory so is doesn't mess up the
#source directory, then add the path to them
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/cgnstypes.h.in
		${CMAKE_CURRENT_BINARY_DIR}/cgnstypes.h )
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/cgnstypes_f.h.in
		${CMAKE_CURRENT_BINARY_DIR}/cgnstypes_f.h )
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/cgnslib_f.h.in
		${CMAKE_CURRENT_BINARY_DIR}/cgnslib_f.h )
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/cgnsconfig.h.in
		${CMAKE_CURRENT_BINARY_DIR}/cgnsconfig.h )
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/cgnsBuild.defs.in
		${CMAKE_CURRENT_BINARY_DIR}/cgnsBuild.defs )

include_directories(${CMAKE_CURRENT_BINARY_DIR})

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
	adf/ADF_internals.c)

if (CGNS_ENABLE_HDF5)
  set(cgns_C_FILES ${cgns_C_FILES} adfh/ADFH.c)
  if (CGNS_ENABLE_PARALLEL)
    set(cgns_C_FILES ${cgns_C_FILES} pcgnslib.c)
  endif (CGNS_ENABLE_PARALLEL)
endif (CGNS_ENABLE_HDF5)

# All the Fortran files need by the cgns library (if enabled)
set(cgns_F_FILES
	cg_ftoc.c
	cgio_ftoc.c)

# Only build those files that are needed
if (CGNS_ENABLE_FORTRAN)
  if (FORTRAN_NAMING)
    set_property(SOURCE ${cgns_F_FILES}
      PROPERTY COMPILE_DEFINITIONS ${FORTRAN_NAMING})
  endif (FORTRAN_NAMING)
  set(cgns_FILES ${cgns_C_FILES} ${cgns_F_FILES})
else (CGNS_ENABLE_FORTRAN)
  set(cgns_FILES ${cgns_C_FILES})
endif (CGNS_ENABLE_FORTRAN)

set(CGNS_BUILD_SHARED "ON" CACHE BOOL "Build a shared version of the library")
option(CGNS_USE_SHARED "Link programs to the CGNS shared library" "ON")

# Build a static version of the library
add_library(cgns_static STATIC ${cgns_FILES})

# Build a shared version of the library
if(CGNS_BUILD_SHARED)
  mark_as_advanced(CLEAR CGNS_USE_SHARED)
  add_library(cgns_shared SHARED ${cgns_FILES})
  # under windows we need to define BUILD_DLL
  # and link with the HDF5 libraries to build a dll
  if (WIN32 OR CYGWIN)
    add_definitions(-DBUILD_DLL)
    if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
      target_link_libraries(cgns_shared ${HDF5_LIBRARY})
      if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
        target_link_libraries(cgns_shared ${ZLIB_LIBRARY})
      endif(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
      if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
        target_link_libraries(cgns_shared ${SZIP_LIBRARY})
      endif(HDF5_NEED_SZIP AND SZIP_LIBRARY)
      if(HDF5_NEED_MPI AND MPI_LIBS)
        target_link_libraries(cgns_shared ${MPI_LIBS})
      endif(HDF5_NEED_MPI AND MPI_LIBS)
    endif (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  endif (WIN32 OR CYGWIN)
else(CGNS_BUILD_SHARED)
  set(CGNS_USE_SHARED "OFF")
  mark_as_advanced(FORCE CGNS_USE_SHARED)
endif(CGNS_BUILD_SHARED)

# Change the output name of the library to be libcgns
set_target_properties(cgns_static PROPERTIES OUTPUT_NAME cgns)
set_target_properties(cgns_static PROPERTIES CLEAN_DIRECT_OUTPUT 1)
if(CGNS_BUILD_SHARED)
  # for windows we need to change the name of the shared library
  # for both static and shared version to exist
  if (WIN32 OR CYGWIN)
    set_target_properties(cgns_shared PROPERTIES OUTPUT_NAME cgnsdll)
  else (WIN32 OR CYGWIN)
    set_target_properties(cgns_shared PROPERTIES OUTPUT_NAME cgns)
  endif (WIN32 OR CYGWIN)
  set_target_properties(cgns_shared PROPERTIES CLEAN_DIRECT_OUTPUT 1)
endif(CGNS_BUILD_SHARED)

# Set the version numbers
set_target_properties(cgns_static PROPERTIES VERSION ${CGNS_VERSION})
set_target_properties(cgns_static PROPERTIES SOVERSION ${CGNS_VERSION})

if(CGNS_BUILD_SHARED)
  set_target_properties(cgns_shared PROPERTIES VERSION ${CGNS_VERSION})
  set_target_properties(cgns_shared PROPERTIES SOVERSION ${CGNS_VERSION})
endif(CGNS_BUILD_SHARED)

# Set the install path of the static library
install(TARGETS cgns_static ARCHIVE DESTINATION lib)
# Set the install path of the shared library
if(CGNS_BUILD_SHARED)
  # for windows, need to install both cgnsdll.dll and cgnsdll.lib
  if (WIN32 OR CYGWIN)
    install(TARGETS cgns_shared ARCHIVE DESTINATION lib)
    install(TARGETS cgns_shared RUNTIME DESTINATION lib)
  else (WIN32 OR CYGWIN)
    install(TARGETS cgns_shared LIBRARY DESTINATION lib)
  endif (WIN32 OR CYGWIN)
endif(CGNS_BUILD_SHARED)

# Set the install path of the header files
install(FILES
	cgnslib.h
	cgns_io.h
	cgnswin_f.h
	${CMAKE_CURRENT_BINARY_DIR}/cgnsconfig.h
	${CMAKE_CURRENT_BINARY_DIR}/cgnstypes.h
	${CMAKE_CURRENT_BINARY_DIR}/cgnstypes_f.h
	${CMAKE_CURRENT_BINARY_DIR}/cgnslib_f.h
	${CMAKE_CURRENT_BINARY_DIR}/cgnsBuild.defs
	DESTINATION include)

if (CGNS_ENABLE_PARALLEL)
  install(FILES	pcgnslib.h DESTINATION include)
endif (CGNS_ENABLE_PARALLEL)

if (CGNS_ENABLE_LEGACY)
  install(FILES adf/ADF.h DESTINATION include/adf)
  if (CGNS_ENABLE_HDF5)
    install(FILES adfh/ADFH.h DESTINATION include/adfh)
  endif (CGNS_ENABLE_HDF5)
endif (CGNS_ENABLE_LEGACY)

#########
# Tools #
#########

add_subdirectory(tools)

#########
# Tests #
#########

if (CGNS_ENABLE_TESTS)
  add_subdirectory(tests)
  if (CGNS_ENABLE_PARALLEL)
    add_subdirectory(ptests)
  endif (CGNS_ENABLE_PARALLEL)
endif (CGNS_ENABLE_TESTS)

##############
# CGNS Tools #
##############


set(CGNS_BUILD_CGNSTOOLS "OFF" CACHE BOOL "Build the CGNSTools package")
if(CGNS_BUILD_CGNSTOOLS)
  add_subdirectory(cgnstools)
endif(CGNS_BUILD_CGNSTOOLS)
