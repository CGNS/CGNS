cmake_minimum_required(VERSION 3.20...3.20)

# Function to handle muticonfig generator
function(set_cgns_build_type)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build.")
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
      "MinSizeRel" "RelWithDebInfo")
  get_property(_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
  if(_isMultiConfig)
    set(CGNS_BUILD_TYPE ${CMAKE_CFG_INTDIR} PARENT_SCOPE)
    set(CGNS_CFG_BUILD_TYPE \${CMAKE_INSTALL_CONFIG_NAME} PARENT_SCOPE)
  else()
    set(CGNS_CFG_BUILD_TYPE "." PARENT_SCOPE)
    set(CGNS_BUILD_TYPE ${CMAKE_BUILD_TYPE} PARENT_SCOPE)
  endif()
  set(CGNS_CFG_NAME ${CMAKE_BUILD_TYPE} PARENT_SCOPE)
endfunction()

set_cgns_build_type()

project("cgns" C)

include(GNUInstallDirs)

# Determine CGNS_VERSION from src/cgnslib.h for 
file (READ ${PROJECT_SOURCE_DIR}/src/cgnslib.h _cgnslib_h_contents)
string (REGEX REPLACE ".*#define[ \t]+CGNS_DOTVERS[ \t]+([0-9]*)\\.([0-9])[0-9]*.*$"
    "\\1.\\2" CGNS_VERSION ${_cgnslib_h_contents})

set(CGNS_PACKAGE "cgns")
set(CGNS_PACKAGE_NAME "CGNS")

# Allow for building a package
set(CPACK_PACKAGE_NAME "${CGNS_PACKAGE_NAME}")
set(CPACK_PACKAGE_VERSION "${CGNS_VERSION}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "cgns-${CGNS_VERSION}")
set(CPACK_GENERATOR "STGZ;TGZ;ZIP")
set(CPACK_SOURCE_GENERATOR "STGZ;TGZ;ZIP")
include(CPack)

CONFIGURE_FILE(
	"${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
	"${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
	IMMEDIATE @ONLY)

ADD_CUSTOM_TARGET(uninstall
	"${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake")

##############
# build mode #
##############

include(CheckIncludeFile)
check_include_file(stdint.h HAVE_STDINT_H)
check_include_file(inttypes.h HAVE_INTTYPES_H)
if (NOT HAVE_STDINT_H)
  message(FATAL_ERROR "stdint.h is not found")
endif ()
if (NOT HAVE_INTTYPES_H)
  message(FATAL_ERROR "inttypes.h is not found")
endif ()

# get some type sizes
include(CheckTypeSize)
CHECK_TYPE_SIZE(long SIZEOF_LONG)
CHECK_TYPE_SIZE(off_t SIZEOF_OFF_T)
CHECK_TYPE_SIZE("int64_t" SIZEOF_INT64_T)
CHECK_TYPE_SIZE("long long" SIZEOF_LONG_LONG)

if ("${SIZEOF_OFF_T}" LESS 8)
  option(CGNS_ENABLE_LFS "Enable or disable large file support" "OFF")
endif ()

if ("${CMAKE_SIZEOF_VOID_P}" GREATER 4)
  option(CGNS_ENABLE_64BIT "Enable or disable 64-bit code building" "ON")
else ()
  set(CGNS_ENABLE_64BIT "OFF")
endif ()

option(CGNS_ENABLE_LEGACY "Enable or disable building legacy code (3.0 compatible)" "OFF")
option(CGNS_ENABLE_SCOPING "Enable or disable scoping of enumeration values" "OFF")
option(CGNS_ENABLE_BASE_SCOPE "Enable or disable base scoped families or connectivities" "OFF")
option(CGNS_ENABLE_MEM_DEBUG "Enable or disable memory debugging" "OFF")

set(tools_install_suffix "/cgnstools")
if(WIN32)
  set(tools_install_suffix "")
endif()

set(CGNS_INSTALL_TOOLS_BINDIR "${CMAKE_INSTALL_BINDIR}${install_suffix}" CACHE PATH
    "CGNS: tools binary install location")
set(CGNS_INSTALL_TOOLS_DATADIR "${CMAKE_INSTALL_DATADIR}${install_suffix}" CACHE PATH
    "CGNS: arch independent tools data install location")

set(CGNS_BUILD_SHARED "ON" CACHE BOOL "Build a shared version of the library")
# useful to write cmake config.
set(CGNS_ENABLE_SHARED_LIB NO)
set(CGNS_ENABLE_STATIC_LIB YES)
if (CGNS_BUILD_SHARED)
  set(CGNS_ENABLE_SHARED_LIB YES)
endif()

if (CGNS_ENABLE_LEGACY)
  set(CGNS_ENABLE_64BIT "OFF")
  mark_as_advanced(FORCE CGNS_ENABLE_64BIT)
else ()
  mark_as_advanced(CLEAR CGNS_ENABLE_64BIT)
endif ()

if (CGNS_ENABLE_64BIT)
  set(CGNS_ENABLE_LEGACY "OFF")
  mark_as_advanced(FORCE CGNS_ENABLE_LEGACY)
else ()
  mark_as_advanced(CLEAR CGNS_ENABLE_LEGACY)
endif ()
	
if (CGNS_ENABLE_LFS)
  include(CheckSymbolExists)
  if (WIN32)
    # Windows does not have an _open64 function
    set(HAVE_OPEN64 0)
    check_symbol_exists(_lseeki64 "io.h" HAVE_LSEEK64)
  else ()
    check_symbol_exists(open64 "sys/types.h;sys/stat.h;unistd.h" HAVE_OPEN64)
    check_symbol_exists(lseek64 "sys/types.h;unistd.h" HAVE_LSEEK64)
  endif ()
  if (HAVE_OPEN64)
    add_compile_definitions(HAVE_OPEN64)
  endif ()
  if (HAVE_LSEEK64)
    add_compile_definitions(HAVE_LSEEK64)
  endif ()
else ()
  remove_definitions(-DHAVE_OPEN64 -DHAVE_LSEEK64)
endif ()

option(CGNS_ENABLE_TESTS "Enable test programs" "OFF")
if (CGNS_ENABLE_TESTS)
  enable_testing()
endif ()

###########
# Fortran #
###########

# Allow fortran support to be toggled
option(CGNS_ENABLE_FORTRAN "Enable or disable the use of Fortran" "OFF")
if (NOT DEFINED FORTRAN_NAMING)
  set(FORTRAN_NAMING "UNKNOWN")
endif ()

# If we are using fortran, enable it and configure the C-Fortran interface
# It would be nice to automatically detect this setting
if(CGNS_ENABLE_FORTRAN)
  enable_language("Fortran" OPTIONAL)

  #-----------------------------------------------------------------------------
  # Detect name mangling convention used between Fortran and C
  #-----------------------------------------------------------------------------
  set(F2CLIST  "LOWERCASE" "LOWERCASE_" "LOWERCASE__" "UPPERCASE" "UPPERCASE_" "UPPERCASE__")
  set(FORTRAN_NAMING_HELP
  "Configures how to link the Fortran components into the C library.
  Options are:
  	  LOWERCASE
	  LOWERCASE_
	  LOWERCASE__
	  UPPERCASE
	  UPPERRCASE_
	  UPPERRCASE__
  LOWERCASE_ is known to work with gfortran.  If this setting
  does not work with your compiler, try the others until one does.
  ")

  include(FortranCInterface)
  set(FortranCInterface_GLOBAL_SYMBOLS mysub mysub_ mysub__ MYSUB MYSUB_ MYSUB__)
  FortranCInterface_HEADER(${CMAKE_BINARY_DIR}/FCMangle.h 
  			   MACRO_NAMESPACE "CGNS_FC_"
  			   SYMBOL_NAMESPACE "CGNS_FC_")

  file (STRINGS ${CMAKE_BINARY_DIR}/FCMangle.h CONTENTS REGEX "CGNS_FC_GLOBAL\\(.*,.*\\) +(.*)")
  string (REGEX MATCH "CGNS_FC_GLOBAL\\(.*,.*\\) +(.*)" RESULT ${CONTENTS})
  set (CGNS_FC_FUNC "${CMAKE_MATCH_1}")

  file(REMOVE ${CMAKE_BINARY_DIR}/FCMangle.h)

  if(CGNS_FC_FUNC MATCHES "^NAME(.*)")
    set (FORTRAN_NAMING "UPPERCASE")
  elseif(CGNS_FC_FUNC MATCHES "^name(.*)")
    set (FORTRAN_NAMING "LOWERCASE")
  endif()
  if(CGNS_FC_FUNC MATCHES "(.*)__$")
    set (FORTRAN_NAMING "${FORTRAN_NAMING}__")
  elseif(CGNS_FC_FUNC MATCHES "(.*)_$")
    set (FORTRAN_NAMING "${FORTRAN_NAMING}_")
  endif()

  message(STATUS "Fortran name mangling convention: ${FORTRAN_NAMING}")

  # check that FORTRAN_NAMING is valid
  if (FORTRAN_NAMING STREQUAL "UNKNOWN")
    if (WIN32)
      set(FORTRAN_NAMING "UPPERCASE" CACHE STRING ${FORTRAN_NAMING_HELP})
    else ()
      set(FORTRAN_NAMING "LOWERCASE_" CACHE STRING ${FORTRAN_NAMING_HELP})
    endif ()
    if (${CMAKE_MAJOR_VERSION} GREATER 2 OR ${CMAKE_MINOR_VERSION} GREATER 7)
      set_property(CACHE FORTRAN_NAMING PROPERTY STRINGS ${F2CLIST})
    endif (${CMAKE_MAJOR_VERSION} GREATER 2 OR ${CMAKE_MINOR_VERSION} GREATER 7)
  else ()
    list(FIND F2CLIST "${FORTRAN_NAMING}" ListIndex)
    if (ListIndex LESS 0)
      message(SEND_ERROR "invalid FORTRAN_NAMING value. Must be empty or one of:
        LOWERCASE
        LOWERCASE_
        LOWERCASE__
        UPPERCASE
        UPPERRCASE_
        UPPERRCASE__")
    endif ()
  endif ()

  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10.0)
    SET (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
  endif()

  # Warn about a gfortran 10.2.0 bug (GCC Bug 100149) which
  # causes cg_goto_f to segfault, other versions are fine.
   if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND 
       CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER "10.1" AND 
       CMAKE_Fortran_COMPILER_VERSION VERSION_LESS "10.3")
    message (WARNING "Fortran mapping (cg_goto_f) is broken for the specified gfortran version.
                           !!! IT'S ADVISABLE TO AVOID VERSION 10.2 !!!")
  endif()

endif (CGNS_ENABLE_FORTRAN)

# this forces cmake to set up the required Fortran variables
if (CGNS_ENABLE_FORTRAN AND HAS_FORTRAN)
  enable_language("Fortran")
endif ()

########
# HDF5 #
########

option(CGNS_ENABLE_HDF5 "Enable or disable HDF5 interface" "ON")
if (CGNS_ENABLE_HDF5)

  if (CGNS_BUILD_SHARED)
    set (CG_HDF5_LINK_TYPE "shared")
    set (CG_HDF5_LINK_TYPE_UPPER "SHARED")
  else ()
    set (CG_HDF5_LINK_TYPE "static")
    set (CG_HDF5_LINK_TYPE_UPPER "STATIC")
    set(HDF5_USE_STATIC_LIBRARIES ON)
  endif ()
  set (FIND_HDF_COMPONENTS C ${CG_HDF5_LINK_TYPE})
  message (STATUS "HDF5 find comps: ${FIND_HDF_COMPONENTS}")

  set (SEARCH_PACKAGE_NAME "hdf5")

  find_package (HDF5 NAMES ${SEARCH_PACKAGE_NAME} COMPONENTS ${FIND_HDF_COMPONENTS})
  message (STATUS "HDF5 C libs:${HDF5_FOUND} static:${HDF5_static_C_FOUND} and shared:${HDF5_shared_C_FOUND}")
  if (HDF5_FOUND)
    if (NOT HDF5_static_C_FOUND AND NOT HDF5_shared_C_FOUND)
      if (CG_HDF5_LINK_TYPE STREQUAL "shared")
        set(CG_HDF5_LINK_TYPE "static")
        set(CG_HDF5_LINK_TYPE_UPPER "STATIC")
      else()
        set(CG_HDF5_LINK_TYPE "shared")
        set(CG_HDF5_LINK_TYPE_UPPER "SHARED")
      endif()

      set (FIND_HDF_COMPONENTS C ${CG_HDF5_LINK_TYPE})

      find_package (HDF5 NAMES ${SEARCH_PACKAGE_NAME} COMPONENTS ${FIND_HDF_COMPONENTS} REQUIRED)
      message (STATUS "HDF5 libs:${HDF5_FOUND} C:${HDF5_C_${CG_HDF5_LINK_TYPE_UPPER}_LIBRARY}")
    endif()
 
    set(LINK_LIBS ${HDF5_C_${CG_HDF5_LINK_TYPE_UPPER}_LIBRARY})
  else ()
    find_package (HDF5) # Legacy find
    
    #Legacy find_package does not set HDF5_TOOLS_DIR, so we set it here
    set(HDF5_TOOLS_DIR ${HDF5_LIBRARY}/../bin)
    
    #Legacy find_package does not set HDF5_BUILD_SHARED_LIBS, so we set it here
    set(GUESS_SHARED "${HDF5_LIBRARY}/libhdf5${CMAKE_SHARED_LIBRARY_SUFFIX}")
    foreach (ITEM ${HDF5_LIBRARIES})
      if (ITEM MATCHES "(.*)hdf5${CMAKE_SHARED_LIBRARY_SUFFIX}")
        set(GUESS_SHARED "${ITEM}")
        break()
      endif()
    endforeach()
    if (CGNS_BUILD_SHARED AND EXISTS "${GUESS_SHARED}")
      set (HDF5_BUILD_SHARED_LIBS 1)
      set (CG_HDF5_LINK_TYPE "shared")
      set (CG_HDF5_LINK_TYPE_UPPER "SHARED")
    else ()
      set (HDF5_BUILD_SHARED_LIBS 0)
      set (CG_HDF5_LINK_TYPE "static")
      set (CG_HDF5_LINK_TYPE_UPPER "STATIC")
    endif ()
    set (LINK_LIBS ${HDF5_LIBRARIES})
  endif ()
  set (HDF5_PACKAGE_NAME ${SEARCH_PACKAGE_NAME})

  if (HDF5_FOUND)
    if (NOT DEFINED HDF5_INCLUDE_DIRS)
      set (HDF5_INCLUDE_DIRS ${HDF5_INCLUDE_DIR})
    endif()
    set (HDF5_HAVE_H5PUBCONF_H 1)
    set (HDF5_HAVE_HDF5 1)
    set (HDF5_HDF5_HEADER "h5pubconf.h")
    message (STATUS "HDF5-${HDF5_VERSION} found: INC=${HDF5_INCLUDE_DIRS} TOOLS=${HDF5_TOOLS_DIR} HDF5_BUILD_SHARED_LIBS=${HDF5_BUILD_SHARED_LIBS}")
    set (HDF5_LIBRARY ${LINK_LIBS})
    message (STATUS "HDF5 link libs: ${LINK_LIBS}")
  else ()
    message (FATAL_ERROR " HDF5 was specified but was not found")
  endif ()

  set(HDF5_NEED_ZLIB "OFF" CACHE BOOL "Does the HDF5 library require linking to zlib?")
  if (HDF5_NEED_ZLIB)
    # Find ZLIB package to create ZLIB::ZLIB target for modern CMake compatibility
    find_package(ZLIB QUIET)
    # Fallback to finding the library directly if find_package failed.
    # Note: do not FATAL_ERROR here — ZLIB may be bundled with HDF5 (on any
    # platform when HDF5 is built with CMake). The auto-detection from HDF5's
    # INTERFACE_LINK_LIBRARIES (further below) will resolve it.
    if (NOT ZLIB_FOUND)
      find_library(ZLIB_LIBRARY z)
    endif ()
    mark_as_advanced(CLEAR ZLIB_LIBRARY)
  else ()
    mark_as_advanced(FORCE ZLIB_LIBRARY)
  endif ()

  set(HDF5_NEED_SZIP "OFF" CACHE BOOL "Does the HDF5 library require linking to szip?")
  if (HDF5_NEED_SZIP)
    # Try to find libaec package first for modern CMake compatibility
    find_package(libaec QUIET)
    if (libaec_FOUND AND TARGET libaec::sz)
      # Extract library location from the imported target for legacy compatibility.
      # SZIP_LIBRARY must be a file path (not a target name) because downstream
      # consumers use it with native_paths(), target_link_libraries(${SZIP_LIBRARY}),
      # and other contexts that expect a path string.
      # Try to resolve the specific config to avoid linking Release lib in Debug build.
      get_target_property(_libaec_configs libaec::sz IMPORTED_CONFIGURATIONS)

      set(_chosen_config "")
      if (_libaec_configs)
        # 1. Try to match the current CMAKE_BUILD_TYPE (if single-config generator)
        if (CMAKE_BUILD_TYPE)
          string(TOUPPER "${CMAKE_BUILD_TYPE}" _build_type_upper)
          if (_build_type_upper IN_LIST _libaec_configs)
            set(_chosen_config "${_build_type_upper}")
          endif ()
        endif ()

        # 2. If no match (or multi-config), fallback to the first available
        if (NOT _chosen_config)
          list(GET _libaec_configs 0 _chosen_config)
        endif ()

        get_target_property(_sz_loc libaec::sz IMPORTED_LOCATION_${_chosen_config})
      endif ()

      # Fallback to config-agnostic IMPORTED_LOCATION
      if (NOT _sz_loc)
        get_target_property(_sz_loc libaec::sz IMPORTED_LOCATION)
      endif ()

      if (_sz_loc)
        set(SZIP_LIBRARY "${_sz_loc}" CACHE FILEPATH "Path to szip library" FORCE)
      endif ()
      unset(_sz_loc)
      unset(_libaec_configs)
      unset(_chosen_config)
      unset(_build_type_upper)
    endif ()
    # Fallback to finding the library directly
    if (NOT SZIP_LIBRARY)
      find_library(SZIP_LIBRARY NAMES szip sz)
    endif ()
    mark_as_advanced(CLEAR SZIP_LIBRARY)
  else ()
    mark_as_advanced(FORCE SZIP_LIBRARY)
  endif ()

  # Check if HDF5 version is 1.8 or greater
  if (HDF5_VERSION VERSION_LESS "1.8.0")
    message (FATAL_ERROR "HDF5 ${HDF5_VERSION}: version must be greater than 1.8.0")
  endif ()

  set(HDF5_NEED_MPI "OFF" CACHE BOOL "Does the HDF5 library require linking to mpi? (Only true if using parallel HDF5)")
  set(MPI_INC)
  set(MPI_LIBS)
  if (HDF5_NEED_MPI)
    if (NOT MPI_FOUND)
      include(FindMPI)
    endif ()
    # MPI variable names changed with cmake 2.8.5
    if (DEFINED MPI_C_COMPILER)
      mark_as_advanced(CLEAR MPI_C_COMPILER MPI_C_INCLUDE_PATH MPI_C_LIBRARIES)
      mark_as_advanced(FORCE MPI_LIBRARY MPI_EXTRA_LIBRARY)
      set(MPI_INC ${MPI_C_INCLUDE_PATH})
      set(MPI_LIBS ${MPI_C_LIBRARIES})
    else ()
      mark_as_advanced(CLEAR MPI_COMPILER MPI_INCLUDE_PATH MPI_LIBRARY MPI_EXTRA_LIBRARY)
      set(MPI_INC ${MPI_INCLUDE_PATH})
      if (MPI_EXTRA_LIBRARY)
        set(MPI_LIBS ${MPI_LIBRARY} ${MPI_EXTRA_LIBRARY})
      else ()
        set(MPI_LIBS ${MPI_LIBRARY})
      endif ()
    endif ()
    mark_as_advanced(CLEAR MPIEXEC)
  else ()
    mark_as_advanced(FORCE MPI_C_COMPILER MPI_C_INCLUDE_PATH MPI_C_LIBRARIES)
    mark_as_advanced(FORCE MPI_COMPILER MPI_INCLUDE_PATH MPI_LIBRARY MPI_EXTRA_LIBRARY MPIEXEC)
  endif ()
  mark_as_advanced(CLEAR HDF5_NEED_ZLIB HDF5_NEED_SZIP HDF5_NEED_MPI)

  #Modern Target Library import if not defined
  if (NOT TARGET hdf5-${CG_HDF5_LINK_TYPE})
    add_library(hdf5-${CG_HDF5_LINK_TYPE} INTERFACE IMPORTED)
    string(REPLACE "-D" "" _hdf5_definitions "${HDF5_DEFINITIONS}")
    set_target_properties(hdf5-${CG_HDF5_LINK_TYPE} PROPERTIES
                          INTERFACE_LINK_LIBRARIES "${HDF5_LIBRARY}"
                          INTERFACE_INCLUDE_DIRECTORIES "${HDF5_INCLUDE_DIRS}"
                          INTERFACE_COMPILE_DEFINITIONS "${_hdf5_definitions}")
    if (CG_HDF5_LINK_TYPE STREQUAL "shared")
      set_target_properties(hdf5-${CG_HDF5_LINK_TYPE} PROPERTIES
                            INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB)

    else()
      set_target_properties(hdf5-${CG_HDF5_LINK_TYPE} PROPERTIES
			    INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_STATIC_LIB)
    endif()
    # User provided information about HDF5 requirements.
    if (HDF5_NEED_ZLIB)
      set(HDF5_ENABLE_ZLIB_SUPPORT ON)
      # HDF5_PACKAGE_EXTLIBS ON prevents the check below from calling find_package(ZLIB)
      # since we already called it above when HDF5_NEED_ZLIB was set
      set(HDF5_PACKAGE_EXTLIBS ON)
    endif()
    if (HDF5_NEED_SZIP)
      set(HDF5_ENABLE_SZIP_SUPPORT ON)
      # Same for SZIP - we already attempted find_package(libaec) above
      set(HDF5_PACKAGE_EXTLIBS ON)
    endif()
  endif ()

  # Check the actual HDF5 target's link interface to detect compression dependencies.
  # This inspects only the target we are actually linking against:
  # - For hdf5-static: zlib/szip must be resolved explicitly (not baked in)
  # - For hdf5-shared: normally no deps in interface, but handles broken configs
  #   or Windows builds where ZLIB::ZLIB may still appear
  if (TARGET hdf5-${CG_HDF5_LINK_TYPE})
    get_target_property(_hdf5_link_libs hdf5-${CG_HDF5_LINK_TYPE} INTERFACE_LINK_LIBRARIES)
    if (_hdf5_link_libs)
      # Normalize with leading/trailing semicolons so delimited patterns always match
      set(_hdf5_link_str ";${_hdf5_link_libs};")
      # Detect ZLIB::ZLIB in link interface — matches both plain references
      # and generator expressions like $<LINK_ONLY:ZLIB::ZLIB>
      # Check for the target explicitly rather than ZLIB_FOUND — a parent project
      # may have called find_package(ZLIB) in legacy mode (variables only) without
      # creating the ZLIB::ZLIB imported target that HDF5's interface requires.
      if (_hdf5_link_str MATCHES "ZLIB::ZLIB" AND NOT TARGET ZLIB::ZLIB)
        message(STATUS "hdf5-${CG_HDF5_LINK_TYPE} target requires ZLIB::ZLIB, finding ZLIB package")
        # Try module mode first — CMake's FindZLIB always creates ZLIB::ZLIB.
        # Config-mode (ZLIBConfig.cmake from a CMake install of zlib) may export
        # different target names (ZLIB::zlib, ZLIB::zlibstatic) without ZLIB::ZLIB.
        find_package(ZLIB MODULE QUIET)
        if (NOT TARGET ZLIB::ZLIB)
          # Fallback to config-mode in case module mode failed
          find_package(ZLIB QUIET)
        endif ()
        if (NOT TARGET ZLIB::ZLIB)
          if (ZLIB_FOUND AND ZLIB_LIBRARIES)
            # ZLIB was found (config-mode) but without ZLIB::ZLIB — create it
            add_library(ZLIB::ZLIB UNKNOWN IMPORTED)
            set_target_properties(ZLIB::ZLIB PROPERTIES
              IMPORTED_LOCATION "${ZLIB_LIBRARIES}"
              INTERFACE_INCLUDE_DIRECTORIES "${ZLIB_INCLUDE_DIRS}")
          else ()
            message(FATAL_ERROR "hdf5-${CG_HDF5_LINK_TYPE} requires ZLIB::ZLIB but ZLIB was not found")
          endif ()
        endif ()
      endif ()
      # Detect SZIP/libaec in link interface — matches target names like
      # szip-static, libaec::sz, $<LINK_ONLY:szip-static>, or bare sz/szip from
      # older HDF5 installations. CMake regex does not support \b, so we use
      # ;-delimited alternation. The string is normalized with leading/trailing
      # semicolons so these patterns always match at element boundaries.
      # Check for the target explicitly rather than libaec_FOUND for the same reason.
      if (_hdf5_link_str MATCHES ";szip-|;szip;|;sz;|;sz-|;libaec" AND NOT TARGET libaec::sz)
        message(STATUS "hdf5-${CG_HDF5_LINK_TYPE} target requires SZIP, finding libaec package")
        find_package(libaec QUIET)
        if (NOT libaec_FOUND)
          find_library(SZIP_LIBRARY NAMES szip sz)
        endif ()
      endif ()
    endif ()
    unset(_hdf5_link_libs)
    unset(_hdf5_link_str)
  endif ()

else ()
  mark_as_advanced(FORCE HDF5_NEED_ZLIB HDF5_NEED_SZIP HDF5_NEED_MPI)
  mark_as_advanced(FORCE ZLIB_LIBRARY SZIP_LIBRARY)
  mark_as_advanced(FORCE MPI_C_COMPILER MPI_C_INCLUDE_PATH MPI_C_LIBRARIES)
  mark_as_advanced(FORCE MPI_COMPILER MPI_INCLUDE_PATH MPI_LIBRARY MPI_EXTRA_LIBRARY MPIEXEC)
endif ()

#################
# Parallel CGNS #
#################

if (CGNS_ENABLE_HDF5 AND HDF5_NEED_MPI)
  set(CGNS_ENABLE_PARALLEL "OFF" CACHE BOOL "Enable or disable parallel interface ?")
  mark_as_advanced(CLEAR CGNS_ENABLE_PARALLEL)
  # Check that HDF5 has parallel support
  if (NOT (HDF5_IS_PARALLEL OR HDF5_ENABLE_PARALLEL OR HDF5_PROVIDES_PARALLEL))
    message(FATAL_ERROR "HDF5 has been found, but is missing parallel support.")
  endif()
else ()
  set(CGNS_ENABLE_PARALLEL "OFF")
  mark_as_advanced(FORCE CGNS_ENABLE_PARALLEL)
endif ()

####################################
# Display CMake HDF5 configuration #
####################################
if ((NOT CGNS_BUILD_SHARED) AND CGNS_ENABLE_HDF5 AND (CG_HDF5_LINK_TYPE STREQUAL "shared"))
  message(STATUS "")
  message(STATUS "=== CGNS Build Configuration Notice ===")
  message(STATUS "CGNS: Building static CGNS  library with shared HDF5")
  message(STATUS "Applications using CGNS will need:")
  message(STATUS "  1. HDF5 shared libraries available at runtime")
  message(STATUS "  2. Proper RPATH/LD_LIBRARY_PATH configuration")
  message(STATUS "  3. find_package(CGNS::cgns_static) in the application's CMakeLists.txt")
  message(STATUS "=====================================")
  message(STATUS "")
endif()

####################
# RPATH Management #
####################

if (CGNS_BUILD_SHARED)
  # use, i.e. don't skip the full RPATH for the build tree
  set(CMAKE_SKIP_BUILD_RPATH  FALSE)

  # when building, don't use the install RPATH already
  # (but later on when installing)
  set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

  # the RPATH to be used when installing
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}; ${CMAKE_INSTALL_PREFIX}/lib")

  # add the automatically determined parts of the RPATH
  # which point to directories outside the build tree to the install RPATH
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_MACOSX_RPATH TRUE)
  endif()
else()
  set(CMAKE_SKIP_RPATH TRUE)
endif()

#-----------------------------------------------------------------------------
# Dashboard and Testing Settings
#-----------------------------------------------------------------------------
option (CGNS_BUILD_TESTING "Build CGNS Testing" OFF)
if (CGNS_BUILD_TESTING)
  set (DART_TESTING_TIMEOUT "600"
      CACHE STRING
      "Timeout in seconds for each test (default 600=10minutes)"
  )
  enable_testing ()
  include (CTest)
  include (${PROJECT_SOURCE_DIR}/CTestConfig.cmake)
endif ()

########
# CGNS #
########

# Include the src directory
add_subdirectory(src)
