#########
# Tools #
#########

# Link all the executables to cgns and hdf5
if (CGNS_USE_SHARED)
  set(TOOLS_LK_LIBS cgns_shared)
  #if (WIN32 OR CYGWIN)
  #  target_compile_definitions(cgns_shared PRIVATE -DBUILD_DLL)
  #  target_compile_definitions(cgns_shared INTERFACE -DUSE_DLL)
  #endif ()
else ()
  set(TOOLS_LK_LIBS cgns_static)
endif ()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND TOOLS_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND TOOLS_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND TOOLS_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND TOOLS_LK_LIBS ${MPI_LIBS})
  endif()
endif ()

if (NOT WIN32)
  list(APPEND TOOLS_LK_LIBS m)
endif ()

# Set the files needed by each tool
set(cgnsnames_FILES
	cgnsnames.c
	cgnames.c)
set(cgnscheck_FILES
	cgnscheck.c
	getargs.c
	hash.c
	cgnames.c)
set(cgnsconvert_FILES
	cgnsconvert.c
	getargs.c)
set(cgnsdiff_FILES
	cgnsdiff.c
	getargs.c)
set(cgnslist_FILES
	cgnslist.c
	getargs.c)
set(cgnscompress_FILES
	cgnscompress.c)

# Build each tool
add_executable( cgnsnames    ${cgnsnames_FILES}   )
add_executable( cgnscheck    ${cgnscheck_FILES}   )
add_executable( cgnsconvert  ${cgnsconvert_FILES} )
add_executable( cgnsdiff     ${cgnsdiff_FILES}    )
add_executable( cgnslist     ${cgnslist_FILES}    )
add_executable( cgnscompress ${cgnscompress_FILES} )

set(TOOLS_LIST cgnsnames cgnscheck cgnsconvert cgnsdiff cgnslist cgnscompress)
# Tools linking
foreach(EXENAME ${TOOLS_LIST})
  target_link_libraries(${EXENAME} PRIVATE ${TOOLS_LK_LIBS})
endforeach()

install(TARGETS
	cgnscheck
	cgnsconvert
	cgnsdiff
	cgnslist
	cgnsnames
	cgnscompress
	RUNTIME DESTINATION bin)

if (WIN32)
  install(PROGRAMS
        cgnsupdate.bat
	DESTINATION bin)
  if (CGNS_ENABLE_HDF5)
    install(PROGRAMS
        adf2hdf.bat
        hdf2adf.bat
	DESTINATION bin)
  endif ()
else ()
  install(PROGRAMS
        cgnsupdate
	DESTINATION bin)
  if (CGNS_ENABLE_HDF5)
    install(PROGRAMS
        adf2hdf
        hdf2adf
	DESTINATION bin)
  endif ()
endif ()

