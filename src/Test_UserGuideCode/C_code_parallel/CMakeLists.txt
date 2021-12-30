#########
# Tests #
#########

# Link all the executables to cgns and hdf5
if(CGNS_BUILD_SHARED)
  #link_directories(.)
  set(C_PARA_EXAMPLE_LK_LIBS cgns_shared)
else()
  set(C_PARA_EXAMPLE_LK_LIBS cgns_static)
endif()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND C_PARA_EXAMPLE_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND C_PARA_EXAMPLE_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND C_PARA_EXAMPLE_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND C_PARA_EXAMPLE_LK_LIBS ${MPI_LIBS})
  endif()
endif ()

if (NOT WIN32)
  list(APPEND C_PARA_EXAMPLE_LK_LIBS m)
endif ()

# Set the files needed by each test
set (examples
	write_grid_str_parinzone
	write_grid_str_paroverzone
	write_flowcentrind_str_parinzone
	write_flowcentrind_str_paroverzone
	read_grid_str_parinzone
	read_grid_str_paroverzone
	read_flowcentrind_str_parinzone
	read_flowcentrind_str_paroverzone
)

# Build each test
foreach (example ${examples})
  add_executable (${example} ${example}.c)
  target_link_libraries(${example} PRIVATE ${C_PARA_EXAMPLE_LK_LIBS})
  add_test(NAME test-${example}
           COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
           ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${example}> ${MPIEXEC_POSTFLAGS})
endforeach (example ${examples})
