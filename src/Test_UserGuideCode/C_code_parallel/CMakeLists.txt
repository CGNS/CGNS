#########
# Tests #
#########

# Link all the executables to cgns and hdf5
link_directories(.)
link_libraries(cgns_static)

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  link_libraries(${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    link_libraries(${ZLIB_LIBRARY})
  endif(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    link_libraries(${SZIP_LIBRARY})
  endif(HDF5_NEED_SZIP AND SZIP_LIBRARY)
  if(HDF5_NEED_MPI AND MPI_LIBS)
    link_libraries(${MPI_LIBS})
  endif(HDF5_NEED_MPI AND MPI_LIBS)
endif (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)

if (NOT WIN32)
  link_libraries(m)
endif (NOT WIN32)

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
endforeach (example ${examples})
