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
	write_grid_str
	write_bc_str
	write_bcpnts_str
	write_bcpnts_unst
	write_con2zn_str
	write_con2zn_genrl_str
	write_convergence
	write_descriptor
	write_dimensional
	write_flowcent_str
	write_flowcentrind_str
	write_floweqn_str
	write_flowvert_str
	write_flowvert_unst
	write_grid2zn_str
	write_grid_unst
	write_nondimensional
	write_timevert_str
	read_bc_str
	read_bcpnts_str
	read_bcpnts_unst
	read_con2zn_genrl_str
	read_con2zn_str
	read_convergence
	read_descriptor
	read_dimensional
	read_flowcent_str
	read_flowcentrind_str
	read_floweqn_str
	read_flowvert_str
	read_flowvert_unst
	read_grid2zn_str
	read_grid_str
	read_grid_unst
	read_nondimensional
	read_timevert_str
)

# Build each test
foreach (example ${examples})
  add_executable (${example} ${example}.c)
endforeach (example ${examples})

# Add the tests so that ctest can find them
#if (CGNS_ENABLE_TESTS)
#	foreach (example ${examples})
#	  add_test (${example} ${example})
#	endforeach (example ${examples})
#endif (CGNS_ENABLE_TESTS)
#	write_grid_str
#	write_flowvert_str
#	write_nondimensional
#	write_descriptor
#	write_convergence
#	write_floweqn_str
#	write_bcpnts_str
#	write_grid_unst
#	write_flowvert_unst
#	write_dimensional
#	write_descriptor
#	write_convergence
#	write_bcpnts_unst
#	write_grid_str
#	write_timevert_str
#	write_grid_str
#	write_flowcent_str
#	write_bc_str
#	write_grid_str
#	write_flowcentrind_str
#	write_grid2zn_str
#	write_con2zn_str
#	write_grid2zn_str
#	write_con2zn_genrl_str