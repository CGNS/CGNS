#########
# Tests #
#########

# Link all the executables to cgns and hdf5
if(CGNS_BUILD_SHARED)
  #link_directories(.)
  set(Fortran_EXAMPLE_LK_LIBS cgns_shared)
else()
  set(Fortran_EXAMPLE_LK_LIBS cgns_static)
endif()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND Fortran_EXAMPLE_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND Fortran_EXAMPLE_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND Fortran_EXAMPLE_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND Fortran_EXAMPLE_LK_LIBS ${MPI_LIBS})
  endif()
endif ()

if (NOT WIN32)
  list(APPEND Fortran_EXAMPLE_LK_LIBS m)
endif ()

# Add a prefix to differentiate from C example's executables
set(PREFIX "Fortran_")

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
  add_executable (${PREFIX}${example} ${example}.F90)
  target_link_libraries(${PREFIX}${example} PRIVATE ${Fortran_EXAMPLE_LK_LIBS})
endforeach ()

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
#	write_discreteface_str
#	write_grid2zn_str
#	write_con2zn_str
#	write_grid2zn_str
#	write_con2zn_genrl_str
