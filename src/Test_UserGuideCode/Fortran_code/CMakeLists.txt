#########
# Tests #
#########

# Link all the executables to cgns and hdf5
if(CGNS_BUILD_SHARED)
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
# Tests must run in specific order based on file dependencies (see test.sh)
if (CGNS_ENABLE_TESTS)
	foreach (example ${examples})
	  add_test (NAME Fortran_code_${example} COMMAND ${PREFIX}${example})
	  # On windows help ctest to find the required dlls
	  if(CGNS_BUILD_SHARED)
	    set_tests_properties(Fortran_code_${example} PROPERTIES ENVIRONMENT_MODIFICATION
	      "PATH=path_list_prepend:$<$<BOOL:${WIN32}>:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${PREFIX}${example}>,\;>>")
	  endif()
	endforeach ()

	# Test_Grid_Str group - sequential dependencies and shared working directory
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_write_grid_str PROPERTIES WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_write_flowvert_str PROPERTIES DEPENDS Fortran_code_write_grid_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_write_nondimensional PROPERTIES DEPENDS Fortran_code_write_flowvert_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_write_descriptor PROPERTIES DEPENDS Fortran_code_write_nondimensional WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_write_convergence PROPERTIES DEPENDS Fortran_code_write_descriptor WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_write_floweqn_str PROPERTIES DEPENDS Fortran_code_write_convergence WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_write_bcpnts_str PROPERTIES DEPENDS Fortran_code_write_floweqn_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	# Read tests depend on corresponding write tests
	set_tests_properties(Fortran_code_read_grid_str PROPERTIES DEPENDS Fortran_code_write_grid_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_read_flowvert_str PROPERTIES DEPENDS Fortran_code_write_flowvert_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_read_nondimensional PROPERTIES DEPENDS Fortran_code_write_nondimensional WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_read_descriptor PROPERTIES DEPENDS Fortran_code_write_descriptor WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_read_convergence PROPERTIES DEPENDS Fortran_code_write_convergence WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_read_floweqn_str PROPERTIES DEPENDS Fortran_code_write_floweqn_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)
	set_tests_properties(Fortran_code_read_bcpnts_str PROPERTIES DEPENDS Fortran_code_write_bcpnts_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str)

	# Test_Grid_Unstr group - sequential dependencies and shared working directory
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)
	set_tests_properties(Fortran_code_write_grid_unst PROPERTIES WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)
	set_tests_properties(Fortran_code_write_flowvert_unst PROPERTIES DEPENDS Fortran_code_write_grid_unst WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)
	set_tests_properties(Fortran_code_write_dimensional PROPERTIES DEPENDS Fortran_code_write_flowvert_unst WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)
	set_tests_properties(Fortran_code_write_bcpnts_unst PROPERTIES DEPENDS Fortran_code_write_grid_unst WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)
	# Read tests depend on corresponding write tests
	set_tests_properties(Fortran_code_read_grid_unst PROPERTIES DEPENDS Fortran_code_write_grid_unst WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)
	set_tests_properties(Fortran_code_read_flowvert_unst PROPERTIES DEPENDS Fortran_code_write_flowvert_unst WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)
	set_tests_properties(Fortran_code_read_dimensional PROPERTIES DEPENDS Fortran_code_write_dimensional WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)
	set_tests_properties(Fortran_code_read_bcpnts_unst PROPERTIES DEPENDS Fortran_code_write_bcpnts_unst WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Unstr)

	# Test_Grid_Str_Timeacc group - separate working directory with grid setup fixture
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_Timeacc)
	add_test(NAME Fortran_code_Test_Grid_Str_Timeacc_setup COMMAND ${PREFIX}write_grid_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_Timeacc)
	set_tests_properties(Fortran_code_Test_Grid_Str_Timeacc_setup PROPERTIES FIXTURES_SETUP Test_Grid_Str_Timeacc_fixture)
	set_tests_properties(Fortran_code_write_timevert_str PROPERTIES FIXTURES_REQUIRED Test_Grid_Str_Timeacc_fixture WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_Timeacc)
	set_tests_properties(Fortran_code_read_timevert_str PROPERTIES DEPENDS Fortran_code_write_timevert_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_Timeacc)

	# Test_Grid_Str_FlowCent group - separate working directory with grid setup fixture
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCent)
	add_test(NAME Fortran_code_Test_Grid_Str_FlowCent_setup COMMAND ${PREFIX}write_grid_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCent)
	set_tests_properties(Fortran_code_Test_Grid_Str_FlowCent_setup PROPERTIES FIXTURES_SETUP Test_Grid_Str_FlowCent_fixture)
	set_tests_properties(Fortran_code_write_flowcent_str PROPERTIES FIXTURES_REQUIRED Test_Grid_Str_FlowCent_fixture WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCent)
	set_tests_properties(Fortran_code_write_bc_str PROPERTIES DEPENDS Fortran_code_write_flowcent_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCent)
	set_tests_properties(Fortran_code_read_flowcent_str PROPERTIES DEPENDS Fortran_code_write_flowcent_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCent)
	set_tests_properties(Fortran_code_read_bc_str PROPERTIES DEPENDS Fortran_code_write_bc_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCent)

	# Test_Grid_Str_FlowCentRind group - separate working directory with grid setup fixture
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCentRind)
	add_test(NAME Fortran_code_Test_Grid_Str_FlowCentRind_setup COMMAND ${PREFIX}write_grid_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCentRind)
	set_tests_properties(Fortran_code_Test_Grid_Str_FlowCentRind_setup PROPERTIES FIXTURES_SETUP Test_Grid_Str_FlowCentRind_fixture)
	set_tests_properties(Fortran_code_write_flowcentrind_str PROPERTIES FIXTURES_REQUIRED Test_Grid_Str_FlowCentRind_fixture WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCentRind)
	set_tests_properties(Fortran_code_read_flowcentrind_str PROPERTIES DEPENDS Fortran_code_write_flowcentrind_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_FlowCentRind)

	# Test_Grid_Str_2zn group - separate working directory
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zn)
	set_tests_properties(Fortran_code_write_grid2zn_str PROPERTIES WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zn)
	set_tests_properties(Fortran_code_write_con2zn_str PROPERTIES DEPENDS Fortran_code_write_grid2zn_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zn)
	set_tests_properties(Fortran_code_read_grid2zn_str PROPERTIES DEPENDS Fortran_code_write_grid2zn_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zn)
	set_tests_properties(Fortran_code_read_con2zn_str PROPERTIES DEPENDS Fortran_code_write_con2zn_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zn)

	# Test_Grid_Str_2zngenrl group - separate working directory (needs separate write_grid2zn_str instance)
	file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zngenrl)
	add_test(NAME Fortran_code_Test_Grid_Str_2zngenrl_setup COMMAND ${PREFIX}write_grid2zn_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zngenrl)
	set_tests_properties(Fortran_code_Test_Grid_Str_2zngenrl_setup PROPERTIES FIXTURES_SETUP Test_Grid_Str_2zngenrl_fixture)
	set_tests_properties(Fortran_code_write_con2zn_genrl_str PROPERTIES FIXTURES_REQUIRED Test_Grid_Str_2zngenrl_fixture WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zngenrl)
	set_tests_properties(Fortran_code_read_con2zn_genrl_str PROPERTIES DEPENDS Fortran_code_write_con2zn_genrl_str WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Test_Grid_Str_2zngenrl)

	# Add Windows DLL path support for fixture tests
	if(CGNS_BUILD_SHARED AND WIN32)
		set_tests_properties(Fortran_code_Test_Grid_Str_Timeacc_setup PROPERTIES ENVIRONMENT_MODIFICATION
			"PATH=path_list_prepend:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${PREFIX}write_grid_str>,\;>")
		set_tests_properties(Fortran_code_Test_Grid_Str_FlowCent_setup PROPERTIES ENVIRONMENT_MODIFICATION
			"PATH=path_list_prepend:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${PREFIX}write_grid_str>,\;>")
		set_tests_properties(Fortran_code_Test_Grid_Str_FlowCentRind_setup PROPERTIES ENVIRONMENT_MODIFICATION
			"PATH=path_list_prepend:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${PREFIX}write_grid_str>,\;>")
		set_tests_properties(Fortran_code_Test_Grid_Str_2zngenrl_setup PROPERTIES ENVIRONMENT_MODIFICATION
			"PATH=path_list_prepend:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${PREFIX}write_grid2zn_str>,\;>")
	endif()

endif (CGNS_ENABLE_TESTS)
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
