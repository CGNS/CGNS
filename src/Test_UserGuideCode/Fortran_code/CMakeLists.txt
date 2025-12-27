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

	# Helper function to setup test group with sequential write chain and corresponding read tests
	function(setup_fortran_test_group GROUP_NAME WRITE_CHAIN)
		set(WORK_DIR ${CMAKE_CURRENT_BINARY_DIR}/${GROUP_NAME})
		set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/${GROUP_NAME})
		file(MAKE_DIRECTORY ${WORK_DIR})

		# Copy OUTPUT reference files to build directory
		file(GLOB OUTPUT_FILES "${SRC_DIR}/OUTPUT*")
		foreach(OUTPUT_FILE ${OUTPUT_FILES})
			file(COPY ${OUTPUT_FILE} DESTINATION ${WORK_DIR})
		endforeach()

		set(PREV_TEST "")
		set(OUTPUT_NUM 1)
		foreach(TEST_NAME ${WRITE_CHAIN})
			# Set working directory and dependency for write test
			if("${PREV_TEST}" STREQUAL "")
				set_tests_properties(Fortran_code_${TEST_NAME} PROPERTIES WORKING_DIRECTORY ${WORK_DIR})
			else()
				set_tests_properties(Fortran_code_${TEST_NAME} PROPERTIES DEPENDS Fortran_code_${PREV_TEST} WORKING_DIRECTORY ${WORK_DIR})
			endif()

			# Setup corresponding read test with output verification
			string(REPLACE "write_" "read_" READ_TEST_NAME ${TEST_NAME})
			set_tests_properties(Fortran_code_${READ_TEST_NAME} PROPERTIES
				DEPENDS Fortran_code_${TEST_NAME}
				WORKING_DIRECTORY ${WORK_DIR})

			# Replace test command with verification script
			set_property(TEST Fortran_code_${READ_TEST_NAME} PROPERTY COMMAND
				${CMAKE_COMMAND}
				-DTEST_PROGRAM=$<TARGET_FILE:${PREFIX}${READ_TEST_NAME}>
				-DREF_OUTPUT=${WORK_DIR}/OUTPUT${OUTPUT_NUM}
				-DWORK_DIR=${WORK_DIR}
				-P ${CMAKE_CURRENT_SOURCE_DIR}/../run_test_with_output_check.cmake)

			set(PREV_TEST ${TEST_NAME})
			math(EXPR OUTPUT_NUM "${OUTPUT_NUM} + 1")
		endforeach()
	endfunction()

	# Helper function to setup fixture-based test with grid setup
	function(setup_fortran_fixture_test GROUP_NAME SETUP_EXE WRITE_TESTS)
		set(WORK_DIR ${CMAKE_CURRENT_BINARY_DIR}/${GROUP_NAME})
		set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/${GROUP_NAME})
		file(MAKE_DIRECTORY ${WORK_DIR})

		# Copy OUTPUT reference files to build directory
		file(GLOB OUTPUT_FILES "${SRC_DIR}/OUTPUT*")
		foreach(OUTPUT_FILE ${OUTPUT_FILES})
			file(COPY ${OUTPUT_FILE} DESTINATION ${WORK_DIR})
		endforeach()

		# Create fixture setup test
		add_test(NAME Fortran_code_${GROUP_NAME}_setup COMMAND ${PREFIX}${SETUP_EXE} WORKING_DIRECTORY ${WORK_DIR})
		set_tests_properties(Fortran_code_${GROUP_NAME}_setup PROPERTIES FIXTURES_SETUP ${GROUP_NAME}_fixture)

		# Add Windows DLL support if needed
		if(CGNS_BUILD_SHARED AND WIN32)
			set_tests_properties(Fortran_code_${GROUP_NAME}_setup PROPERTIES ENVIRONMENT_MODIFICATION
				"PATH=path_list_prepend:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${PREFIX}${SETUP_EXE}>,\;>")
		endif()

		# Setup write tests that require the fixture
		list(GET WRITE_TESTS 0 FIRST_WRITE)
		set_tests_properties(Fortran_code_${FIRST_WRITE} PROPERTIES FIXTURES_REQUIRED ${GROUP_NAME}_fixture WORKING_DIRECTORY ${WORK_DIR})

		# Setup dependency chain for remaining write tests and all read tests
		set(PREV_TEST ${FIRST_WRITE})
		list(LENGTH WRITE_TESTS NUM_TESTS)
		if(NUM_TESTS GREATER 1)
			list(SUBLIST WRITE_TESTS 1 -1 REMAINING_WRITES)
			foreach(TEST_NAME ${REMAINING_WRITES})
				set_tests_properties(Fortran_code_${TEST_NAME} PROPERTIES DEPENDS Fortran_code_${PREV_TEST} WORKING_DIRECTORY ${WORK_DIR})
				set(PREV_TEST ${TEST_NAME})
			endforeach()
		endif()

		# Setup all corresponding read tests with output verification
		set(OUTPUT_NUM 1)
		foreach(TEST_NAME ${WRITE_TESTS})
			string(REPLACE "write_" "read_" READ_TEST_NAME ${TEST_NAME})
			set_tests_properties(Fortran_code_${READ_TEST_NAME} PROPERTIES
				DEPENDS Fortran_code_${TEST_NAME}
				WORKING_DIRECTORY ${WORK_DIR})

			# Replace test command with verification script
			set_property(TEST Fortran_code_${READ_TEST_NAME} PROPERTY COMMAND
				${CMAKE_COMMAND}
				-DTEST_PROGRAM=$<TARGET_FILE:${PREFIX}${READ_TEST_NAME}>
				-DREF_OUTPUT=${WORK_DIR}/OUTPUT${OUTPUT_NUM}
				-DWORK_DIR=${WORK_DIR}
				-P ${CMAKE_CURRENT_SOURCE_DIR}/../run_test_with_output_check.cmake)

			math(EXPR OUTPUT_NUM "${OUTPUT_NUM} + 1")
		endforeach()
	endfunction()

	# Test_Grid_Str group
	setup_fortran_test_group("Test_Grid_Str" "write_grid_str;write_flowvert_str;write_nondimensional;write_descriptor;write_convergence;write_floweqn_str;write_bcpnts_str")

	# Test_Grid_Unstr group
	setup_fortran_test_group("Test_Grid_Unstr" "write_grid_unst;write_flowvert_unst;write_dimensional;write_bcpnts_unst")

	# Fixture-based test groups
	setup_fortran_fixture_test("Test_Grid_Str_Timeacc" "write_grid_str" "write_timevert_str")
	setup_fortran_fixture_test("Test_Grid_Str_FlowCent" "write_grid_str" "write_flowcent_str;write_bc_str")
	setup_fortran_fixture_test("Test_Grid_Str_FlowCentRind" "write_grid_str" "write_flowcentrind_str")
	setup_fortran_fixture_test("Test_Grid_Str_2zn" "write_grid2zn_str" "write_con2zn_str")
	setup_fortran_fixture_test("Test_Grid_Str_2zngenrl" "write_grid2zn_str" "write_con2zn_genrl_str")

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
