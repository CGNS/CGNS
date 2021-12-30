#########
# Tests #
#########

if (NOT MPIEXEC)
  message(SEND_ERROR "need to define MPIEXEC to run tests")
endif ()

link_directories(.)
set(PTESTS_LK_LIBS
	cgns_static
        ${HDF5_LIBRARY}
	${MPI_LIBS})

if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
  list(APPEND PTESTS_LK_LIBS ${ZLIB_LIBRARY})
endif()
if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
  list(APPEND PTESTS_LK_LIBS ${SZIP_LIBRARY})
endif()

if (NOT WIN32)
  list(APPEND PTESTS_LK_LIBS m)
endif ()

set(C_PTESTS_LIST
	pcgns_ctest
	cexample
	benchmark
	benchmark_hdf5
	Metadata_Stressing
	open_close
	comm_test
	test_base
	test_unstructured
	test_unstruc_quad
	test_zone
	test_general_readwrite
	test_poly_unstructured
	thesis_benchmark)

set(pcgns_ctest_FILES pcgns_ctest.c)
add_executable(pcgns_ctest ${pcgns_ctest_FILES})

set(cexample_FILES cexample.c)
add_executable(cexample ${cexample_FILES})

set(benchmark_FILES benchmark.c)
add_executable(benchmark ${benchmark_FILES})

set(benchmark_hdf5_FILES benchmark_hdf5.c)
add_executable(benchmark_hdf5 ${benchmark_hdf5_FILES})

set(Metadata_Stressing_FILES Metadata_Stressing.c)
add_executable(Metadata_Stressing ${Metadata_Stressing_FILES})

set(open_close_FILES open_close.c)
add_executable(open_close ${open_close_FILES})

set(comm_test_FILES comm_test.c)
add_executable(comm_test ${comm_test_FILES})

set(test_base_FILES test_base.c)
add_executable(test_base ${test_base_FILES})

set(test_unstructured_FILES test_unstructured.c)
add_executable(test_unstructured ${test_unstructured_FILES})

set(test_unstruc_quad_FILES test_unstruc_quad.c)
add_executable(test_unstruc_quad ${test_unstruc_quad_FILES})

set(test_zone_FILES test_zone.c)
add_executable(test_zone ${test_zone_FILES})

set(test_general_readwrite_FILES test_general_readwrite.c)
add_executable(test_general_readwrite ${test_general_readwrite_FILES})

set(test_poly_unstructured_FILES test_poly_unstructured.c)
add_executable(test_poly_unstructured ${test_poly_unstructured_FILES})

set(thesis_benchmark_FILES thesis_benchmark.c)
add_executable(thesis_benchmark ${thesis_benchmark_FILES})

# Linking
foreach(TGTNAME ${C_PTESTS_LIST})
  target_link_libraries(${TGTNAME} PRIVATE ${PTESTS_LK_LIBS})
endforeach()

if (CGNS_ENABLE_FORTRAN AND HAVE_FORTRAN_2003)
  set(FORTRAN_PTESTS_LIST
    pcgns_ftest
    fexample
    benchmark_hdf5_f90
    test_mixed_par_ser
  )
  set(pcgns_ftest_FILES pcgns_ftest.F90)
  add_executable(pcgns_ftest ${pcgns_ftest_FILES})
  set(fexample_FILES fexample.F90)
  add_executable(fexample ${fexample_FILES})
  set(benchmark_hdf5_f90_FILES benchmark_hdf5_f90.F90)
  add_executable(benchmark_hdf5_f90 ${benchmark_hdf5_f90_FILES})
  set(test_mixed_par_ser_FILES test_mixed_par_ser.F90)
  add_executable(test_mixed_par_ser ${test_mixed_par_ser_FILES})
  # Linking
  foreach(TGTNAME ${FORTRAN_PTESTS_LIST})
    target_link_libraries(${TGTNAME} PRIVATE ${PTESTS_LK_LIBS})
  endforeach()
endif ()

# Add the tests so that cmake can find them
if (CGNS_ENABLE_TESTS)
  foreach(TEST_NAME ${C_PTESTS_LIST})
    add_test(NAME test-${TEST_NAME}
             COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
	     ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${TEST_NAME}> ${MPIEXEC_POSTFLAGS})
  endforeach()
  if (CGNS_ENABLE_FORTRAN  AND HAVE_FORTRAN_2003)
    foreach(TEST_NAME ${FORTRAN_PTESTS_LIST})
	    add_test(NAME test-${TEST_NAME}
		     COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
                     ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${TEST_NAME}> ${MPIEXEC_POSTFLAGS})
    endforeach()
  endif ()
endif ()
