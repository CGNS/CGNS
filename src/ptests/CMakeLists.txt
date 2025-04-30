#########
# Tests #
#########

include_directories(${CMAKE_SOURCE_DIR}/src/tests)

if (NOT MPIEXEC)
  message(SEND_ERROR "need to define MPIEXEC to run tests")
endif ()

if(CGNS_BUILD_SHARED)
  set(PTESTS_LK_LIBS cgns_shared)
else()
  set(PTESTS_LK_LIBS cgns_static)
endif()

list( APPEND PTESTS_LK_LIBS
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

#
function(add_test_exe file)
  add_executable (${file} ${file}.c)
  target_link_libraries (${file} PRIVATE ${PTESTS_LK_LIBS})
endfunction ()

# Define C tests
foreach(TGTNAME ${C_PTESTS_LIST})
  add_test_exe(${TGTNAME})
endforeach()

if (CGNS_ENABLE_FORTRAN AND HAVE_FORTRAN_2003)
  set(FORTRAN_PTESTS_LIST
    pcgns_ftest
    fexample
    benchmark_hdf5_f90
    test_mixed_par_ser
    test_unstruc_quad_f90
    test_poly_unstructured_f90
  )
  add_library(test_utilsf OBJECT ${CMAKE_SOURCE_DIR}/src/tests/utilsf.F90)

  function (add_fortran_exe file)
    add_executable (${file} ${file}.F90)
    target_link_libraries (${file} PRIVATE ${PTESTS_LK_LIBS} test_utilsf)
  endfunction ()

  foreach(TGTNAME ${FORTRAN_PTESTS_LIST})
    add_fortran_exe(${TGTNAME})
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
