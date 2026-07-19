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
	test_multisets
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
	test_mixed_par_ser
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

if (HDF5_VERSION VERSION_GREATER_EQUAL "2.0.0")
  add_executable(pcgns_unicode_test pcgns_unicode_test.c)
  target_link_libraries(pcgns_unicode_test PRIVATE ${PTESTS_LK_LIBS})
  # Embed UTF-8 active code page manifest on Windows so that MPI_File_open
  # (which uses ANSI file APIs) can handle UTF-8 filenames.
  if (WIN32)
    target_sources(pcgns_unicode_test PRIVATE
      ${CMAKE_CURRENT_SOURCE_DIR}/pcgns_unicode_test.manifest)
  endif()
endif()

if (CGNS_ENABLE_FORTRAN AND HAVE_FORTRAN_2003)
  set(FORTRAN_PTESTS_LIST
    pcgns_ftest
    fexample
    benchmark_hdf5_f90
    test_mixed_par_ser_f90
    test_unstruc_quad_f90
    test_poly_unstructured_f90
    test_general_wrappers_f90
    ftest_zone
  )
  add_library(test_utilsf STATIC ${CMAKE_SOURCE_DIR}/src/tests/utilsf.F90)

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
             COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "4"
	     ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${TEST_NAME}> ${MPIEXEC_POSTFLAGS})
  endforeach()
  if (HDF5_VERSION VERSION_GREATER_EQUAL "2.0.0")
    add_test(NAME test-pcgns_unicode_test
             COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
	     ${MPIEXEC_PREFLAGS} $<TARGET_FILE:pcgns_unicode_test> ${MPIEXEC_POSTFLAGS})
  endif()
  if (CGNS_ENABLE_FORTRAN  AND HAVE_FORTRAN_2003)
    foreach(TEST_NAME ${FORTRAN_PTESTS_LIST})
	    add_test(NAME test-${TEST_NAME}
		     COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "4"
                     ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${TEST_NAME}> ${MPIEXEC_POSTFLAGS})
    endforeach()
  endif ()
endif ()
