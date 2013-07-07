#########
# Tests #
#########

if (NOT MPIEXEC)
  message(SEND_ERROR "need to define MPIEXEC to run tests")
endif (NOT MPIEXEC)

link_directories(.)
link_libraries(cgns_static)
link_libraries(${HDF5_LIBRARY})
link_libraries(${MPI_LIBS})

if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
  link_libraries(${ZLIB_LIBRARY})
endif(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
  link_libraries(${SZIP_LIBRARY})
endif(HDF5_NEED_SZIP AND SZIP_LIBRARY)

if (NOT WIN32)
  link_libraries(m)
endif (NOT WIN32)

set(ctest_FILES ctest.c)
add_executable(ctest ${ctest_FILES})

set(cexample_FILES cexample.c)
add_executable(cexample ${cexample_FILES})

set(benchmark_FILES benchmark.c)
add_executable(benchmark ${benchmark_FILES})

set(open_close_FILES open_close.c)
add_executable(open_close ${open_close_FILES})

set(test_base_FILES test_base.c)
add_executable(test_base ${test_base_FILES})

set(test_unstructured_FILES test_unstructured.c)
add_executable(test_unstructured ${test_unstructured_FILES})

set(test_zone_FILES test_zone.c)
add_executable(test_zone ${test_zone_FILES})

set(thesis_benchmark_FILES thesis_benchmark.c)
add_executable(thesis_benchmark ${thesis_benchmark_FILES})

if (CGNS_ENABLE_FORTRAN AND HAS_FORTRAN AND NOT CGNS_ENABLE_64BIT)
  set(ftest_FILES ftest.F)
  add_executable(ftest ${ftest_FILES})
  set(fexample_FILES fexample.F)
  add_executable(fexample ${fexample_FILES})
endif (CGNS_ENABLE_FORTRAN AND HAS_FORTRAN AND NOT CGNS_ENABLE_64BIT)

# Add the tests so that cmake can find them
if (CGNS_ENABLE_TESTS)
  add_test(ctest ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
      ${MPIEXEC_PREFLAGS} ctest ${MPIEXEC_POSTFLAGS})
  add_test(cexample ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
      ${MPIEXEC_PREFLAGS} cexample ${MPIEXEC_POSTFLAGS})
  add_test(benchmark ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
      ${MPIEXEC_PREFLAGS} benchmark ${MPIEXEC_POSTFLAGS})
  add_test(open_close ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
      ${MPIEXEC_PREFLAGS} open_close ${MPIEXEC_POSTFLAGS})
  add_test(test_base ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
      ${MPIEXEC_PREFLAGS} test_base ${MPIEXEC_POSTFLAGS})
  add_test(test_unstructured ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
      ${MPIEXEC_PREFLAGS} test_unstructured ${MPIEXEC_POSTFLAGS})
  add_test(test_zone ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
      ${MPIEXEC_PREFLAGS} test_zone ${MPIEXEC_POSTFLAGS})
  add_test(thesis_benchmark ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
      ${MPIEXEC_PREFLAGS} thesis_benchmark ${MPIEXEC_POSTFLAGS})
  if (CGNS_ENABLE_FORTRAN AND HAS_FORTRAN AND NOT CGNS_ENABLE_64BIT)
    add_test(ftest ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
        ${MPIEXEC_PREFLAGS} ftest ${MPIEXEC_POSTFLAGS})
    add_test(fexample ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} "2"
        ${MPIEXEC_PREFLAGS} fexample ${MPIEXEC_POSTFLAGS})
  endif (CGNS_ENABLE_FORTRAN AND HAS_FORTRAN AND NOT CGNS_ENABLE_64BIT)
endif (CGNS_ENABLE_TESTS)
