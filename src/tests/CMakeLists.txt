#########
# Tests #
#########

# Link all the executables to cgns and hdf5
link_directories(.)
if(CGNS_BUILD_SHARED)
  link_libraries(cgns_shared)
else(CGNS_BUILD_SHARED)
  link_libraries(cgns_static)
endif(CGNS_BUILD_SHARED)

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
set(cgioc_FILES cgioc.c)
set(dbtest_FILES dbtest.c utils.c)
set(elemtest_FILES elemtest.c)
set(open_cgns_FILES open_cgns.c utils.c)
set(ser_benchmark_hdf5_FILES ser_benchmark_hdf5.c)
set(test_exts_FILES test_exts.c)
set(test_goto_FILES test_goto.c)
set(test_partial_FILES test_partial.c)
set(test_ver31_FILES test_ver31.c)
set(test_multifam_FILES test_multifam.c)
set(test_general_rind_FILES test_general_rind.c)
set(test_general_reshape_FILES test_general_reshape.c)
set(write_array_FILES write_array.c utils.c)
set(write_bcdata_FILES write_bcdata.c utils.c)
set(write_links_FILES write_links.c utils.c)
set(write_rind_FILES write_rind.c)
set(write_test_FILES write_test.c)
set(write_zones_FILES write_zones.c utils.c)
#set(cgiof_FILES cgiof.F90)
#set(cgwrite_FILES cgwrite.F90)
#set(cgread_FILES cgread.F90)
#set(cgzconn_FILES cgzconn.F90)
#set(cgsubreg_FILES cgsubreg.F90)
set(cgiof_f03_FILES cgiof_f03.F90)
set(cgwrite_f03_FILES cgwrite_f03.F90)
set(cgread_f03_FILES cgread_f03.F90)
set(cgzconn_f03_FILES cgzconn_f03.F90)
set(cgsubreg_f03_FILES cgsubreg_f03.F90)
set(test_general_rindf_FILES test_general_rindf.F90)

# Build each test
add_executable( cgioc                ${cgioc_FILES}                )
add_executable( dbtest               ${dbtest_FILES}               )
add_executable( elemtest             ${elemtest_FILES}             )
add_executable( open_cgns            ${open_cgns_FILES}            )
add_executable( ser_benchmark_hdf5   ${ser_benchmark_hdf5_FILES}   )
add_executable( test_exts            ${test_exts_FILES}            )
add_executable( test_goto            ${test_goto_FILES}            )
add_executable( test_partial         ${test_partial_FILES}         )
add_executable( test_ver31           ${test_ver31_FILES}           )
add_executable( test_multifam        ${test_multifam_FILES}        )
add_executable( test_general_rind    ${test_general_rind_FILES}    )
add_executable( test_general_reshape ${test_general_reshape_FILES} )
add_executable( write_array          ${write_array_FILES}          )
add_executable( write_bcdata         ${write_bcdata_FILES}         )
add_executable( write_links          ${write_links_FILES}          )
add_executable( write_rind           ${write_rind_FILES}           )
add_executable( write_test           ${write_test_FILES}           )
add_executable( write_zones          ${write_zones_FILES}          )

# Conditionally build the fortran tests
if (CGNS_ENABLE_FORTRAN)
	#add_executable( f2c_test           ${f2c_test_FILES}           )
	add_executable( cgiof_f03          ${cgiof_f03_FILES}          )
	add_executable( cgwrite_f03        ${cgwrite_f03_FILES}        )
	add_executable( cgread_f03         ${cgread_f03_FILES}         )
	add_executable( cgzconn_f03        ${cgzconn_f03_FILES}        )
	add_executable( cgsubreg_f03       ${cgsubreg_f03_FILES}       )
	add_executable( test_general_rindf ${test_general_rindf_FILES} )
#	if (NOT CGNS_ENABLE_64BIT)
# Don't build the F77 source because they will fail if ENABLE_64BIT is used
#	  add_executable( cgiof       ${cgiof_FILES}  )
#	  add_executable( cgwrite      ${cgwrite_FILES}  )
#	  add_executable( cgread       ${cgread_FILES}   )
#	  add_executable( cgzconn      ${cgzconn_FILES}  )
#	  add_executable( cgsubreg     ${cgsubreg_FILES}  )
#	endif (NOT CGNS_ENABLE_64BIT)
endif (CGNS_ENABLE_FORTRAN)

# Add the tests so that ctest can find them
set (TESTPROGS
     cgioc
     elemtest
     ser_benchmark_hdf5
     test_exts  
     test_goto
     test_partial
     test_ver31
     test_multifam
     test_general_rind
     test_general_reshape
     write_array
     write_bcdata
     write_links
     write_rind
     write_test
     write_zones
)

#	if (NOT CGNS_ENABLE_64BIT)
#	  list(APPEND TESTPROGS
#        cgiof
#	       cgwrite
#	       cgread
#        cgzconn
#	       cgsubreg
#   )
#	endif (NOT CGNS_ENABLE_64BIT)

# Conditionally add the fortran tests
if (CGNS_ENABLE_FORTRAN)
  # This still needs a regex test
  list(APPEND TESTPROGS
       f2c_test
       cgread_f03        
       cgiof_f03
       cgwrite_f03
       cgzconn_f03
       test_general_rindf
	     cgsubreg_f03
  )
endif (CGNS_ENABLE_FORTRAN)

if (CGNS_ENABLE_TESTS)
  foreach (testprog ${TESTPROGS})
    add_test(${testprog} ${testprog})
  endforeach (testprog ${TESTPROGS})
endif (CGNS_ENABLE_TESTS)
