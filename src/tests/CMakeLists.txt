#########
# Tests #
#########

# Link all the executables to cgns and hdf5
if(CGNS_BUILD_SHARED)
  set(TESTS_LK_LIBS cgns_shared)
else()
  set(TESTS_LK_LIBS cgns_static)
endif()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND TESTS_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND TESTS_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND TESTS_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND TESTS_LK_LIBS ${MPI_LIBS})
  endif()
endif ()

if (NOT WIN32)
  list(APPEND TESTS_LK_LIBS m)
endif ()

# Set the files needed by each test
set(cgioc_FILES cgioc.c)
set(dbtest_FILES dbtest.c utils.c)
set(elemtest_FILES elemtest.c)
set(open_cgns_FILES open_cgns.c utils.c)
set(ser_benchmark_hdf5_FILES ser_benchmark_hdf5.c)
set(test_core_vfd_FILES test_core_vfd.c)
set(test_exts_FILES test_exts.c)
set(test_goto_FILES test_goto.c)
set(test_partial_FILES test_partial.c)
set(test_ver31_FILES test_ver31.c)
set(test_multifam_FILES test_multifam.c)
set(test_general_rind_FILES test_general_rind.c)
set(test_general_reshape_FILES test_general_reshape.c)
set(test_bbox_FILES test_bbox.c)
set(write_array_FILES write_array.c utils.c)
set(write_bcdata_FILES write_bcdata.c utils.c)
set(write_links_FILES write_links.c utils.c)
set(write_rind_FILES write_rind.c)
set(write_test_FILES write_test.c)
set(write_zones_FILES write_zones.c utils.c)
set(test_family_tree_FILES test_family_tree.c)
set(test_complex_FILES test_complex.c)
set(test_convert_elem_FILES test_convert_elem.c)
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
set(test_family_treef_FILES test_family_treef.F90)
set(test_bboxf_FILES test_bboxf.F90)
set(test_complexf_FILES test_complexf.F90)

# Build each test
add_executable( cgioc                ${cgioc_FILES}                )
add_executable( dbtest               ${dbtest_FILES}               )
add_executable( elemtest             ${elemtest_FILES}             )
add_executable( open_cgns            ${open_cgns_FILES}            )
add_executable( ser_benchmark_hdf5   ${ser_benchmark_hdf5_FILES}   )
add_executable( test_core_vfd        ${test_core_vfd_FILES}        )
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
add_executable( test_family_tree     ${test_family_tree_FILES}     )
add_executable( test_bbox            ${test_bbox_FILES}            )
add_executable( test_complex         ${test_complex_FILES}         )
add_executable( test_convert_elem    ${test_convert_elem_FILES}    )
#
set(C_TESTS_LIST
	cgioc
	dbtest
	elemtest
	open_cgns
	ser_benchmark_hdf5
	test_core_vfd        
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
	test_family_tree     
	test_bbox            
	test_complex         
	test_convert_elem)
set(SKIP_TESTS
	dbtest
	open_cgns)
# Modern Linking
foreach(TGTNAME ${C_TESTS_LIST})
  target_link_libraries(${TGTNAME} PRIVATE ${TESTS_LK_LIBS})
endforeach()

# Conditionally build the fortran tests
if (CGNS_ENABLE_FORTRAN)
	#add_executable( f2c_test           ${f2c_test_FILES}           )
	add_executable( cgiof_f03          ${cgiof_f03_FILES}          )
	add_executable( cgwrite_f03        ${cgwrite_f03_FILES}        )
	add_executable( cgread_f03         ${cgread_f03_FILES}         )
	add_executable( cgzconn_f03        ${cgzconn_f03_FILES}        )
	add_executable( cgsubreg_f03       ${cgsubreg_f03_FILES}       )
	add_executable( test_general_rindf ${test_general_rindf_FILES} )
	add_executable( test_family_treef  ${test_family_treef_FILES}  )
	add_executable( test_bboxf         ${test_bboxf_FILES}         )
	add_executable( test_complexf      ${test_complexf_FILES}      )
#	if (NOT CGNS_ENABLE_64BIT)
# Don't build the F77 source because they will fail if ENABLE_64BIT is used
#	  add_executable( cgiof       ${cgiof_FILES}  )
#	  add_executable( cgwrite      ${cgwrite_FILES}  )
#	  add_executable( cgread       ${cgread_FILES}   )
#	  add_executable( cgzconn      ${cgzconn_FILES}  )
#	  add_executable( cgsubreg     ${cgsubreg_FILES}  )
#	endif (NOT CGNS_ENABLE_64BIT)
	set(FORTRAN_TESTS_LIST
	cgiof_f03          
	cgwrite_f03        
	cgread_f03         
	cgzconn_f03        
	cgsubreg_f03       
	test_general_rindf 
	test_family_treef  
	test_bboxf         
	test_complexf)
	# Modern Linking
	foreach(TGTNAME ${FORTRAN_TESTS_LIST})
	  target_link_libraries(${TGTNAME} PRIVATE ${TESTS_LK_LIBS})
	endforeach()
endif ()

# Add the tests so that ctest can find them
if (CGNS_ENABLE_TESTS)
  foreach(TARGETNAME ${C_TESTS_LIST})
    if ( ${TARGETNAME} IN_LIST SKIP_TESTS )
      continue()
    endif()
    add_test(NAME ${TARGETNAME} COMMAND ${TARGETNAME})
  endforeach()

  # Conditionally add the fortran tests
  if (CGNS_ENABLE_FORTRAN)
    foreach(TARGETNAME ${FORTRAN_TESTS_LIST})
      add_test(NAME ${TARGETNAME} COMMAND ${TARGETNAME})
    endforeach()
  endif (CGNS_ENABLE_FORTRAN)
endif (CGNS_ENABLE_TESTS)
