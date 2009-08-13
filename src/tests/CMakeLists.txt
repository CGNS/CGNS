#########
# Tests #
#########

# Link all the executables to cgns and hdf5
link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static libhdf5.so libz.so)
if(HDF5_NEED_MPI)
	link_directories(${MPI_LIBRARY_DIR})
	link_libraries(libmpi.so)
endif(HDF5_NEED_MPI)

# Set the files needed by each test
set(dbtest_FILES
	dbtest.c
	utils.c)
set(elemtest_FILES
	elemtest.c)
set(open_cgns_FILES
	open_cgns.c
	utils.c)
set(test_exts_FILES
	test_exts.c)
set(test_goto_FILES
	test_goto.c)
set(test_partial_FILES
	test_partial.c)
set(write_array_FILES
	write_array.c
	utils.c)
set(write_bcdata_FILES
	write_bcdata.c
	utils.c)
set(write_links_FILES
	write_links.c
	utils.c)
set(write_rind_FILES
	write_rind.c)
set(write_test_FILES
	write_test.c)
set(write_zones_FILES
	write_zones.c
	utils.c)
set(cgread_FILES
	cgread.F)
set(cgwrite_FILES
	cgwrite.F)
set(f2c_test_FILES
	../f2c/fmain.f
	../f2c/csub.c)

# Build each test
add_executable( dbtest       ${dbtest_FILES}       )
add_executable( elemtest     ${elemtest_FILES}     )
add_executable( open_cgns    ${open_cgns_FILES}    )
add_executable( test_exts    ${test_exts_FILES}    )
add_executable( test_goto    ${test_goto_FILES}    )
add_executable( test_partial ${test_partial_FILES} )
add_executable( write_array  ${write_array_FILES}  )
add_executable( write_bcdata ${write_bcdata_FILES} )
add_executable( write_links  ${write_links_FILES}  )
add_executable( write_rind   ${write_rind_FILES}   )
add_executable( write_test   ${write_test_FILES}   )
add_executable( write_zones  ${write_zones_FILES}  )

# Conditionally build the fortran tests
if (ENABLE_FORTRAN)
	add_executable( f2c_test     ${f2c_test_FILES} )
	add_executable( cgread       ${cgread_FILES}   )
	add_executable( cgwrite      ${cgwrite_FILES}  )
endif (ENABLE_FORTRAN)

# Add the tests so that ctest can find them
add_test( dbtest       dbtest 1 1 elemtest.cgns )
add_test( elemtest     elemtest                 )
add_test( open_cgns    open_cgns elemtest.cgns  )
add_test( test_exts    test_exts                )
add_test( test_goto    test_goto                )
add_test( test_partial test_partial             )
add_test( write_array  write_array              )
add_test( write_bcdata write_bcdata             )
add_test( write_links  write_links              )
add_test( write_rind   write_rind               )
add_test( write_test   write_test               )
add_test( write_zones  write_zones              )

# Conditionally add the fortran tests
if (ENABLE_FORTRAN)
	add_test( f2c_test f2c_test ) # This still needs a regex test
	add_test( cgwrite  cgwrite  )
	add_test( cgread   cgread   )
endif (ENABLE_FORTRAN)