#########
# Tools #
#########

# Link all the executables to cgns and hdf5
link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static libhdf5.so libz.so)
if(HDF5_NEED_MPI)
	link_directories(${MPI_LIBRARY_DIR})
	link_libraries(libmpi.so)
endif(HDF5_NEED_MPI)

# Set the files needed by each tool
set(cgnames_FILES
	cgnames_standalone.c
	cgnames.c)
set(cgns2xml_FILES
	cgns2xml.c
	getargs.c)
set(cgnscheck_FILES
	cgnscheck.c
	getargs.c
	hash.c
	cgnames.c)
set(cgnsconvert_FILES
	cgnsconvert.c
	getargs.c)
set(cgnsdiff_FILES
	cgnsdiff.c
	getargs.c)
set(cgnslist_FILES
	cgnslist.c
	getargs.c)
set(cgnsversion_FILES
	cgnsversion.c
	getargs.c)
set(adf2hdf_FILES
	../adfh/adf2hdf.c)
set(hdf2adf_FILES
	../adfh/hdf2adf.c)

# Build each tool
add_executable( cgnames     ${cgnames_FILES}     )
add_executable( cgns2xml    ${cgns2xml_FILES}    )
add_executable( cgnscheck   ${cgnscheck_FILES}   )
add_executable( cgnsconvert ${cgnsconvert_FILES} )
add_executable( cgnsdiff    ${cgnsdiff_FILES}    )
add_executable( cgnslist    ${cgnslist_FILES}    )
add_executable( cgnsversion ${cgnsversion_FILES} )
add_executable( adf2hdf     ${adf2hdf_FILES}     )
add_executable( hdf2adf     ${hdf2adf_FILES}     )

# Set the install path for the tools
install(TARGETS
	cgnames
	cgns2xml
	cgnscheck
	cgnsconvert
	cgnsdiff
	cgnslist
	cgnsversion
	adf2hdf
	hdf2adf
	RUNTIME DESTINATION bin)