############
# cgnscalc #
############

include_directories(${TK_INCLUDE_DIR})
include_directories(../calclib)
link_directories(. ${HDF5_LIBRARY_DIR} ${TK_LIBRARY_DIR} ${TCL_LIBRARY_DIR})
link_libraries(cgns_static calclib hdf5 m tcl tk)
if(HDF5_NEED_ZLIB)
	link_directories(${ZLIB_LIBRARY_DIR})
	link_libraries(z)
endif(HDF5_NEED_ZLIB)
if(HDF5_NEED_SZIP)
	link_directories(${SZIP_LIBRARY_DIR})
	link_libraries(szlib)
endif(HDF5_NEED_SZIP)
if(HDF5_NEED_MPI)
	link_directories(${MPI_LIBRARY_DIR})
	link_libraries(mpi)
endif(HDF5_NEED_MPI)

set(cgnscalc_FILES 
	calctcl.c
	calcwish.c)

add_executable(calcwish ${cgnscalc_FILES})
add_dependencies(calcwish calclib cgns_static)

install(TARGETS
	calcwish
	RUNTIME DESTINATION bin)

install(PROGRAMS
	cgnscalc.sh
	DESTINATION bin
	RENAME cgnscalc)

install(PROGRAMS
	unitconv.sh
	DESTINATION bin
	RENAME unitconv)

install(FILES
	cgnscalc.tcl
	unitconv.tcl
	DESTINATION share/cgnstools)