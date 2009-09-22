#############
# adfviewer #
#############

include_directories(${TCL_INCLUDE_DIR})
include_directories(../common)
include_directories(../../adf)
link_directories(. ${HDF5_LIBRARY_DIR} ${TCL_LIBRARY_DIR} ${TK_LIBRARY_DIR})
link_directories(${GL_LIBRARY_DIR} ${GLU_LIBRARY_DIR} ${XMU_LIBRARY_DIR})
link_libraries(cgns_static tkogl hdf5 m tcl tk GL GLU Xmu)
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

set(adfviewer_FILES 
	adfwish.c
	adftcl.c)

add_executable(adfwish ${adfviewer_FILES})
add_dependencies(adfwish cgns_static tkogl)

install(TARGETS
	adfwish
	RUNTIME DESTINATION bin)

install(PROGRAMS
	adfviewer.sh
	DESTINATION bin
	RENAME adfviewer)

install(PROGRAMS
	cgnsnodes.sh
	DESTINATION bin
	RENAME cgnsnodes)

install(FILES
	adfviewer.tcl
	cgns.tcl
	cgnsnodes.tcl
	export.tcl
	import.tcl
	tools.tcl
	tclIndex
	DESTINATION share/cgnstools)