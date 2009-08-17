###########
# cgnsssh #
###########

include_directories(/usr/include/tcl/)
include_directories(../common)
include_directories(../.. ../../adf)
link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static calclib libhdf5.so libtcl.so libtk.so)
if(HDF5_NEED_ZLIB)
	link_directories(${ZLIB_LIBRARY_DIR})
	link_libraries(libz.so)
endif(HDF5_NEED_ZLIB)
if(HDF5_NEED_SZIP)
	link_directories(${SZIP_LIBRARY_DIR})
	link_libraries(szlib.so)
endif(HDF5_NEED_SZIP)
if(HDF5_NEED_MPI)
	link_directories(${MPI_LIBRARY_DIR})
	link_libraries(libmpi.so)
endif(HDF5_NEED_MPI)

set(cgnssh_FILES 
	cgnssh.c
	cgnstcl.c
	../common/cgnames.c)

add_executable(cgnssh ${cgnssh_FILES})
add_dependencies(cgnssh cgns_static)