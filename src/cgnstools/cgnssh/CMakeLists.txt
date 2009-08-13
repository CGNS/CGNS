###########
# cgnsssh #
###########

include_directories(/usr/include/tcl/)
include_directories(../common)
include_directories(../.. ../../adf)
if(HDF5_NEED_MPI)
	include_directories(${MPI_INCLUDE_DIR})
endif(HDF5_NEED_MPI)
link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static calclib libhdf5.so libtcl.so libtk.so libz.so)
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