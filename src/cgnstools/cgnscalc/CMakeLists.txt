############
# cgnscalc #
############

include_directories(/usr/include/tcl/)
include_directories(../calclib)
link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static calclib libhdf5.so libm.so libtcl.so libtk.so libz.so)
if(HDF5_NEED_MPI)
	link_directories(${MPI_LIBRARY_DIR})
	link_libraries(libmpi.so)
endif(HDF5_NEED_MPI)

set(cgnscalc_FILES 
	calctcl.c
	calcwish.c)

add_executable(calcwish ${cgnscalc_FILES})
add_dependencies(calcwish calclib cgns_static)