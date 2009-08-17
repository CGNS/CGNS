############
# cgnsplot #
############

include_directories(/usr/include/tcl/)
include_directories(../common)
link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static tkogl libhdf5.so libm.so libtcl.so libtk.so libGL.so libGLU.so libXmu.so)
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

set(cgnsplot_FILES 
	cgnstcl.c
	plotwish.c
	../common/hash.c)

add_executable(plotwish ${cgnsplot_FILES})
add_dependencies(plotwish cgns_static tkogl)