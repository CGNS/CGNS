###########
# calclib #
###########

link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static libhdf5.so libm.so)
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

set(calclib_FILES 
	calc.c
	vec.c
	vecsym.c)

add_library(calclib STATIC ${calclib_FILES})

# Add test