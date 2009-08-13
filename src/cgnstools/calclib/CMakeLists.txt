###########
# calclib #
###########

link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static libhdf5.so libm.so libz.so)
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