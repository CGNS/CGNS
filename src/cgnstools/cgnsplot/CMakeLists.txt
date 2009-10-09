############
# cgnsplot #
############

include_directories(${TCL_INCLUDE_DIR})
include_directories(../common)
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

set(cgnsplot_FILES 
	cgnstcl.c
	plotwish.c
	../common/hash.c)

add_executable(plotwish ${cgnsplot_FILES})
add_dependencies(plotwish cgns_static tkogl)

install(TARGETS
	plotwish
	RUNTIME DESTINATION bin)

install(PROGRAMS
	cgnsplot.sh
	DESTINATION bin
	RENAME cgnsplot)

install(FILES
	cgnsplot.tcl
	cgnsplot.ico
	cgnsplot.png
	cgnsplot-icon.xbm
	cgnsplot-mask.xbm
	DESTINATION share/cgnstools)

install(DIRECTORY help/
	DESTINATION share/cgnstools/help/cgnsplot
	FILES_MATCHING PATTERN "*.html" PATTERN "*.gif"
	PATTERN ".svn" EXCLUDE)

