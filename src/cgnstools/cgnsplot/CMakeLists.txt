############
# cgnsplot #
############

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH} ${OPENGL_INCLUDE_DIR})
include_directories(../common)
link_directories(. ../tkogl)
if (CGNS_USE_SHARED)
  link_libraries(cgns_shared)
else (CGNS_USE_SHARED)
  link_libraries(cgns_static)
endif (CGNS_USE_SHARED)
link_libraries(tkogl ${TCL_LIBRARY} ${TK_LIBRARY}
	${OPENGL_gl_LIBRARY} ${OPENGL_glu_LIBRARY})

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  link_libraries(${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    link_libraries(${ZLIB_LIBRARY})
  endif(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    link_libraries(${SZIP_LIBRARY})
  endif(HDF5_NEED_SZIP AND SZIP_LIBRARY)
  if(HDF5_NEED_MPI AND MPI_LIBS)
    link_libraries(${MPI_LIBS})
  endif(HDF5_NEED_MPI AND MPI_LIBS)
endif (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)

if (WIN32)
  if (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    add_definitions(-DUSE_HTMLHELP)
    include_directories(${HTML_HELP_INCLUDE_PATH})
    link_libraries(${HTML_HELP_LIBRARY})
    set(cgnsplot_FILES cgnstcl.c winmain.c ../common/hash.c ../common/winhtml.c)
  else (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    set(cgnsplot_FILES cgnstcl.c winmain.c ../common/hash.c)
  endif (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
else (WIN32)
  include_directories(${X11_X11_INCLUDE_PATH})
  link_libraries(${X11_X11_LIB} ${X11_Xmu_LIB} m)
  set(cgnsplot_FILES
	cgnstcl.c
	plotwish.c
	../common/hash.c)
endif (WIN32)

add_executable(plotwish ${cgnsplot_FILES})
add_dependencies(plotwish cgns_static tkogl)

if (WIN32)
  set_target_properties(plotwish PROPERTIES
  	LINK_FLAGS /subsystem:windows)
endif (WIN32)

if (WIN32)
  install(TARGETS
	plotwish
	RUNTIME DESTINATION bin)
  install(PROGRAMS
	cgnsplot.bat
	DESTINATION bin)
  install(FILES
	cgnsplot.tcl
	cgnsplot.ico
	cgnsplot.png
	cgnsplot-icon.xbm
	cgnsplot-mask.xbm
	DESTINATION share)
else (WIN32)
  install(TARGETS
	plotwish
	RUNTIME DESTINATION bin/cgnstools)
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
endif (WIN32)

