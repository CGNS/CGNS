#############
# cgnsview  #
#############

include_directories(${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
include_directories(../common)
include_directories(../..)
link_directories(.)
if (CGNS_USE_SHARED)
  link_libraries(cgns_shared)
else (CGNS_USE_SHARED)
  link_libraries(cgns_static)
endif (CGNS_USE_SHARED)
link_libraries(${TCL_LIBRARY} ${TK_LIBRARY})

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
    set(cgnsview_FILES cgiotcl.c winmain.c ../common/winhtml.c)
  else (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    set(cgnsview_FILES cgiotcl.c winmain.c)
  endif (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
else (WIN32)
  include_directories(${X11_X11_INCLUDE_PATH})
  link_libraries(${X11_X11_LIB} m)
  set(cgnsview_FILES
	cgiowish.c
	cgiotcl.c)
endif (WIN32)


add_executable(cgiowish ${cgnsview_FILES})
add_dependencies(cgiowish cgns_static)

if (WIN32)
  set_target_properties(cgiowish PROPERTIES
  	LINK_FLAGS /subsystem:windows)
endif (WIN32)

if (WIN32)
  install(TARGETS
	cgiowish
	RUNTIME DESTINATION bin)
  install(PROGRAMS
	cgnsview.bat
	cgnsnodes.bat
	DESTINATION bin)
  install(FILES
	cgnsview.tcl
	cgns.tcl
	cgnsnodes.tcl
	export.tcl
	import.tcl
	tools.tcl
	DESTINATION share)
else (WIN32)
  install(TARGETS
	cgiowish
	RUNTIME DESTINATION bin/cgnstools)
  install(PROGRAMS
	cgnsview.sh
	DESTINATION bin
	RENAME cgnsview)
  install(PROGRAMS
	cgnsnodes.sh
	DESTINATION bin
	RENAME cgnsnodes)
  install(FILES
	cgnsview.tcl
	cgns.tcl
	cgnsnodes.tcl
	export.tcl
	import.tcl
	tools.tcl
	DESTINATION share/cgnstools)
endif (WIN32)

