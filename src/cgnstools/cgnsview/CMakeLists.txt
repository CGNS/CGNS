#############
# cgnsview  #
#############

# Define source files
if (WIN32)
  if (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    set(cgnsview_FILES cgiotcl.c winmain.c ../common/winhtml.c)
  else ()
    set(cgnsview_FILES cgiotcl.c winmain.c)
  endif ()
else ()
  set(cgnsview_FILES
	cgiowish.c
	cgiotcl.c)
endif ()

# Define target
add_executable(cgiowish ${cgnsview_FILES})

target_include_directories(cgiowish PRIVATE ${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH} ${OPENGL_INCLUDE_DIR})
target_include_directories(cgiowish PRIVATE ../common)
target_include_directories(cgiowish PRIVATE ../..)

if(CGNS_USE_SHARED)
  set(CGNSVIEW_LK_LIBS cgns_shared)
else()
  set(CGNSVIEW_LK_LIBS cgns_static)
endif()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND CGNSVIEW_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND CGNSVIEW_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND CGNSVIEW_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND CGNSVIEW_LK_LIBS ${MPI_LIBS})
  endif()
endif ()
list(APPEND CGNSVIEW_LK_LIBS ${TCL_LIBRARY} ${TK_LIBRARY})

if (WIN32)
  if (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    add_definitions(-DUSE_HTMLHELP)
    target_include_directories(cgiowish PRIVATE ${HTML_HELP_INCLUDE_PATH})
    list(APPEND CGNSVIEW_LK_LIBS ${HTML_HELP_LIBRARY})
  endif ()
else ()
  target_include_directories(cgiowish PRIVATE ${X11_X11_INCLUDE_PATH})
  list(APPEND CGNSVIEW_LK_LIBS ${X11_X11_LIB} m)
endif ()

target_link_libraries(cgiowish PRIVATE ${CGNSVIEW_LK_LIBS})

if (WIN32)
  set_target_properties(cgiowish PROPERTIES
  	LINK_FLAGS /subsystem:windows)
endif ()

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
else ()
  install(TARGETS
	cgiowish
	RUNTIME DESTINATION bin/cgnstools)
  install(PROGRAMS
	cgnsview.sh
	DESTINATION bin
	RENAME cgnsview)
  install(PROGRAMS
	cgnsview.desktop
	DESTINATION bin)
  install(PROGRAMS
	cgnsnodes.sh
	DESTINATION bin
	RENAME cgnsnodes)
  install(PROGRAMS
	cgnsnodes.desktop
	DESTINATION bin)
  install(FILES
	cgnsview.tcl
	cgns.tcl
	cgnsnodes.tcl
	export.tcl
	import.tcl
	tools.tcl
	DESTINATION share/cgnstools)
endif () 

