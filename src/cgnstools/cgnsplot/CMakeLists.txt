############
# cgnsplot #
############

# Define source files
if (WIN32)
  if (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    set(cgnsplot_FILES cgnstcl.c winmain.c ../common/hash.c ../common/winhtml.c)
  else ()
    set(cgnsplot_FILES cgnstcl.c winmain.c ../common/hash.c)
  endif ()
else ()
  set(cgnsplot_FILES
	cgnstcl.c
	plotwish.c
	../common/hash.c)
endif ()

# Define executable and dependencies
add_subdirectory(../tkogl tkogl)
add_executable(plotwish ${cgnsplot_FILES})

if (CGNS_USE_SHARED)
  set(PLOT_LK_LIBS cgns_shared)
else ()
  set(PLOT_LK_LIBS cgns_static)
endif ()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND PLOT_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND PLOT_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND PLOT_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND PLOT_LK_LIBS ${MPI_LIBS})
  endif()
endif ()

if (WIN32)
  if (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    add_definitions(-DUSE_HTMLHELP)
    target_include_directories(plotwish PRIVATE ${HTML_HELP_INCLUDE_PATH})
    list(APPEND PLOT_LK_LIBS ${HTML_HELP_LIBRARY})
  endif ()
else ()
  target_include_directories(plotwish PRIVATE ${X11_X11_INCLUDE_PATH})
  list(APPEND PLOT_LK_LIBS ${X11_X11_LIB} ${X11_Xmu_LIB} ${X11_SM_LIB} m)
endif ()

target_link_libraries(plotwish PRIVATE tkogl ${PLOT_LK_LIBS} OpenGL::GL)

#target_include_directories(plotwish PRIVATE ${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH} ${OPENGL_INCLUDE_DIR})
#if (X11_FOUND)
#  target_include_directories(plotwish PRIVATE ${X11_INCLUDE_DIR})
#endif()
#target_include_directories(plotwish PRIVATE ../common)

if (WIN32)
  set_target_properties(plotwish PROPERTIES
  	LINK_FLAGS /subsystem:windows)
endif ()

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
else ()
  install(TARGETS
	plotwish
	RUNTIME DESTINATION bin/cgnstools)
  install(PROGRAMS
	cgnsplot.sh
	DESTINATION bin
	RENAME cgnsplot)
  install(PROGRAMS
	cgnsplot.desktop
	DESTINATION bin)
  install(FILES
	cgnsplot.tcl
	cgnsplot.ico
	cgnsplot.png
	cgnsplot-icon.xbm
	cgnsplot-mask.xbm
	DESTINATION share/cgnstools)
endif ()

