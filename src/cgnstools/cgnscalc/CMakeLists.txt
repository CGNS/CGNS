############
# cgnscalc #
############

# Define source files
if (WIN32)
  if (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    set(cgnscalc_FILES calctcl.c winmain.c ../common/winhtml.c)
  else ()
    set(cgnscalc_FILES calctcl.c winmain.c)
  endif ()
else ()
  set(cgnscalc_FILES
	calctcl.c
	calcwish.c)
endif ()

add_executable(calcwish ${cgnscalc_FILES})

target_include_directories(calcwish PRIVATE ${TCL_INCLUDE_PATH} ${TK_INCLUDE_PATH})
target_include_directories(calcwish PRIVATE ../calclib)

if (CGNS_USE_SHARED)
  target_link_libraries(calcwish PRIVATE cgns_shared)
else ()
  target_link_libraries(calcwish PRIVATE cgns_static)
endif ()
target_link_libraries(calcwish PRIVATE calclib ${TCL_LIBRARY} ${TK_LIBRARY})

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  target_link_libraries(calcwish PRIVATE ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    target_link_libraries(calcwish PRIVATE ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    target_link_libraries(calcwish PRIVATE ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    target_link_libraries(calcwish PRIVATE ${MPI_LIBS})
  endif()
endif ()

if (WIN32)
  if (HTML_HELP_INCLUDE_PATH AND HTML_HELP_LIBRARY)
    add_definitions(-DUSE_HTMLHELP)
    target_include_directories(calcwish PRIVATE ${HTML_HELP_INCLUDE_PATH})
    target_link_libraries(calcwish PRIVATE ${HTML_HELP_LIBRARY})
  endif ()
else ()
  target_include_directories(calcwish PRIVATE ${X11_X11_INCLUDE_PATH})
  target_link_libraries(calcwish PRIVATE ${X11_X11_LIB} m)
endif ()

if (WIN32)
  set_target_properties(calcwish PROPERTIES
  	LINK_FLAGS /subsystem:windows)
endif ()

if (WIN32)
  install(TARGETS
	calcwish
	RUNTIME DESTINATION bin)
  install(PROGRAMS
	cgnscalc.bat
	unitconv.bat
	DESTINATION bin)
  install(FILES
	cgnscalc.tcl
	unitconv.tcl
	cgnscalc.ico
	cgnscalc.png
	cgnscalc-icon.xbm
	cgnscalc-mask.xbm
	unitconv.ico
	unitconv.png
	unitconv.xbm
	DESTINATION share)
else ()
  install(TARGETS
	calcwish
	RUNTIME DESTINATION bin/cgnstools)
  install(PROGRAMS
	cgnscalc.sh
	DESTINATION bin
	RENAME cgnscalc)
  install(PROGRAMS
	cgnscalc.desktop
	DESTINATION bin)
  install(PROGRAMS
	unitconv.sh
	DESTINATION bin
	RENAME unitconv)
  install(PROGRAMS
	unitconv.desktop
	DESTINATION bin)
  install(FILES
	cgnscalc.tcl
	unitconv.tcl
	cgnscalc.ico
	cgnscalc.png
	cgnscalc-icon.xbm
	cgnscalc-mask.xbm
	unitconv.ico
	unitconv.png
	unitconv.xbm
	DESTINATION share/cgnstools)
endif ()

