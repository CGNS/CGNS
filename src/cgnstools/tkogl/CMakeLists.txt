#########
# tkogl #
#########

include_directories(/usr/include/tcl/)

set(tkogl_FILES 
	tkogl.c
	get.c
	gencyl.c
	load3ds.c
	nurbs.c
	quadric.c
	tess.c
	printstr.c
	feedback.c
	tkoglparse.c)

add_library(tkogl STATIC ${tkogl_FILES})

link_libraries(tkogl libtk.so libtcl.so libGL.so libGLU.so libXmu.so)

set(glwish_FILES 
	tkAppInit.c)

add_executable(glwish ${glwish_FILES})

install(TARGETS
	glwish
	RUNTIME DESTINATION bin)

install(FILES
	stripcr.tcl
	DESTINATION share/cgnstools)