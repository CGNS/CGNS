#########
# tkogl #
#########

include_directories(${TCL_INCLUDE_DIR})

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

link_directories(. ${TCL_LIBRARY_DIR} ${TK_LIBRARY_DIR})
link_directories(${GL_LIBRARY_DIR} ${GLU_LIBRARY_DIR} ${XMU_LIBRARY_DIR})
link_libraries(tkogl tk tcl GL GLU Xmu)

set(glwish_FILES 
	tkAppInit.c)

add_executable(glwish ${glwish_FILES})

install(TARGETS
	glwish
	RUNTIME DESTINATION bin)

install(FILES
	stripcr.tcl
	DESTINATION share/cgnstools)