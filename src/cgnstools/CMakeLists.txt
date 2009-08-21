#############
# cgnstools #
#############

set(TK_INCLUDE_DIR "" CACHE PATH "Directory containing TK headers")
set(TK_LIBRARY_DIR "" CACHE PATH "Directory containing TK library")

set(TCL_INCLUDE_DIR "" CACHE PATH "Directory containing TCL headers")
set(TCL_LIBRARY_DIR "" CACHE PATH "Directory containing TCL library")

set(GL_INCLUDE_DIR "" CACHE PATH "Directory containing GL headers")
set(GL_LIBRARY_DIR "" CACHE PATH "Directory containing GL library")

set(GLU_INCLUDE_DIR "" CACHE PATH "Directory containing GLU headers")
set(GLU_LIBRARY_DIR "" CACHE PATH "Directory containing GLU library")

set(XMU_INCLUDE_DIR "" CACHE PATH "Directory containing XMU headers")
set(XMU_LIBRARY_DIR "" CACHE PATH "Directory containing XMU library")

add_subdirectory(adfviewer)

add_subdirectory(calclib)
add_subdirectory(cgnscalc)

#add_subdirectory(cgnsplot)
	#gui starts, but no functions work

#add_subdirectory(cgnssh) 
	# posit label is no longer defined
	# not compatible with CGNS>=2.5
	# this code needs to be updated

add_subdirectory(common)

add_subdirectory(tkogl)
add_subdirectory(utilities)

file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/cgconfig
"CG_SYSTEM=${CMAKE_SYSTEM_NAME}; export CG_SYSTEM
CG_BIN_DIR=${CMAKE_INSTALL_PREFIX}/bin; export CG_BIN_DIR
CG_LIB_DIR=${CMAKE_INSTALL_PREFIX}/share/cgnstools; export CG_LIB_DIR
")

install(PROGRAMS
	${CMAKE_CURRENT_BINARY_DIR}/cgconfig
	DESTINATION bin)