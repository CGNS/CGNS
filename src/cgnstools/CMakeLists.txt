#############
# cgnstools #
#############

add_subdirectory(calclib)
add_subdirectory(cgnscalc)

add_subdirectory(cgnsplot)

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