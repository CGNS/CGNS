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



install(FILES
	DESTINATION share/cgnstools)