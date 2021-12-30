###########
# calclib #
###########

set(calclib_FILES 
	calc.c
	vec.c
	vecsym.c)

add_library(calclib STATIC ${calclib_FILES})
if (CGNS_USE_SHARED)
  target_link_libraries(calclib PUBLIC cgns_shared)
else ()
  target_link_libraries(calclib PUBLIC cgns_static)
endif ()

if (NOT WIN32)
  target_link_libraries(calclib PUBLIC m)
endif()
# Add test
