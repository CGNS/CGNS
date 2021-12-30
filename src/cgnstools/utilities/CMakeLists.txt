#############
# utilities #
#############

include_directories(../..)
include_directories(../calclib)
include_directories(../common)


# Link all the executables to cgns and hdf5
if(CGNS_USE_SHARED)
  set(UTILITIES_LK_LIBS cgns_shared)
else()
  set(UTILITIES_LK_LIBS cgns_static)
endif()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND UTILITIES_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND UTILITIES_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND UTILITIES_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND UTILITIES_LK_LIBS ${MPI_LIBS})
  endif()
endif ()

if (NOT WIN32)
  list(APPEND UTILITIES_LK_LIBS m)
endif ()
list(APPEND UTILITIES_LK_LIBS calclib)

set(cgns_info_FILES
	cgns_info.c
	cgnsutil.c
	../common/getargs.c)

add_executable(cgns_info ${cgns_info_FILES})
target_link_libraries(cgns_info PRIVATE ${UTILITIES_LK_LIBS})

#----------

# plot3d_to_cgns
set(plot3d_to_cgns_FILES 
	plot3d_to_cgns.c
	cgnsutil.c
	binaryio.c
	../common/getargs.c)

add_executable(plot3d_to_cgns ${plot3d_to_cgns_FILES})
target_link_libraries(plot3d_to_cgns PRIVATE ${UTILITIES_LK_LIBS})

# cgns_to_plot3d
set(cgns_to_plot3d_FILES 
	cgns_to_plot3d.c
	cgnsutil.c
	binaryio.c
	../common/getargs.c
	p3dfout.c)

add_executable(cgns_to_plot3d ${cgns_to_plot3d_FILES})
target_link_libraries(cgns_to_plot3d PRIVATE ${UTILITIES_LK_LIBS})

#----------

# patran_to_cgns
set(patran_to_cgns_FILES 
	patran_to_cgns.c
	cgnsImport.c
	../common/getargs.c
	../common/hash.c)

add_executable(patran_to_cgns ${patran_to_cgns_FILES})
target_link_libraries(patran_to_cgns PRIVATE ${UTILITIES_LK_LIBS})

#----------

# tecplot_to_cgns
set(tecplot_to_cgns_FILES
	tecplot_to_cgns.c
	cgnsImport.c
	../common/getargs.c
	../common/hash.c)

add_executable(tecplot_to_cgns ${tecplot_to_cgns_FILES})
target_link_libraries(tecplot_to_cgns PRIVATE ${UTILITIES_LK_LIBS})

# cgns_to_tecplot
set(cgns_to_tecplot_FILES
	cgns_to_tecplot.c
	cgnsutil.c
	../common/getargs.c)

add_executable(cgns_to_tecplot ${cgns_to_tecplot_FILES})
target_link_libraries(cgns_to_tecplot PRIVATE ${UTILITIES_LK_LIBS})

#----------

# tetgen_to_cgns
set(tetgen_to_cgns_FILES
	tetgen_to_cgns.c)

add_executable(tetgen_to_cgns ${tetgen_to_cgns_FILES})
target_link_libraries(tetgen_to_cgns PRIVATE ${UTILITIES_LK_LIBS})

#----------

# vgrid_to_cgns
set(vgrid_to_cgns_FILES
	vgrid_to_cgns.c
	binaryio.c
	../common/getargs.c)

add_executable(vgrid_to_cgns ${vgrid_to_cgns_FILES})
target_link_libraries(vgrid_to_cgns PRIVATE ${UTILITIES_LK_LIBS})

#----------

# aflr3_to_cgns
set(aflr3_to_cgns_FILES
	aflr3_to_cgns.c
	binaryio.c
	../common/getargs.c)

add_executable(aflr3_to_cgns ${aflr3_to_cgns_FILES})
target_link_libraries(aflr3_to_cgns PRIVATE ${UTILITIES_LK_LIBS})

# cgns_to_aflr3
set(cgns_to_aflr3_FILES
	cgns_to_aflr3.c
	../common/getargs.c
	../common/hash.c)

add_executable(cgns_to_aflr3 ${cgns_to_aflr3_FILES})
target_link_libraries(cgns_to_aflr3 PRIVATE ${UTILITIES_LK_LIBS})

#----------

# fast_to_cgns
set(fast_to_cgns_FILES
	fast_to_cgns.c
	binaryio.c
	../common/getargs.c)

add_executable(fast_to_cgns ${fast_to_cgns_FILES})
target_link_libraries(fast_to_cgns PRIVATE ${UTILITIES_LK_LIBS})

# cgns_to_fast
set(cgns_to_fast_FILES
	cgns_to_fast.c
	../common/getargs.c
	../common/hash.c)

add_executable(cgns_to_fast ${cgns_to_fast_FILES})
target_link_libraries(cgns_to_fast PRIVATE ${UTILITIES_LK_LIBS})

#----------

# cgns_to_vtk
set(cgns_to_vtk_FILES
	cgns_to_vtk.c
	../common/getargs.c)

add_executable(cgns_to_vtk ${cgns_to_vtk_FILES})
target_link_libraries(cgns_to_vtk PRIVATE ${UTILITIES_LK_LIBS})

#----------

# convert_location
set(convert_location_FILES
	convert_location.c
	cgnsutil.c
	../common/getargs.c)

add_executable(convert_location ${convert_location_FILES})
target_link_libraries(convert_location PRIVATE ${UTILITIES_LK_LIBS})

# convert_variables
set(convert_variables_FILES
	convert_variables.c
	cgnsutil.c
	../common/getargs.c)

add_executable(convert_variables ${convert_variables_FILES})
target_link_libraries(convert_variables PRIVATE ${UTILITIES_LK_LIBS})

# convert_dataclass
set(convert_dataclass_FILES
	convert_dataclass.c
	cgnsutil.c
	../common/getargs.c)

add_executable(convert_dataclass ${convert_dataclass_FILES})
target_link_libraries(convert_dataclass PRIVATE ${UTILITIES_LK_LIBS})

# extract_subset
set(extract_subset_FILES
	extract_subset.c
	cgnsutil.c
	../common/getargs.c)

add_executable(extract_subset ${extract_subset_FILES})
target_link_libraries(extract_subset PRIVATE ${UTILITIES_LK_LIBS})

# interpolate_cgns
set(interpolate_cgns_FILES
	interpolate_cgns.c
	cgnsutil.c
	../common/getargs.c)

add_executable(interpolate_cgns ${interpolate_cgns_FILES})
target_link_libraries(interpolate_cgns PRIVATE ${UTILITIES_LK_LIBS})

# update_ngon
set(update_ngon_FILES
	update_ngon.c
	cgnsutil.c
	../common/getargs.c)

add_executable(update_ngon ${update_ngon_FILES})
target_link_libraries(update_ngon PRIVATE ${UTILITIES_LK_LIBS})

if (WIN32)
  install(TARGETS
        plot3d_to_cgns
	cgns_to_plot3d
	patran_to_cgns
	tecplot_to_cgns
	cgns_to_tecplot
	tetgen_to_cgns
	vgrid_to_cgns
	aflr3_to_cgns
	cgns_to_aflr3
	fast_to_cgns
	cgns_to_fast
	cgns_to_vtk
	convert_location
	convert_variables
	convert_dataclass
	extract_subset
	interpolate_cgns
	update_ngon
        cgns_info
	RUNTIME DESTINATION bin)
  install(FILES
	conserved.cnv
	convert.tcl
	dimensional.cnv
	patran.tcl
	plot3d.tcl
	primitive.cnv
	tecplot.tcl
	tetgen.tcl
	vgrid.tcl
	aflr3.tcl
	fast.tcl
	util.tcl
	utilities.mnu
	vtk.tcl
	DESTINATION share)
else ()
  install(TARGETS
        plot3d_to_cgns
	cgns_to_plot3d
	patran_to_cgns
	tecplot_to_cgns
	cgns_to_tecplot
	tetgen_to_cgns
	vgrid_to_cgns
	aflr3_to_cgns
	cgns_to_aflr3
	fast_to_cgns
	cgns_to_fast
	cgns_to_vtk
	convert_location
	convert_variables
	convert_dataclass
	extract_subset
	interpolate_cgns
	update_ngon
        cgns_info
	RUNTIME DESTINATION bin/cgnstools)
  install(FILES
	conserved.cnv
	convert.tcl
	dimensional.cnv
	patran.tcl
	plot3d.tcl
	primitive.cnv
	tecplot.tcl
	tetgen.tcl
	vgrid.tcl
	aflr3.tcl
	fast.tcl
	util.tcl
	utilities.mnu
	vtk.tcl
	DESTINATION share/cgnstools)
endif ()

