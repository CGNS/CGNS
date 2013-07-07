#############
# utilities #
#############

include_directories(../..)
include_directories(../calclib)
include_directories(../common)

# cgns_info
link_directories(.)
if (CGNS_USE_SHARED)
  link_libraries(cgns_shared)
else (CGNS_USE_SHARED)
  link_libraries(cgns_static)
endif (CGNS_USE_SHARED)
link_libraries(calclib)
if (NOT WIN32)
  link_libraries(m)
endif (NOT WIN32)

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  link_libraries(${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    link_libraries(${ZLIB_LIBRARY})
  endif(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    link_libraries(${SZIP_LIBRARY})
  endif(HDF5_NEED_SZIP AND SZIP_LIBRARY)
  if(HDF5_NEED_MPI AND MPI_LIBS)
    link_libraries(${MPI_LIBS})
  endif(HDF5_NEED_MPI AND MPI_LIBS)
endif (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)

if (NOT WIN32)
  link_libraries(m)
endif (NOT WIN32)

set(cgns_info_FILES
	cgns_info.c
	cgnsutil.c
	../common/getargs.c)

add_executable(cgns_info ${cgns_info_FILES})
add_dependencies(cgns_info cgns_static calclib)

#----------

# plot3d_to_cgns
set(plot3d_to_cgns_FILES 
	plot3d_to_cgns.c
	cgnsutil.c
	binaryio.c
	../common/getargs.c)

add_executable(plot3d_to_cgns ${plot3d_to_cgns_FILES})
add_dependencies(plot3d_to_cgns cgns_static calclib)

# cgns_to_plot3d
set(cgns_to_plot3d_FILES 
	cgns_to_plot3d.c
	cgnsutil.c
	../common/getargs.c
	p3dfout.c)

add_executable(cgns_to_plot3d ${cgns_to_plot3d_FILES})
add_dependencies(cgns_to_plot3d cgns_static calclib)

#----------

# patran_to_cgns
set(patran_to_cgns_FILES 
	patran_to_cgns.c
	cgnsImport.c
	../common/getargs.c
	../common/hash.c)

add_executable(patran_to_cgns ${patran_to_cgns_FILES})
add_dependencies(patran_to_cgns cgns_static calclib)

#----------

# tecplot_to_cgns
set(tecplot_to_cgns_FILES
	tecplot_to_cgns.c
	cgnsImport.c
	../common/getargs.c
	../common/hash.c)

add_executable(tecplot_to_cgns ${tecplot_to_cgns_FILES})
add_dependencies(tecplot_to_cgns cgns_static calclib)

# cgns_to_tecplot
set(cgns_to_tecplot_FILES
	cgns_to_tecplot.c
	cgnsutil.c
	../common/getargs.c)

add_executable(cgns_to_tecplot ${cgns_to_tecplot_FILES})
add_dependencies(cgns_to_tecplot cgns_static calclib)

#----------

# tetgen_to_cgns
set(tetgen_to_cgns_FILES
	tetgen_to_cgns.c)

add_executable(tetgen_to_cgns ${tetgen_to_cgns_FILES})
add_dependencies(tetgen_to_cgns cgns_static calclib)

#----------

# vgrid_to_cgns
set(vgrid_to_cgns_FILES
	vgrid_to_cgns.c
	binaryio.c
	../common/getargs.c)

add_executable(vgrid_to_cgns ${vgrid_to_cgns_FILES})
add_dependencies(vgrid_to_cgns cgns_static calclib)

#----------

# aflr3_to_cgns
set(aflr3_to_cgns_FILES
	aflr3_to_cgns.c
	binaryio.c
	../common/getargs.c)

add_executable(aflr3_to_cgns ${aflr3_to_cgns_FILES})
add_dependencies(aflr3_to_cgns cgns_static calclib)

# cgns_to_aflr3
set(cgns_to_aflr3_FILES
	cgns_to_aflr3.c
	../common/getargs.c
	../common/hash.c)

add_executable(cgns_to_aflr3 ${cgns_to_aflr3_FILES})
add_dependencies(cgns_to_aflr3 cgns_static calclib)

#----------

# fast_to_cgns
set(fast_to_cgns_FILES
	fast_to_cgns.c
	binaryio.c
	../common/getargs.c)

add_executable(fast_to_cgns ${fast_to_cgns_FILES})
add_dependencies(fast_to_cgns cgns_static calclib)

# cgns_to_fast
set(cgns_to_fast_FILES
	cgns_to_fast.c
	../common/getargs.c
	../common/hash.c)

add_executable(cgns_to_fast ${cgns_to_fast_FILES})
add_dependencies(cgns_to_fast cgns_static calclib)

#----------

# cgns_to_vtk
set(cgns_to_vtk_FILES
	cgns_to_vtk.c
	../common/getargs.c)

add_executable(cgns_to_vtk ${cgns_to_vtk_FILES})
add_dependencies(cgns_to_vtk cgns_static calclib)

#----------

# convert_location
set(convert_location_FILES
	convert_location.c
	cgnsutil.c
	../common/getargs.c)

add_executable(convert_location ${convert_location_FILES})

# convert_variables
set(convert_variables_FILES
	convert_variables.c
	cgnsutil.c
	../common/getargs.c)

add_executable(convert_variables ${convert_variables_FILES})
add_dependencies(convert_variables cgns_static calclib)

# convert_dataclass
set(convert_dataclass_FILES
	convert_dataclass.c
	cgnsutil.c
	../common/getargs.c)

add_executable(convert_dataclass ${convert_dataclass_FILES})
add_dependencies(convert_dataclass cgns_static calclib)

# extract_subset
set(extract_subset_FILES
	extract_subset.c
	cgnsutil.c
	../common/getargs.c)

add_executable(extract_subset ${extract_subset_FILES})
add_dependencies(extract_subset cgns_static calclib)

# interpolate_cgns
set(interpolate_cgns_FILES
	interpolate_cgns.c
	cgnsutil.c
	../common/getargs.c)

add_executable(interpolate_cgns ${interpolate_cgns_FILES})
add_dependencies(interpolate_cgns cgns_static calclib)

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
else (WIN32)
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
endif (WIN32)

