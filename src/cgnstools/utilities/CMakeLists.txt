#############
# utilities #
#############

include_directories(/usr/include/tcl/)
include_directories(../..)
include_directories(../calclib)
include_directories(../common)
if(HDF5_NEED_MPI)
	include_directories(${MPI_INCLUDE_DIR})
endif(HDF5_NEED_MPI)


# cgns_info
link_directories(. ${HDF5_LIBRARY_DIR})
link_libraries(cgns_static calclib libhdf5.so libm.so libz.so)
if(HDF5_NEED_MPI)
	link_directories(${MPI_LIBRARY_DIR})
	link_libraries(libmpi.so)
endif(HDF5_NEED_MPI)

set(cgns_info_FILES 
	cgns_info.c
	cgnsutil.c
	../common/getargs.c)

add_executable(cgns_info ${cgns_info_FILES})
add_dependencies(cgns_info cgns_static calclib)

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

# patran_to_cgns
set(patran_to_cgns_FILES 
	patran_to_cgns.c
	cgnsImport.c
	../common/getargs.c
	../common/hash.c)

add_executable(patran_to_cgns ${patran_to_cgns_FILES})
add_dependencies(patran_to_cgns cgns_static calclib)

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

# cgns_to_vtk
set(cgns_to_vtk_FILES 
	cgns_to_vtk.c
	../common/getargs.c)

add_executable(cgns_to_vtk ${cgns_to_vtk_FILES})
add_dependencies(cgns_to_vtk cgns_static calclib)

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