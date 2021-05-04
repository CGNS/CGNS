	program modify_bprop
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on August 2002

!       This example test modifying BCProperty_t data structure and its children
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim
	character*100 linkpath

	integer cg, base, zone, boco, ier, size(Ndim*3), i, num
	integer WallFunctionType, AreaType, pnts(Ndim,2), nbases
	character*32 RegionName32, basename
	real *4 SurfaceArea, version
	integer dim_vals(2)
	

! *** initialize
	ier = 0
	index_dim=Ndim
	cell_dim=Ndim
	phys_dim=Ndim

! *** open CGNS file for writing
 	call cg_open_f('Test_V2', MODE_MODIFY, cg, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,600)'MODIFYING FILE Test_V2'

! *** CGNS Library Version used for file creation:
        call cg_version_f(cg, version, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,102) &
           'Library Version used for file creation:',version

! *** base
	call cg_nbases_f(cg, nbases, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,200)'nbases=',nbases

	base=1

        call cg_base_read_f(cg, base, basename, cell_dim, phys_dim, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,300)'BaseName = "',basename,'"', &
                      'cell_dimension=',cell_dim

	zone =1
	boco=2

! *** write BCProperty_t/WallFunction_t node under boco#2
	WallFunctionType = CGNS_ENUMV(Generic)
	call cg_bc_wallfunction_write_f(cg, base, zone, &
              boco, WallFunctionType, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	write(6,*)'WallFunction_t written'

! *** write BCProperty_t/Area_t node under boco#2
	AreaType = CGNS_ENUMV(BleedArea)
	SurfaceArea = 123.456
 	RegionName32 = 'myRegion'
	call cg_bc_area_write_f(cg, base, zone, boco, AreaType, &
       	     SurfaceArea, RegionName32, ier)

	write(6,*)'Area_t written'

! *** write Descriptor_t, UserDefinedData_t & Link under...
! ...   BCProperty_t
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'ZoneBC_t', 1, &
      	    'BC_t', boco, 'BCProperty_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

	write(6,*)'cg_goto BCProperty_t: ier=',ier

        call cg_descriptor_write_f('BCpropDescriptorName', &
            'BCpropDescriptorText', ier)
	write(6,*)'cg_descriptor_write_f: ier=',ier
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_user_data_write_f('BCPropUserData', ier)
	write(6,*)'cg_user_data_write_f: ier=',ier
        if (ier .eq. ERROR) call cg_error_exit_f

! ...   WallFunction_t
	call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'ZoneBC_t', 1, &
            'BC_t', boco, 'BCProperty_t', 1, 'WallFunction_t', 1, 'end')

	call cg_descriptor_write_f('WFDescriptorName', &
      	    'WFDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_user_data_write_f('WFUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! ...   Area_t
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'ZoneBC_t', 1, &
            'BC_t', boco, 'BCProperty_t', 1, 'Area_t', 1, 'end')
	call cg_descriptor_write_f('AreaDescriptorName', &
      	    'AreaDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_user_data_write_f('AreaUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

 	dim_vals(1)=3
 	dim_vals(2)=2
 	call cg_array_write_f('arraysize', CGNS_ENUMV(Integer), 2, dim_vals, &
              size, ier)
 	if (ier .ne. ERROR) write(6,'(a)') 'Error in API'

! * DataClass & DimensionalUnits under SurfaceArea
	call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'ZoneBC_t', 1, &
            'BC_t', boco, 'BCProperty_t', 1, 'Area_t', 1, 'DataArray_t', &
      	    1, 'end')

	call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
      	    		      CGNS_ENUMV(Radian), ier)
	if (ier .eq. ERROR) call cg_error_exit_f

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

! *** close CGNS file
	call cg_close_f(cg, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

 102    format(a,f8.3)
 200    format(a,i5)
 300    format(3a/a,i2)
 600    format(3a)
	end



