	program write_bprop
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on August 2002

!       This example test the BCProperty_t data structure and its children
!       It tests the functions:
!	- cg_bc_wallfunction_write_f(cg, B, Z, BC, WallFunctionType, ier)
!	- cg_bc_area_write_f(cg, B, Z, BC, AreaType, SurfaceArea, RegionName, ier)
!       And the following:
!       - multiple calls for the same BC => must return error in mode write
!       - Test RegionName in 32 char string, justification, call with a name (not declared)
!       - cg_descriptor_write under BCProperty_t, WallFunction_t, Area_t
!       - cg_user_data_write, under BCProperty_t, WallFunction_t, Area_t
!       - cg_array_write under Area_t => should return an error
!       - links under BCProperty_t, WallFunction_t, Area_t
!       - cg_goto to BCProperty_t, WallFunction_t, Area_t
!       - Memory check with Insure
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim
	character*100 linkpath

	integer cg, base, zone, boco, ier, i, num
	integer(cgsize_t) size(Ndim*3)
	integer WallFunctionType, AreaType
	integer(cgsize_t) pnts(Ndim,2)
	character*32 RegionName32
	character*10 RegionName10
	character*100 RegionName100
	real *4 SurfaceArea
	integer dim_vals(2)
	

! *** initialize
	ier = 0
	index_dim=Ndim
	cell_dim=Ndim
	phys_dim=Ndim

! *** open CGNS file for writing
 	call cg_open_f('Test_V2', MODE_WRITE, cg, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f

! *** base
 	call cg_base_write_f(cg, 'Basename', cell_dim, phys_dim, &
                             base, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f

! *** write a descriptor under the base
        call cg_goto_f(cg, base, ier, 'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('BaseDescriptorName', &
            'BaseDescriptorText', ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! *** write a zone
	do i=1,index_dim                    	! zone#1: 3*4*5
	    size(i) = i+2			! nr of nodes in i,j,k
	    size(i+index_dim) = size(i)-1	! nr of elements in i,j,k
	    size(i+2*index_dim) = 0		! nr of bnd nodes if ordered
	    num = num * size(i)             	! nr of nodes
	enddo

	call cg_zone_write_f(cg, base, 'Zone#1', size, CGNS_ENUMV(Structured), &
       	    zone, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! *** write bocos
	! imin face
	do i=1, index_dim
	    pnts(i,1) = 1
	    pnts(i,2) = size(i)
	enddo
	pnts(1,2) = 1
        call cg_boco_write_f(cg, base, zone, 'boco#1', &
             CGNS_ENUMV(BCInflow), CGNS_ENUMV(PointRange), 2_cgsize_t, pnts, boco, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        ! imax face
	do i=1, index_dim
            pnts(i,1) = 1
            pnts(i,2) = size(i)
        enddo
        pnts(1,1) = size(1)
	call cg_boco_write_f(cg, base, zone, 'boco#2', &
             CGNS_ENUMV(BCOutflow), CGNS_ENUMV(PointRange), 2_cgsize_t, pnts, boco, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	
! *** write BCProperty_t/WallFunction_t node under boco#2
	WallFunctionType = CGNS_ENUMV(Generic)
	call cg_bc_wallfunction_write_f(cg, base, zone, &
              boco, WallFunctionType, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! *** test calling it again
!	call cg_bc_wallfunction_write_f(cg, base, zone,
!    &        boco, WallFunctionType, ier)
!       if (ier .eq. ERROR) call cg_error_exit_f

! *** write BCProperty_t/Area_t node under boco#2
	AreaType = CGNS_ENUMV(BleedArea)
	SurfaceArea = 123.456
!	RegionName10 = 'myRegion'
 	RegionName100 = 'myRegion'
!	write(RegionName32,'(a10)')'myRegion32'
	call cg_bc_area_write_f(cg, base, zone, boco, AreaType, &
       	     SurfaceArea, RegionName100, ier)

! *** write Descriptor_t, UserDefinedData_t & Link under...
! ...   BCProperty_t
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'ZoneBC_t', 1, &
      	    'BC_t', boco, 'BCProperty_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_descriptor_write_f('BCpropDescriptorName', &
            'BCpropDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_user_data_write_f('BCPropUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
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

	end

