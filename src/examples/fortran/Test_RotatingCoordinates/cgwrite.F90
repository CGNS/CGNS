	program write_rotating
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on May 31 2002

!       This example test the RotatingCoordinates_t data structure and its children
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim, i
	integer cg, base_no, zone_no, ier
	integer(cgsize_t) size(Ndim*3)
	real*4 rot_rate(Ndim), rot_center(Ndim)
	character*100 linkpath

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
                             base_no, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f

! *** write a descriptor under the base
        call cg_goto_f(cg, base_no, ier, 'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('BaseDescriptorName', &
            'BaseDescriptorText', ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! *** write RotatingCoordinates_t node
	rot_rate(1)=10.1
	rot_rate(2)=0
	rot_rate(3)=0
	rot_center(1) = 1.1
	rot_center(2) = 1.2
	rot_center(3) = 1.3
	call cg_rotating_write_f(rot_rate, rot_center, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! *** write children of RotatingCoordinates_t node
	call cg_goto_f(cg, base_no, ier, 'RotatingCoordinates_t', &
                       1, 'end')
	if (ier .eq. ERROR) call cg_error_exit_f

! * Descriptor
	call cg_descriptor_write_f('RotatingCoordinatesDescriptorName', &
             'RotatingCoordinatesDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * DataClass
        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! * DimensionalUnits
	call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                              CGNS_ENUMV(Radian), ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! * UserDefinedData
        call cg_user_data_write_f('UserData1', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * Link to BaseDescriptorName
        write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * RotationCenter/Descriptor
	call cg_goto_f(cg, base_no, ier, 'RotatingCoordinates_t', 1, &
                       'DataArray_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f
	call cg_descriptor_write_f('RotationCenterDescriptorName', &
             'RotationCenterDescriptorText', ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	
! * RotationCenter/DimensionalUnits
	call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                              CGNS_ENUMV(Radian), ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * Write a zone
	do i=1,index_dim
	    size(i) = i+1		! nr of nodes in i,j,k = 3,4,5
	    size(i+Ndim) = size(i)-1	! nr of elements in i,j,k
	    size(i+2*Ndim) = 0		! nr of bnd nodes if ordered
	enddo
	call cg_zone_write_f(cg, base_no, 'MyZone', size, &
                                 CGNS_ENUMV(Structured), zone_no, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** write a descriptor under the zone
        call cg_goto_f(cg, base_no, ier, 'Zone_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_descriptor_write_f('ZoneDescriptorName', &
            'ZoneDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** write RotatingCoordinates_t node
        rot_rate(1)=10.1
        rot_rate(2)=0
        rot_rate(3)=0
        rot_center(1) = 1.1
        rot_center(2) = 1.2
        rot_center(3) = 1.3
        call cg_rotating_write_f(rot_rate, rot_center, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** write children of RotatingCoordinates_t node
        call cg_goto_f(cg, base_no, ier, 'Zone_t', 1, &
                       'RotatingCoordinates_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

! * Descriptor
        call cg_descriptor_write_f('RotatingCoordinatesDescriptorName', &
             'RotatingCoordinatesDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * DataClass
        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * DimensionalUnits
        call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                              CGNS_ENUMV(Radian), ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * UserDefinedData
        call cg_user_data_write_f('UserData2', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * Link to ZoneDescriptorName
        write(linkpath,'(a)') '/Basename/MyZone/ZoneDescriptorName'
        call cg_link_write_f('LinkToZoneDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * RotationCenter/Descriptor
        call cg_goto_f(cg, base_no, ier, 'Zone_t', 1, &
             'RotatingCoordinates_t', 1, 'DataArray_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f
        call cg_descriptor_write_f('RotationCenterDescriptorName', &
             'RotationCenterDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * RotationCenter/DimensionalUnits
        call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                              CGNS_ENUMV(Radian), ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** close CGNS file
	call cg_close_f(cg, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	end

