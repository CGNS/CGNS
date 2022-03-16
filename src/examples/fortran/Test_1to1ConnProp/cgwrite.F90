	program write_cprop
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on August 2002

!       This example test the GridConnectivityProperty_t data structure and its children
!       It tests the functions:
!	- cg_conn_average_write_f(cg, B, Z, I, AverageInterfaceType, ier)
!	- cg_conn_periodic_write_f(cg, B, Z, I, RotationCenter, RotationAngle, Translation, ier)
!       And the following:
!       - multiple calls for the same I => must return error in mode write
!       - cg_descriptor_write under GridConnectivityProperty_t, AverageInterface_t, Periodic_t
!       - cg_user_data_write, under GridConnectivityProperty_t, AverageInterface_t, Periodic_t
!       - cg_array_write under Periodic_t => should return an error
!       - links under GridConnectivityProperty_t, AverageInterface_t, Periodic_t
!       - cg_goto to GridConnectivityProperty_t, AverageInterface_t, Periodic_t
!       - Memory check with Insure
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim
	character*100 linkpath

	integer cg, base, zone, conn, ier, i, num, n
	integer(cgsize_t) size(Ndim*3)
	integer AverageInterfaceType
        integer(cgsize_t) pnts(Ndim,20), donor_pnts(Ndim,20)
	real *4 RotationCenter(Ndim), RotationAngle(Ndim)
	real *4 Translation(Ndim)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	integer dim_vals(2)
	integer transform(Ndim), one21
	character*32 donorname

! *** initialize
	ier = 0
	index_dim=Ndim
	cell_dim=Ndim
	phys_dim=Ndim

! *** open CGNS file for writing
 	call cg_open_f('Test_V2', MODE_WRITE, cg, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f

! *** base
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
 	call cg_base_write_f(cg, 'Basename', cell_dim, phys_dim, &
                             base, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f

! *** write a descriptor under the base
        call cg_goto_f(cg, base, ier, 'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('BaseDescriptorName', &
            'BaseDescriptorText', ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! *** write 2 zones
	do i=1,index_dim                    	! zone#1: 3*4*5
	    size(i) = i+2			! nr of nodes in i,j,k
	    size(i+index_dim) = size(i)-1	! nr of elements in i,j,k
	    size(i+2*index_dim) = 0		! nr of bnd nodes if ordered
	    num = num * size(i)             	! nr of nodes
	enddo

	call cg_zone_write_f(cg, base, 'Zone#1', size, CGNS_ENUMV(Structured), &
       	    zone, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_zone_write_f(cg, base, 'Zone#2', size, CGNS_ENUMV(Structured), &
            zone, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	zone = 1

! *** write conns
        DO n=1, 5
           DO i=1,3
              pnts(i,n)=i             ! * dummy data
              donor_pnts(i,n)=i*2
           ENDDO
        ENDDO
        call cg_conn_write_f(cg, base, zone, 'Connect#1', &
            CGNS_ENUMV(Vertex), CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointList), 5_cgsize_t, pnts, 'Zone#2', &
            CGNS_ENUMV(Structured), CGNS_ENUMV(PointListDonor), CGNS_ENUMV(Integer), 5_cgsize_t, donor_pnts, &
            conn, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_conn_write_f(cg, base, zone, 'Connect#2', &
            CGNS_ENUMV(Vertex), CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointList), 5_cgsize_t, pnts, 'Zone#2', &
            CGNS_ENUMV(Structured), CGNS_ENUMV(PointListDonor), CGNS_ENUMV(Integer), 5_cgsize_t, donor_pnts, &
            conn, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** write GridConnectivityProperty_t/AverageInterface_t node under conn#2
	AverageInterfaceType = CGNS_ENUMV(AverageCircumferential)
	call cg_conn_average_write_f(cg, base, zone, &
              conn, AverageInterfaceType, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! *** test calling it again
!	call cg_conn_average_write_f(cg, base, zone,
!    &        conn, AverageInterfaceType, ier)
!       if (ier .eq. ERROR) call cg_error_print_f

! *** write GridConnectivityProperty_t/Periodic_t node under conn#2
	do n=1, Ndim
	    RotationCenter(n) = n*1.1
	    RotationAngle(n) = (n+3)*1.1
	    Translation(n) = (n+6)*1.1
	enddo
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	call cg_conn_periodic_write_f(cg, base, zone, conn, &
       	     RotationCenter, RotationAngle, Translation, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! begin KMW : write 1to1 connectivity node

	! *** connectivity 1to1
	!  generate data
	do i=1,3
	    !**make up some dummy data:
            pnts(i,1)=1
            pnts(i,2)=size(i)
	    donor_pnts(i,1)=1
	    donor_pnts(i,2)=size(i)
	    transform(i)=i*(-1)
        enddo

	if (zone .eq. 1) then
	    donorname='zone#2'
	else if (zone .eq. 2) then
	    donorname='zone#1'
	endif

	call cg_1to1_write_f(cg, base, zone, '1to1_#1', &
      		donorname, pnts, donor_pnts, transform, one21, ier)
 	    if (ier .eq. ERROR) call cg_error_exit_f

! *** write GridConnectivityProperty_t/AverageInterface_t node under conn#2
	AverageInterfaceType = CGNS_ENUMV(AverageCircumferential)
	call cg_1to1_average_write_f(cg, base, zone, &
              one21, AverageInterfaceType, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

        call cg_1to1_periodic_write_f(cg, base, zone, one21, &
       	     RotationCenter, RotationAngle, Translation, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! end KMW : write 1to1 connectivity node
	

! *** write Descriptor_t, UserDefinedData_t & Link under...
! ...   GridConnectivityProperty_t
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
      	    'GridConnectivityProperty_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_descriptor_write_f('GridConnPropDescriptorName', &
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            'GridConnPropDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_user_data_write_f('GridConnPropUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! begin KMW : do for GridConnectivityProperty_t under GridConnectivity1to1_t

	call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity1to1_t',one21, &
      	    'GridConnectivityProperty_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_descriptor_write_f('GridConnPropDescriptorName', &
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            'GridConnPropDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_user_data_write_f('GridConnPropUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! end KMW

! ...   AverageInterface_t
	call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
      	    'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
            'GridConnectivityProperty_t', 1, 'AverageInterface_t', 1, &
            'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('AverageInterfaceDescriptorName', &
      	    'AverageInterfaceDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_user_data_write_f('AverageInterfaceUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! begin KMW : do for AverageInterface_t under GridConnectivity1to1_t
	
	call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
      	    'ZoneGridConnectivity_t', 1, 'GridConnectivity1to1_t',one21, &
            'GridConnectivityProperty_t', 1, 'AverageInterface_t', 1, &
            'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('AverageInterfaceDescriptorName', &
      	    'AverageInterfaceDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_user_data_write_f('AverageInterfaceUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! end KMW

! ...   Periodic_t
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
            'GridConnectivityProperty_t', 1, 'Periodic_t', 1, 'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('PeriodicDescriptorName', &
      	    'PeriodicDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_user_data_write_f('PeriodicUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * DataClass & DimensionalUnits under Periodic
        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                              CGNS_ENUMV(Radian), ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * DataArray under Periodic
 	dim_vals(1)=3
 	dim_vals(2)=2
 	call cg_array_write_f('arraysize', CGNS_ENUMV(Integer), 2, dim_vals, &
              size, ier)
 	if (ier .ne. ERROR) write(6,'(a)') 'Error in API'

! begin KMW : do for Periodic_t under GridConnectivity1to1_t

        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity1to1_t',one21, &
            'GridConnectivityProperty_t', 1, 'Periodic_t', 1, 'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('PeriodicDescriptorName', &
      	    'PeriodicDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_user_data_write_f('PeriodicUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * DataClass & DimensionalUnits under Periodic
        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                              CGNS_ENUMV(Radian), ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * DataArray under Periodic
 	dim_vals(1)=3
 	dim_vals(2)=2
 	call cg_array_write_f('arraysize', CGNS_ENUMV(Integer), 2, dim_vals, &
              size, ier)
 	if (ier .ne. ERROR) write(6,'(a)') 'Error in API'

! end KMW

! * DataClass & DimensionalUnits under RotationCenter
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
            'GridConnectivityProperty_t', 1, 'Periodic_t', 1, &
            'DataArray_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
      	    		      CGNS_ENUMV(Radian), ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! begin KMW : do under GridConnectivity1to1_t

	call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity1to1_t',one21, &
            'GridConnectivityProperty_t', 1, 'Periodic_t', 1, &
            'DataArray_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
      	    		      CGNS_ENUMV(Radian), ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! end KMW

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

! *** close CGNS file
	call cg_close_f(cg, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	end

