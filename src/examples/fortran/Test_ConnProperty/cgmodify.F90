	program modify_cprop
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on August 2002

!       This example test modifying the GridConnectivityProperty_t data structure and its children
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim
	integer cg, base, zone, conn, ier, size(Ndim*3), i, num, n
	integer nbases
	integer dim_vals(2)
	integer AverageInterfaceType, pnts(Ndim,20), donor_pnts(Ndim,20)
	real *4 RotationCenter(Ndim), RotationAngle(Ndim)
	real *4 Translation(Ndim), version
	character*100 linkpath
	character*32 basename

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

        zone = 1
	conn = 2

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
	call cg_conn_periodic_write_f(cg, base, zone, conn, &
       	     RotationCenter, RotationAngle, Translation, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** write Descriptor_t, UserDefinedData_t & Link under...
! ...   GridConnectivityProperty_t
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
      	    'GridConnectivityProperty_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_descriptor_write_f('GridConnPropDescriptorName', &
            'GridConnPropDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_user_data_write_f('GridConnPropUserData', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

!	write(linkpath,'(a)') '/Basename/BaseDescriptorName'
!       call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
!       if (ier .eq. ERROR) call cg_error_exit_f

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

! ...   Periodic_t
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

! * DataClass & DimensionalUnits under RotationCenter
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

! *** close CGNS file
	call cg_close_f(cg, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

 102    format(a,f8.3)
 200    format(a,i5)
 300    format(3a/a,i2)
 600    format(3a)

	end

