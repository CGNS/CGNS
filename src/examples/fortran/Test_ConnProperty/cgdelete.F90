	program delete_cprop
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on August 2002

!       This example test deleting the GridConnectivityProperty_t data structure and its children
!       It tests cg_delete for the following nodes:
!       - child of GridConnectivity_t: GridConnectivityProperty_t
!       - children of GridConnectivityProperty_t: AverageInterface_t, Periodic_t, Descriptor_t, UserDef._t
!       - children of AverageInterface_t: AverageInterfaceType_t, Descriptor_t, UserDef._t
!       - children of Periodic_t: PeriodicType_t, RotationCenter, RotationAngle, Translation,
!				  Descriptor_t, UserDef._t
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim
	character*100 linkpath

	integer cg, base, zone, conn, ier, size(Ndim*3), i, num
	integer AverageInterfaceType, pnts(Ndim,2), nbases, n
	character*32 RegionName32, basename, dummy
	real *4 version, Translation(Ndim)
	real *4 RotationCenter(Ndim), RotationAngle(Ndim)
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

	zone = 1
	conn = 2

! *** delete children of DataArray_t "RotationCenter"
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
      	    'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
            'GridConnectivityProperty_t', 1, 'Periodic_t', 1, &
      	    'DataArray_t', 1, 'end')

        if (ier .eq. ALL_OK) then
            call cg_delete_node_f('DataClass', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_delete_node_f('DimensionalUnits', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            write(6,*) 'Children of RotationCenter deleted.  Continue?'
	    read(5,600) dummy
        endif

! *** delete children of Periodic_t: RotationCenter, RotationAngle,
!     Translation, Descriptor_t, UserDef._t, DataClass, DimensionalUnits
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
            'GridConnectivityProperty_t', 1, 'Periodic_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        if (ier .eq. ALL_OK) then
!           RotationCenter, RotationAngle, Translation can't be deleted,
!           so we expect an error from the API
            call cg_delete_node_f('RotationCenter', ier)
            if (ier .eq. ERROR) call cg_error_print_f

            call cg_delete_node_f('RotationAngle', ier)
            if (ier .eq. ERROR) call cg_error_print_f

            call cg_delete_node_f('Translation',ier)
            if (ier .eq. ERROR) call cg_error_print_f

            call cg_delete_node_f('PeriodicDescriptorName', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_delete_node_f('PeriodicUserData', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

	    call cg_delete_node_f('DataClass', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_delete_node_f('DimensionalUnits', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

	    write(6,*) 'Children of Periodic deleted.  Continue?'
            read(5,600) dummy
        endif

! *** delete children of AverageInterface_t: AverageInterfaceType_t, Descriptor_t, UserDef._t
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
            'GridConnectivityProperty_t', 1, 'AverageInterface_t', 1, &
      	    'end')

        if (ier .eq. ALL_OK) then
            call cg_delete_node_f('AverageInterfaceType', ier)
            if (ier .eq. ERROR) call cg_error_print_f

            call cg_delete_node_f('AverageInterfaceDescriptorName', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_delete_node_f('AverageInterfaceUserData', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

	    write(6,*)'Children of AverageInterface deleted.  Continue?'
            read(5,600) dummy
        endif

! *** delete children of GridConnectivityProperty_t: AverageInterface_t,
!	Periodic_t, Descriptor_t, UserDef._t
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
            'GridConnectivityProperty_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

	if (ier .eq. ALL_OK) then
            call cg_delete_node_f('AverageInterface', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_delete_node_f('Periodic', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_delete_node_f('GridConnPropDescriptorName', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_delete_node_f('GridConnPropUserData', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	    write(6,*)'Children of GridConnProperty deleted.  Continue?'
            read(5,600) dummy
	endif

! *** delete child of GridConnectivity_:  GridConnectivityProperty_t
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
            'ZoneGridConnectivity_t', 1, 'GridConnectivity_t', conn, &
      	    'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	if (ier .eq. ALL_OK) then
            call cg_delete_node_f('GridConnectivityProperty', ier)
            if (ier .eq. ERROR) call cg_error_exit_f

	    write(6,*) 'GridConnectivityProperty deleted.  Continue?'
	    read(5,600) dummy
	endif

! Rewrite everything back

! *** write GridConnectivityProperty_t/AverageInterface_t node under conn#2
        AverageInterfaceType = CGNS_ENUMV(AverageCircumferential)
        call cg_conn_average_write_f(cg, base, zone, &
              conn, AverageInterfaceType, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

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

        write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

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

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

! *** close CGNS file
	call cg_close_f(cg, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

 102    format(a,f8.3)
 200    format(a,i5)
 300    format(3a/a,i2)
 600    format(3a)
	end



