
	program read_cprop
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on August 2002

!       This example test the GridConnectivityProperty_t data structure and its children
!	It tests the functions:
!	- cg_conn_average_read_f(fn, B, Z, I, AverageInterfaceType, ier)
!	- cg_conn_periodic_read_f(fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier)
!	And the following:
!	- return value when GridConnectivityProperty_t doesn't exist, or when AverageInterface/Periodic doesn't exist
!	- printing AverageInterfaceTypeName, & type<=>name association.
!	- cg_ndescriptors, cg_descriptor_read under GridConnectivityProperty_t, AverageInterface_t, Periodic_t
!	- cg_nuser_data, cg_user_data_read, under GridConnectivityProperty_t, AverageInterface_t, Periodic_t
!	- cg_narray, cg_array_read under Periodic_t
!	- links under GridConnectivityProperty_t, AverageInterface_t, Periodic_t
!	- cg_goto to GridConnectivityProperty_t, AverageInterface_t, Periodic_t
! 	- Memory check with Insure
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer ier, cg, nbases, base, nzones, zone, nconns, conn
	integer CellDim, PhysDim, IndexDim
	integer ndescriptors, idescr, zonetype
	integer(cgsize_t) size(3*Ndim)
	integer ptset_type, ndonor_ptset_type, npnts, npnts_donor
	integer location, nzonetype, donor_datatype
	integer AverageInterfaceType, nuser_data, n
	integer type, mass, length, time, temp, deg
	character*32 filename, basename, zonename
	character*32 name, user_data_name, connectname, donorname
	character*100 text
	real*4 version, Translation(Ndim)
	real *4 RotationCenter(Ndim), RotationAngle(Ndim)
	integer(cgsize_t) range(Ndim, 2), donor_range(Ndim, 2)
 	integer transform(Ndim)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

! *** open file
!	write(6,*) 'Input filename'
!	read(5,600) filename
 	write(filename,'(a)')'Test_V2'
	call cg_open_f(filename, MODE_READ, cg, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,600)'READING FILE ',filename

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

	call cg_base_read_f(cg, base, basename, CellDim, PhysDim, ier)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,300)'BaseName = "',basename,'"', &
                      'cell_dimension=',CellDim

! *** base attribute:  GOTO base node
	call cg_goto_f(cg, base, ier, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

! ***   base attribute:  Descriptor
	call cg_ndescriptors_f(ndescriptors, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,400)'Base Descriptor_t Information:'
        write(6,105) 'No. of descriptors=',ndescriptors

	do idescr=1, ndescriptors
          call cg_descriptor_read_f(idescr, name, text, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
          if (ier.eq.ALL_OK) then
            write(6,500)' DescriptorName="',name,'"', &
                        ' DescriptorText="',text,'"'
          endif
	enddo

! *** read Zone
        call cg_nzones_f(cg, base, nzones, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,200)'nzones=',nzones

	zone = 1

	call cg_zone_read_f(cg, base, zone, zonename, size, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,104)'Name of Zone',zone,' is "',zonename,'"'

        call cg_zone_type_f(cg, base, zone, zonetype, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,600)'  Zone type is ', ZoneTypeName(zonetype)
	
        if (zonetype.eq.CGNS_ENUMV(Structured)) then
            IndexDim=CellDim
        else
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            IndexDim=1
        endif

	write(6,104)'  IndexDimension=',IndexDim

! *** read GridConnectivity1to1_t's
        call cg_n1to1_f(cg, base, zone, nconns, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,113)nconns,' GridConnectivity1to1_t found for ', &
                   zonename

        do conn=1, nconns
            call cg_1to1_read_f(cg, base, zone, conn, connectname, &
               donorname, range, donor_range, transform, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            write(6, 101) &
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
              '  GridConnectivity1to1 #',conn,':', &
              '   connect name=',connectname, &
              '   donorname="',donorname,'"'

	    write(6,120) ' range: ', &
                 '(',range(1,1),',',range(2,1),',',range(3,1), &
                 ') to (',range(1,2),',',range(2,2),',',range(3,2),')'

                write(6,121)' donor_range: ', &
                '(', donor_range(1,1), ',', donor_range(2,1), ',', &
                  donor_range(3,1), ') to (', &
                  donor_range(1,2), ',', donor_range(2,2), ',', &
                  donor_range(3,2), ')'

                write(6,122) ' Transform: ', '(', &
                  transform(1), ',', &
                  transform(2), ',', transform(3), ')'

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
! *** read GridConnectivityProperty_t node

	    call cg_1to1_average_read_f(cg, base, zone, conn, &
      		AverageInterfaceType, ier)
!	    if (ier .ne. ALL_OK) call cg_error_print_f
	    if (ier .eq. ERROR) call cg_error_exit_f

	    if (ier .eq. ALL_OK) then
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	        write(6,600) '   AverageInterfaceTypeName= "', &
       		    AverageInterfaceTypeName(AverageInterfaceType),'"'
	    endif

	    call cg_1to1_periodic_read_f(cg, base, zone, conn, &
      		RotationCenter, RotationAngle, Translation, ier)
!           if (ier .ne. ALL_OK) call cg_error_print_f
	    if (ier .eq. ERROR) call cg_error_exit_f

            if (ier .eq. ALL_OK) then
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	 	write(6,110)'   RotationCenter=',RotationCenter(1), &
      				RotationCenter(2), RotationCenter(3)
                write(6,110)'   RotationAngle=',RotationAngle(1), &
                                RotationAngle(2), RotationAngle(3)
                write(6,110)'   Translation=',Translation(1), &
                                Translation(2), Translation(3)
	    endif

! *** read Descriptor_t & UserDefinedData_t under...
! ...   GridConnectivityProperty_t
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
      		'ZoneGridConnectivity_t', 1, 'GridConnectivity1to1_t',conn, &
      		'GridConnectivityProperty_t', 1, 'end')
!	    if (ier .ne. ALL_OK) call cg_error_print_f
	    if (ier .eq. ERROR) call cg_error_exit_f
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

            if (ier .eq. ALL_OK) then

                call cg_ndescriptors_f(ndescriptors, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'GridConnProperty Descriptor_t Information:'
                write(6,105) 'No. of descriptors=',ndescriptors
                do idescr=1, ndescriptors
                    call cg_descriptor_read_f(idescr, name, text, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' DescriptorName="',name,'"', &
                                 ' DescriptorText="',text,'"'
                enddo

                call cg_nuser_data_f(nuser_data, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'GridConnProperty User Data Information:'
                write(6,105) 'No. of UserData=',nuser_data
                do n=1, nuser_data
                    call cg_user_data_read_f(n, user_data_name, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' user_data_name="',user_data_name,'"'
                enddo

	    endif

! ...   AverageInterface_t
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
       		'ZoneGridConnectivity_t', 1, &
                'GridConnectivity1to1_t', conn, &
      		'GridConnectivityProperty_t', 1, 'AverageInterface_t', &
      		1, 'end')
!	    if (ier .ne. ALL_OK) call cg_error_print_f
	    if (ier .eq. ERROR) call cg_error_exit_f

            if (ier .eq. ALL_OK) then

                call cg_ndescriptors_f(ndescriptors, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'AverageInterface Descriptor_t Information:'
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                write(6,105) 'No. of descriptors=',ndescriptors
                do idescr=1, ndescriptors
                    call cg_descriptor_read_f(idescr, name, text, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' DescriptorName="',name,'"', &
                                 ' DescriptorText="',text,'"'
                enddo

                call cg_nuser_data_f(nuser_data, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'AverageInterface User Data Information:'
                write(6,105) 'No. of UserData=',nuser_data
                do n=1, nuser_data
                    call cg_user_data_read_f(n, user_data_name, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' user_data_name="',user_data_name,'"'
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                enddo

            endif

! ...   Periodic_t
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
      		'ZoneGridConnectivity_t', 1, &
      	        'GridConnectivity1to1_t', conn, &
      		'GridConnectivityProperty_t', 1, 'Periodic_t', 1, 'end')
!           if (ier .ne. ALL_OK) call cg_error_print_f
	    if (ier .eq. ERROR) call cg_error_exit_f

            if (ier .eq. ALL_OK) then

                call cg_ndescriptors_f(ndescriptors, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'Periodic Descriptor_t Information:'
                write(6,105) 'No. of descriptors=',ndescriptors
                do idescr=1, ndescriptors
                    call cg_descriptor_read_f(idescr, name, text, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' DescriptorName="',name,'"', &
                                 ' DescriptorText="',text,'"'
                enddo

                call cg_nuser_data_f(nuser_data, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'Periodic User Data Information:'
                write(6,105) 'No. of UserData=',nuser_data
                do n=1, nuser_data
                    call cg_user_data_read_f(n, user_data_name, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' user_data_name="',user_data_name,'"'
                enddo

! * Read DataClass & DimensionalUnits under RotationCenter
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
		call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
      		    'ZoneGridConnectivity_t', 1, &
                    'GridConnectivity1to1_t', conn, &
                    'GridConnectivityProperty_t', 1, &
      		    'Periodic_t', 1, 'DataArray_t', 1, 'end')

		call cg_dataclass_read_f(type,ier)
		if (ier .eq. ERROR) call cg_error_exit_f
		write(6,600)'DataClassName=',DataClassName(type)

        	call cg_units_read_f(mass, length, time, temp, deg, ier)
        	if (ier .eq. ERROR) call cg_error_exit_f
        	if (ier .eq. ALL_OK) then
          	    write(6,100) &
            	    'SurfacePeriodic Dimensional Units:', &
            	    MassUnitsName(mass), LengthUnitsName(length), &
            	    TemperatureUnitsName(temp), TimeUnitsName(time), &
            	    AngleUnitsName(deg)
        	endif

            endif

	enddo ! conn loop

! Close file
        call cg_close_f(cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

 100 	format(a/,'    Mass units: ',a/,'    Length units: ',a/, &
          '    Temperature units: ',a/,'    Time units: ',a/, &
          '    Angle units:',a)
 101    format(a,i1,a,/2a,/2a,/2a,/3a,/a,i4,3a,/2a,/2a,/2a,/a,i4)
 102 	format(a,f8.3)
 103    format(a,6i2)
 104    format(a,i5,3a)
 105	format(a,i2,a)
 110    format(a,5f5.1)
 113    format(i1,3a)
 119    format(a/a,3i2/a,3i2)
 120	format(a10, 3(a1,i1),a6,3(i1,a1))
 121 	format(a16,3(a1,i1),a6,3(i1,a1))
 122	format(a12,3(a1,i2),a1)
 200    format(a,i5)
 300	format(3a/a,i2)
 400	format(/a/)
 500	format(3a/3a)
 600	format(3a)

 9999	end

