	program read_gravity
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
	integer CellDim, PhysDim
	integer(cgsize_t) size(Ndim*3)
	integer ier, n, cg, nbases, base, type, zone, nzones
	integer ndescriptors, idescr, nuser_data
	integer mass, length, time, temp, deg
	integer narrays, iarray, nndim, datatype, zonetype
	integer(cgsize_t) dim_vals(12)
	real*4 version, rot_rate(Ndim), rot_center(Ndim), data_single(100)
	character*32 name, filename, basename, user_data_name, zonename
	character*100 text

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
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,300)'BaseName = "',basename,'"',  &
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

! *** read RotatingCoordinates_t node
	call cg_rotating_read_f(rot_rate, rot_center, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	write(6,110)'rot_rate=',rot_rate(1), &
                     rot_rate(2), rot_rate(3)
	write(6,110)'rot_center=',rot_center(1), &
                     rot_center(2), rot_center(3)

! *** read children of RotatingCoordinates_t
	call cg_goto_f(cg, base, ier, 'RotatingCoordinates_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

! * Descriptor
 	call cg_ndescriptors_f(ndescriptors, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f
 	write(6,400)'RotatingCoordinates Descriptor_t Information:'
        write(6,105) 'No. of descriptors=',ndescriptors

        do idescr=1, ndescriptors
          call cg_descriptor_read_f(idescr, name, text, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
          write(6,500) ' DescriptorName="',name,'"', &
                             ' DescriptorText="',text,'"'
        enddo

! * DataClass
        call cg_dataclass_read_f(type,ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,600)'DataClassName=',DataClassName(type)

! * DimensionalUnits
        call cg_units_read_f(mass, length, time, temp, deg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        if (ier .eq. ALL_OK) then
          write(6,100) &
            'RotatingCoordinates Dimensional Units:', &
            MassUnitsName(mass), LengthUnitsName(length), &
            TemperatureUnitsName(temp), TimeUnitsName(time), &
            AngleUnitsName(deg)
        endif

! * UserDefinedData
	call cg_nuser_data_f(nuser_data, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,400)'RotatingCoordinates User Data Information:'
	write(6,105) 'No. of UserData=',nuser_data

	do n=1, nuser_data
          call cg_user_data_read_f(n, user_data_name, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
	  write(6,500) ' user_data_name="',user_data_name,'"'
	enddo

! * DataArray_t

	call cg_narrays_f(narrays, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,105) 'RotatingCoordinates contains ', &
                            narrays,' array(s)'
	do iarray=1, narrays
            call cg_goto_f(cg, base, ier, 'RotatingCoordinates_t', &
                           1, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f
            call cg_array_info_f(iarray, name, datatype, &
                               nndim, dim_vals, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            write(6,600) ' DataArrayName="',name,'"'
            write(6,600) ' DataType="',DataTypeName(datatype),'"'
            write(6,200) ' DataNdim=',nndim
            write(6,200) ' DataDim=',dim_vals(1)
            write(6,105) ' Data:'
            call cg_array_read_f(iarray, data_single, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            write(6,106) (data_single(n),n=1,dim_vals(1))

! * Descriptor for DataArray_t (RotationCenter & RotationRateVector)
            call cg_goto_f(cg, base, ier, 'RotatingCoordinates_t', 1,  &
                       'DataArray_t', iarray, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_ndescriptors_f(ndescriptors, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            write(6,400)'Descriptor_t Information:'
            write(6,105) 'No. of descriptors=',ndescriptors

            do idescr=1, ndescriptors
              call cg_descriptor_read_f(idescr, name, text, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              write(6,500) ' DescriptorName="',name,'"', &
                             ' DescriptorText="',text,'"'
            enddo

! * DimensionalUnits for DataArray_t (RotationCenter & RotationRateVector)
            call cg_units_read_f(mass, length, time, temp, deg, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            if (ier .eq. ALL_OK) then
              write(6,100) &
              'DataArray_t Dimensional Units:', &
              MassUnitsName(mass), LengthUnitsName(length), &
              TemperatureUnitsName(temp), TimeUnitsName(time), &
              AngleUnitsName(deg)
            endif
	    write(6,102) ' '
	enddo

! * Zone

        call cg_nzones_f(cg, base, nzones, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,200)'nzones=',nzones

	zone=1

        call cg_zone_read_f(cg, base, zone, zonename, size, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,104)'Name of Zone',zone,' is "',zonename,'"'

	call cg_zone_type_f(cg, base, zone, zonetype, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,600)'  Zone type is ', ZoneTypeName(zonetype)

      ! *** zone attribute:  GOTO zone node
        call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

! ***   zone attribute:  Descriptor
	call cg_ndescriptors_f(ndescriptors, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,400)'Zone Descriptor_t Information:'
        write(6,105) 'No. of descriptors=',ndescriptors

	do idescr=1, ndescriptors
          call cg_descriptor_read_f(idescr, name, text, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
          if (ier.eq.ALL_OK) then
            write(6,500)' DescriptorName="',name,'"', &
                        ' DescriptorText="',text,'"'
          endif
	enddo

! *** read RotatingCoordinates_t node
	call cg_rotating_read_f(rot_rate, rot_center, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	write(6,110)'rot_rate=',rot_rate(1), &
                     rot_rate(2), rot_rate(3)
	write(6,110)'rot_center=',rot_center(1), &
                     rot_center(2), rot_center(3)

! *** read children of RotatingCoordinates_t
	call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                       'RotatingCoordinates_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

! * Descriptor
 	call cg_ndescriptors_f(ndescriptors, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f
 	write(6,400)'RotatingCoordinates Descriptor_t Information:'
        write(6,105) 'No. of descriptors=',ndescriptors

        do idescr=1, ndescriptors
          call cg_descriptor_read_f(idescr, name, text, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
          write(6,500) ' DescriptorName="',name,'"', &
                       ' DescriptorText="',text,'"'
        enddo

! * DataClass
        call cg_dataclass_read_f(type,ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,600)'DataClassName=',DataClassName(type)

! * DimensionalUnits
        call cg_units_read_f(mass, length, time, temp, deg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        if (ier .eq. ALL_OK) then
          write(6,100) &
            'RotatingCoordinates Dimensional Units:', &
            MassUnitsName(mass), LengthUnitsName(length), &
            TemperatureUnitsName(temp), TimeUnitsName(time), &
            AngleUnitsName(deg)
        endif

! * UserDefinedData
	call cg_nuser_data_f(nuser_data, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,400)'RotatingCoordinates User Data Information:'
	write(6,105) 'No. of UserData=',nuser_data

	do n=1, nuser_data
          call cg_user_data_read_f(n, user_data_name, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
	  write(6,500) ' user_data_name="',user_data_name,'"'
	enddo

! * DataArray_t

	call cg_narrays_f(narrays, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,105) 'RotatingCoordinates contains ', &
                            narrays,' array(s)'
	do iarray=1, narrays
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                       'RotatingCoordinates_t', 1, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_array_info_f(iarray, name, datatype, &
                               nndim, dim_vals, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            write(6,600) ' DataArrayName="',name,'"'
            write(6,600) ' DataType="',DataTypeName(datatype),'"'
            write(6,200) ' DataNdim=',nndim
            write(6,200) ' DataDim=',dim_vals(1)
            write(6,105) ' Data:'
            call cg_array_read_f(iarray, data_single, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            write(6,106) (data_single(n),n=1,dim_vals(1))

! * Descriptor for DataArray_t (RotationCenter & RotationRateVector)
            call cg_goto_f(cg, base, ier, 'RotatingCoordinates_t', 1,  &
                       'DataArray_t', iarray, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_ndescriptors_f(ndescriptors, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            write(6,400)'Descriptor_t Information:'
            write(6,105) 'No. of descriptors=',ndescriptors

            do idescr=1, ndescriptors
              call cg_descriptor_read_f(idescr, name, text, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              write(6,500) ' DescriptorName="',name,'"', &
                           ' DescriptorText="',text,'"'
            enddo

! * DimensionalUnits for DataArray_t (RotationCenter & RotationRateVector)
            call cg_units_read_f(mass, length, time, temp, deg, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            if (ier .eq. ALL_OK) then
              write(6,100) &
              'DataArray_t Dimensional Units:', &
              MassUnitsName(mass), LengthUnitsName(length), &
              TemperatureUnitsName(temp), TimeUnitsName(time), &
              AngleUnitsName(deg)
	      write(6,102) ' '
            endif
	enddo

        call cg_close_f(cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

 100 	format(a/,'    Mass units: ',a/,'    Length units: ',a/, &
          '    Temperature units: ',a/,'    Time units: ',a/, &
          '    Angle units:',a)
 102 	format(a,f5.3)
 104    format(a,i5,3a)
 105	format(a,i2,a)
 106    format(6f10.3)
 110	format(a,5f5.1)
 200    format(a,i5)
 300	format(3a/a,i2)
 400	format(/a/)
 500	format(3a/3a)
 600	format(3a)

 9999	end
