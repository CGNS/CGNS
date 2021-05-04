
	program read_axisym
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on June 7 2002

!       This example test the Axisymmetry_t data structure and its children
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 2)
	integer CellDim, PhysDim
	integer ier, n, cg, nbases, base, type
	integer ndescriptors, idescr, nuser_data
	integer mass, length, time, temp, deg, i
        integer narrays, iarray, datatype, nndim, num
	integer(cgsize_t) dim_vals(12)
	real*4 version, ref_point(Ndim), axis(Ndim), angle
	character*32 name, filename, basename, user_data_name
	character*100 text
        character*32 arrayname, CoordinateNames(2)

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

! *** read Axisymmetry_t node
	call cg_axisym_read_f(cg, base, ref_point, axis, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	write(6,110)'ref_point=',ref_point(1),ref_point(2)
	write(6,110)'axis=',axis(1),axis(2)

! *** read children of Axisymmetry_t
	call cg_goto_f(cg, base, ier, 'Axisymmetry_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

! * Descriptor
 	call cg_ndescriptors_f(ndescriptors, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f
 	write(6,400)'Axisymmetry Descriptor_t Information:'
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
           'Axisymmetry Dimensional Units:',&
           MassUnitsName(mass), LengthUnitsName(length),&
           TemperatureUnitsName(temp), TimeUnitsName(time),&
           AngleUnitsName(deg)
        endif

! * UserDefinedData
	call cg_nuser_data_f(nuser_data, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,400)'Axisymmetry User Data Information:'
	write(6,105) 'No. of UserData=',nuser_data

	do n=1, nuser_data
          call cg_user_data_read_f(n, user_data_name, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
	  write(6,500) ' user_data_name="',user_data_name,'"'
	enddo

! * DataArray_t
	call cg_narrays_f(narrays, ier)
        if (ier .ne. ALL_OK) call cg_error_exit_f
        write(6,105) 'No. of DataArray_t=',narrays

	do iarray=1,narrays

            call cg_array_info_f(iarray, arrayname, datatype,&
                     nndim, dim_vals, ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f

            write(6,108)'     DataArray_t #',iarray,':'
            write(6,600)'       Name = ',arrayname
            write(6,600)'       Type = ',DataTypeName(datatype)
            write(6,104)'       Ndim=',nndim
            num = 1
            do i=1,nndim
                write(6,111)'       DataDim(',i,')=',dim_vals(i)
                num = num*dim_vals(i)
            enddo

            if (datatype .eq. CGNS_ENUMV(Character)) then
                call cg_array_read_f(iarray, CoordinateNames, ier)
                if (ier .ne. ALL_OK) call cg_error_exit_f

		write(6,600)'1st coord=',CoordinateNames(1)
		write(6,600)'2nd coord=',CoordinateNames(2)

            else if (datatype .eq. CGNS_ENUMV(RealSingle)) then
	 	if (arrayname.eq.'AxisymmetryAngle') then
		    call cg_array_read_f(iarray, angle, ier)
		    if (ier .ne. ALL_OK) call cg_error_exit_f
		else if (arrayname.eq.'AxisymmetryReferencePoint') then
		    call cg_array_read_f(iarray, ref_point, ier)
		    if (ier .ne. ALL_OK) call cg_error_exit_f
		else if (arrayname.eq.'AxisymmetryAxisVector') then
		    call cg_array_read_f(iarray, axis, ier)
                    if (ier .ne. ALL_OK) call cg_error_exit_f
		endif

	    endif

	enddo     ! loop through arrays
	
        write(6,106)'  ref_point=',(ref_point(i),i=1,2)
        write(6,106)'  axis     =',(axis     (i),i=1,2)
        write(6,106)'  angle    =',angle

! * Axisymmetry_t/DataArray_t/Descriptor
        do iarray=1,narrays
            call cg_goto_f(cg, base, ier, 'Axisymmetry_t', 1,&
                          'DataArray_t', iarray, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

            call cg_ndescriptors_f(ndescriptors, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            write(6,400)'Axisymmetry DataArray # ',iarray,&
                       ' Descriptor Information:'
            write(6,105) 'No. of descriptors=',ndescriptors

            do idescr=1, ndescriptors
              call cg_descriptor_read_f(idescr, name, text, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              write(6,500) ' DescriptorName="',name,'"',&
                            ' DescriptorText="',text,'"'
            enddo

! * Axisymmetry_t/DataArray_t/DimensionalUnits
            call cg_units_read_f(mass, length, time, temp, deg, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            if (ier .eq. ALL_OK) then
              write(6,100) &
               'Axisymmetry DataArray Dimensional Units:',&
               MassUnitsName(mass), LengthUnitsName(length),&
               TemperatureUnitsName(temp), TimeUnitsName(time),&
               AngleUnitsName(deg)
            endif
	enddo


        call cg_close_f(cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

 100 	format(a/,'    Mass units: ',a/,'    Length units: ',a/,&
         '    Temperature units: ',a/,'    Time units: ',a/,&
         '    Angle units:',a)
 102 	format(a,f5.3)
 104	format(a,i2,3a)
 105	format(a,i2,a)
 106    format(a,6f10.3)
 108    format(a,i2,a,i2,a)
 110	format(a,5f5.1)
 111    format(a,i1,a,i8)
 200    format(a,i5)
 300	format(3a/a,i2)
 400	format(/a,i1,a/)
 500	format(3a/3a)
 600	format(3a)

 9999	end
