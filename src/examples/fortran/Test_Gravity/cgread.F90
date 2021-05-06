
	program read_gravity
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on May 31 2002

!       This example test the Gravity_t data structure and its children
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer CellDim, PhysDim
	integer ier, n, cg, nbases, base, type
	integer ndescriptors, idescr, nuser_data
	integer mass, length, time, temp, deg
	real*4 version, gravity_vector(Ndim)
	character*32 name, filename, basename, user_data_name
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

! *** read Gravity_t node
	call cg_gravity_read_f(cg, base, gravity_vector, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	write(6,110)'gravity_vector=',gravity_vector(1), &
                     gravity_vector(2), gravity_vector(3)

! *** read children of Gravity_t
	call cg_goto_f(cg, base, ier, 'Gravity_t', 1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

! * Descriptor
 	call cg_ndescriptors_f(ndescriptors, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f
 	write(6,400)'Gravity Descriptor_t Information:'
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
            'Gravity Dimensional Units:', &
            MassUnitsName(mass), LengthUnitsName(length), &
            TemperatureUnitsName(temp), TimeUnitsName(time), &
            AngleUnitsName(deg)
        endif

! * UserDefinedData
	call cg_nuser_data_f(nuser_data, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,400)'Gravity User Data Information:'
	write(6,105) 'No. of UserData=',nuser_data

	do n=1, nuser_data
          call cg_user_data_read_f(n, user_data_name, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
	  write(6,500) ' user_data_name="',user_data_name,'"'
	enddo

! * GravityVector/Descriptor
        call cg_goto_f(cg, base, ier, 'Gravity_t', 1, 'DataArray_t', &
                       1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_ndescriptors_f(ndescriptors, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,400)'Gravity Vector Descriptor_t Information:'
        write(6,105) 'No. of descriptors=',ndescriptors

        do idescr=1, ndescriptors
          call cg_descriptor_read_f(idescr, name, text, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
          write(6,500) ' DescriptorName="',name,'"', &
                             ' DescriptorText="',text,'"'
        enddo

! * GravityVector/DimensionalUnits
        call cg_units_read_f(mass, length, time, temp, deg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        if (ier .eq. ALL_OK) then
          write(6,100) &
            'Gravity Vector Dimensional Units:', &
            MassUnitsName(mass), LengthUnitsName(length), &
            TemperatureUnitsName(temp), TimeUnitsName(time), &
            AngleUnitsName(deg)
        endif
        call cg_close_f(cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

 100 	format(a/,'    Mass units: ',a/,'    Length units: ',a/, &
          '    Temperature units: ',a/,'    Time units: ',a/, &
          '    Angle units:',a)
 102 	format(a,f5.3)
 105	format(a,i2,a)
 110	format(a,5f5.1)
 200    format(a,i5)
 300	format(3a/a,i2)
 400	format(/a/)
 500	format(3a/3a)
 600	format(3a)

 9999	end
