	program read_cgns_motion
	USE CGNS
        IMPLICIT NONE

! This program reads the CGNS file created with write_cgns_motion
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif
        INTEGER Ndim, Nglobal
	parameter (Ndim = 3)
	parameter (Nglobal = 500)
	integer IndexDim, CellDim, PhysDim
	INTEGER ier, i, zonetype
	integer nbases, nzones
	integer(cgsize_t) size(Ndim*3)
	integer nsols, location
        integer datatype
	character*32 basename, zonename
	integer cg, base, zone, sol
	character*32 name, filename
	integer nndim, rind(6), num
	integer(cgsize_t) dim_vals(12)
	real*4 data_single(100000), version
	double precision data_double(100000)
	integer*4 data_int(100000)
	character data_char(100000)

! Variable for GridMotion_t:
	integer nrmotion, rmotion, rmotiontype
	character*32 rmotionname, arrayname
	INTEGER narrays, iarray
	integer mass, length, time, temp, deg
	integer ndescr, descr
	character*32 descrname, descrtext
	integer namotion, amotion, amotiontype
	character*32 amotionname

! Multiple GridCoordinates_t
	integer grid, ngrids
	character*32 gridname

! *** Base/Zone IterativeData_t nodes
        integer nsteps, step, mzone, start, end
        character*32 bitername, zitername

! Iterative/Time accurate Data
	integer simulation

! *** open file
!	write(6,*) 'Input filename'
!	read(5,600) filename
	write(filename,'(a)')'Test_V2'
	call cg_open_f(filename, MODE_READ, cg, ier)
 	if (ier .ne. ALL_OK) call cg_error_exit_f
	write(6,600)'READING FILE ',filename

! *** CGNS Library Version used for file creation:
	call cg_version_f(cg, version, ier)
	if (ier .ne. ALL_OK) call cg_error_exit_f
	write(6,102) &
          'Library Version used for file creation:',version

! *** base
	call cg_nbases_f(cg, nbases, ier)
	if (ier .ne. ALL_OK) call cg_error_exit_f
	write(6,200)'nbases=',nbases
	
	do base=1, nbases

	  call cg_base_read_f(cg, base, basename, CellDim, PhysDim, ier)
	  if (ier .ne. ALL_OK) call cg_error_exit_f
	  write(6,300)'BaseName = "',basename,'"',  &
                      'cell_dimension=',Celldim, &
                      'physical_dimension=',PhysDim

! *** simulation type
	  call cg_simulation_type_read_f(cg, base, simulation, ier)
	  if (ier .ne. ALL_OK) call cg_error_exit_f
	  write(6,600)' Simulation Type is ', &
                        SimulationTypeName(simulation)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	
! *** base iterative data
	  call cg_biter_read_f(cg, base, bitername, nsteps, ier)
	  if (ier .eq. ERROR) call cg_error_exit_f
	  if (ier .eq. ALL_OK) then
	      write(6,300)' BaseIterativeData_t name ="',bitername,'"'
	      write(6,200)'   nsteps=',nsteps

        ! *** Data arrays under BaseIterativeData_t node
	      call cg_goto_f(cg, base, ier, 'BaseIterativeData_t', &
                             1, 'end')
	      if (ier .ne. ALL_OK) call cg_error_exit_f

              call cg_narrays_f(narrays, ier)
              if (ier .ne. ALL_OK) call cg_error_exit_f
              write(6,105) '   ',narrays,' DataArray_t nodes(s)'

              do iarray=1,narrays
		
              	call cg_array_info_f(iarray, arrayname, datatype, &
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
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

                if (datatype .eq. CGNS_ENUMV(Character)) then
                    call cg_array_read_f(iarray, data_char, ier)
                    if (ier .ne. ALL_OK) call cg_error_exit_f
		    if (arrayname(1:12).eq.'ZonePointers') then
		      do step=1, nsteps
		        do mzone=1,dim_vals(2)
			  start = ((step-1)*dim_vals(2)+(mzone-1))*32+1
			  end=start+32-1
			  write(6,601)'step ',step,' zone ',mzone, &
                                      ' is ',(data_char(i),i=start,end)
			enddo
		      enddo
		    endif

		else if (datatype .eq. CGNS_ENUMV(RealSingle)) then
		    call cg_array_read_f(iarray, data_single, ier)
		    if (ier .ne. ALL_OK) call cg_error_exit_f
		    write(6,106)'       data=',(data_single(i),i=1,num)

	        else if (datatype .eq. CGNS_ENUMV(RealDouble)) then
                    call cg_array_read_f(iarray, data_double, ier)
                    if (ier .ne. ALL_OK) call cg_error_exit_f
                    write(6,106)'       data=',(data_double(i),i=1,num)

		else if (datatype .eq. CGNS_ENUMV(Integer)) then
		    call cg_array_read_f(iarray, data_int, ier)
		    if (ier .ne. ALL_OK) call cg_error_exit_f
		    write(6,128)'       data=',(data_int(i),i=1,num)
                endif
	      enddo	! loop through arrays
	  endif		! if BaseIterativeData_t is defined

! *** zone
	  call cg_nzones_f(cg, base, nzones, ier)
	  if (ier .ne. ALL_OK) call cg_error_exit_f
	  write(6,200)'nzones=',nzones

	  do zone=1, nzones

	    call cg_zone_read_f(cg, base, zone, zonename, size, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f
	    write(6,104)'Name of Zone',zone,' is "',zonename,'"'

	    call cg_zone_type_f(cg, base, zone, zonetype, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f
	    write(6,600)'  Zone type is ', ZoneTypeName(zonetype)

	    if (zonetype.eq.CGNS_ENUMV(Structured)) then
                 IndexDim=CellDim
	    else
                 IndexDim=1
            endif
	    write(6,104)'  IndexDimension=',IndexDim

! *** zone iterative data
            call cg_ziter_read_f(cg, base, zone, zitername, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            if (ier .eq. ALL_OK) then
              write(6,300)' ZoneIterativeData_t name ="',zitername,'"'

        ! *** Data arrays under BaseIterativeData_t node
              call cg_goto_f(cg, base, ier, 'Zone_t', zone,  &
                             'ZoneIterativeData_t', 1, 'end')
              if (ier .ne. ALL_OK) call cg_error_exit_f

              call cg_narrays_f(narrays, ier)
              if (ier .ne. ALL_OK) call cg_error_exit_f
              write(6,105) '   ',narrays,' DataArray_t nodes(s)'

              do iarray=1,narrays

                call cg_array_info_f(iarray, arrayname, datatype, &
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
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

                if (datatype .eq. CGNS_ENUMV(Character) ) then
                    call cg_array_read_f(iarray, data_char, ier)
                    if (ier .ne. ALL_OK) call cg_error_exit_f
                    if (arrayname(1:23).eq.'GridCoordinatesPointers'.or. &
                      arrayname(1:20).eq.'FlowSolutionPointers'.or. &
                      arrayname(1:23).eq.'RigidGridMotionPointers'.or. &
                      arrayname(1:27).eq.'ArbitraryGridMotionPointers') &
                      then
                      do step=1, nsteps
                          start = (step-1)*32+1
                          end=start+32-1
                          write(6,602)'step ',step, &
                                      ' is ',(data_char(i),i=start,end)
                      enddo
                    endif
		endif
              enddo     ! loop through arrays
	    endif	! if ZoneIterativeData_t exist
! ***

! *** Multiple GridCoordinates_t Nodes
	    call cg_ngrids_f(cg, base, zone, ngrids, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f

            write(6,113) ngrids,' GridCoordinates_t node(s)', &
                    'found for ',zonename
	
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	    do grid=1, ngrids
	! *** GridCoordinates_t info
	      call cg_grid_read_f(cg, base, zone, grid, gridname, ier)
	      if (ier .ne. ALL_OK) call cg_error_exit_f

              write(6,108)' GridCoordinates_t #',grid,':'
              write(6,600)'   Name = ',gridname

        ! *** GOTO GridCoordinates_t node
              call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                             'GridCoordinates_t', grid, 'end')
       	      if (ier .ne. ALL_OK) call cg_error_exit_f

        ! *** Read coordinate arrays
	      call cg_narrays_f(narrays, ier)
	      if (ier .ne. ALL_OK) call cg_error_exit_f
	      write(6,105) '   ',narrays,' DataArray_t nodes(s)'

	      do iarray=1,narrays

	  ! *** GridCoordinates_t attribute: DataArray_t
                call cg_array_info_f(iarray, name, datatype, &
                                     nndim, dim_vals, ier)
                if (ier .ne. ALL_OK) call cg_error_exit_f
                write(6,600)' DataArrayName="',name,'"'
                write(6,600)' DataType=',DataTypeName(datatype)
                write(6,104)' DataNdim=',nndim
		do i=1,nndim
                  write(6,111)' DataDim(',i,')=',dim_vals(i)
		enddo

	  ! *** Compute nr of data in data array:
	   	num = 1
		do i=1,nndim
		  num = num*dim_vals(i)
		enddo

                if (datatype .eq. CGNS_ENUMV(RealSingle)) then
                  call cg_array_read_f(iarray, data_single, ier)
                  if (ier .ne. ALL_OK) call cg_error_exit_f
                  write(6,106) 'first pts:',(data_single(i),i=1,2)
                  write(6,106) 'last pts:',(data_single(i),i=num-1,num)
                elseif (datatype .eq. CGNS_ENUMV(RealDouble)) then
                  call cg_array_read_f(iarray, data_double, ier)
                  if (ier .ne. ALL_OK) call cg_error_exit_f
                  write(6,106) 'first pts:',(data_double(i),i=1,2)
                  write(6,106) 'last pts:',(data_double(i),i=num-1,num)
                endif

	      enddo	! loop through data arrays

	    enddo 	! loop through GridCoordinates_t nodes

            write(6,400)'                             *     *     *'
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

! *** solution
	
	    call cg_nsols_f(cg, base, zone, nsols, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f
	    write(6,113) nsols,' FlowSolution_t node(s)', &
                    'found for ',zonename

      ! *** Read solution with general cg_array_read function
	    do sol=1, nsols
	      call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                  'FlowSolution_t', sol, 'end')
	      if (ier .ne. ALL_OK) call cg_error_exit_f

	! *** FlowSolution_t attribute:  DataArray_t
	      call cg_narrays_f(narrays, ier)
	      if (ier .ne. ALL_OK) call cg_error_exit_f
              write(6,108) ' FlowSolution_t #',sol, &
                    ' contains ',narrays,' solution arrays'

	! *** FlowSolution_t attribute:  GridLocation
	      call cg_gridlocation_read_f(location, ier)
	      if (ier .eq. ERROR) call cg_error_exit_f
	      write(6,600)'  The solution data are recorded at the ', &
                       GridLocationName(location)

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	! *** FlowSolution_t attribute:  Rind
	      call cg_rind_read_f(rind, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
	      write(6,103)'  The Rind Data is ',(rind(i),i=1,6)

              do iarray=1,narrays
	        call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                  'FlowSolution_t', sol, 'end')
                if (ier .ne. ALL_OK) call cg_error_exit_f

                call cg_array_info_f(iarray, name, datatype, &
                                     nndim, dim_vals, ier)
                if (ier .ne. ALL_OK) call cg_error_exit_f
	        write(6,114) '  DataArray #',iarray
                write(6,600) '   Name="',name,'"'
                write(6,600) '   DataType=',DataTypeName(datatype)
                write(6,103) '   DataNdim=',nndim
                do i=1,nndim
                  write(6,111)'   DataDim(',i,')=',dim_vals(i)
                enddo

                if (datatype .eq. CGNS_ENUMV(RealSingle)) then
                  call cg_array_read_f(iarray, data_single, ier)
                  if (ier .ne. ALL_OK) call cg_error_exit_f
                  write(6,106) '   first pts:',(data_single(i),i=1,2)
                elseif (datatype .eq. CGNS_ENUMV(RealDouble)) then
                  call cg_array_read_f(iarray, data_double, ier)
                  if (ier .ne. ALL_OK) call cg_error_exit_f
                  write(6,106) '   first pts:',(data_double(i),i=1,2)
                endif

	      enddo	! loop through DataArray_t
	      write(6,103)' '

            enddo	! loop through FlowSolution_t

            write(6,400)'                             *     *     *'


! *** Rigid Grid Motion
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	    call cg_n_rigid_motions_f(cg, base, zone, nrmotion, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f
            write(6,113) nrmotion,' RigidGridMotion_t node(s)', &
                    'found for ',zonename

	    do rmotion=1, nrmotion
	        call cg_rigid_motion_read_f(cg, base, zone, rmotion, &
      		    rmotionname, rmotiontype, ier)
	        if (ier .ne. ALL_OK) call cg_error_exit_f

                write(6,108)' RigidGridMotion_t #',rmotion,':'
	        write(6,600)'   Name = ',rmotionname
	        write(6,600)'   Type = ', &
                             RigidGridMotionTypeName(rmotiontype)

                call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                               'RigidGridMotion_t', rmotion, 'end')
                if (ier .ne. ALL_OK) call cg_error_exit_f

		call cg_ndescriptors_f(ndescr, ier)
		if (ier .ne. ALL_OK) call cg_error_exit_f
		write(6,105) '   ',ndescr, ' Descriptor_t node(s)'

		do descr=1, ndescr
		    call cg_descriptor_read_f(descr, descrname,  &
                         descrtext, ier)
		    if (ier .ne. ALL_OK) call cg_error_exit_f
		    write(6,108)'     Descriptor_t #',descr,':'
		    write(6,600)'       Name = ',descrname
		    write(6,600)'       Text = ',descrtext
		enddo

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
! *** Data arrays under RigidGridMotion_t node
	        call cg_narrays_f(narrays, ier)
	        if (ier .ne. ALL_OK) call cg_error_exit_f
		write(6,105) '   ',narrays,' DataArray_t nodes(s)'

		do iarray=1,narrays
		    call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                               'RigidGridMotion_t', rmotion, 'end')

		    call cg_array_info_f(iarray, arrayname, datatype, &
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
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

                    if (datatype .eq. CGNS_ENUMV(RealSingle)) then
                        call cg_array_read_f(iarray, data_single, ier)
                        if (ier .ne. ALL_OK) call cg_error_exit_f
                  	write(6,106)'       first pts:', &
                                     (data_single(i),i=1,dim_vals(1))
		    else if (datatype .eq. CGNS_ENUMV(RealDouble)) then
			call cg_array_read_f(iarray, data_double, ier)
		  	if (ier .ne. ALL_OK) call cg_error_exit_f
			write(6,106)'       first pts:', &
                                     (data_double(i),i=1,dim_vals(1))
		    endif

		    call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                               'RigidGridMotion_t', rmotion, &
                               'DataArray_t', iarray, 'end')
		    if (ier .ne. ALL_OK) call cg_error_exit_f

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
		    call cg_units_read_f(mass,length,time,temp,deg,ier)
              	    if (ier .eq. ERROR) call cg_error_exit_f
              	    if (ier .eq. ALL_OK) then
                	write(6,100) &
                  	'       Dimensional Units:', &
                  	MassUnitsName(mass), LengthUnitsName(length), &
                  	TemperatureUnitsName(temp), TimeUnitsName(time), &
                  	AngleUnitsName(deg)
		    endif
		enddo	! loop through data arrays
	    enddo	! loop through rmotion
            write(6,400)'                             *     *     *'

	
! *** Read arbitrary grid motion
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            call cg_n_arbitrary_motions_f(cg, base, zone, namotion, ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f
            write(6,113) namotion,' ArbitraryGridMotion_t node(s)', &
                    'found for ',zonename

            do amotion=1, namotion
                call cg_arbitrary_motion_read_f(cg, base, zone, amotion, &
                    amotionname, amotiontype, ier)
                if (ier .ne. ALL_OK) call cg_error_exit_f

                write(6,108)' ArbitraryGridMotion_t #',amotion,':'
                write(6,600)'   Name = ',amotionname
                write(6,600)'   Type = ', &
                             ArbitraryGridMotionTypeName(amotiontype)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

                call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                           'ArbitraryGridMotion_t', amotion, 'end')
                if (ier .ne. ALL_OK) call cg_error_exit_f

                call cg_ndescriptors_f(ndescr, ier)
                if (ier .ne. ALL_OK) call cg_error_exit_f
                write(6,105) '   ',ndescr, ' Descriptor_t node(s)'

                do descr=1, ndescr
                    call cg_descriptor_read_f(descr, descrname, &
                         descrtext, ier)
                    if (ier .ne. ALL_OK) call cg_error_exit_f
                    write(6,108)'     Descriptor_t #',descr,':'
                    write(6,600)'       Name = ',descrname
                    write(6,600)'       Text = ',descrtext
                enddo
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
! *** Data arrays under ArbitraryGridMotion_t node
                call cg_narrays_f(narrays, ier)
                if (ier .ne. ALL_OK) call cg_error_exit_f
                write(6,105) '   ',narrays,' DataArray_t nodes(s)'

                do iarray=1,narrays
                    call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                           'ArbitraryGridMotion_t', amotion, 'end')

                    call cg_array_info_f(iarray, arrayname, datatype, &
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
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

                    if (datatype .eq. CGNS_ENUMV(RealSingle)) then
                        call cg_array_read_f(iarray, data_single, ier)
                        if (ier .ne. ALL_OK) call cg_error_exit_f
                        write(6,106)'       first pts:', &
                                     (data_single(i),i=1,dim_vals(1))
		    else if (datatype .eq. CGNS_ENUMV(RealDouble)) then
                        call cg_array_read_f(iarray, data_double, ier)
                        if (ier .ne. ALL_OK) call cg_error_exit_f
                        write(6,106)'       first pts:', &
                                     (data_double(i),i=1,dim_vals(1))
                    endif

                    call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                           'ArbitraryGridMotion_t', amotion, &
                               'DataArray_t', iarray, 'end')
                    if (ier .ne. ALL_OK) call cg_error_exit_f

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                    call cg_units_read_f(mass,length,time,temp,deg,ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    if (ier .eq. ALL_OK) then
                        write(6,100) &
                        '       Dimensional Units:', &
                        MassUnitsName(mass), LengthUnitsName(length), &
                        TemperatureUnitsName(temp), TimeUnitsName(time), &
                        AngleUnitsName(deg)
                    endif
                enddo   ! loop through data arrays
            enddo       ! loop through amotion
            write(6,400)'                             *     *     *'
	  enddo					! loop through zones
 	enddo    				! loop through bases

        call cg_close_f(cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

 100 	format(a/,'        Mass units: ',a/, &
                  '        Length units: ',a/, &
                  '        Temperature units: ',a/, &
                  '        Time units: ',a/, &
                  '        Angle units:',a)
 101	format(a,i1,a,/2a,/2a,/2a,/3a,/a,i4,3a,/2a,/2a,/2a,/a,i4)
 102 	format(a,f5.3)
 103	format(a,6i2)
 104	format(a,i2,3a)
 105	format(a,i2,a)
 106    format(a,6f10.3)
 107	format(i2,2a)
 108    format(a,i2,a,i2,a)
 109	format(a,f5.1)
 110	format(a,5f5.1)
 111	format(a,i1,a,i8)
 112	format(a,i1/2a/3a)
 113	format(i1,3a)
 114	format(/a, i1)
 115	format(a,i1,a/3a/2a)
 116	format(a,i1,a,i1,a)
 117	format(/i4,2a)
 118	format(a,i1,a/3a/2a/a,i1,a,i5)
 119	format(a/a,3i2/a,3i2)
 120	format(a10, 3(a1,i1),a6,3(i1,a1))
 121 	format(a16,3(a1,i1),a6,3(i1,a1))
 122	format(a12,3(a1,i2),a1)
 124	format(4x, f7.2)
 126	format(a/a,3f5.2/a,3f5.2)
 127	format(2a,i1,a)
 128	format(a,6i5)
 130	format(a15, i2, a4)
 131	format(a10, 3(a1,i1),a6,3(i1,a1))
 132	format(a16,3(a1,i1),a6,3(i1,a1))
 133	format(a12,3(a1,i2),a1)
 134	format(a,6f6.2)
 200    format(a,i3)
 300	format(3a/a,i2,/a,i2)
 400	format(/a/)
 401	format(/2a/)
 500	format(3a/3a)
 600	format(3a)
 601	format(7x,a,i2,a,i2,33a)
 602	format(7x,a,i2,33a)

 	end
