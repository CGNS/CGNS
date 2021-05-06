
	program read_bprop
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on August 2002

!       This example test the BCProperty_t data structure and its children
!	It tests the functions:
!	- cg_bc_wallfunction_read_f(fn, B, Z, BC, WallFunctionType, ier)
!	- cg_bc_area_read_f(fn, B, Z, BC, AreaType, SurfaceArea, RegionName, ier)
!	And the following:
!	- return value when BCProperty_t doesn't exist, or when WallFunction/Area doesn't exist
!	- printing WallFunctionTypeName, & type<=>name association.
!	- cg_ndescriptors, cg_descriptor_read under BCProperty_t, WallFunction_t, Area_t
!	- cg_nuser_data, cg_user_data_read, under BCProperty_t, WallFunction_t, Area_t
!	- cg_narray, cg_array_read under Area_t
!	- links under BCProperty_t, WallFunction_t, Area_t
!	- cg_goto to BCProperty_t, WallFunction_t, Area_t
! 	- Memory check with Insure
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer ier, cg, nbases, base, nzones, zone, nbocos, boco
	integer CellDim, PhysDim, IndexDim, datatype
	integer(cgsize_t) NormalListFlag
	integer ndescriptors, idescr, zonetype, ndataset
	integer(cgsize_t) size(3*Ndim)
	integer bocotype, ptset_type, NormalIndex(3), AreaType
	integer(cgsize_t) npnts
	integer WallFunctionType, nuser_data, n
	integer(cgsize_t) pnts(100000)
	integer type, mass, length, time, temp, deg
	character*32 filename, basename, zonename, boconame, bpropname
	character*32 name, RegionName, user_data_name
	character*100 text
	real*4 version, data_single(100), SurfaceArea
        double precision data_double(100)

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
            IndexDim=1
        endif

	write(6,104)'  IndexDimension=',IndexDim

! *** read BC_t's
        call cg_nbocos_f(cg, base, zone, nbocos, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,113)nbocos,' bound. conditions found for ', &
                   zonename

        do boco=1, nbocos
            call cg_boco_info_f(cg, base, zone, boco, boconame, &
                   bocotype, ptset_type, npnts, NormalIndex, &
                   NormalListFlag, datatype, ndataset, ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            write(6,105) ' boundary condition #',boco
            write(6,600) '  boconame=',boconame
            write(6,600) '  bocotype=',BCTypeName(bocotype)
            write(6,600) '  ptset_type=', PointSetTypeName(ptset_type)
	    if (NormalIndex(1).eq.0 .and. NormalIndex(2).eq.0 .and. &
                NormalIndex(3).eq.0) then
	        write(6,103) '  NormalIndex undefined'
	    else
                write(6,103) '  NormalIndex=', NormalIndex(1), &
                                NormalIndex(2), NormalIndex(3)
	    endif
            write(6,104) '  NormalListFlag=',NormalListFlag
	    if (NormalListFlag .eq. 1) then
                write(6,600) '  datatype for normals=', &
      		    	        DataTypeName(datatype)
	    endif

           ! read patch points and InwardNormalList
            if (datatype.eq.CGNS_ENUMV(RealSingle) .or. datatype.eq.Null) then
               call cg_boco_read_f(cg, base, zone, boco, pnts, &
                data_single, ier)
               if (ier .eq. ERROR) call cg_error_exit_f
            elseif (datatype.eq.CGNS_ENUMV(RealDouble)) then
               call cg_boco_read_f(cg, base, zone, boco, pnts, &
                data_double, ier)
               if (ier .eq. ERROR) call cg_error_exit_f
            endif

            write(6,119) '   Bound. Condition Patch:', &
             '    first point:', pnts(1),pnts(2),pnts(3), &
             '    last point :', pnts(3*npnts-2), pnts(3*npnts-1), &
                                 pnts(3*npnts)

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
! *** read BCProperty_t node

	    call cg_bc_wallfunction_read_f(cg, base, zone, boco, &
      		WallFunctionType, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

	    if (ier .eq. ALL_OK) then
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	        write(6,600) '   WallFunctionTypeName= "', &
       		    WallFunctionTypeName(WallFunctionType),'"'
	    endif

	    call cg_bc_area_read_f(cg, base, zone, boco, AreaType, &
      		SurfaceArea, RegionName, ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            if (ier .eq. ALL_OK) then
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                write(6,600)'   AreaTypeName= "',AreaTypeName(AreaType), &
      			    '"'
                write(6,600)'   RegionName= "',RegionName,'"'
	 	write(6,102)'   SurfaceArea=',SurfaceArea
	    endif

! *** read Descriptor_t & UserDefinedData_t under...
! ...   BCProperty_t
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'ZoneBC_t', 1, &
            		   'BC_t', boco, 'BCProperty_t', 1, 'end')
	    if (ier .eq. ERROR) call cg_error_exit_f
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

            if (ier .eq. ALL_OK) then

                call cg_ndescriptors_f(ndescriptors, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'BCProperty Descriptor_t Information:'
                write(6,105) 'No. of descriptors=',ndescriptors
                do idescr=1, ndescriptors
                    call cg_descriptor_read_f(idescr, name, text, ier)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' DescriptorName="',name,'"', &
                                 ' DescriptorText="',text,'"'
                enddo

                call cg_nuser_data_f(nuser_data, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'BCProperty User Data Information:'
                write(6,105) 'No. of UserData=',nuser_data
                do n=1, nuser_data
                    call cg_user_data_read_f(n, user_data_name, ier)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' user_data_name="',user_data_name,'"'
                enddo

	    endif

! ...   WallFunction_t
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'ZoneBC_t', 1, &
            'BC_t', boco, 'BCProperty_t', 1, 'WallFunction_t', 1, 'end')
	    if (ier .eq. ERROR) call cg_error_exit_f

            if (ier .eq. ALL_OK) then

                call cg_ndescriptors_f(ndescriptors, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'WallFunction Descriptor_t Information:'
                write(6,105) 'No. of descriptors=',ndescriptors
                do idescr=1, ndescriptors
                    call cg_descriptor_read_f(idescr, name, text, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' DescriptorName="',name,'"', &
                                 ' DescriptorText="',text,'"'
                enddo

                call cg_nuser_data_f(nuser_data, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'WallFunction User Data Information:'
                write(6,105) 'No. of UserData=',nuser_data
                do n=1, nuser_data
                    call cg_user_data_read_f(n, user_data_name, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' user_data_name="',user_data_name,'"'
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                enddo

            endif

! ...   Area_t
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'ZoneBC_t', 1, &
                'BC_t', boco, 'BCProperty_t', 1, 'Area_t', 1, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

            if (ier .eq. ALL_OK) then

                call cg_ndescriptors_f(ndescriptors, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'Area Descriptor_t Information:'
                write(6,105) 'No. of descriptors=',ndescriptors
                do idescr=1, ndescriptors
                    call cg_descriptor_read_f(idescr, name, text, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' DescriptorName="',name,'"', &
                                 ' DescriptorText="',text,'"'
                enddo

                call cg_nuser_data_f(nuser_data, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                write(6,400)'Area User Data Information:'
                write(6,105) 'No. of UserData=',nuser_data
                do n=1, nuser_data
                    call cg_user_data_read_f(n, user_data_name, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
                    write(6,500) ' user_data_name="',user_data_name,'"'
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                enddo

! * Read DataClass & DimensionalUnits under SurfaceArea
		call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
      		    'ZoneBC_t', 1, 'BC_t', boco, 'BCProperty_t', 1, &
      		    'Area_t', 1, 'DataArray_t', 1, 'end')

		call cg_dataclass_read_f(type,ier)
		if (ier .eq. ERROR) call cg_error_exit_f
		write(6,600)'DataClassName=',DataClassName(type)

        	call cg_units_read_f(mass, length, time, temp, deg, ier)
        	if (ier .eq. ERROR) call cg_error_exit_f
        	if (ier .eq. ALL_OK) then
          	    write(6,100) &
            	    'SurfaceArea Dimensional Units:', &
            	    MassUnitsName(mass), LengthUnitsName(length), &
            	    TemperatureUnitsName(temp), TimeUnitsName(time), &
            	    AngleUnitsName(deg)
        	endif

            endif

	enddo ! boco loop

! Close file
        call cg_close_f(cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

 100 	format(a/,'    Mass units: ',a/,'    Length units: ',a/, &
          '    Temperature units: ',a/,'    Time units: ',a/, &
          '    Angle units:',a)
 102 	format(a,f8.3)
 103    format(a,6i2)
 104    format(a,i5,3a)
 105	format(a,i2,a)
 113    format(i1,3a)
 119    format(a/a,3i2/a,3i2)
 200    format(a,i5)
 300	format(3a/a,i2)
 400	format(/a/)
 500	format(3a/3a)
 600	format(3a)

 9999	end

