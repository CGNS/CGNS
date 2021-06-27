
        program read_mixed_grid
        USE CGNS

!	author: Diane Poirier
!       last revised on October 12 2001

! 	This example test the new data structures created for
! 	unstructured data and geometry reference data.  The
! 	model being read is composed of 2 zones.  The 1st
!	one is a structured 3x3x3 block, and the 2nd is
!	an unstructured 3x3x3 block composed of 8 hexa elements.
! 	The 2 zones interface (Abutting1to1) on one 9-nodes face.
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

        parameter (NNODES=200000, NELEMENTS=200000)

	integer Cdim, Pdim, Idim, Idim_donor
	integer cg, base, zone, ier, ZoneType
        integer(cgsize_t) size(3*3)
	integer nbases, nzones, ncoords, nsections, nconns, nbocos
	integer nfambc, ngeo, nfamilies, nparts
        integer(cgsize_t) dim_vals(12)
	integer i, n, sect
        integer(cgsize_t) range_min(3), range_max(3)
	integer(cgsize_t) start, end
        integer nbndry, type
	integer(cgsize_t) pnts(3*NNODES), donor_data(3*NNODES)
	integer fam, geo, part,bc, bctype, location, connect_type
	integer ptset_type, datatype
        integer(cgsize_t) npnts, ndata_donor
	integer donor_ptset_type, donor_datatype, donor_zonetype
	character*32 nodename, cadformat, filename, donorname
	character*32 coordname(3)
	character*200 cadfile, descriptor
        double precision data_double(NNODES)
	integer NormalIndex(3), ndataset
        integer(cgsize_t) NormalListFlag
	integer size_of_patch, multiplier, narrays, pos, parent_flag
	integer ilen, ilen2
	integer ordinal
        integer bcdataset, nbcdatasets, dirichletflag, neumannflag
        integer mass, length, time, temp, deg
        character*32 dataset_name
        real*4 rot_rate(3), rot_center(3)

! The following seg fault on hp11...
!       integer elements(NELEMENTS*8), parent_data(NELEMENTS*4)
!	double precision NormalListDouble(3*NNODES)
!	real*4 NormalListSingle(3*NNODES)

! Replace with the following fixed size, but without understanding the problem
	integer(cgsize_t) elements(8000), parent_data(4000)
	integer(cgsize_t) elements_offset(8000)
	double precision NormalListDouble(3000)
	real*4 NormalListSingle(3000)

!       initialize
        ier = 0

!       write(6,100) 'Input filename'
!       read(5,100) filename
!       open CGNS file for reading
        write(filename,100)'Test_V2'
 100	format(a)

 	call cg_open_f(filename, MODE_READ, cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
	write(6,101)'File Opened and Read '
 101	format(/a/)

!*******read CGNSBase
	call cg_nbases_f(cg, nbases, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	if (nbases .gt. 1) then
	    write(6,100)'This program reads only the first base'
	else if (nbases .le. 0) then
	    write(6,100)'No base found'
	    goto 9999
	endif
	base = 1
	call cg_base_read_f(cg, base, nodename, Cdim, Pdim, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	ilen=32
        call no_blank(nodename, ilen)
        write(6,100)'*** CGNSBase_t node ***'
        write(6,102)'Name= "',nodename(1:ilen),'"'
        write(6,103)'CellDimension=',Cdim
        write(6,103)'PhysDimension=',Pdim
	write(6,100)' '

! FAMILY and GEOMETRY
!****** Read number of families and family names
	call cg_nfamilies_f(cg, base, nfamilies, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

        do fam=1,nfamilies
	   call cg_family_read_f(cg, base, fam, nodename, nfambc, ngeo, &
                  ier)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	   if (ier .eq. ERROR) call cg_error_exit_f

	    ilen=32
	    call no_blank(nodename, ilen)
            write(6,103)'*** Family_t nodes ***'
	    write(6,106)'Name= "',nodename(1:ilen),'"'
	    write(6,107)'no.of FamilyBC_t=',nfambc
	    write(6,107)'no.of GEO=',ngeo
	    write(6,100)' '

          ! A family may have several GeometryReference_t node
            do geo=1, ngeo
                call cg_geo_read_f(cg, base, fam, geo, nodename, &
                    cadfile, cadformat, nparts, ier)
                if (ier .eq. ERROR) call cg_error_exit_f

		write(6,106)'*** GeometryReference_t nodes ***'
		ilen=32
                call no_blank(nodename, ilen)
		write(6,109)'Name= "',nodename(1:ilen),'"'
                ilen=32
                call no_blank(cadfile, ilen)
		write(6,109)'cadfile= "',cadfile(1:ilen),'"'
                ilen=32
                call no_blank(cadformat, ilen)
		write(6,109)'cadformat= "',cadformat(1:ilen),'"'
		write(6,110)'nparts=',nparts
		write(6,100)' '

                ! A GeometryReference_t may encompass several parts
                do part = 1, nparts
                    call cg_part_read_f(cg, base, fam, geo, part, &
                       nodename, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f
		    ilen=32
		    call no_blank(nodename, ilen)
		    write(6,111)'part= "',nodename(1:ilen),'"'
                enddo

              ! GeometryReference_t descriptor
                call cg_goto_f(cg, base, ier, 'Family_t', fam, &
                 'GeometryReference_t',geo,'end')
                if (ier .eq. ERROR) call cg_error_exit_f

		if (ier .eq. ALL_OK) then

		    call cg_ndescriptors_f(n, ier)
		    if (ier .eq. ERROR) call cg_error_exit_f
		    write(6,100)' '
		    write(6,110)'ndescr=',n
		    write(6,100)' '

		    do i=1, n
                        call cg_descriptor_read_f(i,nodename, &
                           descriptor, ier)
                        if (ier .eq. ERROR) call cg_error_exit_f
			ilen = 32
			ilen2=200
			call no_blank(nodename, ilen)
			call no_blank(descriptor, ilen2)
		        write(6,111)'Descriptor_t: "',nodename(1:ilen), &
                          '" is "',descriptor(1:ilen2),'"'
		    enddo
		endif
            enddo

            do bc=1,nfambc
	        write(6,106)'*** FamilyBC_t nodes ***'
                call cg_fambc_read_f(cg, base, fam, bc, nodename, &
                  bctype, ier)
                if (ier .eq. ERROR) call cg_error_exit_f

		ilen=32
		call no_blank(nodename, ilen)
		write(6,109)'Name= "',nodename(1:ilen),'"'
		write(6,109) &
                  'BCType= ',BCTypeName(bctype)
		write(6,100)' '

! begin KMW family functionality extension
                write(6,106)' V2.4 Family functionality extensions'
                write(6,106)' look for DataSet info under FamilyBC'
                write(6,100)' '

                call cg_goto_f(cg, base, ier, 'Family_t', fam, &
                           'FamilyBC_t', bc,'end')
                if (ier .ne. CG_OK) call cg_error_exit_f

                call cg_bcdataset_info_f(nbcdatasets, ier)
                if (ier .eq. ERROR) call cg_error_exit_f

                if(nbcdatasets.eq.0) then
                   write(6,106)'   none found'
                   write(6,100)' '
                else

                  do bcdataset=1,nbcdatasets
                    call cg_bcdataset_read_f(bcdataset, dataset_name, &
                          type, DirichletFlag, NeumannFlag, ier)
                    if (ier .eq. ERROR) call cg_error_exit_f

                    write(6,107)'   Dataset #',bcdataset
                    write(6,109)'    Name=',dataset_name
                    write(6,109)'    BCType=',BCTypeName(type)

             ! ** boundary condition data:  GOTO BCData_t node
                    if (DirichletFlag.eq.1) then
		      call cg_goto_f(cg, base, ier, 'Family_t', fam, &
                           'FamilyBC_t', bc, 'BCDataSet_t', &
                           bcdataset,'BCData_t',CGNS_ENUMV(Dirichlet),'end')
		      if (ier .eq. ERROR) call cg_error_exit_f

                      call cg_units_read_f(mass, length, time, temp, &
                                           deg, ier)
                      if (ier .eq. ERROR) call cg_error_exit_f
                      if (ier .eq. ALL_OK) then
                        write(6,700) &
                            '        Dirichlet data Dimensional Units:', &
                            MassUnitsName(mass), &
                            LengthUnitsName(length), &
                            TemperatureUnitsName(temp), &
                            TimeUnitsName(time), &
                            AngleUnitsName(deg)
                      endif
		    endif
                  enddo
                endif

! end KMW family functionality extension
            enddo

         !  Family Descriptor and Ordinal
            call cg_goto_f(cg, base, ier, 'Family_t', fam, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

! begin KMW family functionality extension
            write(6,106)' V2.4 Family functionality extensions'
            write(6,106)' look for Rotating Coord info under Family'
            write(6,100)' '

         ! *** read RotatingCoordinates_t node
            call cg_rotating_read_f(rot_rate, rot_center, ier)
            if (ier .eq. ERROR) call cg_error_exit_f

            write(6,701)'rot_rate=',rot_rate(1), &
                     rot_rate(2), rot_rate(3)
            write(6,701)'rot_center=',rot_center(1), &
                     rot_center(2), rot_center(3)
            write(6,100)' '

! end KMW family functionality extension

	    if (ier .eq. ALL_OK) then

	        call cg_ndescriptors_f(n, ier)
	        if (ier .eq. ERROR) call cg_error_exit_f
		write(6,107)'ndescriptors=',n
                do i=1, n
                    call cg_descriptor_read_f(i,nodename, &
                       descriptor, ier)
		    if (ier .eq. ERROR) call cg_error_exit_f

		    ilen = 32
                    ilen2=200
                    call no_blank(nodename, ilen)
                    call no_blank(descriptor, ilen2)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                    write(6,111)'Descriptor_t: "', &
                    nodename(1:ilen),'" is "',descriptor(1:ilen2),'"'
                enddo

                call cg_ordinal_read_f(ordinal, ier)
	        if (ier .eq. ERROR) call cg_error_exit_f
                if (ier.eq.ALL_OK) &
                    write(6,108)'Ordinal=',ordinal
		    write(6,100)' '
	    endif
        enddo

! ******read CGNSBase substructure: Zone
	call cg_nzones_f(cg, base, nzones, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

        write(6,103)'nzones=',nzones
	write(6,100)' '

	! ******read CGNSBase substructure: Zone
        do zone=1, nzones
            call cg_zone_read_f(cg,base,zone, nodename,size,ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

	    call cg_zone_type_f(cg, base, zone, ZoneType, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

	    Idim=Cdim
	    if (ZoneType .eq. CGNS_ENUMV(Unstructured)) Idim=1

            write(6,100)'*** Zone_t node ***'
            ilen=32
            call no_blank(nodename, ilen)
            write(6,102)'Name= "',nodename(1:ilen),'"'
            write(6,102)'ZoneType= ',ZoneTypeName(ZoneType)
            write(6,113)'Size= ', (size(i),i=1,Idim*3)

! COORDINATES
	    call cg_ncoords_f(cg, base, zone, ncoords, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f
	    write(6,103)'ncoords=',ncoords

	    write(6,103)'Idim=',Idim
	    do i=1, Idim
		range_min(i)=1
		range_max(i)=size(i)
	    enddo
	    write(6,113) &
                 'range:',(range_min(i),i=1,Idim), &
                          (range_max(i),i=1,Idim)
! Name convention
            coordname(1) = 'CoordinateX'
            coordname(2) = 'CoordinateY'
            coordname(3) = 'CoordinateZ'
	    do i=1, Pdim
	        call cg_coord_read_f(cg, base, zone, coordname(i), &
                  CGNS_ENUMV(RealDouble), range_min, range_max, data_double, ier)
	        if (ier .eq. ERROR) call cg_error_exit_f
		write(6,114)coordname(i),'=',data_double(1)
	    enddo

! GOTO Zone node and read family name :
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f
	    if (ier .eq. ALL_OK) then
	  	call cg_famname_read_f(nodename, ier)
	        if (ier .eq. ERROR) call cg_error_exit_f
		if (ier .eq. ALL_OK) then
		    ilen=32
              	    call no_blank(nodename, ilen)
		    if (ilen.gt.0) then
		      write(6,102) 'FamilyName= "', &
                                          nodename(1:ilen),'"'
		    endif
		endif
	    endif


! CONNECTIVITY
	    call cg_nconns_f(cg, base, zone, nconns, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

	    do n=1, nconns
		call cg_conn_info_f(cg, base, zone, n, nodename, &
                 location, connect_type, ptset_type, npnts, &
                 donorname, donor_zonetype, donor_ptset_type, &
                 donor_datatype, ndata_donor, ier)
		if (ier .eq. ERROR) call cg_error_exit_f

                write(6,100)'*** GridConnectivity_t node ***'
                ilen=32
                call no_blank(nodename, ilen)
                write(6,102)'Name= "',nodename(1:ilen),'"'
                write(6,102)'GridLocation=', &
                                   GridLocationName(location)
                write(6,102)'Connectivity Type=', &
                    GridConnectivityTypeName(connect_type)
                write(6,102)'PtsetType=', &
                                   PointSetTypeName(ptset_type)
                write(6,104)'npnts=',npnts
                ilen=32
                call no_blank(donorname, ilen)
                write(6,102)'DonorName="',donorname(1:ilen),'"'
                write(6,102)'DonorZonetype=', &
                    ZoneTypeName(donor_zonetype)
                write(6,102)'DonorDatatype=', &
                    DataTypeName(donor_datatype)
                write(6,102)'DonorPtsetType=', &
                    PointSetTypeName(donor_ptset_type)
                write(6,104)'ndata_donor=',ndata_donor

                call cg_conn_read_f(cg, base, zone, n, &
                    pnts, CGNS_ENUMV(Integer), donor_data, ier)
                if (ier .eq. ERROR) call cg_error_exit_f

		write(6,102) 'pnts receiver:'
		write(6,112)(pnts(i),i=1,Idim)
		write(6,108)' to ', &
                     (pnts((npnts-1)*Idim+i),i=1,Idim)

		if (donor_zonetype .eq. CGNS_ENUMV(Unstructured)) then
		    Idim_donor = 1
		else
		    Idim_donor = Cdim
		endif
		write(6,102) 'pnts donor:'
		write(6,112)(donor_data(i), i=1,Idim_donor)
		write(6,108)' to ', &
      		    (donor_data((ndata_donor-1)*Idim_donor+i), &
      			i=1,Idim_donor)
		
! Look for interpolants
	        if (donor_ptset_type .eq. CGNS_ENUMV(CellListDonor)) then
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
		
		    call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                        'ZoneGridConnectivity_t', 1, &
                        'GridConnectivity_t', 1, 'end')
                    if (ier .eq. ERROR) call cg_error_exit_f

		    if (ier .eq. ALL_OK) then
		        call cg_narrays_f(narrays, ier)
		        if (ier .eq. ERROR) call cg_error_exit_f
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
			write(6,103)'narrays under GridConnectivity_t=', &
      			    narrays
		    endif

		    if (narrays .eq. 1) then
			call cg_array_info_f(1, nodename, datatype, &
                             ndim, dim_vals, ier)
			if (ier .eq. ERROR) call cg_error_exit_f
			write(6,114)'nodename=',nodename
			write(6,103)'ndim=',ndim
			do i=1,ndim
			   write(6,103)'dim_vals=',dim_vals(i)
			enddo
	                write(6,102)'Interpolants Datatype=', &
                    		DataTypeName(datatype)

			call cg_array_read_f(1, data_double, ier)
			if (ier .eq. ERROR) call cg_error_exit_f
			do j=1,dim_vals(2)
			do i=1,dim_vals(1)
			  pos = (j-1)*dim_vals(1) + i
		          write(6,120)'Interpolants(',i,',',j,')=', &
                                       data_double(pos)
			enddo
			enddo
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

		    endif
		endif
	    enddo
! BOUNDARY CONDITION PATCH: Put a user defined b.c. at interface
            write(6,100)'*** ZoneBC_t node ***'
	    call cg_nbocos_f(cg, base, zone, nbocos, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

	    write(6,103)'nbocos=',nbocos

	    do bc=1, nbocos
	
		write(6,100)'*** BC_t node ***'
		call cg_boco_info_f(cg, base, zone, bc, nodename, &
                 bctype, ptset_type, npnts, NormalIndex, NormalListFlag, &
                 datatype, ndataset, ier)

                ilen=32
                call no_blank(nodename, ilen)
                write(6,102)'Name= "',nodename(1:ilen),'"'
                write(6,102)'BCType= ',BCTypeName(bctype)
                write(6,102)'PtsetType= ', &
                                   PointSetTypeName(ptset_type)
                write(6,105)'npnts=',npnts

              ! note: Normal index has no definition in unstructured
                if (Idim /= 1) then
		  write(6,102)'NormalIndex:'
		  write(6,112)(NormalIndex(i),i=1,Idim)
                end if
		write(6,105)'NormalListFlag=',NormalListFlag
		write(6,102)'NormalDataType= ', &
                                   DataTypeName(datatype)
		write(6,105)'ndataset=',ndataset

		if (datatype.eq.CGNS_ENUMV(RealSingle)) then
	            call cg_boco_read_f(cg, base, zone, bc, pnts, &
                    	NormalListSingle, ier)
	            if (ier .eq. ERROR) call cg_error_exit_f
		else
		    call cg_boco_read_f(cg, base, zone, bc, pnts, &
                        NormalListDouble, ier)
		    if (ier .eq. ERROR) call cg_error_exit_f
		endif

                write(6,102) 'pnts range:'
		write(6,112)(pnts(i),i=1,Idim)
                write(6,108)' to ', &
                     (pnts((npnts-1)*Idim+i),i=1,Idim)

              ! number of points or faces in bcpatch
                if (ptset_type.eq. CGNS_ENUMV(PointList) .or. &
                    ptset_type.eq. CGNS_ENUMV(ElementList)) then
                    size_of_patch = npnts
                else if (ptset_type.eq. CGNS_ENUMV(PointRange) .or. &
                         ptset_type.eq. CGNS_ENUMV(ElementRange)) then
                    size_of_patch=1
                    do i=1,Idim
                        multiplier=pnts((npnts-1)*Idim+i) - pnts(i) + 1
                        size_of_patch = size_of_patch * multiplier
                    enddo
                endif
                write(6,105)'size_of_patch=',size_of_patch

	      ! InwardNormalList
		if (NormalListFlag.ne.0) then
		    if (datatype.eq.CGNS_ENUMV(RealSingle)) then
			write(6,102) '1st and last normal vector:'
			write(6,118)(NormalListSingle(i),i=1,Pdim)
			write(6,118) &
                        (NormalListSingle((npnts-1)*Pdim+i),i=1,Pdim)
		    else
		  	write(6,102) '1st and last normal vector:'
                        write(6,118)(NormalListDouble(i),i=1,Pdim)
                        write(6,118) &
                        (NormalListDouble((npnts-1)*Pdim+i),i=1,Pdim)
		    endif
		endif


		call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'ZoneBC_t', 1, 'BC_t', bc, 'end')
		if (ier .eq. ERROR) call cg_error_exit_f

		if (ier .eq. ALL_OK) then
		    call cg_famname_read_f(nodename, ier)
		    if (ier .eq. ERROR) call cg_error_exit_f
                    if (ier .eq. ALL_OK) then
		        ilen=32
		        call no_blank(nodename, ilen)
		        if (ilen.gt.0) &
      		            write(6,102)'FamilyName= "', &
                                         nodename(1:ilen),'"'
		    endif
		endif
	    enddo

! ********** SPECIAL FOR UNSTRUCTURED ZONES ONLY **********

	    if (ZoneType .eq. CGNS_ENUMV(Unstructured)) then

! read element sections (Element_t) :
		call cg_nsections_f(cg, base, zone, nsections, ier)
		if (ier .eq. ERROR) call cg_error_exit_f

		write(6,100)'*** Elements_t Nodes ***'

		write(6,103)'nsections=',nsections

		do sect=1, nsections
		    write(6,201)'  *** Section ',sect,' ***'
 201	format(/a,i2,a)
		    call cg_section_read_f(cg, base, zone, sect, &
                        nodename, type, start, end, nbndry, &
                        parent_flag, ier)
		    if (ier .eq. ERROR) call cg_error_exit_f

		    call cg_npe_f(type, npe, ier)
		    if (ier .eq. ERROR) call cg_error_exit_f
  		    write(6,103)'npe=',npe
  		    write(6,103)'type=',type

                    ilen=32
                    call no_blank(nodename, ilen)
                    write(6,102)'Name= "',nodename(1:ilen),'"'
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
		    if (type .gt. CGNS_ENUMV(NGON_n)) then
                         write(6,100) 'Element Type=  NGON_n'
                    else
			write(6,102)'Element Type= ', &
                                      ElementTypeName(type)
		    endif
		    write(6,113)'Range= ',start,end
		    if (nbndry .ne. 0) write(6,102)'Sorted elements'

		    nelem = end-start+1
		    if (type .ge. CGNS_ENUMV(NGON_n)) then
		        call cg_poly_elements_read_f(cg, base, zone, sect, &
		            elements, elements_offset, parent_data, ier)
		        if (ier.eq.ERROR)  call cg_error_exit_f
		    else
		        call cg_elements_read_f(cg, base, zone, sect, &
                            elements,parent_data, ier)
		        if (ier.eq.ERROR)  call cg_error_exit_f
		    endif

		    write(6,102)'Element Connectivity:'
		
		    do i=1, nelem
		       write(6,115)(elements((i-1)*npe+n),n=1,npe)
			if (i.eq.10) goto 17
		    enddo
 17		    continue

		    if (parent_flag .ne. 0) then
			write(6,102)'Parent Data:'
			do i=1, 4
			     write(6,116) &
       				(parent_data((i-1)*nelem+n), n=1,nelem)
			enddo
		    endif
! Auxiliary data
		    call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                        'Elements_t', sect, 'end')
		    if (ier .eq. ERROR) call cg_error_exit_f

		    if (ier .eq. ALL_OK) then
			call cg_ndescriptors_f(n, ier)
                	if (ier .eq. ERROR) call cg_error_exit_f
                	do i=1, n
                    	    call cg_descriptor_read_f(i,nodename, &
                       	                              descriptor, ier)
                    	    if (ier .eq. ERROR) call cg_error_exit_f

                            ilen = 32
                            ilen2=200
                            call no_blank(nodename, ilen)
                            call no_blank(descriptor, ilen2)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
                            write(6,117)'Descriptor_t: "', &
                                        nodename(1:ilen),'" is "', &
                                          descriptor(1:ilen2),'"'
                	enddo
		    endif
		enddo
	    endif

! *********************************************************************

        enddo	! zone loop

	write(6,100)'calling cg_close_f'
	call cg_close_f(cg, ier)
	if (ier .eq. ERROR) call cg_error_exit_f()

 700    format(a/, 10x,'    Mass units: ',a/, 10x,'    Length units: ', &
          a/, 10x,'    Temperature units: ',a/, 10x,'    Time units: ', &
          a/, 10x,'    Angle units:',a/)
 701    format(10x,a,5f5.1)

 102    format(6x,3a)
 103    format(6x,a,i3)
 104    format(6x,a,i3)
 105    format(6x,a,i5)
 106    format(8x,3a)
 107    format(8x,a,i3)
 108    format(8x,a,3i3)
 109    format(10x,3a)
 110    format(10x,a,i3)
 111    format(12x,5a/)
 112    format(12x,3i3)
 113    format(6x, a, 9i6)
 114    format(6x,2a,e13.6)
 115    format(6x,8i6)
 116    format(6x,4i3)
 117    format(6x,5a/)
 118    format(12x,3e13.6)
 120    format(a,i1,a,i3,a,e13.6)
 600	format(3a)

 9999   end


        subroutine no_blank(Text, length)

        character*(*) Text
        integer i, ilen, length

        ilen=0
        do i=length,1,-1
             if (Text(i:i).ne.' ') then
                 ilen=i
                 goto 17
             endif
        enddo

 17     length = ilen

        return
        end

