        program write_mixed_grid
	USE CGNS

!	author: Diane Poirier
!	last revised on March 8 2000

! 	This example test the new data structures created for
! 	unstructured data and geometry reference data.  The
! 	model being create is composed of 2 zones.  The 1st
!	one is a structured 3x3x3 block, and the 2nd is
!	an unstructured 3x3x3 block composed of 8 hexa elements.
! 	The 2 zones interface (Abutting1to1) on one 9-nodes face.
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim
	integer cg, base, zone, num, ier, index, ZoneType
        integer(cgsize_t) size(Ndim*3)
	integer i, j, k, n, pos, coord
        integer(cgsize_t) element(8,8)
	integer hexa_section_no, quad_section_no
        integer(cgsize_t) parent(4,4)
        integer(cgsize_t) elist(4)
	integer(cgsize_t) quads(4,4)
        integer(cgsize_t) dim_vals(12)
        integer(cgsize_t) pnts(3,9), uns_pnts(9)
	integer(cgsize_t) Nindex(3)
	double precision VertexNormals(3,9), FaceNormals(3,4)
	integer fam, geo, part, bc
	integer sol_no, field_no
	character*32 ZoneName, coordname(3), geoname, fambcname
	character*200 cadfile, partname
        double precision data_double(200*3), interpolants(3,9)
	integer grid_no
        real rot_vec(3), rot_center(3), rm_arr(6)
        integer rm_len(2)

!       initialize
        ier = 0
	cell_dim=3
	phys_dim=3

!       open CGNS file for writing

        call cg_open_f('Test_V2', MODE_WRITE, cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

!*******write CGNSBase
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	call cg_base_write_f(cg, 'Name of my Base', cell_dim, phys_dim, &
      			     base, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

!****** Create 2 volume and 1 face families
	call cg_family_write_f(cg, base, 'Family#1', index, ier)
!	write(6,*)'index1=',index
	if (ier .eq. ERROR) call cg_error_exit_f
	call cg_family_write_f(cg, base, 'Family#2', index, ier)
!	write(6,*)'index2=',index
	if (ier .eq. ERROR) call cg_error_exit_f
        call cg_family_write_f(cg, base, 'Outflow', index, ier)
!	write(6,*)'index3=',index
        if (ier .eq. ERROR) call cg_error_exit_f

! ******write CGNSBase substructure: Zone
	do zone=1, 2

! Structured zone:
	    if (zone .eq. 1) then
		ZoneType=CGNS_ENUMV(Structured)
	        write(ZoneName,100) 'StructuredZone#1'
		
		index_dim = 3
		do i=1, index_dim
		    size(i) = 3
		    size(i+index_dim) = size(i)-1
		    size(i+2*index_dim) = 0              ! unsorted data
		enddo

! Unstructured zone:
	    else if (zone .eq. 2) then
		ZoneType=CGNS_ENUMV(Unstructured)
		write(ZoneName,'(a)') 'UnstructuredZone#1'
		index_dim = 1
		size(1) = 27		! no of nodes
	 	size(2) = 8		! no of elements
		size(3) = 0		! unsorted nodes
	    endif

	    num = 1
            do i=1, index_dim               		
                num = num * size(i)
            enddo
	    call cg_zone_write_f(cg,base,ZoneName,size,ZoneType, &
                                 index,ier)
            if (ier .eq. ERROR) call cg_error_exit_f

! GOTO Zone node and write family name :
            call cg_goto_f(cg, base, ier, 'Zone_t', zone, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f
	    if (zone.eq.1) then
	  	call cg_famname_write_f('Family#1', ier)
	    else
		call cg_famname_write_f('Family#2', ier)
	    endif
	    if (ier .eq. ERROR) call cg_error_exit_f

! Name convention
            coordname(1) = 'CoordinateX'
            coordname(2) = 'CoordinateY'
            coordname(3) = 'CoordinateZ'

! Create GridCoordinates_t node
            call cg_grid_write_f(cg,base,index,'GridCoordinates', &
                               grid_no, ier)
            if (ier.ne. ALL_OK) call cg_error_exit_f

! create coordinate data
            do coord=1,phys_dim
               DO k=1, 3
                  DO j=1, 3
                     DO i=1, 3
                        pos = i + (j-1)*3 + (k-1)*9
                        IF (coord.EQ.1) data_double(pos) = (i-1)*5 + &
                             (zone-1)*10
                        IF (coord.EQ.2) data_double(pos) = (j-1)*5
                        IF (coord.EQ.3) data_double(pos) = (k-1)*5
                     ENDDO
                  ENDDO
               ENDDO

! GOTO GridCoordinatesNode & write DataArray
                call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                    'GridCoordinates_t', 1, 'end')
                if (ier .eq. ERROR) call cg_error_exit_f

                call cg_array_write_f(coordname(coord), CGNS_ENUMV(RealDouble), &
                    index_dim, size, data_double, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
	    enddo	! coord. loop

! Flow Solution

	    do n=1,8
		data_double(n) = 1.0 + n/10.0
	    enddo
	    call cg_sol_write_f(cg, base, zone,'Solution1',CGNS_ENUMV(CellCenter), &
               sol_no,ier)
	    if (ier .eq. ERROR) call cg_error_exit_f
	    call cg_field_write_f(cg, base, zone, sol_no, CGNS_ENUMV(RealDouble), &
                'DummySolution', data_double, field_no, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! Grid Connectivity 1to1:  From a structured zone to an unstructured zone
	    ! Unstructured point list:
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	    uns_pnts(1)=1
	    do n=2,9
		uns_pnts(n) = uns_pnts(n-1)+3
	    enddo

	    if (zone .eq. 1) then
		! PointRange for structured zone:
	        pnts(1,1)=3
	        pnts(2,1)=1
	        pnts(3,1)=1
	        pnts(1,2)=3
                pnts(2,2)=3
                pnts(3,2)=3

	        call cg_conn_write_f(cg, base, zone, 'str_to_unstr', &
       	            CGNS_ENUMV(Vertex), CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointRange), 2_cgsize_t, pnts, &
                    'UnstructuredZone#1', CGNS_ENUMV(Unstructured), &
                    CGNS_ENUMV(CellListDonor), CGNS_ENUMV(Integer), 9_cgsize_t, uns_pnts, index, &
                    ier)
		if (ier .eq. ERROR) call cg_error_exit_f

! Fake similation of Overset or Abutting-mismatch: str to unstr
		call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                    'ZoneGridConnectivity_t', 1, &
                    'GridConnectivity_t', 1, 'end')
                if (ier .eq. ERROR) call cg_error_exit_f

! make up some dummy data
		do n=1,9
		do i=1,cell_dim
		    interpolants(i,n)=1.0/(i+n)
		enddo
		enddo

		dim_vals(1)=cell_dim
		dim_vals(2)=9
		call cg_array_write_f('InterpolantsDonor', CGNS_ENUMV(RealDouble), &
                    2, dim_vals, interpolants, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
		
	    endif

	

! Grid Connectivity 1to1:  From an unstructured zone to a structured zone
	    if (zone .eq. 2) then

		! PointList of structured donor
		n=0
		do k=1,3
		do j=1,3
		    n=n+1
		    pnts(1,n) = 3
		    pnts(2,n) = j
		    pnts(3,n) = k
		enddo
		enddo

		call cg_conn_write_f(cg, base, zone, 'unstr_to_str', &
                    CGNS_ENUMV(Vertex), CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointList),9_cgsize_t,uns_pnts, &
                    'StructuredZone#1', CGNS_ENUMV(Structured), CGNS_ENUMV(PointListDonor), &
      		    CGNS_ENUMV(Integer), 9_cgsize_t, pnts, index, ier)
		if (ier .eq. ERROR) call cg_error_exit_f
            endif

! BOUNDARY CONDITION PATCH: Put a user defined b.c. at interface
	    if (zone.eq.1) then
	      ! write a PointRange patch (3,1,1) to (3,3,3) for a structured zone
	        call cg_boco_write_f(cg, base, zone, 'myboco', &
                  UserDefined, CGNS_ENUMV(PointRange), 2_cgsize_t, pnts, index, ier)
	        if (ier .eq. ERROR) call cg_error_exit_f

	      ! Write Normal index and Normal vectors
		Nindex(1)=-1
		Nindex(2)=0
		Nindex(3)=0
		do n=1, 9
		    VertexNormals(1,n) = -1.0	! x-coord
		    VertexNormals(2,n) = 0	! y-coord
		    VertexNormals(3,n) = 0	! z-coord
		enddo
		
		call cg_boco_normal_write_f(cg, base, zone, index, Nindex, &
                      1, CGNS_ENUMV(RealDouble), VertexNormals, ier)
		if (ier .eq. ERROR) call cg_error_exit_f


	    else if (zone.eq.2) then
	      ! BC patch defined using points and normals at the points:
		call cg_boco_write_f(cg, base, zone, 'point_patch', &
                  UserDefined, CGNS_ENUMV(PointList), 9_cgsize_t, uns_pnts, index, &
                  ier)
                if (ier .eq. ERROR) call cg_error_exit_f

	      ! note: Normal index has no definition in unstructured
		do n=1, 9
		    VertexNormals(1,n) = 1.0
		enddo
		call cg_boco_normal_write_f(cg, base, zone, index, Nindex, &
                      1, CGNS_ENUMV(RealDouble), VertexNormals, ier)
                if (ier .eq. ERROR) call cg_error_exit_f

	      ! BC patch defined using face elements & normals at the faces
		do n=1, 4
		  elist(n)=8+n
		  FaceNormals(1,n)=1.0
		  FaceNormals(2,n)=0
		  FaceNormals(3,n)=0
		enddo
		call cg_boco_write_f(cg, base, zone, 'shell_patch', &
                  CGNS_ENUMV(BCOutflow), CGNS_ENUMV(PointList), 4_cgsize_t, elist, index, ier)
		if (ier .eq. ERROR) call cg_error_exit_f

	     ! Specify that the GridLocation is FaceCenter
		call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'ZoneBC_t', 1, 'BC_t', index, 'end')
                if (ier .eq. ERROR) call cg_error_exit_f

		call cg_gridlocation_write_f(CGNS_ENUMV(FaceCenter), ier)
		if (ier .eq. ERROR) call cg_error_exit_f

                call cg_boco_normal_write_f(cg, base, zone, index, &
                      Nindex, 1, CGNS_ENUMV(RealDouble), FaceNormals, ier)
                if (ier .eq. ERROR) call cg_error_exit_f

	      ! Define family name for BC patch
		call cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'ZoneBC_t', 1, 'BC_t', index, 'end')
		if (ier .eq. ERROR) call cg_error_exit_f

		call cg_famname_write_f('Outflow', ier)
		if (ier .eq. ERROR) call cg_error_exit_f
	    endif

! ********** SPECIAL FOR UNSTRUCTURED ZONES ONLY **********

	    if (ZoneType .eq. CGNS_ENUMV(Unstructured)) then

            ! Generate HEXA_8 Element Connectivity
                n=0
                DO k=1,2
                   DO j=1,2
                      DO i=1,2
                         n=n+1
                         pos = i + (j-1)*3 + (k-1)*9
                         element(1,n) = pos
                         element(2,n) = pos+1
                         element(3,n) = pos+4
                         element(4,n) = pos+3
                         element(5,n) = pos+9
                         element(6,n) = pos+10
                         element(7,n) = pos+13
                         element(8,n) = pos+12
                      ENDDO
                   ENDDO
                ENDDO

		call cg_section_write_f(cg, base, zone, &
                  'VolumeElements', CGNS_ENUMV(HEXA_8), 1_cgsize_t, 8_cgsize_t, 0, &
                   element, hexa_section_no, ier)
		if (ier .eq. ERROR) call cg_error_exit_f

	      ! GENERATE some quad elements
                ! 1st node of each element
                quads(1,1) = 3
                quads(1,2) = 6
                quads(1,3) = 12
                quads(1,4) = 15
                ! remaining nodes of each element
                do n=1, 4
                    quads(2,n)=quads(1,n)+3
                    quads(3,n)=quads(1,n)+12
                    quads(4,n)=quads(1,n)+9
                enddo

		call cg_section_write_f(cg, base, zone, 'outflow', &
                  CGNS_ENUMV(QUAD_4), 9_cgsize_t, 12_cgsize_t, 0, quads, &
                  quad_section_no,ier)
		if (ier .eq. ERROR) call cg_error_exit_f

	     !  GENERATE Parent Data for shell elements
		do n=1, 4
		    parent(n,1) = 2*n	! first parent
		    parent(n,2) = 0	! no second parent
		    parent(n,3) = 3	! face of first parent
		    parent(n,4) = 0	! face of 2nd parent
		enddo

		call cg_parent_data_write_f(cg, base, zone, &
                     quad_section_no, parent, ier)
		if (ier .eq. ERROR) call cg_error_exit_f

! Auxiliary info:
                call cg_goto_f(cg,base,ier,'Zone_t',zone,'Elements_t', &
                  hexa_section_no, 'end')
                if (ier .eq. ERROR) call cg_error_exit_f

                call cg_descriptor_write_f('Descriptor_Name', &
                    'Descriptor_Text', ier)
                if (ier .eq. ERROR) call cg_error_exit_f


	    endif	! IF UNSTRUCTURED ZONE


! *********************************************************************

        enddo	! zone loop

! FAMILY and GEOMETRY

	do fam=1,3
          ! A family may have several GeometryReference_t node
            do geo=1, 2
                write(geoname, 200) 'Geo#',geo,'_of_fam#',fam
		write(cadfile, 200)'CADFile#',geo
                call cg_geo_write_f(cg, base, fam, geoname, &
                    cadfile, 'IGES', index, ier)
                if (ier .eq. ERROR) call cg_error_exit_f

		! A GeometryReference_t may encompass several parts
		do part = 1, 2
		    write(partname, 200)'Part#',part
		    call cg_part_write_f(cg, base, fam, geo, &
                        partname, index, ier)
		    if (ier .eq. ERROR) call cg_error_exit_f
		enddo

	      ! GeometryReference_t descriptor
		call cg_goto_f(cg, base, ier, 'Family_t', fam, &
                 'GeometryReference_t',geo,'end')
		if (ier .eq. ERROR) call cg_error_exit_f

		call cg_descriptor_write_f('Descriptor', &
                 geoname, ier)
		if (ier .eq. ERROR) call cg_error_exit_f
	    enddo

	  ! A family may have several FamilyBC_t node
! NOT ACCORDING TO THE SIDS
	    do bc=1,2
		write(fambcname,'(a,i1)')'FamBC#',bc
	 	call cg_fambc_write_f(cg, base, fam, fambcname, &
                  CGNS_ENUMV(BCGeneral), index, ier)
		if (ier .eq. ERROR) call cg_error_exit_f
	    enddo


! ** begin KMW Family Functionality Extension
          ! add to the first FamilyBC_t only

	    call cg_goto_f(cg, base, ier, 'Family_t', fam, &
                           'FamilyBC_t', 1,'end')
	    if (ier .eq. ERROR) call cg_error_exit_f

!           call cg_bcdataset_write_f('FamBCDataSet', CGNS_ENUMV(BCWall), &
!                         0, ier)
!	    if (ier .eq. ERROR)  call cg_error_exit_f

            call cg_bcdataset_write_f('FamBCDataSet', CGNS_ENUMV(BCWall), &
                           CGNS_ENUMV(Dirichlet), ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

            call cg_bcdataset_write_f('FamBCDataSet', CGNS_ENUMV(BCWall), &
                           CGNS_ENUMV(Neumann), ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

            call cg_goto_f(cg, base, ier, 'Family_t', fam, &
                           'FamilyBC_t', 1, 'BCDataSet_t', 1, &
                           'BCData_t', CGNS_ENUMV(Dirichlet), 'end')
	    if (ier .eq. ERROR) call cg_error_exit_f

            call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Celsius), &
                           CGNS_ENUMV(Degree), ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! ** end KMW Family Functionality Extension


	 !  Family Descriptor and Ordinal
	    call cg_goto_f(cg, base, ier, 'Family_t', fam, 'end')
	    if (ier .eq. ERROR) call cg_error_exit_f

	    call cg_descriptor_write_f('Fam_Descr_Name', &
                  'Fam_Descr_Text', ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

	    call cg_ordinal_write_f(fam, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! ** begin KMW Family Functionality Extension

            do i=1,3
               rot_vec(i) = i
               rot_center(i) = -i
            enddo

            call cg_rotating_write_f(rot_vec, rot_center, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! ** end KMW Family Functionality Extension

	enddo

! Auxiliary nodes for FAMILY and GEOMETRY

! *********************************************************************

	call cg_close_f(cg, ier)
	if (ier.eq. ERROR) call cg_error_exit_f()
 100	format(a)
 200	format(a,i1,a,i1)

	end

