
        program write_mixed_elements
        USE CGNS

!	author: Diane Poirier
!	last revised on March 15 2000

! 	This example test the writing of and element section of
!	type CGNS_ENUMV(MIXED), which includes some NGON_x elements of different size
!	The model is not realistic, as all the data is dummy.
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer index_dim, cell_dim, phys_dim
	integer cg, base, zone, ier, index, ZoneType
        integer(cgsize_t) size(3)
	integer i, j, k, n, pos, coord, section_no
        integer(cgsize_t) element(20)
        integer(cgsize_t) connect_offsets(10)
	character*32 ZoneName, coordname(3)
        double precision data_double(27)
	integer grid_no, count

!       initialize
        ier = 0
	index_dim = 1
	cell_dim=3
	phys_dim=3

!       open CGNS file for writing

        call cg_open_f('Test_V2', MODE_WRITE, cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

!*******write CGNSBase
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	call cg_base_write_f(cg, 'MixedElementBase', cell_dim, phys_dim, &
      			     base, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

!*******write only 1 unstructured zone
	zone = 1
	ZoneType=CGNS_ENUMV(Unstructured)
	write(ZoneName,100) 'UnstructuredZone#1'
 100	format(a)
	size(1) = 27		! no of nodes
	size(2) = 3		! no of elements
	size(3) = 0		! unsorted nodes

	call cg_zone_write_f(cg, base, 'UnstructuredZone#1', size, &
                             ZoneType, index, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! Name convention
        coordname(1) = 'CoordinateX'
        coordname(2) = 'CoordinateY'
        coordname(3) = 'CoordinateZ'


! Create GridCoordinates_t node
            call cg_grid_write_f(cg,base,index,'GridCoordinates', &
                               grid_no, ier)
            if (ier.ne. ALL_OK) call cg_error_exit_f

! create coordinate data(10x10x10 box with 3x3x3 equidistant nodes)
        do coord=1,phys_dim
           DO k=1, 3
              DO j=1, 3
                 DO i=1, 3
                    pos = i + (j-1)*3 + (k-1)*9
                    IF (coord.EQ.1) data_double(pos) = (i-1)*5
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

! Generate dummy elements

	count = 0
	connect_offsets(1) = 0
! first element HEXA_8
        count = count + 1
        element(count)=CGNS_ENUMV(HEXA_8)
        do i=1,8
          count = count + 1
          element(count)=i
        enddo
! second element TETRA_4
        connect_offsets(2) = count
        count = count + 1
        element(count)=CGNS_ENUMV(TETRA_4)
        do i=1,4
          count = count + 1
          element(count)=i
        enddo
! third element PYRA_5
        connect_offsets(3) = count
        count = count + 1
        element(count)=CGNS_ENUMV(PYRA_5)
        do i=1,5
          count = count + 1
          element(count)=i
        enddo
        connect_offsets(4) = count

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
! Write MIXED element section
	call cg_poly_section_write_f(cg, base, zone, 'MixedElements', &
                                CGNS_ENUMV(MIXED), 1_cgsize_t, 3_cgsize_t, 0, &
                                element, connect_offsets, section_no, &
                                ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! Write section of 4  NGON_n of 3 nodes
        count = 0
	do n=1,4
	    !count = count + 1
	    !element(count)=3
	    connect_offsets(n) = count
	    do i=1,3
		count = count + 1
		element(count)=10*n+i
	    enddo
	enddo
	connect_offsets(5) = count
	call cg_poly_section_write_f(cg, base, zone, ' NGON_n(3)', &
                       CGNS_ENUMV(NGON_n), 1_cgsize_t, 4_cgsize_t, 0, element, &
                       connect_offsets, section_no, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! Write & close CGNS file

	call cg_close_f(cg, ier)
	if (ier.eq. ERROR) call cg_error_exit_f()

	end

