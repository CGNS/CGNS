
        program write_partial_data
        USE CGNS
        IMPLICIT NONE

!	original author (of write_mixed_elements): Diane Poirier
!       author: Ken Wall
!	May 13 2004

!       Original code came from the example write_mixed_elements.

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
	integer(cgsize_t) connect_offsets(20)
	character*32 ZoneName, coordname(3)
        real*8 data_double(27)
	integer grid_no, C, count
        integer(cgsize_t) rmin, rmax, parent(12)

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
	call cg_base_write_f(cg, 'PartialReadWriteBase', cell_dim, &
                             phys_dim, base, ier)
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

! write coordinate data in 2 separate calls

! Set the ranges for the first partial write of the Coordinate data
            rmin = 15
            rmax = 27

! create coordinate data(10x10x10 box with 3x3x3 equidistant nodes)
        do coord=1,phys_dim
            pos = 0
            DO i= rmin, rmax
               pos = pos + 1
               IF (coord.EQ.1) data_double(pos) = i
               IF (coord.EQ.2) data_double(pos) = -i
               IF (coord.EQ.3) data_double(pos) = i
            ENDDO

            call cg_coord_partial_write_f(cg, base, zone, CGNS_ENUMV(RealDouble), &
                        coordname(coord), rmin, rmax, data_double, C, &
                        ier)
            if (ier .eq. ERROR) call cg_error_exit_f

	enddo	! coord. loop

! Set the ranges for the second partial write of the Coordinate data
            rmin = 1
            rmax = 14

! create coordinate data(10x10x10 box with 3x3x3 equidistant nodes)

        do coord=1,phys_dim
            pos = 0
            DO i=rmin, rmax
               pos = pos + 1
               IF (coord.EQ.1) data_double(pos) = -i
               IF (coord.EQ.2) data_double(pos) = i
               IF (coord.EQ.3) data_double(pos) = -i
            ENDDO

            call cg_coord_partial_write_f(cg, base, zone, CGNS_ENUMV(RealDouble), &
                        coordname(coord), rmin, rmax, data_double, C, &
                        ier)
            if (ier .eq. ERROR) call cg_error_exit_f

	enddo	! coord. loop

! create section with 3 elements

	call cg_section_partial_write_f(cg, base, zone, 'MixedElements', &
             CGNS_ENUMV(MIXED), 1_cgsize_t, 3_cgsize_t, 0, section_no, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

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
        connect_offsets(3) = count

! Write first 2 elements of the MIXED element section
        rmin = 1
        rmax = 2
	call cg_poly_elements_partial_write_f(cg, base, zone, &
             section_no, rmin, rmax, element, connect_offsets, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
! second element TETRA_4
	connect_offsets(1) = 0
	count = 1
	element(count)=CGNS_ENUMV(TETRA_4)
	do i=1,4
	    count = count + 1
            element(count)= -i
        enddo
	connect_offsets(2) = count

! third element PYRA_5
        count = count + 1
        element(count)=CGNS_ENUMV(PYRA_5)
        do i=1,5
            count = count + 1
            element(count)=i
        enddo
        connect_offsets(3) = count

! Write second and third elements of the MIXED element section
        rmin = 2
        rmax = 3
	call cg_poly_elements_partial_write_f(cg, base, zone, &
             section_no, rmin, rmax, element, connect_offsets, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

! Write parent data in 2 separate calls

        count = 0
	do i=5,12
            count = count + 1
            parent(count)=i
        enddo

        ! write parent data for the second and third elements
        call cg_parent_data_partial_write_f(cg, base, zone, section_no, &
             2_cgsize_t, 3_cgsize_t, parent, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

        count = 0
	do i=1,4
            count = count + 1
            parent(count)= -i
        enddo

        ! write parent data for the first element
        call cg_parent_data_partial_write_f(cg, base, zone, section_no, &
             1_cgsize_t, 1_cgsize_t, parent, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! Write & close CGNS file

	call cg_close_f(cg, ier)
	if (ier.eq. ERROR) call cg_error_exit_f()

	end

