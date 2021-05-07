	program write_cgns_motion
	USE CGNS
        implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on November 2000
!       This example test the API functions for grid motion and time accurate data.
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif
        INTEGER Ndim, NumberOfSteps, Nnodes
	parameter (Ndim = 3, NumberOfSteps=2)
	parameter (Nnodes = 120)
	integer index_dim, cell_dim, phys_dim, pos
	integer base_no, zone_no, coord_no, sol_no, field_no
	integer num
        INTEGER i, j, k
	integer(cgsize_t) size(Ndim*3), data_size(2)
	integer cg, ier, coord, sol, field
	double precision Dxyz(Nnodes), values(Nnodes)
	character*32 zonename, solname, fieldname, coordname(Ndim)

! *** Variables for RigidGridMotion_t test
!	real*4 origin(Ndim,2), angle(Ndim), velocity(Ndim)
	double precision origin(Ndim,2), angle(Ndim), velocity(Ndim)
	integer rmotion_no, rmotion
	character*32 rmotionname

! *** Variables for ArbitraryGridMotion_t test
!	real*4 GridVelocity(Nnodes)
	double precision GridVelocity(Nnodes)
	integer amotion_no, amotion
	character*32 amotionname

! *** Multiple GridCoordinates_t node
	integer grid_no, grid
	character*32 gridname

! *** Base/Zone IterativeData_t nodes
!	real*4 time(2)
	double precision time(2)
	integer nsteps, step
	integer(cgsize_t) dimval(3)
	integer*4 nzones(NumberOfSteps), iteration(NumberOfSteps)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	character*32 bitername, zitername, zone_ptrs(2,NumberOfSteps)
	character*32 grid_ptrs(NumberOfSteps), sol_ptrs(NumberOfSteps)
	character*32 rmotion_ptrs(NumberOfSteps)
        character*32 amotion_ptrs(NumberOfSteps)

	coordname(1) = 'CoordinateX'
	coordname(2) = 'CoordinateY'
	coordname(3) = 'CoordinateZ'

! *** initialize
	ier = 0
	index_dim=Ndim
	cell_dim=Ndim
	phys_dim=Ndim

! *** open CGNS file for writing
 	call cg_open_f('Test_V2', MODE_WRITE, cg, ier)
 	if (ier .ne. ALL_OK) call cg_error_exit_f
	
! *** base
 	call cg_base_write_f(cg, 'Basename', cell_dim, phys_dim, &
                             base_no, ier)
 	if (ier .ne. ALL_OK) call cg_error_exit_f

! *** simulation type
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	call cg_simulation_type_write_f(cg, base_no, CGNS_ENUMV(TimeAccurate), ier)
	if (ier .ne. ALL_OK) call cg_error_exit_f

! *** Base iterative data
	write(bitername,100) 'BaseIterativeData'
	nsteps=2
	call cg_biter_write_f(cg, base_no, bitername, nsteps, ier)
	if (ier .ne. ALL_OK) call cg_error_exit_f

	call cg_goto_f(cg, base_no, ier, 'BaseIterativeData_t',1,'end')

	if (ier .ne. ALL_OK) call cg_error_exit_f

! *** TimeValues & IterationValues
	do step=1,nsteps
	    time(step) = 15.5*step
	    iteration(step) = 1000*step
	    nzones(step) = step
	    write(zone_ptrs(1,step),100) 'Zone#',1
	    write(zone_ptrs(2,step),100) 'Null'
	    if (step.eq.2) write(zone_ptrs(2,step),100) 'Zone#',2
	enddo
	!call cg_array_write_f('TimeValues', CGNS_ENUMV(RealSingle), 1, nsteps,
	call cg_array_write_f('TimeValues', CGNS_ENUMV(RealDouble), 1, &
                               INT(nsteps,cgsize_t), time, ier)
        if (ier .ne. ALL_OK) call cg_error_exit_f
	call cg_array_write_f('IterationValues', CGNS_ENUMV(Integer), 1, &
                               INT(nsteps,cgsize_t),iteration, ier)
	if (ier .ne. ALL_OK) call cg_error_exit_f
	call cg_array_write_f('NumberOfZones', CGNS_ENUMV(Integer), 1, &
                               INT(nsteps,cgsize_t),nzones, ier)
	if (ier .ne. ALL_OK) call cg_error_exit_f
	dimval(1)=32
	dimval(2)=2 		! *** MaxNumberOfZones in a step1
	dimval(3)=nsteps
        call cg_array_write_f('ZonePointers', CGNS_ENUMV(Character), 3, dimval, &
                               zone_ptrs, ier)

	if (ier .ne. ALL_OK) call cg_error_exit_f
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012

! *** zone
        write(zonename,100) 'Zone#',1
        num = 1
	do i=1,index_dim      		! zone#1: 3*4*5
            size(i) = i+2		! nr of nodes in i,j,k
	    size(i+Ndim) = size(i)-1	! nr of elements in i,j,k
	    size(i+2*Ndim) = 0		! nr of bnd nodes if ordered
       	    num = num * size(i)		! nr of nodes &
         enddo
         call cg_zone_write_f(cg, base_no, zonename, size, &
                             CGNS_ENUMV(Structured), zone_no, ier)
        if (ier .ne. ALL_OK) call cg_error_exit_f

! *** Zone iterative data
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	write(zitername,100) 'ZoneIterativeData'
        call cg_ziter_write_f(cg, base_no, zone_no, zitername, ier)
        if (ier .ne. ALL_OK) call cg_error_exit_f

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
        call cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
                       'ZoneIterativeData_t',1,'end')
        if (ier .ne. ALL_OK) call cg_error_exit_f

	do step=1, nsteps
	    write(grid_ptrs(step),100)   'GridCoordinates#',    step
	    write(sol_ptrs(step),100)    'FlowSolution#',       step
	    write(rmotion_ptrs(step),100)'RigidGridMotion#',    step
	    write(amotion_ptrs(step),100)'ArbitraryGridMotion#',step
	enddo
	dimval(1)=32
	dimval(2)=nsteps

	call cg_array_write_f('GridCoordinatesPointers', CGNS_ENUMV(Character), 2, &
                               dimval, grid_ptrs, ier)
        if (ier .ne. ALL_OK) call cg_error_exit_f
	call cg_array_write_f('FlowSolutionPointers', CGNS_ENUMV(Character), 2, &
                               dimval, sol_ptrs, ier)
        if (ier .ne. ALL_OK) call cg_error_exit_f
	call cg_array_write_f('RigidGridMotionPointers', CGNS_ENUMV(Character), 2, &
                               dimval, rmotion_ptrs, ier)
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	if (ier .ne. ALL_OK) call cg_error_exit_f
        call cg_array_write_f('ArbitraryGridMotionPointers', CGNS_ENUMV(Character), &
                               2, dimval, amotion_ptrs, ier)
	if (ier .ne. ALL_OK) call cg_error_exit_f

! *** coordinate
      do coord=1, phys_dim
 	    do k=1, size(3)
 	    do j=1, size(2)
 	    do i=1, size(1)
	        pos = i + (j-1)*size(1) + (k-1)*size(1)*size(2)
		! * make up some dummy coordinates just for the test:
 	        if (coord.eq.1) Dxyz(pos) = i
 	        if (coord.eq.2) Dxyz(pos) = j
 	        if (coord.eq.3) Dxyz(pos) = k
 	    enddo
 	    enddo
 	    enddo

            call cg_coord_write_f(cg, base_no, zone_no, CGNS_ENUMV(RealDouble), &
                                  coordname(coord), Dxyz, coord_no, ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f

      enddo

! *** additional GridCoordinates_t Descriptions
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	do grid = 1,2
	    write(gridname,100)'GridCoordinates#',grid
	    call cg_grid_write_f(cg, base_no, zone_no, gridname, &
                                 grid_no, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f

	    call cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
                           'GridCoordinates_t', grid_no, 'end')
            if (ier .ne. ALL_OK) call cg_error_exit_f
	
	    call cg_array_write_f('CoordinateX', CGNS_ENUMV(RealDouble), 3, &
                                   size, Dxyz, ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f
            call cg_array_write_f('CoordinateY', CGNS_ENUMV(RealDouble), 3, &
                                   size, Dxyz, ier)
            call cg_array_write_f('CoordinateZ', CGNS_ENUMV(RealDouble), 3, &
                                   size, Dxyz, ier)

      ! *** Write DataClass_t under CoordinateX
	    call cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
              'GridCoordinates_t', grid_no, 'DataArray_t', 1, 'end')
            if (ier .ne. ALL_OK) call cg_error_exit_f

	    call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f
	enddo

! *** solution
      do sol=1, 2
 	    write(solname,100) 'Solution#',sol
 	    call cg_sol_write_f(cg, base_no, zone_no, solname, &
                                CGNS_ENUMV(Vertex), sol_no, ier)
 	    if (ier .ne. ALL_OK) call cg_error_exit_f

! *** solution field
 	    do field=1, 2
	    ! make up some dummy solution values
 	  	do i=1, num
 		    values(i) = i*field*sol
 	    	enddo
 	    	write(fieldname,100) 'Field#',field
 	    	call cg_field_write_f(cg, base_no, zone_no, sol_no, &
                    CGNS_ENUMV(RealDouble), fieldname, values, field_no, ier)
 	    	if (ier .ne. ALL_OK) call cg_error_exit_f

 	    enddo				! field loop
        enddo				! solution loop

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
! *** Rigid Grid Motion
	do rmotion=1, 2
	    write(rmotionname,100) 'RigidGridMotion#',rmotion
	    do coord=1, phys_dim
	        origin(coord,1) = (rmotion-1)*10.0
	        origin(coord,2) = origin(coord,1) + 10.0
	        angle(coord) = 5.0 * rmotion
		velocity(coord) = 7.0
	    enddo
	    call cg_rigid_motion_write_f(cg, base_no, zone_no, &
            rmotionname, CGNS_ENUMV(ConstantRate), rmotion_no, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f

! ** Goto RigidGridMotion_t node
	    call cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
                           'RigidGridMotion_t', rmotion_no, 'end')
	    if (ier .ne. ALL_OK) call cg_error_exit_f

! *** Add an attribute under RigidGridMotion_t
	    call cg_descriptor_write_f('DescriptorName', &
                                       'DescriptotText', ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f

! *** Add a DataArray_t under RigidGridMotion_t
	    data_size(1)=phys_dim
	    data_size(2)=2
            !call cg_array_write_f('OriginLocation', CGNS_ENUMV(RealSingle), 2,
            call cg_array_write_f('OriginLocation', CGNS_ENUMV(RealDouble), 2, &
                                   data_size, origin, ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f

            !call cg_array_write_f('RigidRotationAngle', CGNS_ENUMV(RealSingle), 1,
            call cg_array_write_f('RigidRotationAngle', CGNS_ENUMV(RealDouble), 1, &
                                  INT(phys_dim,cgsize_t), angle, ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f

	    !call cg_array_write_f('RigidVelocity', CGNS_ENUMV(RealSingle), 1,
	    call cg_array_write_f('RigidVelocity', CGNS_ENUMV(RealDouble), 1, &
                                  INT(phys_dim,cgsize_t), velocity, ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f

! *** Add an attribute for this data array: GOTO DataArray node
	    call cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
                           'RigidGridMotion_t', rmotion_no, &
                           'DataArray_t', 3, 'end')
	    if (ier .ne. ALL_OK) call cg_error_exit_f

            call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                                  CGNS_ENUMV(Radian), ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f

	enddo

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
! *** Arbitrary Grid Motion

        do amotion=1, 2

! *** Create the ArbitraryGridMotion_t node
            write(amotionname,100)'ArbitraryGridMotion#',amotion
	    call cg_arbitrary_motion_write_f(cg, base_no, zone_no, &
                 amotionname, CGNS_ENUMV(DeformingGrid), amotion_no, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f

! *** Goto the ArbitraryGridMotion_t node
            call cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
                          'ArbitraryGridMotion_t', amotion_no, 'end')
            if (ier .ne. ALL_OK) call cg_error_exit_f

!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
! *** Add an attribute under ArbitraryGridMotion_t
            call cg_descriptor_write_f('DescriptorName', &
                                       'DescriptotText', ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f

! *** make up some dummy GridVelocityX values just for the test:
            DO k=1, SIZE(3)
              DO j=1, SIZE(2)
                 DO  i=1, SIZE(1)
                    pos = i + (j-1)*SIZE(1) + (k-1)*SIZE(1)*SIZE(2)
                    GridVelocity(pos) = pos
                 ENDDO
              ENDDO
           ENDDO

! *** Add a DataArray_t 'GridVelocityX' under ArbitraryGridMotion_t
            !call cg_array_write_f('GridVelocityX', CGNS_ENUMV(RealSingle),
            call cg_array_write_f('GridVelocityX', CGNS_ENUMV(RealDouble), &
      		                  index_dim, size, GridVelocity, ier)
	    if (ier .ne. ALL_OK) call cg_error_exit_f

! *** Add an attribute for this data array: GOTO DataArray node
            call cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
                          'ArbitraryGridMotion_t', amotion_no, &
                          'DataArray_t', 1, 'end')
            if (ier .ne. ALL_OK) call cg_error_exit_f

            call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                                  CGNS_ENUMV(Radian), ier)
            if (ier .ne. ALL_OK) call cg_error_exit_f
	enddo

! *** close CGNS file
	call cg_close_f(cg, ier)
	if (ier .ne. ALL_OK) call cg_error_exit_f

 100	format(a,i1)
	end

