	program write_chemistry
	USE CGNS

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on March 8 2000

!       This example test the complete SIDS for multi-block data.
!	It creates a dummy mesh composed of 2 structured blocks in 3D.

! Modified for Version 2.1 chemistry extensions.
! D. Leich, Intelligent Light 11-Jan-02
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim, pos
	integer base_no, zone_no, coord_no, sol_no, discr_no, conn_no
	integer hole_no, boco_no, field_no, dset_no
	integer num, NormalIndex(Ndim)
	integer(cgsize_t) npnts
	integer(cgsize_t) size(Ndim*3)
	integer cg, ier, zone, coord, i, sol, field
	integer(cgsize_t) pnts(Ndim,120), donor_pnts(Ndim,120)
	integer transform(Ndim)
        real*4 units(5)
	real*4 data(120), normals(360)
	double precision Dxyz(120), values(120)
	character*32 zonename, solname, fieldname
	character*32 coordname(Ndim)
	character*32 donorname
        real*4 hofCO, hofO2, hofCO2, RThof

	hofCO  = -26.42
	hofO2  = 0.0
	hofCO2 = -94.054
	RThof  = 298.15

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
 	if (ier .eq. ERROR) call cg_error_exit_f

! *** base
 	call cg_base_write_f(cg, 'Basename', cell_dim, phys_dim, &
                             base_no, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f

! *** Create 'FlowEquationSet' node under 'CGNSBase_t'
	call cg_goto_f(cg, base_no, ier, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_equationset_write_f(Ndim,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** Create 'ChemicalKinetics' under 'FlowEquationSet'
        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_model_write_f('ChemicalKineticsModel_t', &
          CGNS_ENUMV(ChemicalEquilibCurveFit),ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** Create 'ThermalRelaxation' under 'FlowEquationSet'
        call cg_model_write_f('ThermalRelaxationModel_t', &
          CGNS_ENUMV(Frozen),ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** Create 'GasModel' under 'FlowEquationSet'
        call cg_model_write_f('GasModel_t', &
          CGNS_ENUMV(ConstantDensity),ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** Create 'HeatOfFormationCO' under 'ChemicalKineticsModel'
        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_array_write_f('HeatOfFormationCO', CGNS_ENUMV(RealSingle),1, &
          1_cgsize_t,hofCO,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1, &
          'DataArray_t',1,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional),ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_units_write_f(CG_UserDefined,CG_UserDefined,CG_UserDefined, &
            CG_UserDefined,CG_UserDefined,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('Units','kcal/mole',ier)
        if (ier .eq. ERROR) call cg_error_exit_f


! *** Create 'HeatOfFormationO2' under 'ChemicalKineticsModel'
        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_array_write_f('HeatOfFormationO2',CGNS_ENUMV(RealSingle),1, &
          1_cgsize_t,hofO2,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1, &
          'DataArray_t',2,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional),ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_units_write_f(CG_UserDefined,CG_UserDefined,CG_UserDefined, &
            CG_UserDefined,CG_UserDefined,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('Units','kcal/mole',ier)
        if (ier .eq. ERROR) call cg_error_exit_f


! *** Create 'HeatOfFormationCO2' under 'ChemicalKineticsModel'
        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_array_write_f('HeatOfFormationCO2',CGNS_ENUMV(RealSingle),1, &
          1_cgsize_t,hofCO2,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1, &
          'DataArray_t',3,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional),ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_units_write_f(CG_UserDefined,CG_UserDefined,CG_UserDefined, &
            CG_UserDefined,CG_UserDefined,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('Units','kcal/mole',ier)
        if (ier .eq. ERROR) call cg_error_exit_f


! *** Create 'ReferenceTemperatureHOF' under 'ChemicalKineticsModel'
        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_array_write_f('ReferenceTemperatureHOF',CGNS_ENUMV(RealSingle),1, &
          1_cgsize_t,RThof,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1, &
          'DataArray_t',4,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional),ier)
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_units_write_f(CG_Null,CG_Null,CG_Null,CGNS_ENUMV(Kelvin),CG_Null,ierr)
        if (ier .eq. ERROR) call cg_error_exit_f

        units(1)=0
        units(2)=0
        units(3)=0
        units(4)=1
        units(5)=0
        call cg_exponents_write_f(CGNS_ENUMV(RealSingle),units,ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** Create 'ReactionEquation' under 'ChemicalKineticsModel'
        call cg_goto_f(cg, base_no, ier, &
          'FlowEquationSet_t',1,'ChemicalKineticsModel_t',1,'end')
        if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('ReactionEquation', &
            'CO + 0.5O2 = CO2',ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** zone
	do zone=1, 2
    	    write(zonename,'(a5,i1)') 'zone#',zone
    	    num = 1
	    do i=1,index_dim          		! zone#1: 3*4*5, zone#2: 4*5*6
                size(i) = i+zone+1		! nr of nodes in i,j,k
		size(i+Ndim) = size(i)-1	! nr of elements in i,j,k
	 	size(i+2*Ndim) = 0		! nr of bnd nodes if ordered
       		num = num * size(i)		! nr of nodes
    	    enddo
    	    call cg_zone_write_f(cg, base_no, zonename, size, &
                                 CGNS_ENUMV(Structured), zone_no, ier)
    	    if (ier .eq. ERROR) call cg_error_exit_f

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
         	if (ier .eq. ERROR) call cg_error_exit_f

          enddo

! *** solution
          do sol=1, 2
 		write(solname,'(a5,i1,a5,i1)') 'Zone#',zone,' sol#',sol
 		call cg_sol_write_f(cg, base_no, zone_no, solname, &
                                    CGNS_ENUMV(Vertex), sol_no, ier)
 		if (ier .eq. ERROR) call cg_error_exit_f

! *** solution field
 		do field=1, 2
		    ! make up some dummy solution values
 	    	    do i=1, num
 		  	values(i) = i*field*sol
 	    	    enddo
 	    	    write(fieldname,'(a6,i1)') 'Field#',field
 	    	    call cg_field_write_f(cg, base_no, zone_no, sol_no, &
                        CGNS_ENUMV(RealDouble), fieldname, values, field_no, ier)
 	    	    if (ier .eq. ERROR) call cg_error_exit_f

 		enddo				! field loop
            enddo				! solution loop

! *** discrete data
	    call cg_discrete_write_f(cg, base_no, zone_no, 'discrete#1', &
                                     discr_no, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

  	! *** discrete data arrays, defined on vertices:
	    call cg_goto_f(cg, base_no, ier, 'Zone_t', zone, &
                           'DiscreteData_t', discr_no, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

	    DO k=1, SIZE(3)
               DO j=1, SIZE(2)
                  DO i=1, SIZE(1)
                     pos = i + (j-1)*SIZE(1) + (k-1)*SIZE(1)*SIZE(2)
                     DATA(pos) = pos	! * make up some dummy data
                  ENDDO
               ENDDO
            ENDDO
	    CALL cg_array_write_f('arrayname', CGNS_ENUMV(RealSingle), index_dim, &
                                   size, data, ier)
            if (ier .eq. ERROR) call cg_error_exit_f

  	! *** discrete data arrays attribute: GOTO DataArray node
	    call cg_goto_f(cg, base_no, ier, 'Zone_t', zone, &
      	            'DiscreteData_t', discr_no, 'DataArray_t', 1, 'end')
	    if (ier .eq. ERROR) call cg_error_exit_f

	    call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                                  CGNS_ENUMV(Radian), ier)
            if (ier .eq. ERROR) call cg_error_exit_f

! *** overset holes
	 !  create dummy data
	    do i=1,3
	      ! Define 2 separate CGNS_ENUMV(PointRange), for 2 patches in the hole
		pnts(i,1)=1
		pnts(i,2)=size(i)
	      ! second PointRange of hole
		pnts(i,3)=2
		pnts(i,4)=size(i)
	    enddo
	  ! Hole defined with 2 point set type CGNS_ENUMV(PointRange), so 4 points:
	    call cg_hole_write_f(cg, base_no, zone_no, 'hole#1', CGNS_ENUMV(Vertex), &
                                 CGNS_ENUMV(PointRange), 2, 4_cgsize_t, pnts, &
                                 hole_no, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! *** general connectivity
            DO n=1, 5
               DO i=1,3
                  pnts(i,n)=i		! * dummy data
                  donor_pnts(i,n)=i*2
               ENDDO
            ENDDO
	  ! create a point matching connectivity
	    call cg_conn_write_f(cg, base_no, zone_no, 'Connect#1', &
                CGNS_ENUMV(Vertex), CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointList), 5_cgsize_t, pnts, &
                'zone#2', &
                CGNS_ENUMV(Structured), CGNS_ENUMV(PointListDonor), CGNS_ENUMV(Integer), 5_cgsize_t, &
                donor_pnts, conn_no, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! *** connectivity 1to1
	 !  generate data
	    do i=1,3
	     !**make up some dummy data:
                pnts(i,1)=1
                pnts(i,2)=size(i)
		donor_pnts(i,1)=1
		donor_pnts(i,2)=size(i)
		transform(i)=i*(-1)
            enddo
	    if (zone .eq. 1) then
		donorname='zone#2'
	    else if (zone .eq. 2) then
		donorname='zone#1'
	    endif

	    call cg_1to1_write_f(cg, base_no, zone_no, '1to1_#1', &
      		donorname, pnts, donor_pnts, transform, conn_no, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! *** ZoneGridConnectivity attributes:  GOTO ZoneGridConnectivity_t node
	    call cg_goto_f(cg, base_no, ier, 'Zone_t', zone, &
                           'ZoneGridConnectivity_t', 1, 'end')
	    if (ier .eq. ERROR) call cg_error_exit_f

! *** ZoneGridConnectivity attributes: Descriptor_t
	    call cg_descriptor_write_f('DescriptorName', &
                                       'Zone Connectivity', ier)

! *** bocos
	    call cg_boco_write_f(cg, base_no, zone_no, 'boco#1', &
                 CGNS_ENUMV(BCInflow), CGNS_ENUMV(PointRange), 2_cgsize_t, pnts, boco_no, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! *** boco normal
	    npnts = 1
	    do i=1,Ndim
	        NormalIndex(i)=0
		! compute nr of points on bc patch:
		npnts = npnts * (pnts(i,2)-pnts(i,1)+1)
	    enddo
	    NormalIndex(1)=1
	    do i=1,phys_dim*npnts
		normals(i)=i
	    enddo

	    call cg_boco_normal_write_f(cg, base_no, zone_no, boco_no, &
               NormalIndex, 1, CGNS_ENUMV(RealSingle), normals, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! ** boundary condition attributes: GOTO BC_t node
            call cg_goto_f(cg, base_no, ier, 'Zone_t', zone, 'ZoneBC_t', &
                1, 'BC_t', boco_no, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

! ** boundary condition attributes:  GridLocation_t
            call cg_gridlocation_write_f(CGNS_ENUMV(Vertex), ier)
            if (ier .eq. ERROR) call cg_error_exit_f

! ** boundary condition dataset
	    call cg_dataset_write_f(cg, base_no, zone, &
               boco_no, 'DataSetName', CGNS_ENUMV(BCInflow), dset_no, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! ** boundary condition data:
	    call cg_bcdata_write_f(cg, base_no, zone, &
               boco_no, dset_no, CGNS_ENUMV(Neumann), ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

! ** boundary condition data arrays: GOTO BCData_t node
	    call cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
                'ZoneBC_t', 1, 'BC_t', boco_no, 'BCDataSet_t', dset_no, &
                'BCData_t', CGNS_ENUMV(Neumann), 'end')
	    if (ier .eq. ERROR) call cg_error_exit_f

	    do i=1, npnts
		data(i) = i
	    enddo
	    call cg_array_write_f('dataset_arrayname', CGNS_ENUMV(RealSingle), &
                 1, npnts, data, ier)
	    if (ier .eq. ERROR) call cg_error_exit_f

 	enddo					! zone loop

! *** close CGNS file
	call cg_close_f(cg, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

	end

