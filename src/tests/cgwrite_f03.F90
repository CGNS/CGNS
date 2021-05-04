MODULE callback

CONTAINS

  SUBROUTINE error_exit(iserr, msg) bind(C)

    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER, PARAMETER :: MSG_SIZE = 34
    CHARACTER(LEN=MSG_SIZE), PARAMETER :: msg_correct = "file type unknown or not supported"
    INTEGER(C_INT), VALUE :: iserr
    CHARACTER(LEN=1), DIMENSION(*) :: msg
    CHARACTER(LEN=MSG_SIZE) :: msg_check
    INTEGER i

    DO i=1, 34
       msg_check(i:i) = msg(i)
    ENDDO
    IF(msg_check .NE. msg_correct)THEN
       PRINT*,"ERROR:  cg_configure_f failed for CG_CONFIG_ERROR"
       STOP
    ENDIF

  END SUBROUTINE error_exit
END MODULE callback

PROGRAM write_cgns_1
#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif
  USE CGNS
  USE ISO_C_BINDING
  USE callback
  IMPLICIT NONE

  ! author: Diane Poirier (diane@icemcfd.com)

  ! This example test the complete SIDS for multi-block data.
  ! It creates a dummy mesh composed of 2 structured blocks in 3D.
  ! It also tests the cg_configure_f API.

  INTEGER Ndim
  PARAMETER (Ndim = 3)
  INTEGER :: one
  PARAMETER (one = 1)
  INTEGER, PARAMETER :: sp = KIND(1.0)

  INTEGER :: index_dim, cell_dim, phys_dim
  INTEGER :: base_no, zone_no, coord_no, sol_no, discr_no, conn_no
  INTEGER :: hole_no, boco_no, field_no, dset_no
  INTEGER :: num, NormalIndex(Ndim)
  INTEGER :: ndims
  INTEGER(CGSIZE_T) :: size(Ndim*3)
  INTEGER(CGSIZE_T) :: npnts
  INTEGER :: zone, coord, i, j, k, n, pos, sol, field
  INTEGER cg, ier
  INTEGER(CGSIZE_T) :: pnts(Ndim,120), donor_pnts(Ndim,120)
  INTEGER transform(Ndim)
  INTEGER :: nptsets, nrmlistflag
  REAL(KIND=sp) DATA(120), normals(360)
  DOUBLE PRECISION Dxyz(120), values(120)
  CHARACTER(LEN=32) zonename, solname, fieldname
  CHARACTER(LEN=32) coordname(Ndim)
  CHARACTER(LEN=32) donorname

  INTEGER, TARGET :: value_f
  INTEGER(C_SIZE_T), TARGET :: value_size_t_f
  CHARACTER(LEN=32), TARGET :: path

  coordname(1) = 'CoordinateX'
  coordname(2) = 'CoordinateY'
  coordname(3) = 'CoordinateZ'

! *** initialize
  ier = 0
  index_dim=Ndim
  cell_dim=Ndim
  phys_dim=Ndim

  ! *** open CGNS file for writing
  CALL cg_open_f('cgtest.cgns', CG_MODE_WRITE, cg, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  ! *** base
  CALL cg_base_write_f(cg, 'Basename', cell_dim, phys_dim, base_no, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

! *** zone
  DO zone=1, 2
     WRITE(zonename,'(a5,i1)') 'zone#',zone
     num = 1
     DO i=1,index_dim                    ! zone#1: 3*4*5, zone#2: 4*5*6
        size(i) = i+zone+1              ! nr of nodes in i,j,k
        size(i+Ndim) = size(i)-1        ! nr of elements in i,j,k
        size(i+2*Ndim) = 0              ! nr of bnd nodes if ordered
        num = num * size(i)             ! nr of nodes
     ENDDO
     CALL cg_zone_write_f(cg, base_no, zonename, size, &
          CGNS_ENUMV(Structured), zone_no, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! *** coordinate
     DO coord=1, phys_dim
        DO k=1, SIZE(3)
           DO j=1, SIZE(2)
              DO i=1, SIZE(1)
                 pos = i + (j-1)*SIZE(1) + (k-1)*SIZE(1)*SIZE(2)
                 ! * make up some dummy coordinates just for the test:
                 IF (coord.EQ.1) Dxyz(pos) = i
                 IF (coord.EQ.2) Dxyz(pos) = j
                 IF (coord.EQ.3) Dxyz(pos) = k
              ENDDO
           ENDDO
        ENDDO
 
        CALL cg_coord_write_f(cg, base_no, zone_no, CGNS_ENUMV(RealDouble), &
             coordname(coord), Dxyz, coord_no, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ENDDO

     ! *** solution
     DO sol=1, 2
        WRITE(solname,'(a5,i1,a5,i1)') 'Zone#',zone,' sol#',sol
        CALL cg_sol_write_f(cg, base_no, zone_no, solname, &
             CGNS_ENUMV(Vertex), sol_no, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f

        ! *** solution field
        DO field=1, 2
           ! make up some dummy solution values
           DO i=1, num
              values(i) = i*field*sol
           ENDDO
           WRITE(fieldname,'(a6,i1)') 'Field#',field
           CALL cg_field_write_f(cg, base_no, zone_no, sol_no, &
                CGNS_ENUMV(RealDouble), fieldname, values, field_no, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           
        ENDDO                ! field loop
     ENDDO                ! solution loop
     
     ! *** discrete data
     CALL cg_discrete_write_f(cg, base_no, zone_no, 'discrete#1', &
          discr_no, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

! *** discrete data arrays, defined on vertices:
     CALL cg_goto_f(cg, base_no, ier, 'Zone_t', zone, &
          'DiscreteData_t', discr_no, 'end')
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     DO k=1, SIZE(3)
        DO j=1, SIZE(2)
           DO i=1, SIZE(1)
              pos = i + (j-1)*SIZE(1) + (k-1)*SIZE(1)*SIZE(2)
              DATA(pos) = pos    ! * make up some dummy data
           ENDDO
        ENDDO
     ENDDO
     CALL cg_array_write_f('arrayname', CGNS_ENUMV(RealSingle), index_dim, &
          size, DATA, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

! *** discrete data arrays attribute: GOTO DataArray node
     CALL cg_goto_f(cg, base_no, ier, 'Zone_t', zone, &
          'DiscreteData_t', discr_no, 'DataArray_t', 1, 'end')
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     CALL cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), &
          CGNS_ENUMV(Kelvin), CGNS_ENUMV(Radian), ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! *** overset holes
     !  create dummy data
     DO i=1,3
        ! Define 2 separate PointRange, for 2 patches in the hole
        pnts(i,1)=1
        pnts(i,2)=SIZE(i)
        ! second PointRange of hole
        pnts(i,3)=2
        pnts(i,4)=SIZE(i)
     ENDDO
     ! Hole defined with 2 point set type PointRange, so 4 points:
     nptsets = 2
     npnts = 4
     CALL cg_hole_write_f(cg, base_no, zone_no, 'hole#1', CGNS_ENUMV(Vertex),&
          CGNS_ENUMV(PointRange), nptsets, npnts, pnts, hole_no, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

! *** general connectivity
     DO n=1, 5
        DO i=1,3
           pnts(i,n)=i        ! * dummy data
           donor_pnts(i,n)=i*2
        ENDDO
     ENDDO
     ! create a point matching connectivity
     npnts = 5
     CALL cg_conn_write_f(cg, base_no, zone_no, 'Connect#1', &
          CGNS_ENUMV(Vertex), CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointList), npnts, pnts, 'zone#2', &
          CGNS_ENUMV(Structured), CGNS_ENUMV(PointListDonor), CGNS_ENUMV(integer), &
          npnts, donor_pnts, conn_no, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

! *** connectivity 1to1
     !  generate data
     DO i=1,3
        !**make up some dummy data:
        pnts(i,1)=1
        pnts(i,2)=SIZE(i)
        donor_pnts(i,1)=1
        donor_pnts(i,2)=SIZE(i)
        transform(i)=i*(-1)
     ENDDO
     IF (zone .EQ. 1) THEN
        donorname='zone#2'
     ELSE IF (zone .EQ. 2) THEN
        donorname='zone#1'
     ENDIF

     CALL cg_1to1_write_f(cg, base_no, zone_no, '1to1_#1', &
          donorname, pnts, donor_pnts, transform, conn_no, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f
     ! *** ZoneGridConnectivity attributes:  GOTO ZoneGridConnectivity_t node
     CALL cg_goto_f(cg, base_no, ier, 'Zone_t', zone, &
          'ZoneGridConnectivity_t', one, 'end')
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! *** ZoneGridConnectivity attributes: Descriptor_t
     CALL cg_descriptor_write_f('DescriptorName', &
          'Zone Connectivity', ier)

     ! *** bocos
     npnts = 2
     CALL cg_boco_write_f(cg, base_no, zone_no, 'boco#1', &
          CGNS_ENUMV(BCInflow), CGNS_ENUMV(PointRange), npnts, pnts, boco_no, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! *** boco normal
     npnts = 1
     DO i=1,Ndim
        NormalIndex(i)=0
        ! compute nr of points on bc patch:
        npnts = npnts * (pnts(i,2)-pnts(i,1)+1)
     ENDDO
     NormalIndex(1)=1
     DO i=1,phys_dim*npnts
        normals(i)=i
     ENDDO

     nrmlistflag = 1
     CALL cg_boco_normal_write_f(cg, base_no, zone_no, boco_no, &
          NormalIndex, nrmlistflag, CGNS_ENUMV(RealSingle), normals, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! ** boundary condition attributes: GOTO BC_t node
     CALL cg_goto_f(cg, base_no, ier, 'Zone_t', zone, 'ZoneBC_t', &
          one, 'BC_t', boco_no, 'end')
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! ** boundary condition attributes:  GridLocation_t
     CALL cg_gridlocation_write_f(CGNS_ENUMV(Vertex), ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! ** boundary condition dataset
     CALL cg_dataset_write_f(cg, base_no, zone, &
          boco_no, 'DataSetName', CGNS_ENUMV(BCInflow), dset_no, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! ** boundary condition data:
     CALL cg_bcdata_write_f(cg, base_no, zone, &
          boco_no, dset_no, CGNS_ENUMV(Neumann), ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

! ** boundary condition data arrays: GOTO BCData_t node
     CALL cg_goto_f(cg, base_no, ier, 'Zone_t', zone_no, &
          'ZoneBC_t', one, 'BC_t', boco_no, 'BCDataSet_t', &
          dset_no, 'BCData_t', CGNS_ENUMV(Neumann), 'end')
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     DO i=1, npnts
        DATA(i) = i
     ENDDO
     ndims = 1
     CALL cg_array_write_f('dataset_arrayname', CGNS_ENUMV(RealSingle), &
          ndims, [npnts], DATA, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

! ** boundary condition data attributes:
     CALL cg_dataclass_write_f(CGNS_ENUMV(NormalizedByDimensional), ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

  ENDDO ! zone loop

  ! *** close CGNS file
  CALL cg_close_f(cg, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

#if CG_BUILD_HDF5
! Disable with gfortran, GCC Bugzilla - Bug 99982
#ifndef __GFORTRAN__
  ! **************************
  ! Test cg_configure options
  ! **************************
  value_f = 1
  CALL cg_configure_f(CG_CONFIG_HDF5_DISKLESS, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  ! enable committing memory to disk
  value_f = 1
  CALL cg_configure_f(CG_CONFIG_HDF5_DISKLESS_WRITE, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  value_size_t_f = INT(20*1024*1024,C_SIZE_T)
  CALL cg_configure_f(CG_CONFIG_HDF5_DISKLESS_INCR, C_LOC(value_size_t_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  CALL cg_open_f('cgtest_core.cgns', CG_MODE_WRITE, cg, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  ! *** base
  CALL cg_base_write_f(cg, 'Basename', cell_dim, phys_dim, base_no, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  CALL cg_close_f(cg, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  value_f = 0
  CALL cg_configure_f(CG_CONFIG_HDF5_DISKLESS, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  value_f = CG_FILE_ADF2
  value_f = CG_FILE_ADF
  CALL cg_configure_f(CG_CONFIG_FILE_TYPE, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  value_f = CG_FILE_HDF5
  CALL cg_configure_f(CG_CONFIG_FILE_TYPE, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  value_f = -1
  CALL cg_configure_f(CG_CONFIG_HDF5_COMPRESS, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  value_f = -1
  CALL cg_configure_f(CG_CONFIG_COMPRESS, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  value_f =  CG_CONFIG_RIND_ZERO

  CALL cg_configure_f(CG_CONFIG_RIND_INDEX, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  value_f = CG_CONFIG_RIND_CORE
  CALL cg_configure_f(CG_CONFIG_RIND_INDEX, C_LOC(value_f), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  path = "/test/a/b"//C_NULL_CHAR
  CALL cg_configure_f(CG_CONFIG_SET_PATH, C_LOC(path(1:1)), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  path = "/test/c/d"//C_NULL_CHAR
  CALL cg_configure_f(CG_CONFIG_ADD_PATH, C_LOC(path(1:1)), ier)

  path = C_NULL_CHAR
  CALL cg_configure_f(CG_CONFIG_SET_PATH, C_LOC(path(1:1)), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  CALL cg_configure_f(CG_CONFIG_ERROR, c_funloc(error_exit), ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

  value_f = 100 ! Trigger an error
  CALL cg_configure_f(CG_CONFIG_FILE_TYPE, C_LOC(value_f), ier)
  IF (ier .NE. ERROR) CALL cg_error_exit_f
#endif
#endif

END PROGRAM write_cgns_1
