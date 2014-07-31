#include "cgnstypes_f.h"
! @file benchmark_hdf5.F90
! @author M. Scot Breitenfeld <brtnfld@hdfgroup.org>
! @version 0.1
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Benchmarking program for pcgns library
MODULE cgns_c_bindings

!
! Contains needed interfaces for calling the C functions
! 
  
  IMPLICIT NONE

  INTERFACE
     INTEGER(C_INT) FUNCTION cgp_open(filename, mode, fn) BIND(C, name='cgp_open')
       USE ISO_C_BINDING
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: filename
       INTEGER(C_INT)  , INTENT(IN), VALUE  :: mode
       INTEGER(C_INT)  , INTENT(OUT) :: fn
     END FUNCTION cgp_open
  END INTERFACE

  INTERFACE
     INTEGER(C_INT) FUNCTION cgp_pio_mode(mode) BIND(C, name='cgp_pio_mode')
       USE ISO_C_BINDING
       INTEGER(C_INT)  , INTENT(IN), VALUE  :: mode
     END FUNCTION cgp_pio_mode
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_base_write(fn, basename, cell_dim, phys_dim, B) BIND(C, name='cg_base_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: basename
       INTEGER(C_INT)   , INTENT(IN), VALUE  :: cell_dim
       INTEGER(C_INT)   , INTENT(IN), VALUE  :: phys_dim
       INTEGER(C_INT)   , INTENT(OUT)  :: B
     END FUNCTION cg_base_write
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_zone_write(fn, B, zonename, nijk, itype, Z) BIND(C, name='cg_zone_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE  :: B
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: zonename
       CGSIZE_T, DIMENSION(*), INTENT(IN)  :: nijk
       INTEGER(C_INT)   , INTENT(IN), VALUE  :: itype
       INTEGER(C_INT)   , INTENT(OUT)  :: Z
     END FUNCTION cg_zone_write
  END INTERFACE 

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_base_read(fn, B, basename, cell_dim, phys_dim) BIND(C, name='cg_base_read')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(OUT)  :: basename
       INTEGER(C_INT)   , INTENT(OUT)  :: cell_dim
       INTEGER(C_INT)   , INTENT(OUT)  :: phys_dim
     END FUNCTION cg_base_read
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_zone_read(fn, B, Z, zonename, nijk) BIND(C, name='cg_zone_read')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(OUT)  :: zonename
       CGSIZE_T, DIMENSION(*), INTENT(OUT)  :: nijk
     END FUNCTION cg_zone_read
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_coord_write(fn, B, Z, itype, coordname, C) BIND(C, name='cgp_coord_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: coordname
       INTEGER(C_INT)   , INTENT(OUT)  :: C
     END FUNCTION cgp_coord_write
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_coord_write_data(fn, B, Z, C, rmin, rmax, coords) BIND(C, name='cgp_coord_write_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: C
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: coords
     END FUNCTION cgp_coord_write_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_field_write(fn, B, Z, S, itype, fieldname, F) BIND(C, name='cgp_field_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: fieldname
       INTEGER(C_INT)   , INTENT(OUT)  :: F
     END FUNCTION cgp_field_write
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_field_write_data(fn, B, Z, S, F, rmin, rmax, data) BIND(C, name='cgp_field_write_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       INTEGER(C_INT)   , INTENT(IN), VALUE :: F
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: data
     END FUNCTION cgp_field_write_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_field_read_data(fn, B, Z, S, F, rmin, rmax, data) BIND(C, name='cgp_field_read_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       INTEGER(C_INT)   , INTENT(IN), VALUE :: F
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: data
     END FUNCTION cgp_field_read_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_coord_read_data(fn, B, Z, C, rmin, rmax, coords) BIND(C, name='cgp_coord_read_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: C
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: coords
     END FUNCTION cgp_coord_read_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_section_write(fn,B,Z,sectionname,itype,start,end,nbndry,S) BIND(C, name='cgp_section_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: sectionname
       INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
       CGSIZE_T, INTENT(IN), VALUE :: start
       CGSIZE_T, INTENT(IN), VALUE :: end
       INTEGER(C_INT)   , INTENT(IN), VALUE :: nbndry
       INTEGER(C_INT)   , INTENT(OUT) :: S
     END FUNCTION cgp_section_write
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_array_write(arrayname,itype,DataDimension,DimensionVector,A) BIND(C, name='cgp_array_write')
       USE ISO_C_BINDING
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: arrayname
       INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
       INTEGER(C_INT)   , INTENT(IN), VALUE :: DataDimension
       CGSIZE_T, DIMENSION(1:DataDimension), INTENT(IN) :: DimensionVector
       INTEGER(C_INT)   , INTENT(OUT) :: A
     END FUNCTION cgp_array_write
  END INTERFACE


  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_array_write_data(A, rmin, rmax, data) BIND(C, name='cgp_array_write_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: A
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: data
     END FUNCTION cgp_array_write_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_elements_write_data(fn,B,Z,S,emin,emax,elements) BIND(C, name='cgp_elements_write_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       CGSIZE_T, INTENT(IN), VALUE :: emin
       CGSIZE_T, INTENT(IN), VALUE :: emax
       TYPE(C_PTR), VALUE :: elements
     END FUNCTION cgp_elements_write_data
  END INTERFACE


  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_elements_read_data(fn,B,Z,S,start,end,elements) BIND(C, name='cgp_elements_read_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       INTEGER(C_INT)   , INTENT(IN), VALUE :: S
       CGSIZE_T, INTENT(IN), VALUE :: start
       CGSIZE_T, INTENT(IN), VALUE :: end
       TYPE(C_PTR), VALUE :: elements
     END FUNCTION cgp_elements_read_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_array_read_data(A, rmin, rmax, data) BIND(C, name='cgp_array_read_data')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: A
       CGSIZE_T, INTENT(IN) :: rmin
       CGSIZE_T, INTENT(IN) :: rmax
       TYPE(C_PTR), VALUE :: data
     END FUNCTION cgp_array_read_data
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_sol_write(fn,B,Z,solname,location,S) BIND(C, name='cg_sol_write')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: solname
       INTEGER(C_INT)   , INTENT(IN), VALUE :: location
       INTEGER(C_INT)   , INTENT(OUT) :: S
     END FUNCTION cg_sol_write
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_error_exit() BIND(C, name='cgp_error_exit')
       USE ISO_C_BINDING
     END FUNCTION cgp_error_exit
  END INTERFACE
  
  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_close(fn) BIND(C, name='cgp_close')
       USE ISO_C_BINDING
       INTEGER(C_INT), INTENT(IN), VALUE :: fn
     END FUNCTION cgp_close
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_queue_set(use_queue) BIND(C, name='cgp_queue_set')
       USE ISO_C_BINDING
       INTEGER(C_INT), INTENT(IN), VALUE :: use_queue
     END FUNCTION cgp_queue_set
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cgp_queue_flush() BIND(C, name='cgp_queue_flush')
       USE ISO_C_BINDING
     END FUNCTION cgp_queue_flush
  END INTERFACE

  INTERFACE     
     INTEGER(C_INT) FUNCTION cg_user_data_write(UserDataName) BIND(C, name='cg_user_data_write')
       USE ISO_C_BINDING
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: UserDataName
     END FUNCTION cg_user_data_write
  END INTERFACE
  
  INTERFACE 
     INTEGER(C_INT) FUNCTION cg_gorel(fn, UserDataName, i, end ) BIND(C, name='cg_gorel')
       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: UserDataName
       INTEGER(C_INT)   , INTENT(IN), VALUE :: i
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: end
     END FUNCTION cg_gorel
  END INTERFACE

  ! The number of optional parameters should be set to CG_MAX_GOTO_DEPTH, which
  ! is currently set to 20.
  INTERFACE 
     INTEGER(C_INT) FUNCTION cg_goto(fn, B, &
          UserDataName1, i1, UserDataName2, i2, &
          UserDataName3, i3, UserDataName4, i4, &
          UserDataName5, i5, UserDataName6, i6, &
          UserDataName7, i7, UserDataName8, i8, &
          UserDataName9, i9, UserDataName10, i10, &
          UserDataName11, i11, UserDataName12, i12, &
          UserDataName13, i13, UserDataName14, i14, &
          UserDataName15, i15, UserDataName16, i16, &
          UserDataName17, i17, UserDataName18, i18, &
          UserDataName19, i19, UserDataName20, i20, &
          end) BIND(C, name='cg_goto_f03')

       USE ISO_C_BINDING
       INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
       INTEGER(C_INT)   , INTENT(IN), VALUE :: B
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName1
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i1
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName2
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i2
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName3
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i3
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName4
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i4
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName5
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i5
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName6
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i6
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName7
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i7
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName8
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i8
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName9
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i9
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName10
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i10
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName11
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i11
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName12
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i12
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName13
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i13
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName14
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i14
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName15
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i15
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName16
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i16
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName17
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i17
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName18
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i18
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName19
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i19
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName20
       INTEGER(C_INT)   , INTENT(IN), OPTIONAL :: i20
       CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: end 
     END FUNCTION cg_goto
  END INTERFACE

END MODULE cgns_c_bindings

PROGRAM main

  USE mpi
  USE cgns_c_bindings
  USE ISO_C_BINDING
  IMPLICIT NONE

  INCLUDE 'cgnslib_f.h'

  CGSIZE_T, PARAMETER :: Nelem = 16 ! Use multiples of number of cores per node
  CGSIZE_T, PARAMETER :: NodePerElem = 8

  CGSIZE_T :: Nnodes
  INTEGER :: mpi_err
  INTEGER(C_INT) :: err
  INTEGER :: comm_size
  INTEGER :: comm_rank
  INTEGER :: info
  INTEGER(C_INT) :: fn
  INTEGER(C_INT) :: B
  INTEGER(C_INT) :: Z
  INTEGER(C_INT) :: S
  INTEGER(C_INT) :: Cx,Cy,Cz, Fx, Fy, Fz, Ax, Ay, Az
  INTEGER(C_INT), PARAMETER :: cell_dim = 3
  INTEGER(C_INT), PARAMETER :: phys_dim = 3
  INTEGER(C_INT) :: r_cell_dim = 0
  INTEGER(C_INT) :: r_phys_dim = 0
  CGSIZE_T, DIMENSION(1:3) :: nijk, sizes
  CGSIZE_T, DIMENSION(1:1) :: size_1D
  CGSIZE_T :: min, max
  INTEGER(C_INT) :: k, count
  REAL(C_DOUBLE), DIMENSION(:), ALLOCATABLE, TARGET :: Coor_x, Coor_y, Coor_z, Data_Fx, Data_Fy, Data_Fz
  REAL(C_DOUBLE), DIMENSION(:), ALLOCATABLE, TARGET :: Array_x, Array_y, Array_z
  CGSIZE_T :: start, end, emin, emax
  CGSIZE_T, DIMENSION(:), ALLOCATABLE, TARGET :: elements
  logical :: queue
  TYPE(C_PTR) :: f_ptr
  CHARACTER(KIND=C_CHAR,LEN=180) :: bname, zname
  INTEGER :: indx_null

  TYPE, BIND(C) :: goto_struct_1
     CHARACTER(C_CHAR), DIMENSION(1:180) :: UserDataName
     INTEGER(C_INT) :: i
     CHARACTER(C_CHAR), DIMENSION(1:180) :: end
  END TYPE goto_struct_1

  TYPE(goto_struct_1), TARGET, DIMENSION(1:1) :: gt_1


  CALL MPI_INIT(mpi_err)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,comm_size,mpi_err)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,comm_rank,mpi_err)

  queue = .FALSE.

  Nnodes = Nelem*NodePerElem

  nijk(1) = Nnodes ! Number of vertices
  nijk(2) = Nelem ! Number of cells
  nijk(3) = 0 ! Number of boundary vertices

! ======================================
! ==    **WRITE THE CGNS FILE *       ==
! ======================================

  err = cgp_open("test_unstructured_F90.cgns"//C_NULL_CHAR, CG_MODE_WRITE, fn)
  IF(err.NE.CG_OK) PRINT*,'*FAILED* cgp_open'
  err = cg_base_write(fn, "Base 1"//C_NULL_CHAR, cell_dim, phys_dim, B)
  IF(err.NE.CG_OK) PRINT*,'*FAILED* cgp_base_write'
  err = cg_zone_write(fn, B, "Zone 1"//C_NULL_CHAR, nijk, Unstructured, Z)
  IF(err.NE.CG_OK) PRINT*,'*FAILED* cgp_zone_write'

! ======================================
! == (A) WRITE THE NODAL COORDINATES  ==
! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Coor_x(1:count))
  ALLOCATE(Coor_y(1:count))
  ALLOCATE(Coor_z(1:count))

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  DO k = 1, count
     Coor_x(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.1_C_DOUBLE
     Coor_y(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.2_C_DOUBLE
     Coor_z(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.3_C_DOUBLE
  ENDDO

  err = cgp_coord_write(fn,B,Z,RealDouble,"CoordinateX"//C_NULL_CHAR,Cx)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write'
     err = cgp_error_exit()
  ENDIF
  err = cgp_coord_write(fn,B,Z,RealDouble,"CoordinateY"//C_NULL_CHAR,Cy)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write'
     err = cgp_error_exit()
  ENDIF
  err = cgp_coord_write(fn,B,Z,RealDouble,"CoordinateZ"//C_NULL_CHAR,Cz)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write'
     err = cgp_error_exit()
  ENDIF

  ! use queued IO
  IF(queue) err = cgp_queue_set(1)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_queue_set'
     err = cgp_error_exit()
  ENDIF

  f_ptr = C_LOC(Coor_x(1))
  err = cgp_coord_write_data(fn,B,Z,Cx,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Coor_y(1))
  err = cgp_coord_write_data(fn,B,Z,Cy,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Coor_z(1))
  err = cgp_coord_write_data(fn,B,Z,Cz,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_write_data'
     err = cgp_error_exit()
  ENDIF

  ! We need to keep the arrays allocate until cgp_queue_flush is called
  IF(.NOT.queue)THEN
     DEALLOCATE(Coor_x)
     DEALLOCATE(Coor_y)
     DEALLOCATE(Coor_z)
  ENDIF

! ======================================
! == (B) WRITE THE CONNECTIVITY TABLE ==
! ======================================

  start = 1
  end = nijk(2)

  err = cgp_section_write(fn,B,Z,"Elements"//C_NULL_CHAR,HEXA_8,start,END,0,S)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_section_write'
     err = cgp_error_exit()
  ENDIF
 
  count = nijk(2)/comm_size
  ALLOCATE(elements(1:count*NodePerElem))
  
  ! Create ridiculous connectivity table ...
  DO k = 1, count*NodePerElem
     elements(k) = comm_rank*count*NodePerElem + k
  ENDDO

  emin = count*comm_rank+1
  emax = count*(comm_rank+1)

  f_ptr = C_LOC(elements(1))

  err = cgp_elements_write_data(fn, B, Z, S, emin, emax, f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_elements_write_data'
     err = cgp_error_exit()
  ENDIF


! ======================================
! == (C) WRITE THE FIELD DATA         ==
! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Data_Fx(1:count))
  ALLOCATE(Data_Fy(1:count))
  ALLOCATE(Data_Fz(1:count))

  DO k = 1, count
     Data_Fx(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.01_C_DOUBLE
     Data_Fy(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.02_C_DOUBLE
     Data_Fz(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.03_C_DOUBLE
  ENDDO

  err = cg_sol_write(fn, B, Z, "Solution"//C_NULL_CHAR, Vertex, S)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_sol_write'
     err = cgp_error_exit()
  ENDIF

  err = cgp_field_write(fn,B,Z,S,RealDouble,"MomentumX"//C_NULL_CHAR,Fx)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write'
     err = cgp_error_exit()
  ENDIF
  err = cgp_field_write(fn,B,Z,S,RealDouble,"MomentumY"//C_NULL_CHAR,Fy)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write'
     err = cgp_error_exit()
  ENDIF
  err = cgp_field_write(fn,B,Z,S,RealDouble,"MomentumZ"//C_NULL_CHAR,Fz)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write'
     err = cgp_error_exit()
  ENDIF
  
  f_ptr = C_LOC(Data_Fx(1))
  err = cgp_field_write_data(fn,B,Z,S,Fx,min,max,f_ptr)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Data_Fy(1))
  err = cgp_field_write_data(fn,B,Z,S,Fy,min,max,f_ptr) 
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Data_Fz(1))
  err = cgp_field_write_data(fn,B,Z,S,Fz,min,max,f_ptr) 
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_write_data'
     err = cgp_error_exit()
  ENDIF

  IF(.NOT.queue)THEN
     DEALLOCATE(Data_Fx)
     DEALLOCATE(Data_Fy)
     DEALLOCATE(Data_Fz)
  ENDIF

! ======================================
! == (D) WRITE THE ARRAY DATA         ==
! ======================================
 
  count = nijk(1)/comm_size
  count = nijk(1)/comm_size

  ALLOCATE(Array_x(1:count))
  ALLOCATE(Array_y(1:count))
  ALLOCATE(Array_z(1:count))

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  DO k = 1, count
     Array_x(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.001_C_DOUBLE
     Array_y(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.002_C_DOUBLE
     Array_z(k) = REAL(comm_rank*count, KIND=C_DOUBLE) + k + 0.003_C_DOUBLE
  ENDDO

  err = cg_goto(fn, B, "Zone 1"//C_NULL_CHAR, 0, end="end"//C_NULL_CHAR)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_goto'
     err = cgp_error_exit()
  ENDIF
  err = cg_user_data_write("User Data"//C_NULL_CHAR)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_user_data_write'
     err = cgp_error_exit()
  ENDIF
  err = cg_gorel(fn, "User Data"//C_NULL_CHAR, 0, 'end'//C_NULL_CHAR)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_gorel'
     err = cgp_error_exit()
  ENDIF
  size_1D(1) = nijk(1)
  err = cgp_array_write("ArrayX"//C_NULL_CHAR,RealDouble,1,size_1D,Ax)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write'
     err = cgp_error_exit()
  ENDIF
  err = cgp_array_write("ArrayY"//C_NULL_CHAR,RealDouble,1,size_1D,Ay)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write'
     err = cgp_error_exit()
  ENDIF
  err = cgp_array_write("ArrayZ"//C_NULL_CHAR,RealDouble,1,size_1D,Az)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write'
     err = cgp_error_exit()
  ENDIF
  
  f_ptr = C_LOC(Array_x(1))
  err = cgp_array_write_data(Ax,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Array_y(1))
  err = cgp_array_write_data(Ay,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Array_z(1))
  err = cgp_array_write_data(Az,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_array_write'
     err = cgp_error_exit()
  ENDIF

  IF(.NOT.queue)THEN
     DEALLOCATE(Array_x)
     DEALLOCATE(Array_y)
     DEALLOCATE(Array_z)
  ENDIF

  IF(queue)THEN
     err = cgp_queue_flush()
     IF(err.NE.CG_OK)THEN
        PRINT*,'*FAILED* cgp_queue_flush'
        err = cgp_error_exit()
     ENDIF
     err = cgp_queue_set(0)
     IF(err.NE.CG_OK)THEN
        PRINT*,'*FAILED* cgp_queue_set'
        err = cgp_error_exit()
     ENDIF
     DEALLOCATE(Coor_x)
     DEALLOCATE(Coor_y)
     DEALLOCATE(Coor_z)
     DEALLOCATE(Data_Fx)
     DEALLOCATE(Data_Fy)
     DEALLOCATE(Data_Fz)
     DEALLOCATE(Array_x)
     DEALLOCATE(Array_y)
     DEALLOCATE(Array_z)
     DEALLOCATE(elements)
  ELSE
     DEALLOCATE(elements)
  ENDIF

  err = cgp_close(fn)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_close'
     err = cgp_error_exit()
  ENDIF

! ======================================
! ==    **  READ THE CGNS FILE **     ==
! ======================================

  ! Open the cgns file

  err = cgp_open("test_unstructured_F90.cgns"//C_NULL_CHAR, CG_MODE_READ, fn)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_open'
     err = cgp_error_exit()
  ENDIF

  ! Read the base information
  err = cg_base_read(fn, B, bname, r_cell_dim, r_phys_dim)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_base_read'
     err = cgp_error_exit()
  ENDIF

  IF(r_cell_dim.NE.cell_dim.OR.r_phys_dim.NE.phys_dim)THEN
     WRITE(*,'(2(A,I0))') '*FAILED* bad cell dim = ',r_cell_dim,'or phy dim =',r_phys_dim
     err = cgp_error_exit()
  ENDIF

  indx_null = INDEX(bname,C_NULL_CHAR)-1
  IF(bname(1:indx_null).NE.'Base 1')THEN
     WRITE(*,'(A,A)') '*FAILED* bad base name = ',bname(1:indx_null)
     err = cgp_error_exit()
  ENDIF

  ! Read the zone information

  err = cg_zone_read(fn, B, Z, zname, sizes)
  IF(err.NE.CG_OK) PRINT*,'*FAILED* cgp_zone_write'

  ! Check the read zone information is correct 
  IF(sizes(1).NE.Nnodes)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num points = ',sizes(1)
     err = cgp_error_exit()
  ENDIF
     
  IF(sizes(2).NE.Nelem)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num elements = ',sizes(2)
     err = cgp_error_exit()
  ENDIF 
  IF(sizes(3).NE.0)THEN
     WRITE(*,'(A,I0)') '*FAILED* bad num elements = ',sizes(3)
     err = cgp_error_exit()
  ENDIF

  indx_null = INDEX(zname,C_NULL_CHAR)-1
  IF(zname(1:indx_null).NE.'Zone 1')THEN
     WRITE(*,'(A,A)') '*FAILED* bad zone name = ',zname(1:indx_null)
     err = cgp_error_exit()
  ENDIF

! ======================================
! ==  (A) READ THE NODAL COORDINATES  ==
! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Coor_x(1:count))
  ALLOCATE(Coor_y(1:count))
  ALLOCATE(Coor_z(1:count))

  min = count*comm_rank+1
  max = count*(comm_rank+1)

  ! use queued IO
  IF(queue) err = cgp_queue_set(1)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_queue_set'
     err = cgp_error_exit()
  ENDIF

  f_ptr = C_LOC(Coor_x(1))
  err = cgp_coord_read_data(fn,B,Z,Cx,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Coor_y(1))
  err = cgp_coord_read_data(fn,B,Z,Cy,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Coor_z(1))
  err = cgp_coord_read_data(fn,B,Z,Cz,min,max,f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_coord_read_data'
     err = cgp_error_exit()
  ENDIF

  ! See if read the data back correctly
!!$  DO k = 1, count
!!$     PRINT*,Coor_x(k),Coor_y(k),Coor_z(k)
!!$  ENDDO

  ! We need to keep the arrays allocate until cgp_queue_flush is called
  IF(.NOT.queue)THEN
     DEALLOCATE(Coor_x)
     DEALLOCATE(Coor_y)
     DEALLOCATE(Coor_z)
  ENDIF

! ======================================
! == (B) READ THE CONNECTIVITY TABLE  ==
! ======================================

  count = nijk(2)/comm_size
  ALLOCATE(elements(1:count*NodePerElem))
  
  emin = count*comm_rank+1
  emax = count*(comm_rank+1)

  f_ptr = C_LOC(elements(1))
  err = cgp_elements_read_data(fn, B, Z, S, emin, emax, f_ptr)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_elements_read_data'
     err = cgp_error_exit()
  ENDIF

!  PRINT*,elements
! ======================================
! == (C) READ THE FIELD DATA          ==
! ======================================
  count = nijk(1)/comm_size

  ALLOCATE(Data_Fx(1:count))
  ALLOCATE(Data_Fy(1:count))
  ALLOCATE(Data_Fz(1:count))

  f_ptr = C_LOC(Data_Fx(1))
  err = cgp_field_read_data(fn,B,Z,S,Fx,min,max,f_ptr)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Data_Fy(1))
  err = cgp_field_read_data(fn,B,Z,S,Fy,min,max,f_ptr) 
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Data_Fz(1))
  err = cgp_field_read_data(fn,B,Z,S,Fz,min,max,f_ptr) 
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data'
     err = cgp_error_exit()
  ENDIF

  ! See if read the data back correctly
!!$  DO k = 1, count
!!$     PRINT*,Data_Fx(k),Data_Fy(k),Data_Fz(k)
!!$  ENDDO

  IF(.NOT.queue)THEN
     DEALLOCATE(Data_Fx)
     DEALLOCATE(Data_Fy)
     DEALLOCATE(Data_Fz)
  ENDIF

! ======================================
! == (D) READ THE ARRAY DATA          ==
! ======================================

  count = nijk(1)/comm_size

  ALLOCATE(Array_x(1:count))
  ALLOCATE(Array_y(1:count))
  ALLOCATE(Array_z(1:count))

  min = count*comm_rank+1
  max = count*(comm_rank+1)
  
  err = cg_goto(fn,B,"Zone_t"//C_NULL_CHAR,Z,"UserDefinedData_t"//C_NULL_CHAR,1,END="end"//C_NULL_CHAR)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cg_goto (User Defined Data)'
     err = cgp_error_exit()
  ENDIF

  f_ptr = C_LOC(Array_x(1))
  err = cgp_array_read_data(Ax, min, max,f_ptr)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Array_y(1))
  err = cgp_array_read_data(Ay, min, max,f_ptr)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data'
     err = cgp_error_exit()
  ENDIF
  f_ptr = C_LOC(Array_z(1))
  err = cgp_array_read_data(Az, min, max,f_ptr)  
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_field_read_data'
     err = cgp_error_exit()
  ENDIF

  ! See if read the data back correctly
!!$  DO k = 1, count
!!$     PRINT*,Array_x(k),Array_y(k), Array_z(k)
!!$  ENDDO

  IF(.NOT.queue)THEN
     DEALLOCATE(Array_x)
     DEALLOCATE(Array_y)
     DEALLOCATE(Array_z)
  ENDIF

  IF(queue)THEN
     err = cgp_queue_flush()
     IF(err.NE.CG_OK)THEN
        PRINT*,'*FAILED* cgp_queue_flush'
        err = cgp_error_exit()
     ENDIF
     err = cgp_queue_set(0)
     IF(err.NE.CG_OK)THEN
        PRINT*,'*FAILED* cgp_queue_set'
        err = cgp_error_exit()
     ENDIF
     DEALLOCATE(Coor_x)
     DEALLOCATE(Coor_y)
     DEALLOCATE(Coor_z)
     DEALLOCATE(elements)
  ELSE
     DEALLOCATE(elements)
  ENDIF

! closeup shop and go home...
  err = cgp_close(fn)
  IF(err.NE.CG_OK)THEN
     PRINT*,'*FAILED* cgp_close'
     err = cgp_error_exit()
  ENDIF

  CALL MPI_FINALIZE(mpi_err)

END PROGRAM main

        
