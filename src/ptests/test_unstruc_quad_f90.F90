!
! @file test_unstruc_quad_f90.F90
! @author Greg Sjaardema <gsjaardema@gmail.com>
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Test program for pcgns library
! -- Created to test cgp_parent_data_write function
! -- Based on test_unstructured_quad.c
!

!
!
! 2....4....6....8 8...10...12...14 14...16...18...20 L+1.L+3...L+5...L+7
! |    |    |    | |    |    |    | |     |    |    | |     |     |     |
! 1....3....5....7 7....9...11...13 13...15...17...19 L...L+2...L+4...L+6
!   1    2   3      4    5    6       7    8    9      M     M+1   M+2
!       P0               P1                P2             PN
!
!L = P*6+1   M = P*3+1
!The BC "Bottom" is on the bottom (y=0) edge of the mesh.
!The BC "Left"   is on the left (x=0) edge of the mesh.
! -- included to test whether works with 0 entries on some procs.
!


PROGRAM test_unstruc_quad_f

  USE mpi
  USE ISO_C_BINDING
  USE CGNS
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif
  integer, parameter :: dp = kind(0.0d0)

  INTEGER vert_proc
  PARAMETER(vert_proc = 8)
  INTEGER depth
  PARAMETER(depth = 4)

  INTEGER(C_INT) comm_size, comm_rank, mpi_err
  INTEGER info
  INTEGER ierr
  INTEGER(cgsize_t) :: sizes(3)
  INTEGER(cgsize_t) :: n_boco_elems
  INTEGER(cgsize_t) :: start_local(1), end_local(1)
  INTEGER(cgsize_t) :: start, end
  INTEGER(cgsize_t) :: emin(1), emax(1)
  INTEGER(cgsize_t), TARGET :: rmin(1), rmax(1)
  INTEGER(cgsize_t) :: nelem, nvert
  INTEGER(cgsize_t), PARAMETER :: start_1 = 1
  INTEGER Cx, Cy, Cz, F, B, Z, S, BC, Sol, Fld
  INTEGER S_elements, S_bottom, S_right, S_left
  INTEGER i, k
  INTEGER global_num_quads
  INTEGER(C_INT), DIMENSION(4) :: indices

  REAL(dp), allocatable, target ::  fx(:), fy(:), fz(:)
  INTEGER(cgsize_t), allocatable, target :: elements(:)
  INTEGER(cgsize_t), allocatable :: point_list(:)
  INTEGER(cgsize_t), dimension(:), pointer :: null_ptr => null()
  INTEGER(cgsize_t), dimension(:), pointer :: el_ptr => null()
  TYPE(C_PTR) :: c_elements
  LOGICAL found_point


  CHARACTER(len=10, kind=C_CHAR), allocatable, target :: labels(:)
  TYPE(C_PTR), dimension(:), allocatable :: pt_labels
!
!---- initialize MPI
  CALL MPI_INIT(mpi_err)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, comm_size, mpi_err)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, comm_rank, mpi_err)
  CALL MPI_INFO_CREATE(info, mpi_err)

! First define the label array on fortran side 
  allocate(labels(4))
  labels(1) = "Zone_t"//C_NULL_CHAR
  labels(2) = "ZoneBC_t"//C_NULL_CHAR
  labels(3) = "BC_t"//C_NULL_CHAR
  labels(4) = "PointList"//C_NULL_CHAR
! do mapping for C side
  allocate(pt_labels(4))
  pt_labels(1) = C_LOC(labels(1))
  pt_labels(2) = C_LOC(labels(2))
  pt_labels(3) = C_LOC(labels(3))
  pt_labels(4) = C_LOC(labels(4))

!---- open file and create base and zone
  nelem = 3*comm_size
  nvert = 2*nelem + 2
  sizes(1) = nvert
  sizes(2) = nelem
  sizes(3) = 0

  CALL cgp_open_f('test_uns_quad_f90.cgns', CG_MODE_WRITE, F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cg_base_write_f(F, 'Base#1', 3, 3, B, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cg_zone_write_f(F, B, 'Zone_1', sizes, CGNS_ENUMV(Unstructured), Z, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- create the coordinate data 
  allocate(fx(vert_proc))
  allocate(fy(vert_proc))
  allocate(fz(vert_proc))
  
  rmin(1) = 6*comm_rank+1
  rmax(1) = rmin(1)+7

  DO k=1,vert_proc,2
    fx(k+0) = real(rmin(1)+k, dp)
    fx(k+1) = real(rmin(1)+k, dp)
    fy(k+0) = 0.0
    fy(k+1) = 1.0
    fz(k+0) = 0.0
    fz(k+1) = 0.0
  ENDDO

!---- create data nodes for coordinates
  CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealSingle), 'CoordinateX', Cx, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealSingle), 'CoordinateY', Cy, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealSingle), 'CoordinateZ', Cz, ierr)


!---- write the coordinate data in parallel
  CALL cgp_coord_write_data_f(F, B, Z, Cx, C_LOC(rmin), C_LOC(rmax), C_LOC(fx(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_coord_write_data_f(F, B, Z, Cy, C_LOC(rmin), C_LOC(rmax), C_LOC(fy(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_coord_write_data_f(F, B, Z, Cz, C_LOC(rmin), C_LOC(rmax), C_LOC(fz(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! Test no-data path: rank 0 passes C_NULL_PTR, verifying rmin/rmax are not
! examined when data is NULL (all ranks still participate in the collective).
  IF (comm_rank .EQ. 0) THEN
    CALL cgp_coord_write_data_f(F, B, Z, Cx, C_NULL_PTR, C_NULL_PTR, C_NULL_PTR, ierr)
  ELSE
    CALL cgp_coord_write_data_f(F, B, Z, Cx, C_LOC(rmin), C_LOC(rmax), C_LOC(fx(1)), ierr)
  END IF
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- create a solution node and write field data in parallel
  CALL cg_sol_write_f(F, B, Z, 'Solution', CGNS_ENUMV(Vertex), Sol, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_field_write_f(F, B, Z, Sol, CGNS_ENUMV(RealDouble), 'FieldX', Fld, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_field_write_data_f(F, B, Z, Sol, Fld, C_LOC(rmin), C_LOC(rmax), C_LOC(fx(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! Test no-data path: rank 0 passes C_NULL_PTR for data.
  IF (comm_rank .EQ. 0) THEN
    CALL cgp_field_write_data_f(F, B, Z, Sol, Fld, C_NULL_PTR, C_NULL_PTR, C_NULL_PTR, ierr)
  ELSE
    CALL cgp_field_write_data_f(F, B, Z, Sol, Fld, C_LOC(rmin), C_LOC(rmax), C_LOC(fx(1)), ierr)
  END IF
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  end = comm_size*3
!---- create data node for elements
  CALL cgp_section_write_f(F, B, Z, 'Elements', CGNS_ENUMV(QUAD_4), start_1, end, 0, S, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  S_elements = S

  nelem = 3;
  emin(1) = comm_rank*3+1;
  emax(1) = emin(1)+2;

  PRINT *, 'rank ', comm_rank, ': ', nelem, ' elements'

  allocate(elements(nelem*4))
  DO k=1,nelem
    elements(4*(k-1)+1) = 2*(emin(1)+k-1)-1
    elements(4*(k-1)+2) = 2*(emin(1)+k-1)+1
    elements(4*(k-1)+3) = 2*(emin(1)+k-1)+2
    elements(4*(k-1)+4) = 2*(emin(1)+k-1)+0
  ENDDO

  PRINT *, comm_rank, ":", nelem, ":", emin(1), ":", emax(1)

!---- write the element connectivity in parallel
  CALL cgp_elements_write_data_f(F, B, Z, S, emin(1), emax(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! Test no-data path: rank 0 passes C_NULL_PTR; start/end are ignored by
! the C layer when elements is NULL.
  IF (comm_rank .EQ. 0) THEN
    CALL cgp_elements_write_data_f(F, B, Z, S_elements, emin(1), emax(1), C_NULL_PTR, ierr)
  ELSE
    CALL cgp_elements_write_data_f(F, B, Z, S_elements, emin(1), emax(1), C_LOC(elements(1)), ierr)
  END IF
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- create data node for elements
  start = 3*comm_size + 1
  end = start + 3*comm_size - 1
  CALL cgp_section_write_f(F, B, Z, 'Bottom', CGNS_ENUMV(BAR_2), start, end, 0, S, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  S_bottom = S

! Parent elements/side data
  DO k=1,3
    elements(3*0+k) = comm_rank*3+k  ! Element
    elements(3*1+k) = 0
    elements(3*2+k) = 1 ! Side
    elements(3*3+k) = 0
  ENDDO

  emin(1) = comm_rank*3+start
  emax(1) = emin(1)+2
  PRINT *, comm_rank, ':', emin(1), ' ', emax(1) 
  
  CALL cgp_parent_data_write_f(F, B, Z, S, emin(1), emax(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! side connectivity
  DO k=1,3
    elements(2*(k-1)+1) = 2*(comm_rank*3+(k-1))+1
    elements(2*(k-1)+2) = 2*(comm_rank*3+(k-1))+3
  ENDDO
  CALL cgp_elements_write_data_f(F, B, Z, S, emin(1), emax(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  n_boco_elems = end-start+1

  CALL cg_boco_write_f(F, B, Z, "BottomBC", CGNS_ENUMV(FamilySpecified), CGNS_ENUMV(PointList), n_boco_elems, null_ptr, BC, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  i = 1
  DO k=emin(1),emax(1)
    elements(i) = k
    i = i + 1
  ENDDO

  start_local(1) = comm_rank * 3 + 1
  end_local(1) = start_local(1) + 2
  PRINT *, comm_rank, ':', start_local(1), ' ', end_local(1) 

  indices(1) = Z
  indices(2) = 1
  indices(3) = BC
  indices(4) = 0

  ierr = cg_golist(F, B, depth, pt_labels, indices)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  PRINT *, elements(1), elements(2), start_local(1), end_local(1)
  CALL cgp_ptlist_write_data_f(F, start_local(1), end_local(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cg_boco_gridlocation_write_f(F, B, Z, BC, CGNS_ENUMV(EdgeCenter), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! Add family tag
  ierr =  cg_golist(F, B, 3, pt_labels, indices)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cg_famname_write_f("Bottom", ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! right BC — all ranks contribute parent data
  start = end + 1
  end = start + 3*comm_size - 1
  CALL cgp_section_write_f(F, B, Z, 'Right', CGNS_ENUMV(BAR_2), start, end, 0, S, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  S_right = S

! Parent elements/side data
  DO k=1,3
    elements(3*0+k) = comm_rank*3+k  ! Element
    elements(3*1+k) = 0
    elements(3*2+k) = 2 ! Side
    elements(3*3+k) = 0
  ENDDO

  emin(1) = comm_rank*3+start
  emax(1) = emin(1)+2

  CALL cgp_parent_data_write_f(F, B, Z, S, emin(1), emax(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! side connectivity
  DO k=1,3
    elements(2*(k-1)+1) = 2*(comm_rank*3+(k-1))+1
    elements(2*(k-1)+2) = 2*(comm_rank*3+(k-1))+3
  ENDDO
  CALL cgp_elements_write_data_f(F, B, Z, S, emin(1), emax(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! left BC
  start = end + 1
  end = start

  CALL cgp_section_write_f(F, B, Z, 'Left', CGNS_ENUMV(BAR_2), start, end, 0, S, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  S_left = S

  IF (comm_rank .EQ. 0) THEN
    emin(1) = start
    emax(1) = end
! Parent Element/Side data
    elements(1) = 1 ! Element
    elements(2) = 0
    elements(3) = 4 ! Side
    elements(4) = 0
    c_elements = C_LOC(elements(1))
  ELSE
    emin(1) = 0
    emax(1) = 0
    c_elements = C_NULL_PTR
  ENDIF
  PRINT *, comm_rank, ":", emin(1), " ", emax(1)

  CALL cgp_parent_data_write_f(F, B, Z, S, emin(1), emax(1), c_elements, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  IF (comm_rank .EQ. 0) THEN
    elements(1) = 1
    elements(2) = 2
    CALL cgp_elements_write_data_f(F, B, Z, S, emin(1), emax(1), C_LOC(elements(1)), ierr)
  ELSE
    CALL cgp_elements_write_data_f(F, B, Z, S, emin(1), emax(1), C_NULL_PTR, ierr)
  END IF
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  n_boco_elems = 1
  CALL cg_boco_write_f(F, B, Z, "LeftBC", CGNS_ENUMV(FamilySpecified), CGNS_ENUMV(PointList), n_boco_elems, null_ptr, BC, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  IF (comm_rank .EQ. 0) THEN
    start_local(1) = 1
    end_local(1) = 1
    elements(1) = start
    c_elements = C_LOC(elements(1))
  ELSE
    start_local(1) = 0
    end_local(1) = 0
    c_elements = C_NULL_PTR
  ENDIF

  PRINT *, comm_rank, ":", start_local(1), " ", end_local(1)
  indices(3) = BC
  ierr = cg_golist(F, B, depth, pt_labels, indices)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cgp_ptlist_write_data_f(F, start_local(1), end_local(1), c_elements, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cg_boco_gridlocation_write_f(F, B, Z, BC, CGNS_ENUMV(EdgeCenter), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! Add family tag
  ierr = cg_golist(F, B, 3, pt_labels, indices)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cg_famname_write_f("Left", ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cgp_close_f(F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!
! ---  Test file reading
!

  CALL cgp_open_f('test_uns_quad_f90.cgns', CG_MODE_READ, F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  ! Left BC
  start = 9*comm_size + 1
  end   = start

  IF (comm_rank .EQ. 0) THEN
     emin(1) = start
     emax(1) = END
     c_elements = C_LOC(elements(1))
  ELSE
     emin(1) = 0
     emax(1) = 0
     c_elements = C_NULL_PTR
  END IF

  CALL cgp_parentelements_read_data_f(F, 1, 1, S_left, emin(1), emax(1), c_elements, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  IF (comm_rank .EQ. 0) THEN
     IF (elements(1) .NE. 1 .OR. elements(2) .NE. 0)THEN
        WRITE(*,'(A)') "Could not read parent_element"
        CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_err)
     ENDIF
  ENDIF

! All ranks have parent data for Bottom section
  emin(1) = 3*comm_size + comm_rank*3 + 1
  emax(1) = emin(1) + 2
  CALL cgp_parentelements_read_data_f(F, 1, 1, S_bottom, emin(1), emax(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! All ranks read parent data for Right section
  emin(1) = 6*comm_size + comm_rank*3 + 1
  emax(1) = emin(1) + 2
  CALL cgp_parentelements_read_data_f(F, 1, 1, S_right, emin(1), emax(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
!  if (cg_goto(fn,1,"Zone_t",1,"Elements_t",3,"end"))
!    cgp_error_exit();

! reset array
  DO i=1, nelem*4
    elements(i) = 0
  ENDDO

  start_local(1) = comm_rank * 3 + 1
  end_local(1)  = start_local(1) + 2

  indices(1) = Z
  indices(2) = 1
  indices(3) = 1
  indices(4) = 0

  ierr = cg_golist(F, B, depth, pt_labels, indices)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cgp_ptlist_read_data_f(F, start_local(1), end_local(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  PRINT *, comm_rank, ": ", elements(1), " ", elements(2), " ", elements(3)
! Test read values
  global_num_quads = nelem * comm_size
  allocate(point_list(global_num_quads))
  do i=1,global_num_quads
    point_list(i) =  global_num_quads + i
  enddo

  do i=1,3
    found_point = .FALSE.
    do k=1,global_num_quads
      if (elements(i) .EQ. point_list(k)) then
        found_point = .TRUE.
        EXIT
      endif
    enddo
    if (found_point .NEQV. .TRUE.) then
        PRINT *, "Error:: Could not find point ", elements(i), &
                " in boundary list [", point_list(1), ",..., ", &
                point_list(global_num_quads), "]"
        CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_err)
    endif
  enddo
  

!---- read element connectivity in parallel
  emin(1) = comm_rank * nelem + 1
  emax(1) = emin(1) + nelem - 1
  CALL cgp_elements_read_data_f(F, B, Z, S_elements, emin(1), emax(1), C_LOC(elements(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- read coordinate data in parallel
  CALL cgp_coord_read_data_f(F, B, Z, Cx, C_LOC(rmin), C_LOC(rmax), C_LOC(fx(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- read field data in parallel (Sol=1, Fld=1 as written above)
  CALL cgp_field_read_data_f(F, B, Z, 1, 1, C_LOC(rmin), C_LOC(rmax), C_LOC(fx(1)), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- close the file and terminate MPI
  CALL cgp_close_f(F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL MPI_FINALIZE(mpi_err)
END PROGRAM test_unstruc_quad_f


