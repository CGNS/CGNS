
PROGRAM fexample

  USE MPI
  USE ISO_C_BINDING
  USE CGNS
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif

  INTEGER(cgsize_t) :: nperside, totnodes, totelems
  PARAMETER (nperside = 50)
  PARAMETER (totnodes=nperside*nperside*nperside)
  PARAMETER (totelems=(nperside-1)*(nperside-1)*(nperside-1))

  INTEGER(C_INT) commsize, commrank, mpi_err
  INTEGER ierr
  INTEGER F, B, Z, E, S, Fs, Cx, Cy, Cz, A
  INTEGER(cgsize_t) :: i, j, k, n, nn, ne
  INTEGER(cgsize_t) :: nnodes, nelems
  INTEGER(cgsize_t) :: sizes(3), start, END
  INTEGER(cgsize_t), PARAMETER :: start_1 = 1
  REAL*4 fx(totnodes), fy(totnodes), fz(totnodes), fd(totelems)
  INTEGER(cgsize_t) :: ie(8*totelems)
!
!---- initialize MPI
  CALL MPI_INIT(mpi_err)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, commsize, mpi_err)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, commrank, mpi_err)

!---- open file and create base and zone
  sizes(1) = totnodes
  sizes(2) = totelems
  sizes(3) = 0

  CALL cgp_open_f('fexample.cgns', CG_MODE_WRITE, F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cg_base_write_f(F, 'Base', 3, 3, B, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cg_zone_write_f(F, B, 'Zone', sizes, CGNS_ENUMV(Unstructured), Z, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- print info
  IF (commrank .EQ. 0) THEN
     PRINT *, 'writing',totnodes,' coordinates and', totelems, &
          ' hex elements to fexample.cgns'
  ENDIF

!---- create data nodes for coordinates
  CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealSingle), 'CoordinateX', Cx, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealSingle), 'CoordinateY', Cy, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealSingle), 'CoordinateZ', Cz, ierr)

!---- number of nodes and range this process will write
  nnodes = (totnodes + commsize - 1) / commsize
  start  = nnodes * commrank + 1
  end= nnodes * (commrank + 1)
  IF (end .GT. totnodes) END = totnodes

!---- create the coordinate data for this process
  nn = 1
  n  = 1
  DO k=1,nperside
     DO j=1,nperside
        DO i=1,nperside
           IF (n .GE. start .AND. n .LE. END) THEN
              fx(nn) = i
              fy(nn) = j
              fz(nn) = k
              nn = nn + 1
           ENDIF
           n = n + 1
        ENDDO
     ENDDO
  ENDDO

!---- write the coordinate data in parallel
  CALL cgp_coord_write_data_f(F, B, Z, Cx, start, END, fx, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_coord_write_data_f(F, B, Z, Cy, start, END, fy, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_coord_write_data_f(F, B, Z, Cz, start, END, fz, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- create data node for elements
  CALL cgp_section_write_f(F, B, Z, 'Hex', CGNS_ENUMV(HEXA_8), start_1, totelems, 0, E, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- number of elements and range this process will write
  nelems = (totelems + commsize - 1) / commsize
  start  = nelems * commrank + 1
  end = nelems * (commrank + 1)
  IF (end .GT. totelems) END = totelems

!---- create the hex element data for this process
  nn = 0
  n  = 1
  DO k=1,nperside-1
     DO j=1,nperside-1
        DO i=1,nperside-1
           IF (n .GE. start .AND. n .LE. END) THEN
              ne = i + nperside*((j-1)+nperside*(k-1))
              ie(nn+1) = ne
              ie(nn+2) = ne + 1
              ie(nn+3) = ne + 1 + nperside
              ie(nn+4) = ne + nperside
              ne = ne + nperside*nperside
              ie(nn+5) = ne
              ie(nn+6) = ne + 1
              ie(nn+7) = ne + 1 + nperside
              ie(nn+8) = ne + nperside
              nn = nn + 8
           ENDIF
           n = n + 1
        ENDDO
     ENDDO
  ENDDO

!---- write the element connectivity in parallel
  CALL cgp_elements_write_data_f(F, B, Z, E, start, END, ie, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- create a centered solution
  CALL cg_sol_write_f(F, B, Z, 'Solution', CGNS_ENUMV(CellCenter), S, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_field_write_f(F, B, Z, S, CGNS_ENUMV(RealSingle), 'CellIndex', Fs, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- create the field data for this process
  nn = 1
  DO n=1, totelems
     IF (n .GE. start .AND. n .LE. END) THEN
        fd(nn) = n
        nn = nn + 1
     ENDIF
  ENDDO

!---- write the solution field data in parallel
  CALL cgp_field_write_data_f(F, B, Z, S, Fs, start, END, fd, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- create user data under the zone and duplicate solution data
  CALL cg_goto_f(F, B, ierr, 'Zone_t', 1, 'end')
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cg_user_data_write_f('User Data', ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cg_gorel_f(F, ierr, 'User Data', 0, 'end')
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_array_write_f('CellIndex', CGNS_ENUMV(RealSingle), 1, totelems, A, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

!---- write the array data in parallel
  CALL cgp_array_write_data_f(A, start, END, fd, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
!---- close the file and terminate MPI
  CALL cgp_close_f(F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL MPI_FINALIZE(mpi_err)
END PROGRAM fexample

