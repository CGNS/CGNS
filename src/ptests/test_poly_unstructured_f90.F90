!
! @file test_poly_unstructured_f90.F90
! @version 0.1
!
! @section DESCRIPTION
! Test program for pcgns library
! -- created to test cgp_poly_section_write
! -- created to test cgp_poly_element_write_data
!
! @section USAGE
! Launch with:
!    mpirun -np <#> test_unstructured
!
! where number of ranks should be 2.
!
!

PROGRAM test_poly_unstructured_f

  USE mpi
  USE ISO_C_BINDING
  USE CGNS
  USE testing_utils
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif
  INTEGER, PARAMETER :: dp = KIND(0.0d0)

  INTEGER(C_INT) comm_size, comm_rank, mpi_err
  INTEGER Cx, Cy, Cz, F, B, Z, S
  INTEGER :: info
  INTEGER :: ierr
  INTEGER :: cell_dim
  INTEGER :: phys_dim
  INTEGER :: nb_zones
  INTEGER :: count
  INTEGER :: iProc
  INTEGER :: iZone
  INTEGER(cgsize_t) :: nbCellSide
  INTEGER(cgsize_t) :: nbNodeSide
  INTEGER(cgsize_t) :: nbNodeSlice
  INTEGER(cgsize_t) :: nbNodeTotal
  INTEGER(cgsize_t) :: nbCellSlice
  INTEGER(cgsize_t) :: nbCellTotal
  INTEGER(cgsize_t) :: offsetsTotalSize
  INTEGER(cgsize_t) :: startOffset
  INTEGER(cgsize_t) :: nbNodeIdeal
  INTEGER(cgsize_t) :: nbCellIdeal
  INTEGER(cgsize_t) :: cellOnProcStart
  INTEGER(cgsize_t) :: cellOnProcEnd
  INTEGER(cgsize_t) :: nodeOnProcStart
  INTEGER(cgsize_t) :: nodeOnProcEnd
  INTEGER(cgsize_t) :: nbCellWrite
  INTEGER(cgsize_t) :: nbNodeWrite
  INTEGER(cgsize_t) :: start, end
  INTEGER(C_INT64_T) :: start_l, end_l, nb_read
  INTEGER(cgsize_t) :: iNode, jNode, iCell, jCell, i, j, k
  INTEGER(cgsize_t) :: local_size(1)
  INTEGER(cgsize_t) :: sizes(3)
  INTEGER(cgsize_t), ALLOCATABLE :: offsets_sizes(:)
  INTEGER(cgsize_t), ALLOCATABLE :: cells(:), offsets(:)
  INTEGER(cgsize_t) :: size_read
  INTEGER(cgsize_t), ALLOCATABLE, TARGET :: cells_read(:), offsets_read(:)

  REAL(dp), ALLOCATABLE :: nodeX(:), nodeY(:), nodeZ(:)
  REAL(dp) :: spacing

  CHARACTER(len=10) :: cZone
  CHARACTER(len=1) :: ichr1
!
!---- initialize MPI
  CALL MPI_INIT(mpi_err)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, comm_size, mpi_err)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, comm_rank, mpi_err)
  CALL MPI_INFO_CREATE(info, mpi_err)

  IF (comm_size /= 2 .AND. comm_rank == 0) THEN
    PRINT*, "WARNING: test is supposed to be run with two processes"
  END IF

  ! default test values
  cell_dim = 3
  phys_dim = 3
  nb_zones = 5
  nbCellSide = 20

  ! basic mesh information
  nbNodeSide = nbCellSide + 1
  nbNodeSlice = nbNodeSide * nbNodeSide
  nbNodeTotal = nbNodeSide * nbNodeSlice
  nbCellSlice = nbCellSide * nbCellSide
  nbCellTotal = nbCellSide * nbCellSlice

  ! sizing of section
  offsetsTotalSize = 0
  startOffset = 0

  ! distribute the elements and nodes hosted by each rank
  nbNodeIdeal = (nbNodeTotal / comm_size) + 1
  nbCellIdeal = (nbCellTotal / comm_size) + 1
  cellOnProcStart = (comm_rank) * (nbCellIdeal)
  cellOnProcEnd = (comm_rank + 1) * (nbCellIdeal)
  nodeOnProcStart = (comm_rank) * (nbNodeIdeal)
  nodeOnProcEnd = (comm_rank + 1) * (nbNodeIdeal)
  cellOnProcEnd = MIN(cellOnProcEnd, nbCellTotal)
  nodeOnProcEnd = MIN(nodeOnProcEnd, nbNodeTotal)
  nbCellWrite = cellOnProcEnd - cellOnProcStart
  nbNodeWrite = nodeOnProcEnd - nodeOnProcStart

  ! create a simple cube mesh
  ALLOCATE(nodeX(nbNodeWrite))
  ALLOCATE(nodeY(nbNodeWrite))
  ALLOCATE(nodeZ(nbNodeWrite))
  spacing = 1.0 / (nbNodeSide - 1)
  count = 1
  DO iNode = nodeOnProcStart+1, nodeOnProcEnd
    jNode = iNode - 1
    i = FLOOR(jNode / REAL(nbNodeSlice, dp))
    j = FLOOR((jNode - i * nbNodeSlice) / REAL(nbNodeSide, dp))
    k = jNode - i * nbNodeSlice - j * nbNodeSide
    nodeX(count) = (i * spacing)
    nodeY(count) = (j * spacing)
    nodeZ(count) = (k * spacing)
    count = count + 1
  END DO

  ALLOCATE(cells(9*nbCellWrite))
  ALLOCATE(offsets(nbCellWrite+1))
  count = 0
  offsets(1) = 0
  DO iCell = cellOnProcStart+1, cellOnProcEnd
    jCell = iCell - 1
    i = FLOOR(jCell / REAL(nbCellSlice, dp))
    j = FLOOR((jCell - i * nbCellSlice) / REAL(nbCellSide))
    k = jCell - i * nbCellSlice - j * nbCellSide
    cells(count + 1) = INT(CGNS_ENUMV(HEXA_8), cgsize_t)
    cells(count + 2) = &
        (i + 0) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 0) + 1
    cells(count + 3) = &
        (i + 1) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 0) + 1
    cells(count + 4) = &
        (i + 1) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 0) + 1
    cells(count + 5) = &
        (i + 0) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 0) + 1
    cells(count + 6) = &
        (i + 0) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 1) + 1
    cells(count + 7) = &
        (i + 1) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 1) + 1
    cells(count + 8) = &
        (i + 1) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 1) + 1
    cells(count + 9) = &
        (i + 0) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 1) + 1
    count = count + 9
    offsets(iCell - cellOnProcStart + 1) = count
  END DO

  ! get writing offsets
  ALLOCATE(offsets_sizes(comm_size))
  local_size(1) = offsets(nbCellWrite+1)

#if CG_BUILD_64BIT_F
  CALL MPI_Allgather(local_size, 1, MPI_INT64_T, offsets_sizes, 1, MPI_INT64_T, &
                      MPI_COMM_WORLD, ierr)
#else
  CALL MPI_Allgather(local_size, 1, MPI_INT32_T, offsets_sizes, 1, MPI_INT32_T, &
                      MPI_COMM_WORLD, ierr)
#endif

  DO iProc = 1, comm_size
    IF (iProc-1 < comm_rank) THEN
      startOffset = startOffset + INT(offsets_sizes(iProc), cgsize_t)
    END IF
    offsetsTotalSize = offsetsTotalSize + INT(offsets_sizes(iProc), cgsize_t)
  END DO

  DO iCell = 1, nbCellWrite+1
    offsets(iCell) = offsets(iCell) + startOffset
  END DO

  IF(comm_rank.EQ.0) CALL write_test_header("Testing writing a data node selection")

  ! write file
  CALL cgp_mpi_comm_f(MPI_COMM_WORLD, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_open_f('test_poly_unstructured_f90.cgns', CG_MODE_WRITE, F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cg_base_write_f(F, 'Unstructured3D', cell_dim, phys_dim, B, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  DO iZone = 1,nb_zones
     WRITE(ichr1,'(I1.1)') iZone

    ! offset the nodes for each zone
    IF (iZone > 1) THEN
      DO iNode = 1, nbNodeWrite
        nodeX(iNode) = nodeX(iNode) + 1.0
      END DO
    END IF

    ! create the zone
    WRITE(cZone,"(I6)") iZone - 1
    sizes(1) = nbNodeTotal
    sizes(2) = nbCellTotal
    sizes(3) = 0
    CALL cg_zone_write_f(F, B, "domain"//TRIM(ADJUSTL(cZone)), sizes, CGNS_ENUMV(Unstructured), Z, ierr)
    IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

    ! write the nodes in parallel
    start = nodeOnProcStart + 1
    end = nodeOnProcEnd
    CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealDouble), 'CoordinateX', Cx, ierr)
    IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
    CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealDouble), 'CoordinateY', Cy, ierr)
    IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
    CALL cgp_coord_write_f(F, B, Z, CGNS_ENUMV(RealDouble), 'CoordinateZ', Cz, ierr)
    IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
    CALL cgp_coord_write_data_f(F, B, Z, Cx, start, end, nodeX, ierr)
    IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
    CALL cgp_coord_write_data_f(F, B, Z, Cy, start, end, nodeY, ierr)
    IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
    CALL cgp_coord_write_data_f(F, B, Z, Cz, start, end, nodeZ, ierr)
    IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

    ! create element node
    start = 1
    end = nbCellTotal
    CALL cgp_poly_section_write_f(F, B, Z, "Elements 3D", CGNS_ENUMV(MIXED), start, end, offsetsTotalSize, 0, S, ierr)
    IF(ierr.NE.CG_OK)THEN
       IF (comm_rank .EQ. 0) CALL write_test_status(failed, "Test cgp_poly_section_write_f, Zone "//ichr1)
       CALL cgp_error_exit_f()
    ELSE
       IF (comm_rank .EQ. 0) CALL write_test_status(passed, "Test cgp_poly_section_write_f, Zone "//ichr1)
    ENDIF

    start = cellOnProcStart + 1
    end = cellOnProcEnd
    CALL cgp_poly_elements_write_data_f(F, B, Z, S, start, end, cells, offsets, ierr)
    IF(ierr.NE.CG_OK)THEN
       WRITE(ichr1,'(I1.1)') iZone
       IF (comm_rank .EQ. 0) CALL write_test_status(failed, "Test cgp_poly_elements_write_data_f, Zone "//ichr1)
       CALL cgp_error_exit_f()
    ELSE
       IF (comm_rank .EQ. 0) CALL write_test_status(passed, "Test cgp_poly_elements_write_data_f, Zone "//ichr1)
    ENDIF

  END DO

  CALL cgp_close_f(F, ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Read and compare data with expected values
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  IF(comm_rank.EQ.0) CALL write_test_header("Testing reading a data node selection")

  DO iZone = 1, nb_zones

     WRITE(ichr1,'(I1.1)') iZone

     ! Zero out node coordinates
     nodeX = 0.0_dp
     nodeY = 0.0_dp
     nodeZ = 0.0_dp

     ! Open file in parallel
     CALL cgp_mpi_comm_f(MPI_COMM_WORLD, ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL cgp_open_f('test_poly_unstructured_f90.cgns', CG_MODE_READ, F, ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     ! Read nodes and compare
     start = nodeOnProcStart + 1
     end   = nodeOnProcEnd
  
     CALL cgp_coord_read_data_f(F, B, iZone, Cx, start, end, nodeX, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f
     CALL cgp_coord_read_data_f(F, B, iZone, Cy, start, end, nodeY, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f
     CALL cgp_coord_read_data_f(F, B, iZone, Cz, start, end, nodeZ, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f

     count = 1
     DO iNode = nodeOnProcStart+1, nodeOnProcEnd
        jNode = iNode - 1
        i = FLOOR(jNode / REAL(nbNodeSlice, dp))
        j = FLOOR((jNode - i * nbNodeSlice) / REAL(nbNodeSide, dp))
        k = jNode - i * nbNodeSlice - j * nbNodeSide

        ! Compare values
        IF (.NOT. check_eq(nodeX(count), i * spacing + (iZone-1) * 1.0_dp)) THEN
           IF (comm_rank .EQ. 0) CALL write_test_status(failed, "Test cgp_coord_read_data_f", "Mismatch in nodeX")
           CALL cgp_error_exit_f()
        ENDIF
        IF (.NOT. check_eq(nodeY(count) , j * spacing)) THEN
           IF (comm_rank .EQ. 0) CALL write_test_status(failed, "Test cgp_coord_read_data_f", "Mismatch in nodeY")
           CALL cgp_error_exit_f()
        ENDIF
        IF (.NOT. check_eq(nodeZ(count), k * spacing)) THEN
           IF (comm_rank .EQ. 0) CALL write_test_status(failed, "Test cgp_coord_read_data_f", "Mismatch in nodeZ")
           CALL cgp_error_exit_f()
        ENDIF
        count = count + 1
     END DO

     CALL cgp_close_f(F, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f
  END DO

  IF (comm_rank .EQ. 0) CALL write_test_status(passed, "Test cgp_coord_read_data_f")

  ! Read elements in serial mode

  IF(comm_rank.EQ.0) CALL write_test_header("Testing reading mixed element data in serial")

  DO iZone = 1, nb_zones

     WRITE(ichr1,'(I1.1)') iZone

     ! Compare on rank 0 only, read for all procs
     IF (comm_rank == 0) THEN
        CALL cgp_mpi_comm_f(MPI_COMM_SELF, ierr)
        IF (ierr /= CG_OK) CALL cgp_error_exit_f
        CALL cgp_open_f('test_poly_unstructured_f90.cgns', CG_MODE_READ, F, ierr)
        IF (ierr /= CG_OK) CALL cgp_error_exit_f

        start_l = INT(cellOnProcStart + 1, KIND=C_INT64_T)
        end_l   = INT(cellOnProcEnd, KIND=C_INT64_T)
        nb_read = INT(cellOnProcEnd - cellOnProcStart, KIND=C_INT64_T)

        DO iProc = 1, comm_size
           IF (iProc-1 /= comm_rank) THEN
              CALL MPI_Recv(nb_read, 1, MPI_INT64_T, iProc-1, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_err)
              CALL MPI_Recv(start_l, 1, MPI_INT64_T, iProc-1, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_err)
              CALL MPI_Recv(end_l  , 1, MPI_INT64_T, iProc-1, 3, MPI_COMM_WORLD, MPI_STATUS_IGNORE, mpi_err)
           END IF

           CALL cg_ElementPartialSize_f(F, B, iZone, S, INT(start_l, cgsize_t), INT(end_l, cgsize_t), size_read, ierr)
           IF (ierr /= CG_OK) CALL cgp_error_exit_f

           ALLOCATE(cells_read(size_read))
           ALLOCATE(offsets_read(nb_read + 1))

           CALL cg_poly_elements_partial_read_f(F, B, iZone, S, start_l, end_l, &
                cells_read, offsets_read, C_NULL_PTR, ierr)
           IF (ierr /= CG_OK) CALL cgp_error_exit_f

           ! Compare element connectivity
           count = 1
           DO iCell = start_l, end_l
              jCell = iCell - 1
              i = FLOOR(jCell / REAL(nbCellSlice, dp))
              j = FLOOR((jCell - i * nbCellSlice) / REAL(nbCellSide, dp))
              k = jCell - i * nbCellSlice - j * nbCellSide

              IF( .NOT.check_eq(cells_Read(count + 0), INT(CGNS_ENUMV(HEXA_8), CGSIZE_T)) .OR. &
                  .NOT.check_eq(cells_Read(count + 1), (i + 0) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 0) + 1) .OR. &
                  .NOT.check_eq(cells_Read(count + 2), (i + 1) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 0) + 1) .OR. &
                  .NOT.check_eq(cells_Read(count + 3), (i + 1) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 0) + 1) .OR. &
                  .NOT.check_eq(cells_Read(count + 4), (i + 0) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 0) + 1) .OR. &
                  .NOT.check_eq(cells_Read(count + 5), (i + 0) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 1) + 1) .OR. &
                  .NOT.check_eq(cells_Read(count + 6), (i + 1) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 1) + 1) .OR. &
                  .NOT.check_eq(cells_Read(count + 7), (i + 1) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 1) + 1) .OR. &
                  .NOT.check_eq(cells_Read(count + 8), (i + 0) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 1) + 1)) THEN

                 IF (comm_rank .EQ. 0) CALL write_test_status(failed, "read cg_poly_elements_partial_read_f values, Zone "//ichr1)
                 CALL cgp_error_exit_f()
              ENDIF
              count = count + 9
           END DO

           DEALLOCATE(cells_read)
           DEALLOCATE(offsets_read)
        END DO

        CALL cgp_close_f(F, ierr)
        IF (ierr /= CG_OK) CALL cgp_error_exit_f

     ELSE
        start_l = INT(cellOnProcStart + 1, KIND=C_INT64_T)
        end_l = INT(cellOnProcEnd, KIND=C_INT64_T)
        nb_read = INT(cellOnProcEnd - cellOnProcStart, KIND=C_INT64_T)
    
        CALL MPI_Send(nb_read, 1, MPI_INT64_T, 0, 1, MPI_COMM_WORLD, ierr)
        CALL MPI_Send(start_l, 1, MPI_INT64_T, 0, 2, MPI_COMM_WORLD, ierr)
        CALL MPI_Send(end_l  , 1, MPI_INT64_T, 0, 3, MPI_COMM_WORLD, ierr)
     END IF

     IF (comm_rank .EQ. 0) CALL write_test_status(passed, "read cg_poly_elements_partial_read_f values, Zone "//ichr1)

  END DO


  ! Read elements in parallel mode

  IF(comm_rank.EQ.0) CALL write_test_header("Testing reading mixed element data in parallel")

  DO iZone = 1, nb_zones

     WRITE(ichr1,'(I1.1)') iZone

     ! Open file in parallel
     CALL cgp_mpi_comm_f(MPI_COMM_WORLD, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f
     CALL cgp_open_f('test_poly_unstructured_f90.cgns', CG_MODE_READ, F, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f

     start_l = cellOnProcStart + 1
     end_l = cellOnProcEnd
     nb_read = cellOnProcEnd - cellOnProcStart

     ALLOCATE(offsets_read(nb_read + 1))
  
     CALL cgp_poly_elements_read_data_offsets_f(F, B, iZone, S, start_l, end_l, offsets_read, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f

     size_read = offsets_read(nb_read + 1)
     ALLOCATE(cells_read(size_read))

     CALL cgp_poly_elements_read_data_elements_f(F, B, iZone, S, start_l, end_l, offsets_read, cells_read, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f

     count = 1
     DO iCell = start_l, end_l
        jCell = iCell - 1
        i = FLOOR(jCell / REAL(nbCellSlice, dp))
        j = FLOOR((jCell - i * nbCellSlice) / REAL(nbCellSide, dp))
        k = jCell - i * nbCellSlice - j * nbCellSide

        IF( .NOT.check_eq(cells_Read(count + 0), INT(CGNS_ENUMV(HEXA_8), CGSIZE_T)) .OR. &
             .NOT.check_eq(cells_Read(count + 1), (i + 0) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 0) + 1) .OR. &
             .NOT.check_eq(cells_Read(count + 2), (i + 1) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 0) + 1) .OR. &
             .NOT.check_eq(cells_Read(count + 3), (i + 1) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 0) + 1) .OR. &
             .NOT.check_eq(cells_Read(count + 4), (i + 0) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 0) + 1) .OR. &
             .NOT.check_eq(cells_Read(count + 5), (i + 0) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 1) + 1) .OR. &
             .NOT.check_eq(cells_Read(count + 6), (i + 1) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 1) + 1) .OR. &
             .NOT.check_eq(cells_Read(count + 7), (i + 1) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 1) + 1) .OR. &
             .NOT.check_eq(cells_Read(count + 8), (i + 0) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 1) + 1)) THEN

           CALL write_test_status(failed, "read cg_poly_elements_partial_read_f values, Zone "//ichr1)
           CALL cgp_error_exit_f()

        ENDIF
        count = count + 9
     END DO

     IF(comm_rank.EQ.0) CALL write_test_status(passed, "read cg_poly_elements_partial_read_f values, Zone "//ichr1)

     DEALLOCATE(cells_read)
     DEALLOCATE(offsets_read)
  
     CALL cgp_close_f(F, ierr)
     IF (ierr /= CG_OK) CALL cgp_error_exit_f
  END DO

  ! free memory
  DEALLOCATE(nodeX)
  DEALLOCATE(nodeY)
  DEALLOCATE(nodeZ)
  DEALLOCATE(cells)
  DEALLOCATE(offsets)
  DEALLOCATE(offsets_sizes)

  IF(comm_rank.EQ.0) CALL write_test_footer()

  CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
  CALL MPI_FINALIZE(mpi_err)
END PROGRAM test_poly_unstructured_f
