      program testbboxf

#include "cgnstypes_f03.h"
#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      implicit none
      INTEGER, PARAMETER :: sp = KIND(1.0)
      INTEGER, PARAMETER :: dp = KIND(1.d0)
      
      integer, parameter :: celldim = 3, physdim = 3
      integer, parameter :: NUM_SIDE = 5
 
      real(kind=sp), dimension(NUM_SIDE*NUM_SIDE*NUM_SIDE) :: coord
      real(kind=dp), dimension(3, 2) :: bbox

      integer :: n
      integer :: ierr
      integer :: cgfile, cgbase, cgzone, cgcoord
      integer(cgsize_t) :: size(9)
      
      character(len=32) fname
      fname = 'boundingbox_f90.cgns'

      do n=1,NUM_SIDE*NUM_SIDE*NUM_SIDE
        coord(n) = n
      enddo

      call cg_open_f (fname, CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_base_write_f (cgfile,"Base", celldim, physdim, cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     write zone

      do n =1,3
        size(n)   = NUM_SIDE
        size(n+3) = NUM_SIDE - 1
        size(n+6) = 0
      enddo
      call cg_zone_write_f (cgfile, cgbase, "Zone", size, CGNS_ENUMV(Structured), &
     &                    cgzone, ierr)
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle), &
     &            "CoordinateX", coord, cgcoord, ierr)
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle), &
     &            "CoordinateY", coord, cgcoord, ierr)
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle), &
     &            "CoordinateZ", coord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_close_f (cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     read file
      call cg_open_f (fname, CG_MODE_READ, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      cgbase = 1
      cgzone = 1

      bbox(1,1) = 1.0
      bbox(1,2) = -1.0
      bbox(2,1) = 1.0
      bbox(2,2) = -1.0
      bbox(3,1) = 1.0
      bbox(3,2) = -1.0
!     check bounding box is not modified
      call cg_grid_bounding_box_read_f(cgfile, cgbase, cgzone, 1, &
     &                               CGNS_ENUMV(RealDouble), bbox, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (bbox(1,2) .ne. -1.0) stop
      if (bbox(1,1) .ne. 1.0) stop
      call cg_close_f (cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_open_f (fname, CG_MODE_MODIFY, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      cgbase = 1
      cgzone = 1

      bbox(1, 1) = 1.0
      bbox(1 ,2) = (NUM_SIDE*NUM_SIDE*NUM_SIDE)
      bbox(2, 1) = 1.0;
      bbox(2, 2) = (NUM_SIDE*NUM_SIDE*NUM_SIDE)
      bbox(3, 1) = 1.0
      bbox(3, 2) = (NUM_SIDE*NUM_SIDE*NUM_SIDE)

      call cg_grid_bounding_box_write_f(cgfile, cgbase, cgzone, 1, &
     &                                CGNS_ENUMV(RealDouble), bbox, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_open_f(fname, CG_MODE_READ, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      cgbase = 1
      cgzone = 1
      bbox(1,1) = 1.0
      bbox(1,2) = -1.0
      bbox(2,1) = 1.0
      bbox(2,2) = -1.0
      bbox(3,1) = 1.0
      bbox(3,2) = -1.0
!     check bounding box
      call cg_grid_bounding_box_read_f(cgfile, cgbase, cgzone, 1, &
     &                               CGNS_ENUMV(RealDouble), bbox, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (bbox(1, 1) .ne. 1.0) stop
      if (bbox(1, 2) .ne. (NUM_SIDE*NUM_SIDE*NUM_SIDE)) stop
    
      call cg_close_f (cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      end program
