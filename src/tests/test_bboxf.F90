      program testbboxf

#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      implicit none

      integer, parameter :: celldim = 3, physdim = 3
      integer, parameter :: NUM_SIDE = 5

      real*4, dimension(NUM_SIDE*NUM_SIDE*NUM_SIDE) coord

      integer n, i
      integer ierr
      integer cgfile, cgbase, cgzone, cgcoord
      integer(cgsize_t) size(9)
      
      chareacter*32 fname
      fname = "boundingbox_f90.cgns"
      real*8, dimension(3,2) bbox

      do n=1,NUM_SIDE*NUM_SIDE*NUM_SIDE
        coord(n) = n
      enddo

      call unlink(fname)
      call cg_open_f (fname, CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_base_write (cgfile, "Base", celldim, physdim, cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     write zone

      do n =1,3
        size(n)   = NUM_SIDE
        size(n+3) = NUM_SIDE - 1
        size(n+6) = 0
      enddo
      call cg_zone_write (cgfile, cgbase, "Zone", size, Structured, cgzone, ierr)
      call cg_coord_write(cgfile, cgbase, cgzone, RealSingle, &
     &            "CoordinateX", coord, cgcoord, ierr)
      call cg_coord_write(cgfile, cgbase, cgzone, RealSingle, &
     &            "CoordinateY", coord, cgcoord, ierr)
      call cg_coord_write(cgfile, cgbase, cgzone, RealSingle, &
     &            "CoordinateZ", coord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_close_f (cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     read file
      call cg_open (fname, CG_MODE_READ, cgfile, ierr)
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
      call cg_grid_bounding_box_read(cgfile, cgbase, cgzone, 1, &
	 &                               RealDouble, bbox, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (bbox(1,2) .ne. -1.0) stop
      if (bbox(1,1) .ne. 1.0) stop
      call cg_close (cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_open (fname, CG_MODE_MODIFY, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      cgbase = 1
      cgzone = 1

      bbox(1, 1) = 0.0
      bbox(1 ,2) = (NUM_SIDE*NUM_SIDE*NUM_SIDE -1)
      bbox(2, 1) = 0.0;
      bbox(2, 2) = (NUM_SIDE*NUM_SIDE*NUM_SIDE -1)
      bbox(3, 1) = 0.0
      bbox(3, 2) = (NUM_SIDE*NUM_SIDE*NUM_SIDE -1)

      call cg_grid_bounding_box_write(cgfile, cgbase, cgzone, 1, &
     &                                RealDouble, bbox, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_close (cgfile, ierr)
	    if (ierr .ne. CG_OK) call cg_error_exit_f

      call (cg_open (fname, CG_MODE_READ, cgfile, ierr)
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
      call cg_grid_bounding_box_read(cgfile, cgbase, cgzone, 1, &
     &                               RealDouble, bbox, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (bbox(1, 1) .ne. 0.0) stop
	  if (bbox(1, 2) .ne. (NUM_SIDE*NUM_SIDE*NUM_SIDE -1.)) stop

    
      call cg_close (cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      end program
