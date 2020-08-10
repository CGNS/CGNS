      program testcomplexf
#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      IMPLICIT NONE
#if CG_BUILD_COMPLEX_C99_EXT
      INTEGER, PARAMETER :: dp = KIND(1.d0)
      
      integer, parameter :: celldim = 3, physdim = 3
      integer(cgsize_t), parameter :: size(3,3) = &
     &  reshape((/5,5,5, 4,4,4, 0,0,0 /), (/3, 3/))
      integer(cgsize_t), parameter :: NUM_I = &
     &  size(1,1)
      integer(cgsize_t), parameter :: NUM_J = &
     &  size(2,1)
      integer(cgsize_t), parameter :: NUM_K = &
     &  size(3,1)
      integer(cgsize_t), parameter :: num_coord = NUM_I * NUM_J * NUM_K

      integer :: ierr
      integer :: cgfile, cgbase, cgzone, cggrid, cgcoord, cgsol, cgfld
      integer :: n, np

      integer(cgsize_t) :: i, j, k
      integer(cgsize_t) :: dims(3)
      integer(cgsize_t) :: rmin(3), rmax(3)
      integer(cgsize_t) :: m_rmin(3), m_rmax(3)

      REAL(KIND=dp), DIMENSION(NUM_I, NUM_J, NUM_K) :: xcoord, ycoord, zcoord
      COMPLEX(KIND=dp), DIMENSION(NUM_I, NUM_J, NUM_K) :: solution, fbuf

      character*32 coordname(3)
      character*32 fieldname

      coordname(1) = 'CoordinateX'
      coordname(2) = 'CoordinateY'
      coordname(3) = 'CoordinateZ'

      fieldname = 'Pressure'

      do k = 1, NUM_K
        do j = 1, NUM_J
           do i = 1, NUM_I
              call compute_coord()
              call compute_sol()
          enddo
        enddo
      enddo

!     open

      call cg_open_f('complex_dataf.cgns', CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!---- structured grid ----

      print *,'writing structured base with complex data'

!     write base and zone

      call cg_base_write_f(cgfile, 'Structured', celldim, physdim, &
     &                     cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_zone_write_f(cgfile, cgbase, 'Zone', size,              &
     &                     Structured, cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      dims(1) = NUM_I
      dims(2) = NUM_J
      dims(3) = NUM_K

      do n=1,3
        rmin(n)   = 1
        rmax(n)   = dims(n)
        m_rmin(n) = 1
        m_rmax(n) = dims(n)
      enddo

!     write coordinates

      call cg_grid_write_f(cgfile, cgbase, cgzone, 'GridCoordinates', &
     &                     cggrid, ierr)
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone, &
     &               'GridCoordinates_t', cggrid, 'end')

      call cg_coord_general_write_f(cgfile, cgbase, cgzone, &
     &                           coordname(1), RealDouble, &
     &                           rmin, rmax, RealDouble, &
     &                           3, dims, m_rmin, m_rmax, &
     &                           xcoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_general_write_f(cgfile, cgbase, cgzone, &
     &                           coordname(2), RealDouble, &
     &                           rmin, rmax, RealDouble, &
     &                           3, dims, m_rmin, m_rmax, &
     &                           ycoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_general_write_f(cgfile, cgbase, cgzone, &
     &                           coordname(3), RealDouble, &
     &                           rmin, rmax, RealDouble, &
     &                           3, dims, m_rmin, m_rmax, &
     &                           zcoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     write solution

      call cg_sol_write_f(cgfile, cgbase, cgzone, 'Solution', &
     &                    Vertex, cgsol, ierr)
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone, &
     &               'FlowSolution_t', cgsol, 'end')
      call cg_field_general_write_f(cgfile, cgbase, cgzone, cgsol, &
     &                          fieldname, ComplexSingle, &
     &                          rmin, rmax, ComplexDouble, &
     &                          3, dims, m_rmin, m_rmax, &
     &                          solution, cgfld, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     close the file and reopen in read mode

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      print *,'closing and reopening in read mode '     

!     read file and check the data

      call cg_open_f('complex_dataf.cgns', CG_MODE_READ, cgfile, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f

      write(6,*)'checking the data '
      cgbase = 1
      cgzone = 1
      cggrid = 1
      cgsol  = 1

!     check field

      call cg_field_general_read_f(cgfile, cgbase, cgzone, cgsol, &
     &                         'Pressure', &
     &                          rmin, rmax, ComplexDouble, &
     &                          3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k = 1, NUM_K
         do j = 1, NUM_J
            do i = 1, NUM_I
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      if (np .ne. 0) then
        print *, np, ' differences in Field'
      endif

!     close the file
      print *, 'closing file'
      call cg_close_f(cgfile, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f

      if (np .ne. 0) then
        stop
      endif

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      contains

!     initial data
      subroutine compute_coord()
      xcoord(i, j, k) = i
      ycoord(i, j, k) = j
      zcoord(i, j, k) = k
      end subroutine

      subroutine compute_sol()
      solution(i, j, k) = CMPLX(REAL(i,KIND=dp), REAL(j,KIND=dp), KIND=dp)
      end subroutine
#endif
      end program
