      program testgeneralrindf

#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      implicit none

      integer, parameter :: celldim = 3, physdim = 3
      integer(cgsize_t), parameter :: size(3,3) =
     &  reshape((/5,5,5, 4,4,4, 0,0,0 /), (/3, 3/))
      integer, parameter :: rind(2,3) =
     &  reshape((/2,2, 2,2, 1,1/), (/2, 3/))
      integer(cgsize_t), parameter :: NUM_I =
     &  size(1,1) + rind(1,1) + rind(2,1)
      integer(cgsize_t), parameter :: NUM_J =
     &  size(2,1) + rind(1,2) + rind(2,2)
      integer(cgsize_t), parameter :: NUM_K =
     &  size(3,1) + rind(1,3) + rind(2,3)
      integer(cgsize_t), parameter :: num_coord = NUM_I * NUM_J * NUM_K

      integer :: ierr
      integer :: cgfile, cgbase, cgzone, cggrid, cgcoord, cgsol, cgfld
      integer :: nn, np

      integer(cgsize_t) :: i, j, k, n
      integer(cgsize_t) :: dims(3)
      integer(cgsize_t) :: rmin(3), rmax(3)
      integer(cgsize_t) :: m_rmin(3), m_rmax(3)

      real*4, dimension(NUM_I, NUM_J, NUM_K) :: xcoord, ycoord, zcoord
      real*4, dimension(NUM_I, NUM_J, NUM_K) :: solution, fbuf

      character*32 coordname(3)
      character*32 fieldname

      coordname(1) = 'CoordinateX'
      coordname(2) = 'CoordinateY'
      coordname(3) = 'CoordinateZ'

      fieldname = 'Density'

      do k = 1, NUM_K
        do j = 1, NUM_J
           do i = 1, NUM_I
              call compute_coord()
              call compute_sol()
          enddo
        enddo
      enddo

c     open

      call cg_open_f('rindf.cgns', CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

c---- structured grid with rind ----

      print *,'writing structured base with rind'

c     write base and zone

      call cg_base_write_f(cgfile, 'Structured', celldim, physdim,
     &                     cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_zone_write_f(cgfile, cgbase, 'Zone', size,             
     &                     Structured, cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

c     use cg_coord_general_write to write coordinates with all rinds
c     need to use cg_grid_write to create the node, cg_goto to set
c     position at the node, then write rind */
      
      dims(1) = NUM_I
      dims(2) = NUM_J
      dims(3) = NUM_K

      do n=1,3
        rmin(n)   = get_s_rmin(n, rind(1,n))
        rmax(n)   = get_s_rmax(n, rind(2,n))
        m_rmin(n) = get_m_rmin(n, rind(1,n))
        m_rmax(n) = get_m_rmax(n, rind(2,n))
      enddo

c     write coordinates with rind

      call cg_grid_write_f(cgfile, cgbase, cgzone, 'GridCoordinates',
     &                     cggrid, ierr)
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone,
     &               'GridCoordinates_t', cggrid, 'end')
      call cg_rind_write_f(rind, ierr)

      call cg_coord_general_write_f(cgfile, cgbase, cgzone,
     &                              Realsingle, coordname(1),
     &                              rmin, rmax, 3, dims, m_rmin, m_rmax,
     &                              xcoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_general_write_f(cgfile, cgbase, cgzone,
     &                              RealSingle, coordname(2),
     &                              rmin, rmax, 3, dims, m_rmin, m_rmax,
     &                              ycoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_general_write_f(cgfile, cgbase, cgzone,
     &                              RealSingle, coordname(3),
     &                              rmin, rmax, 3, dims, m_rmin, m_rmax,
     &                              zcoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

c     write solution with rind, and the solution dimensions come from the zone
c     sizes

      call cg_sol_write_f(cgfile, cgbase, cgzone, 'VertexSolution',
     &                    Vertex, cgsol, ierr)
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone,
     &               'FlowSolution_t', cgsol, 'end')
      call cg_rind_write_f(rind, ierr)
      call cg_field_general_write_f(cgfile, cgbase, cgzone, cgsol,
     &                              RealSingle, fieldname,
     &                              rmin, rmax, 3, dims, m_rmin, m_rmax,
     &                              solution, cgfld, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

c     close the file and reopen in read mode

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      print *,'closing and reopening in read mode '     

c     read file and check the data

      call cg_open_f('rindf.cgns', CG_MODE_READ, cgfile, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f

      write(6,*)'checking the data '
      cgbase = 1
      cgzone = 1
      cggrid = 1
      cgsol  = 1

      nn = 0

c     check coordinates
c     Only load core coordinates without rind but inside memory with rind

      do n=1,3
        rmin(n)   = get_s_rmin(n, 0)
        rmax(n)   = get_s_rmax(n, 0)
        m_rmin(n) = get_m_rmin(n, 0)
        m_rmax(n) = get_m_rmax(n, 0)
      enddo

c     X
      call cg_coord_general_read_f(cgfile, cgbase, cgzone,
     &                             'CoordinateX', RealSingle,
     &                             rmin, rmax,
     &                             3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k = idxmin(3,0), idxmax(3,0)
         do j = idxmin(2,0), idxmax(2,0)
            do i = idxmin(1,0), idxmax(1,0)
              if (fbuf(i,j,k) .NE. xcoord(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .NE. 0) then
        print *,'differences in CoordinateX'
      endif

c     Y
      call cg_coord_general_read_f(cgfile, cgbase, cgzone,
     &                             'CoordinateY', RealSingle,
     &                             rmin, rmax,
     &                             3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k = idxmin(3,0), idxmax(3,0)
         do j = idxmin(2,0), idxmax(2,0)
            do i = idxmin(1,0), idxmax(1,0)
              if (fbuf(i,j,k) .NE. ycoord(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .NE. 0) then
        print *,'differences in CoordinateY'
      endif

c     Z
      call cg_coord_general_read_f(cgfile, cgbase, cgzone,
     &                             'CoordinateZ', RealSingle,
     &                             rmin, rmax,
     &                             3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k = idxmin(3,0), idxmax(3,0)
         do j = idxmin(2,0), idxmax(2,0)
            do i = idxmin(1,0), idxmax(1,0)
              if (fbuf(i,j,k) .NE. zcoord(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .NE. 0) then
        print *,'differences in CoordinateZ'
      endif

c     check field with only one rind layer

      do n=1,3
        rmin(n)   = get_s_rmin(n, 1)
        rmax(n)   = get_s_rmax(n, 1)
        m_rmin(n) = get_m_rmin(n, 1)
        m_rmax(n) = get_m_rmax(n, 1)
      enddo

      call cg_field_general_read_f(cgfile, cgbase, cgzone, cgsol,
     &                             'Density', RealSingle,
     &                             rmin, rmax,
     &                             3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k = idxmin(3,1), idxmax(3,1)
         do j = idxmin(2,1), idxmax(2,1)
            do i = idxmin(1,1), idxmax(1,1)
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *, np, ' differences in Field'
      endif

      if (nn .eq. 0) then
        print *,'no differences'
      endif

      print *,'closing file'
      call cg_close_f(cgfile, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f

      if (nn .ne. 0) then
        stop
      endif

      return

c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      contains

      function INDEX(ii, jj, kk)
      integer(cgsize_t) :: INDEX, ii, jj, kk
      INDEX = ii + NUM_I*(jj + NUM_J*(kk))
      end function

c     ranges for arrays sent to CGNS
      function get_s_rmin(n, nr)
      integer(cgsize_t) :: get_s_rmin
      integer :: n, nr
      get_s_rmin = 1 - nr
      end function

      function get_s_rmax(n, nr)
      integer(cgsize_t) :: get_s_rmax
      integer :: n, nr
      get_s_rmax = size(n,1) + nr
      end function

      function get_m_rmin(n, nr)
      integer(cgsize_t) :: get_m_rmin
      integer :: n, nr
      get_m_rmin = 1 + rind(1,n) - nr;
      end function

      function get_m_rmax(n, nr)
      integer(cgsize_t) :: get_m_rmax
      integer :: n, nr
      get_m_rmax = rind(1,n) + size(n,1) + nr;
      end function

c     ranges for accessing arrays
      function idxmin(n, nr)
      integer(cgsize_t) :: idxmin
      integer :: n, nr
      idxmin = get_m_rmin(n, nr);
      end function

      function idxmax(n, nr)
      integer(cgsize_t) :: idxmax
      integer :: n, nr
      idxmax = get_m_rmax(n, nr);
      end function

c     initial data
      subroutine compute_coord()
      xcoord(i, j, k) = i - 1 - rind(1,1)
      ycoord(i, j, k) = j - 1 - rind(1,2)
      zcoord(i, j, k) = k - 1 - rind(1,3)
      end subroutine

      subroutine compute_sol()
      integer :: sign
      if ((i - 1 < rind(1,1)) .or. (i - 1 >= size(1,1) + rind(1,1)) .or.
     &    (j - 1 < rind(1,2)) .or. (j - 1 >= size(2,1) + rind(1,2)) .or.
     &    (k - 1 < rind(1,3)) .or. (k - 1 >= size(3,1) + rind(1,3)))
     &  then
        sign = -1
      else
        sign = 1
      endif
      solution(i, j, k) = sign*(1 + (k)*1100 + INDEX(i - 1, j - 1, 0))
      end subroutine
     
      end program
