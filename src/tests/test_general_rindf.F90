      program testgeneralrindf

#include "cgnstypes_f03.h"
#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      implicit none
      
      INTEGER, PARAMETER :: sp = KIND(1.0)

      integer, parameter :: celldim = 3, physdim = 3
      integer(cgsize_t), parameter :: size(3,3) = &
     &  reshape((/5,5,5, 4,4,4, 0,0,0 /), (/3, 3/))
      integer, parameter :: rind(2,3) = &
     &  reshape((/2,2, 2,2, 1,1/), (/2, 3/))
      integer(cgsize_t), parameter :: NUM_I = &
     &  size(1,1) + rind(1,1) + rind(2,1)
      integer(cgsize_t), parameter :: NUM_J = &
     &  size(2,1) + rind(1,2) + rind(2,2)
      integer(cgsize_t), parameter :: NUM_K = &
     &  size(3,1) + rind(1,3) + rind(2,3)
      integer(cgsize_t), parameter :: num_coord = NUM_I * NUM_J * NUM_K

      integer :: ierr
      integer :: cgfile, cgbase, cgzone, cggrid, cgcoord, cgsol, cgfld
      integer :: n, nn, np

      integer(cgsize_t) :: i, j, k
      integer(cgsize_t) :: dims(3)
      integer(cgsize_t) :: rmin(3), rmax(3)
      integer(cgsize_t) :: m_rmin(3), m_rmax(3)

      real(kind=sp), dimension(NUM_I, NUM_J, NUM_K) :: xcoord, ycoord, zcoord
      real(kind=sp), dimension(NUM_I, NUM_J, NUM_K) :: solution, fbuf

      character(len=32) coordname(3)
      character(len=32) fieldname

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

!     open

      call cg_open_f('rindf.cgns', CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!---- structured grid with rind ----

      print *,'writing structured base with rind'

!     write base and zone

      call cg_base_write_f(cgfile, 'Structured', celldim, physdim, &
     &                     cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_zone_write_f(cgfile, cgbase, 'Zone', size,              &
     &                     CGNS_ENUMV(Structured), cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     use cg_coord_general_write to write coordinates with all rinds
!     need to use cg_grid_write to create the node, cg_goto to set
!     position at the node, then write rind */

      dims(1) = NUM_I
      dims(2) = NUM_J
      dims(3) = NUM_K

      do n=1,3
        rmin(n)   = get_s_rmin(n, rind(1,n))
        rmax(n)   = get_s_rmax(n, rind(2,n))
        m_rmin(n) = get_m_rmin(n, rind(1,n))
        m_rmax(n) = get_m_rmax(n, rind(2,n))
      enddo

!     write coordinates with rind

      call cg_grid_write_f(cgfile, cgbase, cgzone, 'GridCoordinates', &
     &                     cggrid, ierr)
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone, &
     &               'GridCoordinates_t', cggrid, 'end')
      call cg_rind_write_f(rind, ierr)

      call cg_coord_general_write_f(cgfile, cgbase, cgzone, &
     &                              coordname(1), CGNS_ENUMV(RealSingle), &
     &                              rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                              3, dims, m_rmin, m_rmax, &
     &                              xcoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_general_write_f(cgfile, cgbase, cgzone, &
     &                              coordname(2), CGNS_ENUMV(RealSingle), &
     &                              rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                              3, dims, m_rmin, m_rmax, &
     &                              ycoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_general_write_f(cgfile, cgbase, cgzone, &
     &                              coordname(3), CGNS_ENUMV(RealSingle), &
     &                              rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                              3, dims, m_rmin, m_rmax, &
     &                              zcoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     write solution with rind, and the solution dimensions come from the zone
!     sizes

      call cg_sol_write_f(cgfile, cgbase, cgzone, 'VertexSolution', &
     &                    CGNS_ENUMV(Vertex), cgsol, ierr)
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone, &
     &               'FlowSolution_t', cgsol, 'end')
      call cg_rind_write_f(rind, ierr)
      call cg_field_general_write_f(cgfile, cgbase, cgzone, cgsol, &
     &                              fieldname, CGNS_ENUMV(RealSingle), &
     &                              rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                              3, dims, m_rmin, m_rmax, &
     &                              solution, cgfld, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     close the file and reopen in read mode

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      print *,'closing and reopening in read mode '     

!     read file and check the data

      call cg_open_f('rindf.cgns', CG_MODE_READ, cgfile, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f

      write(6,*)'checking the data '
      cgbase = 1
      cgzone = 1
      cggrid = 1
      cgsol  = 1

      nn = 0

!     check coordinates
!     Only load core coordinates without rind but inside memory with rind

      do n=1,3
        rmin(n)   = get_s_rmin(n, 0)
        rmax(n)   = get_s_rmax(n, 0)
        m_rmin(n) = get_m_rmin(n, 0)
        m_rmax(n) = get_m_rmax(n, 0)
      enddo

!     X
      call cg_coord_general_read_f(cgfile, cgbase, cgzone, &
     &                             'CoordinateX', &
     &                             rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                             3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k = idxmin(3,0), idxmax(3,0)
         do j = idxmin(2,0), idxmax(2,0)
            do i = idxmin(1,0), idxmax(1,0)
              if (fbuf(i,j,k) .ne. xcoord(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *,'differences in CoordinateX'
      endif

!     Y
      call cg_coord_general_read_f(cgfile, cgbase, cgzone, &
     &                             'CoordinateY', &
     &                             rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                             3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k = idxmin(3,0), idxmax(3,0)
         do j = idxmin(2,0), idxmax(2,0)
            do i = idxmin(1,0), idxmax(1,0)
              if (fbuf(i,j,k) .ne. ycoord(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *,'differences in CoordinateY'
      endif

!     Z
      call cg_coord_general_read_f(cgfile, cgbase, cgzone, &
     &                             'CoordinateZ', &
     &                             rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                             3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k = idxmin(3,0), idxmax(3,0)
         do j = idxmin(2,0), idxmax(2,0)
            do i = idxmin(1,0), idxmax(1,0)
              if (fbuf(i,j,k) .ne. zcoord(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *,'differences in CoordinateZ'
      endif

!     check field with only one rind layer

      do n=1,3
        rmin(n)   = get_s_rmin(n, 1)
        rmax(n)   = get_s_rmax(n, 1)
        m_rmin(n) = get_m_rmin(n, 1)
        m_rmax(n) = get_m_rmax(n, 1)
      enddo

      call cg_field_general_read_f(cgfile, cgbase, cgzone, cgsol, &
     &                             'Density', &
     &                             rmin, rmax, CGNS_ENUMV(RealSingle), &
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
        print *,'no differences (part 1)'
      endif

!===============================================================================

!-----We now go to modify mode and repeadtely test writing and reading of the
!-----field

!     close the file and reopen in modify mode
      print *, 'closing and reopening in modify mode'
      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_open_f('rindf.cgns', CG_MODE_MODIFY, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     delete the node
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone, &
     &     'FlowSolution_t', cgsol, 'end')
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_delete_node_f('Density', ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     write the field using high-level routine
      call cg_field_write_f(cgfile, cgbase, cgzone, cgsol, CGNS_ENUMV(RealSingle), &
     &                      fieldname, solution, cgfld, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     verify the written data
      do n=1,3
        rmin(n) = get_s_rmin(n, -1)
        rmax(n) = get_s_rmax(n, -1)
      enddo
      call cg_field_read_f(cgfile, cgbase, cgzone, cgsol, fieldname, &
     &                     CGNS_ENUMV(RealSingle), rmin, rmax, fbuf, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      np = 0
      do k = idxmin(3,-1), idxmax(3,-1)
         do j = idxmin(2,-1), idxmax(2,-1)
            do i = idxmin(1,-1), idxmax(1,-1)
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *, np, ' differences in Field (T1)'
      endif

!     if given ranges span the full dimensions, the read should succeed no
!     matter what the range is (this behavior is for backwards compatibility and
!     is not documented)
      do n=1,3
        rmin(n) = 1
        rmax(n) = dims(n)
      enddo
      call cg_field_read_f(cgfile, cgbase, cgzone, cgsol, fieldname, &
     &                     CGNS_ENUMV(RealSingle), rmin, rmax, fbuf, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      np = 0
      do k = idxmin(3,-1), idxmax(3,-1)
         do j = idxmin(2,-1), idxmax(2,-1)
            do i = idxmin(1,-1), idxmax(1,-1)
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *, np, ' differences in Field (T2)'
      endif

!     try again with really weird dimensions
      do n=1,3
        rmin(n) = -100*n
        rmax(n) = rmin(n) + dims(n) - 1
      enddo
      call cg_field_read_f(cgfile, cgbase, cgzone, cgsol, fieldname, &
     &                     CGNS_ENUMV(RealSingle), rmin, rmax, fbuf, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      np = 0
      do k = idxmin(3,-1), idxmax(3,-1)
         do j = idxmin(2,-1), idxmax(2,-1)
            do i = idxmin(1,-1), idxmax(1,-1)
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *, np, ' differences in Field (T3)'
      endif

!     however, if given ranges do not span the full dimensions, ranges are
!     checked
      do n=1,3
        rmin(n) = 1
        rmax(n) = dims(n) - 1
      enddo
      write (*, '(" Next error is required: ")', advance='no')
      call cg_field_read_f(cgfile, cgbase, cgzone, cgsol, fieldname, &
     &                     CGNS_ENUMV(RealSingle), rmin, rmax, fbuf, ierr)
      if (ierr == CG_OK) then
         print *, 'read failed to produce error (T4)'
         nn = nn + 1
      endif
      call cg_error_print_f

!     test old behavior where first rind plane is index 1 */
      call cg_set_rind_zero_f(ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     this is the proper range for old behavior
      do n=1,3
        rmin(n) = 1
        rmax(n) = dims(n)
      enddo
      call cg_field_read_f(cgfile, cgbase, cgzone, cgsol, fieldname, &
     &                     CGNS_ENUMV(RealSingle), rmin, rmax, fbuf, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      np = 0
      do k = idxmin(3,-1), idxmax(3,-1)
         do j = idxmin(2,-1), idxmax(2,-1)
            do i = idxmin(1,-1), idxmax(1,-1)
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *, np, ' differences in Field (T5)'
      endif

!     reading full range should still work for any dimension
      do n=1,3
        rmin(n) = -200*n
        rmax(n) = rmin(n) + dims(n) - 1
      enddo
      call cg_field_read_f(cgfile, cgbase, cgzone, cgsol, fieldname, &
     &                     CGNS_ENUMV(RealSingle), rmin, rmax, fbuf, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      np = 0
      do k = idxmin(3,-1), idxmax(3,-1)
         do j = idxmin(2,-1), idxmax(2,-1)
            do i = idxmin(1,-1), idxmax(1,-1)
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *, np, ' differences in Field (T6)'
      endif

!     ranges are checked if they do not span the full dimensions.  Now,
!     rmin < 1 is a failure
      do n=1,3
        rmin(n) = get_s_rmin(n, 1)
        rmax(n) = get_s_rmax(n, 1)
      enddo
      write (*, '(" Next error is required: ")', advance='no')
      call cg_field_read_f(cgfile, cgbase, cgzone, cgsol, fieldname, &
     &                     CGNS_ENUMV(RealSingle), rmin, rmax, fbuf, ierr)
      if (ierr == CG_OK) then
         print *, 'read failed to produce error (T7)'
         nn = nn + 1
      endif
      call cg_error_print_f

      nn = nn + np
      if (nn .eq. 0) then
        print *,'no differences (part 2)'
      endif

!===============================================================================

!     back to testing new behavior
      call cg_set_rind_core_f(ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

!     Tests in part 3 should not encounter anything different than C and are not
!     repeated.  But put the file in the same state by doing tests T13 and T14.
      solution(idxmin(1, 1)  , idxmin(2, 0)  , idxmin(3, 0)) = 8888.5
      solution(idxmin(1, 1)+1, idxmin(2, 0)  , idxmin(3, 0)) = 8888.6
      solution(idxmin(1, 1)  , idxmin(2, 0)+1, idxmin(3, 0)) = 8888.7
      solution(idxmin(1, 1)+1, idxmin(2, 0)+1, idxmin(3, 0)) = 8888.8
      rmin(1)   = get_s_rmin(1, 1)
      rmax(1)   = get_s_rmin(1, 1) + 1
      rmin(2)   = get_s_rmin(2, 0)
      rmax(2)   = get_s_rmin(2, 0) + 1
      rmin(3)   = get_s_rmin(3, 0)
      rmax(3)   = get_s_rmax(3, 0)
      m_rmin(1) = get_m_rmin(1, 1)
      m_rmax(1) = get_m_rmin(1, 1) + 1
      m_rmin(2) = get_m_rmin(2, 0)
      m_rmax(2) = get_m_rmin(2, 0) + 1
      m_rmin(3) = get_m_rmin(3, 0)
      m_rmax(3) = get_m_rmax(3, 0)
      call cg_field_general_write_f(cgfile, cgbase, cgzone, cgsol, &
     &                              fieldname, CGNS_ENUMV(RealSingle), &
     &                              rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                              3, dims, m_rmin, m_rmax, &
     &                              solution, cgfld, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
!     verify the written data
      do n=1,3
        rmin(n) = get_s_rmin(n, -1)
        rmax(n) = get_s_rmax(n, -1)
      enddo
      call cg_field_read_f(cgfile, cgbase, cgzone, cgsol, fieldname, &
     &                     CGNS_ENUMV(RealSingle), rmin, rmax, fbuf, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      np = 0
      do k = idxmin(3,-1), idxmax(3,-1)
         do j = idxmin(2,-1), idxmax(2,-1)
            do i = idxmin(1,-1), idxmax(1,-1)
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *, np, ' differences in Field (T13)'
      endif

!     verify again by only reading the 4 locations
      fbuf(idxmin(1, 1)  , idxmin(2, 0)  , idxmin(3, 0)) = 0.
      fbuf(idxmin(1, 1)+1, idxmin(2, 0)  , idxmin(3, 0)) = 0.
      fbuf(idxmin(1, 1)  , idxmin(2, 0)+1, idxmin(3, 0)) = 0.
      fbuf(idxmin(1, 1)+1, idxmin(2, 0)+1, idxmin(3, 0)) = 0.
      rmin(1) = get_s_rmin(1, 1)
      rmax(1) = get_s_rmin(1, 1) + 1
      rmin(2) = get_s_rmin(2, 0)
      rmax(2) = get_s_rmin(2, 0) + 1
      rmin(3) = get_s_rmin(3, 0)
      rmax(3) = get_s_rmax(3, 0)
      call cg_field_general_read_f(cgfile, cgbase, cgzone, cgsol, &
     &                             'Density', &
     &                             rmin, rmax, CGNS_ENUMV(RealSingle), &
     &                             3, dims, m_rmin, m_rmax, fbuf, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      np = 0
      do k = idxmin(3,-1), idxmax(3,-1)
         do j = idxmin(2,-1), idxmax(2,-1)
            do i = idxmin(1,-1), idxmax(1,-1)
              if (fbuf(i,j,k) .ne. solution(i,j,k)) then
                np = np + 1
              endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .ne. 0) then
        print *, np, ' differences in Field (T14)'
      endif

      if (nn .eq. 0) then
        print *,'no differences (part 3)'
      endif

      print *, 'closing file'
      call cg_close_f(cgfile, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f

      if (nn .ne. 0) then
        stop
      endif

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      contains

      function INDEX(ii, jj, kk)
      integer(cgsize_t) :: INDEX, ii, jj, kk
      INDEX = ii + NUM_I*(jj + NUM_J*(kk))
      end function

!     ranges for arrays sent to CGNS
!     s_ is the range in file space.  Core cells start at 1 so rind planes are
!     <= 0.
      function get_s_rmin(n, nr)
      integer(cgsize_t) :: get_s_rmin
      integer :: n, nr
      integer :: nrl
      if (nr < 0) then
         nrl = rind(1,n)
      else
         nrl = nr
      endif
      get_s_rmin = int(1 - nrl, kind=cgsize_t)
      end function

      function get_s_rmax(n, nr)
      integer(cgsize_t) :: get_s_rmax
      integer :: n, nr
      integer :: nrl
      if (nr < 0) then
         nrl = rind(2,n)
      else
         nrl = nr
      endif
      get_s_rmax = int(size(n,1) + nrl, kind=cgsize_t)
      end function

!     m_ is the range in memory.  The lowest index in each dimension is 1.  If
!     there are rind planes, then he core cells start at a value > 1.
      function get_m_rmin(n, nr)
      integer(cgsize_t) :: get_m_rmin
      integer :: n, nr
      integer :: nrl
      if (nr < 0) then
         nrl = rind(1,n)
      else
         nrl = nr
      endif
      get_m_rmin =  int(1 + rind(1,n) - nrl, kind=cgsize_t)
      end function

      function get_m_rmax(n, nr)
      integer(cgsize_t) :: get_m_rmax
      integer :: n, nr
      integer :: nrl
      if (nr < 0) then
         nrl = rind(2,n)
      else
         nrl = nr
      endif
      get_m_rmax = int(rind(1,n) + size(n,1) + nrl, kind=cgsize_t)
      end function

!     ranges for accessing arrays
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

!     initial data
      subroutine compute_coord()
      xcoord(i, j, k) = i - 1 - rind(1,1)
      ycoord(i, j, k) = j - 1 - rind(1,2)
      zcoord(i, j, k) = k - 1 - rind(1,3)
      end subroutine

      subroutine compute_sol()
      integer :: sign
      if ((i - 1 < rind(1,1)) .or. (i - 1 >= size(1,1) + rind(1,1)) .or. &
     &    (j - 1 < rind(1,2)) .or. (j - 1 >= size(2,1) + rind(1,2)) .or. &
     &    (k - 1 < rind(1,3)) .or. (k - 1 >= size(3,1) + rind(1,3))) &
     &  then
        sign = -1
      else
        sign = 1
      endif
      solution(i, j, k) = sign*(1 + (k)*1100 + &
     &  INDEX(i - 1, j - 1, 0_cgsize_t))
      end subroutine

      end program
