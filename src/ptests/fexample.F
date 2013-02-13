      program fexample

#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      include 'cgnslib_f.h'
      include 'mpif.h'
#include "cgnstypes_f.h"

      integer nperside, totnodes, totelems
      parameter (nperside = 5)
      parameter (totnodes=nperside*nperside*nperside)
      parameter (totelems=(nperside-1)*(nperside-1)*(nperside-1))

      integer commsize, commrank, ierr
      integer i, j, k, n, nn, ne
      integer F, B, Z, E, S, Fs, Cx, Cy, Cz, A
      integer nnodes, nelems
      integer sizes(3), start, end
      real*4 fx(totnodes), fy(totnodes), fz(totnodes), fd(totelems)
      integer ie(8*totelems)
C
#if CG_BUILD_64BIT
      PRINT *,'PROGRAM NOT SET UP FOR 64-BIT'
      STOP
#endif
c---- initialize MPI
      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, commsize, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, commrank, ierr)

c---- open file and create base and zone
      sizes(1) = totnodes
      sizes(2) = totelems
      sizes(3) = 0

      call cgp_open_f('fexample.cgns', CG_MODE_WRITE, F, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cg_base_write_f(F, 'Base', 3, 3, B, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cg_zone_write_f(F, B, 'Zone', sizes, Unstructured, Z, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

c---- print info
      if (commrank .eq. 0) then
        print *, 'writing',totnodes,' coordinates and', totelems,
     &           ' hex elements to fexample.cgns'
      endif

c---- create data nodes for coordinates
      call cgp_coord_write_f(F, B, Z, RealSingle, 'CoordinateX',
     &                       Cx, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cgp_coord_write_f(F, B, Z, RealSingle, 'CoordinateY',
     &                       Cy, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cgp_coord_write_f(F, B, Z, RealSingle, 'CoordinateZ',
     &                       Cz, ierr)

c---- number of nodes and range this process will write
      nnodes = (totnodes + commsize - 1) / commsize
      start  = nnodes * commrank + 1
      end    = nnodes * (commrank + 1)
      if (end .gt. totnodes) end = totnodes

c---- create the coordinate data for this process
      nn = 1
      n  = 1
      do k=1,nperside
        do j=1,nperside
          do i=1,nperside
            if (n .ge. start .and. n .le. end) then
              fx(nn) = i
              fy(nn) = j
              fz(nn) = k
              nn = nn + 1
            endif
            n = n + 1
          enddo
        enddo
      enddo

c---- write the coordinate data in parallel to the queue
      call cgp_queue_set_f(1, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cgp_coord_write_data_f(F, B, Z, Cx, start, end, fx, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cgp_coord_write_data_f(F, B, Z, Cy, start, end, fy, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cgp_coord_write_data_f(F, B, Z, Cz, start, end, fz, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

c---- write out the queued coordinate data
      call cgp_queue_flush_f(ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cgp_queue_set_f(0, ierr)

c---- create data node for elements
      call cgp_section_write_f(F, B, Z, 'Hex', HEXA_8, 1, totelems,
     &                         0, E, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

c---- number of elements and range this process will write
      nelems = (totelems + commsize - 1) / commsize
      start  = nelems * commrank + 1
      end    = nelems * (commrank + 1)
      if (end .gt. totelems) end = totelems

c---- create the hex element data for this process
      nn = 0
      n  = 1
      do k=1,nperside-1
        do j=1,nperside-1
          do i=1,nperside-1
            if (n .ge. start .and. n .le. end) then
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
            endif
            n = n + 1
          enddo
        enddo
      enddo

c---- write the element connectivity in parallel
      call cgp_elements_write_data_f(F, B, Z, E, start, end, ie, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

c---- create a centered solution
      call cg_sol_write_f(F, B, Z, 'Solution', CellCenter, S, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cgp_field_write_f(F, B, Z, S, RealSingle, 'CellIndex',
     &                       Fs, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

c---- create the field data for this process
      nn = 1
      do n=1, totelems
        if (n .ge. start .and. n .le. end) then
          fd(nn) = n
          nn = nn + 1
        endif
      enddo

c---- write the solution field data in parallel
      call cgp_field_write_data_f(F, B, Z, S, Fs, start, end, fd, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

c---- create user data under the zone and duplicate solution data
      call cg_goto_f(F, B, ierr, 'Zone_t', 1, 'end')
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cg_user_data_write_f('User Data', ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cg_gorel_f(F, ierr, 'User Data', 0, 'end')
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call cgp_array_write_f('CellIndex', RealSingle, 1, totelems,
     &                       A, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

c---- write the array data in parallel
      call cgp_array_write_data_f(A, start, end, fd, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

c---- close the file and terminate MPI
      call cgp_close_f(F, ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
      call MPI_FINALIZE(ierr)
      end

