      program testgeneralrindf

#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      implicit none

      integer :: ierr, cgfile, cgbase, cgzone, cgcoord, cgfld
      integer :: cggrid, cgsol 
      integer :: celldim, physdim
      integer :: nn, np
      integer(cgsize_t) :: i, j, k, n, size(9)
      integer(cgsize_t) :: dims(3)
      integer(cgsize_t) :: rmin(3), rmax(3)
      integer(cgsize_t) :: mrmin(3), mrmax(3)

      integer :: rind(6)

      real*4 x(-1:7,-1:7,0:6), y(-1:7,-1:7,0:6), z(-1:7,-1:7,0:6)
      real*4 solution(-1:7,-1:7,0:6), fbuf(-1:7,-1:7,0:6)

      character*32 coordname(3)
      character*32 fieldname

      coordname(1) = 'CoordinateX'
      coordname(2) = 'CoordinateY'
      coordname(3) = 'CoordinateZ'

      fieldname = 'Density'

c open and write base
      celldim = 3
      physdim = 3 
      call cg_open_f('cggeneral_rind.cgns', CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_base_write_f(cgfile, 'Structured', celldim, physdim,
     &                     cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      print *,'writing structured base with rind'

c create some data
      rind(1) = 2
      rind(2) = 2
      rind(3) = 2
      rind(4) = 2
      rind(5) = 1
      rind(6) = 1
      do n=1,3
        size(n)   = 5
        size(n+3) = 4
        size(n+6) = 0
      enddo

      n = 0
      do k=0,6
        do j=-1,7
          do i=-1,7
            n = n + 1
            x(i,j,k) = i - rind(1)
            y(i,j,k) = j - rind(3)
            z(i,j,k) = k - rind(5)
            solution(i,j,k) = n
          enddo
        enddo
      enddo
      
c
      dims(1) = rind(1) + rind(2) + size(1)
      dims(2) = rind(3) + rind(4) + size(2)
      dims(3) = rind(5) + rind(6) + size(3)
c
      do n=1,3
        rmin(n)  = 1 - rind(2*n-1)
        rmax(n)  = size(n) + rind(2*n)
        mrmin(n) = 1
        mrmax(n) = dims(n)
      enddo

c write zone
      call cg_zone_write_f(cgfile, cgbase, 'Zone', size,             
     &                     Structured, cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f


      call cg_grid_write_f(cgfile, cgbase, cgzone, 'GridCoordinates',
     &          cggrid, ierr)
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone,
     &               'GridCoordinates_t', cggrid, 'end')
      call cg_rind_write_f(rind, ierr)
c write coordinates
      call cg_coord_general_write_f(cgfile, cgbase, cgzone,
     &                      Realsingle, coordname(1),
     &                      rmin, rmax, 3, dims, mrmin, mrmax,
     &                      x, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_general_write_f(cgfile, cgbase, cgzone,
     &                      RealSingle, coordname(2),
     &                      rmin, rmax, 3, dims, mrmin, mrmax,
     &                      y, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_general_write_f(cgfile, cgbase, cgzone,
     &                      RealSingle, coordname(3),
     &                      rmin, rmax, 3, dims, mrmin, mrmax,
     &                      z, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

c write solution
      call cg_sol_write_f(cgfile, cgbase, cgzone, 'VertexSolution',
     &                              Vertex, cgsol, ierr)
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone,
     &               'FlowSolution_t', cgsol, 'end')
      call cg_rind_write_f(rind, ierr)
      call cg_field_general_write_f(cgfile, cgbase, cgzone, cgsol,
     &                  RealSingle, fieldname, rmin, rmax,
     &                  3, dims, mrmin, mrmax, solution, cgfld, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
c close the file and reopen in read mode
      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      print *,'closing and reopening in read mode '     

c read file and check the data
      call cg_open_f('cggeneral_rind.cgns', CG_MODE_READ, cgfile, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f

      write(6,*)'checking the data '
      cgbase = 1
      cgzone = 1
      cggrid = 1
      cgsol  = 1

      nn = 0
c check coordinates
c only load core coordinates without rind but inside memory with rind 
      do n=1,3
        rmin(n) = 1
        rmax(n) = size(n)
        mrmin(n) = 1 + rind(2*n -1)
        mrmax(n) = rind(2*n-1) + size(n)
      enddo

      call cg_coord_general_read_f(cgfile, cgbase, cgzone,
     &     'CoordinateX', RealSingle, rmin, rmax, 3,
     &     dims, mrmin, mrmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k=1,size(3)
        do j=1,size(2)
          do i=1,size(1)
            if (fbuf(i,j,k) .NE. x(i,j,k)) then
              np = np + 1
            endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .NE. 0) then
        print *,'differences in CoordinateX'
      endif

      call cg_coord_general_read_f(cgfile, cgbase, cgzone,
     &     'CoordinateY', RealSingle, rmin, rmax, 3,
     &     dims, mrmin, mrmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k=1,size(3)
        do j=1,size(2)
          do i=1,size(1)
            if (fbuf(i,j,k) .NE. y(i,j,k)) then
              np = np + 1
            endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .NE. 0) then
        print *,'differences in CoordinateY'
      endif

      call cg_coord_general_read_f(cgfile, cgbase, cgzone,
     &     'CoordinateZ', RealSingle, rmin, rmax, 3,
     &     dims, mrmin, mrmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k=1,size(3)
        do j=1,size(2)
          do i=1,size(1)
            if (fbuf(i,j,k) .NE. z(i,j,k)) then
              np = np + 1
            endif
          enddo
        enddo
      enddo
      nn = nn + np
      if (np .NE. 0) then
        print *,'differences in CoordinateZ'
      endif

c check field with only one rind layer
      do n=1,3
        rmin(n)  = 0
        rmax(n)  = size(n) + 1
        mrmin(n) = rind(2*n-1)
        mrmax(n) = rind(2*n-1) + size(n) + 1
      enddo

      call cg_field_general_read_f(cgfile, cgbase, cgzone, cgsol,
     &     'Density', RealSingle, rmin, rmax, 3,
     &      dims, mrmin, mrmax, fbuf, ierr)
      if (ierr .eq. ERROR) call cg_error_exit_f
      np = 0
      do k=0,size(3)+1
        do j=0,size(2)+1
          do i=0,size(1)+1
            if (fbuf(i,j,k) .ne. solution(i,j,k)) then
              np = np + 1
            endif
          enddo
        enddo
      enddo

      nn = nn + np
      if (np .ne. 0) then
        print *,'differences in Field'
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

      end program
