      program write_grid_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Creates simple 3-D structured grid and writes it to a
!   CGNS file.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_grid_str.F90
!   ifort -o write_grid_str write_grid_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!
!   dimension statements (note that tri-dimensional arrays
!   x,y,z must be dimensioned exactly as (21,17,N) (N>=9)
!   for this particular case or else they will be written to
!   the CGNS file incorrectly!  Other options are to use 1-D
!   arrays, use dynamic memory, or pass index values to a
!   subroutine and dimension exactly there):
      real*8 x(21,17,9),y(21,17,9),z(21,17,9)
      integer ni,nj,nk,i,j,k
      integer icelldim,iphysdim
      integer index_coord,index_file,ier
      integer index_base,index_zone
      integer(cgsize_t) isize(3,3)
      character basename*32,zonename*32
!
      write(6,'('' Program write_grid_str'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
!
!   create gridpoints for simple example:
      ni=21
      nj=17
      nk=9
      do k=1,nk
        do j=1,nj
          do i=1,ni
            x(i,j,k)=float(i-1)
            y(i,j,k)=float(j-1)
            z(i,j,k)=float(k-1)
          enddo
        enddo
      enddo
      write(6,'('' created simple 3-D grid points'')')
!
!   WRITE X, Y, Z GRID POINTS TO CGNS FILE
!   open CGNS file for write
      call cg_open_f('grid.cgns',CG_MODE_WRITE,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   create base (user can give any name)
      basename='Base'
      icelldim=3
      iphysdim=3
      call cg_base_write_f(index_file,basename,icelldim,iphysdim,              &
           index_base,ier)
!   define zone name (user can give any name)
      zonename = 'Zone  1'
!   vertex size
      isize(1,1)=21
      isize(2,1)=17
      isize(3,1)=9
!   cell size
      isize(1,2)=isize(1,1)-1
      isize(2,2)=isize(2,1)-1
      isize(3,2)=isize(3,1)-1
!   boundary vertex size (always zero for structured grids)
      isize(1,3)=0
      isize(2,3)=0
      isize(3,3)=0
!   create zone
      call cg_zone_write_f(index_file,index_base,zonename,isize,               &
           CGNS_ENUMV(Structured),index_zone,ier)
!   write grid coordinates (user must use SIDS-standard names here)
      call cg_coord_write_f(index_file,index_base,index_zone,CGNS_ENUMV(RealDouble),       &
           'CoordinateX',x,index_coord,ier)
      call cg_coord_write_f(index_file,index_base,index_zone,CGNS_ENUMV(RealDouble),       &
           'CoordinateY',y,index_coord,ier)
      call cg_coord_write_f(index_file,index_base,index_zone,CGNS_ENUMV(RealDouble),       &
           'CoordinateZ',z,index_coord,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote grid to file grid.cgns'')')
      stop
      end
