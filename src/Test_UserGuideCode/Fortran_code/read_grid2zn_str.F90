      program read_grid2zn_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Reads simple 3-D structured 2-zone grid from CGNS file
!   (companion program to write_grid2zn_str.f)
!
!   The CGNS grid file 'grid.cgns' must already exist.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_grid2zn_str.F90
!   ifort -o read_grid2zn_str read_grid2zn_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
!   dimension statements (note that tri-dimensional arrays
!   x,y,z must be dimensioned exactly as (21,17,N) (N>=9)
!   for this particular case or else they will be read from
!   the CGNS file incorrectly!  Other options are to use 1-D
!   arrays, use dynamic memory, or pass index values to a
!   subroutine and dimension exactly there):
      real*4 x(21,17,9),y(21,17,9),z(21,17,9)
      real*4 xsav(21,17,9,2),ysav(21,17,9,2),zsav(21,17,9,2)
      integer(cgsize_t) isize(3,3),irmin(3),irmax(3)
      integer nzone,i,j,k,index_zone,index_base,index_file,ier
      character zonename*32
!
!   READ X, Y, Z GRID POINTS FROM CGNS FILE
!   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   get number of zones (should be 2 for our case)
      call cg_nzones_f(index_file,index_base,nzone,ier)
      if (nzone .ne. 2) then
         write(6,'('' Error.  This program expects 2 zones. '',i5,             &
          '' read'')') nzone
         stop
      end if
!   do loop over the zones
      do index_zone=1,nzone
!   get zone size (and name - although not needed here)
        call cg_zone_read_f(index_file,index_base,index_zone,                  &
             zonename,isize,ier)
!   lower range index
        irmin(1)=1
        irmin(2)=1
        irmin(3)=1
!   upper range index of vertices
        irmax(1)=isize(1,1)
        irmax(2)=isize(2,1)
        irmax(3)=isize(3,1)
!   read grid coordinates
        call cg_coord_read_f(index_file,index_base,index_zone,                 &
             'CoordinateX',CGNS_ENUMV(RealSingle),irmin,irmax,x,ier)
        call cg_coord_read_f(index_file,index_base,index_zone,                 &
             'CoordinateY',CGNS_ENUMV(RealSingle),irmin,irmax,y,ier)
        call cg_coord_read_f(index_file,index_base,index_zone,                 &
             'CoordinateZ',CGNS_ENUMV(RealSingle),irmin,irmax,z,ier)
!   store grid coordinates in xsav,ysav,zsav array:
        do i=1,isize(1,1)
          do j=1,isize(2,1)
            do k=1,isize(3,1)
              xsav(i,j,k,index_zone)=x(i,j,k)
              ysav(i,j,k,index_zone)=y(i,j,k)
              zsav(i,j,k,index_zone)=z(i,j,k)
            enddo
          enddo
        enddo
      enddo
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read grid from file grid.cgns'')')
      write(6,'(''   For example, zone 1 x,y,z(21,17,9)='',3f12.5)')           &
       xsav(21,17,9,1),ysav(21,17,9,1),zsav(21,17,9,1)
      write(6,'(''                zone 2 x,y,z(21,17,9)='',3f12.5)')           &
       xsav(21,17,9,2),ysav(21,17,9,2),zsav(21,17,9,2)
      write(6,'('' Program successful... ending now'')')
      stop
      end
