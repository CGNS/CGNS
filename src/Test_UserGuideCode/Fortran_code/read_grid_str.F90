      program read_grid_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Reads simple 3-D structured grid from CGNS file
!   (companion program to write_grid_str.f)
!
!   The CGNS grid file 'grid.cgns' must already exist.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_grid_str.F90
!   ifort -o read_grid_str read_grid_str.o -L ../../lib -lcgns
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
      integer(cgsize_t) isize(3,3),irmin(3),irmax(3)
      integer index_zone,index_base,index_file,ier
      character zonename*32
!
!   READ X, Y, Z GRID POINTS FROM CGNS FILE
!   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   get zone size (and name - although not needed here)
      call cg_zone_read_f(index_file,index_base,index_zone,zonename,           &
           isize,ier)
!   lower range index
      irmin(1)=1
      irmin(2)=1
      irmin(3)=1
!   upper range index of vertices
      irmax(1)=isize(1,1)
      irmax(2)=isize(2,1)
      irmax(3)=isize(3,1)
!   read grid coordinates
      call cg_coord_read_f(index_file,index_base,index_zone,                   &
           'CoordinateX',CGNS_ENUMV(RealSingle),irmin,irmax,x,ier)
      call cg_coord_read_f(index_file,index_base,index_zone,                   &
           'CoordinateY',CGNS_ENUMV(RealSingle),irmin,irmax,y,ier)
      call cg_coord_read_f(index_file,index_base,index_zone,                   &
           'CoordinateZ',CGNS_ENUMV(RealSingle),irmin,irmax,z,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read grid from file grid.cgns'')')
      write(6,'(''   For example, x,y,z(21,17,9)='',3f12.5)')                  &
       x(21,17,9),y(21,17,9),z(21,17,9)
      write(6,'('' Program successful... ending now'')')
      stop
      end
