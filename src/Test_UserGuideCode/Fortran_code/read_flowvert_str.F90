      program read_flowvert_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid plus a flow solution (at VERTICES),
!   and reads it.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_str.f followed by
!   write_flowvert_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_flowvert_str.F90
!   ifort -o read_flowvert_str read_flowvert_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
!   dimension statements (note that tri-dimensional arrays
!   r and p must be dimensioned exactly as (21,17,N) (N>=9)
!   for this particular case or else they will be read from
!   the CGNS file incorrectly!  Other options are to use 1-D
!   arrays, use dynamic memory, or pass index values to a
!   subroutine and dimension exactly there):
      real*4 r(21,17,9),p(21,17,9)
      integer(cgsize_t) isize(3,3),irmin(3),irmax(3)
      integer loc,index_flow,index_zone,index_base,index_file,ier
      character*32 zonename,solname
!
!   READ FLOW SOLUTION FROM CGNS FILE
!   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   we know there is only one FlowSolution_t (real working code would check!)
      index_flow=1
!   get zone size (and name - although not needed here)
      call cg_zone_read_f(index_file,index_base,index_zone,zonename,isize,ier)
!   lower range index
      irmin(1)=1
      irmin(2)=1
      irmin(3)=1
!   upper range index - use vertex dimensions
!   checking GridLocation first (real working code would check
!   to make sure there are no Rind cells also!):
      call cg_sol_info_f(index_file,index_base,index_zone,index_flow,          &
           solname,loc,ier)
      if (loc .ne. CGNS_ENUMV(Vertex)) then
        write(6,'('' Error, GridLocation must be Vertex!  Currently:'',        &
         a32)') GridLocationName(loc)
        stop
      end if
      irmax(1)=isize(1,1)
      irmax(2)=isize(2,1)
      irmax(3)=isize(3,1)
!   read flow solution
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,        &
           'Density',CGNS_ENUMV(RealSingle),irmin,irmax,r,ier)
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,        &
           'Pressure',CGNS_ENUMV(RealSingle),irmin,irmax,p,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read flow solution from file'',                &
       '' grid.cgns'')')
      write(6,'(''   For example, r,p(21,17,9)='',2f12.5)')                    &
       r(21,17,9),p(21,17,9)
      write(6,'('' Program successful... ending now'')')
      stop
      end
