      program read_flowcentrind_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid plus a flow solution (at CELL CENTERS PLUS
!   RIND CELLS IN THE I AND J DIRECTIONS), and reads it.
!   (Compare this program with read_flowcent_str)
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_str.f followed by
!   write_flowcentrind_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_flowcentrind_str.F90
!   ifort -o read_flowcentrind_str read_flowcentrind_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
!   dimension statements (note that tri-dimensional arrays
!   r and p must be dimensioned exactly as (21-1+2,17-1+2,N-1) (N>=9)
!   for this particular case or else they will be read from
!   the CGNS file incorrectly!  Other options are to use 1-D
!   arrays, use dynamic memory, or pass index values to a
!   subroutine and dimension exactly there):
!   Rind cells are stored in array locations (i,1,k), (i,18,k), (1,j,k), (22,j,k)
      real*4 r(22,18,8),p(22,18,8)
      integer(cgsize_t) isize(3,3),irmin(3),irmax(3)
      integer irinddata(6)
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
!  go to position within tree at FlowSolution\_t node
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowSolution_t',index_flow,'end')
!  read rind data
      call cg_rind_read_f(irinddata,ier)
!   lower range index
      irmin(1)=1-irinddata(1)
      irmin(2)=1-irinddata(3)
      irmin(3)=1-irinddata(5)
!   upper range index - use cell dimensions and rind info
!   checking GridLocation first:
      call cg_sol_info_f(index_file,index_base,index_zone,index_flow,          &
           solname,loc,ier)
      if (loc .ne.  CGNS_ENUMV(CellCenter)) then
        write(6,'('' Error, GridLocation must be CellCenter!'',                &
         ''  Currently:'',a32)') GridLocationName(loc)
        stop
      end if
      irmax(1)=isize(1,2)+irinddata(2)
      irmax(2)=isize(2,2)+irinddata(4)
      irmax(3)=isize(3,2)+irinddata(6)
!   read flow solution
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,        &
           'Density',CGNS_ENUMV(RealSingle),irmin,irmax,r,ier)
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,        &
           'Pressure',CGNS_ENUMV(RealSingle),irmin,irmax,p,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read flow solution from file'',                &
       '' grid.cgns'')')
      write(6,'(''   For example, r,p(21,17,8)='',2f12.5)')                    &
       r(21,17,8),p(21,17,8)
      write(6,'(''          rind: r,p(1,17,8)='',2f12.5)')                     &
       r(1,17,8),p(1,17,8)
      write(6,'(''          rind: r,p(22,17,8)='',2f12.5)')                    &
       r(22,17,8),p(22,17,8)
      write(6,'('' Program successful... ending now'')')
      stop
      end
