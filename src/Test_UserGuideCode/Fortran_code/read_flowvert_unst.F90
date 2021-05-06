      program read_flowvert_unst
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   unstructured grid plus a flow solution (at VERTICES),
!   and reads it.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_unst.f followed by
!   write_flowvert_unst.f)
!   Note that, other than the dimensions of the variables
!   r and p, this program is very similar to that for
!   reading flow solutions from a structured zone:
!   read_flowvert_str.f
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_flowvert_unst.F90
!   ifort -o read_flowvert_unst read_flowvert_unst.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*4 r(21*17*9),p(21*17*9)
      integer(cgsize_t) isize(1,3),irmin,irmax
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
      irmin=1
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
      irmax=isize(1,1)
!   read flow solution
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,        &
           'Density',CGNS_ENUMV(RealSingle),irmin,irmax,r,ier)
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,        &
           'Pressure',CGNS_ENUMV(RealSingle),irmin,irmax,p,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read flow solution from file'',                &
       '' grid.cgns'')')
      write(6,'(''   For example, r,p(380) ='',2f12.5)')                       &
       r(380),p(380)
      write(6,'(''                r,p(3213)='',2f12.5)')                       &
       r(3213),p(3213)
      write(6,'('' Program successful... ending now'')')
      stop
      end
