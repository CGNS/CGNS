      program write_flowcent_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid, and adds a flow solution (at CELL CENTERS)
!   to it.  (Compare this program with write_flowvert_str)
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_flowcent_str.F90
!   ifort -o write_flowcent_str write_flowcent_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
!   dimension statements (note that tri-dimensional arrays
!   r and p must be dimensioned exactly as (21-1,17-1,N-1) (N>=9)
!   for this particular case or else they will be written to
!   the CGNS file incorrectly!  Other options are to use 1-D
!   arrays, use dynamic memory, or pass index values to a
!   subroutine and dimension exactly there):
      real*8 r(20,16,8),p(20,16,8)
      integer index_field,index_flow,index_zone,index_base,index_file,ier
      integer i,j,k,ni,nj,nk
      character solname*32
!
      write(6,'('' Program write_flowcent_str'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...compiled in 64-bit mode, but not needed'')')
      end if
!
!   create fake flow solution AT CELL CENTERS for simple example:
      ni=20
      nj=16
      nk=8
      do k=1,nk
        do j=1,nj
          do i=1,ni
            r(i,j,k)=float(i-1)
            p(i,j,k)=float(j-1)
          enddo
        enddo
      enddo
      write(6,'('' created simple 3-D rho and p flow solution'')')
!
!   WRITE FLOW SOLUTION TO EXISTING CGNS FILE
!   open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   define flow solution node name (user can give any name)
      solname = 'FlowSolution'
!   create flow solution node (NOTE USE OF CellCenter HERE)
      call cg_sol_write_f(index_file,index_base,index_zone,solname,            &
           CGNS_ENUMV(CellCenter),index_flow,ier)
!   write flow solution (user must use SIDS-standard names here)
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,       &
           CGNS_ENUMV(RealDouble),'Density',r,index_field,ier)
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,       &
           CGNS_ENUMV(RealDouble),'Pressure',p,index_field,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added CellCenter flow solution data'',         &
       '' to file grid.cgns'')')
      write(6,'(''   Note:  if the original CGNS file already had'',           &
       '' a FlowSolution_t node,'')')
      write(6,'(''          it has been overwritten'')')
      stop
      end
