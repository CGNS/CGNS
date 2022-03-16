      program write_flowvert_unst
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   unstructured grid, and adds a flow solution (at VERTICES)
!   to it.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_unst.f)
!   Note that, other than the dimensions of the variables
!   r and p, this program is essentially identical to that for
!   writing flow solutions to a structured zone:
!   write_flowvert_str.f
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_flowvert_unst.F90
!   ifort -o write_flowvert_unst write_flowvert_unst.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*8 r(21*17*9),p(21*17*9)
      integer index_field,index_flow,index_zone,index_base,ier
      integer index_file,i,j,k,ni,nj,nk,iset
      character solname*32
!
      write(6,'('' Program write_flowvert_unst'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...compiled in 64-bit mode, but not needed'')')
      end if
!
!   create fake flow solution AT VERTICES for simple example:
      ni=21
      nj=17
      nk=9
      iset=0
      do k=1,nk
        do j=1,nj
          do i=1,ni
            iset=iset+1
            r(iset)=float(i-1)
            p(iset)=float(j-1)
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
!   create flow solution node
      call cg_sol_write_f(index_file,index_base,index_zone,solname,            &
           CGNS_ENUMV(Vertex),index_flow,ier)
!   write flow solution (user must use SIDS-standard names here)
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,       &
           CGNS_ENUMV(RealDouble),'Density',r,index_field,ier)
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,       &
           CGNS_ENUMV(RealDouble),'Pressure',p,index_field,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added flow solution data to file'',            &
       '' grid.cgns (unstructured)'')')
      write(6,'(''   Note:  if the original CGNS file already had'',           &
       '' a FlowSolution_t node,'')')
      write(6,'(''          it has been overwritten'')')
      stop
      end
