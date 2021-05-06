      program write_dimensional
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   grid plus a flow solution and adds its dimensionality
!   (dimensional data).
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (for example, created using
!       write_grid_str.f followed by write_flowcent_str.f or
!       write_grid_str.f followed by write_flowvert_str.f or
!       write_grid_str.f followed by write_flowcentrind_str.f or
!       write_grid_unst.f followed by write_flowvert_unst.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_dimensional.F90
!   ifort -o write_dimensional write_dimensional.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*4 exponents(5)
      integer icc,iff,nfields,index_grid,index_flow,index_zone
      integer index_base,index_file,ier,ncoords,idatatype
      character fieldname*32
!
      write(6,'('' Program write_dimensional'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...compiled in 64-bit mode, but not needed'')')
      end if
!
!   WRITE DIMENSIONAL INFO FOR GRID AND FLOW SOLN
!   open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   we know there is only one FlowSolution_t (real working code would check!)
      index_flow=1
!   we know there is only one GridCoordinates_t (real working code would check!)
      index_grid=1
!   put DataClass and DimensionalUnits under Base
      call cg_goto_f(index_file,index_base,ier,'end')
      call cg_dataclass_write_f(CGNS_ENUMV(Dimensional),ier)
      call cg_units_write_f(CGNS_ENUMV(Kilogram),CGNS_ENUMV(Meter),CGNS_ENUMV(Second),CGNS_ENUMV(Kelvin),CGNS_ENUMV(Degree),ier)
!   read fields
      call cg_nfields_f(index_file,index_base,index_zone,index_flow,           &
           nfields,ier)
      if (nfields .ne. 2) then
        write(6,'('' Error! expecting 2 fields, read '',i5)') nfields
        stop
      end if
      do iff=1,nfields
        call cg_field_info_f(index_file,index_base,index_zone,                 &
             index_flow,iff,idatatype,fieldname,ier)
        write(6,'('' fieldname='',a32)') fieldname
        if (fieldname .eq. 'Density') then
          exponents(1)=1.
          exponents(2)=-3.
          exponents(3)=0.
          exponents(4)=0.
          exponents(5)=0.
        else if (fieldname .eq. 'Pressure') then
          exponents(1)=1.
          exponents(2)=-1.
          exponents(3)=-2.
          exponents(4)=0.
          exponents(5)=0.
        else
          write(6,'('' Error! this fieldname not expected: '',a32)') fieldname
          stop
        end if
!   write DimensionalExponents
        call cg_goto_f(index_file,index_base,ier,'Zone_t',1,                   &
             'FlowSolution_t',1,'DataArray_t',iff,'end')
        call cg_exponents_write_f(CGNS_ENUMV(RealSingle),exponents,ier)
      enddo
!   read grid
      call cg_ncoords_f(index_file,index_base,index_zone,ncoords,ier)
      exponents(1)=0.
      exponents(2)=1.
      exponents(3)=0.
      exponents(4)=0.
      exponents(5)=0.
      do icc=1,ncoords
!   write DimensionalExponents
        call cg_goto_f(index_file,index_base,ier,'Zone_t',1,                   &
             'GridCoordinates_t',1,'DataArray_t',icc,'end')
        call cg_exponents_write_f(CGNS_ENUMV(RealSingle),exponents,ier)
      enddo
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote dimensional data to file'',              &
       '' grid.cgns'')')
      stop
      end
