      program read_dimensional
      use cgns
      implicit none
!
!   Opens an existing CGNS file that contains a simple 3-D
!   grid plus a flow solution WITH DIMENSIONALITY, and reads
!   the dimensionality.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (for example, created using
!       write_grid_str.f followed by write_flowcent_str.f or
!       write_grid_str.f followed by write_flowvert_str.f or
!       write_grid_str.f followed by write_flowcentrind_str.f or
!       write_grid_unst.f followed by write_flowvert_unst.f
!   followed by write_dimensional.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_dimensional.F90
!   ifort -o read_dimensional read_dimensional.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*4 exponents(5)
      integer ic,ncoords,idatatype,iff,nfields,ia,ix,it,il,im,id
      integer index_grid,index_flow,index_zone,index_base,index_file,ier
      character fieldname*32,coordname*32
!
!   READ DIMENSIONAL INFO FOR GRID AND FLOW SOLN
!   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   we know there is only one FlowSolution_t (real working code would check!)
      index_flow=1
!   we know there is only one GridCoordinates_t (real working code would check!)
      index_grid=1
!   read DataClass and DimensionalUnits under Base
      call cg_goto_f(index_file,index_base,ier,'end')
      call cg_dataclass_read_f(id,ier)
      write(6,'('' DataClass = '',a32)') DataClassName(id)
      if (DataClassName(id) .ne. 'Dimensional') then
        write(6,'('' Error!  Expecting Dimensional'')')
        stop
      end if
      call cg_units_read_f(im,il,it,ix,ia,ier)
      write(6,'('' Units='',5(/,8x,a32))') MassUnitsName(im),                  &
       LengthUnitsName(il),TimeUnitsName(it),                                  &
       TemperatureUnitsName(ix),AngleUnitsName(ia)
!   read fields
      call cg_nfields_f(index_file,index_base,index_zone,index_flow,           &
           nfields,ier)
      if (nfields .ne. 2) then
        write(6,'('' Error! expecting 2 fields, read '',i5)') nfields
        stop
      end if
      do iff=1,nfields
!   read DimensionalExponents
        call cg_goto_f(index_file,index_base,ier,'Zone_t',1,                   &
             'FlowSolution_t',1,'DataArray_t',iff,'end')
        call cg_exponents_read_f(exponents,ier)
!   get field name
        call cg_field_info_f(index_file,index_base,index_zone,                 &
             index_flow,iff,idatatype,fieldname,ier)
        write(6,'('' For '',a32,'', exponents are:'',5(/,6x,f5.1))')           &
          fieldname,exponents(1),exponents(2),exponents(3),exponents(4),       &
          exponents(5)
      enddo
!   read grid
      call cg_ncoords_f(index_file,index_base,index_zone,ncoords,ier)
      do ic=1,ncoords
!   read DimensionalExponents
        call cg_goto_f(index_file,index_base,ier,'Zone_t',1,                   &
             'GridCoordinates_t',1,'DataArray_t',ic,'end')
        call cg_exponents_read_f(exponents,ier)
!   get coord name
        call cg_coord_info_f(index_file,index_base,index_zone,                 &
         ic,idatatype,coordname,ier)
        write(6,'('' For '',a32,'', exponents are:'',5(/,6x,f5.1))')           &
       coordname,exponents(1),exponents(2),exponents(3),exponents(4),          &
          exponents(5)
      enddo
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read dimensional data from file'',             &
       '' grid.cgns'')')
      stop
      end
