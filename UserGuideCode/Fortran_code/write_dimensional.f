      program write_dimensional
c
c   Opens an existing CGNS file that contains a simple 3-D 
c   grid plus a flow solution and adds its dimensionality 
c   (dimensional data).
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (for example, created using 
c       write_grid_str.f followed by write_flowcent_str.f or
c       write_grid_str.f followed by write_flowvert_str.f or
c       write_grid_str.f followed by write_flowcentrind_str.f or
c       write_grid_unst.f followed by write_flowvert_unst.f)
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c write_dimensional.f
c   ifort -o write_dimensional write_dimensional.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c   Note Windows machines also need to include cgnswin_f.h
c
      dimension exponents(5)
      character fieldname*32
c
c   WRITE DIMENSIONAL INFO FOR GRID AND FLOW SOLN
c   open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   we know there is only one zone (real working code would check!)
      index_zone=1
c   we know there is only one FlowSolution_t (real working code would check!)
      index_flow=1
c   we know there is only one GridCoordinates_t (real working code would check!)
      index_grid=1
c   put DataClass and DimensionalUnits under Base
      call cg_goto_f(index_file,index_base,ier,'end')
      call cg_dataclass_write_f(Dimensional,ier)
      call cg_units_write_f(Kilogram,Meter,Second,Kelvin,Degree,ier)
c   read fields
      call cg_nfields_f(index_file,index_base,index_zone,index_flow,
     +  nfields,ier) 
      if (nfields .ne. 2) then
        write(6,'('' Error! expecting 2 fields, read '',i5)') nfields
        stop
      end if
      do iff=1,nfields
        call cg_field_info_f(index_file,index_base,index_zone,
     +    index_flow,iff,idatatype,fieldname,ier)
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
          write(6,'('' Error! this fieldname not expected: '',a32)')
     +      fieldname
          stop
        end if
c   write DimensionalExponents
        call cg_goto_f(index_file,index_base,ier,'Zone_t',1,
     +   'FlowSolution_t',1,'DataArray_t',iff,'end')
        call cg_exponents_write_f(RealSingle,exponents,ier)
      enddo
c   read grid
      call cg_ncoords_f(index_file,index_base,index_zone,ncoords,ier)
      exponents(1)=0.
      exponents(2)=1.
      exponents(3)=0.
      exponents(4)=0.
      exponents(5)=0.
      do icc=1,ncoords
c   write DimensionalExponents
        call cg_goto_f(index_file,index_base,ier,'Zone_t',1,
     +   'GridCoordinates_t',1,'DataArray_t',icc,'end')
        call cg_exponents_write_f(RealSingle,exponents,ier)
      enddo
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote dimensional data to file'',
     + '' grid.cgns'')')
      stop
      end
