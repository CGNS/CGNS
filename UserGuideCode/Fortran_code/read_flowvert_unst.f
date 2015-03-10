      program read_flowvert_unst
c
c   Opens an existing CGNS file that contains a simple 3-D 
c   unstructured grid plus a flow solution (at VERTICES),
c   and reads it.
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid_unst.f followed by
c   write_flowvert_unst.f)
c   Note that, other than the dimensions of the variables
c   r and p, this program is very similar to that for 
c   reading flow solutions from a structured zone:  
c   read_flowvert_str.f
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c read_flowvert_unst.f
c   ifort -o read_flowvert_unst read_flowvert_unst.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c
      dimension r(21*17*9),p(21*17*9)
      dimension isize(1,3)
      character*32 zonename,solname
c
c   READ FLOW SOLUTION FROM CGNS FILE
c   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   we know there is only one zone (real working code would check!)
      index_zone=1
c   we know there is only one FlowSolution_t (real working code would check!)
      index_flow=1
c   get zone size (and name - although not needed here)
      call cg_zone_read_f(index_file,index_base,index_zone,zonename,
     + isize,ier)
c   lower range index
      irmin=1
c   upper range index - use vertex dimensions
c   checking GridLocation first (real working code would check
c   to make sure there are no Rind cells also!):
      call cg_sol_info_f(index_file,index_base,index_zone,index_flow,
     + solname,loc,ier)
      if (loc .ne. Vertex) then
        write(6,'('' Error, GridLocation must be Vertex!  Currently:'',
     +   a32)') GridLocationName(loc)
        stop
      end if
      irmax=isize(1,1)
c   read flow solution
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,
     + 'Density',RealSingle,irmin,irmax,r,ier)
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,
     + 'Pressure',RealSingle,irmin,irmax,p,ier)
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read flow solution from file'',
     + '' grid.cgns'')')
      write(6,'(''   For example, r,p(380) ='',2f12.5)')
     + r(380),p(380)
      write(6,'(''                r,p(3213)='',2f12.5)')
     + r(3213),p(3213)
      write(6,'('' Program successful... ending now'')')
      stop
      end
