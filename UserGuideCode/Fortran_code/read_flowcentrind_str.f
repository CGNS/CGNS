      program read_flowcentrind_str
c
c   Opens an existing CGNS file that contains a simple 3-D 
c   structured grid plus a flow solution (at CELL CENTERS PLUS
c   RIND CELLS IN THE I AND J DIRECTIONS), and reads it.  
c   (Compare this program with read_flowcent_str)
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid_str.f followed by
c   write_flowcentrind_str.f)
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c read_flowcentrind_str.f
c   ifort -o read_flowcentrind_str read_flowcentrind_str.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c   Note Windows machines also need to include cgnswin_f.h
c
c   dimension statements (note that tri-dimensional arrays
c   r and p must be dimensioned exactly as (21-1+2,17-1+2,N-1) (N>=9) 
c   for this particular case or else they will be read from 
c   the CGNS file incorrectly!  Other options are to use 1-D 
c   arrays, use dynamic memory, or pass index values to a 
c   subroutine and dimension exactly there):
c   Rind cells are stored in array locations (i,1,k), (i,18,k), (1,j,k), (22,j,k)
      dimension r(22,18,8),p(22,18,8)
      dimension isize(3,3),irmin(3),irmax(3),irinddata(6)
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
c  go to position within tree at FlowSolution\_t node
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowSolution_t',index_flow,'end')
c  read rind data
      call cg_rind_read_f(irinddata,ier)
c   lower range index
      irmin(1)=1
      irmin(2)=1
      irmin(3)=1
c   upper range index - use cell dimensions and rind info
c   checking GridLocation first:
      call cg_sol_info_f(index_file,index_base,index_zone,index_flow,
     + solname,loc,ier)
      if (loc .ne. CellCenter) then
        write(6,'('' Error, GridLocation must be CellCenter!'',
     +   ''  Currently:'',a32)') GridLocationName(loc)
        stop
      end if
      irmax(1)=isize(1,2)+irinddata(1)+irinddata(2)
      irmax(2)=isize(2,2)+irinddata(3)+irinddata(4)
      irmax(3)=isize(3,2)+irinddata(5)+irinddata(6)
c   read flow solution
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,
     + 'Density',RealSingle,irmin,irmax,r,ier)
      call cg_field_read_f(index_file,index_base,index_zone,index_flow,
     + 'Pressure',RealSingle,irmin,irmax,p,ier)
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read flow solution from file'',
     + '' grid.cgns'')')
      write(6,'(''   For example, r,p(21,17,8)='',2f12.5)')
     + r(21,17,8),p(21,17,8)
      write(6,'(''          rind: r,p(1,17,8)='',2f12.5)')
     + r(1,17,8),p(1,17,8)
      write(6,'(''          rind: r,p(22,17,8)='',2f12.5)')
     + r(22,17,8),p(22,17,8)
      write(6,'('' Program successful... ending now'')')
      stop
      end
