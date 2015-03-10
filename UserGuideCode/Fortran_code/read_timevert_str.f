      program read_timevert_str
c
c   Opens an existing CGNS file that contains a simple 3-D 
c   structured grid plus 3 different flow solutions (at VERTICES),
c   along with time-accurate info, and reads it.
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid_str.f followed by
c   write_timevert_str.f)
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c read_timevert_str.f
c   ifort -o read_timevert_str read_timevert_str.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c   Note Windows machines also need to include cgnswin_f.h
c
c   dimension statements (note that tri-dimensional arrays
c   r1, r2, r3 and p1, p2, p3
c   must be dimensioned exactly as (21,17,N) (N>=9)
c   for this particular case or else they will be written to
c   the CGNS file incorrectly!  Other options are to use 1-D
c   arrays, use dynamic memory, or pass index values to a
c   subroutine and dimension exactly there):
      dimension r1(21,17,9),p1(21,17,9)
      dimension r2(21,17,9),p2(21,17,9)
      dimension r3(21,17,9),p3(21,17,9)
      dimension isize(3,3),irmin(3),irmax(3)
      dimension time(3)
      character zonename*32,bitername*32,zitername*32
      character arrayname*32
      character solname(3)*32,solname2*32
      integer idims(2)
c
c   READ FLOW SOLUTION FROM CGNS FILE
c   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   we know there is only one zone (real working code would check!)
      index_zone=1
c   get zone size (and name - although not needed here)
      call cg_zone_read_f(index_file,index_base,index_zone,zonename,
     + isize,ier)
c   lower range index
      irmin(1)=1
      irmin(2)=1
      irmin(3)=1
c   upper range index - use vertex dimensions
      irmax(1)=isize(1,1)
      irmax(2)=isize(2,1)
      irmax(3)=isize(3,1)
c   read BaseIterativeData
      call cg_biter_read_f(index_file,index_base,bitername,nsteps,
     + ier)
      write(6,'('' number of time steps stored = '',i5)') nsteps
      if (nsteps .ne. 3) then
        write(6,'('' Error, expecting nsteps=3!'')')
        stop
      end if
      call cg_goto_f(index_file,index_base,ier,'BaseIterativeData_t',
     + 1,'end')
      call cg_narrays_f(narrays,ier)
      if (narrays .ne. 1) then
        write(6,'('' Error, expecting 1 array in BaseIterativeData'',
     +   ''... there are '',i5)') narrays
        stop
      end if
      call cg_array_info_f(1,arrayname,idatatype,id1,id2,ier)
      if (id1 .ne. 1 .or. id2 .ne. 3) then
        write(6,'('' Error, expecting data dimension and vector to'',
     +   '' be 1 and 3 in BaseIterativeData... read '',2i5)') id1,
     +   id2
        stop
      end if
      call cg_array_read_as_f(1,RealSingle,time,ier)
      write(6,'('' Times stored are:'')')
      do n=1,nsteps
        write(6,'(f12.3)') time(n)
      enddo
c   read ZoneIterativeData
      call cg_ziter_read_f(index_file,index_base,index_zone,zitername,
     + ier)
      call cg_goto_f(index_file,index_base,ier,'Zone_t',
     + index_zone,'ZoneIterativeData_t',1,'end')
      call cg_narrays_f(narrays,ier)
      if (narrays .ne. 1) then
        write(6,'('' Error, expecting 1 array in ZoneIterativeData'',
     +   ''... there are '',i5)') narrays
        stop
      end if
      call cg_array_info_f(1,arrayname,idatatype,id1,idims,ier)
      if (id1 .ne. 2 .or. idims(1) .ne. 32) then
        write(6,'('' Error, expecting data dimension and vector to'',
     +   '' be 2 and 32 in ZoneIterativeData... read '',2i5)') id1,
     +   idims(1)
        stop
      end if
      call cg_array_read_as_f(1,Character,solname,ier)
      write(6,'('' Flow solution names corresponding to each are:'')')
      do n=1,nsteps
        write(6,'(a32)') solname(n)
      enddo
c   read SimulationType
      call cg_simulation_type_read_f(index_file,index_base,
     + isim,ier)
      write(6,'('' Simulation type is: '',a32)') 
     + SimulationTypeName(isim)
c   do loop to read flow solutions
      do n=1,nsteps
c   check that soln names match, and also check GridLocation (real 
c   working code would check to make sure there are no Rind cells 
c   also!):
      call cg_sol_info_f(index_file,index_base,index_zone,n,
     + solname2,loc,ier)
      if (solname2 .ne. solname(n)) then
        write(6,'('' Error, soln names do not match'')')
        stop
      end if
      if (loc .ne. Vertex) then
        write(6,'('' Error, GridLocation must be Vertex!  Currently:'',
     +   a32)') GridLocationName(loc)
        stop
      end if
      if (n .eq. 1) then
      call cg_field_read_f(index_file,index_base,index_zone,n,
     + 'Density',RealSingle,irmin,irmax,r1,ier)
      call cg_field_read_f(index_file,index_base,index_zone,n,
     + 'Pressure',RealSingle,irmin,irmax,p1,ier)
      else if (n .eq. 2) then
      call cg_field_read_f(index_file,index_base,index_zone,n,
     + 'Density',RealSingle,irmin,irmax,r2,ier)
      call cg_field_read_f(index_file,index_base,index_zone,n,
     + 'Pressure',RealSingle,irmin,irmax,p2,ier)
      else
      call cg_field_read_f(index_file,index_base,index_zone,n,
     + 'Density',RealSingle,irmin,irmax,r3,ier)
      call cg_field_read_f(index_file,index_base,index_zone,n,
     + 'Pressure',RealSingle,irmin,irmax,p3,ier)
      end if
      enddo
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read 3 flow solutions from file'',
     + '' grid.cgns'')')
      write(6,'(''   For example, r1,p1(1,1,1)='',2f12.5)')
     + r1(1,1,1),p1(1,1,1)
      write(6,'(''                r2,p2(1,1,1)='',2f12.5)')
     + r2(1,1,1),p2(1,1,1)
      write(6,'(''                r3,p3(1,1,1)='',2f12.5)')
     + r3(1,1,1),p3(1,1,1)
      write(6,'(''   For example, r1,p1(21,17,9)='',2f12.5)')
     + r1(21,17,9),p1(21,17,9)
      write(6,'(''                r2,p2(21,17,9)='',2f12.5)')
     + r2(21,17,9),p2(21,17,9)
      write(6,'(''                r3,p3(21,17,9)='',2f12.5)')
     + r3(21,17,9),p3(21,17,9)
c
      write(6,'('' Program successful... ending now'')')
      stop
      end
