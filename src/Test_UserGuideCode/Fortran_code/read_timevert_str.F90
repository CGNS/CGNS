      program read_timevert_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid plus 3 different flow solutions (at VERTICES),
!   along with time-accurate info, and reads it.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_str.f followed by
!   write_timevert_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_timevert_str.F90
!   ifort -o read_timevert_str read_timevert_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
!   dimension statements (note that tri-dimensional arrays
!   r1, r2, r3 and p1, p2, p3
!   must be dimensioned exactly as (21,17,N) (N>=9)
!   for this particular case or else they will be written to
!   the CGNS file incorrectly!  Other options are to use 1-D
!   arrays, use dynamic memory, or pass index values to a
!   subroutine and dimension exactly there):
      real*4 r1(21,17,9),p1(21,17,9)
      real*4 r2(21,17,9),p2(21,17,9)
      real*4 r3(21,17,9),p3(21,17,9)
      real*4 time(3)
      integer(cgsize_t) isize(3,3),irmin(3),irmax(3)
      integer(cgsize_t) idims(2),id2(1)
      integer n,idatatype,id1,nsteps,narrays,isim,loc
      integer index_zone,index_base,index_file,ier
      character zonename*32,bitername*32,zitername*32
      character arrayname*32
      character solname(3)*32,solname2*32
!
!   READ FLOW SOLUTION FROM CGNS FILE
!   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   get zone size (and name - although not needed here)
      call cg_zone_read_f(index_file,index_base,index_zone,zonename,           &
           isize,ier)
!   lower range index
      irmin(1)=1
      irmin(2)=1
      irmin(3)=1
!   upper range index - use vertex dimensions
      irmax(1)=isize(1,1)
      irmax(2)=isize(2,1)
      irmax(3)=isize(3,1)
!   read BaseIterativeData
      call cg_biter_read_f(index_file,index_base,bitername,nsteps,ier)
      write(6,'('' number of time steps stored = '',i5)') nsteps
      if (nsteps .ne. 3) then
        write(6,'('' Error, expecting nsteps=3!'')')
        stop
      end if
      call cg_goto_f(index_file,index_base,ier,'BaseIterativeData_t',1,'end')
      call cg_narrays_f(narrays,ier)
      if (narrays .ne. 1) then
        write(6,'('' Error, expecting 1 array in BaseIterativeData'',          &
         ''... there are '',i5)') narrays
        stop
      end if
      call cg_array_info_f(1,arrayname,idatatype,id1,id2,ier)
      if (id1 .ne. 1 .or. id2(1) .ne. 3) then
        write(6,'('' Error, expecting data dimension and vector to'',          &
         '' be 1 and 3 in BaseIterativeData... read '',2i5)') id1,id2(1)
        stop
      end if
      call cg_array_read_as_f(1,CGNS_ENUMV(RealSingle),time,ier)
      write(6,'('' Times stored are:'')')
      do n=1,nsteps
        write(6,'(f12.3)') time(n)
      enddo
!   read ZoneIterativeData
      call cg_ziter_read_f(index_file,index_base,index_zone,zitername,ier)
      call cg_goto_f(index_file,index_base,ier,'Zone_t',                       &
           index_zone,'ZoneIterativeData_t',1,'end')
      call cg_narrays_f(narrays,ier)
      if (narrays .ne. 1) then
        write(6,'('' Error, expecting 1 array in ZoneIterativeData'',          &
         ''... there are '',i5)') narrays
        stop
      end if
      call cg_array_info_f(1,arrayname,idatatype,id1,idims,ier)
      if (id1 .ne. 2 .or. idims(1) .ne. 32) then
        write(6,'('' Error, expecting data dimension and vector to'',          &
         '' be 2 and 32 in ZoneIterativeData... read '',2i5)') id1,            &
         idims(1)
        stop
      end if
      call cg_array_read_as_f(1,CGNS_ENUMV(Character),solname,ier)
      write(6,'('' Flow solution names corresponding to each are:'')')
      do n=1,nsteps
        write(6,'(a32)') solname(n)
      enddo
!   read SimulationType
      call cg_simulation_type_read_f(index_file,index_base,isim,ier)
      write(6,'('' Simulation type is: '',a32)') SimulationTypeName(isim)
!   do loop to read flow solutions
      do n=1,nsteps
!   check that soln names match, and also check GridLocation (real
!   working code would check to make sure there are no Rind cells
!   also!):
      call cg_sol_info_f(index_file,index_base,index_zone,n,                   &
           solname2,loc,ier)
      if (solname2 .ne. solname(n)) then
        write(6,'('' Error, soln names do not match'')')
        stop
      end if
      if (loc .ne. CGNS_ENUMV(Vertex)) then
        write(6,'('' Error, GridLocation must be Vertex!  Currently:'',        &
         a32)') GridLocationName(loc)
        stop
      end if
      if (n .eq. 1) then
      call cg_field_read_f(index_file,index_base,index_zone,n,                 &
           'Density',CGNS_ENUMV(RealSingle),irmin,irmax,r1,ier)
      call cg_field_read_f(index_file,index_base,index_zone,n,                 &
           'Pressure',CGNS_ENUMV(RealSingle),irmin,irmax,p1,ier)
      else if (n .eq. 2) then
      call cg_field_read_f(index_file,index_base,index_zone,n,                 &
           'Density',CGNS_ENUMV(RealSingle),irmin,irmax,r2,ier)
      call cg_field_read_f(index_file,index_base,index_zone,n,                 &
           'Pressure',CGNS_ENUMV(RealSingle),irmin,irmax,p2,ier)
      else
      call cg_field_read_f(index_file,index_base,index_zone,n,                 &
           'Density',CGNS_ENUMV(RealSingle),irmin,irmax,r3,ier)
      call cg_field_read_f(index_file,index_base,index_zone,n,                 &
           'Pressure',CGNS_ENUMV(RealSingle),irmin,irmax,p3,ier)
      end if
      enddo
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read 3 flow solutions from file'',             &
       '' grid.cgns'')')
      write(6,'(''   For example, r1,p1(1,1,1)='',2f12.5)')                    &
       r1(1,1,1),p1(1,1,1)
      write(6,'(''                r2,p2(1,1,1)='',2f12.5)')                    &
       r2(1,1,1),p2(1,1,1)
      write(6,'(''                r3,p3(1,1,1)='',2f12.5)')                    &
       r3(1,1,1),p3(1,1,1)
      write(6,'(''   For example, r1,p1(21,17,9)='',2f12.5)')                  &
       r1(21,17,9),p1(21,17,9)
      write(6,'(''                r2,p2(21,17,9)='',2f12.5)')                  &
       r2(21,17,9),p2(21,17,9)
      write(6,'(''                r3,p3(21,17,9)='',2f12.5)')                  &
       r3(21,17,9),p3(21,17,9)
!
      write(6,'('' Program successful... ending now'')')
      stop
      end
