      program write_timevert_str
      use cgns
c
c   Opens an existing CGNS file that contains a simple 3-D
c   structured grid, and adds 3 different flow solutions
c   (at VERTICES) to it, along with time-accurate info.
c   In this example, r1 & p1, r2 & p2, r3 & p3 correspond
c   with solutions at 3 different time steps.
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid_str.f)
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths if needed!):
c   Note: when using the cgns module file, you must use the SAME fortran compiler
c   used to compile CGNS (see make.defs file)
c   ...or change, for example, via environment "setenv FC ifort"
c
c   ifort -I ../.. -c write_timevert_str.f
c   ifort -o write_timevert_str write_timevert_str.o -L ../../lib -lcgns
c
c   (../../lib is the location where the compiled
c   library libcgns.a is located)
c
c   The following is no longer supported; now superceded by "use cgns":
c     include 'cgnslib_f.h'
c   Note Windows machines need to include cgnswin_f.h
c
c   dimension statements (note that tri-dimensional arrays
c   r1, r2, r3 and p1, p2, p3
c   must be dimensioned exactly as (21,17,N) (N>=9)
c   for this particular case or else they will be written to
c   the CGNS file incorrectly!  Other options are to use 1-D
c   arrays, use dynamic memory, or pass index values to a
c   subroutine and dimension exactly there):
      real*8 r1(21,17,9),p1(21,17,9)
      real*8 r2(21,17,9),p2(21,17,9)
      real*8 r3(21,17,9),p3(21,17,9)
      real*8 time(3)
      integer(cgsize_t) idata(2),nuse
      character solname(3)*32
c
      write(6,'('' Program write_timevert_str'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
c
c   set up the times corresponding to the 3 solutions to be
c   stored:
      time(1)=10.d0
      time(2)=20.d0
      time(3)=50.d0
c   create fake flow solution AT VERTICES for simple example:
      ni=21
      nj=17
      nk=9
      do k=1,nk
        do j=1,nj
          do i=1,ni
c           soln at time 1:
            r1(i,j,k)=float(i-1)
            p1(i,j,k)=float(j-1)
c           soln at time 2:
            r2(i,j,k)=r1(i,j,k)+1.d0
            p2(i,j,k)=p1(i,j,k)+1.d0
c           soln at time 3:
            r3(i,j,k)=r2(i,j,k)+1.d0
            p3(i,j,k)=p2(i,j,k)+1.d0
          enddo
        enddo
      enddo
      write(6,'('' created simple 3-D rho and p flow solution'')')
c
c   WRITE FLOW SOLUTION TO EXISTING CGNS FILE
c   open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   we know there is only one zone (real working code would check!)
      index_zone=1
c   define 3 different solution names (user can give any names)
      solname(1) = 'FlowSolution1'
      solname(2) = 'FlowSolution2'
      solname(3) = 'FlowSolution3'
c   do loop for the 3 solutions:
      do n=1,3
c   create flow solution node
      call cg_sol_write_f(index_file,index_base,index_zone,solname(n),
     + Vertex,index_flow,ier)
      write(6,'('' ... writing solution number '',i5)') index_flow
c   write flow solution (user must use SIDS-standard names here)
      if (n .eq. 1) then
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,
     + RealDouble,'Density',r1,index_field,ier)
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,
     + RealDouble,'Pressure',p1,index_field,ier)
      else if (n .eq. 2) then
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,
     + RealDouble,'Density',r2,index_field,ier)
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,
     + RealDouble,'Pressure',p2,index_field,ier)
      else
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,
     + RealDouble,'Density',r3,index_field,ier)
      call cg_field_write_f(index_file,index_base,index_zone,index_flow,
     + RealDouble,'Pressure',p3,index_field,ier)
      end if
      enddo
c   create BaseIterativeData
      nsteps=3
      call cg_biter_write_f(index_file,index_base,'TimeIterValues',
     + nsteps,ier)
c   go to BaseIterativeData level and write time values
      call cg_goto_f(index_file,index_base,ier,'BaseIterativeData_t',
     + 1,'end')
      nuse=3
      call cg_array_write_f('TimeValues',RealDouble,1,nuse,time,ier)
c   create ZoneIterativeData
      call cg_ziter_write_f(index_file,index_base,index_zone,
     + 'ZoneIterativeData',ier)
c   go to ZoneIterativeData level and give info telling which
c   flow solution corresponds with which time (solname(1) corresponds
c   with time(1), solname(2) with time(2), and solname(3) with time(3))
      call cg_goto_f(index_file,index_base,ier,'Zone_t',
     + index_zone,'ZoneIterativeData_t',1,'end')
      idata(1)=32
      idata(2)=3
      call cg_array_write_f('FlowSolutionPointers',Character,2,idata,
     +    solname,ier)
c   add SimulationType
      call cg_simulation_type_write_f(index_file,index_base,
     + TimeAccurate,ier)
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added 3 times of flow solution data'',
     + '' and time info to file grid.cgns'')')
      write(6,'(''   Note:  if the original CGNS file already had'',
     + '' a FlowSolution_t node,'')')
      write(6,'(''          it has been overwritten'')')
      stop
      end
