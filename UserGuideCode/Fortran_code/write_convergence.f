      program write_convergence
c
c   Adds convergence history to an existing CGNS file.
c
c   The CGNS grid file 'grid.cgns' must already exist.
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c write_convergence.f
c   ifort -o write_convergence write_convergence.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c   Note Windows machines also need to include cgnswin_f.h
c
      parameter (ntt=20)
      real*8 cl(ntt)
c
c   create history array simple example:
      do n=1,ntt
        cl(n)=float(n)
      enddo
      write(6,'('' created simple cl history'')')
c
c  WRITE CONVERGENCE HISTORY INFORMATION TO EXISTING CGNS FILE
c  open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c  we know there is only one base (real working code would check!)
      index_base=1
c   go to base node
      call cg_goto_f(index_file,index_base,ier,'end')
c   create history node (SIDS names it GlobalConvergenceHistory at base level)
      call cg_convergence_write_f(ntt,'',ier)
c   go to new history node
      call cg_goto_f(index_file,index_base,ier,'ConvergenceHistory_t',
     +  1,'end')
c   write lift coefficient array (user must use SIDS-standard name here)
      call cg_array_write_f('CoefLift',RealDouble,1,ntt,cl,ier)
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote cl history to file grid.cgns'')')
      stop
      end
