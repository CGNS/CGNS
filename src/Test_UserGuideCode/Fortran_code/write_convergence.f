      program write_convergence
      use cgns
c
c   Adds convergence history to an existing CGNS file.
c
c   The CGNS grid file 'grid.cgns' must already exist.
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
c   ifort -I ../.. -c write_convergence.f
c   ifort -o write_convergence write_convergence.o -L ../../lib -lcgns
c
c   (../../lib is the location where the compiled
c   library libcgns.a is located)
c
c   The following is no longer supported; now superceded by "use cgns":
c     include 'cgnslib_f.h'
c   Note Windows machines need to include cgnswin_f.h
c
      parameter (ntt=20)
      real*8 cl(ntt)
      integer(cgsize_t) nuse
c
      write(6,'('' Program write_convergence'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
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
      call cg_convergence_write_f(ntt,'\0',ier)
c   go to new history node
      call cg_goto_f(index_file,index_base,ier,'ConvergenceHistory_t',
     +  1,'end')
c   write lift coefficient array (user must use SIDS-standard name here)
      nuse=ntt
      call cg_array_write_f('CoefLift',RealDouble,1,nuse,cl,ier)
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote cl history to file grid.cgns'')')
      stop
      end
