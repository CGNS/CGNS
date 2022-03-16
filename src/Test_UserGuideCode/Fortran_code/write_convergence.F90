      program write_convergence
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Adds convergence history to an existing CGNS file.
!
!   The CGNS grid file 'grid.cgns' must already exist.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_convergence.F90
!   ifort -o write_convergence write_convergence.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer, parameter :: ntt=20
      real*8 cl(ntt)
      integer index_base,index_file,ier,n
      integer(cgsize_t) nuse
!
      write(6,'('' Program write_convergence'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
!
!   create history array simple example:
      do n=1,ntt
        cl(n)=float(n)
      enddo
      write(6,'('' created simple cl history'')')
!
!  WRITE CONVERGENCE HISTORY INFORMATION TO EXISTING CGNS FILE
!  open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!  we know there is only one base (real working code would check!)
      index_base=1
!   go to base node
      call cg_goto_f(index_file,index_base,ier,'end')
!   create history node (SIDS names it GlobalConvergenceHistory at base level)
      call cg_convergence_write_f(ntt,CHAR(0),ier)
!   go to new history node
      call cg_goto_f(index_file,index_base,ier,'ConvergenceHistory_t',1,'end')
!   write lift coefficient array (user must use SIDS-standard name here)
      nuse=ntt
      call cg_array_write_f('CoefLift',CGNS_ENUMV(RealDouble),1,nuse,cl,ier)
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote cl history to file grid.cgns'')')
      stop
      end
