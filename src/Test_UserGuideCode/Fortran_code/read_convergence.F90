      program read_convergence
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Reads convergence history from an existing CGNS file.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   and a convergence history should be in it (using write_convergence.f).
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_convergence.F90
!   ifort -o read_convergence read_convergence.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer, parameter :: ntt=20
      real*4 cl(ntt)
      integer(cgsize_t) idim(1)
      integer ndim,itype,index_array,narrays
      integer index_base,index_file,ier
      character arrayname*32
!
!  READ CONVERGENCE HISTORY INFORMATION FROM EXISTING CGNS FILE
!  open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!  we know there is only one base (real working code would check!)
      index_base=1
!   go to base node
      call cg_goto_f(index_file,index_base,ier,'end')
!   go to history node (we assume it exists and that there is only one -
!   real working code would check!)
      call cg_goto_f(index_file,index_base,ier,'ConvergenceHistory_t',         &
           1,'end')
!   find out how many arrays are here (there should be only one!):
      call cg_narrays_f(narrays,ier)
      index_array=narrays
!   some checks:
      if (narrays .ne. 1) then
        write(6,'('' Error!  Expecting only one array, read'',i5)') narrays
        stop
      end if
      call cg_array_info_f(index_array,arrayname,itype,ndim,idim,ier)
      if (idim(1) .gt. ntt) then
        write(6,'('' Error! must increase ntt to at least '',i5)') idim(1)
        stop
      end if
      if (arrayname .ne. 'CoefLift') then
        write(6,'('' Error!  expecting CoefLift, read'',a32)') arrayname
        stop
      end if
!   read lift coefficient array
      call cg_array_read_as_f(index_array,CGNS_ENUMV(RealSingle),cl,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read cl history from file grid.cgns'')')
      write(6,'(''    values are: '',5f12.5)') cl(1),cl(2),cl(3),              &
        cl(4),cl(5)
      write(6,'(''                '',5f12.5)') cl(6),cl(7),cl(8),              &
        cl(9),cl(10)
      stop
      end
