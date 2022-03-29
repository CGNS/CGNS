      program write_descriptor
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Adds descriptor node to an existing CGNS file (under the
!   CGNSBase_t node).
!
!   The CGNS grid file 'grid.cgns' must already exist.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_descriptor.F90
!   ifort -o write_descriptor write_descriptor.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer index_base,index_file,ier
      character text1*36,text2*36,textstring*73
!
      write(6,'('' Program write_descriptor'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...compiled in 64-bit mode, but not needed'')')
      end if
!
!  WRITE DESCRIPTOR NODE AT BASE LEVEL
!  open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!  we know there is only one base (real working code would check!)
      index_base=1
!   go to base node
      call cg_goto_f(index_file,index_base,ier,'end')
!   write descriptor node (user can give any name)
      text1='Supersonic vehicle with landing gear'
      text2='M=4.6, Re=6 million'
      textstring=text1//char(10)//text2
      call cg_descriptor_write_f('Information',textstring,ier)
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote descriptor node to file'',               &
        '' grid.cgns'')')
      stop
      end
