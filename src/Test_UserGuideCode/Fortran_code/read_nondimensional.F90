      program read_nondimensional
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file and reads the DataClass and
!   ReferenceState appropriate for a completely
!   NONDIMENSIONAL data set.
!
!   The CGNS grid file 'grid.cgns' must already exist,
!   processed further using write_nondimensional.f.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_nondimensional.F90
!   ifort -o read_nondimensional read_nondimensional.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*8 data
      integer(cgsize_t) idimvec(1)
      integer n,idim,idata,narrays,id,index_base,index_file,ier
      character state*32,arrayname*32
!
!   READ NONDIMENSIONAL INFO
!   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   read DataClass under Base
      call cg_goto_f(index_file,index_base,ier,'end')
      call cg_dataclass_read_f(id,ier)
      write(6,'('' DataClass = '',a32)') DataClassName(id)
      if (DataClassName(id) .ne. 'NormalizedByUnknownDimensional') then
        write(6,'('' Error!  Expecting NormalizedByUnknownDimensional'')')
        stop
      end if
!   read ReferenceState under Base
      call cg_state_read_f(state,ier)
      write(6,'('' ReferenceState = '',a32)') state
!   Go to ReferenceState node, read Mach array and its dataclass
      call cg_goto_f(index_file,index_base,ier,'ReferenceState_t',1,'end')
!   find out how many data arrays
      call cg_narrays_f(narrays,ier)
      do n=1,narrays
        call cg_array_info_f(n,arrayname,idata,idim,idimvec,ier)
        if (idim .ne. 1 .or. idimvec(1) .ne. 1) then
          write(6,'('' Error! expecting idim,idimvec=1,1'')')
          write(6,'(''    they are idim,idimvec='',2i5)')idim,idimvec(1)
          stop
        end if
        call cg_array_read_as_f(n,CGNS_ENUMV(RealDouble),data,ier)
        write(6,'('' Variable='',a32)') arrayname
        write(6,'(''     data='',f18.8)') data
      enddo
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read nondimensional info from file'',          &
       '' grid.cgns'')')
      stop
      end
