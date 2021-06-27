      program read_bc_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid + BCs (in PointRange format), and reads the BCs
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_str.f), and the BCs must also
!   already have been written (using write_bc_str.f).  Note: whether the
!   existing CGNS file has a flow solution in it already or
!   not is irrelevant.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_bc_str.F90
!   ifort -o read_bc_str read_bc_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer(cgsize_t) ipnts(3,2),npts,normallistflag
      integer normalindex(3)
      integer normallist,ndataset,normaldatatype,iptset,ibocotype,ib,nbocos
      integer index_zone,index_base,index_file,ier
      character boconame*32
!
!  READ BOUNDARY CONDITIONS FROM EXISTING CGNS FILE
!  open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!  we know there is only one base (real working code would check!)
      index_base=1
!  we know there is only one zone (real working code would check!)
      index_zone=1
!  find out number of BCs that exist under this zone
      call cg_nbocos_f(index_file,index_base,index_zone,nbocos,ier)
!  do loop over the total number of BCs
      do ib=1,nbocos
!  get BC info
        call cg_boco_info_f(index_file,index_base,index_zone,ib,               &
             boconame,ibocotype,iptset,npts,normalindex,normallistflag,        &
             normaldatatype,ndataset,ier)
        if (iptset .ne.  CGNS_ENUMV(PointRange)) then
          write(6,'('' Error.  For this program, BCs must be set'',            &
           '' up as PointRange type'',a32)') PointSetTypeName(iptset)
          stop
        end if
        write(6,'('' BC number: '',i5)') ib
        write(6,'(''    name='',a32)') boconame
        write(6,'(''    type='',a32)') BCTypeName(ibocotype)
!  read point range in here
        call cg_boco_read_f(index_file,index_base,index_zone,ib,               &
             ipnts,normallist,ier)
        write(6,'(''    i-range='',2i5)') ipnts(1,1),ipnts(1,2)
        write(6,'(''    j-range='',2i5)') ipnts(2,1),ipnts(2,2)
        write(6,'(''    k-range='',2i5)') ipnts(3,1),ipnts(3,2)
      enddo
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read BCs (PointRange format)'',                &
       '' from file grid.cgns'')')
      stop
      end
