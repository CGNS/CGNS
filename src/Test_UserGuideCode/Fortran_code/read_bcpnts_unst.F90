      program read_bcpnts_unst
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   unstructured grid + BCs (in PointList+GridLocation=FaceCenter
!   format), and reads the BCs
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_unst.f), and the BCs must also
!   already have been written (using write_bcpnts_unst.f).  Note: whether the
!   existing CGNS file has a flow solution in it already or
!   not is irrelevant.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_bcpnts_unst.F90
!   ifort -o read_bcpnts_unst read_bcpnts_unst.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer, parameter :: maxpnts=960
      integer(cgsize_t) ipnts(maxpnts),npts,normallistflag
      integer normalindex(3)
      integer i,normallist,ndataset,normaldatatype,iptset,ibocotype
      integer igr,ib,nbocos
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
!  find out what BC grid location is (expecting FaceCenter)
        call cg_goto_f(index_file,index_base,ier,'Zone_t',1,                   &
             'ZoneBC_t',1,'BC_t',ib,'end')
        call cg_gridlocation_read_f(igr,ier)
        if (igr .eq. CGNS_ENUMV(FaceCenter)) then
          write(6,'('' GridLocation=FaceCenter means BC data refers'',         &
            '' to elements, not nodes'')')
        end if
!  get BC info
        call cg_boco_info_f(index_file,index_base,index_zone,ib,               &
             boconame,ibocotype,iptset,npts,normalindex,normallistflag,        &
             normaldatatype,ndataset,ier)
        if (iptset .ne. CGNS_ENUMV(PointList)) then
          write(6,'('' Error.  For this program, BCs must be set'',            &
           ''  up as PointList type'',a32)') PointSetTypeName(iptset)
          stop
        end if
        write(6,'('' BC number: '',i5)') ib
        write(6,'(''    name='',a32)') boconame
        write(6,'(''    type='',a32)') BCTypeName(ibocotype)
        write(6,'(''    no of elements='',i5)') npts
        if (npts .gt. maxpnts) then
          write(6,'('' Error.  Must increase maxpnts to at least '',           &
           i5)') npts
          stop
        end if
!  read point list in here (nothing done with them in this program)
        call cg_boco_read_f(index_file,index_base,index_zone,ib,               &
             ipnts,normallist,ier)
        write(6,'(''      (these elements read here, but only some'',          &
          '' printed out:)'')')
        do i=1,10
          write(6,'('' ipnts('',i2,'')='',i4)') i,ipnts(i)
        enddo
      enddo
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read BCs (PointList format)'',                 &
       '' from file grid.cgns'')')
      stop
      end
