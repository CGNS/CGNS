      program write_bcpnts_unst
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   unstructured grid, and adds BC definitions (defined
!   as individual FaceCenter "points" = PointList+GridLocation=FaceCenter)
!   The BCs are added as FaceCenter points, associated with
!   face elements (QUAD_4), rather than associated to nodes
!
!   For the following, be sure you are using Version 2.0 or
!   later release of the API
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_unst.f).  Note: whether the
!   existing CGNS file has a flow solution in it already or
!   not is irrelevant.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_bcpnts_unst.F90
!   ifort -o write_bcpnts_unst write_bcpnts_unst.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer, parameter :: maxcount=960
      integer n,ibc,icount,nelem_start,nelem_end
      integer index_zone,index_base,index_file,index_bc,ier
      integer(cgsize_t) ipnts(maxcount),icounts
!
      write(6,'('' Program write_bcpnts_unst'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
!
!  WRITE BOUNDARY CONDITIONS TO EXISTING CGNS FILE
!  open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!  we know there is only one base (real working code would check!)
      index_base=1
!  we know there is only one zone (real working code would check!)
      index_zone=1
!  we know that for the unstructured zone, the following face elements
!  have been defined as inflow (real working code would check!):
      nelem_start=2561
      nelem_end=2688
      icount=0
      do n=nelem_start,nelem_end
        icount=icount+1
        ipnts(icount)=n
      enddo
      if (icount .gt. maxcount) then
        write(6,'('' Error.  Need to increase maxcount to at least'',          &
         i5)') icount
        stop
      end if
!  write boundary conditions for ilo face
      icounts=icount
      call cg_boco_write_f(index_file,index_base,index_zone,'Ilo',             &
           CGNS_ENUMV(BCTunnelInflow),CGNS_ENUMV(PointList),icounts,ipnts,index_bc,ier)
!  we know that for the unstructured zone, the following face elements
!  have been defined as outflow (real working code would check!):
      nelem_start=2689
      nelem_end=2816
      icount=0
      do n=nelem_start,nelem_end
        icount=icount+1
        ipnts(icount)=n
      enddo
      if (icount .gt. maxcount) then
        write(6,'('' Error.  Need to increase maxcount to at least'',          &
         i5)') icount
        stop
      end if
!  write boundary conditions for ihi face
      icounts=icount
      call cg_boco_write_f(index_file,index_base,index_zone,'Ihi',             &
           CGNS_ENUMV(BCExtrapolate),CGNS_ENUMV(PointList),icounts,ipnts,index_bc,ier)
!  we know that for the unstructured zone, the following face elements
!  have been defined as walls (real working code would check!):
      nelem_start=2817
      nelem_end=3776
      icount=0
      do n=nelem_start,nelem_end
        icount=icount+1
        ipnts(icount)=n
      enddo
      if (icount .gt. maxcount) then
        write(6,'('' Error.  Need to increase maxcount to at least'',          &
         i5)') icount
        stop
      end if
!  write boundary conditions for wall faces
      icounts=icount
      call cg_boco_write_f(index_file,index_base,index_zone,'Walls',           &
           CGNS_ENUMV(BCWallInviscid),CGNS_ENUMV(PointList),icounts,ipnts,index_bc,ier)
!
!  the above are all face-center locations for the BCs - must indicate this,
!  otherwise Vertices will be assumed!
      do ibc=1,index_bc
!  (the following call positions you in BC_t - it assumes there
!  is only one Zone_t and one ZoneBC_t - real working code would check!)
        call cg_goto_f(index_file,index_base,ier,'Zone_t',1,                   &
             'ZoneBC_t',1,'BC_t',ibc,'end')
        call cg_gridlocation_write_f(CGNS_ENUMV(FaceCenter),ier)
      enddo
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added FaceCenter BCs (PointList) to'',         &
        '' unstructured grid file grid.cgns'')')
      stop
      end
