      program write_bc_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid, and adds BC definitions (defined
!   over a range of points = PointRange)
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_str.f).  Note: whether the
!   existing CGNS file has a flow solution in it already or
!   not is irrelevant.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_bc_str.F90
!   ifort -o write_bc_str write_bc_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer ilo,ihi,jlo,jhi,klo,khi
      integer index_zone,index_base,index_file,index_bc,ier
      integer(cgsize_t) isize(3,3),ipnts(3,2)
      character zonename*32
!
      write(6,'('' Program write_bc_str'')')
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
!   get zone size (and name - although not needed here)
      call cg_zone_read_f(index_file,index_base,index_zone,zonename,           &
           isize,ier)
      write(6,'('' zonename='',a32)') zonename
      ilo=1
      ihi=isize(1,1)
      jlo=1
      jhi=isize(2,1)
      klo=1
      khi=isize(3,1)
!  write boundary conditions for ilo face, defining range first
!  (user can give any name)
!  lower point of range
      ipnts(1,1)=ilo
      ipnts(2,1)=jlo
      ipnts(3,1)=klo
!  upper point of range
      ipnts(1,2)=ilo
      ipnts(2,2)=jhi
      ipnts(3,2)=khi
      call cg_boco_write_f(index_file,index_base,index_zone,'Ilo',             &
           CGNS_ENUMV(BCTunnelInflow),CGNS_ENUMV(PointRange),2_cgsize_t,ipnts,index_bc,ier)
!  write boundary conditions for ihi face, defining range first
!  (user can give any name)
!  lower point of range
      ipnts(1,1)=ihi
      ipnts(2,1)=jlo
      ipnts(3,1)=klo
!  upper point of range
      ipnts(1,2)=ihi
      ipnts(2,2)=jhi
      ipnts(3,2)=khi
      call cg_boco_write_f(index_file,index_base,index_zone,'Ihi',             &
           CGNS_ENUMV(BCExtrapolate),CGNS_ENUMV(PointRange),2_cgsize_t,ipnts,index_bc,ier)
!  write boundary conditions for jlo face, defining range first
!  (user can give any name)
!  lower point of range
      ipnts(1,1)=ilo
      ipnts(2,1)=jlo
      ipnts(3,1)=klo
!  upper point of range
      ipnts(1,2)=ihi
      ipnts(2,2)=jlo
      ipnts(3,2)=khi
      call cg_boco_write_f(index_file,index_base,index_zone,'Jlo',             &
           CGNS_ENUMV(BCWallInviscid),CGNS_ENUMV(PointRange),2_cgsize_t,ipnts,index_bc,ier)
!  write boundary conditions for jhi face, defining range first
!  (user can give any name)
!  lower point of range
      ipnts(1,1)=ilo
      ipnts(2,1)=jhi
      ipnts(3,1)=klo
!  upper point of range
      ipnts(1,2)=ihi
      ipnts(2,2)=jhi
      ipnts(3,2)=khi
      call cg_boco_write_f(index_file,index_base,index_zone,'Jhi',             &
           CGNS_ENUMV(BCWallInviscid),CGNS_ENUMV(PointRange),2_cgsize_t,ipnts,index_bc,ier)
!  write boundary conditions for klo face, defining range first
!  (user can give any name)
!  lower point of range
      ipnts(1,1)=ilo
      ipnts(2,1)=jlo
      ipnts(3,1)=klo
!  upper point of range
      ipnts(1,2)=ihi
      ipnts(2,2)=jhi
      ipnts(3,2)=klo
      call cg_boco_write_f(index_file,index_base,index_zone,'Klo',             &
           CGNS_ENUMV(BCWallInviscid),CGNS_ENUMV(PointRange),2_cgsize_t,ipnts,index_bc,ier)
!  write boundary conditions for khi face, defining range first
!  (user can give any name)
!  lower point of range
      ipnts(1,1)=ilo
      ipnts(2,1)=jlo
      ipnts(3,1)=khi
!  upper point of range
      ipnts(1,2)=ihi
      ipnts(2,2)=jhi
      ipnts(3,2)=khi
      call cg_boco_write_f(index_file,index_base,index_zone,'Khi',             &
           CGNS_ENUMV(BCWallInviscid),CGNS_ENUMV(PointRange),2_cgsize_t,ipnts,index_bc,ier)
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added BCs (PointRange) to file'',              &
        '' grid.cgns'')')
      stop
      end
