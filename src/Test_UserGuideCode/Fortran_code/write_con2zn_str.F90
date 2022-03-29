      program write_con2zn_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid (2 zones), and adds 1-to-1
!   connectivity information to it.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid2zn_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_con2zn_str.F90
!   ifort -o write_con2zn_str write_con2zn_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer ilo(2),ihi(2),jlo(2),jhi(2),klo(2),khi(2)
      integer itranfrm(3)
      integer index_conn,n1to1,nconns,nzone
      integer index_zone,index_base,index_file,ier
      integer(cgsize_t) isize(3,3),ipnts(3,2),ipntsdonor(3,2)
      character donorname*32,zonename(2)*32
!
      write(6,'('' Program write_con2zn_str'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
!
!  WRITE 1-TO-1 CONNECTIVITY INFORMATION TO EXISTING CGNS FILE
!  open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!  we know there is only one base (real working code would check!)
      index_base=1
!   get number of zones (should be 2 for our case)
      call cg_nzones_f(index_file,index_base,nzone,ier)
      if (nzone .ne. 2) then
         write(6,'('' Error.  This program expects 2 zones. '',i5,             &
          '' read'')') nzone
         stop
      end if
!   loop over zones to get zone sizes and names
      do index_zone=1,nzone
        call cg_zone_read_f(index_file,index_base,index_zone,                  &
             zonename(index_zone),isize,ier)
        ilo(index_zone)=1
        ihi(index_zone)=isize(1,1)
        jlo(index_zone)=1
        jhi(index_zone)=isize(2,1)
        klo(index_zone)=1
        khi(index_zone)=isize(3,1)
      enddo
!   loop over zones again
      do index_zone=1,nzone
!   for this program, there should be no existing connectivity info:
        call cg_nconns_f(index_file,index_base,index_zone,nconns,ier)
        if (nconns .ne. 0) then
          write(6,'('' Error.  This program expects no interfaces'',           &
            '' yet.'',i5,'' read'')') nconns
          stop
        end if
        call cg_n1to1_f(index_file,index_base,index_zone,n1to1,ier)
        if (n1to1 .ne. 0) then
          write(6,'('' Error.  This program expects no interfaces'',           &
            '' yet.'',i5,'' read'')') n1to1
          stop
        end if
!   set up index ranges
        if (index_zone .eq. 1) then
          donorname=zonename(2)
!   lower point of receiver range
          ipnts(1,1)=ihi(1)
          ipnts(2,1)=jlo(1)
          ipnts(3,1)=klo(1)
!   upper point of receiver range
          ipnts(1,2)=ihi(1)
          ipnts(2,2)=jhi(1)
          ipnts(3,2)=khi(1)
!   lower point of donor range
          ipntsdonor(1,1)=ilo(2)
          ipntsdonor(2,1)=jlo(2)
          ipntsdonor(3,1)=klo(2)
!   upper point of donor range
          ipntsdonor(1,2)=ilo(2)
          ipntsdonor(2,2)=jhi(2)
          ipntsdonor(3,2)=khi(2)
        else
          donorname=zonename(1)
!   lower point of receiver range
          ipnts(1,1)=ilo(2)
          ipnts(2,1)=jlo(2)
          ipnts(3,1)=klo(2)
!   upper point of receiver range
          ipnts(1,2)=ilo(2)
          ipnts(2,2)=jhi(2)
          ipnts(3,2)=khi(2)
!   lower point of donor range
          ipntsdonor(1,1)=ihi(1)
          ipntsdonor(2,1)=jlo(1)
          ipntsdonor(3,1)=klo(1)
!   upper point of donor range
          ipntsdonor(1,2)=ihi(1)
          ipntsdonor(2,2)=jhi(1)
          ipntsdonor(3,2)=khi(1)
        end if
!   set up Transform
        itranfrm(1)=1
        itranfrm(2)=2
        itranfrm(3)=3
!   write 1-to-1 info (user can give any name)
        call cg_1to1_write_f(index_file,index_base,index_zone,                 &
             'Interface',donorname,ipnts,ipntsdonor,itranfrm,                  &
             index_conn,ier)
      enddo
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added 1-to-1 connectivity info to'',           &
       '' file grid.cgns'')')
      stop
      end
