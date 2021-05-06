      program write_con2zn_genrl_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid (2 zones), and adds 1-to-1
!   connectivity information to it (using GENERAL
!   method, as opposed to specific 1-to-1 method).
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid2zn_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_con2zn_genrl_str.F90
!   ifort -o write_con2zn_genrl_str write_con2zn_genrl_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer, parameter :: maxcount=400
      integer ilo(2),ihi(2),jlo(2),jhi(2),klo(2),khi(2)
      integer(cgsize_t) isize(3,3),ipnts(3,maxcount)
      integer(cgsize_t) ipntsdonor(3,maxcount),icounts
      integer index_conn,index_zone,index_base,index_file,ier
      integer nconns,n1to1,icount,nzone,j,k
      character donorname*32,zonename(2)*32
!
      write(6,'('' Program write_con2zn_genrl_str'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
!
!  WRITE GENERAL CONNECTIVITY INFORMATION TO EXISTING CGNS FILE
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
!   set up point lists
        if (index_zone .eq. 1) then
          icount=0
          do j=jlo(index_zone),jhi(index_zone)
            do k=klo(index_zone),khi(index_zone)
              icount=icount+1
              ipnts(1,icount)=ihi(1)
              ipnts(2,icount)=j
              ipnts(3,icount)=k
              ipntsdonor(1,icount)=ilo(2)
              ipntsdonor(2,icount)=j
              ipntsdonor(3,icount)=k
            enddo
          enddo
          if (icount .gt. maxcount) then
            write(6,'('' Error.  Need to increase maxcount to at least'',      &
              i5)') icount
            stop
          end if
          donorname=zonename(2)
        else
          icount=0
          do j=jlo(index_zone),jhi(index_zone)
            do k=klo(index_zone),khi(index_zone)
              icount=icount+1
              ipnts(1,icount)=ilo(2)
              ipnts(2,icount)=j
              ipnts(3,icount)=k
              ipntsdonor(1,icount)=ihi(1)
              ipntsdonor(2,icount)=j
              ipntsdonor(3,icount)=k
            enddo
          enddo
          if (icount .gt. maxcount) then
            write(6,'('' Error.  Need to increase maxcount to at least'',      &
              i5)') icount
            stop
          end if
          donorname=zonename(1)
        end if
!   write integer connectivity info (user can give any name)
        icounts=icount
        call cg_conn_write_f(index_file,index_base,index_zone,                 &
             'GenInterface',CGNS_ENUMV(Vertex),CGNS_ENUMV(Abutting1to1),CGNS_ENUMV(PointList),icounts,ipnts,       &
             donorname,CGNS_ENUMV(Structured),CGNS_ENUMV(PointListDonor),CGNS_ENUMV(Integer),icounts,              &
             ipntsdonor,index_conn,ier)
      enddo
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added 1-to-1 connectivity info to'',           &
       '' file grid.cgns (using GENERAL method)'')')
      stop
      end
