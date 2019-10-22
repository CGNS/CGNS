      program read_con2zn_str
      use cgns
      implicit none
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid (2 zones) plus 1-to-1 connectivity
!   information, and reads the connectivity info.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid2zn_str.f plus write_con2zn_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_con2zn_str.F90
!   ifort -o read_con2zn_str read_con2zn_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer(cgsize_t) ipnts(3,2),ipntsdonor(3,2)
      integer itranfrm(3)
      integer nzone,n1to1
      integer index_conn,index_zone,index_base,index_file,ier
      character donorname*32,connectname*32
!
!  READ 1-TO-1 CONNECTIVITY INFORMATION FROM EXISTING CGNS FILE
!  open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
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
!   loop over zones
      do index_zone=1,nzone
!   find out how many 1-to-1 interfaces there are in this zone
!   (for this program, there should only be one)
        call cg_n1to1_f(index_file,index_base,index_zone,n1to1,ier)
        if (n1to1 .ne. 1) then
          write(6,'('' Error.  Expecting one 1-to-1 interface.'',              &
            i6,'' read'')') n1to1
          stop
        end if
        index_conn=n1to1
!   read 1-to-1 info
        call cg_1to1_read_f(index_file,index_base,index_zone,index_conn,       &
             connectname,donorname,ipnts,ipntsdonor,itranfrm,ier)
        write(6,'('' In zone '',i5,'':'')') index_zone
        write(6,'(''    donor name='',a32)') donorname
        write(6,'(''    range  (this zone)='',6i5)') ipnts(1,1),               &
         ipnts(2,1),ipnts(3,1),ipnts(1,2),ipnts(2,2),ipnts(3,2)
        write(6,'(''    range (donor zone)='',6i5)') ipntsdonor(1,1),          &
         ipntsdonor(2,1),ipntsdonor(3,1),ipntsdonor(1,2),                      &
         ipntsdonor(2,2),ipntsdonor(3,2)
        write(6,'(''    transform='',3i5)') itranfrm(1),itranfrm(2),           &
         itranfrm(3)
      enddo
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read 1-to-1 connectivity info from'',          &
       '' file grid.cgns'')')
      stop
      end
