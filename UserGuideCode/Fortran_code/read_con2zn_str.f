      program read_con2zn_str
c
c   Opens an existing CGNS file that contains a simple 3-D 
c   structured grid (2 zones) plus 1-to-1 connectivity 
c   information, and reads the connectivity info.
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid2zn_str.f plus write_con2zn_str.f)
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c read_con2zn_str.f
c   ifort -o read_con2zn_str read_con2zn_str.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c
      dimension ipnts(3,2),ipntsdonor(3,2),itranfrm(3)
      character donorname*32,connectname*32
c
c  READ 1-TO-1 CONNECTIVITY INFORMATION FROM EXISTING CGNS FILE
c  open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c  we know there is only one base (real working code would check!)
      index_base=1
c   get number of zones (should be 2 for our case)
      call cg_nzones_f(index_file,index_base,nzone,ier)
      if (nzone .ne. 2) then
         write(6,'('' Error.  This program expects 2 zones. '',i5,
     +    '' read'')') nzone
         stop
      end if
c   loop over zones 
      do index_zone=1,nzone
c   find out how many 1-to-1 interfaces there are in this zone
c   (for this program, there should only be one)
        call cg_n1to1_f(index_file,index_base,index_zone,n1to1,ier)
        if (n1to1 .ne. 1) then
          write(6,'('' Error.  Expecting one 1-to-1 interface.'',
     +      i6,'' read'')') n1to1
          stop
        end if
        index_conn=n1to1
c   read 1-to-1 info
        call cg_1to1_read_f(index_file,index_base,index_zone,index_conn,
     +    connectname,donorname,ipnts,ipntsdonor,itranfrm,ier)
        write(6,'('' In zone '',i5,'':'')') index_zone
        write(6,'(''    donor name='',a32)') donorname
        write(6,'(''    range  (this zone)='',6i5)') ipnts(1,1),
     +   ipnts(2,1),ipnts(3,1),ipnts(1,2),ipnts(2,2),ipnts(3,2)
        write(6,'(''    range (donor zone)='',6i5)') ipntsdonor(1,1),
     +   ipntsdonor(2,1),ipntsdonor(3,1),ipntsdonor(1,2),
     +   ipntsdonor(2,2),ipntsdonor(3,2)
        write(6,'(''    transform='',3i5)') itranfrm(1),itranfrm(2),
     +   itranfrm(3)
      enddo
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read 1-to-1 connectivity info from'',
     + '' file grid.cgns'')')
      stop
      end
