      program write_con2zn_str
c
c   Opens an existing CGNS file that contains a simple 3-D 
c   structured grid (2 zones), and adds 1-to-1
c   connectivity information to it.
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid2zn_str.f)
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c write_con2zn_str.f
c   ifort -o write_con2zn_str write_con2zn_str.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
      dimension isize(3,3),ilo(2),ihi(2),jlo(2),jhi(2),klo(2),khi(2)
      dimension ipnts(3,2),ipntsdonor(3,2),itranfrm(3)
      character donorname*32,zonename(2)*32
c
c  WRITE 1-TO-1 CONNECTIVITY INFORMATION TO EXISTING CGNS FILE
c  open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
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
c   loop over zones to get zone sizes and names
      do index_zone=1,nzone
        call cg_zone_read_f(index_file,index_base,index_zone,
     +    zonename(index_zone),isize,ier)
        ilo(index_zone)=1
        ihi(index_zone)=isize(1,1)
        jlo(index_zone)=1
        jhi(index_zone)=isize(2,1)
        klo(index_zone)=1
        khi(index_zone)=isize(3,1)
      enddo
c   loop over zones again
      do index_zone=1,nzone
c   for this program, there should be no existing connectivity info:
        call cg_nconns_f(index_file,index_base,index_zone,nconns,ier)
        if (nconns .ne. 0) then
          write(6,'('' Error.  This program expects no interfaces'',
     +      '' yet.'',i5,'' read'')') nconns
          stop
        end if
        call cg_n1to1_f(index_file,index_base,index_zone,n1to1,ier)
        if (n1to1 .ne. 0) then
          write(6,'('' Error.  This program expects no interfaces'',
     +      '' yet.'',i5,'' read'')') n1to1
          stop
        end if
c   set up index ranges
        if (index_zone .eq. 1) then
          donorname=zonename(2)
c   lower point of receiver range
          ipnts(1,1)=ihi(1)
          ipnts(2,1)=jlo(1)
          ipnts(3,1)=klo(1)
c   upper point of receiver range
          ipnts(1,2)=ihi(1)
          ipnts(2,2)=jhi(1)
          ipnts(3,2)=khi(1)
c   lower point of donor range
          ipntsdonor(1,1)=ilo(2)
          ipntsdonor(2,1)=jlo(2)
          ipntsdonor(3,1)=klo(2)
c   upper point of donor range
          ipntsdonor(1,2)=ilo(2)
          ipntsdonor(2,2)=jhi(2)
          ipntsdonor(3,2)=khi(2)
        else
          donorname=zonename(1)
c   lower point of receiver range
          ipnts(1,1)=ilo(2)
          ipnts(2,1)=jlo(2)
          ipnts(3,1)=klo(2)
c   upper point of receiver range
          ipnts(1,2)=ilo(2)
          ipnts(2,2)=jhi(2)
          ipnts(3,2)=khi(2)
c   lower point of donor range
          ipntsdonor(1,1)=ihi(1)
          ipntsdonor(2,1)=jlo(1)
          ipntsdonor(3,1)=klo(1)
c   upper point of donor range
          ipntsdonor(1,2)=ihi(1)
          ipntsdonor(2,2)=jhi(1)
          ipntsdonor(3,2)=khi(1)
        end if
c   set up Transform
        itranfrm(1)=1
        itranfrm(2)=2
        itranfrm(3)=3
c   write 1-to-1 info (user can give any name)
        call cg_1to1_write_f(index_file,index_base,index_zone,
     +    'Interface',donorname,ipnts,ipntsdonor,itranfrm,
     +    index_conn,ier)
      enddo
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added 1-to-1 connectivity info to'',
     + '' file grid.cgns'')')
      stop
      end
