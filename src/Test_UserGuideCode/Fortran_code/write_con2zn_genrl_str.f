      program write_con2zn_genrl_str
      use cgns
c
c   Opens an existing CGNS file that contains a simple 3-D
c   structured grid (2 zones), and adds 1-to-1
c   connectivity information to it (using GENERAL
c   method, as opposed to specific 1-to-1 method).
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid2zn_str.f)
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths if needed!):
c   Note: when using the cgns module file, you must use the SAME fortran compiler
c   used to compile CGNS (see make.defs file)
c   ...or change, for example, via environment "setenv FC ifort"
c
c   ifort -I ../.. -c write_con2zn_genrl_str.f
c   ifort -o write_con2zn_genrl_str write_con2zn_genrl_str.o -L ../../lib -lcgns
c
c   (../../lib is the location where the compiled
c   library libcgns.a is located)
c
c   The following is no longer supported; now superceded by "use cgns":
c     include 'cgnslib_f.h'
c   Note Windows machines need to include cgnswin_f.h
c
      parameter (maxcount=400)
      dimension ilo(2),ihi(2),jlo(2),jhi(2),klo(2),khi(2)
      integer(cgsize_t) isize(3,3),ipnts(3,maxcount)
      integer(cgsize_t) ipntsdonor(3,maxcount),icounts
      character donorname*32,zonename(2)*32
c
      write(6,'('' Program write_con2zn_genrl_str'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
c
c  WRITE GENERAL CONNECTIVITY INFORMATION TO EXISTING CGNS FILE
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
c   set up point lists
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
            write(6,'('' Error.  Need to increase maxcount to at least'',
     +        i5)') icount
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
            write(6,'('' Error.  Need to increase maxcount to at least'',
     +        i5)') icount
            stop
          end if
          donorname=zonename(1)
        end if
c   write integer connectivity info (user can give any name)
        icounts=icount
        call cg_conn_write_f(index_file,index_base,index_zone,
     +    'GenInterface',Vertex,Abutting1to1,PointList,icounts,ipnts,
     +    donorname,Structured,PointListDonor,Integer,icounts,
     +    ipntsdonor,index_conn,ier)
      enddo
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added 1-to-1 connectivity info to'',
     + '' file grid.cgns (using GENERAL method)'')')
      stop
      end
