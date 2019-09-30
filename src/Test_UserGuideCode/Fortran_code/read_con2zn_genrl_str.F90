      program read_con2zn_genrl_str
      use cgns
      implicit none
!
!   Opens an existing CGNS file that contains a simple 3-D
!   structured grid (2 zones) plus 1-to-1 connectivity
!   information (written in GENERAL form), and reads the
!   connectivity info.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid2zn_str.f plus write_con2zn_genrl_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_con2zn_genrl_str.F90
!   ifort -o read_con2zn_genrl_str read_con2zn_genrl_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      integer, parameter :: maxpnts=400
      integer(cgsize_t) npts,ndata_donor
      integer(cgsize_t) ipnts(3,maxpnts),ipntsdonor(3,maxpnts)
      integer i,idonor_datatype,idonor_ptset_type,idonor_zonetype,iptset_type
      integer iconnect_type,location,nconns,nzone
      integer index_conn,index_zone,index_base,index_file,ier
      character donorname*32,connectname*32
!
!  READ GENERAL CONNECTIVITY INFORMATION FROM EXISTING CGNS FILE
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
!   find out how many general interfaces there are in this zone
!   (for this program, there should only be one)
        call cg_nconns_f(index_file,index_base,index_zone,nconns,ier)
        if (nconns .ne. 1) then
          write(6,'('' Error.  Expecting one general interface.'',             &
            i6,'' read'')') nconns
          stop
        end if
        index_conn=nconns
!   read general connectivity info
        call cg_conn_info_f(index_file,index_base,index_zone,index_conn,       &
             connectname,location,iconnect_type,iptset_type,npts,              &
             donorname,idonor_zonetype,idonor_ptset_type,idonor_datatype,      &
             ndata_donor,ier)
        if (npts .gt. maxpnts) then
          write(6,'('' Error.  Must increase maxpnts to at least '',i5)') npts
          stop
        end if
        call cg_conn_read_f(index_file,index_base,index_zone,index_conn,       &
             ipnts,idonor_datatype,ipntsdonor,ier)
        write(6,'('' In zone '',i5,'':'')') index_zone
        write(6,'(''    donor name='',a32)') donorname
        write(6,'(''    number of connectivity pts='',i6)') npts
        write(6,'(''    grid location='',a32)')                                &
         GridLocationName(location)
        write(6,'(''    connectivity type='',a32)')                            &
         GridConnectivityTypeName(iconnect_type)
        write(6,'(''    pointset type='',a32)')                                &
         PointSetTypeName(iptset_type)
        write(6,'(''    donor zonetype='',a32)')                               &
         ZoneTypeName(idonor_zonetype)
        write(6,'(''    donor pointset type='',a32)')                          &
         PointSetTypeName(idonor_ptset_type)
!       write(6,'(''    data type='',a32)') DataTypeName(idonor_datatype)
        write(6,'(''    ipnts and ipntsdonor arrays read, only some'',         &
         '' written out here:'')')
        do i=1,10
          write(6,'('' ipnts(1,'',i2,''), (2,'',i2,''), (3,'',i2,'')='',       &
            3i4,''   ipntsdonor(1,'',i2,''), (2,'',i2,''), (3,'',i2,           &
            '')='',3i4)') i,i,i,ipnts(1,i),ipnts(2,i),ipnts(3,i),              &
            i,i,i,ipntsdonor(1,i),ipntsdonor(2,i),ipntsdonor(3,i)
        enddo
      enddo
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read general 1-to-1 connectivity'',            &
       '' info from file grid.cgns'')')
      stop
      end
