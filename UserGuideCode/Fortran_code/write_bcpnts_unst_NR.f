      program write_bcpnts_unst_NR
c
c   *** This method, although accepted, is not recommended ***
c   *** As of V3.1.3, use of ElementRange and ElementList
c   *** has been deprecated.
c   *** See program write_bcpnts_unst instead ***
c
c   Opens an existing CGNS file that contains a simple 3-D 
c   unstructured grid, and adds BC definitions (defined
c   as individual elements = ElementList, associated with
c   face elements (QUAD_4)
c
c   For the following, be sure you are using Version 2.0 or 
c   later release of the API
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid_unst.f).  Note: whether the 
c   existing CGNS file has a flow solution in it already or
c   not is irrelevant.
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c write_bcpnts_unst_NR.f
c   ifort -o write_bcpnts_unst_NR write_bcpnts_unst_NR.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c   Note Windows machines also need to include cgnswin_f.h
c
      parameter (maxcount=960)
      dimension ipnts(maxcount)
c
c  WRITE BOUNDARY CONDITIONS TO EXISTING CGNS FILE
c  open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c  we know there is only one base (real working code would check!)
      index_base=1
c  we know there is only one zone (real working code would check!)
      index_zone=1
c  we know that for the unstructured zone, the following face elements
c  have been defined as inflow (real working code would check!):
      nelem_start=2561
      nelem_end=2688
      icount=0
      do n=nelem_start,nelem_end
        icount=icount+1
        ipnts(icount)=n
      enddo
      if (icount .gt. maxcount) then
        write(6,'('' Error.  Need to increase maxcount to at least'',
     +   i5)') icount
        stop
      end if
c  write boundary conditions for ilo face
      call cg_boco_write_f(index_file,index_base,index_zone,'Ilo',
     + BCTunnelInflow,ElementList,icount,ipnts,index_bc,ier)
c  we know that for the unstructured zone, the following face elements
c  have been defined as outflow (real working code would check!):
      nelem_start=2689
      nelem_end=2816
      icount=0
      do n=nelem_start,nelem_end
        icount=icount+1
        ipnts(icount)=n
      enddo
      if (icount .gt. maxcount) then
        write(6,'('' Error.  Need to increase maxcount to at least'',
     +   i5)') icount
        stop
      end if
c  write boundary conditions for ihi face
      call cg_boco_write_f(index_file,index_base,index_zone,'Ihi',
     + BCExtrapolate,ElementList,icount,ipnts,index_bc,ier)
c  we know that for the unstructured zone, the following face elements
c  have been defined as walls (real working code would check!):
      nelem_start=2817
      nelem_end=3776
      icount=0
      do n=nelem_start,nelem_end
        icount=icount+1
        ipnts(icount)=n
      enddo
      if (icount .gt. maxcount) then
        write(6,'('' Error.  Need to increase maxcount to at least'',
     +   i5)') icount
        stop
      end if
c  write boundary conditions for wall faces
      call cg_boco_write_f(index_file,index_base,index_zone,'Walls',
     + BCWallInviscid,ElementList,icount,ipnts,index_bc,ier)
c
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully added FaceCenter BCs (ElementList) to'',
     +  '' unstructured grid file grid.cgns'')')
      stop
      end
