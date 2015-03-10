      program read_bc_str
c
c   Opens an existing CGNS file that contains a simple 3-D 
c   structured grid + BCs (in PointRange format), and reads the BCs
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid_str.f), and the BCs must also
c   already have been written (using write_bc_str.f).  Note: whether the
c   existing CGNS file has a flow solution in it already or
c   not is irrelevant.
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c read_bc_str.f
c   ifort -o read_bc_str read_bc_str.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c
      dimension ipnts(3,2),normalindex(3)
      character boconame*32
c
c  READ BOUNDARY CONDITIONS FROM EXISTING CGNS FILE
c  open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c  we know there is only one base (real working code would check!)
      index_base=1
c  we know there is only one zone (real working code would check!)
      index_zone=1
c  find out number of BCs that exist under this zone
      call cg_nbocos_f(index_file,index_base,index_zone,nbocos,ier)
c  do loop over the total number of BCs
      do ib=1,nbocos
c  get BC info
        call cg_boco_info_f(index_file,index_base,index_zone,ib,
     +    boconame,ibocotype,iptset,npts,normalindex,normallistflag,
     +    normaldatatype,ndataset,ier)
        if (iptset .ne. PointRange) then
          write(6,'('' Error.  For this program, BCs must be set'',
     +     '' up as PointRange type'',a32)') PointSetTypeName(iptset)
          stop
        end if
        write(6,'('' BC number: '',i5)') ib
        write(6,'(''    name='',a32)') boconame
        write(6,'(''    type='',a32)') BCTypeName(ibocotype)
c  read point range in here
        call cg_boco_read_f(index_file,index_base,index_zone,ib,
     +    ipnts,normallist,ier)
        write(6,'(''    i-range='',2i5)') ipnts(1,1),ipnts(1,2)
        write(6,'(''    j-range='',2i5)') ipnts(2,1),ipnts(2,2)
        write(6,'(''    k-range='',2i5)') ipnts(3,1),ipnts(3,2)
      enddo
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read BCs (PointRange format)'',
     + '' from file grid.cgns'')')
      stop
      end
