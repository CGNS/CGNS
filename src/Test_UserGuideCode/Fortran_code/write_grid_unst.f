      program write_grid_unst
      use cgns
c
c   Creates simple 3-D unstructured grid and writes it to a
c   CGNS file.
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
c   ifort -I ../.. -c write_grid_unst.f
c   ifort -o write_grid_unst write_grid_unst.o -L ../../lib -lcgns
c
c   (../../lib is the location where the compiled
c   library libcgns.a is located)
c
c   The following is no longer supported; now superceded by "use cgns":
c     include 'cgnslib_f.h'
c   Note Windows machines need to include cgnswin_f.h
c
      parameter (maxelemi=20*16*8, maxelemj=1216)
      real*8 x(21*17*9),y(21*17*9),z(21*17*9)
      integer(cgsize_t) isize(1,3),ielem(8,maxelemi),jelem(4,maxelemj)
      integer(cgsize_t) nelem_start,nelem_end
      character basename*32,zonename*32
c
      write(6,'('' Program write_grid_unst'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
c
c   create gridpoints for simple example:
      ni=21
      nj=17
      nk=9
      iset=0
      do k=1,nk
        do j=1,nj
          do i=1,ni
            iset=iset+1
            x(iset)=float(i-1)
            y(iset)=float(j-1)
            z(iset)=float(k-1)
          enddo
        enddo
      enddo
      write(6,'('' created simple 3-D grid points'')')
c
c   WRITE X, Y, Z GRID POINTS TO CGNS FILE
c   open CGNS file for write
      call cg_open_f('grid.cgns',CG_MODE_WRITE,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   create base (user can give any name)
      basename='Base'
      icelldim=3
      iphysdim=3
      call cg_base_write_f(index_file,basename,icelldim,iphysdim,
     + index_base,ier)
c   define zone name (user can give any name)
      zonename = 'Zone  1'
c   vertex size
      isize(1,1)=ni*nj*nk
c   cell size
      isize(1,2)=(ni-1)*(nj-1)*(nk-1)
c   boundary vertex size (zero if elements not sorted)
      isize(1,3)=0
c   create zone
      call cg_zone_write_f(index_file,index_base,zonename,isize,
     + Unstructured,index_zone,ier)
c   write grid coordinates (user must use SIDS-standard names here)
      call cg_coord_write_f(index_file,index_base,index_zone,RealDouble,
     + 'CoordinateX',x,index_coord,ier)
      call cg_coord_write_f(index_file,index_base,index_zone,RealDouble,
     + 'CoordinateY',y,index_coord,ier)
      call cg_coord_write_f(index_file,index_base,index_zone,RealDouble,
     + 'CoordinateZ',z,index_coord,ier)
c  set element connectivity:
c  ----------------------------------------------------------
c  do all the HEXA_8 elements (this part is mandatory):
c  maintain SIDS-standard ordering
      ielem_no=0
c  index no of first element
      nelem_start=1
      do k=1,nk-1
        do j=1,nj-1
          do i=1,ni-1
            ielem_no=ielem_no+1
c  in this example, due to the order in the node numbering, the
c  hexahedral elements can be reconstructed using the following
c  relationships:
            ifirstnode=i+(j-1)*ni+(k-1)*ni*nj
            ielem(1,ielem_no)=ifirstnode
            ielem(2,ielem_no)=ifirstnode+1
            ielem(3,ielem_no)=ifirstnode+1+ni
            ielem(4,ielem_no)=ifirstnode+ni
            ielem(5,ielem_no)=ifirstnode+ni*nj
            ielem(6,ielem_no)=ifirstnode+ni*nj+1
            ielem(7,ielem_no)=ifirstnode+ni*nj+1+ni
            ielem(8,ielem_no)=ifirstnode+ni*nj+ni
          enddo
        enddo
      enddo
c  index no of last element (=2560)
      nelem_end=ielem_no
      if (nelem_end .gt. maxelemi) then
        write(6,'('' Error, must increase maxelemi to at least '',
     +   i7)') nelem_end
        stop
      end if
c  unsorted boundary elements
      nbdyelem=0
c  write HEXA_8 element connectivity (user can give any name)
      call cg_section_write_f(index_file,index_base,index_zone,
     +  'Elem',HEXA_8,nelem_start,nelem_end,nbdyelem,ielem,
     +  index_section,ier)
c  ----------------------------------------------------------
c  do boundary (QUAD) elements (this part is optional,
c  but you must do it if you eventually want to define BCs
c  at element faces rather than at nodes):
c  maintain SIDS-standard ordering
c  INFLOW:
      ielem_no=0
c  index no of first element
      nelem_start=nelem_end+1
      i=1
      do k=1,nk-1
        do j=1,nj-1
          ielem_no=ielem_no+1
          ifirstnode=i+(j-1)*ni+(k-1)*ni*nj
          jelem(1,ielem_no)=ifirstnode
          jelem(2,ielem_no)=ifirstnode+ni*nj
          jelem(3,ielem_no)=ifirstnode+ni*nj+ni
          jelem(4,ielem_no)=ifirstnode+ni
        enddo
      enddo
c  index no of last element
      nelem_end=nelem_start+ielem_no-1
      if (ielem_no .gt. maxelemj) then
        write(6,'('' Error, must increase maxelemj to at least '',
     +   i7)') ielem_no
        stop
      end if
c  write QUAD element connectivity for inflow face (user can give any name)
      call cg_section_write_f(index_file,index_base,index_zone,
     +  'InflowElem',QUAD_4,nelem_start,nelem_end,nbdyelem,
     +  jelem,index_section,ier)
c  OUTFLOW:
      ielem_no=0
c  index no of first element
      nelem_start=nelem_end+1
      i=ni-1
      do k=1,nk-1
        do j=1,nj-1
          ielem_no=ielem_no+1
          ifirstnode=i+(j-1)*ni+(k-1)*ni*nj
          jelem(1,ielem_no)=ifirstnode+1
          jelem(2,ielem_no)=ifirstnode+1+ni
          jelem(3,ielem_no)=ifirstnode+ni*nj+1+ni
          jelem(4,ielem_no)=ifirstnode+ni*nj+1
        enddo
      enddo
c  index no of last element
      nelem_end=nelem_start+ielem_no-1
      if (ielem_no .gt. maxelemj) then
        write(6,'('' Error, must increase maxelemj to at least '',
     +   i7)') ielem_no
        stop
      end if
c  write QUAD element connectivity for outflow face (user can give any name)
      call cg_section_write_f(index_file,index_base,index_zone,
     +  'OutflowElem',QUAD_4,nelem_start,nelem_end,nbdyelem,
     +  jelem,index_section,ier)
c  SIDEWALLS:
      ielem_no=0
c  index no of first element
      nelem_start=nelem_end+1
      j=1
      do k=1,nk-1
        do i=1,ni-1
          ielem_no=ielem_no+1
          ifirstnode=i+(j-1)*ni+(k-1)*ni*nj
          jelem(1,ielem_no)=ifirstnode
          jelem(2,ielem_no)=ifirstnode+ni*nj
          jelem(3,ielem_no)=ifirstnode+ni*nj+1
          jelem(4,ielem_no)=ifirstnode+1
        enddo
      enddo
      j=nj-1
      do k=1,nk-1
        do i=1,ni-1
          ielem_no=ielem_no+1
          ifirstnode=i+(j-1)*ni+(k-1)*ni*nj
          jelem(1,ielem_no)=ifirstnode+1+ni
          jelem(2,ielem_no)=ifirstnode+ni
          jelem(3,ielem_no)=ifirstnode+ni*nj+ni
          jelem(4,ielem_no)=ifirstnode+ni*nj+1+ni
        enddo
      enddo
      k=1
      do j=1,nj-1
        do i=1,ni-1
          ielem_no=ielem_no+1
          ifirstnode=i+(j-1)*ni+(k-1)*ni*nj
          jelem(1,ielem_no)=ifirstnode
          jelem(2,ielem_no)=ifirstnode+1
          jelem(3,ielem_no)=ifirstnode+1+ni
          jelem(4,ielem_no)=ifirstnode+ni
        enddo
      enddo
      k=nk-1
      do j=1,nj-1
        do i=1,ni-1
          ielem_no=ielem_no+1
          ifirstnode=i+(j-1)*ni+(k-1)*ni*nj
          jelem(1,ielem_no)=ifirstnode+ni*nj
          jelem(2,ielem_no)=ifirstnode+ni*nj+ni
          jelem(3,ielem_no)=ifirstnode+ni*nj+1+ni
          jelem(4,ielem_no)=ifirstnode+ni*nj+1
        enddo
      enddo
c  index no of last element
      nelem_end=nelem_start+ielem_no-1
      if (ielem_no .gt. maxelemj) then
        write(6,'('' Error, must increase maxelemj to at least '',
     +   i7)') ielem_no
        stop
      end if
c  write QUAD element connectivity for sidewall face (user can give any name)
      call cg_section_write_f(index_file,index_base,index_zone,
     +  'SidewallElem',QUAD_4,nelem_start,nelem_end,nbdyelem,
     +  jelem,index_section,ier)
c  ----------------------------------------------------------
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote unstructured grid to file'',
     +  '' grid.cgns'')')
      stop
      end
