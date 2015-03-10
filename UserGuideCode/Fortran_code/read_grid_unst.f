      program read_grid_unst
c
c   Reads simple 3-D unstructured grid from a CGNS file
c   (created using write_grid_unst.f).
c
c   The CGNS grid file 'grid.cgns' must already exist.
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c read_grid_unst.f
c   ifort -o read_grid_unst read_grid_unst.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c
      dimension x(21*17*9),y(21*17*9),z(21*17*9)
      dimension isize(1,3),ielem(8,20*16*8)
      character zonename*32,sectionname*32
c
c   READ X, Y, Z GRID POINTS FROM CGNS FILE
c   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   we know there is only one zone (real working code would check!)
      index_zone=1
c   get zone size (and name - although not needed here)
      call cg_zone_read_f(index_file,index_base,index_zone,zonename,
     + isize,ier)
c   lower range index
      irmin=1
c   upper range index of vertices
      irmax=isize(1,1)
c   read grid coordinates
      call cg_coord_read_f(index_file,index_base,index_zone,
     + 'CoordinateX',RealSingle,irmin,irmax,x,ier)
      call cg_coord_read_f(index_file,index_base,index_zone,
     + 'CoordinateY',RealSingle,irmin,irmax,y,ier)
      call cg_coord_read_f(index_file,index_base,index_zone,
     + 'CoordinateZ',RealSingle,irmin,irmax,z,ier)
c  find out how many sections
      call cg_nsections_f(index_file,index_base,index_zone,nsections,
     +  ier)
      write(6,'('' number of sections='',i7)') nsections
c  read element connectivity
      do index_sect=1,nsections
        call cg_section_read_f(index_file,index_base,index_zone,
     +    index_sect,sectionname,itype,istart,iend,nbndry,
     +    iparent_flag,ier)
        write(6,'('' Reading section data...'')')
        write(6,'(''    section name='',a32)') sectionname
        write(6,'(''    section type='',a32)') ElementTypeName(itype)
        write(6,'(''    istart,iend='',2i6)') istart,iend
        if (ElementTypeName(itype) .eq. 'HEXA_8') then
          write(6,'(''    reading element data for this element'')')
          call cg_elements_read_f(index_file,index_base,index_zone,
     +      index_sect,ielem,iparentdata,ier)
        else
          write(6,'(''    not reading element data for this element'')')
        end if
      enddo
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read unstructured grid from file'',
     +  '' grid.cgns'')')
      write(6,'(''   for example, element 1 is made up of nodes:'',
     +  8i5)') ielem(1,1),ielem(2,1),ielem(3,1),ielem(4,1),
     +  ielem(5,1),ielem(6,1),ielem(7,1),ielem(8,1)
      write(6,'(''   x,y,z of node 358 are:'',3f12.5)')
     +  x(358),y(358),z(358)
      write(6,'(''   x,y,z of node 1358 are:'',3f12.5)')
     +  x(1358),y(1358),z(1358)
      stop
      end
