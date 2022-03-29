      program read_grid_unst
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Reads simple 3-D unstructured grid from a CGNS file
!   (created using write_grid_unst.f).
!
!   The CGNS grid file 'grid.cgns' must already exist.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_grid_unst.F90
!   ifort -o read_grid_unst read_grid_unst.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*4 x(21*17*9),y(21*17*9),z(21*17*9)
      integer(cgsize_t) isize(1,3),ielem(8,20*16*8)
      integer(cgsize_t) irmin,irmax,istart,iend,iparentdata
      integer iparent_flag,nbndry,itype,index_sect,nsections
      integer index_zone,index_base,index_file,ier
      character zonename*32,sectionname*32
!
!   READ X, Y, Z GRID POINTS FROM CGNS FILE
!   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   get zone size (and name - although not needed here)
      call cg_zone_read_f(index_file,index_base,index_zone,zonename,           &
           isize,ier)
!   lower range index
      irmin=1
!   upper range index of vertices
      irmax=isize(1,1)
!   read grid coordinates
      call cg_coord_read_f(index_file,index_base,index_zone,                   &
           'CoordinateX',CGNS_ENUMV(RealSingle),irmin,irmax,x,ier)
      call cg_coord_read_f(index_file,index_base,index_zone,                   &
           'CoordinateY',CGNS_ENUMV(RealSingle),irmin,irmax,y,ier)
      call cg_coord_read_f(index_file,index_base,index_zone,                   &
           'CoordinateZ',CGNS_ENUMV(RealSingle),irmin,irmax,z,ier)
!  find out how many sections
      call cg_nsections_f(index_file,index_base,index_zone,nsections,ier)
      write(6,'('' number of sections='',i7)') nsections
!  read element connectivity
      do index_sect=1,nsections
        call cg_section_read_f(index_file,index_base,index_zone,               &
             index_sect,sectionname,itype,istart,iend,nbndry,                  &
             iparent_flag,ier)
        write(6,'('' Reading section data...'')')
        write(6,'(''    section name='',a32)') sectionname
        write(6,'(''    section type='',a32)') ElementTypeName(itype)
        write(6,'(''    istart,iend='',2i6)') istart,iend
        if (ElementTypeName(itype) .eq. 'HEXA_8') then
          write(6,'(''    reading element data for this element'')')
          call cg_elements_read_f(index_file,index_base,index_zone,            &
               index_sect,ielem,iparentdata,ier)
        else
          write(6,'(''    not reading element data for this element'')')
        end if
      enddo
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read unstructured grid from file'',            &
        '' grid.cgns'')')
      write(6,'(''   for example, element 1 is made up of nodes:'',            &
        8i5)') ielem(1,1),ielem(2,1),ielem(3,1),ielem(4,1),                    &
        ielem(5,1),ielem(6,1),ielem(7,1),ielem(8,1)
      write(6,'(''   x,y,z of node 358 are:'',3f12.5)')                        &
        x(358),y(358),z(358)
      write(6,'(''   x,y,z of node 1358 are:'',3f12.5)')                       &
        x(1358),y(1358),z(1358)
      stop
      end
