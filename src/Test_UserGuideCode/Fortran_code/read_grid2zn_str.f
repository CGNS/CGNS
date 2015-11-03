      program read_grid2zn_str
      use cgns
c
c   Reads simple 3-D structured 2-zone grid from CGNS file
c   (companion program to write_grid2zn_str.f)
c
c   The CGNS grid file 'grid.cgns' must already exist.
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
c   ifort -I ../.. -c read_grid2zn_str.f
c   ifort -o read_grid2zn_str read_grid2zn_str.o -L ../../lib -lcgns
c
c   (../../lib is the location where the compiled
c   library libcgns.a is located)
c
c   The following is no longer supported; now superceded by "use cgns":
c     include 'cgnslib_f.h'
c   Note Windows machines need to include cgnswin_f.h
c
c   dimension statements (note that tri-dimensional arrays
c   x,y,z must be dimensioned exactly as (21,17,N) (N>=9)
c   for this particular case or else they will be read from
c   the CGNS file incorrectly!  Other options are to use 1-D
c   arrays, use dynamic memory, or pass index values to a
c   subroutine and dimension exactly there):
      dimension x(21,17,9),y(21,17,9),z(21,17,9)
      dimension xsav(21,17,9,2),ysav(21,17,9,2),zsav(21,17,9,2)
      integer(cgsize_t) isize(3,3),irmin(3),irmax(3)
      character zonename*32
c
c   READ X, Y, Z GRID POINTS FROM CGNS FILE
c   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   get number of zones (should be 2 for our case)
      call cg_nzones_f(index_file,index_base,nzone,ier)
      if (nzone .ne. 2) then
         write(6,'('' Error.  This program expects 2 zones. '',i5,
     +    '' read'')') nzone
         stop
      end if
c   do loop over the zones
      do index_zone=1,nzone
c   get zone size (and name - although not needed here)
        call cg_zone_read_f(index_file,index_base,index_zone,
     +    zonename,isize,ier)
c   lower range index
        irmin(1)=1
        irmin(2)=1
        irmin(3)=1
c   upper range index of vertices
        irmax(1)=isize(1,1)
        irmax(2)=isize(2,1)
        irmax(3)=isize(3,1)
c   read grid coordinates
        call cg_coord_read_f(index_file,index_base,index_zone,
     +   'CoordinateX',RealSingle,irmin,irmax,x,ier)
        call cg_coord_read_f(index_file,index_base,index_zone,
     +   'CoordinateY',RealSingle,irmin,irmax,y,ier)
        call cg_coord_read_f(index_file,index_base,index_zone,
     +   'CoordinateZ',RealSingle,irmin,irmax,z,ier)
c   store grid coordinates in xsav,ysav,zsav array:
        do i=1,isize(1,1)
          do j=1,isize(2,1)
            do k=1,isize(3,1)
              xsav(i,j,k,index_zone)=x(i,j,k)
              ysav(i,j,k,index_zone)=y(i,j,k)
              zsav(i,j,k,index_zone)=z(i,j,k)
            enddo
          enddo
        enddo
      enddo
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read grid from file grid.cgns'')')
      write(6,'(''   For example, zone 1 x,y,z(21,17,9)='',3f12.5)')
     + xsav(21,17,9,1),ysav(21,17,9,1),zsav(21,17,9,1)
      write(6,'(''                zone 2 x,y,z(21,17,9)='',3f12.5)')
     + xsav(21,17,9,2),ysav(21,17,9,2),zsav(21,17,9,2)
      write(6,'('' Program successful... ending now'')')
      stop
      end
