      program write_grid_str
c
c   Creates simple 3-D structured grid and writes it to a 
c   CGNS file.
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c write_grid_str.f
c   ifort -o write_grid_str write_grid_str.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c
c   dimension statements (note that tri-dimensional arrays
c   x,y,z must be dimensioned exactly as (21,17,N) (N>=9) 
c   for this particular case or else they will be written to 
c   the CGNS file incorrectly!  Other options are to use 1-D 
c   arrays, use dynamic memory, or pass index values to a 
c   subroutine and dimension exactly there):
      real*8 x(21,17,9),y(21,17,9),z(21,17,9)
      dimension isize(3,3)
      character basename*32,zonename*32
c
c   create gridpoints for simple example:
      ni=21
      nj=17
      nk=9
      do k=1,nk
        do j=1,nj
          do i=1,ni
            x(i,j,k)=float(i-1)
            y(i,j,k)=float(j-1)
            z(i,j,k)=float(k-1)
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
      isize(1,1)=21
      isize(2,1)=17
      isize(3,1)=9
c   cell size
      isize(1,2)=isize(1,1)-1
      isize(2,2)=isize(2,1)-1
      isize(3,2)=isize(3,1)-1
c   boundary vertex size (always zero for structured grids)
      isize(1,3)=0
      isize(2,3)=0
      isize(3,3)=0
c   create zone
      call cg_zone_write_f(index_file,index_base,zonename,isize,
     + Structured,index_zone,ier)
c   write grid coordinates (user must use SIDS-standard names here)
      call cg_coord_write_f(index_file,index_base,index_zone,RealDouble,
     + 'CoordinateX',x,index_coord,ier)
      call cg_coord_write_f(index_file,index_base,index_zone,RealDouble,
     + 'CoordinateY',y,index_coord,ier)
      call cg_coord_write_f(index_file,index_base,index_zone,RealDouble,
     + 'CoordinateZ',z,index_coord,ier)
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote grid to file grid.cgns'')')
      stop
      end
