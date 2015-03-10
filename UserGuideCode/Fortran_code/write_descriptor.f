      program write_descriptor
c
c   Adds descriptor node to an existing CGNS file (under the 
c   CGNSBase_t node).
c
c   The CGNS grid file 'grid.cgns' must already exist.
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c write_descriptor.f
c   ifort -o write_descriptor write_descriptor.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c   Note Windows machines also need to include cgnswin_f.h
c
      character text1*36,text2*36,textstring*73
c
c  WRITE DESCRIPTOR NODE AT BASE LEVEL
c  open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c  we know there is only one base (real working code would check!)
      index_base=1
c   go to base node
      call cg_goto_f(index_file,index_base,ier,'end')
c   write descriptor node (user can give any name)
      text1='Supersonic vehicle with landing gear'
      text2='M=4.6, Re=6 million'
      textstring=text1//char(10)//text2
      call cg_descriptor_write_f('Information',textstring,ier)
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote descriptor node to file'',
     +  '' grid.cgns'')')
      stop
      end
