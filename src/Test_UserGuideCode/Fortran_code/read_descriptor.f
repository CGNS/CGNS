      program read_descriptor
      use cgns
c
c   Reads descriptor node (under CGNSBase_t) from an existing CGNS file.
c
c   The CGNS grid file 'grid.cgns' must already exist
c   and a descriptor node should be in it (using write_descriptor.f).
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
c   ifort -I ../.. -c read_descriptor.f
c   ifort -o read_descriptor read_descriptor.o -L ../../lib -lcgns
c
c   (../../lib is the location where the compiled
c   library libcgns.a is located)
c
c   The following is no longer supported; now superceded by "use cgns":
c     include 'cgnslib_f.h'
c   Note Windows machines need to include cgnswin_f.h
c
c   maxsize should match the length of 'text':
      parameter (maxsize=70)
      character text*70,name*32
c
c  READ DESCRIPTOR FROM EXISTING CGNS FILE
c  open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c  we know there is only one base (real working code would check!)
      index_base=1
c   go to base node
      call cg_goto_f(index_file,index_base,ier,'end')
c   find out how many descriptors are here:
      call cg_ndescriptors_f(ndescriptors,ier)
      do n=1,ndescriptors
        call cg_descriptor_size_f(n,isize,ier)
        if (isize .gt. maxsize) then
          write(6,'('' Error!  must increase maxsize to at least '',
     +      i5)') isize
          stop
        end if
c   read descriptor
        call cg_descriptor_read_f(n,name,text,ier)
        write(6,'('' The descriptor is:'',/,/,a)') text
      enddo
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'(/,'' Successfully read descriptors from file'',
     +  '' grid.cgns'')')
      stop
      end
