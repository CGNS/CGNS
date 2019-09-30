      program read_descriptor
      use cgns
      implicit none
!
!   Reads descriptor node (under CGNSBase_t) from an existing CGNS file.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   and a descriptor node should be in it (using write_descriptor.f).
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_descriptor.F90
!   ifort -o read_descriptor read_descriptor.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
!   maxsize should match the length of 'text':
      integer, parameter :: maxsize=70
      integer isize,n,ndescriptors,index_base,index_file,ier
      character text*70,name*32
!
!  READ DESCRIPTOR FROM EXISTING CGNS FILE
!  open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!  we know there is only one base (real working code would check!)
      index_base=1
!   go to base node
      call cg_goto_f(index_file,index_base,ier,'end')
!   find out how many descriptors are here:
      call cg_ndescriptors_f(ndescriptors,ier)
      do n=1,ndescriptors
        call cg_descriptor_size_f(n,isize,ier)
        if (isize .gt. maxsize) then
          write(6,'('' Error!  must increase maxsize to at least '',           &
            i5)') isize
          stop
        end if
!   read descriptor
        call cg_descriptor_read_f(n,name,text,ier)
        write(6,'('' The descriptor is:'',/,/,a)') text
      enddo
!  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'(/,'' Successfully read descriptors from file'',                &
        '' grid.cgns'')')
      stop
      end
