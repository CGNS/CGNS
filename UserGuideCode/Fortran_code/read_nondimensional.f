      program read_nondimensional
c
c   Opens an existing CGNS file and reads the DataClass and
c   ReferenceState appropriate for a completely
c   NONDIMENSIONAL data set.
c
c   The CGNS grid file 'grid.cgns' must already exist,
c   processed further using write_nondimensional.f.
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c read_nondimensional.f
c   ifort -o read_nondimensional read_nondimensional.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c   Note Windows machines also need to include cgnswin_f.h
c
      real*8 data
      character state*32,arrayname*32
c
c   READ NONDIMENSIONAL INFO
c   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   read DataClass under Base
      call cg_goto_f(index_file,index_base,ier,'end')
      call cg_dataclass_read_f(id,ier)
      write(6,'('' DataClass = '',a32)') DataClassName(id)
      if (DataClassName(id) .ne. 
     + 'NormalizedByUnknownDimensional') then
        write(6,'('' Error!  Expecting'',
     +   '' NormalizedByUnknownDimensional'')')
        stop
      end if
c   read ReferenceState under Base
      call cg_state_read_f(state,ier)
      write(6,'('' ReferenceState = '',a32)') state
c   Go to ReferenceState node, read Mach array and its dataclass
      call cg_goto_f(index_file,index_base,ier,'ReferenceState_t',1,
     +  'end')
c   find out how many data arrays
      call cg_narrays_f(narrays,ier)
      do n=1,narrays
        call cg_array_info_f(n,arrayname,idata,idim,idimvec,ier)
        if (idim .ne. 1 .or. idimvec .ne. 1) then
          write(6,'('' Error! expecting idim,idimvec=1,1'')')
          write(6,'(''    they are idim,idimvec='',2i5)') idim,idimvec
          stop
        end if
        call cg_array_read_as_f(n,RealDouble,data,ier)
        write(6,'('' Variable='',a32)') arrayname
        write(6,'(''     data='',f18.8)') data
      enddo
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read nondimensional info from file'',
     + '' grid.cgns'')')
      stop
      end
