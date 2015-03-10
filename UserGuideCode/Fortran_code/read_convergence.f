      program read_convergence
c
c   Reads convergence history from an existing CGNS file.
c
c   The CGNS grid file 'grid.cgns' must already exist
c   and a convergence history should be in it (using write_convergence.f).
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths!):
c
c   ifort -I ../CGNS_CVS/cgnslib_2.5 -c read_convergence.f
c   ifort -o read_convergence read_convergence.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns
c
c   (../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
c   library libcgns.a is located)
c
c   cgnslib_f.h file must be located in directory specified by -I during compile:
      include 'cgnslib_f.h'
c   Note Windows machines also need to include cgnswin_f.h
c
      parameter (ntt=20)
      dimension cl(ntt)
      character arrayname*32
c
c  READ CONVERGENCE HISTORY INFORMATION FROM EXISTING CGNS FILE
c  open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c  we know there is only one base (real working code would check!)
      index_base=1
c   go to base node
      call cg_goto_f(index_file,index_base,ier,'end')
c   go to history node (we assume it exists and that there is only one - 
c   real working code would check!)
      call cg_goto_f(index_file,index_base,ier,'ConvergenceHistory_t',
     +  1,'end')
c   find out how many arrays are here (there should be only one!):
      call cg_narrays_f(narrays,ier) 
      index_array=narrays
c   some checks:
      if (narrays .ne. 1) then
        write(6,'('' Error!  Expecting only one array, read'',i5)')
     +    narrays
        stop
      end if
      call cg_array_info_f(index_array,arrayname,itype,ndim, 
     +  idim,ier)
      if (idim .gt. ntt) then
        write(6,'('' Error! must increase ntt to at least '',i5)') idim
        stop
      end if
      if (arrayname .ne. 'CoefLift') then
        write(6,'('' Error!  expecting CoefLift, read'',a32)')
     +    arrayname
        stop
      end if
c   read lift coefficient array
      call cg_array_read_as_f(index_array,RealSingle,cl,ier)
c  close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read cl history from file grid.cgns'')')
      write(6,'(''    values are: '',5f12.5)') cl(1),cl(2),cl(3),
     +  cl(4),cl(5)
      write(6,'(''                '',5f12.5)') cl(6),cl(7),cl(8),
     +  cl(9),cl(10)
      stop
      end
