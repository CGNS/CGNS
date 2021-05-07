PROGRAM pcgns_ftest

  USE mpi
  USE ISO_C_BINDING
  USE CGNS
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif

  INTEGER(cgsize_t), PARAMETER :: totcnt = 40320 * 10
  INTEGER, PARAMETER :: NLOOPS = 5000
  INTEGER(cgsize_t) npp
  INTEGER(C_INT) commsize, commrank, mpi_err
  INTEGER i, nb, nz, nerrs
  INTEGER ierr, F, B, Z, E, S
  INTEGER Cx, Cy, Cz, Fx, Fy, Fz, Ax, Ay, Az
  INTEGER(cgsize_t) sizes(3), start, END, n, j
  INTEGER(cgsize_t), PARAMETER :: start_1 = 1
  REAL*8 ts, te, tt, dsize
  REAL*8 dx(totcnt), dy(totcnt), dz(totcnt)
  INTEGER(cgsize_t), ALLOCATABLE, DIMENSION(:) :: ie
  CHARACTER*32 name
  CHARACTER*11 piomode(2)
  INTEGER :: istat
  INTEGER :: precision
  INTEGER, TARGET :: value

  DATA piomode /'independent','collective'/

  CALL MPI_INIT(mpi_err)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,commsize,mpi_err)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,commrank,mpi_err)

  IF (commsize .GT. 8) THEN
     IF (commrank .EQ. 0) &
          PRINT *, 'number of processes must be 8 or less'
     CALL cgp_error_exit_f
     STOP
  ENDIF

  ALLOCATE(ie(1:4*totcnt), STAT = istat)
  IF (istat.NE.0)THEN
     PRINT*, '*FAILED* allocation of ie'
     CALL cgp_error_exit_f()
  ENDIF

  npp = totcnt / commsize
  start = commrank * npp + 1
  end = start + npp - 1

  j = 0
  DO n=1,npp
     dx(n) = start + n - 1
     dy(n) = commrank + 1
     dz(n) = n
     DO i=1,4
        j = j + 1
        ie(j) = start + n - 1
     ENDDO
  ENDDO
  sizes(1) = totcnt
  sizes(2) = totcnt
  sizes(3) = 0

  dsize = (9.0 * totcnt * 8.0 + 4.0 * totcnt * 4.0) / (1024.0 * 1024.0)

  IF (commrank .EQ. 0) THEN
     PRINT *,'number processes       =', commsize
     PRINT *,'array size per process =', npp
     PRINT *,'total array size       =', totcnt
     PRINT *,'total Mb for all data  =', dsize
  ENDIF

  ! default is MPI_COMM_WORLD, but can set another communicator with this
  ! call cgp_mpi_comm_f(MPI_COMM_WORLD,ierr)

  ! Check repeated opening and closing of a file to detect issues with 
  ! missed closed HDF5 objects, CGNS-109.

  ! Note, this is extremely slow when used with Github Actions 
  ! and Linux, so it is disabled in that case.
#ifndef DISABLE_CGNS109
  DO n = 1, NLOOPS
     CALL cgp_open_f('pcgns_ftest.cgns',CG_MODE_WRITE,F,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_close_f(F,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  ENDDO
#endif

  CALL cgp_open_f('pcgns_ftest.cgns',CG_MODE_WRITE,F,ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  DO nb=1,2
     WRITE(name,'(a4,i2)') 'Base',nb
     CALL cg_base_write_f(F,name,3,3,B,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_pio_mode_f(INT(nb-1,C_INT), ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     DO nz=1,2
        WRITE(name,'(a4,i2)') 'Zone',nz
        CALL cg_zone_write_f(F,B,name,sizes,CGNS_ENUMV(Unstructured),Z,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_f(F,B,Z,CGNS_ENUMV(RealDouble),'CoordinateX',Cx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_f(F,B,Z,CGNS_ENUMV(RealDouble),'CoordinateY',Cy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_f(F,B,Z,CGNS_ENUMV(RealDouble),'CoordinateZ',Cz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_section_write_f(F,B,Z,'Tets',CGNS_ENUMV(TETRA_4),start_1,totcnt,0,E,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_sol_write_f(F,B,Z,'Solution',CGNS_ENUMV(Vertex),S,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_f(F,B,Z,S,CGNS_ENUMV(RealDouble),'MomentumX',Fx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_f(F,B,Z,S,CGNS_ENUMV(RealDouble),'MomentumY',Fy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_f(F,B,Z,S,CGNS_ENUMV(RealDouble),'MomentumZ',Fz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_goto_f(F,B,ierr,name,0,'end')
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_user_data_write_f('User Data',ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_gorel_f(F,ierr,'User Data',0,'end')
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_f('ArrayX',CGNS_ENUMV(RealDouble),1,totcnt,Ax,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_f('ArrayY',CGNS_ENUMV(RealDouble),1,totcnt,Ay,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_f('ArrayZ',CGNS_ENUMV(RealDouble),1,totcnt,Az,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
        ts = MPI_WTIME()

        CALL cgp_coord_write_data_f(F,B,Z,Cx,start,END,dx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_data_f(F,B,Z,Cy,start,END,dy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_coord_write_data_f(F,B,Z,Cz,start,END,dz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_elements_write_data_f(F,B,Z,E,start,END,ie,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_data_f(F,B,Z,S,Fx,start,END,dx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_data_f(F,B,Z,S,Fy,start,END,dy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_field_write_data_f(F,B,Z,S,Fz,start,END,dz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cg_goto_f(F,B,ierr,'Zone_t',Z,'UserDefinedData_t',1,'end')
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_data_f(Ax,start,END,dx,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_data_f(Ay,start,END,dy,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
        CALL cgp_array_write_data_f(Az,start,END,dz,ierr)
        IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

        CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
        te = MPI_WTIME()
        tt = te - ts;
        IF (commrank .EQ. 0) &
             PRINT *,'write:',tt,' secs,', dsize/tt, ' Mb/sec (', piomode(nb),')'
     ENDDO
  ENDDO
  CALL cgp_close_f(F,ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  CALL cgp_open_f('pcgns_ftest.cgns',CG_MODE_READ,F,ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  CALL cg_precision_f(F, PRECISION, ierr)
  IF (commrank .EQ. 0) PRINT*,'cg_precision_f PRECISION ',PRECISION

  Z = 1
  S = 1
  E = 1
  DO B=1,2
     CALL cgp_pio_mode_f(INT(B-1,C_INT),ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     ts = MPI_WTIME()

     CALL cgp_coord_read_data_f(F,B,Z,1,start,END,dx,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_coord_read_data_f(F,B,Z,2,start,END,dy,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_coord_read_data_f(F,B,Z,3,start,END,dz,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_elements_read_data_f(F,B,Z,E,start,END,ie,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     te = MPI_WTIME()
     tt = te - ts

     nerrs = 0
     j = 0
     DO n=1,npp
        IF (dx(n) .NE. (start+n-1)) nerrs = nerrs + 1
        IF (dy(n) .NE. (commrank+1)) nerrs = nerrs + 1
        IF (dz(n) .NE. n) nerrs = nerrs + 1
        DO i=1,4
           j = j + 1
         !  IF (ie(j) .NE. (start+n-1)) nerrs = nerrs + 1
        ENDDO
     ENDDO

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     ts = MPI_WTIME()

     CALL cgp_field_read_data_f(F,B,Z,S,1,start,END,dx,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_field_read_data_f(F,B,Z,S,2,start,END,dy,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_field_read_data_f(F,B,Z,S,3,start,END,dz,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     te = MPI_WTIME()
     tt = tt + te - ts

     DO n=1,npp
        IF (dx(n) .NE. (start+n-1)) nerrs = nerrs + 1
        IF (dy(n) .NE. (commrank+1)) nerrs = nerrs + 1
        IF (dz(n) .NE. n) nerrs = nerrs + 1
     ENDDO

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     ts = MPI_WTIME()
     CALL cg_goto_f(F,B,ierr,'Zone_t',Z,'UserDefinedData_t',1,'end')
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_array_read_data_f(1,start,END,dx,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_array_read_data_f(2,start,END,dy,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_array_read_data_f(3,start,END,dz,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

     CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_err)
     te = MPI_WTIME()
     tt = tt + te - ts

     DO n=1,npp
        IF (dx(n) .NE. (start+n-1)) nerrs = nerrs + 1
        IF (dy(n) .NE. (commrank+1)) nerrs = nerrs + 1
        IF (dz(n) .NE. n) nerrs = nerrs + 1
     ENDDO

     IF (commrank .EQ. 0) THEN
        PRINT *,'read :',tt,' secs,',dsize/tt,' Mb/sec (', &
             piomode(B),') errors =',nerrs
     ENDIF
  ENDDO

  CALL cgp_close_f(F,ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

! Disable with gfortran, GCC Bugzilla - Bug 99982
#ifndef __GFORTRAN__ 
  ! test cg_configure_f
  value = MPI_COMM_SELF
  CALL cg_configure_f(CG_CONFIG_HDF5_MPI_COMM, C_LOC(value), ierr)
  IF (ierr .NE. CG_OK) CALL cgp_error_exit_f

  IF (commrank .EQ. 0) THEN
     CALL cgp_open_f('pcgns_ftest.cgns',CG_MODE_READ,F,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
     CALL cgp_close_f(F,ierr)
     IF (ierr .NE. CG_OK) CALL cgp_error_exit_f
  ENDIF
#endif
  CALL MPI_FINALIZE(mpi_err)
END PROGRAM pcgns_ftest

