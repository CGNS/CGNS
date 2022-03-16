
PROGRAM ftest_zone

  USE cgns
  USE mpi
  
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif
      
  INTEGER ierr, mrank, msize, fid 
  INTEGER basenum, zonenum, blocknum
  INTEGER blocksize, subblocksize, lowerblock, upperblock
  INTEGER nharms, ipde, n, npde, solnum 
  INTEGER fieldnum, arraynum
  INTEGER xnum, ynum, znum, i, j, k, l, m
  INTEGER(cgsize_t) sizes(3,3)
  INTEGER(cgsize_t) qsizes(5),minarrrange(5),maxarrrange(5)
  CHARACTER*11 zonename,tempzonename,fieldname
  DOUBLE PRECISION starttime, endtime
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: q
  DOUBLE PRECISION  tempq(5,5,5,5,5)


  CALL MPI_INIT(ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,msize,ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,mrank,ierr)
  
  npde = 5
  nharms = 5
  qsizes=5
  qsizes(4)=npde
  qsizes(5)=nharms
  
  blocksize= 128
  subblocksize = blocksize/msize
  
  IF(MOD(blocksize,msize) .NE. 0) THEN
     WRITE(*,*) 'error with block size'
  END IF
  
  sizes(1,1) = 20
  sizes(2,1) = 20
  sizes(3,1) = 20
  
  sizes(1,2) = sizes(1,1)-1
  sizes(2,2) = sizes(2,1)-1
  sizes(3,2) = sizes(3,1)-1
  
  sizes(1,3) = 0
  sizes(2,3) = 0
  sizes(3,3) = 0
  
  
  ALLOCATE(q(5,5,5,npde,nharms,subblocksize))

  fid = 0
  
  IF(mrank .EQ. 0) THEN
     
     WRITE(*,*) 'starting restart stuff'
     
     CALL cg_open_f('rest.cgns',CG_MODE_WRITE,fid,ierr)     
     IF(ierr .NE. CG_OK) THEN
        WRITE(*,*) 'cg_open_f restart error'
        CALL cg_error_print_f()
     END IF
     
     starttime = MPI_WTIME()
     
     CALL cg_base_write_f(fid,'gridbase',3,3,basenum,ierr)
     IF(ierr .NE. CG_OK) THEN
        WRITE(*,*) 'cg_base_write_f error'
        CALL cg_error_print_f()
     END IF
     
     endtime = MPI_WTIME()
     IF(mrank .EQ. 0) THEN
        WRITE(*,*) 'base write',endtime-starttime
     END IF
     
     starttime = MPI_WTIME()
     
     DO i = 1,blocksize
        blocknum = i
        WRITE(zonename, "(A5,I6)") "block",blocknum
        CALL cg_zone_write_f(fid,basenum,zonename,sizes, &
             CGNS_ENUMV(Structured),zonenum,ierr)
        IF(ierr .NE. CG_OK) THEN
           WRITE(*,*) 'cg_zone_write_f error'
           CALL cg_error_print_f()
        END IF
        CALL cg_goto_f(fid,basenum,ierr,'Zone_t',zonenum,'end')
        IF(ierr .NE. CG_OK) THEN
           WRITE(*,*) 'cg_goto_f error'
           CALL cg_error_print_f()
        END IF
        CALL cg_user_data_write_f('User Data',ierr)
        
        IF(ierr .NE. CG_OK) THEN
           WRITE(*,*) 'cg_user_write_f error'
           CALL cg_error_print_f()
        END IF
     END DO
     
     endtime = MPI_WTIME()
     IF(mrank .EQ. 0) THEN
        WRITE(*,*) 'zone and user data write',endtime-starttime
     END IF
     
     CALL cg_close_f(fid,ierr)
     IF(ierr .NE. CG_OK) THEN
        WRITE(*,*) 'cg_close_f error'
        CALL cg_error_print_f()
     END IF
     
  END IF

  basenum = 1
  
  IF(mrank .EQ. 0) THEN
     WRITE(*,*) 'restart',mrank,msize
  END IF
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  starttime = MPI_WTIME()
  
  CALL cgp_open_f('rest.cgns',CG_MODE_MODIFY,fid,ierr)     
  IF(ierr .NE. CG_OK) THEN
     WRITE(*,*) 'cgp_open_f error parallelwrite'
     CALL cg_error_print_f()
  END IF
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  endtime = MPI_WTIME()
  IF(mrank .EQ. 0) THEN
     WRITE(*,*) 'open restart file',endtime-starttime
  END IF
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  starttime = MPI_WTIME()
  
  solnum = 1
  
  DO i = 1,blocksize
     zonenum = i
     CALL cg_goto_f(fid,basenum,ierr,'Zone_t',zonenum, &
          'UserDefinedData_t',solnum,'end')
     WRITE(fieldname, "(A5,I6)") "array",1
     CALL cgp_array_write_f(fieldname,CGNS_ENUMV(RealDouble),5,qsizes, &
          arraynum,ierr)
     IF(ierr .NE. CG_OK) THEN
        WRITE(*,*) 'cg_array_write_f error'
        CALL cg_error_print_f()
     END IF
  END DO
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  endtime = MPI_WTIME()
  IF(mrank .EQ. 0) THEN
     WRITE(*,*) 'initial array create',endtime-starttime
  END IF
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  starttime = MPI_WTIME()
  
  lowerblock = mrank*subblocksize+1
  upperblock = lowerblock + subblocksize - 1
  
  
  minarrrange = 1
  maxarrrange = 5
  PRINT*,'npde,nharms',npde,nharms
  maxarrrange(4) = npde
  maxarrrange(5) = nharms

  solnum = 1
  
  blocknum = 1
  DO i = lowerblock,upperblock
     zonenum = i
     q(:,:,:,:,:,blocknum) = i*101
     arraynum = 1
     CALL cg_goto_f(fid,basenum,ierr,'Zone_t',zonenum, &
          'UserDefinedData_t',solnum,'end')
     IF(ierr .NE. CG_OK) THEN
        WRITE(*,*) 'cg_goto_f error'
        CALL cg_error_print_f()
     END IF
     CALL cgp_array_write_data_f(arraynum,minarrrange,maxarrrange, &
          q(:,:,:,:,:,blocknum),ierr)
     IF(ierr .NE. CG_OK) THEN
        WRITE(*,*) 'q cgp_array_write_data_f error',ierr
        CALL cg_error_print_f()
     END IF
     
     blocknum = blocknum + 1
  END DO
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  endtime = MPI_WTIME()
  IF(mrank .EQ. 0) THEN
     WRITE(*,*) 'array data write',endtime-starttime
  END IF
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  
  
  starttime = MPI_WTIME()
  
  blocknum = 1
  solnum = 1
  
  DO i = lowerblock,upperblock
     
     zonenum = i
     tempq = 0.0
     arraynum = 1
     
     CALL cg_goto_f(fid,basenum,ierr,'Zone_t',zonenum, &
          'UserDefinedData_t',solnum,'end')
     IF(ierr .NE. CG_OK) THEN
        WRITE(*,*) 'cg_goto_f error'
        CALL cg_error_print_f()
     END IF
     CALL cgp_array_read_data_f(arraynum,minarrrange,maxarrrange, &
          tempq(:,:,:,:,:),ierr)
     IF(ierr .NE. CG_OK) THEN
        WRITE(*,*) 'q cgp_array_read_data_f error'
        CALL cg_error_print_f()
     END IF
     
     
     DO n = 1,nharms
        DO j = 1,npde
           DO k = 1,5
              DO l = 1,5
                 DO m = 1,5
                    IF(q(m,l,k,j,n,blocknum) .NE. &
                         tempq(m,l,k,j,n)) THEN
                       WRITE(*,*) 'error with solution', &
                            q(m,l,k,j,n,blocknum), &
                            tempq(m,l,k,j,n)
                    END IF
                 END DO
              END DO
           END DO
        END DO
     END DO
     
     blocknum = blocknum + 1
         
  END DO
  

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  endtime = MPI_WTIME()
  IF(mrank .EQ. 0) THEN
     WRITE(*,*) 'array data read',endtime-starttime
  END IF
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  starttime = MPI_WTIME()
  
  CALL cgp_close_f(fid,ierr)
  IF(ierr .NE. CG_OK) THEN
     WRITE(*,*) 'cgp_close_f error'
     CALL cg_error_print_f()
  END IF
  
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  endtime = MPI_WTIME()
  IF(mrank .EQ. 0) THEN
     WRITE(*,*) 'close file',endtime-starttime
  END IF
  
  DEALLOCATE(q)
  
  
  CALL MPI_FINALIZE(ierr)

END PROGRAM ftest_zone
