!
! @file fopen_close.F90
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Test program for pcgns library
!

PROGRAM fopen_close

  USE mpi
  USE ISO_C_BINDING
  USE testing_functions
  USE cgns
  IMPLICIT NONE

#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif

  INTEGER :: err;
  INTEGER :: comm_size
  INTEGER :: comm_rank
  INTEGER :: info
  INTEGER :: comm_self = MPI_COMM_SELF
  INTEGER :: fn
  INTEGER(C_SIZE_T), DIMENSION(1:2), TARGET :: value
  INTEGER(C_SIZE_T), TARGET :: val
  CHARACTER(LEN=2), TARGET :: valc

  CALL MPI_Init(err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  CALL MPI_Comm_rank(MPI_COMM_WORLD, comm_rank, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  CALL MPI_Info_Create(info,err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  IF(comm_rank .EQ. 0)THEN
     CALL cg_open_f("fopen_close.cgns", CG_MODE_WRITE, fn, err)
     IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

     CALL cg_close_f(fn, err)
     IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  ENDIF

  CALL MPI_Barrier(MPI_COMM_WORLD, err)
  CALL cgp_open_f("fopen_close.cgns", CG_MODE_MODIFY, fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  CALL cgp_close_f(fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL cgp_open_f("fopen_close.cgns", CG_MODE_READ, fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL cgp_close_f(fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL MPI_Barrier(MPI_COMM_WORLD, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL cgp_mpi_comm_f(comm_self, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  IF(comm_rank .EQ. 0)THEN
     CALL cgp_open_f("fopen_close.cgns", CG_MODE_READ, fn, err)
     IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
     CALL cgp_close_f(fn, err)
     IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  ENDIF

  CALL cgp_mpi_comm_f(MPI_COMM_WORLD, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL cgp_open_f("fopen_close_p.cgns", CG_MODE_WRITE, fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  CALL cgp_close_f(fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  ! test setting COMM via configure API
  val = comm_self
  CALL cg_configure_f(CG_CONFIG_HDF5_MPI_COMM, C_LOC(val), err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  IF (comm_rank .EQ. 0) THEN
     CALL cgp_open_f("fopen_close.cgns", CG_MODE_READ, fn, err)
     CALL cgp_close_f(fn, err)
     IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  ENDIF
  CALL MPI_Barrier(MPI_COMM_WORLD, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL cgp_mpi_comm_f(MPI_COMM_WORLD,err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  value(1) = 0 ! threshold for H5Pset_alignment
  value(2) = 8*1024*1024 ! alignment for H5Pset_alignment

  CALL cg_configure_f(CG_CONFIG_HDF5_ALIGNMENT, C_LOC(value(1)), err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  val = 8*1024
  CALL cg_configure_f(CG_CONFIG_HDF5_MD_BLOCK_SIZE, C_LOC(val), err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  val = 4*1024*1024
  CALL cg_configure_f(CG_CONFIG_HDF5_BUFFER, C_LOC(val), err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  val = 2*1024*1024
  CALL cg_configure_f(CG_CONFIG_HDF5_SIEVE_BUF_SIZE, C_LOC(val), err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  val = 10
  CALL cg_configure_f(CG_CONFIG_HDF5_ELINK_CACHE_SIZE, C_LOC(val), err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL cgp_open_f("ftest_cg_conf.cgns", CG_MODE_WRITE, fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  CALL cgp_close_f(fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  val = CG_CONFIG_RESET_HDF5
  CALL cg_configure_f(CG_CONFIG_RESET, C_LOC(val), err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  valc = "."//C_NULL_CHAR
  CALL cg_configure_f(CG_CONFIG_SET_PATH, C_LOC(valc), err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL cgp_open_f("ftest_cg_conf.cgns", CG_MODE_WRITE, fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()
  CALL cgp_close_f(fn, err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

  CALL MPI_Finalize(err)
  IF(err .NE. MPI_SUCCESS) CALL cgp_error_exit_f()

END PROGRAM fopen_close

