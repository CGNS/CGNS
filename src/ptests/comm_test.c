/*
! @file comm_test.c
! @version 0.1
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Testing MPI Communicator. 
*/
#include <stdio.h>
#include <string.h>
#include "pcgnslib.h"
#include "mpi.h"

int main(int argc, char** argv)
{
  MPI_Comm dummy;
  char test[20];
  int index_file;
  int rank; /* rank in MPI_COMM_WORLD */
    
  snprintf(test, 20, "comm_test.cgns");

  MPI_Init(&argc, &argv);

  if( cgp_open(test, CG_MODE_WRITE, &index_file) != CG_OK) {
    printf("*FAILED* cgp_open \n");
    cgp_error_exit();
  }
  if( cgp_close(index_file) != CG_OK) {
    printf("*FAILED* cgp_close \n");
    cgp_error_exit();
  }

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  if (rank==0) 
    MPI_Comm_split(MPI_COMM_WORLD, 0, 0, &dummy);
  else
    MPI_Comm_split(MPI_COMM_WORLD, 1, rank, &dummy);

  if (rank>0) /*If member of the dummy communicator */
  {
    printf("%d\n",rank);
    if( cgp_mpi_comm(dummy) != CG_OK) {
      printf("*FAILED* cgp_mpi_comm \n");
      cgp_error_exit();
    }
    if( cgp_open(test, CG_MODE_MODIFY, &index_file) != CG_OK) {
      printf("*FAILED* cgp_open \n");
      cgp_error_exit();
    }
    if( cgp_close(index_file) != CG_OK) {
      printf("*FAILED* cgp_close \n");
      cgp_error_exit();
    }
  }
  MPI_Finalize();
  return 0;
}
