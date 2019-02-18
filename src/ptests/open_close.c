/*
! @file open_close.c
! @author Kyle Horne <horne.kyle@gmail.com>
! @version 0.2
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Test program for pcgns library
*/

#include <stdio.h>
#include <stdlib.h>

#include "pcgnslib.h"
#include "mpi.h"

#define cgp_doError {printf("Error at %s:%u\n",__FILE__, __LINE__); return 1;}

int main(int argc, char* argv[]) {

  int err;
  int comm_size;
  int comm_rank;
  MPI_Info info;
  int fn;
  int modes[6] ={CG_MODE_WRITE, CG_MODE_READ, CG_MODE_MODIFY,
                 CG_MODE_WRITE_SERIAL, CG_MODE_READ_SERIAL, CG_MODE_MODIFY_SERIAL};
  int i;

  err = MPI_Init(&argc,&argv);
  if(err!=MPI_SUCCESS) cgp_doError;
  err = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
  if(err!=MPI_SUCCESS) cgp_doError;
  err = MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
  if(err!=MPI_SUCCESS) cgp_doError;
  err = MPI_Info_create(&(info));
  if(err!=MPI_SUCCESS) cgp_doError;

#ifdef DEBUG_MPI
  for (i=0;i<6;i++) {
    if (comm_rank==0) printf("Mode %d:\n",modes[i]);
    fflush(stdout);
    if (modes[i] <= CG_MODE_MODIFY || (modes[i] > CG_MODE_MODIFY && comm_rank==0)) {
      printf("[%d]cgp_open\n",comm_rank);
      fflush(stdout);
      if (cgp_open("open_close.cgns", modes[i], &fn)) {
        printf("Open Error\n");cgp_error_exit();}
      printf("[%d]cgp_close\n",comm_rank);
      fflush(stdout);
      if (cgp_close(fn)){
        printf("Close Error\n"); cgp_error_exit();    }
    }
  }

#else
  for (i=0;i<6;i++) {
    if (modes[i] <= CG_MODE_MODIFY || (modes[i] > CG_MODE_MODIFY && comm_rank==0)) {
      if (cgp_open("open_close.cgns", modes[i], &fn))
        cgp_error_exit();
      if (cgp_close(fn))
        cgp_error_exit();
    }
  }

#endif
  err = MPI_Finalize();
  if(err!=MPI_SUCCESS) cgp_doError;
  return err;
}
