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

	err = MPI_Init(&argc,&argv);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Info_create(&(info));
	if(err!=MPI_SUCCESS) cgp_doError;

#ifdef DEBUG_MPI
        printf("[%d]cgp_open\n",comm_rank);
        fflush(stdout);
	if (cgp_open("open_close.cgns", CG_MODE_WRITE, &fn))
	    cgp_error_exit();
        printf("[%d]cgp_close\n",comm_rank);
        fflush(stdout);
	if (cgp_close(fn))
	    cgp_error_exit();
#else
	if (cgp_open("open_close.cgns", CG_MODE_WRITE, &fn))
	    cgp_error_exit();
	if (cgp_close(fn))
	    cgp_error_exit();
#endif
	err = MPI_Finalize();
	if(err!=MPI_SUCCESS) cgp_doError;
	return err;
	}
