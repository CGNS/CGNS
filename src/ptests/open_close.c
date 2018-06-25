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

double xtiming[2], timing[2], timingMin[2], timingMax[2];
double t1, t2;

#define cgp_doError {printf("Error at %s:%u\n",__FILE__, __LINE__); return 1;}

int main(int argc, char* argv[]) {
	int err;
	int comm_size;
	int comm_rank;
	MPI_Info info;
	int fn;
	int k;
	char fname[32];

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
	t1 = MPI_Wtime();
	if (cgp_open("open_close.cgns", CG_MODE_WRITE, &fn))
	    cgp_error_exit();
	t2 = MPI_Wtime();
	xtiming[0] = t2-t1;
	t1 = MPI_Wtime();
	if (cgp_close(fn))
	    cgp_error_exit();
	t2 = MPI_Wtime();
	xtiming[1] = t2-t1;

	MPI_Reduce(&xtiming, &timing, 2, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
	MPI_Reduce(&xtiming, &timingMin, 2, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
	MPI_Reduce(&xtiming, &timingMax, 2, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

	if(comm_rank==0) {
	  sprintf(fname, "OpenClose_time_%06d.dat", comm_size);
	  FILE *fid = fopen(fname, "w");
	  if (fid == NULL) {
	    printf("Error opening timing file!\n");
	  } else {
	    fprintf(fid,"#nprocs, cgp_open, (min, max), cgp_close, (min, max)\n %d", comm_size);
	    for (k = 0; k < 2; k++) {
	      fprintf(fid," %.3f %.3f %.3f ",timing[k]/((double)comm_size), timingMin[k], timingMax[k]);
	    }
	    fprintf(fid,"\n");
	    fclose(fid);
	  }
	}


#endif
	err = MPI_Finalize();
	if(err!=MPI_SUCCESS) cgp_doError;
	return err;
	}
