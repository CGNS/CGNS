/*
! @file test_zone.c
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

int main(int argc, char* argv[])
{
	int err;
	int comm_size;
	int comm_rank;
	MPI_Info info;
	int fn;
	int B;
	int Z;
	char zonename[100];
	int cell_dim = 3;
	int phys_dim = 3;
	cgsize_t nijk[9];

	nijk[0] = 10;
	nijk[1] = 10;
	nijk[2] = 1;
	nijk[3] = nijk[0]-1;
	nijk[4] = nijk[1]-1;
	nijk[5] = nijk[2]-1;
	nijk[6] = 0;
	nijk[7] = 0;
	nijk[8] = 0;

	err = MPI_Init(&argc,&argv);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Info_create(&(info));
	if(err!=MPI_SUCCESS) cgp_doError;

	if (cgp_open("test_zone.cgns", CG_MODE_WRITE, &fn) ||
	    cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) ||
	    cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Structured), &Z) ||
	    cgp_close(fn))
	    cgp_error_exit();

	if (cgp_open("test_zone.cgns", CG_MODE_READ, &fn) ||
	    cg_zone_read(fn, B, Z, zonename, nijk) ||
	    cgp_close(fn))
	    cgp_error_exit();

	err = MPI_Finalize();
	if(err!=MPI_SUCCESS) cgp_doError;
	return 0;
	}
