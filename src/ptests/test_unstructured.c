/*
! @file test_unstructured.c
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
	int B;
	int Z;
	int S;
	int Cx,Cy,Cz;
	int cell_dim = 3;
	int phys_dim = 3;
	cgsize_t nijk[3], min, max;
	int k, count;
	double *x, *y, *z;
	int nelems;
	cgsize_t start, end, emin, emax, *elements;

	nijk[0] = 10;
	nijk[1] = nijk[0]-1;
	nijk[2] = 0;

	err = MPI_Init(&argc,&argv);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Info_create(&(info));
	if(err!=MPI_SUCCESS) cgp_doError;

	if (cgp_open("test_unstructured.cgns", CG_MODE_WRITE, &fn) ||
	    cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) ||
	    cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z))
	    cgp_error_exit();

	count = 10 / comm_size;
	x = (double *)malloc(count*sizeof(double));
	y = (double *)malloc(count*sizeof(double));
	z = (double *)malloc(count*sizeof(double));

	min = count*comm_rank+1;
	max = count*(comm_rank+1);

	for(k=0;k<count;k++) {
		x[k] = (double) (min+k);
		y[k] = 0.0;
		z[k] = 0.0;
		}

	if (cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX",&Cx) ||
	    cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY",&Cy) ||
	    cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ",&Cz))
	    cgp_error_exit();

	if (cgp_coord_write_data(fn,B,Z,Cx,&min,&max,x) ||
	    cgp_coord_write_data(fn,B,Z,Cy,&min,&max,y) ||
	    cgp_coord_write_data(fn,B,Z,Cz,&min,&max,z))
	    cgp_error_exit();

	start = 1;
	end = 9;
	if (cgp_section_write(fn,B,Z,"Elements",CGNS_ENUMV(BAR_2),start,end,0,&S))
	    cgp_error_exit();

	nelems = (comm_rank!=comm_size-1)?9/comm_size:9-(9/comm_size)*(comm_size-1);
	printf("%d:%d\n",comm_rank,nelems);
	emin = (9/comm_size)*comm_rank+1;
	emax = (comm_rank!=comm_size-1)?(9/comm_size)*(comm_rank+1):9;

	elements = (cgsize_t *)malloc(nelems*2*sizeof(cgsize_t));
	for(k=0;k<nelems;k++) {
		elements[2*k] = k+emin;
		elements[2*k+1] = k+emin+1;
		}
	printf("%d:%d %d %d\n",comm_rank,nelems,(int)emin,(int)emax);

	if (cgp_elements_write_data(fn,B,Z,S,emin,emax,elements))
	    cgp_error_exit();

	if (cgp_close(fn)) cgp_error_exit();

	err = MPI_Finalize();
	if(err!=MPI_SUCCESS) cgp_doError;
	return 0;
	}
