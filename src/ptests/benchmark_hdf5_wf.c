/*
! @file benchmark_hdf5.F90
! @author M. Scot Breitenfeld <brtnfld@hdfgroup.org>
! @version 0.1
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Benchmarking program for pcgns library
!
! TO COMPILE: h5pcc -O2 benchmark_hdf5.c -I.. -L../lib -lcgns
!
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "pcgnslib.h"
#include "mpi.h"

#define false 0
#define true 1

int comm_size;
int comm_rank;
MPI_Info info;

/* cgsize_t Nelem = 33554432; */
cgsize_t Nelem = 262144;
cgsize_t NodePerElem = 512;

cgsize_t Nnodes;
int mpi_err;
int err;
int comm_size;
int comm_rank;
int fn;
int B;
int Z;
int S;
int Cx,Cy,Cz, Fx, Fy, Fz, Ar, Ai;
int cell_dim = 3;
int phys_dim = 3;
int r_cell_dim = 0;
int r_phys_dim = 0;
cgsize_t nijk[3], sizes[3];
cgsize_t size_1D[1];
cgsize_t min, max;
int k, count;
/* For writing and reading data*/
double* Coor_x;
double* Coor_y;
double* Coor_z;
double* Data_Fx;
double* Data_Fy;
double* Data_Fz;
double* Array_r;
cgsize_t* Array_i;
cgsize_t start, end, emin, emax;
cgsize_t* elements;
char name[33];
int queue, debug;
double t0, t1, t2;

/*
 * Timing storage convention:
 * timing(0) = Total program time
 * timing(1) = Time to write nodal coordinates
 * timing(2) = Time to write connectivity table
 * timing(3) = Time to write solution data (field data)
 * timing(4) = Time to write array data
 * timing(5) = Time to read nodal coordinates
 * timing(6) = Time to read connectivity table
 * timing(7) = Time to read solution data (field data)
 * timing(8) = Time to read array data
 * timing(9) = Time for cgp_open, CG_MODE_WRITE
 * timing(10) = Time for cg_base_write
 * timing(11) = Time for cg_zone_write
 * timing(12) = Time for cgp_open, CG_MODE_READ
 * timing(13) = Time for cg_read_write
 * timing(14) = Time for cg_read_write
 */
double xtiming[15], timing[15], timingMin[15], timingMax[15];

int piomode[2] = {0, 1};
int piomode_i;

int initialize(int* argc, char** argv[]) {
	MPI_Init(argc,argv);
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
	MPI_Info_create(&info);
	MPI_Info_set(info, "striping_unit", "8388608");
	/* or whatever your GPFS block size actually is*/
	return 0;
}

int c_double_eq(double a, double b) {

  double eps = 1.e-8;

  if(fabs(a-b) < eps) {
    return true;
  }
  return false;
}

int main(int argc, char* argv[]) {
  /* Initialize variables */
  initialize(&argc,&argv);

  char fname[32];
  char name[32];
  int Cvec[3];
  int Fvec[3];
  int Avec[2];

  /* parameters */
  piomode_i = 1;
  queue = false;
  debug = false;

  t0 = MPI_Wtime(); /* Timer */

  err = (int)cgp_mpi_info(info)
  err = (int)cgp_pio_mode((CGNS_ENUMT(PIOmode_t))piomode_i);

  Nnodes = Nelem*NodePerElem;

  nijk[0] = Nnodes; /* Number of vertices */
  nijk[1] = Nelem; /* Number of cells */
  nijk[2] = 0; /* Number of boundary vertices */

  /* ====================================== */
  /* ==    **WRITE THE CGNS FILE **      == */
  /* ====================================== */

  sprintf(fname, "benchmark_%06d.cgns", comm_size);

  t1 = MPI_Wtime();
  if(cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
    printf("*FAILED* cgp_open \n");
    cgp_error_exit();
  }
  t2 = MPI_Wtime();
  xtiming[9] = t2-t1;

  t1 = MPI_Wtime();
  if(cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) != CG_OK) {
    printf("*FAILED* cg_base_write \n");
    cgp_error_exit();
  }
  t2 = MPI_Wtime();
  xtiming[10] = t2-t1;

  t1 = MPI_Wtime();
  if(cg_zone_write(fn, B, "Zone 1", nijk, Unstructured, &Z) != CG_OK) {
    printf("*FAILED* cg_zone_write \n");
    cgp_error_exit();
  t2 = MPI_Wtime();
  xtiming[11] = t2-t1;

  }

  /* ====================================== */
  /* == (C) WRITE THE FIELD DATA         == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fx \n");
    cgp_error_exit();
  }

  if( !(Data_Fy= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fy \n");
    cgp_error_exit();
  }

  if( !(Data_Fz= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fz \n");
    cgp_error_exit();
  }

  for ( k = 0; k < count; k++) {
     Data_Fx[k] = comm_rank*count+k + 1.01;
     Data_Fy[k] = comm_rank*count+k + 1.02;
     Data_Fz[k] = comm_rank*count+k + 1.03;
  }

  if(cg_sol_write(fn, B, Z, "Solution", Vertex, &S) != CG_OK) {
    printf("*FAILED* cg_sol_write \n");
    cgp_error_exit();
  }

  if(cgp_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumX",&Fx) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumX) \n");
    cgp_error_exit();
  }
  if(cgp_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumY",&Fy) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumY) \n");
    cgp_error_exit();
  }
  if(cgp_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumZ",&Fz) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumZ) \n");
    cgp_error_exit();
  }

  t1 = MPI_Wtime();


#if HDF5_HAVE_MULTI_DATASETS

  Fvec[0] = Fx;
  Fvec[1] = Fy;
  Fvec[2] = Fz;

  if(cgp_field_multi_write_data(fn,B,Z,S,Fvec,&min,&max,3,Data_Fx,Data_Fy,Data_Fz) != CG_OK) {
    printf("*FAILED* cgp_field_multi_write_data \n");
    cgp_error_exit();
  }
#else
  if(cgp_field_write_data(fn,B,Z,S,Fx,&min,&max,Data_Fx) != CG_OK) {
    printf("*FAILED* cgp_field_write_data (Data_Fx) \n");
    cgp_error_exit();
  }
  if(cgp_field_write_data(fn,B,Z,S,Fy,&min,&max,Data_Fy) != CG_OK) {
    printf("*FAILED* cgp_field_write_data (Data_Fy)\n");
    cgp_error_exit();
  }
  if(cgp_field_write_data(fn,B,Z,S,Fz,&min,&max,Data_Fz) != CG_OK) {
    printf("*FAILED* cgp_field_write_data (Data_Fz)\n");
    cgp_error_exit();
  }
#endif
  t2 = MPI_Wtime();
  xtiming[3] = t2-t1;

  if(!queue) {
    free(Data_Fx);
    free(Data_Fy);
    free(Data_Fz);
  }

  if(cgp_close(fn) !=CG_OK) {
     printf("*FAILED* cgp_close\n");
     cgp_error_exit();
  }

  xtiming[0] = t2-t0;

  MPI_Reduce(&xtiming, &timing, 15, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  MPI_Reduce(&xtiming, &timingMin, 15, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
  MPI_Reduce(&xtiming, &timingMax, 15, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

  if(comm_rank==0) {
    sprintf(fname, "timing_%06d_%d.dat", comm_size, piomode_i+1);
    FILE *fid = fopen(fname, "w");
    if (fid == NULL) {
      printf("Error opening timing file!\n");
    } else {
      fprintf(fid,"#nprocs, wcoord, welem, wfield, warray, rcoord, relem, rfield, rarray \n%d", comm_size);

      for ( k = 0; k < 15; k++) {
	fprintf(fid," %20f %20f %20f ",timing[k]/((double) comm_size), timingMin[k], timingMax[k]);
      }
      fprintf(fid,"\n");
    }
  }

  MPI_Finalize();

  return 0;
}


