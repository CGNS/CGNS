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
! Results From Largest Simulation to-date:
! --------------------------------------------
!  1/2/2015
!  Cetus@Argonne National Laboratory: IBM Blue Gene/Q, GPFS
!  (Note: uses Multi-dataset APIs)
!  Number of Elements = 8,589,934,592
!  Number of Nodes    = 51,539,607,552
!  Resulting File Size = 3.375 TiB
!
!  Total Time to Run Program (@4096 processors) 1379s
!  Total Time to Write Coordinates: 215s (71s in MPI IO)
!  Total Time to Write Elements: 71s (0.04s in MPI IO)
!  Total Time to Write Fields: 216s (72s in MPI IO)
!  Total Time to Write Arrays: 143s (71s in MPI IO)
!  Total Time to Read Coordinates: 237s (71s in MPI IO)
!  Total Time to Read Elements: 79s (0.04s in MPI IO)
!  Total Time to Read Fields: 237s (78s in MPI IO)
!  Total Time to Read Arrays: 158s (79s in MPI IO)

!  Number of Elements = 1,073,741,824
!  Number of Nodes    = 6,442,450,944
!  Resulting File Size = 431 GiB
!
!  Total Time to Run Program (@256 processors) 439s
!  Total Time to Write Coordinates: 59s
!  Total Time to Write Elements: 20s
!  Total Time to Write Fields: 61s
!  Total Time to Write Arrays: 40s
!  Total Time to Read Coordinates: 83s
!  Total Time to Read Elements: 27s
!  Total Time to Read Fields: 83s
!  Total Time to Read Arrays: 55s
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include <limits.h>
#include "pcgnslib.h"
#include "mpi.h"

#if SIZE_MAX == UCHAR_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_CHAR
#elif SIZE_MAX == USHRT_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_SHORT
#elif SIZE_MAX == UINT_MAX
   #define MPI_SIZE_T MPI_UNSIGNED
#elif SIZE_MAX == ULONG_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_LONG
#elif SIZE_MAX == ULLONG_MAX
   #define MPI_SIZE_T MPI_UNSIGNED_LONG_LONG
#else
   #error "size_t size not found"
#endif

#define false 0
#define true 1

int comm_size;
int comm_rank;
MPI_Info info;


int piomode = CGP_COLLECTIVE; /* DEFAULT */ 
/* cgsize_t Nelem = 33554432; */
cgsize_t Nelem = 65536; /* DEFAULT */
cgsize_t NodePerElem = 6;

cgsize_t Nnodes;
int mpi_err;

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
cgsize_t k, count;
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
int  debug;
double t0, t1, t2;

/*
 * Timing storage convention:                            avg.| min. | max.
 * timing(0) = Total program time,                        2,    3,    4
 * timing(1) = Time to write nodal coordinates            5,    6,    7
 * timing(2) = Time to write connectivity table           8,    9,    10
 * timing(3) = Time to write solution data (field data)  11,   12,    13
 * timing(4) = Time to write array data                  14,   15,    16
 * timing(5) = Time to read nodal coordinates            17,   18,    19
 * timing(6) = Time to read connectivity table,          20,   21,    22
 * timing(7) = Time to read solution data (field data)   23,   24,    25
 * timing(8) = Time to read array data                   26,   27,    28
 * timing(9) = Time for cgp_open, CG_MODE_WRITE          29,   30,    31
 * timing(10) = Time for cgp_open, CG_MODE_READ          32,   33,    34
 * timing(11) = Time for cg_close, WRITE                 35,   36,    37
 * timing(12) = Time for cg_close, READ                  38,   39,    40
 * timing(13) = Time for cg_base_write, cg_zone_write    41,   42,    43
 * timing(14) = Time for cg_read base, cg_zone_read      44,   45,    46
 */
double xtiming[15], timing[15], timingMin[15], timingMax[15];

int read_inputs(int* argc, char*** argv) {
  int k;

  if(comm_rank==0) {
    for(k=1;k<*argc;k++) {
      if(strcmp((*argv)[k],"-nelem")==0) {
        k++;
        sscanf((*argv)[k],"%zu",&Nelem);
      }
      if(strcmp((*argv)[k],"-ind")==0) {
        piomode = CGP_INDEPENDENT; 
      }

    }
  }
  MPI_Bcast(&Nelem, 1, MPI_SIZE_T, 0, MPI_COMM_WORLD);
  MPI_Bcast(&piomode, 1, MPI_INT, 0, MPI_COMM_WORLD);

  return 0;
}

int initialize(int* argc, char** argv[]) {
  MPI_Init(argc,argv);
  MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
  MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
  MPI_Info_create(&info);
  MPI_Info_set(info, "striping_unit", "8388608");
  /* or whatever your GPFS block size actually is*/

  if(*argc > 2) 
    read_inputs(argc,argv);
  
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
  int err;
  /* Initialize variables */
  initialize(&argc,&argv);

  char fname[32];
  char name[32];

  size_t Mb_coor, Mb_elem, Mb_field, Mb_array;

  const char* PIOMODE[] = {"IND", "COLL"};

  /* parameters */
  debug = false;

  t0 = MPI_Wtime(); /* Timer */

  err = (int)cgp_mpi_info(info);
  if(err != CG_OK) {
    printf("*FAILED* cgp_mpi_info \n");
    cgp_error_exit();
  }
  err = (int)cgp_pio_mode((CGNS_ENUMT(PIOmode_t))piomode);
  if(err != CG_OK) {
    printf("*FAILED* cgp_pio_mode \n");
    cgp_error_exit();
  }

  Nnodes = Nelem*NodePerElem;

  nijk[0] = Nnodes; /* Number of vertices */
  nijk[1] = Nelem; /* Number of cells */
  nijk[2] = 0; /* Number of boundary vertices */

  /* Compute the size of the arrays */
  Mb_coor = sizeof(double)*3*Nnodes/1048576;
  Mb_elem = sizeof(cgsize_t)*Nelem*NodePerElem/1048576;
  Mb_field = sizeof(double)*3*Nnodes/1048576;
  Mb_array = (sizeof(double)+sizeof(cgsize_t))*Nnodes/1048576;

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
  if(cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z) != CG_OK) {
    printf("*FAILED* cg_zone_write \n");
    cgp_error_exit();
  t2 = MPI_Wtime();
  xtiming[13] = t2-t1;

  }

  /* ====================================== */
  /* == (A) WRITE THE NODAL COORDINATES  == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_x \n");
    cgp_error_exit();
  }

  if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_y \n");
    cgp_error_exit();
  }

  if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_z \n");
    cgp_error_exit();
  }

  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  for (k=0; k < count; k++) {
    Coor_x[k] = comm_rank*count + k + 1.1;
    Coor_y[k] = Coor_x[k] + 0.1;
    Coor_z[k] = Coor_y[k] + 0.1;
  }

  if(cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX",&Cx) != CG_OK) {
    printf("*FAILED* cgp_coord_write (Coor_x) \n");
    cgp_error_exit();
  }
  if(cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY",&Cy) != CG_OK) {
    printf("*FAILED* cgp_coord_write (Coor_y) \n");
    cgp_error_exit();
  }
  if(cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ",&Cz) != CG_OK) {
    printf("*FAILED* cgp_coord_write (Coor_z) \n");
    cgp_error_exit();
  }

  t1 = MPI_Wtime();
#if HDF5_HAVE_MULTI_DATASETS
  int Cvec[3];
  Cvec[0] = Cx;
  Cvec[1] = Cy;
  Cvec[2] = Cz;
  if(cgp_coord_multi_write_data(fn, B, Z, Cvec, &min,&max, Coor_x, Coor_y, Coor_z)!= CG_OK) {
    printf("*FAILED* cgp_coords_write_data \n");
    cgp_error_exit();
  }
#else
  if((cgp_coord_write_data(fn,B,Z,Cx,&min,&max,Coor_x)) != CG_OK) {
    printf("*FAILED* cgp_coord_write_data (Coor_x) \n");
    cgp_error_exit();
  }
  if((cgp_coord_write_data(fn,B,Z,Cy,&min,&max,Coor_y)) != CG_OK) {
    printf("*FAILED* cgp_coord_write_data (Coor_y) \n");
    cgp_error_exit();
  }
  if((cgp_coord_write_data(fn,B,Z,Cz,&min,&max,Coor_z)) != CG_OK) {
    printf("*FAILED* cgp_coord_write_data (Coor_z) \n");
    cgp_error_exit();
  }
#endif
  t2 = MPI_Wtime();
  xtiming[1] = t2-t1;

  free(Coor_x);
  free(Coor_y);
  free(Coor_z);
  /* ====================================== */
  /* == (B) WRITE THE CONNECTIVITY TABLE == */
  /* ====================================== */

  start = 1;
  end = nijk[1];

  if(cgp_section_write(fn,B,Z,"Elements",CGNS_ENUMV(PENTA_6),start,end,0,&S) != CG_OK) {
    printf("*FAILED* cgp_section_write \n");
    cgp_error_exit();
  }

  count = nijk[1]/comm_size;

  if( !(elements = malloc(count*NodePerElem*sizeof(cgsize_t)) )) {
    printf("*FAILED* allocation of elements \n");
    cgp_error_exit();
  }

  /* Create ridiculous connectivity table ... */
  for ( k = 0; k < count*NodePerElem; k++) {
    elements[k] = comm_rank*count*NodePerElem + k + 1;
  }

  emin = count*comm_rank+1;
  emax = count*(comm_rank+1);

  t1 = MPI_Wtime();
  if(cgp_elements_write_data(fn, B, Z, S, emin, emax, elements) != CG_OK) {
    printf("*FAILED* cgp_elements_write_data (elements) \n");
    cgp_error_exit();
  }

  t2 = MPI_Wtime();
  xtiming[2] = t2-t1;

  free(elements);


  /* ====================================== */
  /* == (C) WRITE THE FIELD DATA         == */
  /* ====================================== */

  count = nijk[0]/comm_size;

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

  if(cg_sol_write(fn, B, Z, "Solution", CGNS_ENUMV(Vertex), &S) != CG_OK) {
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
  int Fvec[3];
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

  free(Data_Fx);
  free(Data_Fy);
  free(Data_Fz);

  /* ====================================== */
  /* == (D) WRITE THE ARRAY DATA         == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Array_r = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Array_r \n");
    cgp_error_exit();
  }

  if( !(Array_i= (cgsize_t*) malloc(count*sizeof(cgsize_t))) ) {
    printf("*FAILED* allocation of Array_i  \n");
    cgp_error_exit();
  }

  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  for ( k = 0; k < count; k++) {
    Array_r[k] = comm_rank*count + k + 1.001;
    Array_i[k] = comm_rank*count + k + 1;
  }

  if(cg_goto(fn, B, "Zone 1", 0, "end") != CG_OK) {
    printf("*FAILED* cg_goto\n");
    cgp_error_exit();
  }

  if(cg_user_data_write("User Data") != CG_OK) {
    printf("*FAILED* cg_user_data_write \n");
    cgp_error_exit();
  }

  if(cg_gorel(fn,"User Data",0,"end") != CG_OK) {
    printf("*FAILED* cg_gorel\n");
    cgp_error_exit();
  }

   size_1D[0] = nijk[0];
  if(cgp_array_write("ArrayR",CGNS_ENUMV(RealDouble),1,size_1D,&Ar) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array_Ar)\n");
    cgp_error_exit();
  }

#if CG_BUILD_64BIT
  if(cgp_array_write("ArrayI",CGNS_ENUMV(LongInteger),1,size_1D,&Ai) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array_Ai)\n");
    cgp_error_exit();
  }
#else
  if(cgp_array_write("ArrayI",CGNS_ENUMV(Integer),1,size_1D,&Ai) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array Ai)\n");
    cgp_error_exit();
  }
#endif

  t1 = MPI_Wtime();
#if HDF5_HAVE_MULTI_DATASETS
  int Avec[2];
  Avec[0] = Ai;
  Avec[1] = Ar;
  if(cgp_array_multi_write_data(fn, Avec,&min,&max, 2, Array_i, Array_r) != CG_OK) {
    printf("*FAILED* cgp_field_array_data (Array_Ai)\n");
    cgp_error_exit();
  }
#else
  if(cgp_array_write_data(Ai,&min,&max,Array_i) != CG_OK) {
    printf("*FAILED* cgp_array_write_data (Array_Ai)\n");
    cgp_error_exit();
  }
  if(cgp_array_write_data(Ar,&min,&max,Array_r) != CG_OK) {
    printf("*FAILED* cgp_array_write_data (Array_Ar)\n");
    cgp_error_exit();
  }
#endif
  t2 = MPI_Wtime();
  xtiming[4] = t2-t1;

  free(Array_r);
  free(Array_i);

  t1 = MPI_Wtime();
  if(cgp_close(fn) != CG_OK) {
    printf("*FAILED* cgp_close \n");
    cgp_error_exit();
  };
  t2 = MPI_Wtime();
  xtiming[11] = t2-t1;

  /* ====================================== */
  /* ==    **  READ THE CGNS FILE **     == */
  /* ====================================== */
  MPI_Barrier(MPI_COMM_WORLD);

  t1 = MPI_Wtime();
  /* Open the cgns file for reading */
  if(cgp_open(fname, CG_MODE_MODIFY, &fn) != CG_OK) {
    printf("*FAILED* cgp_open \n");
    cgp_error_exit();
  }
  t2 = MPI_Wtime();
  xtiming[10] = t2-t1;

  /* Read the base information */
  t1 = MPI_Wtime();
  if(cg_base_read(fn, B, name, &r_cell_dim, &r_phys_dim) != CG_OK) {
    printf("*FAILED* cg_base_read\n");
    cgp_error_exit();
  }

  if(r_cell_dim != cell_dim || r_phys_dim != phys_dim) {
    printf("*FAILED* bad cell dim=%d or phy dim=%d\n", r_cell_dim, r_phys_dim);
    cgp_error_exit();
  }

  if (strcmp (name, "Base 1")) {
    printf("*FAILED* bad base name=%s\n", name);
    cgp_error_exit();
  }
  /* Read the zone information */
  t1 = MPI_Wtime();
  if(cg_zone_read(fn, B, Z, name, sizes) != CG_OK) {
    printf("*FAILED* cg_zoneread\n");
    cgp_error_exit();
  }
  t2 = MPI_Wtime();
  xtiming[14] = t2-t1;

  /* Check the read zone information is correct */
  if(sizes[0] != Nnodes) {
    printf("bad num points=%ld\n", (long)sizes[0]);
    cgp_error_exit();
  }

  if(sizes[1] != Nelem) {
    printf("bad num points=%ld\n", (long)sizes[1]);
    cgp_error_exit();
  }

  if(sizes[2] != 0) {
    printf("bad num points=%ld\n", (long)sizes[2]);
    cgp_error_exit();
  }

  if (strcmp (name, "Zone 1")) {
    printf("bad zone name=%s\n", name);
    cgp_error_exit();
  }
  /* ====================================== */
  /* ==  (A) READ THE NODAL COORDINATES  == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_x \n");
    cgp_error_exit();
  }

  if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_y \n");
    cgp_error_exit();
  }

  if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_z \n");
    cgp_error_exit();
  }
  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  t1 = MPI_Wtime();
#if HDF5_HAVE_MULTI_DATASETS
  Cvec[0] = Cx;
  Cvec[1] = Cy;
  Cvec[2] = Cz;
  if (cgp_coord_multi_read_data(fn, B, Z, Cvec, &min,&max, Coor_x, Coor_y, Coor_z)!= CG_OK) {
    printf("*FAILED* cgp_coords_mulit_read_data \n");
    cgp_error_exit();
  }
#else
  if (cgp_coord_read_data(fn,B,Z,Cx,&min,&max,Coor_x) != CG_OK) {
    printf("*FAILED* cgp_coord_read_data ( Reading Coor_x) \n");
    cgp_error_exit();
  }
  if (cgp_coord_read_data(fn,B,Z,Cy,&min,&max,Coor_y) != CG_OK) {
    printf("*FAILED* cgp_coord_read_data (Reading Coor_y) \n");
    cgp_error_exit();
  }
  if (cgp_coord_read_data(fn,B,Z,Cz,&min,&max,Coor_z) != CG_OK) {
    printf("*FAILED* cgp_coord_read_data (Reading Coor_z) \n");
    cgp_error_exit();
  }
#endif
  t2 = MPI_Wtime();
  xtiming[5] = t2-t1;

  /* Check if read the data back correctly */
  if(debug) {
    for ( k = 0; k < count; k++) {
      if( !c_double_eq(Coor_x[k], comm_rank*count + k + 1.1) ||
	  !c_double_eq(Coor_y[k], Coor_x[k] + 0.1) ||
	  !c_double_eq(Coor_z[k], Coor_y[k] + 0.1) ) {
	   printf("*FAILED* cgp_coord_read_data values are incorrect \n");
	   cgp_error_exit();
      }
    }
  }

  free(Coor_x);
  free(Coor_y);
  free(Coor_z);

/* ====================================== */
/* == (B) READ THE CONNECTIVITY TABLE  == */
/* ====================================== */

  count = nijk[1]/comm_size;
  if( !(elements = malloc(count*NodePerElem*sizeof(cgsize_t)) )) {
    printf("*FAILED* allocation of elements \n");
    cgp_error_exit();
  }

  emin = count*comm_rank+1;
  emax = count*(comm_rank+1);

  t1 = MPI_Wtime();
  if( cgp_elements_read_data(fn, B, Z, S, emin, emax, elements) != CG_OK) {
    printf("*FAILED* cgp_elements_read_data ( Reading elements) \n");
    cgp_error_exit();
  }
  t2 = MPI_Wtime();
  xtiming[6] = t2-t1;

  if(debug) {
    for ( k = 0; k < count; k++) {
      if(elements[k] != comm_rank*count*NodePerElem + k + 1) {
	printf("*FAILED* cgp_elements_read_data values are incorrect\n");
	cgp_error_exit();
      }
    }
  }
  free(elements);

  /* ====================================== */
  /* == (C) READ THE FIELD DATA          == */
  /* ====================================== */
  count = nijk[0]/comm_size;

  if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Reading Data_Fx \n");
    cgp_error_exit();
  }

  if( !(Data_Fy = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of  Reading Data_Fy \n");
    cgp_error_exit();
  }

  if( !(Data_Fz = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of  Reading Data_Fz \n");
    cgp_error_exit();
  }

  t1 = MPI_Wtime();

#if HDF5_HAVE_MULTI_DATASETS

  Fvec[0] = Fx;
  Fvec[1] = Fy;
  Fvec[2] = Fz;

  if(cgp_field_multi_read_data(fn,B,Z,S,Fvec,&min,&max,3,Data_Fx,Data_Fy,Data_Fz) != CG_OK) {
    printf("*FAILED* cgp_field_multi_read_data \n");
    cgp_error_exit();
  }
#else
  if (cgp_field_read_data(fn,B,Z,S,Fx,&min,&max,Data_Fx) != CG_OK) {
    printf("*FAILED* cgp_field_read_data (Data_Fx) \n");
    cgp_error_exit();
  }
  if (cgp_field_read_data(fn,B,Z,S,Fy,&min,&max,Data_Fy) != CG_OK) {
    printf("*FAILED* cgp_field_read_data (Data_Fy) \n");
    cgp_error_exit();
  }
  if (cgp_field_read_data(fn,B,Z,S,Fz,&min,&max,Data_Fz) != CG_OK) {
    printf("*FAILED* cgp_field_read_data (Data_Fz) \n");
    cgp_error_exit();
  }
#endif
  t2 = MPI_Wtime();
  xtiming[7] = t2-t1;

  /* Check if read the data back correctly */
  if(debug) {
    for ( k = 0; k < count; k++) {
      if(!c_double_eq(Data_Fx[k], comm_rank*count + k + 1.01) ||
	 !c_double_eq(Data_Fy[k], comm_rank*count + k + 1.02) ||
	 !c_double_eq(Data_Fz[k], comm_rank*count + k + 1.03) ) {
	printf("*FAILED* cgp_field_read_data values are incorrect \n");
	cgp_error_exit();
      }
    }
  }
  free(Data_Fx);
  free(Data_Fy);
  free(Data_Fz);

  /* ====================================== */
  /* == (D) READ THE ARRAY DATA          == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Array_r = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of  Reading Array_r \n");
    cgp_error_exit();
  }

  if( !(Array_i= (cgsize_t*) malloc(count*sizeof(cgsize_t))) ) {
    printf("*FAILED* allocation of  Reading Array_i  \n");
    cgp_error_exit();
  }

  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  if(cg_goto(fn,B,"Zone_t",Z,"UserDefinedData_t",1,"end") != CG_OK) {
    printf("*FAILED* cg_goto (User Defined Data)\n");
    cgp_error_exit();
  }

  t1 = MPI_Wtime();
#if HDF5_HAVE_MULTI_DATASETS

  Avec[0] = Ar;
  Avec[1] = Ai;

  if( cgp_array_multi_read_data(fn, Avec, &min, &max, 2, Array_r, Array_i) != CG_OK) {
    printf("*FAILED* cgp_array_multi_read_data  \n");
    cgp_error_exit();
  }
#else
  if( cgp_array_read_data(Ar, &min, &max, Array_r) != CG_OK) {
    printf("*FAILED* cgp_array_read_data (Array_r) \n");
    cgp_error_exit();
  }
  if( cgp_array_read_data(Ai, &min, &max, Array_i) != CG_OK) {
    printf("*FAILED* cgp_array_read_data (Array_i) \n");
    cgp_error_exit();
  }
#endif
  t2 = MPI_Wtime();
  xtiming[8] = t2-t1;

  /* Check if read the data back correctly */
  if(debug) {
    for ( k = 0; k < count; k++) {
      if(!c_double_eq(Array_r[k], comm_rank*count + k + 1.001) ||
	 Array_i[k] != comm_rank*count + k +1) {
	  printf("*FAILED* cgp_array_read_data values are incorrect \n");
	  cgp_error_exit();
      }
    }
  }

  free(Array_r);
  free(Array_i);

  t1 = MPI_Wtime();
  if(cgp_close(fn) !=CG_OK) {
     printf("*FAILED* cgp_close\n");
     cgp_error_exit();
  }
  t2 = MPI_Wtime();
  xtiming[12] = t2-t1;

  xtiming[0] = t2-t0;

  MPI_Reduce(&xtiming, &timing, 15, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  MPI_Reduce(&xtiming, &timingMin, 15, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
  MPI_Reduce(&xtiming, &timingMax, 15, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

  if(comm_rank==0) {
    sprintf(fname, "timing_%06d_%s.dat", comm_size, PIOMODE[piomode]);
    FILE *fid = fopen(fname, "w");
    if (fid == NULL) {
      printf("Error opening timing file!\n");
    } else {
      fprintf(fid,"#nelem = %zu \n",Nelem);
      fprintf(fid,"#nprocs, total time, write: coord., elem., field, array, read: coord., elem., field, array, MB: coord, elem, field, array \n%d", comm_size);

      for ( k = 0; k < 15; k++) {
	fprintf(fid," %.3f %.3f %.3f ",timing[k]/((double) comm_size), timingMin[k], timingMax[k]);
      }
      fprintf(fid," %zu %zu %zu %zu \n", Mb_coor, Mb_elem, Mb_field, Mb_array);
      fclose(fid);
    }
  }

  MPI_Finalize();

  return 0;
}


