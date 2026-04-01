/*
 * test_multisets.c - Tests for the multi-dataset parallel I/O API.
 *
 * Covers two scenarios:
 *
 *  All ranks contribute real data: verifies round-trip correctness for
 *  cgp_{coord,field,array}_multi_{write,read}_data.
 *
 *  NULL buffer: ranks passing NULL data buffers participate correctly in HDF5
 *  collective I/O without causing deadlocks.  Two sub-cases:
 *    - mixed:      all ranks but the last contribute data; last rank uses NULL.
 *    - rank0-only: only rank 0 contributes; all other ranks use NULL.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "pcgnslib.h"
#include "utils.h"
#include "mpi.h"

static MPI_Comm comm = MPI_COMM_WORLD;
static int comm_size, comm_rank;

static int multisets()
{
  char fname[32];

  void **buf;
  int Cvec[3];
  int Fvec[3];
  int Avec[2];

  cgsize_t Nelem = 1024; /* DEFAULT */
  cgsize_t NodePerElem = 6;

  cgsize_t Nnodes;

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
  int piomode = CGP_COLLECTIVE; /* DEFAULT */
  int err;

  err = (int)cgp_pio_mode((CGNS_ENUMT(PIOmode_t))piomode);
  if(err != CG_OK) {
    printf("*FAILED* cgp_pio_mode \n");
    cgp_error_exit();
  }

  Nnodes = Nelem*NodePerElem;

  nijk[0] = Nnodes; /* Number of vertices */
  nijk[1] = Nelem; /* Number of cells */
  nijk[2] = 0; /* Number of boundary vertices */

  /* ====================================== */
  /* ==    **WRITE THE CGNS FILE **      == */
  /* ====================================== */

  sprintf(fname, "cmultiset_%06d.cgns", comm_size);

  if(cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
    printf("*FAILED* cgp_open \n");
    cgp_error_exit();
  }

  if(cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) != CG_OK) {
    printf("*FAILED* cg_base_write \n");
    cgp_error_exit();
  }
  if(cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z) != CG_OK) {
    printf("*FAILED* cg_zone_write \n");
    cgp_error_exit();
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

  Cvec[0] = Cx;
  Cvec[1] = Cy;
  Cvec[2] = Cz;

  buf = (void *)malloc(3*sizeof(void *));

  buf[0] =&Coor_x[0];
  buf[1] =&Coor_y[0];
  buf[2] =&Coor_z[0];

  if(cgp_coord_multi_write_data(fn, B, Z, Cvec, &min,&max,3,(const void **)buf)!= CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_coord_multi_write_data", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_coord_multi_write_data", NULL);
  }

  free(buf);

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

  if(cgp_elements_write_data(fn, B, Z, S, emin, emax, elements) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_elements_write_data (elements)", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_elements_write_data (elements)", NULL);

  }
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

  Fvec[0] = Fx;
  Fvec[1] = Fy;
  Fvec[2] = Fz;

  buf = (void *)malloc(3*sizeof(void *));
  buf[0] = &Data_Fx[0];
  buf[1] = &Data_Fy[0];
  buf[2] = &Data_Fz[0];

  if(cgp_field_multi_write_data(fn,B,Z,S,Fvec,&min,&max,3,(const void **)buf) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_field_multi_write_data (Momentum)", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_field_multi_write_data (Momentum)", NULL);
  }
  free(buf);

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

  Avec[0] = Ai;
  Avec[1] = Ar;

  buf = (void *)malloc(2*sizeof(void *));
  buf[0] = &Array_i[0];
  buf[1] = &Array_r[0];

  if(cgp_array_multi_write_data(fn, Avec,&min,&max, 2, (const void **)buf) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_field_array_data (Array_A)", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_field_array_data (Array_A)", NULL);
  }
  free(buf);
  free(Array_r);
  free(Array_i);

  if(cgp_close(fn) != CG_OK) {
    printf("*FAILED* cgp_close \n");
    cgp_error_exit();
  };

  /* ====================================== */
  /* ==    **  READ THE CGNS FILE **     == */
  /* ====================================== */
  MPI_Barrier(MPI_COMM_WORLD);

  /* Open the cgns file for reading */
  if(cgp_open(fname, CG_MODE_MODIFY, &fn) != CG_OK) {
    printf("*FAILED* cgp_open \n");
    cgp_error_exit();
  }

  /* Read the base information */
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
  if(cg_zone_read(fn, B, Z, name, sizes) != CG_OK) {
    printf("*FAILED* cg_zone_read\n");
    cgp_error_exit();
  }

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

  Cvec[0] = Cx;
  Cvec[1] = Cy;
  Cvec[2] = Cz;

  buf = (void *)malloc(3*sizeof(void *));
  buf[0] = &Coor_x[0];
  buf[1] = &Coor_y[0];
  buf[2] = &Coor_z[0];

  if (cgp_coord_multi_read_data(fn, B, Z, Cvec, &min,&max, 3, buf)!= CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_coord_multi_read_data", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_coord_multi_read_data", NULL);
  }
  free(buf);

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if( !compareValuesDouble(Coor_x[k], comm_rank*count + k + 1.1) ||
        !compareValuesDouble(Coor_y[k], Coor_x[k] + 0.1) ||
        !compareValuesDouble(Coor_z[k], Coor_y[k] + 0.1) ) {
      if(comm_rank == 0) write_test_status(FAILED, "Check cgp_coord_multi_read_data values", NULL);
      cgp_error_exit();
    }
  }
  if(comm_rank == 0) write_test_status(PASSED, "Check cgp_coord_multi_read_data values", NULL);

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

  if( cgp_elements_read_data(fn, B, Z, S, emin, emax, elements) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_elements_read_data ( Reading elements)", NULL);
    cgp_error_exit();
  }
  if(comm_rank == 0) write_test_status(PASSED, "Test cgp_elements_read_data ( Reading elements)", NULL);

  for ( k = 0; k < count; k++) {
    if(elements[k] != comm_rank*count*NodePerElem + k + 1) {
      if(comm_rank == 0) write_test_status(FAILED, "Check cgp_elements_read_data values", NULL);
      cgp_error_exit();
    }
  }
  if(comm_rank == 0) write_test_status(PASSED, "Check cgp_elements_read_data values", NULL);
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
    printf("*FAILED* allocation of Reading Data_Fy \n");
    cgp_error_exit();
  }

  if( !(Data_Fz = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Reading Data_Fz \n");
    cgp_error_exit();
  }

  Fvec[0] = Fx;
  Fvec[1] = Fy;
  Fvec[2] = Fz;

  buf = (void *)malloc(3*sizeof(void *));
  buf[0] = &Data_Fx[0];
  buf[1] = &Data_Fy[0];
  buf[2] = &Data_Fz[0];

  if(cgp_field_multi_read_data(fn,B,Z,S,Fvec,&min,&max,3,buf) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_field_multi_read_data", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_field_multi_read_data", NULL);
  }
  free(buf);

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if(!compareValuesDouble(Data_Fx[k], comm_rank*count + k + 1.01) ||
       !compareValuesDouble(Data_Fy[k], comm_rank*count + k + 1.02) ||
       !compareValuesDouble(Data_Fz[k], comm_rank*count + k + 1.03) ) {
      if(comm_rank == 0) write_test_status(FAILED, "Check cgp_field_multi_read_data values", NULL);
      cgp_error_exit();
    }
  }
  if(comm_rank == 0) write_test_status(PASSED, "Check cgp_field_multi_read_data values", NULL);

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

  Avec[0] = Ar;
  Avec[1] = Ai;

  buf = (void *)malloc(3*sizeof(void *));
  buf[0] = &Array_r[0];
  buf[1] = &Array_i[0];

  if( cgp_array_multi_read_data(fn, Avec, &min, &max, 2, buf) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_array_multi_read_data", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_array_multi_read_data", NULL);
  }
  free(buf);

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if(!compareValuesDouble(Array_r[k], comm_rank*count + k + 1.001) ||
       Array_i[k] != comm_rank*count + k +1) {
      if(comm_rank == 0) write_test_status(FAILED, "Check cgp_array_multi_read_data values", NULL);
      cgp_error_exit();
    }
  }
  if(comm_rank == 0) write_test_status(PASSED, "Check cgp_array_multi_read_data values", NULL);

  free(Array_r);
  free(Array_i);

  if(cgp_close(fn) !=CG_OK) {
     printf("*FAILED* cgp_close\n");
     cgp_error_exit();
  }

  return 0;
}

/* ------------------------------------------------------------------ */
/* NULL buffer tests                                                    */
/*                                                                      */
/* Test 1 (mixed): all ranks except the last contribute data; the last  */
/* rank passes NULL buffers.  Covers coords, fields, and arrays.        */
/* ------------------------------------------------------------------ */
static void test_mixed_null(void)
{
  char fname[64];
  void **buf;
  int Cvec[3], Fvec[3], Avec[2];
  int fn, B, Z, S;
  int Cx, Cy, Cz, Fx, Fy, Fz, Ar, Ai;
  cgsize_t nijk[3], size_1D[1];
  cgsize_t min, max, k, count;
  double *Coor_x = NULL, *Coor_y = NULL, *Coor_z = NULL;
  double *Data_Fx = NULL, *Data_Fy = NULL, *Data_Fz = NULL;
  double *Array_r = NULL;
  int    *Array_i = NULL;
  int null_rank = comm_size - 1;
  int err;

  nijk[0] = 100 * comm_size;
  nijk[1] = 100 * comm_size;
  nijk[2] = 0;
  count = nijk[0] / comm_size;
  min   = count * comm_rank + 1;
  max   = count * (comm_rank + 1);

  sprintf(fname, "cnullbuf_mixed_%06d.cgns", comm_size);

  /* ---- WRITE ---- */

  if (cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
    printf("*FAILED* cgp_open (mixed write)\n"); cgp_error_exit();
  }
  cg_base_write(fn, "Base 1", 3, 3, &B);
  cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z);

  /* Coords */
  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", &Cx);
  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", &Cy);
  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateZ", &Cz);
  Cvec[0] = Cx; Cvec[1] = Cy; Cvec[2] = Cz;

  buf = (void **)malloc(3 * sizeof(void *));
  if (comm_rank != null_rank) {
    Coor_x = (double *)malloc(count * sizeof(double));
    Coor_y = (double *)malloc(count * sizeof(double));
    Coor_z = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) {
      Coor_x[k] = comm_rank * count + k + 1.1;
      Coor_y[k] = comm_rank * count + k + 2.2;
      Coor_z[k] = comm_rank * count + k + 3.3;
    }
    buf[0] = Coor_x; buf[1] = Coor_y; buf[2] = Coor_z;
  } else {
    buf[0] = NULL; buf[1] = NULL; buf[2] = NULL;
  }

  err = cgp_coord_multi_write_data(fn, B, Z, Cvec, &min, &max,
                                   3, (const void **)buf);
  if (comm_rank == 0)
    write_test_status(err == CG_OK ? PASSED : FAILED,
                      "cgp_coord_multi_write_data (mixed NULL)", NULL);
  if (err != CG_OK) cgp_error_exit();
  free(buf); free(Coor_x); free(Coor_y); free(Coor_z);
  Coor_x = NULL; Coor_y = NULL; Coor_z = NULL;

  /* Fields */
  cg_sol_write(fn, B, Z, "Solution", CGNS_ENUMV(Vertex), &S);
  cgp_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "FieldA", &Fx);
  cgp_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "FieldB", &Fy);
  cgp_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "FieldC", &Fz);
  Fvec[0] = Fx; Fvec[1] = Fy; Fvec[2] = Fz;

  buf = (void **)malloc(3 * sizeof(void *));
  if (comm_rank != null_rank) {
    Data_Fx = (double *)malloc(count * sizeof(double));
    Data_Fy = (double *)malloc(count * sizeof(double));
    Data_Fz = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) {
      Data_Fx[k] = comm_rank * count + k + 10.1;
      Data_Fy[k] = comm_rank * count + k + 20.2;
      Data_Fz[k] = comm_rank * count + k + 30.3;
    }
    buf[0] = Data_Fx; buf[1] = Data_Fy; buf[2] = Data_Fz;
  } else {
    buf[0] = NULL; buf[1] = NULL; buf[2] = NULL;
  }

  err = cgp_field_multi_write_data(fn, B, Z, S, Fvec, &min, &max,
                                   3, (const void **)buf);
  if (comm_rank == 0)
    write_test_status(err == CG_OK ? PASSED : FAILED,
                      "cgp_field_multi_write_data (mixed NULL)", NULL);
  if (err != CG_OK) cgp_error_exit();
  free(buf); free(Data_Fx); free(Data_Fy); free(Data_Fz);
  Data_Fx = NULL; Data_Fy = NULL; Data_Fz = NULL;

  /* Arrays */
  cg_goto(fn, B, "Zone_t", Z, "end");
  cg_user_data_write("User Data");
  cg_gorel(fn, "User Data", 0, "end");
  size_1D[0] = nijk[0];
  cgp_array_write("ArrayR", CGNS_ENUMV(RealDouble), 1, size_1D, &Ar);
  cgp_array_write("ArrayI", CGNS_ENUMV(Integer),    1, size_1D, &Ai);
  Avec[0] = Ai; Avec[1] = Ar;

  buf = (void **)malloc(2 * sizeof(void *));
  if (comm_rank != null_rank) {
    Array_r = (double *)malloc(count * sizeof(double));
    Array_i = (int    *)malloc(count * sizeof(int));
    for (k = 0; k < count; k++) {
      Array_r[k] = comm_rank * count + k + 1.001;
      Array_i[k] = (int)(comm_rank * count + k + 1);
    }
    buf[0] = Array_i; buf[1] = Array_r;
  } else {
    buf[0] = NULL; buf[1] = NULL;
  }

  err = cgp_array_multi_write_data(fn, Avec, &min, &max,
                                   2, (const void **)buf);
  if (comm_rank == 0)
    write_test_status(err == CG_OK ? PASSED : FAILED,
                      "cgp_array_multi_write_data (mixed NULL)", NULL);
  if (err != CG_OK) cgp_error_exit();
  free(buf); free(Array_r); free(Array_i);
  Array_r = NULL; Array_i = NULL;

  cgp_close(fn);
  MPI_Barrier(comm);

  /* ---- READ + VERIFY ---- */

  if (cgp_open(fname, CG_MODE_READ, &fn) != CG_OK) {
    printf("*FAILED* cgp_open (mixed read)\n"); cgp_error_exit();
  }

  /* Coords */
  buf = (void **)malloc(3 * sizeof(void *));
  if (comm_rank != null_rank) {
    Coor_x = (double *)malloc(count * sizeof(double));
    Coor_y = (double *)malloc(count * sizeof(double));
    Coor_z = (double *)malloc(count * sizeof(double));
    buf[0] = Coor_x; buf[1] = Coor_y; buf[2] = Coor_z;
  } else {
    buf[0] = NULL; buf[1] = NULL; buf[2] = NULL;
  }

  err = cgp_coord_multi_read_data(fn, B, Z, Cvec, &min, &max, 3, buf);
  if (err != CG_OK) {
    if (comm_rank == 0)
      write_test_status(FAILED, "cgp_coord_multi_read_data (mixed NULL)", NULL);
    cgp_error_exit();
  }
  if (comm_rank != null_rank) {
    int ok = 1;
    for (k = 0; k < count; k++) {
      if (!compareValuesDouble(Coor_x[k], comm_rank * count + k + 1.1) ||
          !compareValuesDouble(Coor_y[k], comm_rank * count + k + 2.2) ||
          !compareValuesDouble(Coor_z[k], comm_rank * count + k + 3.3)) {
        ok = 0; break;
      }
    }
    if (comm_rank == 0)
      write_test_status(ok ? PASSED : FAILED,
                        "cgp_coord_multi_read_data (mixed NULL)", NULL);
    if (!ok) cgp_error_exit();
  }
  free(buf); free(Coor_x); free(Coor_y); free(Coor_z);
  Coor_x = NULL; Coor_y = NULL; Coor_z = NULL;

  /* Fields */
  buf = (void **)malloc(3 * sizeof(void *));
  if (comm_rank != null_rank) {
    Data_Fx = (double *)malloc(count * sizeof(double));
    Data_Fy = (double *)malloc(count * sizeof(double));
    Data_Fz = (double *)malloc(count * sizeof(double));
    buf[0] = Data_Fx; buf[1] = Data_Fy; buf[2] = Data_Fz;
  } else {
    buf[0] = NULL; buf[1] = NULL; buf[2] = NULL;
  }

  err = cgp_field_multi_read_data(fn, B, Z, S, Fvec, &min, &max, 3, buf);
  if (err != CG_OK) {
    if (comm_rank == 0)
      write_test_status(FAILED, "cgp_field_multi_read_data (mixed NULL)", NULL);
    cgp_error_exit();
  }
  if (comm_rank != null_rank) {
    int ok = 1;
    for (k = 0; k < count; k++) {
      if (!compareValuesDouble(Data_Fx[k], comm_rank * count + k + 10.1) ||
          !compareValuesDouble(Data_Fy[k], comm_rank * count + k + 20.2) ||
          !compareValuesDouble(Data_Fz[k], comm_rank * count + k + 30.3)) {
        ok = 0; break;
      }
    }
    if (comm_rank == 0)
      write_test_status(ok ? PASSED : FAILED,
                        "cgp_field_multi_read_data (mixed NULL)", NULL);
    if (!ok) cgp_error_exit();
  }
  free(buf); free(Data_Fx); free(Data_Fy); free(Data_Fz);
  Data_Fx = NULL; Data_Fy = NULL; Data_Fz = NULL;

  /* Arrays — swap order to [Ar, Ai] for read */
  cg_goto(fn, B, "Zone_t", Z, "UserDefinedData_t", 1, "end");
  Avec[0] = Ar; Avec[1] = Ai;

  buf = (void **)malloc(2 * sizeof(void *));
  if (comm_rank != null_rank) {
    Array_r = (double *)malloc(count * sizeof(double));
    Array_i = (int    *)malloc(count * sizeof(int));
    buf[0] = Array_r; buf[1] = Array_i;
  } else {
    buf[0] = NULL; buf[1] = NULL;
  }

  err = cgp_array_multi_read_data(fn, Avec, &min, &max, 2, buf);
  if (err != CG_OK) {
    if (comm_rank == 0)
      write_test_status(FAILED, "cgp_array_multi_read_data (mixed NULL)", NULL);
    cgp_error_exit();
  }
  if (comm_rank != null_rank) {
    int ok = 1;
    for (k = 0; k < count; k++) {
      if (!compareValuesDouble(Array_r[k], comm_rank * count + k + 1.001) ||
          Array_i[k] != (int)(comm_rank * count + k + 1)) {
        ok = 0; break;
      }
    }
    if (comm_rank == 0)
      write_test_status(ok ? PASSED : FAILED,
                        "cgp_array_multi_read_data (mixed NULL)", NULL);
    if (!ok) cgp_error_exit();
  }
  free(buf); free(Array_r); free(Array_i);

  cgp_close(fn);
}

/* ------------------------------------------------------------------ */
/* Test 2 (rank0-only): only rank 0 writes; all others pass NULL.      */
/* Covers cgp_coord_multi_{write,read}_data.                           */
/* ------------------------------------------------------------------ */
static void test_rank0_only(void)
{
  char fname[64];
  void **buf;
  int Cvec[3];
  int fn, B, Z;
  int Cx, Cy, Cz;
  cgsize_t nijk[3];
  cgsize_t min, max, k, count;
  double *Coor_x = NULL, *Coor_y = NULL, *Coor_z = NULL;
  int err;

  nijk[0] = 100 * comm_size;
  nijk[1] = 100 * comm_size;
  nijk[2] = 0;
  count = nijk[0] / comm_size;
  min   = count * comm_rank + 1;
  max   = count * (comm_rank + 1);

  sprintf(fname, "cnull_r0_%06d.cgns", comm_size);

  /* ---- WRITE ---- */

  if (cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
    printf("*FAILED* cgp_open (rank0-only write)\n"); cgp_error_exit();
  }
  cg_base_write(fn, "Base 1", 3, 3, &B);
  cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z);

  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", &Cx);
  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", &Cy);
  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateZ", &Cz);
  Cvec[0] = Cx; Cvec[1] = Cy; Cvec[2] = Cz;

  buf = (void **)malloc(3 * sizeof(void *));
  if (comm_rank == 0) {
    Coor_x = (double *)malloc(count * sizeof(double));
    Coor_y = (double *)malloc(count * sizeof(double));
    Coor_z = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) {
      Coor_x[k] = k + 1.1;
      Coor_y[k] = k + 2.2;
      Coor_z[k] = k + 3.3;
    }
    buf[0] = Coor_x; buf[1] = Coor_y; buf[2] = Coor_z;
  } else {
    buf[0] = NULL; buf[1] = NULL; buf[2] = NULL;
  }

  err = cgp_coord_multi_write_data(fn, B, Z, Cvec, &min, &max,
                                   3, (const void **)buf);
  if (comm_rank == 0)
    write_test_status(err == CG_OK ? PASSED : FAILED,
                      "cgp_coord_multi_write_data (rank0-only)", NULL);
  if (err != CG_OK) cgp_error_exit();
  free(buf); free(Coor_x); free(Coor_y); free(Coor_z);
  Coor_x = NULL; Coor_y = NULL; Coor_z = NULL;

  cgp_close(fn);
  MPI_Barrier(comm);

  /* ---- READ + VERIFY ---- */

  if (cgp_open(fname, CG_MODE_READ, &fn) != CG_OK) {
    printf("*FAILED* cgp_open (rank0-only read)\n"); cgp_error_exit();
  }

  buf = (void **)malloc(3 * sizeof(void *));
  if (comm_rank == 0) {
    Coor_x = (double *)malloc(count * sizeof(double));
    Coor_y = (double *)malloc(count * sizeof(double));
    Coor_z = (double *)malloc(count * sizeof(double));
    buf[0] = Coor_x; buf[1] = Coor_y; buf[2] = Coor_z;
  } else {
    buf[0] = NULL; buf[1] = NULL; buf[2] = NULL;
  }

  err = cgp_coord_multi_read_data(fn, B, Z, Cvec, &min, &max, 3, buf);
  if (err != CG_OK) {
    if (comm_rank == 0)
      write_test_status(FAILED, "cgp_coord_multi_read_data (rank0-only)", NULL);
    cgp_error_exit();
  }
  if (comm_rank == 0) {
    int ok = 1;
    for (k = 0; k < count; k++) {
      if (!compareValuesDouble(Coor_x[k], k + 1.1) ||
          !compareValuesDouble(Coor_y[k], k + 2.2) ||
          !compareValuesDouble(Coor_z[k], k + 3.3)) {
        ok = 0; break;
      }
    }
    write_test_status(ok ? PASSED : FAILED,
                      "cgp_coord_multi_read_data (rank0-only)", NULL);
    if (!ok) cgp_error_exit();
  }
  free(buf); free(Coor_x); free(Coor_y); free(Coor_z);

  cgp_close(fn);
}

int main(int argc, char *argv[])
{
  char test_str[TAB_SPACE];

  MPI_Init(&argc, &argv);
  MPI_Comm_size(comm, &comm_size);
  MPI_Comm_rank(comm, &comm_rank);

  if (comm_size > 8) {
    if (comm_rank == 0)
      fprintf(stderr, "number of processes must be 8 or less\n");
    cgp_error_exit();
  }

  if (cgp_pio_mode(CGP_COLLECTIVE) != CG_OK) {
    printf("*FAILED* cgp_pio_mode\n"); cgp_error_exit();
  }

  strcpy(test_str, "Multi-sets API Testing");
  if (comm_rank == 0) write_test_header(test_str, strlen(test_str));
  multisets();

  strcpy(test_str, "Multi-dataset NULL Buffer Testing: mixed");
  if (comm_rank == 0) write_test_header(test_str, strlen(test_str));
  test_mixed_null();

  strcpy(test_str, "Multi-dataset NULL Buffer Testing: rank0-only");
  if (comm_rank == 0) write_test_header(test_str, strlen(test_str));
  test_rank0_only();

  MPI_Finalize();
  return 0;
}
