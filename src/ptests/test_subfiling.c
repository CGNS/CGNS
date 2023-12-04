/*
! @file test_subfiling.c
! @author M. Scot Breitenfeld <brtnfld@hdfgroup.org>
! @version 0.1
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Tests the subfiling feature of HDF5. Requires HDF5 version 1.14 or greater.
!
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include <limits.h>
#include "pcgnslib.h"
#include "utils.h"
#include "mpi.h"
#include "timer.h"
#include "hdf5.h"
#include <sys/stat.h>
#include <sys/wait.h>

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

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

int piomode = CGP_INDEPENDENT;
cgsize_t Nelem = 65536;
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
struct timer_statinfo timing[15];

int initialize(int* argc, char** argv[]) {

  int required = MPI_THREAD_MULTIPLE;
  int provided = 0;
  MPI_Init_thread(argc, argv, required, &provided);
  if (provided < required) {
    printf("MPI_THREAD_MULTIPLE not supported\n");
    cgp_error_exit();
  }

  MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
  MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
  MPI_Info_create(&info);

  char test_str[TAB_SPACE];
  strcpy(test_str,"Testing CGNS with HDF5 Subfiling");
  if ( comm_rank == 0) write_test_header(test_str, strlen(test_str));

  return 0;
}

int test_reading(char *fname)  {

  char name[33];

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
    printf("*FAILED* cg_zoneread\n");
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

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if( !compareValuesDouble(Coor_x[k], comm_rank*count + k + 1.1) ||
        !compareValuesDouble(Coor_y[k], Coor_x[k] + 0.1) ||
        !compareValuesDouble(Coor_z[k], Coor_y[k] + 0.1) ) {
      printf("*FAILED* cgp_coord_read_data values are incorrect \n");
      cgp_error_exit();
    }
  }

  free(Coor_x);
  free(Coor_y);
  free(Coor_z);

  if(comm_rank == 0) write_test_status(PASSED, "Read Coordinates", NULL);

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
    printf("*FAILED* cgp_elements_read_data ( Reading elements) \n");
    cgp_error_exit();
  }

  for ( k = 0; k < count; k++) {
    if(elements[k] != comm_rank*count*NodePerElem + k + 1) {
      printf("*FAILED* cgp_elements_read_data values are incorrect\n");
      cgp_error_exit();
    }
  }
  free(elements);

  if(comm_rank == 0) write_test_status(PASSED, "Read Connectivity Table", NULL);

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

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if(!compareValuesDouble(Data_Fx[k], comm_rank*count + k + 1.01) ||
       !compareValuesDouble(Data_Fy[k], comm_rank*count + k + 1.02) ||
       !compareValuesDouble(Data_Fz[k], comm_rank*count + k + 1.03) ) {
      printf("*FAILED* cgp_field_read_data values are incorrect \n");
      cgp_error_exit();
    }
  }
  free(Data_Fx);
  free(Data_Fy);
  free(Data_Fz);

  if(comm_rank == 0) write_test_status(PASSED, "Read Field Data", NULL);

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

  if( cgp_array_read_data(Ar, &min, &max, Array_r) != CG_OK) {
    printf("*FAILED* cgp_array_read_data (Array_r) \n");
    cgp_error_exit();
  }
  if( cgp_array_read_data(Ai, &min, &max, Array_i) != CG_OK) {
    printf("*FAILED* cgp_array_read_data (Array_i) \n");
    cgp_error_exit();
  }

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if(!compareValuesDouble(Array_r[k], comm_rank*count + k + 1.001) ||
       Array_i[k] != comm_rank*count + k +1) {
      printf("*FAILED* cgp_array_read_data values are incorrect \n");
      cgp_error_exit();
    }
  }
  
  free(Array_r);
  free(Array_i);

  if(comm_rank == 0) write_test_status(PASSED, "Read Array Data", NULL);

  if(cgp_close(fn) !=CG_OK) {
     printf("*FAILED* cgp_close\n");
     cgp_error_exit();
  }

  return 0;

}

int main(int argc, char* argv[]) {

  /* Initialize MPI */
  initialize(&argc,&argv);

#if HDF5_SUBFILING_ENABLED

  int err;
  char fname[32];

  timer_start(&t0, MPI_COMM_WORLD, t_nobarrier);

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

  if (cg_configure(CG_CONFIG_HDF5_SUBFILING, (void *)1))
    cg_error_exit();

  int64_t align_size = 8*1024*1024;

  /* Sets the subfiling stripe size  */
  cg_subfiling_config_t h5subf_config;
  h5subf_config.stripe_size = align_size;

  /* Sets the subfiling stripe count */
  h5subf_config.stripe_count = 2;

  /* Sets the subfiling thread pool size */
  h5subf_config.thread_pool_size = 2;

  /* Sets the subfilng(ioc) selection option */
  h5subf_config.ioc_selection = 3;

  if (cg_configure(CG_CONFIG_HDF5_SUBFILING_CONFIG, &h5subf_config))
    cgp_error_exit();

  Nnodes = Nelem*NodePerElem;

  nijk[0] = Nnodes; /* Number of vertices */
  nijk[1] = Nelem; /* Number of cells */
  nijk[2] = 0; /* Number of boundary vertices */


  /* ====================================== */
  /* ==    **WRITE THE CGNS FILE **      == */
  /* ====================================== */

  sprintf(fname, "subfiling_%06d.cgns", comm_size);

  if(cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Create CGNS file", "cgp_open failed");
    cgp_error_exit();
  }
  if(comm_rank == 0) write_test_status(PASSED, "Create CGNS file", NULL);

  if(cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Create CGNS Base", "cgp_base_write failed");
    cgp_error_exit();
  }
  if(comm_rank == 0) write_test_status(PASSED, "Create CGNS Base", NULL);
  
  if(cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Create CGNS Zone", "cg_zone_write failed");
    cgp_error_exit();
  }
  if(comm_rank == 0) write_test_status(PASSED, "Create CGNS Zone", NULL);


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

  free(Coor_x);
  free(Coor_y);
  free(Coor_z);

  if(comm_rank == 0) write_test_status(PASSED, "Write Coordinates", NULL);

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
    printf("*FAILED* cgp_elements_write_data (elements) \n");
    cgp_error_exit();
  }
  
  free(elements);

  if(comm_rank == 0) write_test_status(PASSED, "Write Connectivity Table", NULL);

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
  
  timer_start(&t1, MPI_COMM_WORLD, t_nobarrier);

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

  if(comm_rank == 0) write_test_status(PASSED, "Write Field Data", NULL);

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

  if(cgp_array_write_data(Ai,&min,&max,Array_i) != CG_OK) {
    printf("*FAILED* cgp_array_write_data (Array_Ai)\n");
    cgp_error_exit();
  }
  if(cgp_array_write_data(Ar,&min,&max,Array_r) != CG_OK) {
    printf("*FAILED* cgp_array_write_data (Array_Ar)\n");
    cgp_error_exit();
  }

  free(Array_r);
  free(Array_i);

  if(comm_rank == 0) write_test_status(PASSED, "Write Array Data", NULL);

  if(cgp_close(fn) != CG_OK) {
    printf("*FAILED* cgp_close \n");
    cgp_error_exit();
  };

  /* Verify the subfiling configuration parameters were set */

  char *config_filename = NULL;
  FILE *config_file;
  struct stat file_info;
  config_filename = malloc(PATH_MAX);
  stat(fname, &file_info);

  snprintf(config_filename, PATH_MAX, "%s/" H5FD_SUBFILING_CONFIG_FILENAME_TEMPLATE, ".",
                  fname, (uint64_t)file_info.st_ino);

  config_file = fopen(config_filename, "r");

  fseek(config_file, 0, SEEK_END);

  long config_file_len = ftell(config_file);

  fseek(config_file, 0, SEEK_SET);

  char *config_buf = NULL;
  config_buf = malloc((size_t)config_file_len + 1);

  fread(config_buf, (size_t)config_file_len, 1, config_file);
  config_buf[config_file_len] = '\0';

  /* Check the stripe_size field in the configuration file */
  char *substr;
  substr = strstr(config_buf, "stripe_size");

  int64_t read_stripe_size;
  int read_stripe_count;

  sscanf(substr, "stripe_size=%" PRId64, &read_stripe_size);
  if(! compareValuescgSize_t((cgsize_t)read_stripe_size, (cgsize_t)h5subf_config.stripe_size)) {
    printf("*FAILED* subfiling stripe size value is incorrect \n");
    cgp_error_exit();
  }

  /* Check the subfile_count field in the configuration file */
  substr = strstr(config_buf, "subfile_count");
  sscanf(substr, "subfile_count=%d", &read_stripe_count);
  if(! compareValuesInt(read_stripe_count, h5subf_config.stripe_count)) {
    printf("*FAILED* subfiling stripe count value is incorrect \n");
    cgp_error_exit();
  }

  if(comm_rank == 0) write_test_status(PASSED, "Check subfiling configure parameters", NULL);

  /* ====================================== */
  /* ==    **  READ THE CGNS SUBFILES ** == */
  /* ====================================== */
  MPI_Barrier(MPI_COMM_WORLD);

  test_reading(fname);

  /* ================================================= */
  /* ==  FUSE THE SUBFILES INTO A SINGLE CGNS FILE, == */
  /* ==  AND CHECK READING THE FILE                 == */
  /* ================================================= */

  int skip_test = 0;

  FILE *h5fuse_script = fopen("h5fuse", "r");
  if (h5fuse_script)
    fclose(h5fuse_script);
  else
    skip_test = 1;

  if (skip_test == 0) {

    if(comm_rank == 0) {

      pid_t pid = 0;
      int   status;

      pid = fork();

      if (pid == 0) {
        char *args[8];

        args[0] = strdup("env");
        args[1] = strdup("sh");
        args[2] = strdup("h5fuse");
        args[3] = strdup("-q");
        args[4] = strdup("-r");
        args[5] = strdup("-f");
        args[6] = config_filename;
        args[7] = NULL;

        /* Call h5fuse script from MPI rank 0 */
        execvp("env", args);
      }
      else {
        waitpid(pid, &status, 0);

        if (WIFEXITED(status)) {
          int ret;

          if ((ret = WEXITSTATUS(status)) != 0) {
            printf("h5fuse process exited with error code %d\n", ret);
            fflush(stdout);
            MPI_Abort(MPI_COMM_WORLD, -1);
          }
        }
        else {
          printf("h5fuse process terminated abnormally\n");
          fflush(stdout);
          MPI_Abort(MPI_COMM_WORLD, -1);
        }
      }
    }

    if(comm_rank == 0) write_test_status(PASSED, "Checking using h5fuse", NULL);

    if(comm_rank == 0) {
      printf("=====================================\n");
      printf("CHECK READING THE FUSED CGNS FILE ...\n");
      printf("=====================================\n");
    }

  /* ======================================== */
  /* ==    **  READ THE FUSED CGNS FILE ** == */
  /* ======================================== */

    MPI_Barrier(MPI_COMM_WORLD);

    if (cg_configure(CG_CONFIG_HDF5_SUBFILING, (void *)0))
      cg_error_exit();

    test_reading(fname);

    free(config_filename);

  }

#else

  if(comm_rank == 0) write_test_status(SKIP, "Checking if Subfiling is supported by HDF5", NULL);

#endif

  MPI_Finalize();

  return 0;
}


