/*
! @file benchmark_hdf5.F90
! @author M. Scot Breitenfeld <brtnfld@hdfgroup.org>
! @version 0.1
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Benchmarking program for cgns library. The mesh is a single
! element layer of wedge elements in the X- and Z-directions.
! The value of nnY is the number of nodes along the Y direction. 
!
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define _USE_MATH_DEFINES /* for windows */
#include <math.h>
#include <time.h>

#include "cgnslib.h"

#define false 0
#define true 1

/* Number of nodes on one edge of the box */
cgsize_t nnY=32768;


cgsize_t Nelem;
cgsize_t NodePerElem = 6;
cgsize_t Nnodes;

int comm_rank;
int fn;
int B;
int Z;
int S;
int Cx,Cy,Cz, Fx, Fy, Fz, Ar=1, Ai=2;
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

int c_double_eq(double a, double b) {

  double eps = 1.e-8;

  if(fabs(a-b) < eps) {
    return true;
  }
  return false;
}

int main(int argc, char* argv[]) {

  char fname[32];
  char name[32];

  clock_t t0, tic, toc;

  size_t Mb_coor, Mb_elem, Mb_field, Mb_array;

  double DimY = 10.0;
  double dy;
  double dval;
  cgsize_t i;
 
  /* parameters */
  debug = true;
  comm_rank = 1;

  t0 = clock(); /* Timer */

  Nnodes = nnY*4;
  Nelem = 2*(nnY-1);

  nijk[0] = Nnodes; /* Number of vertices */
  nijk[1] = Nelem; /* Number of cells */
  nijk[2] = 0; /* Number of boundary vertices */

  dy = DimY/nnY;

  /* Compute the size of the arrays */
  Mb_coor = sizeof(double)*3*Nnodes/1048576;
  Mb_elem = sizeof(cgsize_t)*Nelem*NodePerElem/1048576;
  Mb_field = sizeof(double)*3*Nnodes/1048576;
  Mb_array = (sizeof(double)+sizeof(cgsize_t))*Nnodes/1048576;

  /* ====================================== */
  /* ==    **WRITE THE CGNS FILE **      == */
  /* ====================================== */

  sprintf(fname, "benchmark.cgns");

  tic = clock();
  if(cg_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
    printf("*FAILED* cg_open \n");
    cg_error_exit();
  }
  toc = clock();
  xtiming[9] = (double)(toc - tic) / CLOCKS_PER_SEC;

  tic = clock();
  if(cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) != CG_OK) {
    printf("*FAILED* cg_base_write \n");
    cg_error_exit();
  }
  if(cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z) != CG_OK) {
    printf("*FAILED* cg_zone_write \n");
    cg_error_exit();
  }
  toc = clock();
  xtiming[13] = (double)(toc - tic) / CLOCKS_PER_SEC;

  /* ====================================== */
  /* == (A) WRITE THE NODAL COORDINATES  == */
  /* ====================================== */

  count = nijk[0];

  if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_x \n");
    cg_error_exit();
  }

  if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_y \n");
    cg_error_exit();
  }

  if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_z \n");
    cg_error_exit();
  }

  for(i=0;i < nnY; i++) {
    Coor_x[i] = 0.0;
    Coor_y[i] = dy*i;
    Coor_z[i] = 0.0;
  }
  for(i=nnY;i < nnY*2; i++) {
    Coor_x[i] = dy;
    Coor_y[i] = Coor_y[i-nnY];
    Coor_z[i] = 0.0;
  }
  for(i=nnY*2;i < nnY*3; i++) {
    Coor_x[i] = dy;
    Coor_y[i] = Coor_y[i-nnY*2];
    Coor_z[i] = dy;
  }
  for(i=nnY*3;i < nnY*4; i++) {
    Coor_x[i] = 0.0;
    Coor_y[i] = Coor_y[i-nnY*3];
    Coor_z[i] = dy;
  }

  tic = clock();
  if(cg_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX", Coor_x,&Cx) != CG_OK) {
    printf("*FAILED* cg_coord_write (Coor_x) \n");
    cg_error_exit();
  }
  if(cg_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY", Coor_y,&Cy) != CG_OK) {
    printf("*FAILED* cg_coord_write (Coor_y) \n");
    cg_error_exit();
  }
  if(cg_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ", Coor_z,&Cz) != CG_OK) {
    printf("*FAILED* cg_coord_write (Coor_z) \n");
    cg_error_exit();
  }

  toc = clock();
  xtiming[1] = (double)(toc - tic) / CLOCKS_PER_SEC;

  free(Coor_x);
  free(Coor_y);
  free(Coor_z);
  /* ====================================== */
  /* == (B) WRITE THE CONNECTIVITY TABLE == */
  /* ====================================== */

  start = 1;
  end = nijk[1];

  count = nijk[1];

  if( !(elements = malloc(count*NodePerElem*sizeof(cgsize_t)) )) {
    printf("*FAILED* allocation of elements \n");
    cg_error_exit();
  }

  /* Create connectivity table ... */
  
  i = 0;
  for( k = 0; k < Nelem/2; k++ ) {
    elements[i++] = k+1;
    elements[i++] = nnY + 2 + k;
    elements[i++] = k+2;
    elements[i++] = nnY*3 + 1 + k;
    elements[i++] = nnY*2 + 2 + k;
    elements[i++] = nnY*3 + 2 + k;
  }
  for( k = 0; k < Nelem/2; k++ ) {
   elements[i++] = k+1;
   elements[i++] = nnY + 1 + k;
   elements[i++] = nnY + 2 + k;
   elements[i++] = nnY*3 + 1 + k;
   elements[i++] = nnY*2 + 1 + k;
   elements[i++] = nnY*2 + 2 + k;
  }

  tic = clock();
  if(cg_section_write(fn,B,Z,"Elements",CGNS_ENUMV(PENTA_6), start, end, 0, elements, &S) != CG_OK) {
    printf("*FAILED* cgp_section_write \n");
    cg_error_exit();
  }
  toc = clock();
  xtiming[2] = (double)(toc - tic) / CLOCKS_PER_SEC;

  free(elements);

  /* ====================================== */
  /* == (C) WRITE THE FIELD DATA         == */
  /* ====================================== */

  count = nijk[0];

  if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fx \n");
    cg_error_exit();
  }

  if( !(Data_Fy= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fy \n");
    cg_error_exit();
  }

  if( !(Data_Fz= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fz \n");
    cg_error_exit();
  }

  for ( k = 0; k < count; k++) {
    dval = M_PI*(double)k/(double)count;
    Data_Fx[k] = sin(dval);
    Data_Fy[k] = 2*Data_Fx[k];
    Data_Fz[k] = 3*Data_Fx[k];
  }

  if(cg_sol_write(fn, B, Z, "Solution", CGNS_ENUMV(Vertex), &S) != CG_OK) {
    printf("*FAILED* cg_sol_write \n");
    cg_error_exit();
  }

  tic = clock();
  if(cg_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumX",Data_Fx, &Fx) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumX) \n");
    cg_error_exit();
  }
  if(cg_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumY",Data_Fy, &Fy) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumY) \n");
    cg_error_exit();
  }
  if(cg_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumZ",Data_Fz, &Fz) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumZ) \n");
    cg_error_exit();
  }

  toc = clock();
  xtiming[3] = (double)(toc - tic) / CLOCKS_PER_SEC;

  free(Data_Fx);
  free(Data_Fy);
  free(Data_Fz);

  /* ====================================== */
  /* == (D) WRITE THE ARRAY DATA         == */
  /* ====================================== */

  count = nijk[0];

  if( !(Array_r = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Array_r \n");
    cg_error_exit();
  }

  if( !(Array_i= (cgsize_t*) malloc(count*sizeof(cgsize_t))) ) {
    printf("*FAILED* allocation of Array_i  \n");
    cg_error_exit();
  }

  for ( k = 0; k < count; k++) {
    dval = M_PI*(double)k/(double)count;
    Array_r[k] = sin(dval);
    Array_i[k] = k%10 + 1;
  }

  if(cg_goto(fn, B, "Zone 1", 0, "end") != CG_OK) {
    printf("*FAILED* cg_goto\n");
    cg_error_exit();
  }

  if(cg_user_data_write("User Data") != CG_OK) {
    printf("*FAILED* cg_user_data_write \n");
    cg_error_exit();
  }

  if(cg_gorel(fn,"User Data",0,"end") != CG_OK) {
    printf("*FAILED* cg_gorel\n");
    cg_error_exit();
  }

  size_1D[0] = nijk[0];

  tic = clock();
  if(cg_array_write("ArrayR",CGNS_ENUMV(RealDouble),1,size_1D, Array_r) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array_Ar)\n");
    cg_error_exit();
  }

#if CG_BUILD_64BIT
  if(cg_array_write("ArrayI",CGNS_ENUMV(LongInteger),1,size_1D, Array_i) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array_Ai)\n");
    cg_error_exit();
  }
#else
  if(cg_array_write("ArrayI",CGNS_ENUMV(Integer),1,size_1D, Array_i) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array Ai)\n");
    cg_error_exit();
  }
#endif

  toc = clock();
  xtiming[4] = (double)(toc - tic) / CLOCKS_PER_SEC;

  free(Array_r);
  free(Array_i);

  tic = clock();
  if(cg_close(fn) != CG_OK) {
    printf("*FAILED* cg_close \n");
    cg_error_exit();
  };
  toc = clock();
  xtiming[11] = (double)(toc - tic) / CLOCKS_PER_SEC;


  /* ====================================== */
  /* ==    **  READ THE CGNS FILE **     == */
  /* ====================================== */

  tic = clock();
  /* Open the cgns file for reading */
  if(cg_open(fname, CG_MODE_MODIFY, &fn) != CG_OK) {
    printf("*FAILED* cg_open \n");
    cg_error_exit();
  }
  toc = clock();
  xtiming[10] = (double)(toc - tic) / CLOCKS_PER_SEC;

  /* Read the base information */
  tic = clock();
  if(cg_base_read(fn, B, name, &r_cell_dim, &r_phys_dim) != CG_OK) {
    printf("*FAILED* cg_base_read\n");
    cg_error_exit();
  }

  if(r_cell_dim != cell_dim || r_phys_dim != phys_dim) {
    printf("*FAILED* bad cell dim=%d or phy dim=%d\n", r_cell_dim, r_phys_dim);
    cg_error_exit();
  }

  if (strcmp (name, "Base 1")) {
    printf("*FAILED* bad base name=%s\n", name);
    cg_error_exit();
  }
  /* Read the zone information */
  tic = clock();
  if(cg_zone_read(fn, B, Z, name, sizes) != CG_OK) {
    printf("*FAILED* cg_zoneread\n");
    cg_error_exit();
  }
  toc = clock();
  xtiming[14] = (double)(toc - tic) / CLOCKS_PER_SEC;

  /* Check the read zone information is correct */
  if(sizes[0] != Nnodes) {
    printf("bad num points=%ld\n", (long)sizes[0]);
    cg_error_exit();
  }

  if(sizes[1] != Nelem) {
    printf("bad num points=%ld\n", (long)sizes[1]);
    cg_error_exit();
  }

  if(sizes[2] != 0) {
    printf("bad num points=%ld\n", (long)sizes[2]);
    cg_error_exit();
  }

  if (strcmp (name, "Zone 1")) {
    printf("bad zone name=%s\n", name);
    cg_error_exit();
  }
  /* ====================================== */
  /* ==  (A) READ THE NODAL COORDINATES  == */
  /* ====================================== */

  count = nijk[0];

  if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_x \n");
    cg_error_exit();
  }

  if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_y \n");
    cg_error_exit();
  }

  if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_z \n");
    cg_error_exit();
  }
  min = 1;
  max = count;

  tic = clock();
  if (cg_coord_read(fn,B,Z,"CoordinateX",CGNS_ENUMV(RealDouble),&min,&max,Coor_x) != CG_OK) {
    printf("*FAILED* cg_coord_read_data ( Reading Coor_x) \n");
    cg_error_exit();
  }
  if (cg_coord_read(fn,B,Z,"CoordinateY",CGNS_ENUMV(RealDouble),&min,&max,Coor_y) != CG_OK) {
    printf("*FAILED* cg_coord_read_data (Reading Coor_y) \n");
    cg_error_exit();
  }
  if (cg_coord_read(fn,B,Z,"CoordinateZ",CGNS_ENUMV(RealDouble),&min,&max,Coor_z) != CG_OK) {
    printf("*FAILED* cg_coord_read_data (Reading Coor_z) \n");
    cg_error_exit();
  }
  toc = clock();
  xtiming[5] = (double)(toc - tic) / CLOCKS_PER_SEC;

  /* Check if read the data back correctly */
  if(debug) {
    double val_x, val_y, val_z;
    for ( i = 0; i < count; i++) {

      if(i >= 0 && i < nnY) {
	val_x = 0.0;
	val_y = dy*i;
	val_z = 0.0;
      } else if( i >= nnY && i <nnY*2) {
	val_x = dy;
	val_y = dy*(i-nnY);
	val_z = 0.0;
      } else if( i >= nnY*2 && i <nnY*3) {
	val_x = dy;
	val_y = dy*(i-nnY*2);
	val_z = dy;
      } else if(  i >= nnY*3 && i <nnY*4) {
	val_x = 0.0;
	val_y = dy*(i-nnY*3);
	val_z = dy;
      }

      if( !c_double_eq(Coor_x[i], val_x) ||
	  !c_double_eq(Coor_y[i], val_y) ||
	  !c_double_eq(Coor_z[i], val_z) ) {
	   printf("*FAILED* cg_coord_read_data values are incorrect \n");
	   cg_error_exit();
      }
    }
  }

  free(Coor_x);
  free(Coor_y);
  free(Coor_z);

/* ====================================== */
/* == (B) READ THE CONNECTIVITY TABLE  == */
/* ====================================== */

  count = nijk[1];
  if( !(elements = malloc(count*NodePerElem*sizeof(cgsize_t)) )) {
    printf("*FAILED* allocation of elements \n");
    cg_error_exit();
  }

  tic = clock();
  if( cg_elements_read(fn, B, Z, S, elements, NULL) != CG_OK) {
    printf("*FAILED* cg_elements_read_data ( Reading elements) \n");
    cg_error_exit();
  }
  toc = clock();
  xtiming[6] = (double)(toc - tic) / CLOCKS_PER_SEC;

  if(debug) {
    i = 0;
    for( k = 0; k < Nelem/2; k++ ) {
      if(elements[i++] != k+1) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY + 2 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != k+2) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY*3 + 1 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY*2 + 2 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY*3 + 2 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
    }
    for( k = 0; k < Nelem/2; k++ ) {
      if(elements[i++] != k+1) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY + 1 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY + 2 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY*3 + 1 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY*2 + 1 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
      if(elements[i++] != nnY*2 + 2 + k) {
	printf("*FAILED* cg_elements_read values are incorrect\n");
	cg_error_exit();
      }
    }
  }
  free(elements);


  /* ====================================== */
  /* == (C) READ THE FIELD DATA          == */
  /* ====================================== */
  count = nijk[0];

  if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Reading Data_Fx \n");
    cg_error_exit();
  }

  if( !(Data_Fy = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of  Reading Data_Fy \n");
    cg_error_exit();
  }

  if( !(Data_Fz = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of  Reading Data_Fz \n");
    cg_error_exit();
  }

  tic = clock();

  if (cg_field_read(fn,B,Z,S,"MomentumX",CGNS_ENUMV(RealDouble),&min,&max,Data_Fx) != CG_OK) {
    printf("*FAILED* cgp_field_read (Data_Fx) \n");
    cg_error_exit();
  }
  if (cg_field_read(fn,B,Z,S,"MomentumY",CGNS_ENUMV(RealDouble),&min,&max,Data_Fy) != CG_OK) {
    printf("*FAILED* cgp_field_read (Data_Fy) \n");
    cg_error_exit();
  }
  if (cg_field_read(fn,B,Z,S,"MomentumZ",CGNS_ENUMV(RealDouble),&min,&max,Data_Fz) != CG_OK) {
    printf("*FAILED* cgp_field_read (Data_Fz) \n");
    cg_error_exit();
  }

  toc = clock();
  xtiming[7] = (double)(toc - tic) / CLOCKS_PER_SEC;

  /* Check if read the data back correctly */
  if(debug) {
    for ( k = 0; k < count; k++) {
      dval = M_PI*(double)k/(double)count;
      if(!c_double_eq(Data_Fx[k],sin(dval))  ||
	 !c_double_eq(Data_Fy[k],2*sin(dval))  ||
	 !c_double_eq(Data_Fz[k],3*sin(dval))  ) {
	printf("*FAILED* cg_field_read values are incorrect \n");
	cg_error_exit();
      }
    }
  }
  free(Data_Fx);
  free(Data_Fy);
  free(Data_Fz);

  /* ====================================== */
  /* == (D) READ THE ARRAY DATA          == */
  /* ====================================== */

  count = nijk[0];

  if( !(Array_r = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of  Reading Array_r \n");
    cg_error_exit();
  }

  if( !(Array_i= (cgsize_t*) malloc(count*sizeof(cgsize_t))) ) {
    printf("*FAILED* allocation of  Reading Array_i  \n");
    cg_error_exit();
  }

  if(cg_goto(fn,B,"Zone_t",Z,"UserDefinedData_t",1,"end") != CG_OK) {
    printf("*FAILED* cg_goto (User Defined Data)\n");
    cg_error_exit();
  }

  tic = clock();
  if( cg_array_read(Ar, Array_r) != CG_OK) {
    printf("*FAILED* cg_array_read (Array_r) \n");
    cg_error_exit();
  }
  if( cg_array_read(Ai, Array_i) != CG_OK) {
    printf("*FAILED* cg_array_read (Array_i) \n");
    cg_error_exit();
  }
  toc = clock();
  xtiming[8] = (double)(toc - tic) / CLOCKS_PER_SEC;

  /* Check if read the data back correctly */
  if(debug) {
    for ( k = 0; k < count; k++) {
      dval = M_PI*(double)k/(double)count;;
      if(!c_double_eq(Array_r[k], sin(dval)) ||
	 Array_i[k] != k%10 + 1) {
	  printf("*FAILED* cg_array_read values are incorrect \n");
	  cg_error_exit();
      }
    }
  }

  free(Array_r);
  free(Array_i);

  tic = clock();
  if(cg_close(fn) !=CG_OK) {
     printf("*FAILED* cg_close\n");
     cg_error_exit();
  }
  toc = clock();
  xtiming[12] = (double)(toc - tic) / CLOCKS_PER_SEC;

  xtiming[0] = (double)(toc - t0) / CLOCKS_PER_SEC;
  sprintf(fname, "timing.dat");
  FILE *fid = fopen(fname, "w");
  if (fid == NULL) {
    printf("Error opening timing file!\n");
  } else {
    fprintf(fid,"#nprocs, total time, write: coord., elem., field, array, read: coord., elem., field, array, MB: coord, elem, field, array \n");
    
    for ( k = 0; k < 15; k++) {
      fprintf(fid," %.3f",xtiming[k]);
    }
    fprintf(fid," %zu %zu %zu %zu \n", Mb_coor, Mb_elem, Mb_field, Mb_array);
    fclose(fid);
  }
  
  return 0;
}


