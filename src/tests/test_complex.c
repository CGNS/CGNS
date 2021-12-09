#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <complex.h>
#undef I
#if defined(_MSC_VER) && !(defined(__INTEL_LLVM_COMPILER) || defined(__INTEL_COMPILER))
#define cg_complex_float _Fcomplex
#define cg_complex_double _Dcomplex
#define __real__(c)  c._Val[0]
#define __imag__(c)  c._Val[1]
#else
#define cg_complex_float float _Complex
#define cg_complex_double double _Complex
#endif

#if defined(_WIN32) && !defined(__NUTC__)
# include <io.h>
# define unlink _unlink
#else
# include <unistd.h>
#endif
#include "cgnslib.h"

#ifndef CGNSTYPES_H
# define cgsize_t int
#endif
#ifndef CGNS_ENUMT
# define CGNS_ENUMT(e) e
# define CGNS_ENUMV(e) e
#endif

/* from cgns_internal: so we can reset expected error messages */
void cgi_error(const char *format, ...);

int CellDim = 3, PhyDim = 3;

int cgfile, cgbase, cgzone;
cgsize_t size[9];

#define NUM_SIDE 5
#define NODE_INDEX(I,J,K) ((I)+NUM_SIDE*(((J)-1)+NUM_SIDE*((K)-1)))
#define CELL_INDEX(I,J,K) ((I)+(NUM_SIDE-1)*(((J)-1)+(NUM_SIDE-1)*((K)-1)))

int num_coord;
float *xcoord, *ycoord, *zcoord;

cg_complex_float  *var_fourier_1;
cg_complex_double *var_fourier_2;

char errmsg[256];

static float exponents[5] = {0, 1, 0, 0, 0};

void init_data();
void write_structured();
void test_complex_solution();
void release_data();

void error_exit (char *where)
{
  fprintf (stderr, "ERROR:%s:%s\n", where, cg_get_error());
  exit (1);
}

#if CG_BUILD_COMPLEX_C99_EXT
int main (int argc, char *argv[])
{

  char outfile[32];
  int nbases, nzones;

  strcpy (outfile, "complex_data.cgns");
  if (argc > 1) {
    int type = 0;
    int n = 0;
    if (argv[1][n] == '-') n++;
    if (argv[1][n] == 'a' || argv[1][n] == 'A' || argv[1][n] == '1') {
    if (NULL != strchr(argv[1], '2'))
      type = CG_FILE_ADF2;
    else
      type = CG_FILE_ADF;
  }
  else if (argv[1][n] == 'h' || argv[1][n] == 'H' || argv[1][n] == '2') {
    type = CG_FILE_HDF5;
  }
  else if (argv[1][n] == '3') {
    type = CG_FILE_ADF2;
  }
  else {
    fprintf(stderr, "unknown option\n");
    exit (1);
  }
  if (cg_set_file_type(type))
    error_exit("cg_set_file_type");
  }
  
  init_data();
  unlink(outfile);
  if (cg_open(outfile, CG_MODE_WRITE, &cgfile)) error_exit("cg_open");
  write_structured();
  if (cg_close(cgfile)) error_exit("cg_close");

  if (cg_open(outfile, CG_MODE_READ, &cgfile)) error_exit("cg_open");
  if (cg_nbases(cgfile, &nbases)) error_exit("cg_nbases");
  for (cgbase = 1; cgbase <= nbases; cgbase++) {
    if (cg_nzones(cgfile, cgbase, &nzones)) error_exit("cg_nzones");
    for (cgzone = 1; cgzone <= nzones; cgzone++) {
      test_complex_solution();
    }
  }
  if (cg_close(cgfile)) error_exit("cg_close");

  return 0;
}

void init_data()
{
  int n, i, j, k;

  /* compute coordinates - make it twice as big for use with cylindrical */

  num_coord = NUM_SIDE * NUM_SIDE * NUM_SIDE;
  xcoord = (float *) malloc (3 * num_coord * sizeof(float));
  if (NULL == xcoord) {
    fprintf(stderr, "malloc failed for coordinates\n");
    exit(1);
  }
  ycoord = xcoord + num_coord;
  zcoord = ycoord + num_coord;
  for (n = 0, k = 0; k < NUM_SIDE; k++) {
    for (j = 0; j < NUM_SIDE; j++) {
      for (i = 0; i < NUM_SIDE; i++, n++) {
        xcoord[n] = (float)i;
        ycoord[n] = (float)j;
        zcoord[n] = (float)k;
      }
    }
  }

  /* compute solution values */
  var_fourier_1 = (cg_complex_float *) malloc(num_coord * sizeof(cg_complex_float));
  if (NULL == var_fourier_1) {
    fprintf(stderr, "malloc failed for solution data\n");
    exit(1);
  }
  var_fourier_2 = (cg_complex_double *) malloc(num_coord * sizeof(cg_complex_double));
  if (NULL == var_fourier_2) {
    fprintf(stderr, "malloc failed for solution data\n");
    exit(1);
  }

  for (n = 0, k = 0; k < NUM_SIDE; k++) {
    for (j = 0; j < NUM_SIDE; j++) {
      for (i = 0; i < NUM_SIDE; i++, n++) {
        __real__(var_fourier_1[n]) = ((float)i);
        __imag__(var_fourier_1[n]) = ((float)j);
        __real__(var_fourier_2[n]) = ((float)j);
        __imag__(var_fourier_2[n]) = ((float)k);
      }
    }
  }
}

void release_data(){
  if (xcoord != NULL)
    free(xcoord);
  if (var_fourier_1 != NULL)
    free(var_fourier_1);
  if (var_fourier_2 != NULL)
    free(var_fourier_2);
}

void write_coords(int nz)
{
  int k, nn, n, nij, koff, cgcoord;

  koff = nz == 1 ? 1 - NUM_SIDE : 0;
  nij = NUM_SIDE * NUM_SIDE;
  for (n = 0, k = 0; k < NUM_SIDE; k++) {
    for (nn = 0; nn < nij; nn++)
      zcoord[n++] = (float)(k + koff);
  }

  if (cg_coord_write(cgfile, cgbase, nz, CGNS_ENUMV(RealSingle),
      "CoordinateX", xcoord, &cgcoord) ||
      cg_coord_write(cgfile, cgbase, nz, CGNS_ENUMV(RealSingle),
      "CoordinateY", ycoord, &cgcoord) ||
      cg_coord_write(cgfile, cgbase, nz, CGNS_ENUMV(RealSingle),
      "CoordinateZ", zcoord, &cgcoord)) {
    sprintf (errmsg, "zone %d coordinates", nz);
    error_exit(errmsg);
  }
  for (n = 1; n <= 3; n++) {
    if (cg_goto(cgfile, cgbase, "Zone_t", nz, "GridCoordinates_t", 1,
        "DataArray_t", n, NULL) ||
        cg_exponents_write(CGNS_ENUMV(RealSingle), exponents))
      error_exit("coordinate exponents");
  }
}

void write_solution(int nz)
{
  int cgfld, cgsol;
  cgsize_t dims[3] = {NUM_SIDE, NUM_SIDE, NUM_SIDE};

  if (cg_sol_write(cgfile, cgbase, nz, "Solution", CGNS_ENUMV(Vertex), &cgsol) ||
    cg_field_write(cgfile, cgbase, nz, cgsol, CGNS_ENUMV(ComplexSingle), "Pressure", var_fourier_1, &cgfld) ||
    cg_field_write(cgfile, cgbase, nz, cgsol, CGNS_ENUMV(ComplexDouble), "Velocity", var_fourier_2, &cgfld)) {
    sprintf (errmsg, "zone %d Complex solution", nz);
    error_exit(errmsg);
  }

  if (cg_goto(cgfile, cgbase, "Zone_t", nz, "FlowSolution_t", 1,"end") ||
     cg_array_write("Density", CGNS_ENUMV(ComplexDouble), 3, dims, var_fourier_2)){
    sprintf (errmsg, "Complex array write failed in zone %d", nz);
    error_exit(errmsg);
  }
}

/*------------------------------------------------------------------------
 * structured grid
 *------------------------------------------------------------------------*/

void write_structured()
{
  int n;
  char name[33];

  if (cg_base_write(cgfile, "Structured", CellDim, PhyDim, &cgbase) ||
      cg_goto(cgfile, cgbase, "end") ||
      cg_descriptor_write("Descriptor", "Multi-block Structured Grid") ||
      cg_dataclass_write(CGNS_ENUMV(Dimensional)) ||
      cg_units_write(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter),
      CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), CGNS_ENUMV(Radian)))
    error_exit("structured base");

  /* write zones */
  for (n = 0; n < 3; n++) {
    size[n]   = NUM_SIDE;
    size[n+3] = NUM_SIDE - 1;
    size[n+6] = 0;
  }
  for (n = 1; n <= 2; n++) {
    sprintf(name, "Zone%d", n);
    if (cg_zone_write(cgfile, cgbase, name, size,
        CGNS_ENUMV(Structured), &cgzone)) {
      sprintf (errmsg, "structured zone %d", n);
      error_exit(errmsg);
    }
    write_coords(n);
    write_solution(n);
  }
}

void test_complex_solution()
{
  int cgsol=1;
  int i,j,k,n;
  cgsize_t rmin[3] = {1, 1, 1};
  cgsize_t rmax[3] = {NUM_SIDE, NUM_SIDE, NUM_SIDE};

  float *fbuf;
  double *dbuf;
  cg_complex_float *cfbuf;
  cg_complex_double *cdbuf;

  cfbuf = (cg_complex_float *) malloc(num_coord * sizeof(cg_complex_float));
  cdbuf = (cg_complex_double *) malloc(num_coord * sizeof(cg_complex_double));
  fbuf = (float *) malloc(num_coord * 2 * sizeof(float));
  dbuf = (double *) malloc(num_coord * 2 * sizeof(double));


  if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Pressure", CGNS_ENUMV(ComplexSingle), rmin, rmax, cfbuf))
    error_exit("Error reading ComplexSingle data");

  if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Pressure", CGNS_ENUMV(ComplexDouble), rmin, rmax, cdbuf))
    error_exit("Error reading ComplexSingle data to ComplexDouble data");

  /* This case should failed because we do not want to have Out Of Bounds access on fbuf */
  if (!cg_field_read(cgfile, cgbase, cgzone, cgsol, "Pressure", CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) {
     printf("Error reading ComplexSingle data to real single data failed to report error\n");
     exit(1);
  }
  cg_error_print();
  cgi_error("no CGNS error reported"); /* reset */
 
  /* This aliasing should work to garantee that complex float matches float[2] */
  if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Pressure", CGNS_ENUMV(ComplexSingle), rmin, rmax, fbuf))
    error_exit("Error reading ComplexSingle data to real single data");

  /* This case should failed because we do not want to have Out Of Bounds access on dbuf */
  if (!cg_field_read(cgfile, cgbase, cgzone, cgsol, "Pressure", CGNS_ENUMV(RealDouble), rmin, rmax, dbuf)) {
     printf("Error reading ComplexDouble data to real double data failed to report error\n");
     exit(1);
  }
  cg_error_print();
  cgi_error("no CGNS error reported"); /* reset */

  /* This aliasing should work to garantee that complex double matches double[2] */
  if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Pressure", CGNS_ENUMV(ComplexDouble), rmin, rmax, dbuf))
    error_exit("Error reading ComplexDouble data to real double data");

  // Now check some values read !
  for (n = 0, k = 0; k < NUM_SIDE; k++) {
    for (j = 0; j < NUM_SIDE; j++) {
      for (i = 0; i < NUM_SIDE; i++, n++) {
        if ((crealf(var_fourier_1[n]) != crealf(cfbuf[n])) ||
            (cimagf(var_fourier_1[n]) != cimagf(cfbuf[n]))) {
          printf("%f + %f I !=  %f + %f I", crealf(var_fourier_1[n]), cimagf(var_fourier_1[n]),
                                                crealf(cfbuf[n]), cimagf(cfbuf[n]));
          error_exit("Read of complex single data failed\n");
        }
        if ((creal(cdbuf[n]) != (double)i) ||
            (cimag(cdbuf[n]) != (double)j)) {
          printf("%lf + %lf I !=  %lf + %lf I", (double)i, (double)j, creal(cdbuf[n]), cimag(cdbuf[n]));
          error_exit("Read of complex single to complex double data failed\n");
        }
        if ((crealf(var_fourier_1[n]) != fbuf[2*n]) ||
            (cimagf(var_fourier_1[n]) != fbuf[2*n+1])) {
          error_exit("Read of complex single to aliased single array failed\n");
        }
        if (((double)crealf(var_fourier_1[n]) != dbuf[2*n]) ||
            ((double)cimagf(var_fourier_1[n]) != dbuf[2*n+1])) {
          error_exit("Read of complex single to double array failed\n");
        }
      }
    }
  }

  if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Velocity", CGNS_ENUMV(ComplexSingle), rmin, rmax, cfbuf))
    error_exit("Error reading ComplexDouble data to ComplexSingle data");

  if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Velocity", CGNS_ENUMV(ComplexDouble), rmin, rmax, cdbuf))
    error_exit("Error reading ComplexDouble data");

  /* This case should failed because we do not want to have Out Of Bounds access on fbuf */
  if (!cg_field_read(cgfile, cgbase, cgzone, cgsol, "Velocity", CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) {
     printf("Error reading ComplexDouble data to real single data failed to report error\n");
     exit(1);
  }
  cg_error_print();
  cgi_error("no CGNS error reported"); /* reset */
 
  /* This aliasing should work to garantee that complex float matches float[2] */
  if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Velocity", CGNS_ENUMV(ComplexSingle), rmin, rmax, fbuf))
    error_exit("Error reading ComplexDouble data to real single data");

  /* This case should failed because we do not want to have Out Of Bounds access on dbuf */
  if (!cg_field_read(cgfile, cgbase, cgzone, cgsol, "Velocity", CGNS_ENUMV(RealDouble), rmin, rmax, dbuf)) {
     printf("Error reading ComplexDouble data to real double data failed to report error\n");
     exit(1);
  }
  cg_error_print();
  cgi_error("no CGNS error reported"); /* reset */

  /* This aliasing should work to garantee that complex double matches double[2] */
  if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Velocity", CGNS_ENUMV(ComplexDouble), rmin, rmax, dbuf))
    error_exit("Error reading ComplexDouble data to real double data");

  // Now check some values read !
  for (n = 0, k = 0; k < NUM_SIDE; k++) {
    for (j = 0; j < NUM_SIDE; j++) {
      for (i = 0; i < NUM_SIDE; i++, n++) {
        if ((creal(var_fourier_2[n]) != creal(cdbuf[n])) ||
            (cimag(var_fourier_2[n]) != cimag(cdbuf[n]))) {
          printf("%lf + %lf I !=  %lf + %lf I", creal(var_fourier_2[n]), cimag(var_fourier_2[n]),
                                            creal(cdbuf[n]), cimag(cdbuf[n]));
          error_exit("Read of complex double data failed\n");
        }
        if ((crealf(cfbuf[n]) != ((float)j)) ||
            (cimagf(cfbuf[n]) != ((float)k))) {
          printf("%f + %f I !=  %f + %f I", (float)j, (float)k, crealf(cfbuf[n]), cimagf(cfbuf[n]));
          error_exit("Read of complex double to complex single data failed\n");
        }
        if ((creal(var_fourier_2[n]) != dbuf[2*n]) ||
            (cimag(var_fourier_2[n]) != dbuf[2*n+1])) {
          error_exit("Read of complex double to aliased double array failed\n");
        }
        if (((float)creal(var_fourier_2[n]) != fbuf[2*n]) ||
            ((float)cimag(var_fourier_2[n]) != fbuf[2*n+1])) {
          error_exit("Read of complex double to single array failed\n");
        }
      }
    }
  }

  if (cg_gopath(cgfile, "/Structured/Zone1/Solution") ||
      cg_array_general_read(3, rmin, rmax, CGNS_ENUMV(ComplexDouble),
                            3, rmax, rmin, rmax, cdbuf))
    error_exit("Read of complex array failed");

  for (n = 0, k = 0; k < NUM_SIDE; k++) {
    for (j = 0; j < NUM_SIDE; j++) {
      for (i = 0; i < NUM_SIDE; i++, n++) {
        if ((creal(var_fourier_2[n]) != creal(cdbuf[n])) ||
            (cimag(var_fourier_2[n]) != cimag(cdbuf[n]))) {
          printf("%lf + %lf I !=  %lf + %lf I", creal(var_fourier_2[n]), cimag(var_fourier_2[n]),
                                            creal(cdbuf[n]), cimag(cdbuf[n]));
          error_exit("Read of complex double data failed\n");
        }
      }
    }
  }

  free(dbuf);
  free(fbuf);
  free(cdbuf);
  free(cfbuf);
}
#else
int main (int argc, char *argv[])
{
  return 0;
}
#endif
