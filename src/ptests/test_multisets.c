/*
 * test_multisets.c - Tests for the multi-dataset parallel I/O API.
 *
 * A single parameterized driver, run_multiset_test(), exercises four
 * NULL-buffer scenarios for cgp_{coord,field,array}_multi_{write,read}_data:
 *
 *   NULL_MODE_NONE    – all ranks contribute real data (round-trip correctness).
 *   NULL_MODE_MIXED   – all ranks but the last contribute; last rank uses NULL.
 *   NULL_MODE_RANK0   – only rank 0 contributes; all others use NULL.
 *   NULL_MODE_PARTIAL – every rank contributes, but the middle dataset in each
 *                       multi call is NULL (tests per-dataset has_data_k logic).
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

typedef enum { NULL_MODE_NONE, NULL_MODE_MIXED, NULL_MODE_RANK0, NULL_MODE_PARTIAL } NullMode;

/* Return 1 if this rank should supply NULL for a given dataset index.
 *   NONE    – never NULL
 *   MIXED   – last rank is NULL for every dataset
 *   RANK0   – all ranks except 0 are NULL for every dataset
 *   PARTIAL – every rank NULLs only dataset index 1 (the "middle" one)
 */
static int is_null_buf(NullMode mode, int dataset_idx)
{
  switch (mode) {
    case NULL_MODE_NONE:    return 0;
    case NULL_MODE_MIXED:   return (comm_rank == comm_size - 1);
    case NULL_MODE_RANK0:   return (comm_rank != 0);
    case NULL_MODE_PARTIAL: return (dataset_idx == 1);
  }
  return 0;
}

/* Return 1 if this rank should verify read-back data for a dataset index. */
static int should_verify(NullMode mode, int dataset_idx)
{
  return !is_null_buf(mode, dataset_idx);
}

static void run_multiset_test(const char *tag, NullMode mode)
{
  char fname[64], label[128];
  void **buf;
  int Cvec[3], Fvec[3], Avec[2];
  int fn, B, Z, S;
  int Cx, Cy, Cz, Fx, Fy, Fz, Ar, Ai;
  cgsize_t nijk[3], size_1D[1];
  cgsize_t min, max, k, count;
  double *Coor_x = NULL, *Coor_y = NULL, *Coor_z = NULL;
  double *Data_Fx = NULL, *Data_Fy = NULL, *Data_Fz = NULL;
  double   *Array_r = NULL;
  cgsize_t *Array_i = NULL;
  int err;

  nijk[0] = 100 * comm_size;
  nijk[1] = 100 * comm_size;
  nijk[2] = 0;
  count = nijk[0] / comm_size;
  min   = count * comm_rank + 1;
  max   = count * (comm_rank + 1);

  sprintf(fname, "%s_%06d.cgns", tag, comm_size);

  /* ============================== */
  /* ==    WRITE THE CGNS FILE   == */
  /* ============================== */

  if (cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
    printf("*FAILED* cgp_open (%s write)\n", tag); cgp_error_exit();
  }
  cg_base_write(fn, "Base 1", 3, 3, &B);
  cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z);

  /* ---- Coordinates ---- */

  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", &Cx);
  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", &Cy);
  cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateZ", &Cz);
  Cvec[0] = Cx; Cvec[1] = Cy; Cvec[2] = Cz;

  buf = (void **)malloc(3 * sizeof(void *));
  if (!is_null_buf(mode, 0)) {
    Coor_x = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) Coor_x[k] = comm_rank * count + k + 1.1;
    buf[0] = Coor_x;
  } else { buf[0] = NULL; }
  if (!is_null_buf(mode, 1)) {
    Coor_y = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) Coor_y[k] = comm_rank * count + k + 2.2;
    buf[1] = Coor_y;
  } else { buf[1] = NULL; }
  if (!is_null_buf(mode, 2)) {
    Coor_z = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) Coor_z[k] = comm_rank * count + k + 3.3;
    buf[2] = Coor_z;
  } else { buf[2] = NULL; }

  err = cgp_coord_multi_write_data(fn, B, Z, Cvec, &min, &max,
                                   3, (const void **)buf);
  if (comm_rank == 0) {
    sprintf(label, "cgp_coord_multi_write_data (%s)", tag);
    write_test_status(err == CG_OK ? PASSED : FAILED, label, NULL);
  }
  if (err != CG_OK) cgp_error_exit();
  free(buf); free(Coor_x); free(Coor_y); free(Coor_z);
  Coor_x = NULL; Coor_y = NULL; Coor_z = NULL;

  /* ---- Fields ---- */

  cg_sol_write(fn, B, Z, "Solution", CGNS_ENUMV(Vertex), &S);
  cgp_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "FieldA", &Fx);
  cgp_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "FieldB", &Fy);
  cgp_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "FieldC", &Fz);
  Fvec[0] = Fx; Fvec[1] = Fy; Fvec[2] = Fz;

  buf = (void **)malloc(3 * sizeof(void *));
  if (!is_null_buf(mode, 0)) {
    Data_Fx = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) Data_Fx[k] = comm_rank * count + k + 10.1;
    buf[0] = Data_Fx;
  } else { buf[0] = NULL; }
  if (!is_null_buf(mode, 1)) {
    Data_Fy = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) Data_Fy[k] = comm_rank * count + k + 20.2;
    buf[1] = Data_Fy;
  } else { buf[1] = NULL; }
  if (!is_null_buf(mode, 2)) {
    Data_Fz = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) Data_Fz[k] = comm_rank * count + k + 30.3;
    buf[2] = Data_Fz;
  } else { buf[2] = NULL; }

  err = cgp_field_multi_write_data(fn, B, Z, S, Fvec, &min, &max,
                                   3, (const void **)buf);
  if (comm_rank == 0) {
    sprintf(label, "cgp_field_multi_write_data (%s)", tag);
    write_test_status(err == CG_OK ? PASSED : FAILED, label, NULL);
  }
  if (err != CG_OK) cgp_error_exit();
  free(buf); free(Data_Fx); free(Data_Fy); free(Data_Fz);
  Data_Fx = NULL; Data_Fy = NULL; Data_Fz = NULL;

  /* ---- Arrays ---- */

  cg_goto(fn, B, "Zone_t", Z, "end");
  cg_user_data_write("User Data");
  cg_gorel(fn, "User Data", 0, "end");
  size_1D[0] = nijk[0];
  cgp_array_write("ArrayR", CGNS_ENUMV(RealDouble), 1, size_1D, &Ar);
#if CG_BUILD_64BIT
  cgp_array_write("ArrayI", CGNS_ENUMV(LongInteger), 1, size_1D, &Ai);
#else
  cgp_array_write("ArrayI", CGNS_ENUMV(Integer),     1, size_1D, &Ai);
#endif
  Avec[0] = Ai; Avec[1] = Ar;

  buf = (void **)malloc(2 * sizeof(void *));
  if (!is_null_buf(mode, 0)) {
    Array_i = (cgsize_t *)malloc(count * sizeof(cgsize_t));
    for (k = 0; k < count; k++) Array_i[k] = comm_rank * count + k + 1;
    buf[0] = Array_i;
  } else { buf[0] = NULL; }
  if (!is_null_buf(mode, 1)) {
    Array_r = (double *)malloc(count * sizeof(double));
    for (k = 0; k < count; k++) Array_r[k] = comm_rank * count + k + 1.001;
    buf[1] = Array_r;
  } else { buf[1] = NULL; }

  err = cgp_array_multi_write_data(fn, Avec, &min, &max,
                                   2, (const void **)buf);
  if (comm_rank == 0) {
    sprintf(label, "cgp_array_multi_write_data (%s)", tag);
    write_test_status(err == CG_OK ? PASSED : FAILED, label, NULL);
  }
  if (err != CG_OK) cgp_error_exit();
  free(buf); free(Array_r); free(Array_i);
  Array_r = NULL; Array_i = NULL;

  cgp_close(fn);
  MPI_Barrier(comm);

  /* ============================== */
  /* ==   READ + VERIFY          == */
  /* ============================== */

  if (cgp_open(fname, CG_MODE_READ, &fn) != CG_OK) {
    printf("*FAILED* cgp_open (%s read)\n", tag); cgp_error_exit();
  }

  /* ---- Coordinates ---- */

  buf = (void **)malloc(3 * sizeof(void *));
  if (!is_null_buf(mode, 0)) {
    Coor_x = (double *)malloc(count * sizeof(double));
    buf[0] = Coor_x;
  } else { buf[0] = NULL; }
  if (!is_null_buf(mode, 1)) {
    Coor_y = (double *)malloc(count * sizeof(double));
    buf[1] = Coor_y;
  } else { buf[1] = NULL; }
  if (!is_null_buf(mode, 2)) {
    Coor_z = (double *)malloc(count * sizeof(double));
    buf[2] = Coor_z;
  } else { buf[2] = NULL; }

  sprintf(label, "cgp_coord_multi_read_data (%s)", tag);
  err = cgp_coord_multi_read_data(fn, B, Z, Cvec, &min, &max, 3, buf);
  if (err != CG_OK) {
    if (comm_rank == 0) write_test_status(FAILED, label, NULL);
    cgp_error_exit();
  }
  {
    int ok = 1;
    for (k = 0; k < count && ok; k++) {
      if (should_verify(mode, 0) &&
          !compareValuesDouble(Coor_x[k], comm_rank * count + k + 1.1))
        ok = 0;
      if (should_verify(mode, 1) &&
          !compareValuesDouble(Coor_y[k], comm_rank * count + k + 2.2))
        ok = 0;
      if (should_verify(mode, 2) &&
          !compareValuesDouble(Coor_z[k], comm_rank * count + k + 3.3))
        ok = 0;
    }
    if (comm_rank == 0)
      write_test_status(ok ? PASSED : FAILED, label, NULL);
    if (!ok) cgp_error_exit();
  }
  free(buf); free(Coor_x); free(Coor_y); free(Coor_z);
  Coor_x = NULL; Coor_y = NULL; Coor_z = NULL;

  /* ---- Fields ---- */

  buf = (void **)malloc(3 * sizeof(void *));
  if (!is_null_buf(mode, 0)) {
    Data_Fx = (double *)malloc(count * sizeof(double));
    buf[0] = Data_Fx;
  } else { buf[0] = NULL; }
  if (!is_null_buf(mode, 1)) {
    Data_Fy = (double *)malloc(count * sizeof(double));
    buf[1] = Data_Fy;
  } else { buf[1] = NULL; }
  if (!is_null_buf(mode, 2)) {
    Data_Fz = (double *)malloc(count * sizeof(double));
    buf[2] = Data_Fz;
  } else { buf[2] = NULL; }

  sprintf(label, "cgp_field_multi_read_data (%s)", tag);
  err = cgp_field_multi_read_data(fn, B, Z, S, Fvec, &min, &max, 3, buf);
  if (err != CG_OK) {
    if (comm_rank == 0) write_test_status(FAILED, label, NULL);
    cgp_error_exit();
  }
  {
    int ok = 1;
    for (k = 0; k < count && ok; k++) {
      if (should_verify(mode, 0) &&
          !compareValuesDouble(Data_Fx[k], comm_rank * count + k + 10.1))
        ok = 0;
      if (should_verify(mode, 1) &&
          !compareValuesDouble(Data_Fy[k], comm_rank * count + k + 20.2))
        ok = 0;
      if (should_verify(mode, 2) &&
          !compareValuesDouble(Data_Fz[k], comm_rank * count + k + 30.3))
        ok = 0;
    }
    if (comm_rank == 0)
      write_test_status(ok ? PASSED : FAILED, label, NULL);
    if (!ok) cgp_error_exit();
  }
  free(buf); free(Data_Fx); free(Data_Fy); free(Data_Fz);
  Data_Fx = NULL; Data_Fy = NULL; Data_Fz = NULL;

  /* ---- Arrays ---- */

  cg_goto(fn, B, "Zone_t", Z, "UserDefinedData_t", 1, "end");
  Avec[0] = Ai; Avec[1] = Ar;  /* same order as write */

  buf = (void **)malloc(2 * sizeof(void *));
  if (!is_null_buf(mode, 0)) {
    Array_i = (cgsize_t *)malloc(count * sizeof(cgsize_t));
    buf[0] = Array_i;
  } else { buf[0] = NULL; }
  if (!is_null_buf(mode, 1)) {
    Array_r = (double *)malloc(count * sizeof(double));
    buf[1] = Array_r;
  } else { buf[1] = NULL; }

  sprintf(label, "cgp_array_multi_read_data (%s)", tag);
  err = cgp_array_multi_read_data(fn, Avec, &min, &max, 2, buf);
  if (err != CG_OK) {
    if (comm_rank == 0) write_test_status(FAILED, label, NULL);
    cgp_error_exit();
  }
  {
    int ok = 1;
    for (k = 0; k < count && ok; k++) {
      if (should_verify(mode, 0) &&
          Array_i[k] != comm_rank * count + k + 1)
        ok = 0;
      if (should_verify(mode, 1) &&
          !compareValuesDouble(Array_r[k], comm_rank * count + k + 1.001))
        ok = 0;
    }
    if (comm_rank == 0)
      write_test_status(ok ? PASSED : FAILED, label, NULL);
    if (!ok) cgp_error_exit();
  }
  free(buf); free(Array_r); free(Array_i);

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
  run_multiset_test("cmultiset", NULL_MODE_NONE);

  strcpy(test_str, "Multi-dataset NULL Buffer Testing: mixed");
  if (comm_rank == 0) write_test_header(test_str, strlen(test_str));
  run_multiset_test("cnullbuf_mixed", NULL_MODE_MIXED);

  strcpy(test_str, "Multi-dataset NULL Buffer Testing: rank0-only");
  if (comm_rank == 0) write_test_header(test_str, strlen(test_str));
  run_multiset_test("cnull_r0", NULL_MODE_RANK0);

  strcpy(test_str, "Multi-dataset NULL Buffer Testing: partial");
  if (comm_rank == 0) write_test_header(test_str, strlen(test_str));
  run_multiset_test("cnull_partial", NULL_MODE_PARTIAL);

  MPI_Finalize();
  return 0;
}
