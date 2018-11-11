/*******************************************************************************
 * Test cg_array_general_read and cg_array_general_write.  This series of tests
 * focuses on memory/file spaces of different shapes and data conversions.
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#ifdef _WIN32
# include <io.h>
# define unlink _unlink
#else
# include <unistd.h>
#endif
#include "cgnslib.h"

/* From cgns_internal: so we can reset expected error messages */
void cgi_error(const char *format, ...);

const int CellDim = 3, PhyDim = 3;

#define dims_1d(idx) 64
#define dims_2d(idx) (((idx)==0)*4 + ((idx)==1)*16)
#define dims_3d(idx) 4

const cgsize_t size[3][3] = { {dims_3d(2)+1, dims_3d(1)+1, dims_3d(0)+1},
                              {dims_3d(2)  , dims_3d(1)  , dims_3d(0)  },
                              {0           , 0           , 0           } };

float *fvalues_1d, *fbuf_1d;
double *dvalues_1d, *dbuf_1d;
int* ibuf_1d;


inline static cgsize_t idx3d_1d(cgsize_t ii, cgsize_t jj, cgsize_t kk) {
    return ii + dims_3d(0)*(jj + dims_3d(1)*(kk));
}
inline static void idx1d_3d(cgsize_t nn,
                            cgsize_t *ii, cgsize_t *jj, cgsize_t *kk) {
    *kk =    nn/(dims_3d(0)*dims_3d(1));
    nn -= (*kk)*(dims_3d(0)*dims_3d(1));
    *jj =    nn/(dims_3d(0));
    nn -= (*jj)*(dims_3d(0));
    *ii =    nn;
}

inline static cgsize_t idx2d_1d(cgsize_t ii, cgsize_t jj) {
    return ii + dims_2d(0)*(jj);
}

static void compute_values(int i, int j, int k, int a)
{
    fvalues_1d[idx3d_1d(i, j, k)] = (float)(1 + (k+a+0)*1100 +
                                            idx3d_1d(i, j, 0));
    dvalues_1d[idx3d_1d(i, j, k)] = (double)(1 + (k+a+1)*1100 +
                                             idx3d_1d(i, j, 0));
}

int main (int argc, char *argv[])
{
    int cgfile, cgbase, cgzone, cgdiscr;
    int nn, np;

    int n, i, j, k, ii, jj, kk;
    cgsize_t dims[3];
    cgsize_t rmin[3];
    cgsize_t rmax[3];
    cgsize_t m_dims[3];
    cgsize_t m_rmin[3];
    cgsize_t m_rmax[3];

    if (argc > 1) {
        n = 0;
        if (argv[1][n] == '-') n++;
        if (argv[1][n] == 'a' || argv[1][n] == 'A')
            nn = CG_FILE_ADF;
        else if (argv[1][n] == 'h' || argv[1][n] == 'H')
            nn = CG_FILE_HDF5;
        else {
            fprintf(stderr, "unknown option\n");
            exit (1);
        }
        if (cg_set_file_type(nn))
            cg_error_exit();
    }

    fvalues_1d = (float *) malloc((size_t)(2 * dims_1d(0) * sizeof(float)));
    if (NULL == fvalues_1d) {
        fprintf(stderr, "malloc failed for float data\n");
        exit(1);
    }
    fbuf_1d    = fvalues_1d + dims_1d(0);
    dvalues_1d = (double *) malloc((size_t)(2 * dims_1d(0) * sizeof(double)));
    if (NULL == dvalues_1d) {
        fprintf(stderr, "malloc failed for double data\n");
        exit(1);
    }
    dbuf_1d    = dvalues_1d + dims_1d(0);\
    ibuf_1d    = (int *) malloc((size_t)(1 * dims_1d(0) * sizeof(int)));
    if (NULL == ibuf_1d) {
        fprintf(stderr, "malloc failed for int data\n");
        exit(1);
    }

    // 2d reshape b
    float (*fvalues_2d)[dims_2d(0)]  = (float (*)[dims_2d(0)])fvalues_1d;
    double (*dvalues_2d)[dims_2d(0)] = (double (*)[dims_2d(0)])dvalues_1d;

    // 3d reshape
    float (*fvalues_3d)[dims_3d(1)][dims_3d(0)] =
      (float (*)[dims_3d(1)][dims_3d(0)])fvalues_1d;
    double (*dvalues_3d)[dims_3d(1)][dims_3d(0)] =
      (double (*)[dims_3d(1)][dims_3d(0)])dvalues_1d;
    double (*dbuf_3d)[dims_3d(1)][dims_3d(0)] =
      (double (*)[dims_3d(1)][dims_3d(0)])dbuf_1d;

    /* Initialize data and sanity checks */
    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                compute_values(i, j, k, 0);
                assert(fvalues_3d[k][j][i] = fvalues_1d[idx3d_1d(i,j,k)]);
                assert(dvalues_3d[k][j][i] = dvalues_1d[idx3d_1d(i,j,k)]);
                n = idx3d_1d(i, j, k);
                idx1d_3d(n, &ii, &jj, &kk);
                assert(i == ii && j == jj && k == kk);
            }
        }
    }
    for (j = 0; j < dims_2d(1); j++)
      for (i = 0; i < dims_2d(0); i++) {
          assert(fvalues_2d[j][i] = fvalues_1d[idx2d_1d(i,j)]);
          assert(dvalues_2d[j][i] = dvalues_1d[idx2d_1d(i,j)]);
      }

    /* open */
    unlink("reshape.cgns");
    if (cg_open("reshape.cgns", CG_MODE_WRITE, &cgfile)) cg_error_exit();

    /*---- structured grid discrete values ----*/

    printf ("writing structured base and discrete\n");
    fflush (stdout);

    /* write base and zone */
    if (cg_base_write(cgfile, "Structured", CellDim, PhyDim, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", (cgsize_t*)size, Structured,
                      &cgzone))
        cg_error_exit();

    if (cg_discrete_write(cgfile, cgbase, cgzone, "Flux", &cgdiscr))
        cg_error_exit();

    /* use cg_array_general_write to write the arrays */
    for (n=0; n<3; n++) {
        dims[n]   = dims_3d(n);
        rmin[n]   = 1;
        rmax[n]   = dims_3d(n);
        m_dims[n] = dims_3d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_3d(n);
    }

    if (cg_goto(cgfile, cgbase, "Zone_t", cgzone, "DiscreteData_t", cgdiscr,
                "end") ||
        cg_gridlocation_write(CellCenter))
        cg_error_exit();
    if (cg_array_general_write("FValues", RealSingle,
                               3,   dims,   rmin,   rmax,
                               3, m_dims, m_rmin, m_rmax, fvalues_1d))
        cg_error_exit();
    if (cg_array_general_write("DValues", RealDouble,
                               3,   dims,   rmin,   rmax,
                               3, m_dims, m_rmin, m_rmax, dvalues_1d))
        cg_error_exit();

    /* close the file and reopen in modify mode */
    puts ("closing and reopening in modify mode");
    if (cg_close(cgfile))
        cg_error_exit();
    fflush (stdout);
    if (cg_open ("reshape.cgns", CG_MODE_MODIFY, &cgfile))
        cg_error_exit();
    fflush (stdout);
    cgbase = cgzone = cgdiscr = 1;

    puts("checking the data");

    nn = 0;

    /*--- Full check on data in arrays ---*/

    char name1[16], name2[16];
    DataType_t type1, type2;
    if (cg_goto(cgfile, cgbase, "Zone_t", cgzone, "DiscreteData_t", cgdiscr,
                "end") ||
        cg_narrays(&n) ||
        cg_array_info(1, name1, &type1, &i, rmin) ||
        cg_array_info(2, name2, &type2, &j, rmax))
        cg_error_exit();
    if (n != 2) ++nn;
    if (strcmp(name1, "FValues") != 0) ++nn;
    if (type1 != RealSingle) ++nn;
    if (i != 3) ++nn;
    for (n=0; n<3; n++) {
        if (rmin[n] != dims_3d(n)) ++nn;
    }
    if (strcmp(name2, "DValues") != 0) ++nn;
    if (type2 != RealDouble) ++nn;
    if (j != 3) ++nn;
    for (n=0; n<3; n++) {
        if (rmax[n] != dims_3d(n)) ++nn;
    }

    for (n=0; n<3; n++) {
        dims[n] = dims_3d(n);
        rmin[n] = 1;
        rmax[n] = dims_3d(n);
    }
    /* verify the written data */
    np = 0;
    if (cg_array_general_read(1, RealSingle, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != fvalues_1d[i]) ++np;
    }
    if (cg_array_general_read(2, RealDouble, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != dvalues_1d[i]) ++np;
    }
    nn += np;
    if (np) printf("%d differences in values (T1)\n", np);
    /* verify with type conversion */
    np = 0;
    if (cg_array_general_read(1, RealDouble, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != (double)fvalues_1d[i]) ++np;
    }
    if (cg_array_general_read(1, Integer, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, ibuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (ibuf_1d[i] != (int)fvalues_1d[i]) ++np;
    }
    nn += np;
    if (np) printf("%d differences in values (T2)\n", np);    

    /*--- check error for type mismatch on write ---*/

    printf("Next error is required: ");
    fflush (stdout);
    if (!cg_array_general_write("FValues", RealDouble,
                                3,   dims,   rmin,   rmax,
                                3, m_dims, m_rmin, m_rmax, dvalues_1d)) {
        
        printf("write with mismatched types failed to produce error (T3)\n");
        ++n;
    }
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */

    /*--- 2d mem full write/read ---*/

    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                compute_values(i, j, k, 1);
            }
        }
    }
    for (n=0; n<2; n++) {
        m_dims[n] = dims_2d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_2d(n);
    }
    if (cg_array_general_write("FValues", RealSingle,
                               3,   dims,   rmin,   rmax,
                               2, m_dims, m_rmin, m_rmax, fvalues_1d))
        cg_error_exit();
    if (cg_array_general_write("DValues", RealDouble,
                               3, dims,   rmin,   rmax,
                               2, m_dims, m_rmin, m_rmax, dvalues_1d))
        cg_error_exit();
    /* verify the written data */
    np = 0;
    if (cg_array_general_read(1, RealSingle, rmin, rmax,
                              2, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != fvalues_1d[i]) ++np;
    }
    /* read as double */
    if (cg_array_general_read(1, RealDouble, rmin, rmax,
                              2, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != (double)fvalues_1d[i]) ++np;
    }
    /* read the second in 3d */
    for (n=0; n<3; n++) {
        m_dims[n] = dims_3d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_3d(n);
    }
    if (cg_array_general_read(2, RealDouble, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != dvalues_1d[i]) ++np;
    }
    /* read as float */
    if (cg_array_general_read(2, RealSingle, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != (float)dvalues_1d[i]) ++np;
    }
    nn += np;
    if (np) printf("%d differences in values (T4)\n", np);

    /*--- 1d mem full write/read ---*/

    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                compute_values(i, j, k, 2);
            }
        }
    }
    for (n=0; n<1; n++) {
        m_dims[n] = dims_1d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_1d(n);
    }
    if (cg_array_general_write("FValues", RealSingle,
                               3,   dims,   rmin,   rmax,
                               1, m_dims, m_rmin, m_rmax, fvalues_1d))
        cg_error_exit();
    if (cg_array_general_write("DValues", RealDouble,
                               3, dims,   rmin,   rmax,
                               1, m_dims, m_rmin, m_rmax, dvalues_1d))
        cg_error_exit();
    /* verify the written data */
    np = 0;
    if (cg_array_general_read(1, RealSingle, rmin, rmax,
                              1, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != fvalues_1d[i]) ++np;
    }
    /* read as int */
    if (cg_array_general_read(1, Integer, rmin, rmax,
                              1, m_dims, m_rmin, m_rmax, ibuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (ibuf_1d[i] != (int)fvalues_1d[i]) ++np;
    }
    /* read the second in 3d */
    for (n=0; n<3; n++) {
        m_dims[n] = dims_3d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_3d(n);
    }
    if (cg_array_general_read(2, RealDouble, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != dvalues_1d[i]) ++np;
    }
    /* read as int */
    if (cg_array_general_read(2, Integer, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, ibuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (ibuf_1d[i] != (int)dvalues_1d[i]) ++np;
    }
    nn += np;
    if (np) printf("%d differences in values (T5)\n", np);

    /*--- 3d partial write/full and partial read ---*/

    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                fvalues_3d[k][j][i] = 0.f;
                dvalues_3d[k][j][i] = 0.;
                if (i > 0 && i < dims_3d(0)-1 &&
                    j > 0 && j < dims_3d(1)-1 &&
                    k > 0 && k < dims_3d(2)-1) {
                    compute_values(i, j, k, -9);
                }
            }
        }
    }
    /* partial write of negative data */
    for (n=0; n<3; n++) {
        dims[n] = dims_3d(n);
        rmin[n] = 1 + 1;
        rmax[n] = dims_3d(n) - 1;
        m_dims[n] = dims_3d(n);
        m_rmin[n] = 1 + 1;
        m_rmax[n] = dims_3d(n) - 1;
    }
    if (cg_array_general_write("FValues", RealSingle,
                               3,   dims,   rmin,   rmax,
                               3, m_dims, m_rmin, m_rmax, fvalues_1d))
        cg_error_exit();
    if (cg_array_general_write("DValues", RealDouble,
                               3,   dims,   rmin,   rmax,
                               3, m_dims, m_rmin, m_rmax, dvalues_1d))
        cg_error_exit();
    /* verify the written data using a partial read */
    np = 0;
    for (n = 0; n < dims_1d(0); n++) {
        fbuf_1d[n] = 0.f;
        dbuf_1d[n] = 0.;
        ibuf_1d[n] = 0;
    }
    if (cg_array_general_read(1, RealDouble, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                if (i > 0 && i < dims_3d(0)-1 &&
                    j > 0 && j < dims_3d(1)-1 &&
                    k > 0 && k < dims_3d(2)-1) {
                    if (dbuf_3d[k][j][i] != (double)fvalues_3d[k][j][i]) ++np;
                }
            }
        }
    }
    /* read the full range */
    for (n=0; n<3; n++) {
        dims[n] = dims_3d(n);
        rmin[n] = 1;
        rmax[n] = dims_3d(n);
        m_dims[n] = dims_3d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_3d(n);
    }
    if (cg_array_general_read(2, RealSingle, rmin, rmax,
                              3, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    /* expected values */
    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                compute_values(i, j, k, 2);
                if (i > 0 && i < dims_3d(0)-1 &&
                    j > 0 && j < dims_3d(1)-1 &&
                    k > 0 && k < dims_3d(2)-1) {
                    compute_values(i, j, k, -9);
                }
            }
        }
    }
    /* compare */
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != (float)dvalues_1d[i]) ++np;
    }
    nn += np;
    if (np) printf("%d differences in values (T6)\n", np);

    if (nn == 0) puts("no differences");

    puts ("closing file");
    cg_close (cgfile);
    free(fvalues_1d);
    free(dvalues_1d);
    free(ibuf_1d);
    if (nn != 0) exit(1);

    return 0;
}

