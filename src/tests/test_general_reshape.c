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

static void compute_values(cgsize_t i, cgsize_t j, cgsize_t k, int a)
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
    int n, narr, rank1, rank2;

    cgsize_t i, j, k, ii, jj, kk;
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

    /* 2d reshape b */
    float (*fvalues_2d)[dims_2d(0)]  = (float (*)[dims_2d(0)])fvalues_1d;
    double (*dvalues_2d)[dims_2d(0)] = (double (*)[dims_2d(0)])dvalues_1d;

    /* 3d reshape */
    float (*fvalues_3d)[dims_3d(1)][dims_3d(0)] =
      (float (*)[dims_3d(1)][dims_3d(0)])fvalues_1d;
    float (*fbuf_3d)[dims_3d(1)][dims_3d(0)] =
      (float (*)[dims_3d(1)][dims_3d(0)])fbuf_1d;
    double (*dvalues_3d)[dims_3d(1)][dims_3d(0)] =
      (double (*)[dims_3d(1)][dims_3d(0)])dvalues_1d;
    double (*dbuf_3d)[dims_3d(1)][dims_3d(0)] =
      (double (*)[dims_3d(1)][dims_3d(0)])dbuf_1d;

    /* Initialize data and sanity checks */
    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                compute_values(i, j, k, 0);
                assert(fvalues_3d[k][j][i] == fvalues_1d[idx3d_1d(i,j,k)]);
                assert(dvalues_3d[k][j][i] == dvalues_1d[idx3d_1d(i,j,k)]);
                n = idx3d_1d(i, j, k);
                idx1d_3d(n, &ii, &jj, &kk);
                assert(i == ii && j == jj && k == kk);
            }
        }
    }
    for (j = 0; j < dims_2d(1); j++)
      for (i = 0; i < dims_2d(0); i++) {
          assert(fvalues_2d[j][i] == fvalues_1d[idx2d_1d(i,j)]);
          assert(dvalues_2d[j][i] == dvalues_1d[idx2d_1d(i,j)]);
      }

    /* open */
    unlink("reshape.cgns");
    if (cg_open("reshape.cgns", CG_MODE_WRITE, &cgfile)) cg_error_exit();

    /*---- structured grid discrete values ----*/

    printf ("writing structured base and discrete\n");
    fflush (stdout);

    /* write base and zone */
    if (cg_base_write(cgfile, "Structured", CellDim, PhyDim, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", (cgsize_t*)size, CGNS_ENUMV(Structured),
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
        cg_gridlocation_write(CGNS_ENUMV(CellCenter)))
        cg_error_exit();
    if (cg_array_general_write("FValues",
                               CGNS_ENUMV(RealSingle), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealSingle), 3, m_dims, m_rmin, m_rmax,
                               fvalues_1d))
        cg_error_exit();
    if (cg_array_general_write("DValues",
                               CGNS_ENUMV(RealDouble), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealDouble), 3, m_dims, m_rmin, m_rmax,
                               dvalues_1d))
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
    int isHDF5;
    if (cg_get_file_type(cgfile, &isHDF5)) cg_error_exit();
    isHDF5 = (isHDF5 == CG_FILE_HDF5);

    puts("checking the data");

    nn = 0;

    /*--- Full check on data in arrays ---*/

    char name1[16], name2[16];
    CGNS_ENUMT(DataType_t) type1, type2;
    if (cg_goto(cgfile, cgbase, "Zone_t", cgzone, "DiscreteData_t", cgdiscr,
                "end") ||
        cg_narrays(&narr) ||
        cg_array_info(1, name1, &type1, &rank1, rmin) ||
        cg_array_info(2, name2, &type2, &rank2, rmax))
        cg_error_exit();
    if (narr != 2) ++nn;
    if (strcmp(name1, "FValues") != 0) ++nn;
    if (type1 != CGNS_ENUMV(RealSingle)) ++nn;
    if (rank1 != 3) ++nn;
    for (n=0; n<3; n++) {
        if (rmin[n] != dims_3d(n)) ++nn;
    }
    if (strcmp(name2, "DValues") != 0) ++nn;
    if (type2 != CGNS_ENUMV(RealDouble)) ++nn;
    if (rank2 != 3) ++nn;
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
    if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealSingle),
                              3, m_dims, m_rmin, m_rmax, fbuf_1d))
        cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != fvalues_1d[i]) ++np;
    }
    if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(RealDouble),
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
        cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != dvalues_1d[i]) ++np;
    }
    nn += np;
    if (np) printf("%d differences in values (T1)\n", np);
    /* verify with type conversion */
    np = 0;
    if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealDouble),
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
        cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != (double)fvalues_1d[i]) ++np;
    }
    if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(Integer),
                              3, m_dims, m_rmin, m_rmax, ibuf_1d))
        cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (ibuf_1d[i] != (int)fvalues_1d[i]) ++np;
    }
    nn += np;
    if (np) printf("%d differences in values (T2)\n", np);

    /*--- 3d mem partial write/read with and without conversion ---*/

    /* only supported in hdf5 */
    if (isHDF5) {
        for (k = 0; k < dims_3d(2); k++) {
            for (j = 0; j < dims_3d(1); j++) {
                for (i = 0; i < dims_3d(0); i++) {
                    compute_values(i, j, k, 1);
                }
            }
        }
        for (n=0; n<3; n++) {
            dims[n]   = dims_3d(n);
            rmin[n]   = 2;
            rmax[n]   = dims_3d(n);
            m_dims[n] = dims_3d(n);
            m_rmin[n] = 2;
            m_rmax[n] = dims_3d(n);
        }
        if (cg_array_general_write("FValues",
                                   CGNS_ENUMV(RealSingle), 3,   dims,   rmin,   rmax,
                                   CGNS_ENUMV(RealSingle), 3, m_dims, m_rmin, m_rmax,
                                   fvalues_1d))
            cg_error_exit();
        if (cg_array_general_write("DValues",
                                   CGNS_ENUMV(RealDouble), 3,   dims,   rmin,   rmax,
                                   CGNS_ENUMV(RealDouble), 3, m_dims, m_rmin, m_rmax,
                                   dvalues_1d))
            cg_error_exit();
        /* read with conversion */
        if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealDouble),
                                  3, m_dims, m_rmin, m_rmax, dbuf_1d))
            cg_error_exit();
        for (k = 1; k < dims_3d(2); k++) {
            for (j = 1; j < dims_3d(1); j++) {
                for (i = 1; i < dims_3d(0); i++) {
                    if (dbuf_3d[k][j][i] != (double)fvalues_3d[k][j][i]) ++np;
                }
            }
        }
        if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(RealSingle),
                                  3, m_dims, m_rmin, m_rmax, fbuf_1d))
            cg_error_exit();
        for (k = 1; k < dims_3d(2); k++) {
            for (j = 1; j < dims_3d(1); j++) {
                for (i = 1; i < dims_3d(0); i++) {
                    if (fbuf_3d[k][j][i] != (float)dvalues_3d[k][j][i]) ++np;
                }
            }
        }
        /* write with conversion */
        for (i = 0; i < dims_1d(0); i++) {
            dbuf_1d[i] = fvalues_1d[i];
        }
        if (cg_array_general_write("FValues",
                                   CGNS_ENUMV(RealSingle), 3,   dims,   rmin,   rmax,
                                   CGNS_ENUMV(RealDouble), 3, m_dims, m_rmin, m_rmax,
                                   dbuf_1d))
            cg_error_exit();
        for (i = 0; i < dims_1d(0); i++) {
            fbuf_1d[i] = dvalues_1d[i];
        }
        if (cg_array_general_write("DValues",
                                   CGNS_ENUMV(RealDouble), 3,   dims,   rmin,   rmax,
                                   CGNS_ENUMV(RealSingle), 3, m_dims, m_rmin, m_rmax,
                                   fbuf_1d))
            cg_error_exit();
        if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealSingle),
                                  3, m_dims, m_rmin, m_rmax, fbuf_1d))
            cg_error_exit();
        for (k = 1; k < dims_3d(2); k++) {
            for (j = 1; j < dims_3d(1); j++) {
                for (i = 1; i < dims_3d(0); i++) {
                    if (fbuf_3d[k][j][i] != fvalues_3d[k][j][i]) ++np;
                }
            }
        }
        if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(RealDouble),
                                  3, m_dims, m_rmin, m_rmax, dbuf_1d))
            cg_error_exit();
        for (k = 1; k < dims_3d(2); k++) {
            for (j = 1; j < dims_3d(1); j++) {
                for (i = 1; i < dims_3d(0); i++) {
                    if (dbuf_3d[k][j][i] != dvalues_3d[k][j][i]) ++np;
                }
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in values (T3)\n", np);

    /*--- 2d mem full write/read ---*/

    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                compute_values(i, j, k, 2);
            }
        }
    }
    for (n=0; n<3; n++) {
        dims[n] = dims_3d(n);
        rmin[n] = 1;
        rmax[n] = dims_3d(n);
    }
    for (n=0; n<2; n++) {
        m_dims[n] = dims_2d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_2d(n);
    }
    /* write to float location with double */
    for (i = 0; i < dims_1d(0); i++) {
        dbuf_1d[i] = fvalues_1d[i];
    }
    if (cg_array_general_write("FValues",
                               CGNS_ENUMV(RealSingle), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealDouble), 2, m_dims, m_rmin, m_rmax,
                               dbuf_1d))
        cg_error_exit();
    /* write to double location with integer */
    for (i = 0; i < dims_1d(0); i++) {
        ibuf_1d[i] = dvalues_1d[i];
    }
    if (cg_array_general_write("DValues",
                               CGNS_ENUMV(RealDouble), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(Integer), 2, m_dims, m_rmin, m_rmax,
                               ibuf_1d))
        cg_error_exit();
    /* verify the written data */
    np = 0;
    if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealSingle),
                              2, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != fvalues_1d[i]) ++np;
    }
    /* read as double */
    if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealDouble),
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
    if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(RealDouble),
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != dvalues_1d[i]) ++np;
    }
    /* read as float */
    if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(RealSingle),
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
                compute_values(i, j, k, 3);
            }
        }
    }
    for (n=0; n<1; n++) {
        m_dims[n] = dims_1d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_1d(n);
    }
    if (cg_array_general_write("FValues",
                               CGNS_ENUMV(RealSingle), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealSingle), 1, m_dims, m_rmin, m_rmax,
                               fvalues_1d))
        cg_error_exit();
    if (cg_array_general_write("DValues",
                               CGNS_ENUMV(RealDouble), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealDouble), 1, m_dims, m_rmin, m_rmax,
                               dvalues_1d))
        cg_error_exit();
    /* verify the written data */
    np = 0;
    if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealSingle),
                              1, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != fvalues_1d[i]) ++np;
    }
    /* read as int */
    if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(Integer),
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
    if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(RealDouble),
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != dvalues_1d[i]) ++np;
    }
    /* read as int */
    if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(Integer),
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
    if (cg_array_general_write("FValues",
                               CGNS_ENUMV(RealSingle), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealSingle), 3, m_dims, m_rmin, m_rmax,
                               fvalues_1d))
        cg_error_exit();
    if (cg_array_general_write("DValues",
                               CGNS_ENUMV(RealDouble), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealDouble), 3, m_dims, m_rmin, m_rmax,
                               dvalues_1d))
        cg_error_exit();
    /* verify the written data using a partial read (only supported with hdf5
       files */
    np = 0;
    if (isHDF5) {
        for (n = 0; n < dims_1d(0); n++) {
            fbuf_1d[n] = 0.f;
            dbuf_1d[n] = 0.;
            ibuf_1d[n] = 0;
        }
        if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealDouble),
                                  3, m_dims, m_rmin, m_rmax, dbuf_1d))
            cg_error_exit();
        for (k = 0; k < dims_3d(2); k++) {
            for (j = 0; j < dims_3d(1); j++) {
                for (i = 0; i < dims_3d(0); i++) {
                    if (i > 0 && i < dims_3d(0)-1 &&
                        j > 0 && j < dims_3d(1)-1 &&
                        k > 0 && k < dims_3d(2)-1) {
                        if (dbuf_3d[k][j][i] != (double)fvalues_3d[k][j][i])
                            ++np;
                    }
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
    if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(RealSingle),
                              3, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    /* expected values */
    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                compute_values(i, j, k, 3);
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

    /*--- 1d mem full write/read with rind planes ---*/

    if (cg_delete_node("FValues")) cg_error_exit();
    if (cg_delete_node("DValues")) cg_error_exit();
    int rind[6] = { 1, 1, 1, 1, 1, 1 };
    if (cg_rind_write(rind)) cg_error_exit();

    for (k = 0; k < dims_3d(2); k++) {
        for (j = 0; j < dims_3d(1); j++) {
            for (i = 0; i < dims_3d(0); i++) {
                compute_values(i, j, k, 4);
            }
        }
    }
    for (n=0; n<3; n++) {
        dims[n] = dims_3d(n);
        rmin[n] = 0;
        rmax[n] = dims_3d(n)-1;
    }
    m_dims[0] = dims_1d(n);
    m_rmin[0] = 1;
    m_rmax[0] = dims_1d(n);
    if (cg_array_general_write("FValues",
                               CGNS_ENUMV(RealSingle), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealSingle), 1, m_dims, m_rmin, m_rmax,
                               fvalues_1d))
        cg_error_exit();
    if (cg_array_general_write("DValues",
                               CGNS_ENUMV(RealDouble), 3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealDouble), 1, m_dims, m_rmin, m_rmax,
                               dvalues_1d))
        cg_error_exit();
    /* verify the written data */
    np = 0;
    if (cg_array_general_read(1, rmin, rmax, CGNS_ENUMV(RealSingle),
                              1, m_dims, m_rmin, m_rmax, fbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (fbuf_1d[i] != fvalues_1d[i]) ++np;
    }
    /* read the second in 3d */
    for (n=0; n<3; n++) {
        m_dims[n] = dims_3d(n);
        m_rmin[n] = 1;
        m_rmax[n] = dims_3d(n);
    }
    if (cg_array_general_read(2, rmin, rmax, CGNS_ENUMV(RealDouble),
                              3, m_dims, m_rmin, m_rmax, dbuf_1d))
      cg_error_exit();
    for (i = 0; i < dims_1d(0); i++) {
        if (dbuf_1d[i] != dvalues_1d[i]) ++np;
    }
    nn += np;
    if (np) printf("%d differences in values (T7)\n", np);

    if (nn == 0) puts("no differences");

    puts ("closing file");
    cg_close (cgfile);
    free(fvalues_1d);
    free(dvalues_1d);
    free(ibuf_1d);
    if (nn != 0) exit(1);

    return 0;
}

