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

/* from cgns_internal: so we can reset expected error messages */
void cgi_error(const char *format, ...);

const int CellDim = 3, PhyDim = 3;

const cgsize_t size[3][3] = { {5, 5, 5},  {4, 4, 4},  {0, 0, 0} };
const int      rind[3][2] = { {2, 2},  {2, 2},  {1, 1}};

#define NUM_I (size[0][0] + rind[0][0] + rind[0][1])
#define NUM_J (size[0][1] + rind[1][0] + rind[1][1])
#define NUM_K (size[0][2] + rind[2][0] + rind[2][1])
#define num_coord (NUM_I * NUM_J * NUM_K)

float *xcoord, *ycoord, *zcoord;
float *solution, *fbuf;

inline static cgsize_t INDEX(cgsize_t ii, cgsize_t jj, cgsize_t kk) {
    return ii + NUM_I*(jj + NUM_J*(kk));
}

/* ranges for arrays sent to CGNS */
/* s_ is the range in file space.  Core cells start at 1 so rind planes are
 * <= 0. */
inline static cgsize_t get_s_rmin(const int n, int nr) {
    if (nr < 0) nr = rind[n][0];
    assert(nr <= rind[n][0]);
    return 1 - nr;
}

inline static cgsize_t get_s_rmax(const int n, int nr) {
    if (nr < 0) nr = rind[n][1];
    assert(nr <= rind[n][1]);
    return size[0][n] + nr;
}

/* m_ is the range in memory.  The lowest index in each dimension is 1.  If
 * there are rind planes, then he core cells start at a value > 1. */
inline static cgsize_t get_m_rmin(const int n, int nr) {
    if (nr < 0) nr = rind[n][0];
    assert(nr <= rind[n][0]);
    return 1 + rind[n][0] - nr;
}

inline static cgsize_t get_m_rmax(const int n, int nr) {
    if (nr < 0) nr = rind[n][1];
    assert(nr <= rind[n][1]);
    return rind[n][0] + size[0][n] + nr;
}

/* ranges for accessing arrays */
inline static cgsize_t idxmin(const int n, const int nr) {
    return get_m_rmin(n, nr) - 1;
}

inline static cgsize_t idxmax(const int n, const int nr) {
    return get_m_rmax(n, nr) - 1;
}

/* initial data */
static void compute_coord(cgsize_t i, cgsize_t j, cgsize_t k)
{
    xcoord[INDEX(i, j, k)] = (float)(i - rind[0][0]);
    ycoord[INDEX(i, j, k)] = (float)(j - rind[1][0]);
    zcoord[INDEX(i, j, k)] = (float)(k - rind[2][0]);
}

static void compute_sol(cgsize_t i, cgsize_t j, cgsize_t k)
{
    int sign = 1 - 2*((i < rind[0][0]) + (i >= size[0][0] + rind[0][0]) +
                      (j < rind[1][0]) + (j >= size[0][1] + rind[1][0]) +
                      (k < rind[2][0]) + (k >= size[0][2] + rind[2][0]) > 0);
    solution[INDEX(i, j, k)] = (float)(sign*(1 + (k+1)*1100 + INDEX(i, j, 0)));
}

int main (int argc, char *argv[])
{
    int cgfile, cgbase, cgzone, cggrid, cgcoord, cgsol, cgfld;
    int n, nn, np;

    cgsize_t i, j, k;
    cgsize_t dims[3];
    cgsize_t rmin[3];
    cgsize_t rmax[3];
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

    xcoord = (float *) malloc((size_t)(5 * num_coord * sizeof(float)));
    if (NULL == xcoord) {
        fprintf(stderr, "malloc failed for data\n");
        exit(1);
    }
    ycoord = xcoord + num_coord;
    zcoord = ycoord + num_coord;
    solution = zcoord + num_coord;
    fbuf = solution + num_coord;

    for (k = 0; k < NUM_K; k++) {
        for (j = 0; j < NUM_J; j++) {
            for (i = 0; i < NUM_I; i++) {
                compute_coord(i, j, k);
                compute_sol(i, j, k);
            }
        }
    }

    /* open */

    unlink("rind.cgns");
    if (cg_open("rind.cgns", CG_MODE_WRITE, &cgfile))
        cg_error_exit();

    /*---- structured grid with rind ----*/

    printf ("writing structured base with rind\n");
    fflush (stdout);

    /* write base and zone */

    if (cg_base_write(cgfile, "Structured", CellDim, PhyDim, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", (cgsize_t*)size, CGNS_ENUMV(Structured),
                      &cgzone))
        cg_error_exit();

    /* use cg_coord_general_write to write coordinates with all rinds
       need to use cg_grid_write to create the node, cg_goto to set
       position at the node, then write rind */

    dims[0] = NUM_I;
    dims[1] = NUM_J;
    dims[2] = NUM_K;

    for (n=0; n<3; n++) {
        rmin[n]   = get_s_rmin(n, rind[n][0]);
        rmax[n]   = get_s_rmax(n, rind[n][1]);
        m_rmin[n] = get_m_rmin(n, rind[n][0]);
        m_rmax[n] = get_m_rmax(n, rind[n][1]);
    }

    /* write coordinates with rind */

    if (cg_grid_write(cgfile, cgbase, cgzone, "GridCoordinates", &cggrid) ||
        cg_goto(cgfile, cgbase, "Zone_t", cgzone,
            "GridCoordinates_t", cggrid, "end") ||
        cg_rind_write((int*)rind) ||
	cg_coord_general_write(cgfile, cgbase, cgzone, "CoordinateX",
                               CGNS_ENUMV(RealSingle), rmin, rmax,
                               CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                               xcoord, &cgcoord) ||
	cg_coord_general_write(cgfile, cgbase, cgzone, "CoordinateY",
                               CGNS_ENUMV(RealSingle), rmin, rmax,
                               CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                               ycoord, &cgcoord) ||
	cg_coord_general_write(cgfile, cgbase, cgzone, "CoordinateZ",
                               CGNS_ENUMV(RealSingle), rmin, rmax,
                               CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                               zcoord, &cgcoord))
        cg_error_exit();

    /* write solution with rind, and the solution dimensions come from the zone
     * sizes */

    if (cg_sol_write(cgfile, cgbase, cgzone, "VertexSolution", CGNS_ENUMV(Vertex),
                     &cgsol) ||
        cg_goto(cgfile, cgbase, "Zone_t", cgzone,
            "FlowSolution_t", cgsol, "end") ||
        cg_rind_write((int*)rind) ||
	cg_field_general_write(cgfile, cgbase, cgzone, cgsol, "Density",
                               CGNS_ENUMV(RealSingle), rmin, rmax,
                               CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                               solution, &cgfld))
        cg_error_exit();

    /* close the file and reopen in read mode */

    puts ("closing and reopening in read mode");
    cg_close(cgfile);

    /* read file and check the data */

    if (cg_open ("rind.cgns", CG_MODE_READ, &cgfile))
        cg_error_exit ();
    cgbase = cgzone = cggrid = cgsol = 1;

    puts("checking the data");

    nn = 0;

    /* check coordinates */
    /* Only load core coordinates without rind but inside memory with rind */

    for (n=0; n<3; n++) {
        rmin[n]   = get_s_rmin(n, 0);
        rmax[n]   = get_s_rmax(n, 0);
        m_rmin[n] = get_m_rmin(n, 0);
        m_rmax[n] = get_m_rmax(n, 0);
    }

    /* X */
    if (cg_coord_general_read(cgfile, cgbase, cgzone, "CoordinateX",
                              rmin, rmax, CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                              fbuf))
        cg_error_exit();
    np = 0;
    for (k = idxmin(2,0); k <= idxmax(2,0); k++) {
        for (j = idxmin(1,0); j <= idxmax(1,0); j++) {
            for (i = idxmin(0,0); i <= idxmax(0,0); i++) {
                if (fbuf[INDEX(i,j,k)] != xcoord[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in CoordinateX\n", np);

    /* Y */
    if (cg_coord_general_read(cgfile, cgbase, cgzone, "CoordinateY",
                              rmin, rmax, CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                              fbuf))
        cg_error_exit();
    np = 0;
    for (k = idxmin(2,0); k <= idxmax(2,0); k++) {
        for (j = idxmin(1,0); j <= idxmax(1,0); j++) {
            for (i = idxmin(0,0); i <= idxmax(0,0); i++) {
                if (fbuf[INDEX(i,j,k)] != ycoord[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in CoordinateY\n", np);

    /* Z */
    if (cg_coord_general_read(cgfile, cgbase, cgzone, "CoordinateZ",
                              rmin, rmax, CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                              fbuf))
        cg_error_exit();
    np = 0;
    for (k = idxmin(2,0); k <= idxmax(2,0); k++) {
        for (j = idxmin(1,0); j <= idxmax(1,0); j++) {
            for (i = idxmin(0,0); i <= idxmax(0,0); i++) {
                if (fbuf[INDEX(i,j,k)] != zcoord[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in CoordinateZ\n", np);

    /* check field with one layer of rind */

    for (n=0; n<3; n++) {
        rmin[n]   = get_s_rmin(n, 1);
        rmax[n]   = get_s_rmax(n, 1);
        m_rmin[n] = get_m_rmin(n, 1);
        m_rmax[n] = get_m_rmax(n, 1);
    }

    if (cg_field_general_read(cgfile, cgbase, cgzone, cgsol, "Density",
                              rmin, rmax, CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                              fbuf))
        cg_error_exit();

    np = 0;
    for (k = idxmin(2,1); k <= idxmax(2,1); k++) {
        for (j = idxmin(1,1); j <= idxmax(1,1); j++) {
            for (i = idxmin(0,1); i <= idxmax(0,1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field\n", np);

    if (nn == 0) puts("no differences (part 1)");

/*============================================================================*/

/* We now go to modify mode and repeadtely test writing and reading of the
 * field */

    /* close the file and reopen in modify mode */

    puts ("closing and reopening in modify mode");
    cg_close(cgfile);
    if (cg_open ("rind.cgns", CG_MODE_MODIFY, &cgfile))
        cg_error_exit ();

    /* delete the node */
    if (cg_goto(cgfile, cgbase, "Zone_t", cgzone, "FlowSolution_t", cgsol,
                "end") ||
        cg_delete_node("Density"))
        cg_error_exit();

    /* write the field using high-level routine */
    if (cg_field_write(cgfile, cgbase, cgzone, cgsol, CGNS_ENUMV(RealSingle), "Density",
                       solution, &cgfld))
        cg_error_exit();

    /* verify the written data */
    for (n=0; n<3; n++) {
        rmin[n] = get_s_rmin(n, -1);
        rmax[n] = get_s_rmax(n, -1);
    }
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T1)\n", np);

    /* if given ranges span the full dimensions, the read should succeed no
     * matter what the range is (this behavior is for backwards compatibility and
     * is not documented) */
    for (n=0; n<3; n++) {
        rmin[n] = 1;
        rmax[n] = dims[n];
    }
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T2)\n", np);

    /* try again with really weird dimensions */
    for (n=0; n<3; n++) {
        rmin[n] = -100*n;
        rmax[n] = rmin[n] + dims[n] - 1;
    }
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T3)\n", np);

    /* however, if given ranges do not span the full dimensions, ranges are
     * checked */
    for (n=0; n<3; n++) {
        rmin[n] = 1;
        rmax[n] = dims[n] - 1;
    }
    printf("Next error is required: ");
    fflush (stdout);
    if (!cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                       CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) {
        printf("read failed to produce error (T4)\n");
        ++nn;
    }
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */

    /* test old behavior where first rind plane is index 1 */
    if (cg_configure(CG_CONFIG_RIND_INDEX, CG_CONFIG_RIND_ZERO))
        cg_error_exit();

    /* this is the proper range for old behavior */
    for (n=0; n<3; n++) {
        rmin[n] = 1;
        rmax[n] = dims[n];
    }
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T5)\n", np);

    /* reading full range should still work for any dimension */
    for (n=0; n<3; n++) {
        rmin[n] = -200*n;
        rmax[n] = rmin[n] + dims[n] - 1;
    }
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T6)\n", np);

    /* ranges are checked if they do not span the full dimensions.  Now,
     * rmin < 1 is a failure */
    for (n=0; n<3; n++) {
        rmin[n]   = get_s_rmin(n, 1);  /* 1 rind plane */
        rmax[n]   = get_s_rmax(n, 1);  /* 1 rind plane */
    }
    printf("Next error is required: ");
    fflush (stdout);
    if (!cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                       CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) {
        printf("read failed to produce error (T7)\n");
        ++nn;
    }
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */

    if (nn == 0) puts("no differences (part 2)");

/*============================================================================*/

    /* delete the node */
    if (cg_goto(cgfile, cgbase, "Zone_t", cgzone, "FlowSolution_t", cgsol,
                "end") ||
        cg_delete_node("Density"))
        cg_error_exit();

    /* ranges must always be correct when writing.  This is for new behavior
     * but the library is still configured for old behavior */
    for (n=0; n<3; n++) {
        rmin[n]   = get_s_rmin(n, -1);
        rmax[n]   = get_s_rmax(n, -1);
    }
    printf("Next error is required: ");
    fflush (stdout);
    if (!cg_field_partial_write(cgfile, cgbase, cgzone, cgsol,
                                CGNS_ENUMV(RealSingle), "Density", rmin, rmax,
                                solution, &cgfld))
      {
        printf("write failed to produce error (T8)\n");
        ++nn;
      }
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */

    /* With correct range */
    for (n=0; n<3; n++) {
        rmin[n]   = 1;
        rmax[n]   = dims[n];
    }
    if (cg_field_partial_write(cgfile, cgbase, cgzone, cgsol,
                               CGNS_ENUMV(RealSingle), "Density", rmin, rmax,
                               solution, &cgfld)) cg_error_exit();
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T9)\n", np);

    /* delete the node */
    if (cg_goto(cgfile, cgbase, "Zone_t", cgzone, "FlowSolution_t", cgsol,
                "end") ||
        cg_delete_node("Density"))
        cg_error_exit();

    /* test new behavior where first core cell is index 1 */
    if (cg_configure(CG_CONFIG_RIND_INDEX, CG_CONFIG_RIND_CORE))
        cg_error_exit();

    /* ranges must always be correct when writing.  This is for old behavior */
    for (n=0; n<3; n++) {
        rmin[n]   = 1;
        rmax[n]   = dims[n];
    }
    printf("Next error is required: ");
    fflush (stdout);
    if (!cg_field_partial_write(cgfile, cgbase, cgzone, cgsol,
                                CGNS_ENUMV(RealSingle), "Density", rmin, rmax,
                                solution, &cgfld))
      {
        printf("write failed to produce error (T10)\n");
        ++nn;
      }
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */

    /* With correct range */
    for (n=0; n<3; n++) {
        rmin[n]   = get_s_rmin(n, -1);
        rmax[n]   = get_s_rmax(n, -1);
    }
    if (cg_field_partial_write(cgfile, cgbase, cgzone, cgsol,
                               CGNS_ENUMV(RealSingle), "Density", rmin, rmax,
                               solution, &cgfld)) cg_error_exit();
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T11)\n", np);

    /* Let's do an actual partial write and only write that range
     * This writes 4 cells from the beginning of the solution array */
    rmin[0] = get_s_rmin(0, 1);
    rmax[0] = get_s_rmin(0, 1) + 1;
    rmin[1] = get_s_rmin(1, 0);
    rmax[1] = get_s_rmin(1, 0) + 1;
    rmin[2] = get_s_rmin(2, 0);
    rmax[2] = get_s_rmin(2, 0);
    if (cg_field_partial_write(cgfile, cgbase, cgzone, cgsol,
                               CGNS_ENUMV(RealSingle), "Density", rmin, rmax,
                               solution, &cgfld)) cg_error_exit();
    for (n = 0; n < 4; ++n) {
        fbuf[n] = 0.;
    }
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    for (n = 0; n < 4; ++n) {
        if (fbuf[n] != solution[n]) np++;
    }
    nn += np;
    if (np) printf("%d differences in Field (T12)\n", np);

    /* Let's repeat that but write from the location in solution that we are
     * actually modifying.  Requires general_write */
    /* First, clear solution in memory to make sure our write is truly
     * partial */
    for (n = 0; n < num_coord; ++n) {
        solution[n] = 0.;
    }
    solution[INDEX(idxmin(0, 1)  , idxmin(1, 0)  , idxmin(2, 0))] = 8888.5;
    solution[INDEX(idxmin(0, 1)+1, idxmin(1, 0)  , idxmin(2, 0))] = 8888.6;
    solution[INDEX(idxmin(0, 1)  , idxmin(1, 0)+1, idxmin(2, 0))] = 8888.7;
    solution[INDEX(idxmin(0, 1)+1, idxmin(1, 0)+1, idxmin(2, 0))] = 8888.8;
    m_rmin[0] = get_m_rmin(0, 1);
    m_rmax[0] = get_m_rmin(0, 1) + 1;
    m_rmin[1] = get_m_rmin(1, 0);
    m_rmax[1] = get_m_rmin(1, 0) + 1;
    m_rmin[2] = get_m_rmin(2, 0);
    m_rmax[2] = get_m_rmin(2, 0);
    if (cg_field_general_write(cgfile, cgbase, cgzone, cgsol, "Density",
                               CGNS_ENUMV(RealSingle), rmin, rmax,
                               CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                               solution, &cgfld))
        cg_error_exit();
    /* Reset the solution array */
    for (k = 0; k < NUM_K; k++) {
        for (j = 0; j < NUM_J; j++) {
            for (i = 0; i < NUM_I; i++) {
                compute_sol(i, j, k);
            }
        }
    }
    solution[INDEX(idxmin(0, 1)  , idxmin(1, 0)  , idxmin(2, 0))] = 8888.5;
    solution[INDEX(idxmin(0, 1)+1, idxmin(1, 0)  , idxmin(2, 0))] = 8888.6;
    solution[INDEX(idxmin(0, 1)  , idxmin(1, 0)+1, idxmin(2, 0))] = 8888.7;
    solution[INDEX(idxmin(0, 1)+1, idxmin(1, 0)+1, idxmin(2, 0))] = 8888.8;
    /* verify the written data */
    for (n=0; n<3; n++) {
        rmin[n] = get_s_rmin(n, -1);
        rmax[n] = get_s_rmax(n, -1);
    }
    if (cg_field_read(cgfile, cgbase, cgzone, cgsol, "Density",
                      CGNS_ENUMV(RealSingle), rmin, rmax, fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T13)\n", np);

    /* verify again by only reading the 4 locations */
    fbuf[INDEX(idxmin(0, 1)  , idxmin(1, 0)  , idxmin(2, 0))] = 0.;
    fbuf[INDEX(idxmin(0, 1)+1, idxmin(1, 0)  , idxmin(2, 0))] = 0.;
    fbuf[INDEX(idxmin(0, 1)  , idxmin(1, 0)+1, idxmin(2, 0))] = 0.;
    fbuf[INDEX(idxmin(0, 1)+1, idxmin(1, 0)+1, idxmin(2, 0))] = 0.;
    rmin[0] = get_s_rmin(0, 1);
    rmax[0] = get_s_rmin(0, 1) + 1;
    rmin[1] = get_s_rmin(1, 0);
    rmax[1] = get_s_rmin(1, 0) + 1;
    rmin[2] = get_s_rmin(2, 0);
    rmax[2] = get_s_rmin(2, 0);
    if (cg_field_general_read(cgfile, cgbase, cgzone, cgsol, "Density",
                              rmin, rmax, CGNS_ENUMV(RealSingle), 3, dims, m_rmin, m_rmax,
                              fbuf)) cg_error_exit();
    np = 0;
    for (k = idxmin(2,-1); k <= idxmax(2,-1); k++) {
        for (j = idxmin(1,-1); j <= idxmax(1,-1); j++) {
            for (i = idxmin(0,-1); i <= idxmax(0,-1); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    if (np) printf("%d differences in Field (T14)\n", np);

    if (nn == 0) puts("no differences (part 3)");

    puts ("closing file");
    cg_close (cgfile);
    free(xcoord);
    if (nn != 0) exit(1);

    return 0;
}
