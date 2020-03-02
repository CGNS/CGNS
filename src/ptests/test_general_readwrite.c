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

#include "pcgnslib.h"
#include "mpi.h"

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
    MPI_Comm comm = MPI_COMM_WORLD;
    int comm_size, comm_rank;   

    int cgfile, cgbase, cgzone, cggrid, cgcoordx, cgcoordy, cgcoordz, cgsol,
      cgfld;
    int n, nn, np, global_nn, global_np;

    cgsize_t i, j, k;
    cgsize_t dims[3];
    cgsize_t rmin[3];
    cgsize_t rmax[3];
    cgsize_t m_rmin[3];
    cgsize_t m_rmax[3];

    MPI_Init(&argc,&argv);
    MPI_Comm_size(comm, &comm_size);
    MPI_Comm_rank(comm, &comm_rank);
    cgp_mpi_comm(comm);

    if (comm_size != 2 && comm_rank == 0) {
        printf("WARNING: you are supposed to run this test with two "
               "processes\n");
    }

    xcoord = (float *) malloc((size_t)(5 * num_coord * sizeof(float)));
    if (NULL == xcoord) {
        fprintf(stderr, "malloc failed for data\n");
        cgp_error_exit();
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

    if (comm_rank == 0) {
        unlink("general_readwrite.cgns");
    }
    MPI_Barrier(comm);
    if (cgp_open("general_readwrite.cgns", CG_MODE_WRITE, &cgfile))
        cgp_error_exit();

    /*---- structured grid with rind ----*/

    if (comm_rank == 0) {
        printf ("writing structured base with rind\n");
        fflush (stdout);
    }

    /* write base and zone */

    if (cg_base_write(cgfile, "Structured", CellDim, PhyDim, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", (cgsize_t*)size,
                      CGNS_ENUMV(Structured), &cgzone))
        cgp_error_exit();

    /* use cgp_coord_general_write to write coordinates with all rinds
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
    if (comm_size == 2) {  /* Split in k direction */
        if (comm_rank == 0) {
              rmax[2] = size[0][2]/2;
            m_rmax[2] = size[0][2]/2 + rind[2][0];
        }
        if (comm_rank == 1) {
              rmin[2] = size[0][2]/2 + 1;
            m_rmin[2] = size[0][2]/2 + 1 + rind[2][0];
        }
    }

    /* write coordinates with rind */

    if (cg_grid_write(cgfile, cgbase, cgzone, "GridCoordinates", &cggrid) ||
        cg_goto(cgfile, cgbase, "Zone_t", cgzone,
            "GridCoordinates_t", cggrid, "end") ||
        cg_rind_write((int*)rind))
        cgp_error_exit();
        
    if (cgp_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                        "CoordinateX", &cgcoordx) ||
        cgp_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                        "CoordinateY", &cgcoordy) ||
        cgp_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                        "CoordinateZ", &cgcoordz))
        cgp_error_exit();

    if (cgp_coord_general_write_data(cgfile, cgbase, cgzone, cgcoordx,
                                     rmin, rmax, CGNS_ENUMV(RealSingle),
                                     3, dims, m_rmin, m_rmax, xcoord) ||
        cgp_coord_general_write_data(cgfile, cgbase, cgzone, cgcoordy,
                                     rmin, rmax, CGNS_ENUMV(RealSingle),
                                     3, dims, m_rmin, m_rmax, ycoord) ||
        cgp_coord_general_write_data(cgfile, cgbase, cgzone, cgcoordz,
                                     rmin, rmax, CGNS_ENUMV(RealSingle),
                                     3, dims, m_rmin, m_rmax, zcoord))
        cgp_error_exit();

    /* write solution with rind, and the solution dimensions come from the zone
     * sizes */

    if (cg_sol_write(cgfile, cgbase, cgzone, "VertexSolution", CGNS_ENUMV(Vertex),
                     &cgsol) ||
        cg_goto(cgfile, cgbase, "Zone_t", cgzone,
            "FlowSolution_t", cgsol, "end") ||
        cg_rind_write((int*)rind))
        cgp_error_exit();

    if (cgp_field_write(cgfile, cgbase, cgzone, cgsol,
                        CGNS_ENUMV(RealSingle), "Density", &cgfld))
        cgp_error_exit();

    if (cgp_field_general_write_data(cgfile, cgbase, cgzone, cgsol, cgfld,
                                     rmin, rmax, CGNS_ENUMV(RealSingle),
                                     3, dims, m_rmin, m_rmax, solution))
        cgp_error_exit();

    /* close the file and reopen in read mode */

    MPI_Barrier(comm);
    if (comm_rank == 0) {
        puts ("closing and reopening in read mode");
    }
    cgp_close(cgfile);

    /* read file and check the data */

    if (cgp_open ("general_readwrite.cgns", CG_MODE_READ, &cgfile))
        cgp_error_exit ();
    cgbase = cgzone = cggrid = cgsol = 1;

    if (comm_rank == 0) {
        puts("checking the data");
    }

    nn = 0;

    /* check coordinates */
    /* Only load core coordinates without rind but inside memory with rind */
    /* If comm == 2, reverse the partitions read */
    for (n=0; n<3; n++) {
        rmin[n]   = get_s_rmin(n, 0);
        rmax[n]   = get_s_rmax(n, 0);
        m_rmin[n] = get_m_rmin(n, 0);
        m_rmax[n] = get_m_rmax(n, 0);
    }
    int kmin = idxmin(2,0);
    int kmax = idxmax(2,0);
    if (comm_size == 2) {  /* Split in k direction */
        if (comm_rank == 1) {
              rmax[2] = size[0][2]/2;
            m_rmax[2] = size[0][2]/2 + rind[2][0];
              kmax    = m_rmax[2] - 1;
        }
        if (comm_rank == 0) {
              rmin[2] = size[0][2]/2 + 1;
            m_rmin[2] = size[0][2]/2 + 1 + rind[2][0];
              kmin    = m_rmin[2] - 1;
        }
    }

    /* X */
    if (cgp_coord_general_read_data(cgfile, cgbase, cgzone, cgcoordx,
                                    rmin, rmax, CGNS_ENUMV(RealSingle),
                                    3, dims, m_rmin, m_rmax, fbuf))
        cgp_error_exit();
    np = 0;
    for (k = kmin; k <= kmax; k++) {
        for (j = idxmin(1,0); j <= idxmax(1,0); j++) {
            for (i = idxmin(0,0); i <= idxmax(0,0); i++) {
                if (fbuf[INDEX(i,j,k)] != xcoord[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    global_np = 0;
    MPI_Reduce(&np, &global_np, 1, MPI_INT, MPI_SUM, 0, comm);
    if (comm_rank == 0) {
        if (global_np) printf("%d differences in CoordinateX\n", global_np);
    }

    /* Y */
    if (cgp_coord_general_read_data(cgfile, cgbase, cgzone, cgcoordy,
                                    rmin, rmax, CGNS_ENUMV(RealSingle),
                                    3, dims, m_rmin, m_rmax, fbuf))
        cgp_error_exit();
    np = 0;
    for (k = kmin; k <= kmax; k++) {
        for (j = idxmin(1,0); j <= idxmax(1,0); j++) {
            for (i = idxmin(0,0); i <= idxmax(0,0); i++) {
                if (fbuf[INDEX(i,j,k)] != ycoord[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    global_np = 0;
    MPI_Reduce(&np, &global_np, 1, MPI_INT, MPI_SUM, 0, comm);
    if (comm_rank == 0) {
        if (global_np) printf("%d differences in CoordinateY\n", global_np);
    }

    /* Z */
    if (cgp_coord_general_read_data(cgfile, cgbase, cgzone, cgcoordz,
                                    rmin, rmax, CGNS_ENUMV(RealSingle),
                                    3, dims, m_rmin, m_rmax, fbuf))
        cgp_error_exit();
    np = 0;
    for (k = kmin; k <= kmax; k++) {
        for (j = idxmin(1,0); j <= idxmax(1,0); j++) {
            for (i = idxmin(0,0); i <= idxmax(0,0); i++) {
                if (fbuf[INDEX(i,j,k)] != zcoord[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    global_np = 0;
    MPI_Reduce(&np, &global_np, 1, MPI_INT, MPI_SUM, 0, comm);
    if (comm_rank == 0) {
        if (global_np) printf("%d differences in CoordinateZ\n", global_np);
    }

    /* check field with one layer of rind */

    for (n=0; n<3; n++) {
        rmin[n]   = get_s_rmin(n, 1);
        rmax[n]   = get_s_rmax(n, 1);
        m_rmin[n] = get_m_rmin(n, 1);
        m_rmax[n] = get_m_rmax(n, 1);
    }
    kmin = idxmin(2,0);
    kmax = idxmax(2,0);
    if (comm_size == 2) {  /* Split in k direction */
        if (comm_rank == 1) {
              rmax[2] = size[0][2]/2;
            m_rmax[2] = size[0][2]/2 + rind[2][0];
              kmax    = m_rmax[2] - 1;
        }
        if (comm_rank == 0) {
              rmin[2] = size[0][2]/2 + 1;
            m_rmin[2] = size[0][2]/2 + 1 + rind[2][0];
              kmin    = m_rmin[2] - 1;
        }
    }

    if (cgp_field_general_read_data(cgfile, cgbase, cgzone, cgsol, cgfld,
                                    rmin, rmax, CGNS_ENUMV(RealSingle),
                                    3, dims, m_rmin, m_rmax, fbuf))
        cgp_error_exit();
    np = 0;
    for (k = kmin; k <= kmax; k++) {
        for (j = idxmin(1,0); j <= idxmax(1,0); j++) {
            for (i = idxmin(0,0); i <= idxmax(0,0); i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    nn += np;
    global_np = 0;
    MPI_Reduce(&np, &global_np, 1, MPI_INT, MPI_SUM, 0, comm);
    if (comm_rank == 0) {
        if (global_np) printf("%d differences in Field\n", global_np);
    }

    global_nn = 0; 
    MPI_Allreduce(&nn, &global_nn, 1, MPI_INT, MPI_MAX, comm);
    if (comm_rank == 0) {
        if (global_nn == 0) puts("no differences");
    }
    free(xcoord);

    MPI_Finalize();
    /* cannot just return (global_nn) because exit code is limited to 1byte */
    return(global_nn!=0);
}
