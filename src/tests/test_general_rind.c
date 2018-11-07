#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef _WIN32
# include <io.h>
# define unlink _unlink
#else
# include <unistd.h>
#endif
#include "cgnslib.h"

int CellDim = 3, PhyDim = 3;
int cgfile, cgbase, cgzone, cggrid, cgsol, cgfld, cgcoor;

cgsize_t size[9] = {5, 5, 5,  4, 4, 4,  0, 0, 0};
int rind[6] = {2, 2,  2, 2,  1, 1};

#define NUM_I (size[0] + rind[0] + rind[1])
#define NUM_J (size[1] + rind[2] + rind[3])
#define NUM_K (size[2] + rind[4] + rind[5])

#define INDEX(I,J,K) (int)((I) + NUM_I * ((J) + NUM_J * (K)))

cgsize_t num_coord;
float *xcoord, *ycoord, *zcoord;
float *solution, *fbuf;

static void compute_coord (int n, int i, int j, int k)
{
    xcoord[n] = (float)(i - rind[0]);
    ycoord[n] = (float)(j - rind[2]);
    zcoord[n] = (float)(k - rind[4]);
}

static void compute_sol (int i, int j, int k)
{
    int sign = 1 - 2*((i < rind[0]) + (i >= size[0] + rind[0]) +
                      (j < rind[2]) + (j >= size[1] + rind[2]) +
                      (k < rind[4]) + (k >= size[2] + rind[4]) > 0);
    solution[INDEX(i, j, k)] = (float)(sign*(1 + (k+1)*1100 + INDEX(i, j, 0)));
}

int main (int argc, char *argv[])
{
    int n, nn, i, j, k, np;
    cgsize_t dims[3];
    cgsize_t m_dims[3];
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

    num_coord = NUM_I * NUM_J * NUM_K;
    xcoord = (float *) malloc((size_t)(5 * num_coord * sizeof(float)));
    if (NULL == xcoord) {
        fprintf(stderr, "malloc failed for data\n");
        exit(1);
    }
    ycoord = xcoord + num_coord;
    zcoord = ycoord + num_coord;
    solution = zcoord + num_coord;
    fbuf = solution + num_coord;

    unlink("rind.cgns");
    if (cg_open("rind.cgns", CG_MODE_WRITE, &cgfile))
        cg_error_exit();

    /*--- structured grid with rind ---*/

    printf ("writing structured base with rind\n");
    fflush (stdout);

    for (n = 0, k = 0; k < NUM_K; k++) {
        for (j = 0; j < NUM_J; j++) {
            for (i = 0; i < NUM_I; i++) {
                compute_coord(n++, i, j, k);
                compute_sol(i, j, k);
            }
        }
    }

    if (cg_base_write(cgfile, "Structured", CellDim, PhyDim, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV( Structured ),
                      &cgzone))
        cg_error_exit();

    /* use cg_coord_general_write to write coordinates with all rinds
       need to use cg_grid_write to create the node, cg_goto to set
       position at the node, then write rind */

    dims[0] = NUM_I;
    dims[1] = NUM_J;
    dims[2] = NUM_K;

    for (n=0; n<3; n++) {
        rmin[n]   = 1-rind[2*n];
        rmax[n]   = size[n] + rind[2*n+1];
        m_rmin[n] = 1;
        m_rmax[n] = dims[n];
    }

    if (cg_grid_write(cgfile, cgbase, cgzone, "GridCoordinates", &cggrid) ||
        cg_goto(cgfile, cgbase, "Zone_t", cgzone,
            "GridCoordinates_t", cggrid, "end") ||
        cg_rind_write(rind) ||
	cg_coord_general_write(cgfile, cgbase, cgzone,
	       	CGNS_ENUMV( RealSingle ), "CoordinateX",
		rmin, rmax, 3, dims, m_rmin, m_rmax, xcoord, &cgcoor) ||
	cg_coord_general_write(cgfile, cgbase, cgzone,
	       	CGNS_ENUMV( RealSingle ), "CoordinateY",
		rmin, rmax, 3, dims, m_rmin, m_rmax, ycoord, &cgcoor) ||
	cg_coord_general_write(cgfile, cgbase, cgzone,
		CGNS_ENUMV( RealSingle ), "CoordinateZ",
		rmin, rmax, 3, dims, m_rmin, m_rmax, zcoord, &cgcoor))
        cg_error_exit();

    /* solution with rind,
       and the solution dimensions come from the zone sizes */

    if (cg_sol_write(cgfile, cgbase, cgzone, "VertexSolution",
		     CGNS_ENUMV( Vertex ), &cgsol) ||
        cg_goto(cgfile, cgbase, "Zone_t", cgzone,
            "FlowSolution_t", cgsol, "end") ||
        cg_rind_write(rind) ||
	cg_field_general_write(cgfile, cgbase, cgzone, cgsol, CGNS_ENUMV( RealSingle ), 
            "Density", rmin, rmax, 3, dims, m_rmin, m_rmax, solution, &cgfld))
        cg_error_exit();


    puts ("closing and reopening in read mode");
    cg_close(cgfile);

    /* read file and check the data */

    if (cg_open ("rind.cgns", CG_MODE_READ, &cgfile))
        cg_error_exit ();
    cgbase = cgzone = cggrid = cgsol = 1;

    puts("checking the data");

    nn = 0;

    /* check coordinates */
    /* Only load core coordinates without rind but inside memory with rind !*/

    for (n=0; n<3; n++) {
        rmin[n]   = 1;
        rmax[n]   = size[n];
        m_rmin[n] = 1 + rind[2*n];
        m_rmax[n] = rind[2*n] + size[n];
    }

    if (cg_coord_general_read(cgfile, cgbase, cgzone, "CoordinateX",
            CGNS_ENUMV(RealSingle), rmin, rmax, 3, dims, m_rmin, m_rmax, fbuf))
        cg_error_exit();

    np = 0;
    for (k = 0 + rind[4]; k < NUM_K-rind[5]; k++) {
        for (j = 0 + rind[2]; j < NUM_J-rind[3]; j++) {
            for (i = 0 + rind[0]; i < NUM_I-rind[1]; i++) {
                if (fbuf[INDEX(i,j,k)] != xcoord[INDEX(i,j,k)]) np++;
            }
        }
    }

    nn += np;
    if (np) printf("%d differences in CoordinateX\n", np);

    if (cg_coord_general_read(cgfile, cgbase, cgzone, "CoordinateY",
            CGNS_ENUMV(RealSingle), rmin, rmax, 3, dims, m_rmin, m_rmax, fbuf))
        cg_error_exit();

    np = 0;
    for (k = 0 + rind[4]; k < NUM_K-rind[5]; k++) {
        for (j = 0 + rind[2]; j < NUM_J-rind[3]; j++) {
            for (i = 0 + rind[0]; i < NUM_I-rind[1]; i++) {
                if (fbuf[INDEX(i,j,k)] != ycoord[INDEX(i,j,k)]) np++;
            }
        }
    }

    nn += np;
    if (np) printf("%d differences in CoordinateY\n", np);

    if (cg_coord_general_read(cgfile, cgbase, cgzone, "CoordinateZ",
            CGNS_ENUMV(RealSingle), rmin, rmax, 3, dims, m_rmin, m_rmax, fbuf))
        cg_error_exit();
                                                                                  
    np = 0;
    for (k = 0 + rind[4]; k < NUM_K-rind[5]; k++) {
        for (j = 0 + rind[2]; j < NUM_J-rind[3]; j++) {
            for (i = 0 + rind[0]; i < NUM_I-rind[1]; i++) {
                if (fbuf[INDEX(i,j,k)] != zcoord[INDEX(i,j,k)]) np++;
            }
        }
    }

    nn += np;
    if (np) printf("%d differences in CoordinateZ\n", np);

    /* check Field with one layer of rind */

    for (n=0; n<3; n++) {
        rmin[n]   = 1 - 1;
        rmax[n]   = size[n] + 1;
        m_rmin[n] = 1 - 1 + rind[2*n];
        m_rmax[n] = rind[2*n] + size[n] + 1;
    }

    if (cg_field_general_read(cgfile, cgbase, cgzone, cgsol, "Density",
            CGNS_ENUMV(RealSingle), rmin, rmax, 3, dims, m_rmin, m_rmax, fbuf))
        cg_error_exit();

    np = 0;
    for (k = 0+rind[4]-1; k < NUM_K-rind[5]+1; k++) {
        for (j = 0+rind[2]-1; j < NUM_J-rind[3]+1; j++) {
            for (i = 0+rind[0]-1; i < NUM_I-rind[1]+1; i++) {
                if (fbuf[INDEX(i,j,k)] != solution[INDEX(i,j,k)]) np++;
            }
        }
    }
    
    nn += np;
    if (np) printf("%d differences in Field\n", np);
    if (nn == 0) puts("no differences");
    puts ("closing file");
    cg_close (cgfile);
    free(xcoord);
    if (nn != 0) exit(1);

    return 0;
}

