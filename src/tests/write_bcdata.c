#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
# include <unistd.h>
#endif
#include "utils.h"

#define WRITE_NODES  0
#define WRITE_BCDATA 1

#define NNODES  1000
#define NCELLS  1000
#define NBCPNTS 100

char *fname = "bcdata.cgns";

int cgfile, cgbase, cgzone, cgcoord, cgsect, cgbc, cgdset;
char name[33];

int size[] = {NNODES, NCELLS, 0};
float coord[NNODES];
int elements[4*NCELLS];

int npnts = NBCPNTS;
int pnts[NBCPNTS];
int dims[] = {NBCPNTS};
float data[NBCPNTS];

float start, finish;

int main (int argc, char **argv)
{
    int i, j, nb = 50, nv = 100;

    if (argc > 1) {
        nb = atoi (argv[1]);
        if (argc > 2)
            nv = atoi (argv[2]);
    }
#if WRITE_NODES
    printf ("number of nodes       = %d\n", NNODES);
    printf ("number of elements    = %d\n", NCELLS);
#endif
#if WRITE_BCDATA
    printf ("number of BC data pts = %d\n", NBCPNTS);
#endif
    printf ("number of BC_t        = %d\n", nb);
    printf ("number of BCDataSet_t = %d\n", nv);

    unlink (fname);
    printf ("creating file ...");
    fflush (stdout);
    start = elapsed_time();

    if (cg_open (fname, CG_MODE_WRITE, &cgfile) ||
        cg_base_write (cgfile, "Base", 3, 3, &cgbase) ||
#if WRITE_NODES
        cg_zone_write (cgfile, cgbase, "Zone", size, Unstructured, &cgzone) ||
        cg_coord_write (cgfile, cgbase, cgzone, RealSingle,
            "CoordinateX", coord, &cgcoord) ||
        cg_coord_write (cgfile, cgbase, cgzone, RealSingle,
            "CoordinateY", coord, &cgcoord) ||
        cg_coord_write (cgfile, cgbase, cgzone, RealSingle,
            "CoordinateZ", coord, &cgcoord) ||
        cg_section_write (cgfile, cgbase, cgzone, "Elements", TETRA_4,
            1, NCELLS, 0, elements, &cgsect))
#else
        cg_zone_write (cgfile, cgbase, "Zone", size, Unstructured, &cgzone))
#endif
        cg_error_exit();

    for (j = 1; j <= nb; j++) {
        sprintf (name, "BC%d", j);
        if (cg_boco_write (cgfile, cgbase, cgzone, name, BCWall, ElementList,
                npnts, pnts, &cgbc))
            cg_error_exit();

        for (i = 1; i <= nv; i++) {
            sprintf (name, "BCData%d", i);
            if (cg_dataset_write (cgfile, cgbase, cgzone, cgbc, name,
                    BCWall, &cgdset) ||
#if WRITE_BCDATA
                cg_bcdata_write (cgfile, cgbase, cgzone, cgbc, cgdset,
                    Dirichlet) ||
                cg_goto (cgfile, cgbase, "Zone_t", 1, "ZoneBC_t", 1,
                    "BC_t", cgbc, "BCDataSet_t", cgdset, "end") ||
                cg_descriptor_write ("label", name) ||
                cg_descriptor_write ("basis", "intensive") ||
                cg_goto (cgfile, cgbase, "Zone_t", 1, "ZoneBC_t", 1,
                    "BC_t", cgbc, "BCDataSet_t", cgdset,
                    "BCData_t", Dirichlet, "end") ||
                cg_array_write ("Data", RealSingle, 1, dims, data))
#else
                cg_bcdata_write (cgfile, cgbase, cgzone, cgbc, cgdset,
                    Dirichlet))
#endif
                cg_error_exit();
        }
    }

    finish = elapsed_time();
    printf (" %.2f secs\n", finish - start);

    printf ("closing file  ...");
    fflush (stdout);
    start = elapsed_time();
    if (cg_close(cgfile)) cg_error_exit();
    finish = elapsed_time();
    printf (" %.2f secs\n", finish - start);

    printf ("opening file  ...");
    fflush (stdout);
    start = elapsed_time();
    if (cg_open (fname, CG_MODE_MODIFY, &cgfile)) cg_error_exit();
    finish = elapsed_time();
    printf (" %.2f secs\n", finish - start);
    cg_close (cgfile);

    return 0;
}

