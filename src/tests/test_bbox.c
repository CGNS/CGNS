#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
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

#define NUM_SIDE 5

float coord[NUM_SIDE*NUM_SIDE*NUM_SIDE];

#define CHECK(L,B) if(!(B)){fprintf(stderr,"mismatch in %s\n",L);exit(1);}

void error_exit (char *where)
{
    fprintf (stderr, "ERROR:%s:%s\n", where, cg_get_error());
    exit (1);
}

int main (int argc, char **argv)
{
    int n, i;
    int cgfile, cgbase, cgzone, cgcoord;
    cgsize_t size[9];
    static char *fname = "boundingbox.cgns";
    double bbox[2][3];


    if (argc > 1) {
        n = 0;
        if (argv[1][n] == '-') n++;
        if (argv[1][n] == 'a' || argv[1][n] == 'A')
            i = CG_FILE_ADF;
        else if (argv[1][n] == 'h' || argv[1][n] == 'H')
            i = CG_FILE_HDF5;
        else {
            fprintf(stderr, "unknown option\n");
            exit (1);
        }
        if (cg_set_file_type(i))
            error_exit("cg_set_file_type");
    }

    for (n = 0; n < NUM_SIDE*NUM_SIDE*NUM_SIDE; n++)
        coord[n] = (float)n;

    unlink (fname);
    printf ("creating CGNS file %s\n", fname);
    if (cg_open (fname, CG_MODE_WRITE, &cgfile) ||
        cg_base_write (cgfile, "Base", 3, 3, &cgbase))
        error_exit ("write base");

    /* write zone */

    puts("writing zone");
    for (n = 0; n < 3; n++) {
        size[n]   = NUM_SIDE;
        size[n+3] = NUM_SIDE - 1;
        size[n+6] = 0;
    }
    if (cg_zone_write (cgfile, cgbase, "Zone", size, CGNS_ENUMV(Structured), &cgzone) ||
        cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
            "CoordinateX", coord, &cgcoord) ||
        cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
            "CoordinateY", coord, &cgcoord) ||
        cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
            "CoordinateZ", coord, &cgcoord))
        cg_error_exit();

    puts ("closing and reopening in read mode");
    if (cg_close (cgfile))
        cg_error_exit ();

    /* read file */

    if (cg_open (fname, CG_MODE_READ, &cgfile))
        cg_error_exit ();
    cgbase = cgzone = 1;

    bbox[0][0] = 1.0;
    bbox[1][0] = -1.0;
    bbox[0][1] = 1.0;
    bbox[1][1] = -1.0;
    bbox[0][2] = 1.0;
    bbox[1][2] = -1.0;
    /* check bounding box is not modified */
    if (cg_grid_bounding_box_read(cgfile, cgbase, cgzone, 1, CGNS_ENUMV(RealDouble), bbox)) {
        cg_error_exit();
    }
    CHECK("Unmodified bounding box data when missing data in GridCoordinates node", bbox[1][0] == -1.0);
    CHECK("Unmodified bounding box data when missing data in GridCoordinates node", bbox[0][0] == 1.0);
    puts ("closing and reopening in modify mode");
    if (cg_close (cgfile))
        cg_error_exit ();

    if (cg_open (fname, CG_MODE_MODIFY, &cgfile))
        cg_error_exit ();
    cgbase = cgzone = 1;

    bbox[0][0] = 0.0;
    bbox[1][0] = (double)(NUM_SIDE*NUM_SIDE*NUM_SIDE -1);
    bbox[0][1] = 0.0;
    bbox[1][1] = (double)(NUM_SIDE*NUM_SIDE*NUM_SIDE -1);
    bbox[0][2] = 0.0;
    bbox[1][2] = (double)(NUM_SIDE*NUM_SIDE*NUM_SIDE -1);

    if (cg_grid_bounding_box_write(cgfile, cgbase, cgzone, 1, CGNS_ENUMV(RealDouble), bbox)){
        cg_error_exit();
    }

    puts ("closing and reopening in read mode");
    if (cg_close (cgfile)) {
        cg_error_exit();
    }

    if (cg_open (fname, CG_MODE_READ, &cgfile))
        cg_error_exit ();
    cgbase = cgzone = 1;

    bbox[0][0] = 1.0;
    bbox[1][0] = -1.0;
    bbox[0][1] = 1.0;
    bbox[1][1] = -1.0;
    bbox[0][2] = 1.0;
    bbox[1][2] = -1.0;
    /* check bounding box is not modified */
    if (cg_grid_bounding_box_read(cgfile, cgbase, cgzone, 1, CGNS_ENUMV(RealDouble), bbox)){
        cg_error_exit ();
    }
    CHECK("Read bouding box", bbox[0][0] == 0.0);
    CHECK("Read bouding box", bbox[1][0] == (double)(NUM_SIDE*NUM_SIDE*NUM_SIDE -1));

    puts ("closing and reopening to test limit cases");
    if (cg_close (cgfile))
        cg_error_exit ();

    if (cg_open (fname, CG_MODE_MODIFY, &cgfile))
        cg_error_exit ();
    cgbase = cgzone = 1;

    /* writing integer type should fail */
    if (!cg_grid_bounding_box_write(cgfile, cgbase, cgzone, 1, CGNS_ENUMV(Integer), bbox)){
        printf("writing Integer bounding box data failed to produce error\n");
	exit(1);
    }
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */

    /* writing empty bbox should do nothing */
    if (cg_grid_bounding_box_write(cgfile, cgbase, cgzone, 1, CGNS_ENUMV(RealSingle), NULL)){
        printf("writing NULL bounding box raised an unexpected error\n");
        cg_error_print();
	exit(1);
    }

    puts ("closing file");
    if (cg_close (cgfile))
        cg_error_exit ();

    return 0;
}

