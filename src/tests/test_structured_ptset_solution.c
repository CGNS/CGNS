/*
 * test_structured_ptset_solution.c -- CTest fixture for cgnscheck's
 * check_solution() point-set buffer-overflow fix (commit 688b3530).
 *
 * cg_sol_ptset_read() returns npts * IndexDimension values -- a structured
 * zone stores one index per dimension for every point -- but check_solution()
 * used to size its read buffer as 2 (for a PointRange) or npts (for a
 * PointList), regardless of idim. On a 3-D structured zone with a PointRange
 * that was a 48-byte write into a 16-byte allocation. Nothing high-order is
 * needed to reach this bug; it is ordinary, otherwise-conformant CGNS -- which
 * is exactly why no fixture in the CGNSCHECK_HO_FILES list (all
 * Unstructured-zone, high-order-specific) exercised it. This writes a plain
 * 3-D structured zone with a CellCenter FlowSolution_t restricted to a
 * PointRange subset, for cgnscheck to read in both modes.
 */

#include <stdio.h>
#include <stdlib.h>
#include "cgnslib.h"

int main(void)
{
    int fn, B, Z, S;
    cgsize_t size[9];
    cgsize_t range[6]; /* [imin,jmin,kmin, imax,jmax,kmax] */
    int i;

    for (i = 0; i < 3; i++) {
        size[i]     = 4;      /* vertices per direction */
        size[i + 3] = 3;      /* cells per direction */
        size[i + 6] = 0;      /* unsorted boundary count (structured: always 0) */
    }

    if (cg_open("test_structured_ptset_solution.cgns", CG_MODE_WRITE, &fn) ||
        cg_base_write(fn, "Base", 3, 3, &B) ||
        cg_zone_write(fn, B, "Zone", size, CGNS_ENUMV(Structured), &Z))
    {
        fprintf(stderr, "ERROR: could not create base structure: %s\n", cg_get_error());
        return 1;
    }

    {
        cgsize_t npts = (cgsize_t)(size[0] * size[1] * size[2]);
        double *coord = (double *) malloc((size_t)npts * sizeof(double));
        int ci;
        if (!coord) { fprintf(stderr, "ERROR: OOM\n"); return 1; }
        for (i = 0; i < npts; i++) coord[i] = (double)i;
        if (cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", coord, &ci) ||
            cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", coord, &ci) ||
            cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateZ", coord, &ci))
        {
            fprintf(stderr, "ERROR: could not write coordinates: %s\n", cg_get_error());
            free(coord);
            return 1;
        }
        free(coord);
    }

    /* A 2x2x2 subset of the 3x3x3 cells (1-based cell indices). */
    range[0] = 1; range[1] = 1; range[2] = 1;
    range[3] = 2; range[4] = 2; range[5] = 2;
    if (cg_sol_ptset_write(fn, B, Z, "FS", CGNS_ENUMV(CellCenter),
                           CGNS_ENUMV(PointRange), 2, range, &S))
    {
        fprintf(stderr, "ERROR: could not write PointRange solution: %s\n", cg_get_error());
        return 1;
    }
    /* No field: check_solution()'s point-set read (the code path
     * commit 688b3530 fixed) runs unconditionally once a PointRange/PointList
     * is present, before the field-array loop -- a field's own dimensions are
     * a separate, unrelated check (and cgnscheck's plain, non-high-order field
     * sizing does not itself account for a point-set restricting a
     * CellCenter/Vertex solution to a subset, an independent, pre-existing
     * characteristic of check_solution() this fixture is not testing). */

    if (cg_close(fn)) { fprintf(stderr, "ERROR: close failed\n"); return 1; }

    printf("wrote test_structured_ptset_solution.cgns\n");
    return 0;
}
