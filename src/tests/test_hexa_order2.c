#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"

/* Fill parametric coordinates for a hexahedron with given order
 * Grid-like sorted ordering: (u,v,w) from -1 to +1
 * For order 2: 3x3x3 = 27 points
 */
void fillHexaLagrangePoints(int order, double *u, double *v, double *w)
{
    int i, j, k;
    int idx = 0;
    for (k = 0; k < (order+1); k++)
    {
        for (j = 0; j < (order+1); j++)
        {
            for (i = 0; i < (order+1); i++)
            {
                u[idx] = -1.0 + i * 2.0 / order;
                v[idx] = -1.0 + j * 2.0 / order;
                w[idx] = -1.0 + k * 2.0 / order;
                idx++;
            }
        }
    }
}

int main(int argc, char **argv)
{
    int error, i, nfam1, nfam2;
    int n;
    int nsize;
    double *pu, *pv, *pw, *puu, *pvv, *pww;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) type, etyperead;
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    char einterpName[33], familyname[33], zonename[33];
    int order = 2;
    type = CGNS_ENUMV(HEXA_27);

    printf("==============================================\n");
    printf("  CPEX0045 Test: HEXA_27 (Order 2 Hexahedron)\n");
    printf("==============================================\n\n");

    /* Simple 3D structured grid size (just for metadata) */
    size[0] = 8;  /* vertex size */
    size[1] = 1;  /* cell size (1 hex element) */
    size[2] = 0;  /* boundary vertex size */

    /* ========================================================================
     *                              WRITE MODE
     * ======================================================================== */

    printf("Creating CGNS file test_hexa27.cgns...\n");
    if (cg_open("test_hexa27.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
        cg_error_exit();

    printf("Writing family node...\n");
    if (cg_family_write(cgfile, cgbase, "HexaFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        cg_error_exit();
    }

    printf("Writing ElementInterpolation_t node for HEXA_27...\n");
    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "HexaInterpolation",
                                       type, &cgeinterp))
    {
        fprintf(stderr, "ERROR: Failed to write ElementInterpolation_t node\n");
        cg_error_exit();
    }
    printf("ElementInterpolation_t node created (index=%d)\n", cgeinterp);

    /* Allocate and fill control points */
    n = 27; /* 3x3x3 for order 2 */
    pu = (double*) malloc(n * sizeof(double));
    pv = (double*) malloc(n * sizeof(double));
    pw = (double*) malloc(n * sizeof(double));

    fillHexaLagrangePoints(order, pu, pv, pw);
    /* CPEX-0045 S3.2.2: leading points must be the HEXA_8 principal vertices
     * in Figure 1 order; the lattice above is lexicographic. */
    if (ho_reorder_corners_first(CGNS_ENUMV(HEXA_8), n, pu, pv, pw))
    {
        fprintf(stderr, "ERROR: could not order control points corner-first\n");
        cg_error_exit();
    }

    printf("Writing Lagrange control points (27 points)...\n");
    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, cgeinterp,
                                              pu, pv, pw))
    {
        fprintf(stderr, "ERROR: Failed to write Lagrange control points\n");
        cg_error_exit();
    }
    printf("Written 27 control points (3x3x3 grid)\n");

    printf("Closing file...\n\n");
    cg_close(cgfile);

    free(pu);
    free(pv);
    free(pw);

    /* ========================================================================
     *                              READ MODE
     * ======================================================================== */

    printf("==============================================\n");
    printf("  Reading and Validating HEXA_27 Data\n");
    printf("==============================================\n\n");

    printf("Opening test_hexa27.cgns in READ mode...\n");
    if (cg_open("test_hexa27.cgns", CG_MODE_READ, &cgfile))
        cg_error_exit();

    /* Read family */
    if (cg_family_read(cgfile, cgbase, cgfamily, familyname, &nfam1, &nfam2))
        cg_error_exit();

    if (strcmp(familyname, "HexaFamily"))
    {
        fprintf(stderr, "ERROR: Wrong family name!\n");
        cg_error_exit();
    }
    printf("Family name: %s\n", familyname);

    /* Check ElementInterpolation_t count */
    printf("Validating ElementInterpolation_t node count...\n");
    if (cg_element_lagrange_interpolation_count(cgfile, cgbase, cgfamily, type, &n))
        cg_error_exit();

    if (n != 1)
    {
        fprintf(stderr, "ERROR: Expected 1 ElementInterpolation_t node, found %d\n", n);
        cg_error_exit();
    }
    printf("Found %d ElementInterpolation_t node for HEXA_27\n", n);

    /* Read ElementInterpolation_t node */
    printf("Reading ElementInterpolation_t properties...\n");
    if (cg_element_interpolation_read(cgfile, cgbase, cgfamily, cgeinterp,
                                      einterpName, &etyperead))
    {
        fprintf(stderr, "ERROR: Cannot read ElementInterpolation_t node\n");
        cg_error_exit();
    }

    if (strcmp(einterpName, "HexaInterpolation"))
    {
        fprintf(stderr, "ERROR: Wrong interpolation name\n");
        cg_error_exit();
    }
    printf("Interpolation name: %s\n", einterpName);

    if (etyperead != type)
    {
        fprintf(stderr, "ERROR: Wrong element type (expected %s, got %s)\n",
                cg_ElementTypeName(type), cg_ElementTypeName(etyperead));
        cg_error_exit();
    }
    printf("Element type: %s\n", cg_ElementTypeName(etyperead));

    /* Check control point size */
    printf("Validating Lagrange control point dimensions...\n");
    if (cg_element_lagrange_interpolation_size(etyperead, &nsize))
        cg_error_exit();

    if (nsize != 27)
    {
        fprintf(stderr, "ERROR: Expected 27 control points, got %d\n", (int)nsize);
        cg_error_exit();
    }
    printf("Expected %d control points for HEXA_27 (3x3x3)\n", (int)nsize);

    /* Allocate arrays for reading */
    pu = (double*) malloc(nsize * sizeof(double));
    pv = (double*) malloc(nsize * sizeof(double));
    pw = (double*) malloc(nsize * sizeof(double));
    puu = (double*) malloc(nsize * sizeof(double));
    pvv = (double*) malloc(nsize * sizeof(double));
    pww = (double*) malloc(nsize * sizeof(double));

    /* Fill expected values */
    fillHexaLagrangePoints(order, pu, pv, pw);
    /* CPEX-0045 S3.2.2: leading points must be the HEXA_8 principal vertices
     * in Figure 1 order; the lattice above is lexicographic. */
    if (ho_reorder_corners_first(CGNS_ENUMV(HEXA_8), (int)nsize, pu, pv, pw))
    {
        fprintf(stderr, "ERROR: could not order control points corner-first\n");
        cg_error_exit();
    }

    /* Read control points */
    printf("Reading Lagrange control points...\n");
    if (cg_element_interpolation_points_read(cgfile, cgbase, cgfamily, cgeinterp,
                                             puu, pvv, pww))
    {
        fprintf(stderr, "ERROR: Cannot read Lagrange control points\n");
        cg_error_exit();
    }

    /* Validate control points */
    printf("Validating control point coordinates...\n");
    int failed_points = 0;
    for (i = 0; i < nsize; i++)
    {
        if (fabs(pu[i] - puu[i]) > 1.e-06 ||
            fabs(pv[i] - pvv[i]) > 1.e-06 ||
            fabs(pw[i] - pww[i]) > 1.e-06)
        {
            fprintf(stderr, "ERROR: Control point %d mismatch\n", i);
            fprintf(stderr, "  Expected: (%f, %f, %f)\n", pu[i], pv[i], pw[i]);
            fprintf(stderr, "  Got:      (%f, %f, %f)\n", puu[i], pvv[i], pww[i]);
            fprintf(stderr, "  Error:    (%e, %e, %e)\n",
                    fabs(pu[i] - puu[i]), fabs(pv[i] - pvv[i]), fabs(pw[i] - pww[i]));
            failed_points++;
        }
    }

    if (failed_points > 0)
    {
        fprintf(stderr, "ERROR: %d control points failed validation\n", failed_points);
        cg_error_exit();
    }
    printf("All %d control points validated successfully\n", (int)nsize);

    /* Verify the leading points are the HEXA_8 principal vertices in
     * Figure 1 order, as CPEX-0045 S3.2.2 requires. */
    printf("Verifying principal-vertex ordering...\n");
    {
        static const double cu[8] = {-1., 1., 1.,-1.,-1., 1., 1.,-1.};
        static const double cv[8] = {-1.,-1., 1., 1.,-1.,-1., 1., 1.};
        static const double cw[8] = {-1.,-1.,-1.,-1., 1., 1., 1., 1.};
        int c;
        for (c = 0; c < 8; c++)
        {
            if (fabs(puu[c] - cu[c]) > 1.e-06 || fabs(pvv[c] - cv[c]) > 1.e-06 ||
                fabs(pww[c] - cw[c]) > 1.e-06)
            {
                fprintf(stderr, "ERROR: control point %d should be the HEXA_8 "
                        "vertex (%g, %g, %g), got (%g, %g, %g)\n",
                        c, cu[c], cv[c], cw[c], puu[c], pvv[c], pww[c]);
                cg_error_exit();
            }
            printf("Point %d (vertex %d): (%g, %g, %g)\n", c, c+1,
                   puu[c], pvv[c], pww[c]);
        }
    }

    /* The centre (0,0,0) is present at order 2; locate it rather than assuming
     * lattice index 13, since the set is no longer in lattice order. */
    {
        int c, found = 0;
        for (c = 0; c < (int)nsize; c++)
            if (fabs(puu[c]) < 1.e-06 && fabs(pvv[c]) < 1.e-06 &&
                fabs(pww[c]) < 1.e-06) { found = 1; break; }
        if (!found)
        {
            fprintf(stderr, "ERROR: HEXA_27 must contain the centre (0,0,0)\n");
            cg_error_exit();
        }
        printf("Point %d (centre): (%g, %g, %g)\n", c, puu[c], pvv[c], pww[c]);
    }

    printf("\nClosing file...\n");
    cg_close(cgfile);

    free(pu);
    free(pv);
    free(pw);
    free(puu);
    free(pvv);
    free(pww);

    printf("\n==============================================\n");
    printf("ALL TESTS PASSED FOR HEXA_27\n");
    printf("==============================================\n");

    return 0;
}
