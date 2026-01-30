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
    cgsize_t nsize;
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

    /* Verify some key points explicitly */
    printf("Verifying key control point positions...\n");
    /* Corner points in parametric space should be at (-1,-1,-1) to (1,1,1) */
    if (fabs(pu[0] + 1.0) > 1.e-06 || fabs(pv[0] + 1.0) > 1.e-06 || fabs(pw[0] + 1.0) > 1.e-06)
    {
        fprintf(stderr, "ERROR: First point should be (-1,-1,-1)\n");
        cg_error_exit();
    }
    printf("Point 0 (corner): (%g, %g, %g)\n", puu[0], pvv[0], pww[0]);

    /* Center point should be at (0,0,0) - index 13 for 3x3x3 */
    int center_idx = 13; /* k=1, j=1, i=1 in 3x3x3 grid */
    if (fabs(puu[center_idx]) > 1.e-06 || fabs(pvv[center_idx]) > 1.e-06 ||
        fabs(pww[center_idx]) > 1.e-06)
    {
        fprintf(stderr, "ERROR: Center point should be (0,0,0)\n");
        cg_error_exit();
    }
    printf("Point %d (center): (%g, %g, %g)\n", center_idx,
           puu[center_idx], pvv[center_idx], pww[center_idx]);

    /* Last point should be at (1,1,1) - index 26 */
    if (fabs(puu[26] - 1.0) > 1.e-06 || fabs(pvv[26] - 1.0) > 1.e-06 ||
        fabs(pww[26] - 1.0) > 1.e-06)
    {
        fprintf(stderr, "ERROR: Last point should be (1,1,1)\n");
        cg_error_exit();
    }
    printf("Point 26 (corner): (%g, %g, %g)\n", puu[26], pvv[26], pww[26]);

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
