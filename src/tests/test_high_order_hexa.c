#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"

/* Fill parametric coordinates for a hexahedron with given order
 * Grid-like sorted ordering: (u,v,w) from -1 to +1
 * For order 3: 4x4x4 = 64 points (HEXA_64)
 * For order 4: 5x5x5 = 125 points (HEXA_125)
 */
void fillHexaLagrangePoints(int order, double *u, double *v, double *w)
{
    int i, j, k;
    int idx = 0;

    for (k = 0; k <= order; k++)
    {
        for (j = 0; j <= order; j++)
        {
            for (i = 0; i <= order; i++)
            {
                u[idx] = -1.0 + i * 2.0 / order;
                v[idx] = -1.0 + j * 2.0 / order;
                w[idx] = -1.0 + k * 2.0 / order;
                idx++;
            }
        }
    }
}

/* Test a single hexahedron element type */
int test_hexa_element(CGNS_ENUMT(ElementType_t) type, const char* name,
                      int order, int npts)
{
    int error, i, nfam1, nfam2, n;
    int nsize;
    double *pu, *pv, *pw, *puu, *pvv, *pww;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) etyperead;
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    char einterpName[33], familyname[33];
    char filename[64];
    int failed_points = 0;

    printf("\n==============================================\n");
    printf("  Testing %s (Order %d, %d nodes)\n", name, order, npts);
    printf("==============================================\n\n");

    snprintf(filename, sizeof(filename), "test_%s.cgns", name);

    /* Simple 3D structured grid size (metadata only) */
    size[0] = npts;  /* vertex size */
    size[1] = 1;     /* cell size (1 element) */
    size[2] = 0;     /* boundary vertex size */

    /* ========================================================================
     *                              WRITE MODE
     * ======================================================================== */

    printf("Creating CGNS file %s...\n", filename);
    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        return 1;
    }

    printf("Writing family node...\n");
    if (cg_family_write(cgfile, cgbase, "TestFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        return 1;
    }

    printf("Writing ElementInterpolation_t node for %s...\n", name);
    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "HexaInterpolation",
                                       type, &cgeinterp))
    {
        fprintf(stderr, "ERROR: Failed to write ElementInterpolation_t node\n");
        return 1;
    }
    printf("ElementInterpolation_t node created (index=%d)\n", cgeinterp);

    /* Allocate and fill control points */
    pu = (double*) malloc(npts * sizeof(double));
    pv = (double*) malloc(npts * sizeof(double));
    pw = (double*) malloc(npts * sizeof(double));

    fillHexaLagrangePoints(order, pu, pv, pw);

    /* CPEX-0045 S3.2.2 requires the leading points to be the principal
     * vertices in Figure 1 order; the lattice above is lexicographic. */
    if (ho_reorder_corners_first(CGNS_ENUMV(HEXA_8), npts, pu, pv, pw))
    {
        fprintf(stderr, "ERROR: could not order control points corner-first\n");
        return 1;
    }

    printf("Writing Lagrange control points (%d points)...\n", npts);
    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, cgeinterp,
                                              pu, pv, pw))
    {
        fprintf(stderr, "ERROR: Failed to write Lagrange control points\n");
        return 1;
    }
    printf("Written %d control points (%dx%dx%d grid)\n", npts, order+1, order+1, order+1);

    printf("Closing file...\n");
    cg_close(cgfile);

    /* ========================================================================
     *                              READ MODE
     * ======================================================================== */

    printf("\nOpening %s in READ mode...\n", filename);
    if (cg_open(filename, CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: Failed to open file\n");
        return 1;
    }

    /* Read family */
    if (cg_family_read(cgfile, cgbase, cgfamily, familyname, &nfam1, &nfam2))
    {
        fprintf(stderr, "ERROR: Failed to read family\n");
        return 1;
    }
    printf("Family name: %s\n", familyname);

    /* Check ElementInterpolation_t count */
    printf("Validating ElementInterpolation_t node count...\n");
    if (cg_element_lagrange_interpolation_count(cgfile, cgbase, cgfamily, type, &n))
    {
        fprintf(stderr, "ERROR: Failed to count interpolation nodes\n");
        return 1;
    }

    if (n != 1)
    {
        fprintf(stderr, "ERROR: Expected 1 ElementInterpolation_t node, found %d\n", n);
        return 1;
    }
    printf("Found %d ElementInterpolation_t node for %s\n", n, name);

    /* Read ElementInterpolation_t node */
    printf("Reading ElementInterpolation_t properties...\n");
    if (cg_element_interpolation_read(cgfile, cgbase, cgfamily, cgeinterp,
                                      einterpName, &etyperead))
    {
        fprintf(stderr, "ERROR: Cannot read ElementInterpolation_t node\n");
        return 1;
    }
    printf("Interpolation name: %s\n", einterpName);

    if (etyperead != type)
    {
        fprintf(stderr, "ERROR: Wrong element type (expected %s, got %s)\n",
                cg_ElementTypeName(type), cg_ElementTypeName(etyperead));
        return 1;
    }
    printf("Element type: %s\n", cg_ElementTypeName(etyperead));

    /* Check control point size */
    printf("Validating Lagrange control point dimensions...\n");
    if (cg_element_lagrange_interpolation_size(etyperead, &nsize))
    {
        fprintf(stderr, "ERROR: Failed to get interpolation size\n");
        return 1;
    }

    if (nsize != npts)
    {
        fprintf(stderr, "ERROR: Expected %d control points, got %d\n", npts, (int)nsize);
        return 1;
    }
    printf("Expected %d control points for %s\n", (int)nsize, name);

    /* Allocate arrays for reading */
    puu = (double*) malloc(nsize * sizeof(double));
    pvv = (double*) malloc(nsize * sizeof(double));
    pww = (double*) malloc(nsize * sizeof(double));

    /* Read control points */
    printf("Reading Lagrange control points...\n");
    if (cg_element_interpolation_points_read(cgfile, cgbase, cgfamily, cgeinterp,
                                             puu, pvv, pww))
    {
        fprintf(stderr, "ERROR: Cannot read Lagrange control points\n");
        return 1;
    }

    /* Validate control points */
    printf("Validating control point coordinates...\n");
    failed_points = 0;
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
        cg_close(cgfile);
        free(pu); free(pv); free(pw);
        free(puu); free(pvv); free(pww);
        return 1;
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
                cg_close(cgfile);
                free(pu); free(pv); free(pw); free(puu); free(pvv); free(pww);
                return 1;
            }
            printf("Point %d (vertex %d): (%g, %g, %g)\n", c, c+1,
                   puu[c], pvv[c], pww[c]);
        }
    }

    /* The centre (0,0,0) exists for even orders; find it rather than assuming
     * a lattice position, since the set is no longer in lattice order. */
    if (order % 2 == 0)
    {
        int c, found = 0;
        for (c = 0; c < npts; c++)
            if (fabs(puu[c]) < 1.e-06 && fabs(pvv[c]) < 1.e-06 &&
                fabs(pww[c]) < 1.e-06) { found = 1; break; }
        if (!found)
        {
            fprintf(stderr, "ERROR: even order %d must contain the centre (0,0,0)\n", order);
            cg_close(cgfile);
            free(pu); free(pv); free(pw); free(puu); free(pvv); free(pww);
            return 1;
        }
        printf("Point %d (centre): (%g, %g, %g)\n", c, puu[c], pvv[c], pww[c]);
    }

    printf("Closing file...\n");
    cg_close(cgfile);

    free(pu); free(pv); free(pw);
    free(puu); free(pvv); free(pww);

    printf("\nALL TESTS PASSED FOR %s\n", name);
    return 0;
}

int main(int argc, char **argv)
{
    int errors = 0;

    printf("\n");
    printf("##################################################\n");
    printf("#  CPEX0045 Test: Higher Order 3D Elements      #\n");
    printf("##################################################\n");

    /* Test HEXA_64 (order 3) */
    if (test_hexa_element(CGNS_ENUMV(HEXA_64), "HEXA_64", 3, 64))
        errors++;

    /* Test HEXA_125 (order 4) */
    if (test_hexa_element(CGNS_ENUMV(HEXA_125), "HEXA_125", 4, 125))
        errors++;

    printf("\n");
    printf("##################################################\n");
    if (errors == 0)
    {
        printf("#ALL HIGHER ORDER HEXA TESTS PASSED (2/2)  #\n");
    }
    else
    {
        printf("#  ✗ FAILURES: %d element type(s) failed        #\n", errors);
    }
    printf("##################################################\n");
    printf("\n");

    return errors;
}
