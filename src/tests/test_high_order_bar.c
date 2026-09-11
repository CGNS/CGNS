/*
 * test_high_order_bar.c -- CPEX-0045 high-order interpolation on 1-D elements.
 *
 * Every other high-order test in the suite writes a 2-D or 3-D element, so the
 * one-dimensional path through the interpolation API had no coverage at all:
 * of the 18 cg_element_interpolation_points_write() calls in the suite, none
 * passed pv = pw = NULL.  That is the branch cgi_validate_spatial_ptrs() exists
 * to allow -- for an element of dimension 1 only pu is required, and supplying
 * v/w arrays would be meaningless -- and the pack/unpack helpers likewise had
 * never been exercised with a stride of one coordinate per point.
 *
 * A 1-D element is not a curiosity: BC patches on a 2-D mesh and 1-D slip lines
 * are in scope per the standard's own element table, which lists BAR_3/4/5.
 *
 * Covers BAR_3 (p=2), BAR_4 (p=3) and BAR_5 (p=4):
 *   - ElementInterpolation_t with LagrangeControlPoints, pv = pw = NULL
 *   - round-trip read-back of the control points
 *   - CPEX-0045 S3.2.2 principal-vertex ordering (BAR_2 vertices -1, +1 first)
 *   - cg_npe_ho() and cg_element_lagrange_interpolation_size() agreement
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"

/* Equidistant lattice on the bi-unit interval, then the two BAR_2 principal
 * vertices moved to the front as CPEX-0045 S3.2.2 requires. */
static void fillBarLagrangePoints(int order, double *u)
{
    int i;
    for (i = 0; i <= order; i++)
        u[i] = -1.0 + i * 2.0 / order;
}

static int test_bar_element(CGNS_ENUMT(ElementType_t) type, const char *name,
                            int order, int npts)
{
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    int nfam1, nfam2, n, nsize, npe_ho, i;
    cgsize_t size[3];
    double *pu, *puu;
    char einterpName[33], familyname[33], filename[64];
    CGNS_ENUMT(ElementType_t) etyperead;

    printf("\n==============================================\n");
    printf("  Testing %s (order %d, %d nodes)\n", name, order, npts);
    printf("==============================================\n\n");

    snprintf(filename, sizeof(filename), "test_%s.cgns", name);

    /* cg_npe_ho() on the basic tag must agree with the tag's own cardinality;
     * for a 1-D element both are p+1. */
    if (cg_npe_ho(CGNS_ENUMV(BAR_2), order, &npe_ho))
    {
        fprintf(stderr, "ERROR: cg_npe_ho failed for BAR_2 at order %d\n", order);
        return 1;
    }
    if (npe_ho != npts)
    {
        fprintf(stderr, "ERROR: cg_npe_ho(BAR_2, %d) = %d, expected %d\n",
                order, npe_ho, npts);
        return 1;
    }
    printf("cg_npe_ho(BAR_2, %d) = %d\n", order, npe_ho);

    size[0] = npts;
    size[1] = 1;
    size[2] = 0;

    printf("Creating %s...\n", filename);
    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 1, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        return 1;
    }

    if (cg_family_write(cgfile, cgbase, "BarFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        return 1;
    }

    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "BarInterpolation",
                                       type, &cgeinterp))
    {
        fprintf(stderr, "ERROR: Failed to write ElementInterpolation_t node\n");
        return 1;
    }

    pu = (double *) malloc((size_t)npts * sizeof(double));
    if (pu == NULL)
    {
        fprintf(stderr, "ERROR: malloc failed\n");
        return 1;
    }
    fillBarLagrangePoints(order, pu);
    if (ho_reorder_corners_first(CGNS_ENUMV(BAR_2), npts, pu, NULL, NULL))
    {
        fprintf(stderr, "ERROR: could not order control points corner-first\n");
        free(pu);
        return 1;
    }

    /* The point of this test: a 1-D element supplies pu only.  pv and pw must
     * be accepted as NULL rather than demanded. */
    printf("Writing %d control points with pv = pw = NULL...\n", npts);
    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, cgeinterp,
                                              pu, NULL, NULL))
    {
        fprintf(stderr, "ERROR: Failed to write Lagrange control points: %s\n",
                cg_get_error());
        free(pu);
        return 1;
    }
    cg_close(cgfile);

    /* ---------------------------------------------------------------- read */

    printf("Reopening in READ mode...\n");
    if (cg_open(filename, CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: Failed to reopen file\n");
        free(pu);
        return 1;
    }
    if (cg_family_read(cgfile, cgbase, cgfamily, familyname, &nfam1, &nfam2))
    {
        fprintf(stderr, "ERROR: Failed to read family\n");
        free(pu);
        return 1;
    }
    if (cg_element_lagrange_interpolation_count(cgfile, cgbase, cgfamily, type, &n) ||
        n != 1)
    {
        fprintf(stderr, "ERROR: Expected 1 ElementInterpolation_t node, found %d\n", n);
        free(pu);
        return 1;
    }
    if (cg_element_interpolation_read(cgfile, cgbase, cgfamily, cgeinterp,
                                      einterpName, &etyperead))
    {
        fprintf(stderr, "ERROR: Cannot read ElementInterpolation_t node\n");
        free(pu);
        return 1;
    }
    if (etyperead != type)
    {
        fprintf(stderr, "ERROR: Wrong element type (expected %s, got %s)\n",
                cg_ElementTypeName(type), cg_ElementTypeName(etyperead));
        free(pu);
        return 1;
    }
    if (cg_element_lagrange_interpolation_size(etyperead, &nsize) || nsize != npts)
    {
        fprintf(stderr, "ERROR: Expected %d control points, got %d\n", npts, nsize);
        free(pu);
        return 1;
    }

    puu = (double *) malloc((size_t)nsize * sizeof(double));
    if (puu == NULL)
    {
        fprintf(stderr, "ERROR: malloc failed\n");
        free(pu);
        return 1;
    }

    /* Read back the same way: one coordinate array, the other two NULL. */
    if (cg_element_interpolation_points_read(cgfile, cgbase, cgfamily, cgeinterp,
                                             puu, NULL, NULL))
    {
        fprintf(stderr, "ERROR: Cannot read Lagrange control points: %s\n",
                cg_get_error());
        free(pu); free(puu);
        return 1;
    }

    for (i = 0; i < nsize; i++)
    {
        if (fabs(pu[i] - puu[i]) > 1.e-06)
        {
            fprintf(stderr, "ERROR: control point %d mismatch: wrote %f, read %f\n",
                    i, pu[i], puu[i]);
            free(pu); free(puu);
            return 1;
        }
    }
    printf("All %d control points round-tripped\n", nsize);

    /* CPEX-0045 S3.2.2: the leading points are the BAR_2 principal vertices. */
    if (fabs(puu[0] + 1.0) > 1.e-06 || fabs(puu[1] - 1.0) > 1.e-06)
    {
        fprintf(stderr, "ERROR: leading points should be the BAR_2 vertices "
                "(-1, +1), got (%g, %g)\n", puu[0], puu[1]);
        free(pu); free(puu);
        return 1;
    }
    printf("Principal vertices in order: u[0]=%g u[1]=%g\n", puu[0], puu[1]);

    cg_close(cgfile);
    free(pu);
    free(puu);
    printf("%s PASSED\n", name);
    return 0;
}

int main(void)
{
    int errors = 0;

    printf("##################################################\n");
    printf("#   CPEX-0045 1-D (BAR) high-order interpolation  #\n");
    printf("##################################################\n");

    errors += test_bar_element(CGNS_ENUMV(BAR_3), "BAR_3", 2, 3);
    errors += test_bar_element(CGNS_ENUMV(BAR_4), "BAR_4", 3, 4);
    errors += test_bar_element(CGNS_ENUMV(BAR_5), "BAR_5", 4, 5);

    printf("\n##################################################\n");
    if (errors == 0)
        printf("#   ALL 1-D HIGH-ORDER TESTS PASSED (3/3)         #\n");
    else
        printf("#   %d 1-D HIGH-ORDER TEST(S) FAILED               #\n", errors);
    printf("##################################################\n");

    return errors == 0 ? 0 : 1;
}
