#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"

/* Test 7.1: Invalid Element Types and Interpolation Types */
int test_invalid_types(void)
{
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    cgsize_t size[9];
    int result;

    printf("\n==============================================\n");
    printf("  Test 7.1: Invalid Types Error Handling\n");
    printf("==============================================\n\n");

    /* Create basic CGNS structure */
    size[0] = 4;
    size[1] = 1;
    size[2] = 0;

    if (cg_open("test_error_invalid_types.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone) ||
        cg_family_write(cgfile, cgbase, "TestFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to create basic CGNS structure\n");
        return 1;
    }

    printf("Testing invalid element type (MIXED)...\n");
    /* MIXED element type should be rejected for ElementInterpolation_t */
    result = cg_element_interpolation_write(cgfile, cgbase, cgfamily, "BadInterp",
                                           CGNS_ENUMV(MIXED), &cgeinterp);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: MIXED element type should have been rejected\n");
        cg_close(cgfile);
        return 1;
    }
    printf("MIXED element type correctly rejected (error code: %d)\n", result);

    /* NODE is a valid tag, not a degenerate one: it is 0-dimensional, so its
     * basis has exactly one control point.  Accepting it is correct, and the
     * cardinality is what makes that meaningful -- assert both. */
    printf("Testing NODE element type (0-dimensional, one control point)...\n");
    result = cg_element_interpolation_write(cgfile, cgbase, cgfamily, "NodeInterp",
                                           CGNS_ENUMV(NODE), &cgeinterp);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: NODE element type should be accepted: %s\n",
                cg_get_error());
        cg_close(cgfile);
        return 1;
    }
    {
        int nsize = -1;
        if (cg_element_lagrange_interpolation_size(CGNS_ENUMV(NODE), &nsize))
        {
            fprintf(stderr, "ERROR: cannot size the NODE basis: %s\n", cg_get_error());
            cg_close(cgfile);
            return 1;
        }
        if (nsize != 1)
        {
            fprintf(stderr, "ERROR: NODE basis should have 1 control point, got %d\n",
                    nsize);
            cg_close(cgfile);
            return 1;
        }
    }
    printf("NODE accepted with a 1-point basis (index=%d)\n", cgeinterp);

    printf("Testing out-of-range element type enum...\n");
    /* Completely invalid enum value */
    result = cg_element_interpolation_write(cgfile, cgbase, cgfamily, "BadInterp",
                                           (CGNS_ENUMT(ElementType_t))9999, &cgeinterp);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: Invalid enum value should have been rejected\n");
        cg_close(cgfile);
        return 1;
    }
    printf("Invalid enum value correctly rejected (error code: %d)\n", result);

    cg_close(cgfile);

    printf("\nTEST 7.1 PASSED: Invalid Types Correctly Rejected\n");
    return 0;
}

/* Test 7.2: control-point array dimensions.
 *
 * The caller supplies the buffers, so the load-bearing contract is what the
 * library puts on disk: LagrangeControlPoints must be shaped (dim, npts) with
 * npts the cardinality of the element's basis and dim the element's parametric
 * dimension.  A 2D element must store 2 rows even when the caller passes a pw
 * array, because a 3-row array would make every reader mis-stride the data.
 * Both the shape and the values are checked here. */
int test_mismatched_dimensions(void)
{
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp, cgsinterp;
    cgsize_t size[9];
    double pu[25], pv[25], pw[25];
    int i, nsize;

    printf("\n==============================================\n");
    printf("  Test 7.2: Control-Point Array Dimensions\n");
    printf("==============================================\n\n");

    /* Create basic CGNS structure */
    size[0] = 9;
    size[1] = 1;
    size[2] = 0;

    if (cg_open("test_error_dimensions.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone) ||
        cg_family_write(cgfile, cgbase, "TestFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to create basic CGNS structure\n");
        return 1;
    }

    printf("QUAD_9 basis cardinality...\n");
    if (cg_element_lagrange_interpolation_size(CGNS_ENUMV(QUAD_9), &nsize))
    {
        fprintf(stderr, "ERROR: cannot size the QUAD_9 basis: %s\n", cg_get_error());
        cg_close(cgfile);
        return 1;
    }
    if (nsize != 9)
    {
        fprintf(stderr, "ERROR: QUAD_9 basis should have 9 control points, got %d\n",
                nsize);
        cg_close(cgfile);
        return 1;
    }
    printf("  %d control points\n", nsize);

    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "QuadInterp",
                                      CGNS_ENUMV(QUAD_9), &cgeinterp))
    {
        fprintf(stderr, "ERROR: Failed to write ElementInterpolation_t\n");
        cg_close(cgfile);
        return 1;
    }

    /* Distinct values per axis, so a transposed or mis-strided read is visible */
    for (i = 0; i < 9; i++) {
        pu[i] = (double)i;
        pv[i] = 100.0 + (double)i;
        pw[i] = 200.0 + (double)i;
    }

    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, cgeinterp,
                                              pu, pv, NULL))
    {
        fprintf(stderr, "ERROR: Failed to write control points\n");
        cg_close(cgfile);
        return 1;
    }
    printf("Control points written\n");

    printf("\nPassing a pw array to a 2D element must not add a third row...\n");
    if (cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "Solution2D",
                                       CGNS_ENUMV(QUAD_4), 2, 0,
                                       CGNS_ENUMV(ParametricLagrange), &cgsinterp))
    {
        fprintf(stderr, "ERROR: Failed to write SolutionInterpolation_t\n");
        cg_close(cgfile);
        return 1;
    }
    if (cg_solution_interpolation_points_write(cgfile, cgbase, cgfamily, cgsinterp, 9,
                                               pu, pv, pw, NULL))
    {
        fprintf(stderr, "ERROR: Failed to write solution control points\n");
        cg_close(cgfile);
        return 1;
    }

    cg_close(cgfile);

    if (cg_open("test_error_dimensions.cgns", CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: Failed to reopen file\n");
        return 1;
    }

    /* The on-disk shape of both LagrangeControlPoints arrays */
    {
        struct { const char *label; const char *nodelabel; int npts; } shape[2] = {
            { "ElementInterpolation_t",  "ElementInterpolation_t",  9 },
            { "SolutionInterpolation_t", "SolutionInterpolation_t", 9 }
        };
        int s;
        for (s = 0; s < 2; s++)
        {
            char aname[33];
            int ndim;
            cgsize_t dimv[3];
            CGNS_ENUMT(DataType_t) dt;

            if (cg_goto(cgfile, cgbase, "Family_t", cgfamily,
                        shape[s].nodelabel, 1, NULL) ||
                cg_array_info(1, aname, &dt, &ndim, dimv))
            {
                fprintf(stderr, "ERROR: cannot inspect %s array: %s\n",
                        shape[s].label, cg_get_error());
                cg_close(cgfile);
                return 1;
            }
            if (strcmp(aname, "LagrangeControlPoints"))
            {
                fprintf(stderr, "ERROR: %s array is '%s', expected "
                                "LagrangeControlPoints\n", shape[s].label, aname);
                cg_close(cgfile);
                return 1;
            }
            if (ndim != 2 || dimv[0] != 2 || dimv[1] != shape[s].npts)
            {
                fprintf(stderr, "ERROR: %s LagrangeControlPoints shape is "
                                "%dD (%d,%d), expected 2D (2,%d)\n",
                        shape[s].label, ndim, (int)dimv[0], (int)dimv[1],
                        shape[s].npts);
                cg_close(cgfile);
                return 1;
            }
            printf("  %s: (%d,%d) as required\n",
                   shape[s].label, (int)dimv[0], (int)dimv[1]);
        }
    }

    /* And the values survive the round trip on both axes */
    {
        double back_u[9], back_v[9], back_w[9];

        if (cg_element_interpolation_points_read(cgfile, cgbase, cgfamily, cgeinterp,
                                                 back_u, back_v, NULL))
        {
            fprintf(stderr, "ERROR: Failed to read element control points: %s\n",
                    cg_get_error());
            cg_close(cgfile);
            return 1;
        }
        for (i = 0; i < 9; i++)
        {
            if (back_u[i] != pu[i] || back_v[i] != pv[i])
            {
                fprintf(stderr, "ERROR: element point %d = (%g,%g), wrote (%g,%g)\n",
                        i, back_u[i], back_v[i], pu[i], pv[i]);
                cg_close(cgfile);
                return 1;
            }
        }

        /* pw was supplied but must have been ignored: the reader fills only the
         * two stored axes, so a third buffer is left untouched. */
        for (i = 0; i < 9; i++) back_w[i] = -1.0;
        if (cg_solution_interpolation_points_read(cgfile, cgbase, cgfamily, cgsinterp,
                                                  back_u, back_v, back_w, NULL))
        {
            fprintf(stderr, "ERROR: Failed to read solution control points: %s\n",
                    cg_get_error());
            cg_close(cgfile);
            return 1;
        }
        for (i = 0; i < 9; i++)
        {
            if (back_u[i] != pu[i] || back_v[i] != pv[i])
            {
                fprintf(stderr, "ERROR: solution point %d = (%g,%g), wrote (%g,%g)\n",
                        i, back_u[i], back_v[i], pu[i], pv[i]);
                cg_close(cgfile);
                return 1;
            }
            if (back_w[i] != -1.0)
            {
                fprintf(stderr, "ERROR: a third axis was stored for a 2D element: "
                                "back_w[%d] = %g\n", i, back_w[i]);
                cg_close(cgfile);
                return 1;
            }
        }
    }
    printf("Control points round-trip on both axes; pw was ignored\n");

    cg_close(cgfile);

    printf("\nTEST 7.2 PASSED: Array Dimensions Verified\n");
    return 0;
}

/* Test 7.3: Out of Range Orders */
int test_out_of_range_orders(void)
{
    int cgfile, cgbase, cgzone, cgfamily, cgsinterp;
    cgsize_t size[9];
    int result;

    printf("\n==============================================\n");
    printf("  Test 7.3: Out of Range Interpolation Orders\n");
    printf("==============================================\n\n");

    /* Create basic CGNS structure */
    size[0] = 4;
    size[1] = 1;
    size[2] = 0;

    if (cg_open("test_error_orders.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone) ||
        cg_family_write(cgfile, cgbase, "TestFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to create basic CGNS structure\n");
        return 1;
    }

    /* Spatial order 0 is valid, not an edge case to be rejected: it denotes one
     * spatial degree of freedom per element, i.e. a solution constant over the
     * element -- v2's "standard interpolation (constant per element)" and the
     * natural form of a finite-volume cell average.  For Lagrange it is a single
     * control point whose nodal function is identically one, and one point is
     * unisolvent for P_0. */
    printf("Testing spatial order = 0 (valid: constant per element)...\n");
    result = cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "Order0",
                                            CGNS_ENUMV(QUAD_4), 0, 0,
                                            CGNS_ENUMV(ParametricLagrange), &cgsinterp);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: spatial order 0 should be accepted: %s\n", cg_get_error());
        return 1;
    }
    printf("  accepted (constant per element)\n");

    /* A negative degree must be refused at the point it would enter the file.
     * Every path that later sizes the basis -- cg_npe_ho,
     * cg_solution_lagrange_interpolation_size, cg_solution_monomial_size --
     * rejects anything outside [0, CG_MAX_ORDER], so a writer that accepted one
     * would produce a file it could not itself size on read. */
    printf("Testing negative spatial order (invalid)...\n");
    result = cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "BadOrderNeg",
                                            CGNS_ENUMV(QUAD_4), -1, 0,
                                            CGNS_ENUMV(ParametricLagrange), &cgsinterp);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: negative spatial order must be rejected\n");
        cg_close(cgfile);
        return 1;
    }
    printf("Negative spatial order correctly rejected (error code: %d)\n", result);

    printf("Testing spatial order above CG_MAX_ORDER...\n");
    result = cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "TooHigh",
                                            CGNS_ENUMV(QUAD_4), CG_MAX_ORDER + 1, 0,
                                            CGNS_ENUMV(ParametricLagrange), &cgsinterp);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: spatial order %d exceeds CG_MAX_ORDER (%d) and "
                        "must be rejected\n", CG_MAX_ORDER + 1, CG_MAX_ORDER);
        cg_close(cgfile);
        return 1;
    }
    printf("Spatial order %d correctly rejected (error code: %d)\n",
           CG_MAX_ORDER + 1, result);

    /* Order 100 is inside CG_MAX_ORDER, so it must be accepted -- and the basis
     * it names must size correctly: QUAD_4 Lagrange at degree p has (p+1)^2
     * control points, so 101^2 = 10201. */
    printf("Testing high but valid spatial order (100)...\n");
    result = cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "HighOrder",
                                            CGNS_ENUMV(QUAD_4), 100, 0,
                                            CGNS_ENUMV(ParametricLagrange), &cgsinterp);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: order 100 is within CG_MAX_ORDER and must be "
                        "accepted: %s\n", cg_get_error());
        cg_close(cgfile);
        return 1;
    }
    {
        int npts = 0;
        if (cg_solution_lagrange_interpolation_size(CGNS_ENUMV(QUAD_4), 100, 0, &npts))
        {
            fprintf(stderr, "ERROR: cannot size the order-100 basis: %s\n",
                    cg_get_error());
            cg_close(cgfile);
            return 1;
        }
        if (npts != 101 * 101)
        {
            fprintf(stderr, "ERROR: QUAD_4 at degree 100 should have %d control "
                            "points, got %d\n", 101 * 101, npts);
            cg_close(cgfile);
            return 1;
        }
        printf("Order 100 accepted, basis has %d control points\n", npts);
    }

    /* INT_MAX spatial order via cg_solution_monomial_size: must not silently overflow
     * binomial_coefficient(os + dim, dim).  A malicious or corrupted file providing
     * os=INT_MAX makes os+dim wrap negative, binomial_coefficient returns 1, and a
     * subsequent write can overflow the tiny allocation.  Verify the guard fires. */
    {
        int msize;
        printf("Testing INT_MAX spatial order in cg_solution_monomial_size (overflow guard)...\n");
        result = cg_solution_monomial_size(CGNS_ENUMV(QUAD_4), INT_MAX, 0, &msize);
        if (result == CG_OK)
        {
            fprintf(stderr, "ERROR: INT_MAX spatial order must be rejected by "
                    "cg_solution_monomial_size (overflow risk)\n");
            cg_close(cgfile);
            return 1;
        }
        printf("INT_MAX spatial order correctly rejected (error code: %d)\n", result);

        printf("Testing INT_MAX temporal order in cg_solution_monomial_size (overflow guard)...\n");
        result = cg_solution_monomial_size(CGNS_ENUMV(QUAD_4), 2, INT_MAX, &msize);
        if (result == CG_OK)
        {
            fprintf(stderr, "ERROR: INT_MAX temporal order must be rejected by "
                    "cg_solution_monomial_size (overflow risk)\n");
            cg_close(cgfile);
            return 1;
        }
        printf("INT_MAX temporal order correctly rejected (error code: %d)\n", result);
    }

    printf("\nTesting negative temporal order...\n");
    result = cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "BadTemporal",
                                            CGNS_ENUMV(QUAD_4), 2, -1,
                                            CGNS_ENUMV(ParametricLagrange), &cgsinterp);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: negative temporal order must be rejected\n");
        cg_close(cgfile);
        return 1;
    }
    printf("Negative temporal order correctly rejected (error code: %d)\n", result);

    printf("\nTesting valid edge case: order = 1...\n");
    result = cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "Order1",
                                            CGNS_ENUMV(QUAD_4), 1, 0,
                                            CGNS_ENUMV(ParametricLagrange), &cgsinterp);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: Order 1 should be valid\n");
        cg_close(cgfile);
        return 1;
    }
    printf("Spatial order 1 accepted (valid)\n");

    cg_close(cgfile);

    printf("\nTEST 7.3 PASSED: Order Validation Working\n");
    return 0;
}

/* Test 7.4: Duplicate Node Detection */
int test_duplicate_nodes(void)
{
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp1, cgeinterp2;
    cgsize_t size[9];
    int result;

    printf("\n==============================================\n");
    printf("  Test 7.4: Duplicate Node Detection\n");
    printf("==============================================\n\n");

    /* Create basic CGNS structure */
    size[0] = 9;
    size[1] = 1;
    size[2] = 0;

    if (cg_open("test_error_duplicates.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone) ||
        cg_family_write(cgfile, cgbase, "TestFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to create basic CGNS structure\n");
        return 1;
    }

    printf("Writing first ElementInterpolation_t for QUAD_9...\n");
    result = cg_element_interpolation_write(cgfile, cgbase, cgfamily, "QuadInterp1",
                                           CGNS_ENUMV(QUAD_9), &cgeinterp1);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: First ElementInterpolation write failed\n");
        cg_close(cgfile);
        return 1;
    }
    printf("First node created (index=%d)\n", cgeinterp1);

    printf("Attempting to write duplicate ElementInterpolation_t for QUAD_9...\n");
    result = cg_element_interpolation_write(cgfile, cgbase, cgfamily, "QuadInterp2",
                                           CGNS_ENUMV(QUAD_9), &cgeinterp2);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: Duplicate ElementInterpolation_t should have been rejected\n");
        cg_close(cgfile);
        return 1;
    }
    printf("Duplicate correctly rejected (error code: %d)\n", result);

    printf("Writing ElementInterpolation_t for different element type (TRI_10)...\n");
    result = cg_element_interpolation_write(cgfile, cgbase, cgfamily, "TriInterp",
                                           CGNS_ENUMV(TRI_10), &cgeinterp2);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: Different element type should be allowed\n");
        cg_close(cgfile);
        return 1;
    }
    printf("Different element type allowed (index=%d)\n", cgeinterp2);

    cg_close(cgfile);

    printf("\nTEST 7.4 PASSED: Duplicate Detection Working\n");
    return 0;
}

/* Test 7.5: NULL Pointer Handling */
int test_null_pointers(void)
{
    int cgfile, cgbase, cgzone, cgfamily, cgsinterp;
    cgsize_t size[9];
    double pu[9], pv[9];
    int i, result;

    printf("\n==============================================\n");
    printf("  Test 7.5: NULL Pointer Handling\n");
    printf("==============================================\n\n");

    /* Create basic CGNS structure */
    size[0] = 9;
    size[1] = 1;
    size[2] = 0;

    if (cg_open("test_error_nulls.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone) ||
        cg_family_write(cgfile, cgbase, "TestFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to create basic CGNS structure\n");
        return 1;
    }

    if (cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "TestInterp",
                                       CGNS_ENUMV(QUAD_4), 2, 0,
                                       CGNS_ENUMV(ParametricLagrange), &cgsinterp))
    {
        fprintf(stderr, "ERROR: Failed to write SolutionInterpolation_t\n");
        cg_close(cgfile);
        return 1;
    }

    for (i = 0; i < 9; i++) {
        pu[i] = pv[i] = (double)i;
    }

    printf("Testing NULL pu parameter (required)...\n");
    result = cg_solution_interpolation_points_write(cgfile, cgbase, cgfamily, cgsinterp, 9,
                                                    NULL, pv, NULL, NULL);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: NULL pu should have been rejected\n");
        cg_close(cgfile);
        return 1;
    }
    printf("NULL pu correctly rejected (error code: %d)\n", result);

    printf("Testing NULL pv parameter (required for 2D)...\n");
    result = cg_solution_interpolation_points_write(cgfile, cgbase, cgfamily, cgsinterp, 9,
                                                    pu, NULL, NULL, NULL);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: NULL pv should have been rejected\n");
        cg_close(cgfile);
        return 1;
    }
    printf("NULL pv correctly rejected (error code: %d)\n", result);

    printf("Testing NULL pw parameter (allowed for 2D)...\n");
    result = cg_solution_interpolation_points_write(cgfile, cgbase, cgfamily, cgsinterp, 9,
                                                    pu, pv, NULL, NULL);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: NULL pw should be allowed for 2D elements\n");
        cg_close(cgfile);
        return 1;
    }
    printf("NULL pw accepted for 2D element (valid)\n");

    cg_close(cgfile);

    printf("\nTEST 7.5 PASSED: NULL Pointer Validation Working\n");
    return 0;
}

/* Test 7.6: re-write guards on the Lagrange/monomial arrays.
 *
 * A second write of LagrangeControlPoints or MonomialCoefficients must be
 * rejected in CG_MODE_WRITE and must replace the existing array in
 * CG_MODE_MODIFY. All four writers are required to behave the same way. */
int test_rewrite_guards(void)
{
    int cgfile, cgbase, cgzone, cgfamily, en, sn;
    cgsize_t size[9];
    double pu[9] = {-1.,0.,1.,-1.,0.,1.,-1.,0.,1.};
    double pv[9] = {-1.,-1.,-1.,0.,0.,0.,1.,1.,1.};
    int result;

    printf("\n==============================================\n");
    printf("  Test 7.6: Re-write Guards (WRITE vs MODIFY)\n");
    printf("==============================================\n\n");

    size[0] = 9; size[1] = 1; size[2] = 0;
    if (cg_open("test_error_rewrite.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size,
                      CGNS_ENUMV(Unstructured), &cgzone) ||
        cg_family_write(cgfile, cgbase, "Fam", &cgfamily))
    {
        fprintf(stderr, "ERROR: could not create base structure\n");
        return 1;
    }

    /* --- ElementInterpolation_t: LagrangeControlPoints --- */
    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "Quad9",
                                      CGNS_ENUMV(QUAD_9), &en))
    {
        fprintf(stderr, "ERROR: element_interpolation_write failed\n");
        cg_close(cgfile); return 1;
    }
    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, en,
                                              pu, pv, NULL))
    {
        fprintf(stderr, "ERROR: first points write failed\n");
        cg_close(cgfile); return 1;
    }
    printf("Testing second element points write in CG_MODE_WRITE...\n");
    result = cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, en,
                                                  pu, pv, NULL);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: duplicate element points write should be "
                        "rejected in CG_MODE_WRITE\n");
        cg_close(cgfile); return 1;
    }
    printf("  correctly rejected (error code: %d)\n", result);

    /* A modal SolutionInterpolation_t stores no array (CPEX-0045: the basis is
     * fixed by element dimension, degrees and Pascal traversal order), so there
     * is no second-write guard to exercise on it. */
    if (cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "Quad_P2_modal",
                                        CGNS_ENUMV(QUAD_4), 2, 0,
                                        CGNS_ENUMV(ParametricMonomialsPascal), &sn))
    {
        fprintf(stderr, "ERROR: solution_interpolation_write failed\n");
        cg_close(cgfile); return 1;
    }
    cg_close(cgfile);

    /* --- CG_MODE_MODIFY must replace rather than reject --- */
    printf("Testing replacement in CG_MODE_MODIFY...\n");
    if (cg_open("test_error_rewrite.cgns", CG_MODE_MODIFY, &cgfile))
    {
        fprintf(stderr, "ERROR: could not reopen in MODIFY\n");
        return 1;
    }
    pu[4] = 0.25;   /* perturb so a successful replace is observable */
    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, en,
                                              pu, pv, NULL))
    {
        fprintf(stderr, "ERROR: points write in CG_MODE_MODIFY should succeed: "
                        "%s\n", cg_get_error());
        cg_close(cgfile); return 1;
    }
    printf("  replacement accepted in CG_MODE_MODIFY\n");
    cg_close(cgfile);

    /* --- and the replacement actually took effect --- */
    if (cg_open("test_error_rewrite.cgns", CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: could not reopen in READ\n");
        return 1;
    }
    {
        double back_u[9], back_v[9];
        if (cg_element_interpolation_points_read(cgfile, cgbase, cgfamily, en,
                                                back_u, back_v, NULL))
        {
            fprintf(stderr, "ERROR: points read failed\n");
            cg_close(cgfile); return 1;
        }
        if (back_u[4] != 0.25)
        {
            fprintf(stderr, "ERROR: MODIFY did not replace points "
                            "(got %g, expected 0.25)\n", back_u[4]);
            cg_close(cgfile); return 1;
        }
    }
    printf("  replaced values verified on read-back\n");
    cg_close(cgfile);

    printf("\nTEST 7.6 PASSED: Re-write Guards Consistent\n");
    return 0;
}

/* Test 7.7: cg_solution_interpolation_find bidirectional lookup.
 *
 * A query must match an exact element tag when one is stored, and otherwise
 * fall back to the basic (linear) tag of the same element family. A triplet
 * that matches neither must report CG_NODE_NOT_FOUND rather than an error.
 *
 * Note that cg_solution_interpolation_write normalises the element type to the
 * basic tag before storing it (sinterp->type = basic type, and the same value
 * goes into the node payload). A high-order tag such as HEXA_27 is therefore
 * never what is on disk, so querying with a high-order tag exercises the
 * fallback while querying with the basic tag exercises the exact match. Both
 * are checked below, and both must resolve to the same node. */
int test_interpolation_find(void)
{
    int cgfile, cgbase, cgzone, cgfamily, sn_hex, sn_tet, found;
    cgsize_t size[9];
    CGNS_ENUMT(InterpolationType_t) it;
    int result;

    printf("\n==============================================\n");
    printf("  Test 7.7: Bidirectional Interpolation Lookup\n");
    printf("==============================================\n\n");

    size[0] = 8; size[1] = 1; size[2] = 0;
    if (cg_open("test_error_find.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size,
                      CGNS_ENUMV(Unstructured), &cgzone) ||
        cg_family_write(cgfile, cgbase, "Fam", &cgfamily))
    {
        fprintf(stderr, "ERROR: could not create base structure\n");
        return 1;
    }

    /* Basic-tag entry (TETRA_4) and an exact high-order entry (HEXA_27). */
    if (cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "Tet_P2",
                                        CGNS_ENUMV(TETRA_4), 2, 0,
                                        CGNS_ENUMV(ParametricLagrange), &sn_tet) ||
        cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "Hex27_P2",
                                        CGNS_ENUMV(HEXA_27), 2, 0,
                                        CGNS_ENUMV(CartesianMonomialsPascal), &sn_hex))
    {
        fprintf(stderr, "ERROR: solution_interpolation_write failed\n");
        cg_close(cgfile); return 1;
    }
    cg_close(cgfile);

    if (cg_open("test_error_find.cgns", CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: could not reopen\n");
        return 1;
    }

    printf("Exact match: query basic tag HEXA_8 (as stored)...\n");
    result = cg_solution_interpolation_find(cgfile, cgbase, cgfamily,
                                           CGNS_ENUMV(HEXA_8), 2, 0, &found, &it);
    if (result != CG_OK)
    {
        /* CG_NODE_NOT_FOUND leaves no message, so report the code. */
        fprintf(stderr, "ERROR: exact HEXA_8 lookup returned %d\n", result);
        cg_close(cgfile); return 1;
    }
    if (found != sn_hex || it != CGNS_ENUMV(CartesianMonomialsPascal))
    {
        fprintf(stderr, "ERROR: exact match returned sn=%d it=%d, "
                        "expected sn=%d CartesianMonomialsPascal\n",
                found, (int)it, sn_hex);
        cg_close(cgfile); return 1;
    }
    printf("  matched index %d (%s)\n", found, cg_InterpolationTypeName(it));

    printf("Fallback: query high-order HEXA_27 -> basic HEXA_8...\n");
    result = cg_solution_interpolation_find(cgfile, cgbase, cgfamily,
                                           CGNS_ENUMV(HEXA_27), 2, 0, &found, &it);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: HEXA_27 should fall back to HEXA_8, "
                        "returned %d\n", result);
        cg_close(cgfile); return 1;
    }
    if (found != sn_hex || it != CGNS_ENUMV(CartesianMonomialsPascal))
    {
        fprintf(stderr, "ERROR: fallback returned sn=%d it=%d, expected sn=%d\n",
                found, (int)it, sn_hex);
        cg_close(cgfile); return 1;
    }
    printf("  resolved to the same node, index %d\n", found);

    printf("Fallback: query TETRA_10 -> basic TETRA_4...\n");
    result = cg_solution_interpolation_find(cgfile, cgbase, cgfamily,
                                           CGNS_ENUMV(TETRA_10), 2, 0, &found, &it);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: TETRA_10 should fall back to TETRA_4, "
                        "returned %d\n", result);
        cg_close(cgfile); return 1;
    }
    if (found != sn_tet || it != CGNS_ENUMV(ParametricLagrange))
    {
        fprintf(stderr, "ERROR: fallback returned sn=%d it=%d, "
                        "expected sn=%d ParametricLagrange\n",
                found, (int)it, sn_tet);
        cg_close(cgfile); return 1;
    }
    printf("  fell back to basic tag, index %d (%s)\n",
           found, cg_InterpolationTypeName(it));

    printf("Order mismatch must not match...\n");
    result = cg_solution_interpolation_find(cgfile, cgbase, cgfamily,
                                           CGNS_ENUMV(TETRA_10), 3, 0,
                                           &found, &it);
    if (result != CG_NODE_NOT_FOUND)
    {
        fprintf(stderr, "ERROR: order 3 should report CG_NODE_NOT_FOUND, "
                        "got %d\n", result);
        cg_close(cgfile); return 1;
    }
    printf("  correctly reported CG_NODE_NOT_FOUND\n");

    printf("Unrelated element family must not match...\n");
    result = cg_solution_interpolation_find(cgfile, cgbase, cgfamily,
                                           CGNS_ENUMV(PENTA_6), 2, 0,
                                           &found, &it);
    if (result != CG_NODE_NOT_FOUND)
    {
        fprintf(stderr, "ERROR: PENTA_6 should report CG_NODE_NOT_FOUND, "
                        "got %d\n", result);
        cg_close(cgfile); return 1;
    }
    printf("  correctly reported CG_NODE_NOT_FOUND\n");
    cg_close(cgfile);

    printf("\nTEST 7.7 PASSED: Bidirectional Lookup Working\n");
    return 0;
}

/* Test 7.8: GridLocation requirements of cg_sol_interpolation_degree_write.
 *
 * InterpolationPoints is always acceptable. Legacy CellCenter is accepted only
 * when an explicit PointRange/PointList is present. Any other location, and
 * CellCenter without a point set, must be rejected. */
int test_interpolation_order_location(void)
{
    int cgfile, cgbase, cgzone, S;
    cgsize_t size[9];
    cgsize_t range[2] = {1, 1};
    int result;

    printf("\n==============================================\n");
    printf("  Test 7.8: InterpolationDegrees GridLocation\n");
    printf("==============================================\n\n");

    size[0] = 8; size[1] = 1; size[2] = 0;
    if (cg_open("test_error_location.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size,
                      CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: could not create base structure\n");
        return 1;
    }

    printf("InterpolationPoints, whole zone (must be accepted)...\n");
    if (cg_sol_write(cgfile, cgbase, cgzone, "FS_ip",
                     CGNS_ENUMV(InterpolationPoints), &S) ||
        cg_sol_interpolation_degree_write(cgfile, cgbase, cgzone, S, 2, 0))
    {
        fprintf(stderr, "ERROR: InterpolationPoints should be accepted: %s\n",
                cg_get_error());
        cg_close(cgfile); return 1;
    }
    printf("  accepted\n");

    printf("Vertex location (must be rejected)...\n");
    if (cg_sol_write(cgfile, cgbase, cgzone, "FS_vertex",
                     CGNS_ENUMV(Vertex), &S))
    {
        fprintf(stderr, "ERROR: sol_write failed\n");
        cg_close(cgfile); return 1;
    }
    result = cg_sol_interpolation_degree_write(cgfile, cgbase, cgzone, S, 2, 0);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: Vertex location should have been rejected\n");
        cg_close(cgfile); return 1;
    }
    printf("  correctly rejected (error code: %d)\n", result);

    printf("CellCenter without a point set (must be rejected)...\n");
    if (cg_sol_write(cgfile, cgbase, cgzone, "FS_cc_nopts",
                     CGNS_ENUMV(CellCenter), &S))
    {
        fprintf(stderr, "ERROR: sol_write failed\n");
        cg_close(cgfile); return 1;
    }
    result = cg_sol_interpolation_degree_write(cgfile, cgbase, cgzone, S, 2, 0);
    if (result == CG_OK)
    {
        fprintf(stderr, "ERROR: CellCenter without PointRange/PointList should "
                        "have been rejected\n");
        cg_close(cgfile); return 1;
    }
    printf("  correctly rejected (error code: %d)\n", result);

    printf("CellCenter with a PointRange (legacy, must be accepted)...\n");
    if (cg_sol_ptset_write(cgfile, cgbase, cgzone, "FS_cc_pts",
                           CGNS_ENUMV(CellCenter), CGNS_ENUMV(PointRange),
                           2, range, &S))
    {
        fprintf(stderr, "ERROR: sol_ptset_write failed: %s\n", cg_get_error());
        cg_close(cgfile); return 1;
    }
    if (cg_sol_interpolation_degree_write(cgfile, cgbase, cgzone, S, 2, 0))
    {
        fprintf(stderr, "ERROR: legacy CellCenter + PointRange should be "
                        "accepted: %s\n", cg_get_error());
        cg_close(cgfile); return 1;
    }
    printf("  accepted (back-compatibility path)\n");

    /* SpatialOrder 0 with TemporalOrder > 0 is valid: a per-element value that is
     * constant in space and varies in time, i.e. an unsteady finite-volume
     * solution, with N_DOFs = q+1 per element.  No constraint couples the two
     * orders, so this must be accepted. */
    printf("TemporalOrder > 0 with SpatialOrder 0 (must be accepted)...\n");
    result = cg_sol_interpolation_degree_write(cgfile, cgbase, cgzone, S, 0, 1);
    if (result != CG_OK)
    {
        fprintf(stderr, "ERROR: constant-in-space, varying-in-time solution "
                        "should have been accepted: %s\n", cg_get_error());
        cg_close(cgfile); return 1;
    }
    printf("  accepted (unsteady finite-volume case)\n");

    cg_close(cgfile);

    printf("\nTEST 7.8 PASSED: GridLocation Rules Enforced\n");
    return 0;
}

/* CPEX-0045: CartesianMonomialsPascal is defined only where the base's
 * CellDimension equals its PhysDim.  On a surface mesh the monomials of the
 * physical frame restricted to the cell are linearly dependent, so the modal
 * cardinality does not describe a determined space. */
int test_cartesian_modal_surface_base(void)
{
    int fn, B, F, si;
    int errors = 0;

    printf("\n=== Test: CartesianMonomialsPascal on a CellDim<PhysDim base ===\n");

    if (cg_open("test_cartesian_surface.cgns", CG_MODE_WRITE, &fn) ||
        cg_base_write(fn, "Base", 2, 3, &B) ||          /* CellDim 2, PhysDim 3 */
        cg_family_write(fn, B, "Fam", &F))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        return 1;
    }

    if (cg_solution_interpolation_write(fn, B, F, "SurfCartesian",
                                        CGNS_ENUMV(QUAD_4), 2, 0,
                                        CGNS_ENUMV(CartesianMonomialsPascal),
                                        &si) == CG_OK)
    {
        fprintf(stderr, "ERROR: CartesianMonomialsPascal should be rejected "
                        "when CellDimension != PhysDim\n");
        errors++;
    }
    else
        printf("CartesianMonomialsPascal correctly rejected on a surface base\n");

    /* The parametric types carry no such restriction: they live in the
     * reference domain, whose dimension is the element dimension. */
    if (cg_solution_interpolation_write(fn, B, F, "SurfParametric",
                                        CGNS_ENUMV(QUAD_4), 2, 0,
                                        CGNS_ENUMV(ParametricMonomialsPascal),
                                        &si))
    {
        fprintf(stderr, "ERROR: ParametricMonomialsPascal should be accepted: %s\n",
                cg_get_error());
        errors++;
    }
    else
        printf("ParametricMonomialsPascal correctly accepted on the same base\n");

    cg_close(fn);
    return errors;
}

/* CPEX-0045: a family-level IsoParametric points read has no element, so where
 * the family carries several geometric orders of one element family the mesh
 * node it refers to is ambiguous and must be reported rather than guessed. */
int test_isoparametric_ambiguous_reference(void)
{
    int fn, B, F, e1, e2, si;
    int errors = 0;
    double pu[35], pv[35], pw[35];
    int i;

    printf("\n=== Test: ambiguous IsoParametric reference ===\n");

    for (i = 0; i < 35; i++) { pu[i] = pv[i] = pw[i] = 0.0; }

    if (cg_open("test_isoparam_ambig.cgns", CG_MODE_WRITE, &fn) ||
        cg_base_write(fn, "Base", 3, 3, &B) ||
        cg_family_write(fn, B, "Fam", &F))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        return 1;
    }

    /* Two geometric orders of the same element family */
    if (cg_element_interpolation_write(fn, B, F, "Tet10", CGNS_ENUMV(TETRA_10), &e1) ||
        cg_element_interpolation_points_write(fn, B, F, e1, pu, pv, pw) ||
        cg_element_interpolation_write(fn, B, F, "Tet35", CGNS_ENUMV(TETRA_35), &e2) ||
        cg_element_interpolation_points_write(fn, B, F, e2, pu, pv, pw))
    {
        fprintf(stderr, "ERROR: Failed to write mesh bases: %s\n", cg_get_error());
        cg_close(fn);
        return 1;
    }

    /* One IsoParametric solution node, stored under the basic tag */
    if (cg_solution_interpolation_write(fn, B, F, "TetIso", CGNS_ENUMV(TETRA_4),
                                        2, 0, CGNS_ENUMV(IsoParametric), &si))
    {
        fprintf(stderr, "ERROR: Failed to write solution basis: %s\n", cg_get_error());
        cg_close(fn);
        return 1;
    }
    cg_close(fn);

    if (cg_open("test_isoparam_ambig.cgns", CG_MODE_READ, &fn))
    {
        fprintf(stderr, "ERROR: Failed to reopen: %s\n", cg_get_error());
        return 1;
    }
    if (cg_solution_interpolation_points_read(fn, B, F, si, pu, pv, pw, NULL) != CG_ERROR)
    {
        fprintf(stderr, "ERROR: an ambiguous IsoParametric reference should be "
                        "reported, not resolved to whichever node comes first\n");
        errors++;
    }
    else
        printf("Ambiguous IsoParametric reference correctly reported\n");

    cg_close(fn);
    return errors;
}

int main(int argc, char **argv)
{
    int errors = 0;

    printf("\n");
    printf("##################################################\n");
    printf("#  CPEX0045 Test: Error Handling & Edge Cases   #\n");
    printf("##################################################\n");

    if (test_invalid_types())
        errors++;

    if (test_mismatched_dimensions())
        errors++;

    if (test_out_of_range_orders())
        errors++;

    if (test_duplicate_nodes())
        errors++;

    if (test_null_pointers())
        errors++;

    if (test_rewrite_guards())
        errors++;

    if (test_interpolation_find())
        errors++;

    if (test_interpolation_order_location())
        errors++;

    if (test_cartesian_modal_surface_base())
        errors++;

    if (test_isoparametric_ambiguous_reference())
        errors++;

    printf("\n");
    printf("##################################################\n");
    if (errors == 0)
    {
        printf("#ALL ERROR HANDLING TESTS PASSED (10/10)   #\n");
    }
    else
    {
        printf("#  ✗ FAILURES: %d test(s) failed                 #\n", errors);
    }
    printf("##################################################\n");
    printf("\n");

    return (errors == 0) ? 0 : 1;
}
