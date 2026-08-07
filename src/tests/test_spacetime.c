#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"

/* Test 5.1: Temporal Order 1 (Linear Time)
 * Tests space-time interpolation with linear temporal component
 * QUAD_9 element with spatial order 2, temporal order 1
 * Uses SolutionInterpolation_t with InterpolationDegrees = [2, 2, 1]
 * LagrangeControlPoints: (3+1) × ((2+1)^2 × (1+1)) = 4 × 18 array
 */
int test_temporal_degree1(void)
{
    int i, j, n;
    double *pu, *pv, *pw, *pt, *pu_read, *pv_read, *pt_read;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) type = CGNS_ENUMV(QUAD_9);
    CGNS_ENUMT(ElementType_t) etyperead;
    CGNS_ENUMT(InterpolationType_t) itype_read;
    int cgfile, cgbase, cgzone, cgfamily, cgsinterp;
    char sinterpName[33], familyname[33];
    const char *filename = "test_spacetime_order1.cgns";
    int spatial_degree = 2, temporal_degree = 1;
    int os_read, ot_read;
    cgsize_t npts_spatial;
    int npts_total;
    int failed_points = 0;

    printf("\n==============================================\n");
    printf("  Test 5.1: Temporal Order 1 (Linear Time)\n");
    printf("==============================================\n\n");

    printf("Testing QUAD_9 with SolutionInterpolation_t\n");
    printf("  InterpolationDegrees = [%d, %d, %d] (spatial_u, spatial_v, temporal)\n",
           spatial_degree, spatial_degree, temporal_degree);

    /* Calculate the number of Lagrange control points */
    if (cg_solution_lagrange_interpolation_size(type, spatial_degree, temporal_degree, &npts_total))
    {
        fprintf(stderr, "ERROR: Failed to get solution interpolation size\n");
        return 1;
    }
    printf("  Total Lagrange control points: %d\n", npts_total);

    /* Simple 2D structured grid size (metadata only) */
    size[0] = 9;   /* vertex size */
    size[1] = 1;   /* cell size (1 element) */
    size[2] = 0;   /* boundary vertex size */

    /* ========================================================================
     *                              WRITE MODE
     * ======================================================================== */

    printf("\nCreating CGNS file %s...\n", filename);
    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        return 1;
    }

    printf("Writing family node...\n");
    if (cg_family_write(cgfile, cgbase, "SpaceTimeFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        return 1;
    }

    printf("Writing SolutionInterpolation_t node with space-time support...\n");
    if (cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "SpaceTimeInterpolation",
                                        type, spatial_degree, temporal_degree,
                                        CGNS_ENUMV(ParametricLagrange), &cgsinterp))
    {
        fprintf(stderr, "ERROR: Failed to write SolutionInterpolation_t node\n");
        return 1;
    }
    printf("SolutionInterpolation_t node created (index=%d)\n", cgsinterp);

    /* Allocate and fill control points for space-time
     * For QUAD_9 with spatial order 2, temporal order 1:
     * - Spatial: 3×3 = 9 points per time level
     * - Temporal: 2 time levels (order 1)
     * - Total: 9 × 2 = 18 points
     */
    pu = (double*) malloc(npts_total * sizeof(double));
    pv = (double*) malloc(npts_total * sizeof(double));
    pw = (double*) malloc(npts_total * sizeof(double));  /* Set to 0 for 2D */
    pt = (double*) malloc(npts_total * sizeof(double));  /* Temporal coordinate */

    /* QUAD_9 spatial parametric coordinates (standard 3x3 grid from -1 to 1) */
    double u_spatial[9] = {-1.0, 0.0, 1.0, -1.0, 0.0, 1.0, -1.0, 0.0, 1.0};
    double v_spatial[9] = {-1.0, -1.0, -1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0};

    /* Temporal parametric coordinates: t=0 and t=1 (linear) */
    double t_temporal[2] = {0.0, 1.0};

    /* Fill arrays: outer loop over time, inner loop over space */
    int idx = 0;
    for (int t = 0; t < 2; t++)  /* 2 temporal levels for order 1 */
    {
        for (int s = 0; s < 9; s++)  /* 9 spatial points for QUAD_9 */
        {
            pu[idx] = u_spatial[s];
            pv[idx] = v_spatial[s];
            pw[idx] = 0.0;  /* 2D element */
            pt[idx] = t_temporal[t];
            idx++;
        }
    }

    printf("Writing space-time Lagrange control points (%d total points)...\n", npts_total);
    printf("  Format: 9 spatial nodes × 2 temporal levels\n");
    /* For 2D elements with temporal dimension, pass NULL for pw and pt as 4th parameter */
    if (cg_solution_interpolation_points_write(cgfile, cgbase, cgfamily, cgsinterp,
                                               npts_total, pu, pv, NULL, pt))
    {
        fprintf(stderr, "ERROR: Failed to write Lagrange control points\n");
        return 1;
    }
    printf("Written %d control points\n", npts_total);

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
    if (cg_family_read(cgfile, cgbase, cgfamily, familyname, &i, &n))
    {
        fprintf(stderr, "ERROR: Failed to read family\n");
        return 1;
    }
    printf("Family name: %s\n", familyname);

    /* Check SolutionInterpolation_t count */
    {
        int nsi = 0;
        if (cg_nsolution_interpolation_read(cgfile, cgbase, cgfamily, &nsi))
        {
            fprintf(stderr, "ERROR: Cannot count SolutionInterpolation_t nodes: %s\n",
                    cg_get_error());
            return 1;
        }
        if (nsi != 1)
        {
            fprintf(stderr, "ERROR: Expected 1 SolutionInterpolation_t node, got %d\n",
                    nsi);
            return 1;
        }
        printf("SolutionInterpolation_t node count: %d\n", nsi);
    }

    /* Read SolutionInterpolation_t node */
    printf("Reading SolutionInterpolation_t properties...\n");
    if (cg_solution_interpolation_read(cgfile, cgbase, cgfamily, cgsinterp,
                                       sinterpName, &etyperead, &os_read, &ot_read, &itype_read))
    {
        fprintf(stderr, "ERROR: Cannot read SolutionInterpolation_t node\n");
        return 1;
    }
    printf("Interpolation name: %s\n", sinterpName);

    /* NOTE: SolutionInterpolation_t stores the basic element type (e.g., QUAD_4)
     * rather than the high-order type (e.g., QUAD_9). The actual order is in
     * the spatialdegree field. */
    CGNS_ENUMT(ElementType_t) basic_type;
    cg_element_basic_element_type(type, &basic_type);
    if (etyperead != basic_type)
    {
        fprintf(stderr, "ERROR: Wrong element type (expected %s, got %s)\n",
                cg_ElementTypeName(basic_type), cg_ElementTypeName(etyperead));
        return 1;
    }
    printf("Element type: %s\n", cg_ElementTypeName(etyperead));

    if (os_read != spatial_degree || ot_read != temporal_degree)
    {
        fprintf(stderr, "ERROR: Wrong interpolation orders (expected [%d, %d], got [%d, %d])\n",
                spatial_degree, temporal_degree, os_read, ot_read);
        return 1;
    }
    printf("Interpolation orders: spatial=%d, temporal=%d\n", os_read, ot_read);

    /* Check control point size */
    printf("Validating Lagrange control point dimensions...\n");
    int size_check;
    if (cg_solution_lagrange_interpolation_size(etyperead, os_read, ot_read, &size_check))
    {
        fprintf(stderr, "ERROR: Failed to get interpolation size\n");
        return 1;
    }

    if (size_check != npts_total)
    {
        fprintf(stderr, "ERROR: Expected %d control points, got %d\n", npts_total, size_check);
        return 1;
    }
    printf("Expected %d control points\n", size_check);

    /* Allocate arrays for reading */
    pu_read = (double*) malloc(npts_total * sizeof(double));
    pv_read = (double*) malloc(npts_total * sizeof(double));
    pt_read = (double*) malloc(npts_total * sizeof(double));

    /* Read control points */
    printf("Reading space-time Lagrange control points...\n");
    /* For 2D elements with temporal dimension, read NULL for pw and pt from 4th parameter */
    if (cg_solution_interpolation_points_read(cgfile, cgbase, cgfamily, cgsinterp,
                                              pu_read, pv_read, NULL, pt_read))
    {
        fprintf(stderr, "ERROR: Cannot read Lagrange control points\n");
        return 1;
    }

    /* Validate control points */
    printf("Validating control point coordinates...\n");
    failed_points = 0;
    for (i = 0; i < npts_total; i++)
    {
        if (fabs(pu[i] - pu_read[i]) > 1.e-12 ||
            fabs(pv[i] - pv_read[i]) > 1.e-12 ||
            fabs(pt[i] - pt_read[i]) > 1.e-12)
        {
            fprintf(stderr, "ERROR: Control point %d mismatch\n", i);
            fprintf(stderr, "  Expected: (u=%f, v=%f, t=%f)\n", pu[i], pv[i], pt[i]);
            fprintf(stderr, "  Got:      (u=%f, v=%f, t=%f)\n", pu_read[i], pv_read[i], pt_read[i]);
            failed_points++;
        }
    }

    if (failed_points > 0)
    {
        fprintf(stderr, "ERROR: %d control points failed validation\n", failed_points);
        cg_close(cgfile);
        free(pu); free(pv); free(pw); free(pt);
        free(pu_read); free(pv_read); free(pt_read);
        return 1;
    }
    printf("All %d control points validated successfully\n", npts_total);

    /* Verify key space-time points */
    printf("Verifying key space-time control points...\n");

    /* First spatial node at t=0: (-1, -1, t=0) */
    if (fabs(pu_read[0] + 1.0) > 1.e-12 || fabs(pv_read[0] + 1.0) > 1.e-12 || fabs(pt_read[0]) > 1.e-12)
    {
        fprintf(stderr, "ERROR: First point at t=0 should be (-1, -1, 0)\n");
        cg_close(cgfile);
        free(pu); free(pv); free(pw); free(pt);
        free(pu_read); free(pv_read); free(pt_read);
        return 1;
    }
    printf("Point 0 (corner at t=0): (u=%g, v=%g, t=%g)\n", pu_read[0], pv_read[0], pt_read[0]);

    /* Center spatial node at t=0: (0, 0, t=0) */
    int center_t0 = 4;  /* Node 4 is center of QUAD_9 */
    if (fabs(pu_read[center_t0]) > 1.e-12 || fabs(pv_read[center_t0]) > 1.e-12 || fabs(pt_read[center_t0]) > 1.e-12)
    {
        fprintf(stderr, "ERROR: Center point at t=0 should be (0, 0, 0)\n");
        cg_close(cgfile);
        free(pu); free(pv); free(pw); free(pt);
        free(pu_read); free(pv_read); free(pt_read);
        return 1;
    }
    printf("Point %d (center at t=0): (u=%g, v=%g, t=%g)\n", center_t0, pu_read[center_t0], pv_read[center_t0], pt_read[center_t0]);

    /* First spatial node at t=1: (-1, -1, t=1) */
    int first_t1 = 9;  /* Start of second temporal level */
    if (fabs(pu_read[first_t1] + 1.0) > 1.e-12 || fabs(pv_read[first_t1] + 1.0) > 1.e-12 || fabs(pt_read[first_t1] - 1.0) > 1.e-12)
    {
        fprintf(stderr, "ERROR: First point at t=1 should be (-1, -1, 1)\n");
        cg_close(cgfile);
        free(pu); free(pv); free(pw); free(pt);
        free(pu_read); free(pv_read); free(pt_read);
        return 1;
    }
    printf("Point %d (corner at t=1): (u=%g, v=%g, t=%g)\n", first_t1, pu_read[first_t1], pv_read[first_t1], pt_read[first_t1]);

    /* Center spatial node at t=1: (0, 0, t=1) */
    int center_t1 = 9 + 4;
    if (fabs(pu_read[center_t1]) > 1.e-12 || fabs(pv_read[center_t1]) > 1.e-12 || fabs(pt_read[center_t1] - 1.0) > 1.e-12)
    {
        fprintf(stderr, "ERROR: Center point at t=1 should be (0, 0, 1)\n");
        cg_close(cgfile);
        free(pu); free(pv); free(pw); free(pt);
        free(pu_read); free(pv_read); free(pt_read);
        return 1;
    }
    printf("Point %d (center at t=1): (u=%g, v=%g, t=%g)\n", center_t1, pu_read[center_t1], pv_read[center_t1], pt_read[center_t1]);

    printf("Closing file...\n");
    cg_close(cgfile);

    free(pu); free(pv); free(pw); free(pt);
    free(pu_read); free(pv_read); free(pt_read);

    printf("\nTEST 5.1 PASSED: Temporal Order 1 (Linear Time)\n");
    return 0;
}

/* Test 5.2: Higher Temporal Orders
 * Tests quadratic and cubic temporal interpolation
 * QUAD_9 element with spatial order 2, temporal orders 2 and 3
 */
int test_temporal_higher_orders(void)
{
    int i, j, n;
    double *pu, *pv, *pw, *pt, *pu_read, *pv_read, *pt_read;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) type = CGNS_ENUMV(QUAD_9);
    CGNS_ENUMT(ElementType_t) etyperead;
    CGNS_ENUMT(InterpolationType_t) itype_read;
    int cgfile, cgbase, cgzone, cgfamily, cgsinterp;
    char sinterpName[33], familyname[33];
    char filename[64];
    int spatial_degree = 2;
    int os_read, ot_read;
    int npts_total;
    int failed_points;

    /* Test both quadratic (order 2) and cubic (order 3) temporal interpolation */
    int temporal_degrees[2] = {2, 3};
    const char *order_names[2] = {"Quadratic", "Cubic"};

    for (int test_idx = 0; test_idx < 2; test_idx++)
    {
        int temporal_degree = temporal_degrees[test_idx];
        int npts_temporal = temporal_degree + 1;  /* Order 2 -> 3 levels, Order 3 -> 4 levels */

        printf("\n==============================================\n");
        printf("  Test 5.2.%d: %s Temporal Interpolation\n", test_idx + 1, order_names[test_idx]);
        printf("==============================================\n\n");

        printf("Testing QUAD_9 with SolutionInterpolation_t\n");
        printf("  InterpolationDegrees = [%d, %d, %d] (spatial_u, spatial_v, temporal)\n",
               spatial_degree, spatial_degree, temporal_degree);

        /* Calculate the number of Lagrange control points */
        if (cg_solution_lagrange_interpolation_size(type, spatial_degree, temporal_degree, &npts_total))
        {
            fprintf(stderr, "ERROR: Failed to get solution interpolation size\n");
            return 1;
        }
        printf("  Total Lagrange control points: %d\n", npts_total);
        printf("  (9 spatial × %d temporal levels)\n", npts_temporal);

        snprintf(filename, sizeof(filename), "test_spacetime_order%d.cgns", temporal_degree);

        /* Simple 2D structured grid size (metadata only) */
        size[0] = 9;   /* vertex size */
        size[1] = 1;   /* cell size (1 element) */
        size[2] = 0;   /* boundary vertex size */

        /* ========================================================================
         *                              WRITE MODE
         * ======================================================================== */

        printf("\nCreating CGNS file %s...\n", filename);
        if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
            cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
            cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
        {
            fprintf(stderr, "ERROR: Failed to create file structure\n");
            return 1;
        }

        printf("Writing family node...\n");
        if (cg_family_write(cgfile, cgbase, "SpaceTimeFamily", &cgfamily))
        {
            fprintf(stderr, "ERROR: Failed to write Family_t node\n");
            return 1;
        }

        printf("Writing SolutionInterpolation_t node with %s time...\n", order_names[test_idx]);
        if (cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "SpaceTimeInterpolation",
                                            type, spatial_degree, temporal_degree,
                                            CGNS_ENUMV(ParametricLagrange), &cgsinterp))
        {
            fprintf(stderr, "ERROR: Failed to write SolutionInterpolation_t node\n");
            return 1;
        }
        printf("SolutionInterpolation_t node created (index=%d)\n", cgsinterp);

        /* Allocate and fill control points */
        pu = (double*) malloc(npts_total * sizeof(double));
        pv = (double*) malloc(npts_total * sizeof(double));
        pw = (double*) malloc(npts_total * sizeof(double));
        pt = (double*) malloc(npts_total * sizeof(double));

        /* QUAD_9 spatial parametric coordinates */
        double u_spatial[9] = {-1.0, 0.0, 1.0, -1.0, 0.0, 1.0, -1.0, 0.0, 1.0};
        double v_spatial[9] = {-1.0, -1.0, -1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0};

        /* Fill arrays: outer loop over time, inner loop over space */
        int idx = 0;
        for (int t = 0; t < npts_temporal; t++)
        {
            double t_val = (double)t / (double)temporal_degree;
            for (int s = 0; s < 9; s++)  /* 9 spatial points for QUAD_9 */
            {
                pu[idx] = u_spatial[s];
                pv[idx] = v_spatial[s];
                pw[idx] = 0.0;  /* 2D element */
                pt[idx] = t_val;
                idx++;
            }
        }

        printf("Writing space-time Lagrange control points (%d total points)...\n", npts_total);
        /* For 2D elements with temporal dimension, pass NULL for pw and pt as 4th parameter */
        if (cg_solution_interpolation_points_write(cgfile, cgbase, cgfamily, cgsinterp,
                                                   npts_total, pu, pv, NULL, pt))
        {
            fprintf(stderr, "ERROR: Failed to write Lagrange control points\n");
            return 1;
        }
        printf("Written %d control points\n", npts_total);

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
        if (cg_family_read(cgfile, cgbase, cgfamily, familyname, &i, &n))
        {
            fprintf(stderr, "ERROR: Failed to read family\n");
            return 1;
        }
        printf("Family name: %s\n", familyname);

        /* Read SolutionInterpolation_t node */
        printf("Reading SolutionInterpolation_t properties...\n");
        if (cg_solution_interpolation_read(cgfile, cgbase, cgfamily, cgsinterp,
                                           sinterpName, &etyperead, &os_read, &ot_read, &itype_read))
        {
            fprintf(stderr, "ERROR: Cannot read SolutionInterpolation_t node\n");
            return 1;
        }

        if (os_read != spatial_degree || ot_read != temporal_degree)
        {
            fprintf(stderr, "ERROR: Wrong interpolation orders (expected [%d, %d], got [%d, %d])\n",
                    spatial_degree, temporal_degree, os_read, ot_read);
            return 1;
        }
        printf("Interpolation orders: spatial=%d, temporal=%d\n", os_read, ot_read);

        /* Allocate arrays for reading */
        pu_read = (double*) malloc(npts_total * sizeof(double));
        pv_read = (double*) malloc(npts_total * sizeof(double));
        pt_read = (double*) malloc(npts_total * sizeof(double));

        /* Read control points */
        printf("Reading space-time Lagrange control points...\n");
        /* For 2D elements with temporal dimension, read NULL for pw and pt from 4th parameter */
        if (cg_solution_interpolation_points_read(cgfile, cgbase, cgfamily, cgsinterp,
                                                  pu_read, pv_read, NULL, pt_read))
        {
            fprintf(stderr, "ERROR: Cannot read Lagrange control points\n");
            return 1;
        }

        /* Validate control points */
        printf("Validating control point coordinates...\n");
        failed_points = 0;
        for (i = 0; i < npts_total; i++)
        {
            if (fabs(pu[i] - pu_read[i]) > 1.e-12 ||
                fabs(pv[i] - pv_read[i]) > 1.e-12 ||
                fabs(pt[i] - pt_read[i]) > 1.e-12)
            {
                fprintf(stderr, "ERROR: Control point %d mismatch\n", i);
                fprintf(stderr, "  Expected: (u=%f, v=%f, t=%f)\n", pu[i], pv[i], pt[i]);
                fprintf(stderr, "  Got:      (u=%f, v=%f, t=%f)\n", pu_read[i], pv_read[i], pt_read[i]);
                failed_points++;
            }
        }

        if (failed_points > 0)
        {
            fprintf(stderr, "ERROR: %d control points failed validation\n", failed_points);
            cg_close(cgfile);
            free(pu); free(pv); free(pw); free(pt);
            free(pu_read); free(pv_read); free(pt_read);
            return 1;
        }
        printf("All %d control points validated successfully\n", npts_total);

        /* Verify temporal progression */
        printf("Verifying temporal coordinate progression...\n");
        for (int t = 0; t < npts_temporal; t++)
        {
            /* Check first spatial node at each time level */
            int idx_check = t * 9;  /* 9 spatial points per time level */
            double expected_time = (double)t / (double)temporal_degree;

            if (fabs(pt_read[idx_check] - expected_time) > 1.e-12)
            {
                fprintf(stderr, "ERROR: Temporal coordinate at level %d incorrect\n", t);
                fprintf(stderr, "  Expected: %f, Got: %f\n", expected_time, pt_read[idx_check]);
                cg_close(cgfile);
                free(pu); free(pv); free(pw); free(pt);
                free(pu_read); free(pv_read); free(pt_read);
                return 1;
            }
            printf("Time level %d: t = %g\n", t, pt_read[idx_check]);
        }

        printf("Closing file...\n");
        cg_close(cgfile);

        free(pu); free(pv); free(pw); free(pt);
        free(pu_read); free(pv_read); free(pt_read);

        printf("\nTEST 5.2.%d PASSED: %s Temporal Interpolation\n", test_idx + 1, order_names[test_idx]);
    }

    return 0;
}

int main(int argc, char **argv)
{
    int errors = 0;

    printf("\n");
    printf("##################################################\n");
    printf("#  CPEX0045 Test: Space-Time Interpolation      #\n");
    printf("##################################################\n");

    /* Test 5.1: Temporal Order 1 (Linear Time) */
    if (test_temporal_degree1())
        errors++;

    /* Test 5.2: Higher Temporal Orders */
    if (test_temporal_higher_orders())
        errors++;

    printf("\n");
    printf("##################################################\n");
    if (errors == 0)
    {
        printf("#ALL SPACE-TIME TESTS PASSED (3/3)         #\n");
    }
    else
    {
        printf("#  ✗ FAILURES: %d test(s) failed                #\n", errors);
    }
    printf("##################################################\n");
    printf("\n");

    return errors;
}
