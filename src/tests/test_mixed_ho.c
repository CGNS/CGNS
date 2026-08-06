/**
 * Test file for MIXED element sections with high-order solutions
 *
 * Tests:
 * 1. test_mixed_ho_simple() - MIXED section with TRI_3 and QUAD_4 elements
 * 2. test_mixed_ho_complex() - MIXED section with multiple element types
 * 3. Verify high-order solution data sizes are computed correctly
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"
#include "utils.h"

int total_tests = 0;
int failed_tests = 0;

/**
 * Test 1: Simple MIXED Section (TRI_3 + QUAD_4) with High-Order Solution
 *
 * Creates a MIXED section with triangles and quads, then writes a high-order
 * solution (order 2) with GridLocation=InterpolationPoints.
 *
 * Expected data sizes:
 * - TRI_3 at order 2: 6 points per element
 * - QUAD_4 at order 2: 9 points per element
 */
int test_mixed_ho_simple()
{
    int fn, bn, zn, sn, en, soln;
    int ier;
    char filename[64];
    cgsize_t sizes[3];
    cgsize_t connectivity[100];
    cgsize_t pos = 0;
    int i;

    printf("\n=== Test 1: Simple MIXED Section (TRI + QUAD) with HO Solution ===\n");
    total_tests++;

    sprintf(filename, "test_mixed_ho_simple.cgns");

    /* Create CGNS file */
    if (cg_open(filename, CG_MODE_WRITE, &fn)) {
        printf("ERROR: Failed to create file: %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    /* Create base (2D) */
    sizes[0] = 8;  /* 8 vertices total */
    sizes[1] = 3;  /* 3 cells (2 TRI + 1 QUAD) */
    sizes[2] = 0;

    if (cg_base_write(fn, "Base", 2, 2, &bn)) {
        printf("ERROR: Failed to create base: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    /* Create zone */
    if (cg_zone_write(fn, bn, "Zone", sizes, CGNS_ENUMV(Unstructured), &zn)) {
        printf("ERROR: Failed to create zone: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    /* Write grid coordinates (8 points) */
    double x[8] = {0.0, 1.0, 2.0, 0.0, 1.0, 2.0, 0.5, 1.5};
    double y[8] = {0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.5, 1.5};
    double z[8] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

    if (cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateX", x, &sn) ||
        cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateY", y, &sn) ||
        cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateZ", z, &sn)) {
        printf("ERROR: Failed to write coordinates: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    /* Create MIXED connectivity:
     * Element 1 (TRI_3): nodes 1,2,4
     * Element 2 (TRI_3): nodes 2,5,4
     * Element 3 (QUAD_4): nodes 4,5,6,7
     */
    pos = 0;
    /* TRI 1 */
    connectivity[pos++] = CGNS_ENUMV(TRI_3);
    connectivity[pos++] = 1; connectivity[pos++] = 2; connectivity[pos++] = 4;
    /* TRI 2 */
    connectivity[pos++] = CGNS_ENUMV(TRI_3);
    connectivity[pos++] = 2; connectivity[pos++] = 5; connectivity[pos++] = 4;
    /* QUAD 1 */
    connectivity[pos++] = CGNS_ENUMV(QUAD_4);
    connectivity[pos++] = 4; connectivity[pos++] = 5;
    connectivity[pos++] = 6; connectivity[pos++] = 7;

    /* Create offset array for MIXED section:
     * offset[0] = 0 (start of first element)
     * offset[1] = 4 (start of second element: type + 3 nodes)
     * offset[2] = 8 (start of third element: type + 3 nodes)
     * offset[3] = 13 (end: type + 4 nodes)
     */
    cgsize_t offsets[4] = {0, 4, 8, 13};

    /* Write MIXED section using poly_section_write */
    if (cg_poly_section_write(fn, bn, zn, "MixedElements", CGNS_ENUMV(MIXED),
                              1, 3, 0, connectivity, offsets, &en)) {
        printf("ERROR: Failed to write MIXED section: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  Created MIXED section with 2 TRI_3 + 1 QUAD_4\n");

    /* Create high-order solution (order 2, temporal 0) */
    int spatial_degree = 2;
    int temporal_degree = 0;

    /* A heterogeneous block needs one SolutionInterpolation_t per element
     * family present in the section: N_DOFs is resolved per element, so both
     * TRI_3 and QUAD_4 must declare their basis.  Declaring them also makes
     * the field length well defined -- the library derives it from these
     * nodes rather than assuming a cardinality. */
    {
        int fam, si;
        /* Equidistant control points on the bi-unit reference domains.
         * Positions are the writer's responsibility; only the count matters
         * to the field-length rule. */
        double tri_u[6] = {-1.0,  0.0,  1.0, -1.0,  0.0, -1.0};
        double tri_v[6] = {-1.0, -1.0, -1.0,  0.0,  0.0,  1.0};
        double quad_u[9] = {-1.0, 0.0, 1.0, -1.0, 0.0, 1.0, -1.0, 0.0, 1.0};
        double quad_v[9] = {-1.0,-1.0,-1.0,  0.0, 0.0, 0.0,  1.0, 1.0, 1.0};

        if (cg_family_write(fn, bn, "MixedFam", &fam)) {
            printf("ERROR: Failed to write family: %s\n", cg_get_error());
            cg_close(fn); failed_tests++; return 1;
        }
        if (cg_solution_interpolation_write(fn, bn, fam, "Tri_P2",
                CGNS_ENUMV(TRI_3), spatial_degree, temporal_degree,
                CGNS_ENUMV(ParametricLagrange), &si) ||
            cg_solution_interpolation_points_write(fn, bn, fam, si,
                tri_u, tri_v, NULL, NULL)) {
            printf("ERROR: Failed to write TRI interpolation: %s\n", cg_get_error());
            cg_close(fn); failed_tests++; return 1;
        }
        if (cg_solution_interpolation_write(fn, bn, fam, "Quad_P2",
                CGNS_ENUMV(QUAD_4), spatial_degree, temporal_degree,
                CGNS_ENUMV(ParametricLagrange), &si) ||
            cg_solution_interpolation_points_write(fn, bn, fam, si,
                quad_u, quad_v, NULL, NULL)) {
            printf("ERROR: Failed to write QUAD interpolation: %s\n", cg_get_error());
            cg_close(fn); failed_tests++; return 1;
        }
        if (cg_goto(fn, bn, "Zone_t", zn, NULL) ||
            cg_famname_write("MixedFam")) {
            printf("ERROR: Failed to attach family to zone: %s\n", cg_get_error());
            cg_close(fn); failed_tests++; return 1;
        }
    }

    if (cg_sol_write(fn, bn, zn, "HighOrderSolution", CGNS_ENUMV(InterpolationPoints), &soln)) {
        printf("ERROR: Failed to create solution: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    /* Set interpolation orders */
    if (cg_sol_interpolation_degree_write(fn, bn, zn, soln, spatial_degree, temporal_degree)) {
        printf("ERROR: Failed to set interpolation orders: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    /* Calculate expected size:
     * - 2 TRI_3 elements at order 2: 2 * 6 = 12 points
     * - 1 QUAD_4 element at order 2: 1 * 9 = 9 points
     * - Total: 21 points
     */
    cgsize_t expected_size = 21;

    /* Write a test field */
    double *field_data = malloc(expected_size * sizeof(double));
    for (i = 0; i < expected_size; i++) {
        field_data[i] = (double)i;
    }

    if (cg_field_write(fn, bn, zn, soln, CGNS_ENUMV(RealDouble),
                       "Density", field_data, &sn)) {
        printf("ERROR: Failed to write field: %s\n", cg_get_error());
        free(field_data);
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    free(field_data);
    printf("  Successfully wrote high-order solution field (size=%ld)\n", (long)expected_size);

    /* Close and reopen to test reading */
    if (cg_close(fn)) {
        printf("ERROR: Failed to close file: %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    /* Reopen and verify */
    if (cg_open(filename, CG_MODE_READ, &fn)) {
        printf("ERROR: Failed to reopen file: %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    /* Read solution and verify size */
    int nfields;
    char fieldname[33];
    CGNS_ENUMT(DataType_t) datatype;

    if (cg_nfields(fn, bn, zn, soln, &nfields)) {
        printf("ERROR: Failed to read nfields: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    if (nfields != 1) {
        printf("ERROR: Expected 1 field, got %d\n", nfields);
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    /* The array length is the thing under test: in a heterogeneous section the
     * library must resolve N_DOFs per element from the matching
     * SolutionInterpolation_t and sum it, giving 2*6 + 1*9 = 21.  A section
     * sized as if every element were a TRI (18) or a QUAD (27) would be a
     * silent corruption, so read the length off disk rather than inferring it
     * from the write having succeeded. */
    {
        int ndim;
        cgsize_t dimv[3];

        if (cg_goto(fn, bn, "Zone_t", zn, "FlowSolution_t", soln, NULL) ||
            cg_array_info(1, fieldname, &datatype, &ndim, dimv)) {
            printf("ERROR: Failed to read field array info: %s\n", cg_get_error());
            cg_close(fn);
            failed_tests++;
            return 1;
        }
        if (strcmp(fieldname, "Density")) {
            printf("ERROR: Expected field 'Density', got '%s'\n", fieldname);
            cg_close(fn);
            failed_tests++;
            return 1;
        }
        if (ndim != 1 || dimv[0] != expected_size) {
            printf("ERROR: field length on disk is %d, expected %ld "
                   "(2 TRI_3 x 6 DOFs + 1 QUAD_4 x 9 DOFs)\n",
                   (int)dimv[0], (long)expected_size);
            cg_close(fn);
            failed_tests++;
            return 1;
        }
        printf("  Field length on disk: %d (2x6 + 1x9)\n", (int)dimv[0]);
    }

    /* Read back the field */
    cgsize_t range_min[1] = {1};
    cgsize_t range_max[1] = {expected_size};
    field_data = malloc(expected_size * sizeof(double));
    for (i = 0; i < expected_size; i++) {
        field_data[i] = -1.0;
    }
    if (cg_field_read(fn, bn, zn, soln, "Density", CGNS_ENUMV(RealDouble),
                      range_min, range_max, field_data)) {
        printf("ERROR: Failed to read field: %s\n", cg_get_error());
        free(field_data);
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    for (i = 0; i < expected_size; i++) {
        if (field_data[i] != (double)i) {
            printf("ERROR: field value %d is %g, expected %g\n",
                   i, field_data[i], (double)i);
            free(field_data);
            cg_close(fn);
            failed_tests++;
            return 1;
        }
    }
    printf("  All %ld field values round-tripped\n", (long)expected_size);

    free(field_data);

    /* The two SolutionInterpolation_t nodes the length was derived from must
     * themselves come back as written -- the field length is only meaningful if
     * the bases behind it are. */
    {
        struct { const char *name; CGNS_ENUMT(ElementType_t) type; int npts; }
        want[2] = { { "Tri_P2",  CGNS_ENUMV(TRI_3),  6 },
                    { "Quad_P2", CGNS_ENUMV(QUAD_4), 9 } };
        int k;
        int nsi = 0;

        if (cg_nsolution_interpolation_read(fn, bn, 1, &nsi)) {
            printf("ERROR: Failed to count SolutionInterpolation_t: %s\n",
                   cg_get_error());
            cg_close(fn);
            failed_tests++;
            return 1;
        }
        if (nsi != 2) {
            printf("ERROR: Expected 2 SolutionInterpolation_t nodes, got %d\n", nsi);
            cg_close(fn);
            failed_tests++;
            return 1;
        }

        for (k = 0; k < 2; k++) {
            char siname[33];
            CGNS_ENUMT(ElementType_t) et;
            CGNS_ENUMT(InterpolationType_t) it;
            int os, ot, npts = 0;

            if (cg_solution_interpolation_read(fn, bn, 1, k + 1, siname, &et,
                                               &os, &ot, &it)) {
                printf("ERROR: Failed to read SolutionInterpolation_t %d: %s\n",
                       k + 1, cg_get_error());
                cg_close(fn);
                failed_tests++;
                return 1;
            }
            if (strcmp(siname, want[k].name) || et != want[k].type ||
                os != spatial_degree || ot != temporal_degree ||
                it != CGNS_ENUMV(ParametricLagrange)) {
                printf("ERROR: SolutionInterpolation_t %d is (%s,%s,%d,%d,%s), "
                       "expected (%s,%s,%d,%d,ParametricLagrange)\n",
                       k + 1, siname, cg_ElementTypeName(et), os, ot,
                       cg_InterpolationTypeName(it), want[k].name,
                       cg_ElementTypeName(want[k].type),
                       spatial_degree, temporal_degree);
                cg_close(fn);
                failed_tests++;
                return 1;
            }
            if (cg_solution_lagrange_interpolation_size(et, os, ot, &npts) ||
                npts != want[k].npts) {
                printf("ERROR: %s at degree %d should have %d DOFs, got %d\n",
                       cg_ElementTypeName(et), os, want[k].npts, npts);
                cg_close(fn);
                failed_tests++;
                return 1;
            }
            printf("  %s: %s degree %d -> %d DOFs\n",
                   siname, cg_ElementTypeName(et), os, npts);
        }
    }

    cg_close(fn);

    printf("  ✓ Test PASSED: MIXED section with high-order solution\n");
    return 0;
}

int main()
{
    printf("\n");
    printf("========================================\n");
    printf("Testing MIXED Elements with High-Order Solutions\n");
    printf("========================================\n");

    test_mixed_ho_simple();

    printf("\n========================================\n");
    printf("Test Summary\n");
    printf("========================================\n");
    printf("Total tests: %d\n", total_tests);
    printf("Passed:      %d\n", total_tests - failed_tests);
    printf("Failed:      %d\n", failed_tests);
    printf("========================================\n\n");

    return (failed_tests > 0) ? 1 : 0;
}
