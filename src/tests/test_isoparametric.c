/**
 * Test file for CPEX0045 IsoParametric Interpolation (Priority 3)
 *
 * Tests:
 * 1. test_quad9_isoparametric() - QUAD_9 with isoparametric interpolation
 * 2. test_hexa27_isoparametric() - HEXA_27 with isoparametric interpolation
 *
 * IsoParametric Interpolation:
 * - Uses element's own node coordinates from grid as control points
 * - No LagrangeControlPoints need to be written
 * - InterpolationType_t = IsoParametric
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"
#include "utils.h"

int total_tests = 0;
int failed_tests = 0;

/**
 * Test 1: QUAD_9 Isoparametric Interpolation
 *
 * Creates a QUAD_9 element with isoparametric interpolation.
 * Verifies:
 * - ElementInterpolation_t node created
 * - InterpolationType_t = IsoParametric
 * - No LagrangeControlPoints needed
 * - Grid coordinates used directly
 */
int test_quad9_isoparametric()
{
    int fn, bn, zn, sn, fam, en;
    int ier;
    char filename[64];
    cgsize_t sizes[3];
    CGNS_ENUMT(ElementType_t) elem_type;
    CGNS_ENUMT(InterpolationType_t) interp_type;
    char interp_name[33];
    double x[9], y[9], z[9];
    int i;

    printf("\n=== Test 1: QUAD_9 IsoParametric Interpolation ===\n");
    total_tests++;

    sprintf(filename, "test_quad9_isoparam.cgns");

    // Create CGNS file
    if (cg_open(filename, CG_MODE_WRITE, &fn)) {
        printf("ERROR: Failed to create file: %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    // Create base
    sizes[0] = 9;  // 9 vertices
    sizes[1] = 1;  // 1 cell
    sizes[2] = 0;  // no boundary vertices

    if (cg_base_write(fn, "Base", 2, 2, &bn)) {
        printf("ERROR: Failed to create base: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Create zone
    if (cg_zone_write(fn, bn, "Zone", sizes, CGNS_ENUMV(Unstructured), &zn)) {
        printf("ERROR: Failed to create zone: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Write grid coordinates for 3x3 QUAD_9 element
    // Standard parametric grid: [-1,0,1] x [-1,0,1]
    for (i = 0; i < 9; i++) {
        x[i] = (double)(i % 3) - 1.0;  // -1, 0, 1, -1, 0, 1, -1, 0, 1
        y[i] = (double)(i / 3) - 1.0;  // -1, -1, -1, 0, 0, 0, 1, 1, 1
        z[i] = 0.0;
    }

    if (cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateX", x, &sn) ||
        cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateY", y, &sn) ||
        cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateZ", z, &sn)) {
        printf("ERROR: Failed to write coordinates: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Create family for interpolation
    if (cg_family_write(fn, bn, "ElementFamily", &fam)) {
        printf("ERROR: Failed to create family: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Write isoparametric interpolation metadata
    printf("  Writing IsoParametric interpolation for QUAD_9...\n");
    if (cg_element_isoparametric_write(fn, bn, fam, "QUAD9_IsoParam", CGNS_ENUMV(QUAD_9), &en)) {
        printf("ERROR: Failed to write isoparametric interpolation: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  ElementInterpolation index: %d\n", en);

    // Close and reopen for reading
    cg_close(fn);

    if (cg_open(filename, CG_MODE_READ, &fn)) {
        printf("ERROR: Failed to reopen file: %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    // Read back and verify
    printf("  Reading back interpolation metadata...\n");
    if (cg_element_interpolation_read(fn, bn, fam, 1, interp_name, &elem_type)) {
        printf("ERROR: Failed to read interpolation metadata: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  Interpolation name: %s\n", interp_name);
    printf("  Element type: %s\n", cg_ElementTypeName(elem_type));

    // Verify element type
    if (elem_type != CGNS_ENUMV(QUAD_9)) {
        printf("ERROR: Element type mismatch. Expected QUAD_9, got %s\n",
               cg_ElementTypeName(elem_type));
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Read interpolation type
    printf("  Reading interpolation type...\n");
    ier = cg_element_interpolation_type_read(fn, bn, fam, 1, &interp_type);
    if (ier != CG_OK) {
        printf("ERROR: Failed to read interpolation type: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  Interpolation type: %s\n", cg_InterpolationTypeName(interp_type));

    // Verify it's IsoParametric
    if (interp_type != CGNS_ENUMV(IsoParametric)) {
        printf("ERROR: Interpolation type mismatch. Expected IsoParametric, got %s\n",
               cg_InterpolationTypeName(interp_type));
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Try to read Lagrange points (should fail for isoparametric)
    double pu[9], pv[9];
    ier = cg_element_interpolation_points_read(fn, bn, fam, 1, pu, pv, NULL);
    if (ier != CG_NODE_NOT_FOUND) {
        printf("ERROR: IsoParametric should not have LagrangeControlPoints\n");
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  Verified: No LagrangeControlPoints (as expected for IsoParametric)\n");

    cg_close(fn);

    printf("Test 1 PASSED: QUAD_9 IsoParametric\n");
    return 0;
}

/**
 * Test 2: HEXA_27 Isoparametric Interpolation
 *
 * Creates a HEXA_27 element with isoparametric interpolation.
 * Verifies:
 * - 3D element support
 * - InterpolationType_t = IsoParametric
 * - Grid coordinates used (27 nodes)
 */
int test_hexa27_isoparametric()
{
    int fn, bn, zn, sn, fam, en;
    int ier;
    char filename[64];
    cgsize_t sizes[3];
    CGNS_ENUMT(ElementType_t) elem_type;
    CGNS_ENUMT(InterpolationType_t) interp_type;
    char interp_name[33];
    double x[27], y[27], z[27];
    int i, j, k, idx;

    printf("\n=== Test 2: HEXA_27 IsoParametric Interpolation ===\n");
    total_tests++;

    sprintf(filename, "test_hexa27_isoparam.cgns");

    // Create CGNS file
    if (cg_open(filename, CG_MODE_WRITE, &fn)) {
        printf("ERROR: Failed to create file: %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    // Create base
    sizes[0] = 27;  // 27 vertices
    sizes[1] = 1;   // 1 cell
    sizes[2] = 0;   // no boundary vertices

    if (cg_base_write(fn, "Base", 3, 3, &bn)) {
        printf("ERROR: Failed to create base: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Create zone
    if (cg_zone_write(fn, bn, "Zone", sizes, CGNS_ENUMV(Unstructured), &zn)) {
        printf("ERROR: Failed to create zone: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Write grid coordinates for 3x3x3 HEXA_27 element
    // Standard parametric grid: [-1,0,1] x [-1,0,1] x [-1,0,1]
    idx = 0;
    for (k = 0; k < 3; k++) {
        for (j = 0; j < 3; j++) {
            for (i = 0; i < 3; i++) {
                x[idx] = (double)i - 1.0;  // -1, 0, 1
                y[idx] = (double)j - 1.0;
                z[idx] = (double)k - 1.0;
                idx++;
            }
        }
    }

    if (cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateX", x, &sn) ||
        cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateY", y, &sn) ||
        cg_coord_write(fn, bn, zn, CGNS_ENUMV(RealDouble), "CoordinateZ", z, &sn)) {
        printf("ERROR: Failed to write coordinates: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Create family for interpolation
    if (cg_family_write(fn, bn, "ElementFamily", &fam)) {
        printf("ERROR: Failed to create family: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Write isoparametric interpolation metadata
    printf("  Writing IsoParametric interpolation for HEXA_27...\n");
    if (cg_element_isoparametric_write(fn, bn, fam, "HEXA27_IsoParam", CGNS_ENUMV(HEXA_27), &en)) {
        printf("ERROR: Failed to write isoparametric interpolation: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  ElementInterpolation index: %d\n", en);

    // Close and reopen for reading
    cg_close(fn);

    if (cg_open(filename, CG_MODE_READ, &fn)) {
        printf("ERROR: Failed to reopen file: %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    // Read back and verify
    printf("  Reading back interpolation metadata...\n");
    if (cg_element_interpolation_read(fn, bn, fam, 1, interp_name, &elem_type)) {
        printf("ERROR: Failed to read interpolation metadata: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  Interpolation name: %s\n", interp_name);
    printf("  Element type: %s\n", cg_ElementTypeName(elem_type));

    // Verify element type
    if (elem_type != CGNS_ENUMV(HEXA_27)) {
        printf("ERROR: Element type mismatch. Expected HEXA_27, got %s\n",
               cg_ElementTypeName(elem_type));
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Read interpolation type
    printf("  Reading interpolation type...\n");
    ier = cg_element_interpolation_type_read(fn, bn, fam, 1, &interp_type);
    if (ier != CG_OK) {
        printf("ERROR: Failed to read interpolation type: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  Interpolation type: %s\n", cg_InterpolationTypeName(interp_type));

    // Verify it's IsoParametric
    if (interp_type != CGNS_ENUMV(IsoParametric)) {
        printf("ERROR: Interpolation type mismatch. Expected IsoParametric, got %s\n",
               cg_InterpolationTypeName(interp_type));
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    // Try to read Lagrange points (should fail for isoparametric)
    double pu[27], pv[27], pw[27];
    ier = cg_element_interpolation_points_read(fn, bn, fam, 1, pu, pv, pw);
    if (ier != CG_NODE_NOT_FOUND) {
        printf("ERROR: IsoParametric should not have LagrangeControlPoints\n");
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  Verified: No LagrangeControlPoints (as expected for IsoParametric)\n");

    cg_close(fn);

    printf("Test 2 PASSED: HEXA_27 IsoParametric\n");
    return 0;
}

/*
 * Test 3: IsoParametric type for SolutionInterpolation_t
 *
 * Verifies that a SolutionInterpolation_t node with InterpolationType=IsoParametric
 * round-trips correctly and has no LagrangeControlPoints child.
 */
int test_sol_isoparametric()
{
    int fn, bn, fam, sn;
    char name[33];
    CGNS_ENUMT(ElementType_t) et;
    CGNS_ENUMT(InterpolationType_t) it;
    int os, ot, ier;
    const char *fname = "test_sol_isoparam.cgns";
    double dummy_u[4], dummy_v[4];

    printf("\n=== Test 3: SolutionInterpolation_t IsoParametric ===\n");
    total_tests++;

    /* --- Write --- */
    if (cg_open(fname, CG_MODE_WRITE, &fn) ||
        cg_base_write(fn, "Base", 3, 3, &bn) ||
        cg_family_write(fn, bn, "SolFamily", &fam)) {
        printf("ERROR (setup): %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    if (cg_solution_interpolation_write(fn, bn, fam, "QUAD4_IsoParam",
                                        CGNS_ENUMV(QUAD_4), 1, 0,
                                        CGNS_ENUMV(IsoParametric), &sn)) {
        printf("ERROR: cg_solution_interpolation_write: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }
    cg_close(fn);

    /* --- Read back --- */
    if (cg_open(fname, CG_MODE_READ, &fn)) {
        printf("ERROR: reopen: %s\n", cg_get_error());
        failed_tests++;
        return 1;
    }

    if (cg_solution_interpolation_read(fn, bn, fam, 1, name, &et, &os, &ot, &it)) {
        printf("ERROR: cg_solution_interpolation_read: %s\n", cg_get_error());
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    printf("  Name=%s  ElementType=%s  Orders=(%d,%d)  InterpolationType=%s\n",
           name, cg_ElementTypeName(et), os, ot, cg_InterpolationTypeName(it));

    if (it != CGNS_ENUMV(IsoParametric)) {
        printf("ERROR: InterpolationType mismatch: expected IsoParametric, got %s\n",
               cg_InterpolationTypeName(it));
        cg_close(fn);
        failed_tests++;
        return 1;
    }
    if (et != CGNS_ENUMV(QUAD_4)) {
        printf("ERROR: ElementType mismatch: expected QUAD_4, got %s\n",
               cg_ElementTypeName(et));
        cg_close(fn);
        failed_tests++;
        return 1;
    }
    if (os != 1 || ot != 0) {
        printf("ERROR: Orders mismatch: expected (1,0), got (%d,%d)\n", os, ot);
        cg_close(fn);
        failed_tests++;
        return 1;
    }

    /* No LagrangeControlPoints should exist for IsoParametric */
    ier = cg_solution_interpolation_points_read(fn, bn, fam, 1,
                                                dummy_u, dummy_v, NULL, NULL);
    if (ier != CG_NODE_NOT_FOUND) {
        printf("ERROR: IsoParametric SolutionInterpolation_t should have no "
               "LagrangeControlPoints\n");
        cg_close(fn);
        failed_tests++;
        return 1;
    }
    printf("  Verified: No LagrangeControlPoints (expected for IsoParametric)\n");

    cg_close(fn);
    printf("Test 3 PASSED: SolutionInterpolation_t IsoParametric\n");
    return 0;
}

int main()
{
    printf("========================================\n");
    printf("CPEX0045 IsoParametric Interpolation Tests\n");
    printf("Priority 3: IsoParametric Support\n");
    printf("========================================\n");

    // Run all tests
    test_quad9_isoparametric();
    test_hexa27_isoparametric();
    test_sol_isoparametric();

    // Summary
    printf("\n========================================\n");
    printf("Test Summary\n");
    printf("========================================\n");
    printf("Total tests: %d\n", total_tests);
    printf("Passed: %d\n", total_tests - failed_tests);
    printf("Failed: %d\n", failed_tests);
    printf("========================================\n");

    if (failed_tests > 0) {
        printf("FAILED: %d test(s) failed\n", failed_tests);
        return 1;
    }

    printf("SUCCESS: All tests passed!\n");
    return 0;
}
