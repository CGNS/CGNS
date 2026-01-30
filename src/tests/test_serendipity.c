#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"

/*
 * Serendipity Element Tests
 *
 * Tests high-order serendipity elements that have control points only on
 * corners and edges (2D), or corners, edges and faces (3D), but NOT in the interior.
 *
 * Test 6.1: QUAD_8 - 2D serendipity quadrilateral (order 2, no center node)
 * Test 6.2: HEXA_20 - 3D serendipity hexahedron (order 2, no interior nodes)
 */

/* Fill parametric coordinates for QUAD_8 serendipity element
 * 8 nodes: 4 corners + 4 edge midpoints (NO center point)
 * Parametric space: (u,v) where -1 <= u,v <= 1
 *
 * Node ordering:
 *   v=1:  3----6----2
 *         |         |
 *   v=0:  7         5
 *         |         |
 *   v=-1: 0----4----1
 *        u=-1  0   1
 */
void fillQuad8LagrangePoints(double *pu, double *pv)
{
    int idx = 0;

    /* 4 corner nodes */
    pu[idx] = -1.0; pv[idx] = -1.0; idx++;  /* Node 0: bottom-left */
    pu[idx] =  1.0; pv[idx] = -1.0; idx++;  /* Node 1: bottom-right */
    pu[idx] =  1.0; pv[idx] =  1.0; idx++;  /* Node 2: top-right */
    pu[idx] = -1.0; pv[idx] =  1.0; idx++;  /* Node 3: top-left */

    /* 4 edge midpoint nodes */
    pu[idx] =  0.0; pv[idx] = -1.0; idx++;  /* Node 4: bottom edge */
    pu[idx] =  1.0; pv[idx] =  0.0; idx++;  /* Node 5: right edge */
    pu[idx] =  0.0; pv[idx] =  1.0; idx++;  /* Node 6: top edge */
    pu[idx] = -1.0; pv[idx] =  0.0;         /* Node 7: left edge */

    /* NOTE: No center node at (0, 0) - that's what makes it serendipity! */
}

/* Fill parametric coordinates for HEXA_20 serendipity element
 * 20 nodes: 8 corners + 12 edge midpoints (NO face or interior nodes)
 * Parametric space: (u,v,w) where -1 <= u,v,w <= 1
 *
 * Node ordering:
 *   Bottom face (w=-1): corners 0-3, edge midpoints 8-11
 *   Top face (w=1):     corners 4-7, edge midpoints 12-15
 *   Vertical edges:     midpoints 16-19
 */
void fillHexa20LagrangePoints(double *pu, double *pv, double *pw)
{
    int idx = 0;

    /* 8 corner nodes */
    /* Bottom face (w=-1) */
    pu[idx] = -1.0; pv[idx] = -1.0; pw[idx] = -1.0; idx++;  /* Node 0 */
    pu[idx] =  1.0; pv[idx] = -1.0; pw[idx] = -1.0; idx++;  /* Node 1 */
    pu[idx] =  1.0; pv[idx] =  1.0; pw[idx] = -1.0; idx++;  /* Node 2 */
    pu[idx] = -1.0; pv[idx] =  1.0; pw[idx] = -1.0; idx++;  /* Node 3 */

    /* Top face (w=1) */
    pu[idx] = -1.0; pv[idx] = -1.0; pw[idx] =  1.0; idx++;  /* Node 4 */
    pu[idx] =  1.0; pv[idx] = -1.0; pw[idx] =  1.0; idx++;  /* Node 5 */
    pu[idx] =  1.0; pv[idx] =  1.0; pw[idx] =  1.0; idx++;  /* Node 6 */
    pu[idx] = -1.0; pv[idx] =  1.0; pw[idx] =  1.0; idx++;  /* Node 7 */

    /* 12 edge midpoint nodes */
    /* Bottom face edges (w=-1) */
    pu[idx] =  0.0; pv[idx] = -1.0; pw[idx] = -1.0; idx++;  /* Node 8: bottom edge */
    pu[idx] =  1.0; pv[idx] =  0.0; pw[idx] = -1.0; idx++;  /* Node 9: right edge */
    pu[idx] =  0.0; pv[idx] =  1.0; pw[idx] = -1.0; idx++;  /* Node 10: top edge */
    pu[idx] = -1.0; pv[idx] =  0.0; pw[idx] = -1.0; idx++;  /* Node 11: left edge */

    /* Top face edges (w=1) */
    pu[idx] =  0.0; pv[idx] = -1.0; pw[idx] =  1.0; idx++;  /* Node 12 */
    pu[idx] =  1.0; pv[idx] =  0.0; pw[idx] =  1.0; idx++;  /* Node 13 */
    pu[idx] =  0.0; pv[idx] =  1.0; pw[idx] =  1.0; idx++;  /* Node 14 */
    pu[idx] = -1.0; pv[idx] =  0.0; pw[idx] =  1.0; idx++;  /* Node 15 */

    /* Vertical edges (u,v at corners, w=0) */
    pu[idx] = -1.0; pv[idx] = -1.0; pw[idx] =  0.0; idx++;  /* Node 16 */
    pu[idx] =  1.0; pv[idx] = -1.0; pw[idx] =  0.0; idx++;  /* Node 17 */
    pu[idx] =  1.0; pv[idx] =  1.0; pw[idx] =  0.0; idx++;  /* Node 18 */
    pu[idx] = -1.0; pv[idx] =  1.0; pw[idx] =  0.0;         /* Node 19 */

    /* NOTE: No face center nodes or volume center - that's serendipity! */
}

/* Test 6.1: QUAD_8 Serendipity Element */
int test_quad8_serendipity()
{
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    int i, npe, npts = 8;
    double *pu, *pv, *puu, *pvv;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) type = CGNS_ENUMV(QUAD_8);
    CGNS_ENUMT(ElementType_t) type_read;
    char filename[] = "test_quad8.cgns";
    char einterpname[33];

    printf("\n==============================================\n");
    printf("  Test 6.1: QUAD_8 Serendipity Element\n");
    printf("==============================================\n\n");

    printf("Testing QUAD_8 (order 2 serendipity quadrilateral)...\n");
    printf("  8 nodes: 4 corners + 4 edge midpoints (NO center node)\n");

    /* Allocate arrays */
    pu = (double*) malloc(npts * sizeof(double));
    pv = (double*) malloc(npts * sizeof(double));

    /* Fill parametric coordinates */
    fillQuad8LagrangePoints(pu, pv);

    /* Create CGNS file */
    size[0] = 8;  /* vertex size */
    size[1] = 1;  /* cell size */
    size[2] = 0;  /* boundary vertex size */

    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        free(pu); free(pv);
        return 1;
    }

    /* Write family */
    if (cg_family_write(cgfile, cgbase, "Quad8Family", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        free(pu); free(pv);
        return 1;
    }
    printf("Family created\n");

    /* Write ElementInterpolation_t */
    printf("Writing ElementInterpolation_t for QUAD_8...\n");
    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "Quad8Interp",
                                       type, &cgeinterp))
    {
        fprintf(stderr, "ERROR: Failed to write ElementInterpolation_t\n");
        free(pu); free(pv);
        return 1;
    }
    printf("ElementInterpolation_t created (index=%d)\n", cgeinterp);

    /* Write Lagrange control points */
    printf("Writing %d Lagrange control points...\n", npts);
    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, cgeinterp,
                                              pu, pv, NULL))
    {
        fprintf(stderr, "ERROR: Failed to write Lagrange control points\n");
        free(pu); free(pv);
        return 1;
    }
    printf("Written %d control points\n", npts);

    cg_close(cgfile);

    /* Read back and validate */
    printf("\nReading back QUAD_8 serendipity element...\n");
    if (cg_open(filename, CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: Failed to open file for reading\n");
        free(pu); free(pv);
        return 1;
    }

    /* Read ElementInterpolation_t */
    if (cg_element_interpolation_read(cgfile, cgbase, cgfamily, cgeinterp,
                                      einterpname, &type_read))
    {
        fprintf(stderr, "ERROR: Failed to read ElementInterpolation_t\n");
        free(pu); free(pv);
        return 1;
    }

    if (type_read != type)
    {
        fprintf(stderr, "ERROR: Wrong element type (expected QUAD_8, got %s)\n",
                cg_ElementTypeName(type_read));
        free(pu); free(pv);
        return 1;
    }
    printf("Element type: %s\n", cg_ElementTypeName(type_read));

    /* Allocate arrays for reading */
    puu = (double*) malloc(npts * sizeof(double));
    pvv = (double*) malloc(npts * sizeof(double));

    /* Read control points */
    if (cg_element_interpolation_points_read(cgfile, cgbase, cgfamily, cgeinterp,
                                             puu, pvv, NULL))
    {
        fprintf(stderr, "ERROR: Failed to read Lagrange control points\n");
        free(pu); free(pv); free(puu); free(pvv);
        return 1;
    }
    printf("Read %d control points\n", npts);

    /* Validate control points */
    printf("\nValidating QUAD_8 serendipity node positions...\n");
    int failures = 0;

    /* Check corner nodes */
    if (fabs(puu[0] + 1.0) > 1.e-06 || fabs(pvv[0] + 1.0) > 1.e-06)
    {
        fprintf(stderr, "ERROR: Node 0 should be (-1, -1)\n");
        failures++;
    }
    printf("Node 0 (corner): (%g, %g)\n", puu[0], pvv[0]);

    /* Check edge midpoint */
    if (fabs(puu[4]) > 1.e-06 || fabs(pvv[4] + 1.0) > 1.e-06)
    {
        fprintf(stderr, "ERROR: Node 4 should be (0, -1)\n");
        failures++;
    }
    printf("Node 4 (edge midpoint): (%g, %g)\n", puu[4], pvv[4]);

    /* Verify NO center node exists by checking total count */
    cg_npe(type_read, &npe);
    if (npe != 8)
    {
        fprintf(stderr, "ERROR: QUAD_8 should have exactly 8 nodes (no center)\n");
        failures++;
    }
    printf("Verified: 8 nodes total (serendipity - no center node)\n");

    /* Validate all points */
    for (i = 0; i < npts; i++)
    {
        if (fabs(pu[i] - puu[i]) > 1.e-12 || fabs(pv[i] - pvv[i]) > 1.e-12)
        {
            fprintf(stderr, "ERROR: Control point %d mismatch\n", i);
            failures++;
        }
    }

    if (failures > 0)
    {
        fprintf(stderr, "ERROR: %d validation failures\n", failures);
        free(pu); free(pv); free(puu); free(pvv);
        return 1;
    }

    printf("All %d control points validated successfully\n", npts);

    cg_close(cgfile);
    free(pu); free(pv); free(puu); free(pvv);

    printf("\nQUAD_8 SERENDIPITY TEST PASSED\n");
    return 0;
}

/* Test 6.2: HEXA_20 Serendipity Element */
int test_hexa20_serendipity()
{
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    int i, npe, npts = 20;
    double *pu, *pv, *pw, *puu, *pvv, *pww;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) type = CGNS_ENUMV(HEXA_20);
    CGNS_ENUMT(ElementType_t) type_read;
    char filename[] = "test_hexa20.cgns";
    char einterpname[33];

    printf("\n==============================================\n");
    printf("  Test 6.2: HEXA_20 Serendipity Element\n");
    printf("==============================================\n\n");

    printf("Testing HEXA_20 (order 2 serendipity hexahedron)...\n");
    printf("  20 nodes: 8 corners + 12 edge midpoints (NO face/volume centers)\n");

    /* Allocate arrays */
    pu = (double*) malloc(npts * sizeof(double));
    pv = (double*) malloc(npts * sizeof(double));
    pw = (double*) malloc(npts * sizeof(double));

    /* Fill parametric coordinates */
    fillHexa20LagrangePoints(pu, pv, pw);

    /* Create CGNS file */
    size[0] = 20;  /* vertex size */
    size[1] = 1;   /* cell size */
    size[2] = 0;   /* boundary vertex size */

    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        free(pu); free(pv); free(pw);
        return 1;
    }

    /* Write family */
    if (cg_family_write(cgfile, cgbase, "Hexa20Family", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        free(pu); free(pv); free(pw);
        return 1;
    }
    printf("Family created\n");

    /* Write ElementInterpolation_t */
    printf("Writing ElementInterpolation_t for HEXA_20...\n");
    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "Hexa20Interp",
                                       type, &cgeinterp))
    {
        fprintf(stderr, "ERROR: Failed to write ElementInterpolation_t\n");
        free(pu); free(pv); free(pw);
        return 1;
    }
    printf("ElementInterpolation_t created (index=%d)\n", cgeinterp);

    /* Write Lagrange control points */
    printf("Writing %d Lagrange control points...\n", npts);
    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, cgeinterp,
                                              pu, pv, pw))
    {
        fprintf(stderr, "ERROR: Failed to write Lagrange control points\n");
        free(pu); free(pv); free(pw);
        return 1;
    }
    printf("Written %d control points\n", npts);

    cg_close(cgfile);

    /* Read back and validate */
    printf("\nReading back HEXA_20 serendipity element...\n");
    if (cg_open(filename, CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: Failed to open file for reading\n");
        free(pu); free(pv); free(pw);
        return 1;
    }

    /* Read ElementInterpolation_t */
    if (cg_element_interpolation_read(cgfile, cgbase, cgfamily, cgeinterp,
                                      einterpname, &type_read))
    {
        fprintf(stderr, "ERROR: Failed to read ElementInterpolation_t\n");
        free(pu); free(pv); free(pw);
        return 1;
    }

    if (type_read != type)
    {
        fprintf(stderr, "ERROR: Wrong element type (expected HEXA_20, got %s)\n",
                cg_ElementTypeName(type_read));
        free(pu); free(pv); free(pw);
        return 1;
    }
    printf("Element type: %s\n", cg_ElementTypeName(type_read));

    /* Allocate arrays for reading */
    puu = (double*) malloc(npts * sizeof(double));
    pvv = (double*) malloc(npts * sizeof(double));
    pww = (double*) malloc(npts * sizeof(double));

    /* Read control points */
    if (cg_element_interpolation_points_read(cgfile, cgbase, cgfamily, cgeinterp,
                                             puu, pvv, pww))
    {
        fprintf(stderr, "ERROR: Failed to read Lagrange control points\n");
        free(pu); free(pv); free(pw); free(puu); free(pvv); free(pww);
        return 1;
    }
    printf("Read %d control points\n", npts);

    /* Validate control points */
    printf("\nValidating HEXA_20 serendipity node positions...\n");
    int failures = 0;

    /* Check corner node (0,0,0) -> Node 0 at (-1,-1,-1) */
    if (fabs(puu[0] + 1.0) > 1.e-06 || fabs(pvv[0] + 1.0) > 1.e-06 || fabs(pww[0] + 1.0) > 1.e-06)
    {
        fprintf(stderr, "ERROR: Node 0 should be (-1, -1, -1)\n");
        failures++;
    }
    printf("Node 0 (corner): (%g, %g, %g)\n", puu[0], pvv[0], pww[0]);

    /* Check edge midpoint - bottom edge */
    if (fabs(puu[8]) > 1.e-06 || fabs(pvv[8] + 1.0) > 1.e-06 || fabs(pww[8] + 1.0) > 1.e-06)
    {
        fprintf(stderr, "ERROR: Node 8 should be (0, -1, -1)\n");
        failures++;
    }
    printf("Node 8 (edge midpoint): (%g, %g, %g)\n", puu[8], pvv[8], pww[8]);

    /* Check vertical edge midpoint */
    if (fabs(puu[16] + 1.0) > 1.e-06 || fabs(pvv[16] + 1.0) > 1.e-06 || fabs(pww[16]) > 1.e-06)
    {
        fprintf(stderr, "ERROR: Node 16 should be (-1, -1, 0)\n");
        failures++;
    }
    printf("Node 16 (vertical edge midpoint): (%g, %g, %g)\n", puu[16], pvv[16], pww[16]);

    /* Verify exactly 20 nodes (serendipity - no face centers or volume center) */
    cg_npe(type_read, &npe);
    if (npe != 20)
    {
        fprintf(stderr, "ERROR: HEXA_20 should have exactly 20 nodes\n");
        failures++;
    }
    printf("Verified: 20 nodes total (serendipity - no face/volume centers)\n");

    /* Validate all points */
    for (i = 0; i < npts; i++)
    {
        if (fabs(pu[i] - puu[i]) > 1.e-12 ||
            fabs(pv[i] - pvv[i]) > 1.e-12 ||
            fabs(pw[i] - pww[i]) > 1.e-12)
        {
            fprintf(stderr, "ERROR: Control point %d mismatch\n", i);
            failures++;
        }
    }

    if (failures > 0)
    {
        fprintf(stderr, "ERROR: %d validation failures\n", failures);
        free(pu); free(pv); free(pw); free(puu); free(pvv); free(pww);
        return 1;
    }

    printf("All %d control points validated successfully\n", npts);

    cg_close(cgfile);
    free(pu); free(pv); free(pw); free(puu); free(pvv); free(pww);

    printf("\nHEXA_20 SERENDIPITY TEST PASSED\n");
    return 0;
}

int main(int argc, char **argv)
{
    int errors = 0;

    printf("\n");
    printf("##################################################\n");
    printf("#  CPEX0045 Test: Serendipity Elements          #\n");
    printf("##################################################\n");

    /* Test 6.1: QUAD_8 */
    if (test_quad8_serendipity())
        errors++;

    /* Test 6.2: HEXA_20 */
    if (test_hexa20_serendipity())
        errors++;

    printf("\n");
    printf("##################################################\n");
    if (errors == 0)
    {
        printf("#ALL SERENDIPITY TESTS PASSED (2/2)        #\n");
    }
    else
    {
        printf("#  ✗ SERENDIPITY TESTS FAILED (%d/2)             #\n", errors);
    }
    printf("##################################################\n");
    printf("\n");

    return errors;
}
