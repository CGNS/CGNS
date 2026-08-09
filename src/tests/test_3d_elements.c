#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"

/* Fill parametric coordinates for TETRA_20 (order 2 tetrahedron)
 * Parametric space: (r,s,t) where r,s,t >= 0 and r+s+t <= 1
 * 20 nodes arranged on tetrahedral grid
 */
void fillTetraLagrangePoints(int order, double *r, double *s, double *t)
{
    /* Predefined 20 parametric points for order-2 tetrahedron
     * Points distributed on vertices, edges, faces of tetrahedron
     */
    int idx = 0;

    /* 4 vertices */
    r[idx] = 0.0; s[idx] = 0.0; t[idx] = 0.0; idx++;
    r[idx] = 1.0; s[idx] = 0.0; t[idx] = 0.0; idx++;
    r[idx] = 0.0; s[idx] = 1.0; t[idx] = 0.0; idx++;
    r[idx] = 0.0; s[idx] = 0.0; t[idx] = 1.0; idx++;

    /* 12 edge midpoints */
    r[idx] = 0.5; s[idx] = 0.0; t[idx] = 0.0; idx++;
    r[idx] = 0.5; s[idx] = 0.5; t[idx] = 0.0; idx++;
    r[idx] = 0.0; s[idx] = 0.5; t[idx] = 0.0; idx++;
    r[idx] = 0.0; s[idx] = 0.0; t[idx] = 0.5; idx++;
    r[idx] = 0.5; s[idx] = 0.0; t[idx] = 0.5; idx++;
    r[idx] = 0.0; s[idx] = 0.5; t[idx] = 0.5; idx++;
    r[idx] = 0.0; s[idx] = 0.5; t[idx] = 0.25; idx++;
    r[idx] = 0.0; s[idx] = 0.25; t[idx] = 0.5; idx++;
    r[idx] = 0.5; s[idx] = 0.0; t[idx] = 0.25; idx++;
    r[idx] = 0.25; s[idx] = 0.0; t[idx] = 0.5; idx++;
    r[idx] = 0.5; s[idx] = 0.25; t[idx] = 0.0; idx++;
    r[idx] = 0.25; s[idx] = 0.5; t[idx] = 0.0; idx++;

    /* 4 face centers */
    r[idx] = 0.333; s[idx] = 0.333; t[idx] = 0.0; idx++;
    r[idx] = 0.333; s[idx] = 0.0; t[idx] = 0.333; idx++;
    r[idx] = 0.0; s[idx] = 0.333; t[idx] = 0.333; idx++;
    r[idx] = 0.333; s[idx] = 0.333; t[idx] = 0.333;

    /* The points above are laid out on the UNIT simplex {r,s,t >= 0,
     * r+s+t <= 1}.  CPEX-0045 (Section "Coordinate-System Conventions", and
     * Figure 1) fixes the reference tetrahedron as the BI-UNIT simplex
     * {u,v,w >= -1, u+v+w <= -1}; the CGNS documentation states this as a
     * Critical Interoperability Requirement, since a [0,1]-based writer
     * produces files that read back as a different element.  The affine map
     * x -> 2x-1 carries one onto the other and sends the four leading
     * vertices to exactly the TETRA_4 principal vertices of Figure 1. */
    for (idx = 0; idx < 20; idx++) {
        r[idx] = 2.0 * r[idx] - 1.0;
        s[idx] = 2.0 * s[idx] - 1.0;
        t[idx] = 2.0 * t[idx] - 1.0;
    }
}

/* Fill parametric coordinates for PENTA_18 (order 2 prism/wedge)
 * Parametric space: bi-unit triangle in (r,s) extruded along t in [-1,1]
 * 18 nodes: 6 vertices + 9 edge midpoints + 3 face centers
 */
void fillPentaLagrangePoints(int order, double *r, double *s, double *t)
{
    int idx = 0;

    /* Bottom triangle vertices (t=-1) */
    r[idx] = 0.0; s[idx] = 0.0; t[idx] = -1.0; idx++;
    r[idx] = 1.0; s[idx] = 0.0; t[idx] = -1.0; idx++;
    r[idx] = 0.0; s[idx] = 1.0; t[idx] = -1.0; idx++;

    /* Top triangle vertices (t=+1) */
    r[idx] = 0.0; s[idx] = 0.0; t[idx] = 1.0; idx++;
    r[idx] = 1.0; s[idx] = 0.0; t[idx] = 1.0; idx++;
    r[idx] = 0.0; s[idx] = 1.0; t[idx] = 1.0; idx++;

    /* Bottom triangle edge midpoints (t=-1) */
    r[idx] = 0.5; s[idx] = 0.0; t[idx] = -1.0; idx++;
    r[idx] = 0.5; s[idx] = 0.5; t[idx] = -1.0; idx++;
    r[idx] = 0.0; s[idx] = 0.5; t[idx] = -1.0; idx++;

    /* Top triangle edge midpoints (t=+1) */
    r[idx] = 0.5; s[idx] = 0.0; t[idx] = 1.0; idx++;
    r[idx] = 0.5; s[idx] = 0.5; t[idx] = 1.0; idx++;
    r[idx] = 0.0; s[idx] = 0.5; t[idx] = 1.0; idx++;

    /* Vertical edge midpoints (t=0) */
    r[idx] = 0.0; s[idx] = 0.0; t[idx] = 0.0; idx++;
    r[idx] = 1.0; s[idx] = 0.0; t[idx] = 0.0; idx++;
    r[idx] = 0.0; s[idx] = 1.0; t[idx] = 0.0; idx++;

    /* Quadrilateral face centers */
    r[idx] = 0.5; s[idx] = 0.0; t[idx] = 0.0; idx++;
    r[idx] = 0.5; s[idx] = 0.5; t[idx] = 0.0; idx++;
    r[idx] = 0.0; s[idx] = 0.5; t[idx] = 0.0;

    /* (r,s) above are on the UNIT triangle {r,s >= 0, r+s <= 1} while t is
     * already on [-1,1].  CPEX-0045 fixes the reference prism as the BI-UNIT
     * triangle extruded along w in [-1,1], so only the triangular coordinates
     * are remapped; x -> 2x-1 sends the six leading vertices to exactly the
     * PENTA_6 principal vertices of Figure 1. */
    for (idx = 0; idx < 18; idx++) {
        r[idx] = 2.0 * r[idx] - 1.0;
        s[idx] = 2.0 * s[idx] - 1.0;
    }
}

/* Fill parametric coordinates for PYRA_14 (order 2 pyramid)
 * Parametric space: {-1 <= t <= 1, |r| <= (1-t)/2, |s| <= (1-t)/2},
 * the square base at t = -1 contracting to the apex (0,0,1)
 * 14 nodes: 5 corners + 8 edge midpoints + 1 base center
 */
void fillPyraLagrangePoints(int order, double *r, double *s, double *t)
{
    int idx = 0;

    /* 4 base vertices (t=0) */
    r[idx] = -1.0; s[idx] = -1.0; t[idx] = 0.0; idx++;
    r[idx] =  1.0; s[idx] = -1.0; t[idx] = 0.0; idx++;
    r[idx] =  1.0; s[idx] =  1.0; t[idx] = 0.0; idx++;
    r[idx] = -1.0; s[idx] =  1.0; t[idx] = 0.0; idx++;

    /* 1 apex vertex (t=1) */
    r[idx] = 0.0; s[idx] = 0.0; t[idx] = 1.0; idx++;

    /* 4 base edge midpoints (t=0) */
    r[idx] =  0.0; s[idx] = -1.0; t[idx] = 0.0; idx++;
    r[idx] =  1.0; s[idx] =  0.0; t[idx] = 0.0; idx++;
    r[idx] =  0.0; s[idx] =  1.0; t[idx] = 0.0; idx++;
    r[idx] = -1.0; s[idx] =  0.0; t[idx] = 0.0; idx++;

    /* 4 lateral edge midpoints (t=0.5) */
    r[idx] = -0.5; s[idx] = -0.5; t[idx] = 0.5; idx++;
    r[idx] =  0.5; s[idx] = -0.5; t[idx] = 0.5; idx++;
    r[idx] =  0.5; s[idx] =  0.5; t[idx] = 0.5; idx++;
    r[idx] = -0.5; s[idx] =  0.5; t[idx] = 0.5; idx++;

    /* 1 base center */
    r[idx] = 0.0; s[idx] = 0.0; t[idx] = 0.0;

    /* (r,s) above are already on [-1,1] but t is on [0,1].  CPEX-0045 fixes
     * the reference pyramid as {-1 <= w <= 1, |u| <= (1-w)/2, |v| <= (1-w)/2}
     * -- the square base at w = -1 contracting to the apex (0,0,1) -- so only
     * the axial coordinate is remapped.  t -> 2t-1 sends the five leading
     * vertices to exactly the PYRA_5 principal vertices of Figure 1, and puts
     * the lateral edge midpoints on the lateral edges (at w = 0 the section
     * half-width is 1/2, which is where those points sit). */
    for (idx = 0; idx < 14; idx++) {
        t[idx] = 2.0 * t[idx] - 1.0;
    }
}

/* Test a single 3D element type */
int test_3d_element(CGNS_ENUMT(ElementType_t) type, const char* name,
                    int npts, void (*fill_func)(int, double*, double*, double*))
{
    int error, i, nfam1, nfam2, n;
    int nsize;
    double *pu, *pv, *pw, *puu, *pvv, *pww;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) etyperead;
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    char einterpName[33], familyname[33], zonename[33];
    char filename[64];
    int order = 2;
    int failed_points = 0;

    printf("\n==============================================\n");
    printf("  Testing %s (%d nodes)\n", name, npts);
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
    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "Interpolation",
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

    fill_func(order, pu, pv, pw);

    printf("Writing Lagrange control points (%d points)...\n", npts);
    if (cg_element_interpolation_points_write(cgfile, cgbase, cgfamily, cgeinterp,
                                              pu, pv, pw))
    {
        fprintf(stderr, "ERROR: Failed to write Lagrange control points\n");
        return 1;
    }
    printf("Written %d control points\n", npts);

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
    printf("#  CPEX0045 Test Suite: 3D High-Order Elements  #\n");
    printf("##################################################\n");

    /* Test TETRA_20 */
    if (test_3d_element(CGNS_ENUMV(TETRA_20), "TETRA_20", 20, fillTetraLagrangePoints))
        errors++;

    /* Test PENTA_18 */
    if (test_3d_element(CGNS_ENUMV(PENTA_18), "PENTA_18", 18, fillPentaLagrangePoints))
        errors++;

    /* Test PYRA_14 */
    if (test_3d_element(CGNS_ENUMV(PYRA_14), "PYRA_14", 14, fillPyraLagrangePoints))
        errors++;

    printf("\n");
    printf("##################################################\n");
    if (errors == 0)
    {
        printf("#ALL 3D ELEMENT TESTS PASSED (3/3)         #\n");
    }
    else
    {
        printf("#  ✗ FAILURES: %d element type(s) failed        #\n", errors);
    }
    printf("##################################################\n");
    printf("\n");

    return errors;
}
