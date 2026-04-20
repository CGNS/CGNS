/*
 * CPEX-0045 variable-order tests.
 *
 * Scenario A – variable order per cell via PointRange (spec §3.2.5):
 *   Two FlowSolution_t blocks with disjoint PointRanges:
 *     FS1  cells [1..N/2]   InterpolationOrders=(2,0)
 *     FS2  cells [N/2+1..N] InterpolationOrders=(3,0)
 *
 * Scenario B – variable order per field variable:
 *   Two FlowSolution_t blocks each covering all cells,
 *   carrying different field arrays at different orders:
 *     FS1  "Density"   over [1..N]  InterpolationOrders=(2,0)
 *     FS2  "VelocityX" over [1..N]  InterpolationOrders=(3,0)
 *
 * Scenario C – variable order per cell via PointList:
 *   Two FlowSolution_t blocks with disjoint PointLists (non-contiguous):
 *     FS1  odd  cells {1,3,5,7}  InterpolationOrders=(2,0)
 *     FS2  even cells {2,4,6,8}  InterpolationOrders=(3,0)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"

#define N_ELEM  8
#define N_VERT  (N_ELEM * 4)

static int check(int err, const char *what)
{
    if (err) {
        fprintf(stderr, "ERROR in %s: %s\n", what, cg_get_error());
        return 1;
    }
    return 0;
}

/* ------------------------------------------------------------------ */
/* Scenario A: variable order per cell via disjoint PointRange blocks  */
/* ------------------------------------------------------------------ */
static int test_variable_order_per_cell(void)
{
    const char *filename = "test_variable_order_cell.cgns";
    int fn, B, Z, F, S1, S2, si1, si2;
    cgsize_t size[3];
    cgsize_t range1[2] = {1,        N_ELEM / 2};
    cgsize_t range2[2] = {N_ELEM / 2 + 1, N_ELEM};

    printf("\n--- Scenario A: variable order per cell ---\n");

    /* Write */
    if (check(cg_open(filename, CG_MODE_WRITE, &fn),         "open W"))     return 1;
    if (check(cg_base_write(fn, "Base", 3, 3, &B),           "base"))       return 1;
    size[0] = N_VERT; size[1] = N_ELEM; size[2] = 0;
    if (check(cg_zone_write(fn, B, "Zone", size,
                            CGNS_ENUMV(Unstructured), &Z),   "zone"))       return 1;
    if (check(cg_family_write(fn, B, "VarOrderFam", &F),     "family"))     return 1;

    if (check(cg_solution_interpolation_write(fn, B, F, "Tet_P2",
              CGNS_ENUMV(TETRA_4), 2, 0,
              CGNS_ENUMV(ParametricLagrange), &si1),          "SI P2"))     return 1;
    if (check(cg_solution_interpolation_write(fn, B, F, "Tet_P3",
              CGNS_ENUMV(TETRA_4), 3, 0,
              CGNS_ENUMV(ParametricLagrange), &si2),          "SI P3"))     return 1;

    if (check(cg_sol_ptset_write(fn, B, Z, "FS_lowOrder",
              CGNS_ENUMV(CellCenter),
              CGNS_ENUMV(PointRange), 2, range1, &S1),        "ptset S1"))  return 1;
    if (check(cg_sol_interpolation_order_write(fn, B, Z, S1, 2, 0),
              "order S1"))                                                   return 1;

    if (check(cg_sol_ptset_write(fn, B, Z, "FS_highOrder",
              CGNS_ENUMV(CellCenter),
              CGNS_ENUMV(PointRange), 2, range2, &S2),        "ptset S2"))  return 1;
    if (check(cg_sol_interpolation_order_write(fn, B, Z, S2, 3, 0),
              "order S2"))                                                   return 1;

    if (check(cg_close(fn), "close W")) return 1;

    /* Read back */
    if (check(cg_open(filename, CG_MODE_READ, &fn), "open R")) return 1;

    for (int s = 1; s <= 2; s++) {
        CGNS_ENUMT(PointSetType_t) ptype;
        cgsize_t npts, pts[2];
        int os, ot;
        int exp_order     = (s == 1) ? 2 : 3;
        cgsize_t exp_lo   = (s == 1) ? range1[0] : range2[0];
        cgsize_t exp_hi   = (s == 1) ? range1[1] : range2[1];

        if (check(cg_sol_ptset_info(fn, B, Z, s, &ptype, &npts), "ptset_info")) return 1;
        if (ptype != CGNS_ENUMV(PointRange) || npts != 2) {
            fprintf(stderr, "FS%d: unexpected ptset (type=%d npts=%lld)\n",
                    s, (int)ptype, (long long)npts);
            return 1;
        }
        if (check(cg_sol_ptset_read(fn, B, Z, s, pts), "ptset_read")) return 1;
        if (pts[0] != exp_lo || pts[1] != exp_hi) {
            fprintf(stderr, "FS%d: range [%lld,%lld], expected [%lld,%lld]\n",
                    s, (long long)pts[0], (long long)pts[1],
                    (long long)exp_lo, (long long)exp_hi);
            return 1;
        }
        if (check(cg_sol_interpolation_order_read(fn, B, Z, s, &os, &ot),
                  "order_read")) return 1;
        if (os != exp_order || ot != 0) {
            fprintf(stderr, "FS%d: order (%d,%d), expected (%d,0)\n",
                    s, os, ot, exp_order);
            return 1;
        }
        printf("  FS%d OK: range=[%lld,%lld] order=(%d,%d)\n",
               s, (long long)pts[0], (long long)pts[1], os, ot);
    }

    if (check(cg_close(fn), "close R")) return 1;
    printf("Scenario A PASSED\n");
    return 0;
}

/* ------------------------------------------------------------------ */
/* Scenario B: variable order per field variable                       */
/*   Same PointRange (all cells), different arrays, different orders.  */
/* ------------------------------------------------------------------ */
static int test_variable_order_per_field(void)
{
    const char *filename = "test_variable_order_field.cgns";
    int fn, B, Z, F, S1, S2, si1, si2;
    cgsize_t size[3];
    cgsize_t range_all[2] = {1, N_ELEM};
    /* Dummy field data – values not meaningful for this structural test */
    double density[N_ELEM], velocity[N_ELEM];
    int i;

    for (i = 0; i < N_ELEM; i++) { density[i] = (double)i; velocity[i] = (double)(i*2); }

    printf("\n--- Scenario B: variable order per field variable ---\n");

    /* Write */
    if (check(cg_open(filename, CG_MODE_WRITE, &fn),         "open W"))     return 1;
    if (check(cg_base_write(fn, "Base", 3, 3, &B),           "base"))       return 1;
    size[0] = N_VERT; size[1] = N_ELEM; size[2] = 0;
    if (check(cg_zone_write(fn, B, "Zone", size,
                            CGNS_ENUMV(Unstructured), &Z),   "zone"))       return 1;
    if (check(cg_family_write(fn, B, "VarFieldFam", &F),     "family"))     return 1;

    /* Two SolutionInterpolation_t nodes in family – same element, different orders */
    if (check(cg_solution_interpolation_write(fn, B, F, "Tet_P2",
              CGNS_ENUMV(TETRA_4), 2, 0,
              CGNS_ENUMV(ParametricLagrange), &si1),          "SI P2"))     return 1;
    if (check(cg_solution_interpolation_write(fn, B, F, "Tet_P3",
              CGNS_ENUMV(TETRA_4), 3, 0,
              CGNS_ENUMV(ParametricLagrange), &si2),          "SI P3"))     return 1;

    /* FS1: Density at order 2 – covers all cells */
    if (check(cg_sol_ptset_write(fn, B, Z, "FS_Density",
              CGNS_ENUMV(CellCenter),
              CGNS_ENUMV(PointRange), 2, range_all, &S1),     "ptset S1"))  return 1;
    if (check(cg_sol_interpolation_order_write(fn, B, Z, S1, 2, 0),
              "order S1"))                                                   return 1;
    {
        int fld;
        if (check(cg_field_write(fn, B, Z, S1, CGNS_ENUMV(RealDouble),
                                 "Density", density, &fld),   "field Density")) return 1;
    }

    /* FS2: VelocityX at order 3 – also covers all cells */
    if (check(cg_sol_ptset_write(fn, B, Z, "FS_VelocityX",
              CGNS_ENUMV(CellCenter),
              CGNS_ENUMV(PointRange), 2, range_all, &S2),     "ptset S2"))  return 1;
    if (check(cg_sol_interpolation_order_write(fn, B, Z, S2, 3, 0),
              "order S2"))                                                   return 1;
    {
        int fld;
        if (check(cg_field_write(fn, B, Z, S2, CGNS_ENUMV(RealDouble),
                                 "VelocityX", velocity, &fld),"field VelX")) return 1;
    }

    if (check(cg_close(fn), "close W")) return 1;

    /* Read back */
    if (check(cg_open(filename, CG_MODE_READ, &fn), "open R")) return 1;

    struct { int exp_order; const char *exp_field; } cases[2] = {
        {2, "Density"},
        {3, "VelocityX"},
    };

    for (int s = 1; s <= 2; s++) {
        CGNS_ENUMT(PointSetType_t) ptype;
        cgsize_t npts, pts[2];
        int os, ot, nflds;
        char sol_name[33], fld_name[33];
        CGNS_ENUMT(DataType_t) dtype;

        /* Verify PointRange covers all cells */
        if (check(cg_sol_ptset_info(fn, B, Z, s, &ptype, &npts), "ptset_info")) return 1;
        if (ptype != CGNS_ENUMV(PointRange) || npts != 2) {
            fprintf(stderr, "FS%d: unexpected ptset\n", s);
            return 1;
        }
        if (check(cg_sol_ptset_read(fn, B, Z, s, pts), "ptset_read")) return 1;
        if (pts[0] != 1 || pts[1] != N_ELEM) {
            fprintf(stderr, "FS%d: range [%lld,%lld], expected [1,%d]\n",
                    s, (long long)pts[0], (long long)pts[1], N_ELEM);
            return 1;
        }

        /* Verify InterpolationOrders */
        if (check(cg_sol_interpolation_order_read(fn, B, Z, s, &os, &ot),
                  "order_read")) return 1;
        if (os != cases[s-1].exp_order || ot != 0) {
            fprintf(stderr, "FS%d: order (%d,%d), expected (%d,0)\n",
                    s, os, ot, cases[s-1].exp_order);
            return 1;
        }

        /* Verify field variable name */
        if (check(cg_nfields(fn, B, Z, s, &nflds), "nfields")) return 1;
        if (nflds != 1) {
            fprintf(stderr, "FS%d: expected 1 field, got %d\n", s, nflds);
            return 1;
        }
        if (check(cg_field_info(fn, B, Z, s, 1, &dtype, fld_name), "field_info")) return 1;
        if (strcmp(fld_name, cases[s-1].exp_field) != 0) {
            fprintf(stderr, "FS%d: field '%s', expected '%s'\n",
                    s, fld_name, cases[s-1].exp_field);
            return 1;
        }

        printf("  FS%d OK: range=[%lld,%lld] order=(%d,%d) field='%s'\n",
               s, (long long)pts[0], (long long)pts[1], os, ot, fld_name);
    }

    if (check(cg_close(fn), "close R")) return 1;
    printf("Scenario B PASSED\n");
    return 0;
}

/* ------------------------------------------------------------------ */
/* Scenario C: variable order per cell via disjoint PointList blocks  */
/*   Odd cells  {1,3,5,7} at order 2                                  */
/*   Even cells {2,4,6,8} at order 3                                  */
/* ------------------------------------------------------------------ */
static int test_variable_order_pointlist(void)
{
    const char *filename = "test_variable_order_ptlist.cgns";
    int fn, B, Z, F, S1, S2, si1, si2;
    cgsize_t size[3];
    /* Non-contiguous cell subsets */
    cgsize_t odd_cells[4]  = {1, 3, 5, 7};
    cgsize_t even_cells[4] = {2, 4, 6, 8};

    printf("\n--- Scenario C: variable order per cell (PointList) ---\n");

    /* Write */
    if (check(cg_open(filename, CG_MODE_WRITE, &fn),         "open W"))     return 1;
    if (check(cg_base_write(fn, "Base", 3, 3, &B),           "base"))       return 1;
    size[0] = N_VERT; size[1] = N_ELEM; size[2] = 0;
    if (check(cg_zone_write(fn, B, "Zone", size,
                            CGNS_ENUMV(Unstructured), &Z),   "zone"))       return 1;
    if (check(cg_family_write(fn, B, "PtListFam", &F),       "family"))     return 1;

    if (check(cg_solution_interpolation_write(fn, B, F, "Tet_P2",
              CGNS_ENUMV(TETRA_4), 2, 0,
              CGNS_ENUMV(ParametricLagrange), &si1),          "SI P2"))     return 1;
    if (check(cg_solution_interpolation_write(fn, B, F, "Tet_P3",
              CGNS_ENUMV(TETRA_4), 3, 0,
              CGNS_ENUMV(ParametricLagrange), &si2),          "SI P3"))     return 1;

    /* FS1: odd cells at order 2 via PointList */
    if (check(cg_sol_ptset_write(fn, B, Z, "FS_Odd",
              CGNS_ENUMV(CellCenter),
              CGNS_ENUMV(PointList), 4, odd_cells, &S1),      "ptset S1"))  return 1;
    if (check(cg_sol_interpolation_order_write(fn, B, Z, S1, 2, 0),
              "order S1"))                                                   return 1;

    /* FS2: even cells at order 3 via PointList */
    if (check(cg_sol_ptset_write(fn, B, Z, "FS_Even",
              CGNS_ENUMV(CellCenter),
              CGNS_ENUMV(PointList), 4, even_cells, &S2),     "ptset S2"))  return 1;
    if (check(cg_sol_interpolation_order_write(fn, B, Z, S2, 3, 0),
              "order S2"))                                                   return 1;

    if (check(cg_close(fn), "close W")) return 1;

    /* Read back */
    if (check(cg_open(filename, CG_MODE_READ, &fn), "open R")) return 1;

    for (int s = 1; s <= 2; s++) {
        CGNS_ENUMT(PointSetType_t) ptype;
        cgsize_t npts, pts[4];
        int os, ot;
        int exp_order          = (s == 1) ? 2 : 3;
        cgsize_t *exp_cells    = (s == 1) ? odd_cells : even_cells;

        if (check(cg_sol_ptset_info(fn, B, Z, s, &ptype, &npts), "ptset_info")) return 1;
        if (ptype != CGNS_ENUMV(PointList) || npts != 4) {
            fprintf(stderr, "FS%d: expected PointList/4, got type=%d npts=%lld\n",
                    s, (int)ptype, (long long)npts);
            return 1;
        }
        if (check(cg_sol_ptset_read(fn, B, Z, s, pts), "ptset_read")) return 1;
        for (int i = 0; i < 4; i++) {
            if (pts[i] != exp_cells[i]) {
                fprintf(stderr, "FS%d: pts[%d]=%lld, expected %lld\n",
                        s, i, (long long)pts[i], (long long)exp_cells[i]);
                return 1;
            }
        }

        if (check(cg_sol_interpolation_order_read(fn, B, Z, s, &os, &ot),
                  "order_read")) return 1;
        if (os != exp_order || ot != 0) {
            fprintf(stderr, "FS%d: order (%d,%d), expected (%d,0)\n",
                    s, os, ot, exp_order);
            return 1;
        }
        printf("  FS%d OK: PointList={%lld,%lld,%lld,%lld} order=(%d,%d)\n",
               s, (long long)pts[0], (long long)pts[1],
               (long long)pts[2], (long long)pts[3], os, ot);
    }

    if (check(cg_close(fn), "close R")) return 1;
    printf("Scenario C PASSED\n");
    return 0;
}

int main(void)
{
    int rc = 0;
    printf("\n=== CPEX-0045 variable-order tests ===\n");
    rc |= test_variable_order_per_cell();
    rc |= test_variable_order_per_field();
    rc |= test_variable_order_pointlist();
    if (rc == 0)
        printf("\ntest_variable_order: PASS\n");
    return rc;
}
