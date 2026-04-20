/*
 * CPEX-0045 variable-order-per-cell round-trip test.
 *
 * Scenario (spec §3.2.5, variable-order use case):
 *   - An unstructured zone with N (TETRA_4) elements.
 *   - Family carries two SolutionInterpolation_t blocks:
 *       (TETRA_4, SpatialOrder=2, TemporalOrder=0)
 *       (TETRA_4, SpatialOrder=3, TemporalOrder=0)
 *   - Two FlowSolution_t blocks at CellCenter, each with a disjoint
 *     PointRange selecting a subset of cells:
 *       FS1 picks cells [1 .. N/2], InterpolationOrders = (2, 0)
 *       FS2 picks cells [N/2+1 .. N], InterpolationOrders = (3, 0)
 *   - Close, reopen, and verify all four read-backs (ptset + orders).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"

#define N_ELEM      8
#define N_VERT      (N_ELEM * 4)   /* loose upper bound; nodes unused in test */

static int check(int err, const char *what)
{
    if (err) {
        fprintf(stderr, "ERROR in %s: %s\n", what, cg_get_error());
        return 1;
    }
    return 0;
}

int main(void)
{
    const char *filename = "test_variable_order.cgns";
    int fn, B, Z, F, S1, S2;
    int si1, si2;
    cgsize_t size[3];
    cgsize_t range1[2] = {1, N_ELEM / 2};
    cgsize_t range2[2] = {N_ELEM / 2 + 1, N_ELEM};

    printf("\n=== CPEX-0045 variable-order-per-cell test ===\n");

    /* ----------- WRITE ----------- */
    if (check(cg_open(filename, CG_MODE_WRITE, &fn),       "cg_open W"))   return 1;
    if (check(cg_base_write(fn, "Base", 3, 3, &B),          "cg_base_write")) return 1;

    size[0] = N_VERT;
    size[1] = N_ELEM;
    size[2] = 0;
    if (check(cg_zone_write(fn, B, "Zone", size, CGNS_ENUMV(Unstructured), &Z),
              "cg_zone_write")) return 1;

    if (check(cg_family_write(fn, B, "VariableOrderFamily", &F), "cg_family_write")) return 1;

    /* Two SolutionInterpolation_t blocks: same element type, different orders. */
    if (check(cg_solution_interpolation_write(fn, B, F, "Tet_P2",
                                              CGNS_ENUMV(TETRA_4), 2, 0,
                                              CGNS_ENUMV(ParametricLagrange), &si1),
              "SolutionInterpolation_t P2")) return 1;

    if (check(cg_solution_interpolation_write(fn, B, F, "Tet_P3",
                                              CGNS_ENUMV(TETRA_4), 3, 0,
                                              CGNS_ENUMV(ParametricLagrange), &si2),
              "SolutionInterpolation_t P3")) return 1;

    /* Two CellCenter FlowSolution_t blocks with disjoint PointRange and
     * matching InterpolationOrders. */
    if (check(cg_sol_ptset_write(fn, B, Z, "FS_lowOrder",
                                 CGNS_ENUMV(CellCenter),
                                 CGNS_ENUMV(PointRange), 2, range1, &S1),
              "ptset FS1")) return 1;
    if (check(cg_sol_interpolation_order_write(fn, B, Z, S1, 2, 0),
              "order FS1")) return 1;

    if (check(cg_sol_ptset_write(fn, B, Z, "FS_highOrder",
                                 CGNS_ENUMV(CellCenter),
                                 CGNS_ENUMV(PointRange), 2, range2, &S2),
              "ptset FS2")) return 1;
    if (check(cg_sol_interpolation_order_write(fn, B, Z, S2, 3, 0),
              "order FS2")) return 1;

    if (check(cg_close(fn), "cg_close W")) return 1;
    printf("Wrote: FS1 range=[%lld,%lld] order=2, FS2 range=[%lld,%lld] order=3\n",
           (long long)range1[0], (long long)range1[1],
           (long long)range2[0], (long long)range2[1]);

    /* ----------- READ ----------- */
    if (check(cg_open(filename, CG_MODE_READ, &fn), "cg_open R")) return 1;

    for (int s = 1; s <= 2; s++) {
        CGNS_ENUMT(PointSetType_t) ptype;
        cgsize_t npts, pts[2];
        int os, ot;
        int expected_order = (s == 1) ? 2 : 3;
        cgsize_t expected_lo = (s == 1) ? range1[0] : range2[0];
        cgsize_t expected_hi = (s == 1) ? range1[1] : range2[1];

        if (check(cg_sol_ptset_info(fn, B, Z, s, &ptype, &npts), "ptset_info")) return 1;
        if (ptype != CGNS_ENUMV(PointRange) || npts != 2) {
            fprintf(stderr, "FS%d: unexpected ptset (type=%d, npts=%lld)\n",
                    s, (int)ptype, (long long)npts);
            return 1;
        }
        if (check(cg_sol_ptset_read(fn, B, Z, s, pts), "ptset_read")) return 1;
        if (pts[0] != expected_lo || pts[1] != expected_hi) {
            fprintf(stderr, "FS%d: PointRange [%lld,%lld], expected [%lld,%lld]\n",
                    s, (long long)pts[0], (long long)pts[1],
                    (long long)expected_lo, (long long)expected_hi);
            return 1;
        }

        if (check(cg_sol_interpolation_order_read(fn, B, Z, s, &os, &ot),
                  "order_read")) return 1;
        if (os != expected_order || ot != 0) {
            fprintf(stderr, "FS%d: order (%d,%d), expected (%d,0)\n",
                    s, os, ot, expected_order);
            return 1;
        }

        printf("  FS%d OK: range=[%lld,%lld] order=(%d,%d)\n",
               s, (long long)pts[0], (long long)pts[1], os, ot);
    }

    if (check(cg_close(fn), "cg_close R")) return 1;
    printf("test_variable_order: PASS\n");
    return 0;
}
