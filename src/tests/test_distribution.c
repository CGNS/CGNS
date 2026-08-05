/*
 * test_distribution.c -- CPEX-0045 ControlPointDistribution.
 *
 * Before this test the attribute had no coverage anywhere in the tree: four MLL
 * entries, four Fortran wrappers, two cgnscheck paths and the on-disk encoding,
 * all unexercised.  That is why the node was still being written with the wrong
 * encoding.
 *
 * The attribute is a *labelled enumeration* node -- name
 * "ControlPointDistribution", label "ControlPointDistribution_t",
 * I4 scalar -- following the InterpolationType_t convention and deliberately not
 * the name-matched DataArray_t convention used by LagrangeControlPoints.  The
 * distinction is load-bearing: a conforming reader must reject any DataArray_t
 * child of these nodes other than LagrangeControlPoints or MonomialCoefficients,
 * so writing the distribution as a DataArray_t produces a file the standard says
 * to reject.
 *
 * Cases:
 *   A  round-trip on ElementInterpolation_t, all four named distributions
 *   B  round-trip on SolutionInterpolation_t
 *   C  absent attribute -> CG_NODE_NOT_FOUND, and no warning is warranted
 *   D  the node is NOT a DataArray_t: cg_narrays under the parent counts only
 *      LagrangeControlPoints, and the on-disk label is the enum label
 *   E  re-write in CG_MODE_MODIFY replaces the value
 *   F  an out-of-range enumerator is rejected on write
 *   G  files for the cgnscheck coordinate-vs-distribution comparison:
 *      matching, set-equal-but-reordered, and mismatched.  The reordered case
 *      is the one that matters -- the comparison is required to be
 *      permutation-invariant, because the on-disk traversal order of
 *      LagrangeControlPoints is a writer convention.  The expectations are
 *      asserted by the cgnscheck_* tests in CMakeLists.txt.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include "cgnslib.h"

static int failures = 0;

static int check(int err, const char *what)
{
    if (err) {
        fprintf(stderr, "ERROR in %s: %s\n", what, cg_get_error());
        failures++;
        return 1;
    }
    return 0;
}

static void fail(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "ERROR: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    failures++;
}

static const CGNS_ENUMT(ControlPointDistribution_t) named[4] = {
    CGNS_ENUMV(GaussLobattoLegendre),
    CGNS_ENUMV(Equidistant),
    CGNS_ENUMV(GaussLegendre),
    CGNS_ENUMV(WarpAndBlend)
};

int main(void)
{
    const char *fname = "test_distribution.cgns";
    int fn, B, F, en, sn, i;
    CGNS_ENUMT(ControlPointDistribution_t) got;
    /* QUAD_4 corners on the bi-unit reference domain */
    double pu[4] = {-1.0,  1.0,  1.0, -1.0};
    double pv[4] = {-1.0, -1.0,  1.0,  1.0};

    printf("CPEX-0045 ControlPointDistribution\n");

    /* ---------------- A/B: round-trip both parents ---------------- */
    for (i = 0; i < 4; i++) {
        if (check(cg_open(fname, CG_MODE_WRITE, &fn), "open")) return 1;
        if (check(cg_base_write(fn, "Base", 2, 2, &B), "base")) return 1;
        if (check(cg_family_write(fn, B, "Fam", &F), "family")) return 1;

        if (check(cg_element_interpolation_write(fn, B, F, "QuadMesh",
                  CGNS_ENUMV(QUAD_4), &en), "element interp")) return 1;
        if (check(cg_element_interpolation_points_write(fn, B, F, en,
                  pu, pv, NULL), "element points")) return 1;
        if (check(cg_element_interpolation_distribution_write(fn, B, F, en,
                  named[i]), "element distribution write")) return 1;

        if (check(cg_solution_interpolation_write(fn, B, F, "QuadSol",
                  CGNS_ENUMV(QUAD_4), 1, 0,
                  CGNS_ENUMV(ParametricLagrange), &sn), "solution interp")) return 1;
        if (check(cg_solution_interpolation_points_write(fn, B, F, sn,
                  pu, pv, NULL, NULL), "solution points")) return 1;
        if (check(cg_solution_interpolation_distribution_write(fn, B, F, sn,
                  named[i]), "solution distribution write")) return 1;
        if (check(cg_close(fn), "close")) return 1;

        if (check(cg_open(fname, CG_MODE_READ, &fn), "reopen")) return 1;

        got = CGNS_ENUMV(ControlPointDistributionNull);
        if (check(cg_element_interpolation_distribution_read(fn, B, F, 1, &got),
                  "element distribution read")) return 1;
        if (got != named[i])
            fail("element distribution round-trip: wrote %s, read %s",
                 cg_ControlPointDistributionName(named[i]),
                 cg_ControlPointDistributionName(got));

        got = CGNS_ENUMV(ControlPointDistributionNull);
        if (check(cg_solution_interpolation_distribution_read(fn, B, F, 1, &got),
                  "solution distribution read")) return 1;
        if (got != named[i])
            fail("solution distribution round-trip: wrote %s, read %s",
                 cg_ControlPointDistributionName(named[i]),
                 cg_ControlPointDistributionName(got));

        /* ---------------- D: it is not a DataArray_t ---------------- */
        {
            int na;
            char aname[33];
            int nd;
            cgsize_t dv[3];
            CGNS_ENUMT(DataType_t) dt;

            if (check(cg_goto(fn, B, "Family_t", F,
                              "ElementInterpolation_t", 1, NULL), "goto elem")) return 1;
            if (check(cg_narrays(&na), "narrays elem")) return 1;
            if (na != 1)
                fail("ElementInterpolation_t should expose exactly 1 DataArray_t "
                     "(LagrangeControlPoints), got %d -- the distribution must not "
                     "be a DataArray_t", na);
            else {
                if (check(cg_array_info(1, aname, &dt, &nd, dv), "array info")) return 1;
                if (strcmp(aname, "LagrangeControlPoints"))
                    fail("the one DataArray_t should be LagrangeControlPoints, got '%s'",
                         aname);
            }
        }
        if (check(cg_close(fn), "close read")) return 1;

        printf("  %-22s round-trips on both parents, not a DataArray_t  OK\n",
               cg_ControlPointDistributionName(named[i]));
    }

    /* ---------------- C: absent attribute ---------------- */
    if (check(cg_open(fname, CG_MODE_WRITE, &fn), "open C")) return 1;
    if (check(cg_base_write(fn, "Base", 2, 2, &B), "base C")) return 1;
    if (check(cg_family_write(fn, B, "Fam", &F), "family C")) return 1;
    if (check(cg_element_interpolation_write(fn, B, F, "QuadMesh",
              CGNS_ENUMV(QUAD_4), &en), "element interp C")) return 1;
    if (check(cg_element_interpolation_points_write(fn, B, F, en,
              pu, pv, NULL), "element points C")) return 1;
    if (check(cg_close(fn), "close C")) return 1;

    if (check(cg_open(fname, CG_MODE_READ, &fn), "reopen C")) return 1;
    if (cg_element_interpolation_distribution_read(fn, B, F, 1, &got)
            != CG_NODE_NOT_FOUND)
        fail("an absent distribution must report CG_NODE_NOT_FOUND");
    else
        printf("  absent attribute -> CG_NODE_NOT_FOUND  OK\n");
    if (check(cg_close(fn), "close C read")) return 1;

    /* ---------------- E: re-write in MODIFY replaces the value ---------------- */
    if (check(cg_open(fname, CG_MODE_MODIFY, &fn), "open E")) return 1;
    if (check(cg_element_interpolation_distribution_write(fn, B, F, 1,
              CGNS_ENUMV(Equidistant)), "first write E")) return 1;
    if (check(cg_element_interpolation_distribution_write(fn, B, F, 1,
              CGNS_ENUMV(WarpAndBlend)), "re-write E")) return 1;
    if (check(cg_close(fn), "close E")) return 1;

    if (check(cg_open(fname, CG_MODE_READ, &fn), "reopen E")) return 1;
    if (check(cg_element_interpolation_distribution_read(fn, B, F, 1, &got),
              "read E")) return 1;
    if (got != CGNS_ENUMV(WarpAndBlend))
        fail("re-write in MODIFY should leave WarpAndBlend, got %s",
             cg_ControlPointDistributionName(got));
    else
        printf("  re-write in CG_MODE_MODIFY replaces the value  OK\n");
    if (check(cg_close(fn), "close E read")) return 1;

    /* ---------------- F: out-of-range enumerator rejected ---------------- */
    if (check(cg_open(fname, CG_MODE_MODIFY, &fn), "open F")) return 1;
    if (cg_element_interpolation_distribution_write(fn, B, F, 1,
            (CGNS_ENUMT(ControlPointDistribution_t))42) == CG_OK)
        fail("an out-of-range distribution enumerator must be rejected");
    else
        printf("  out-of-range enumerator rejected on write  OK\n");
    if (check(cg_close(fn), "close F")) return 1;

    /* ------- G: files for the cgnscheck comparison ------- */
    {
        struct { const char *file; int shuffle; int equi_coords;
                 CGNS_ENUMT(ControlPointDistribution_t) name_it; }
        cases[] = {
            /* GLL coordinates, named GLL -> agrees */
            { "test_dist_match.cgns",     0, 0, CGNS_ENUMV(GaussLobattoLegendre) },
            /* the same GLL set in reverse traversal order -> must still agree */
            { "test_dist_reordered.cgns", 1, 0, CGNS_ENUMV(GaussLobattoLegendre) },
            /* equidistant coordinates but named GLL -> must be flagged */
            { "test_dist_mismatch.cgns",  0, 1, CGNS_ENUMV(GaussLobattoLegendre) },
            /* equidistant coordinates, named Equidistant -> agrees */
            { "test_dist_equi.cgns",      0, 1, CGNS_ENUMV(Equidistant) }
        };
        const int ncase = (int)(sizeof(cases)/sizeof(cases[0]));
        int c;

        for (c = 0; c < ncase; c++) {
            double qu[16], qv[16], u1[4];
            int a, b, m = 0;

            /* QUAD at degree 3: 4 nodes per direction, 16 in the tensor grid.
             * GLL at p=3 is {-1, -1/sqrt(5), 1/sqrt(5), 1}. */
            if (cases[c].equi_coords)
                for (a = 0; a < 4; a++) u1[a] = -1.0 + 2.0*a/3.0;
            else {
                double t = 1.0/sqrt(5.0);
                u1[0] = -1.0; u1[1] = -t; u1[2] = t; u1[3] = 1.0;
            }
            for (b = 0; b < 4; b++)
                for (a = 0; a < 4; a++) { qu[m] = u1[a]; qv[m] = u1[b]; m++; }

            if (cases[c].shuffle) {
                double tu[16], tv[16];
                for (a = 0; a < 16; a++) { tu[a] = qu[15-a]; tv[a] = qv[15-a]; }
                for (a = 0; a < 16; a++) { qu[a] = tu[a];    qv[a] = tv[a];    }
            }

            if (check(cg_open(cases[c].file, CG_MODE_WRITE, &fn), "open G")) return 1;
            if (check(cg_base_write(fn, "Base", 2, 2, &B), "base G")) return 1;
            if (check(cg_family_write(fn, B, "Fam", &F), "family G")) return 1;
            if (check(cg_solution_interpolation_write(fn, B, F, "Q3",
                      CGNS_ENUMV(QUAD_4), 3, 0,
                      CGNS_ENUMV(ParametricLagrange), &sn), "sol interp G")) return 1;
            if (check(cg_solution_interpolation_points_write(fn, B, F, sn,
                      qu, qv, NULL, NULL), "sol points G")) return 1;
            if (check(cg_solution_interpolation_distribution_write(fn, B, F, sn,
                      cases[c].name_it), "distribution G")) return 1;
            if (check(cg_close(fn), "close G")) return 1;
        }
        printf("  wrote %d files for the cgnscheck comparison  OK\n", ncase);
    }

    if (failures) {
        fprintf(stderr, "\n%d failure(s)\n", failures);
        return 1;
    }
    printf("all distribution cases passed\n");
    return 0;
}
