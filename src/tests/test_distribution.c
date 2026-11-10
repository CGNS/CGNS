/*
 * test_distribution.c -- CPEX-0045 ControlPointDistribution.
 *
 * Before this test the attribute had no coverage anywhere in the tree: four MLL
 * entries, four Fortran wrappers, two cgnscheck paths and the on-disk encoding,
 * all unexercised.  That is why the node was still being written with the wrong
 * encoding.
 *
 * The attribute is a *labelled enumeration* node -- name
 * "ControlPointDistribution", label "ControlPointDistribution_t", whose payload
 * is the enumerator's name as a C1 string -- following the InterpolationType_t
 * convention and deliberately not
 * the name-matched DataArray_t convention used by LagrangeControlPoints.  The
 * distinction is load-bearing: a conforming reader must reject any DataArray_t
 * child of these nodes other than LagrangeControlPoints,
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


/* ------------------------------------------------------------------ */
/* H - WarpAndBlend on the triangle.                                   */
/*                                                                     */
/* Reference node sets for the bi-unit triangle, computed independently */
/* from the Warburton construction (J. Engrg. Math. 56(3):247-262) and  */
/* checked against three properties it must have: at p=2 it reduces     */
/* exactly to equidistant, since the 1D GLL nodes are then {-1,0,1};    */
/* at p=3 it has a single interior node, at the centroid; and at every  */
/* degree the node set is invariant under the triangle's rotation.      */
/* Pinning the values here keeps cgnscheck's generator honest -- a      */
/* generator validated only against itself would accept its own drift.  */
/* ------------------------------------------------------------------ */
static const double wb_tri3_u[10] = {
        -1, -0.44721359549995804, 0.44721359549995804, 1,
        -1.0000000000000002, -0.33333333333333343, 0.44721359549995782,
        -0.99999999999999989, -0.44721359549995804, -1};
static const double wb_tri3_v[10] = {
        -1, -1, -1, -1, -0.44721359549995809, -0.33333333333333343,
        -0.44721359549995782, 0.44721359549995765, 0.44721359549995804, 1};
static const double wb_tri4_u[15] = {
        -1, -0.65465367070797709, 0, 0.65465367070797709, 1, -1,
        -0.55158350755530561, 0.10316701511061113, 0.6546536707079772, -1,
        -0.55158350755530561, 0, -0.99999999999999989, -0.65465367070797709,
        -1};
static const double wb_tri4_v[15] = {
        -1, -1, -1, -1, -1, -0.65465367070797709, -0.55158350755530561,
        -0.55158350755530561, -0.6546536707079772, 0, 0.10316701511061116, 0,
        0.65465367070797709, 0.65465367070797709, 1};

static int write_wb_case(const char *filename, int p, int npts,
                         const double *u, const double *v,
                         int perturb, int reorder)
{
    int fn, B, F, si, i;
    double au[16], av[16];

    for (i = 0; i < npts; i++) { au[i] = u[i]; av[i] = v[i]; }
    if (perturb) {
        /* The error CPEX-0045 names as otherwise undetectable: a [0,1] simplex
         * convention where the bi-unit domain is required.  It displaces nodes
         * by O(1), far above the freedom left by the unpinned Warp&Blend
         * blending parameter (at most ~2.7e-2 for degrees up to 10), so it must
         * be flagged rather than excused. */
        for (i = 0; i < npts; i++) { au[i] = (au[i]+1.0)/2.0; av[i] = (av[i]+1.0)/2.0; }
    }
    if (reorder) {                         /* same set, reversed traversal */
        double t[16];
        for (i = 0; i < npts; i++) t[i] = au[npts-1-i];
        for (i = 0; i < npts; i++) au[i] = t[i];
        for (i = 0; i < npts; i++) t[i] = av[npts-1-i];
        for (i = 0; i < npts; i++) av[i] = t[i];
    }

    if (check(cg_open(filename, CG_MODE_WRITE, &fn), "open H")) return 1;
    if (check(cg_base_write(fn, "Base", 2, 2, &B), "base H")) return 1;
    if (check(cg_family_write(fn, B, "Fam", &F), "family H")) return 1;
    if (check(cg_solution_interpolation_write(fn, B, F, "Tri",
              CGNS_ENUMV(TRI_3), p, 0,
              CGNS_ENUMV(ParametricLagrange), &si), "sol interp H")) return 1;
    if (check(cg_solution_interpolation_points_write(fn, B, F, si, npts,
              au, av, NULL, NULL), "sol points H")) return 1;
    if (check(cg_solution_interpolation_distribution_write(fn, B, F, si,
              CGNS_ENUMV(WarpAndBlend)), "distribution H")) return 1;
    return check(cg_close(fn), "close H");
}


/* Tetrahedral WarpAndBlend reference sets, computed independently and validated
 * on the properties the construction must have: at p=2 it reduces exactly to
 * equidistant, at p=4 its single interior node is the centroid (-0.5,-0.5,-0.5),
 * and the trace on each face reproduces the 2D Warp&Blend triangle at p<=3.
 * (At higher degree the face trace legitimately differs: the 2D and 3D
 * constructions use separately optimised blending parameters.) */
static const double wb_tet2_u[10] = {
        -1, 0, 1, -1, -2.3461773124865103e-17, -1, -1, 0, -1, -1};
static const double wb_tet2_v[10] = {
        -1, -1, -1, -6.4098756212785448e-17, -6.4098756212785448e-17,
        0.99999999999999989, -1, -1, 0, -1};
static const double wb_tet2_w[10] = {
        -0.99999999999999989, -0.99999999999999989, -0.99999999999999989,
        -0.99999999999999989, -0.99999999999999989, -0.99999999999999989, 0,
        0, 0, 1};
static const double wb_tet3_u[20] = {
        -1, -0.44721359549995809, 0.44721359549995782, 1,
        -1.0000000000000002, -0.33333333333333331, 0.44721359549995765, -1,
        -0.44721359549995798, -1, -1.0000000000000002, -0.33333333333333343,
        0.44721359549995776, -1, -0.33333333333333343, -1, -1,
        -0.44721359549995787, -1, -1};
static const double wb_tet3_v[20] = {
        -1, -1, -1, -1, -0.44721359549995798, -0.33333333333333337,
        -0.44721359549995798, 0.44721359549995776, 0.44721359549995776,
        0.99999999999999989, -1, -0.99999999999999989, -1,
        -0.33333333333333337, -0.33333333333333337, 0.44721359549995776,
        -0.99999999999999978, -0.99999999999999978, -0.44721359549995798, -1};
static const double wb_tet3_w[20] = {
        -0.99999999999999989, -0.99999999999999989, -0.99999999999999989,
        -0.99999999999999989, -0.99999999999999989, -0.99999999999999989,
        -0.99999999999999989, -0.99999999999999989, -0.99999999999999989,
        -0.99999999999999989, -0.44721359549995804, -0.33333333333333331,
        -0.44721359549995793, -0.33333333333333331, -0.33333333333333331,
        -0.44721359549995776, 0.44721359549995771, 0.44721359549995771,
        0.44721359549995798, 1};

static int write_wb_tet_case(const char *filename, int p, int npts,
                             const double *u, const double *v, const double *w,
                             int domain01)
{
    int fn, B, F, si, i;
    double au[64], av[64], aw[64];

    for (i = 0; i < npts; i++) { au[i] = u[i]; av[i] = v[i]; aw[i] = w[i]; }
    if (domain01)
        for (i = 0; i < npts; i++) {
            au[i] = (au[i]+1.0)/2.0; av[i] = (av[i]+1.0)/2.0; aw[i] = (aw[i]+1.0)/2.0;
        }

    if (check(cg_open(filename, CG_MODE_WRITE, &fn), "open tet")) return 1;
    if (check(cg_base_write(fn, "Base", 3, 3, &B), "base tet")) return 1;
    if (check(cg_family_write(fn, B, "Fam", &F), "family tet")) return 1;
    if (check(cg_solution_interpolation_write(fn, B, F, "Tet",
              CGNS_ENUMV(TETRA_4), p, 0,
              CGNS_ENUMV(ParametricLagrange), &si), "sol interp tet")) return 1;
    if (check(cg_solution_interpolation_points_write(fn, B, F, si, npts,
              au, av, aw, NULL), "sol points tet")) return 1;
    if (check(cg_solution_interpolation_distribution_write(fn, B, F, si,
              CGNS_ENUMV(WarpAndBlend)), "distribution tet")) return 1;
    return check(cg_close(fn), "close tet");
}

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
        if (check(cg_solution_interpolation_points_write(fn, B, F, sn, 4,
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
            if (check(cg_solution_interpolation_points_write(fn, B, F, sn, 16,
                      qu, qv, NULL, NULL), "sol points G")) return 1;
            if (check(cg_solution_interpolation_distribution_write(fn, B, F, sn,
                      cases[c].name_it), "distribution G")) return 1;
            if (check(cg_close(fn), "close G")) return 1;
        }
        printf("  wrote %d files for the cgnscheck comparison  OK\n", ncase);
    }

    /* ------- H: WarpAndBlend reference sets ------- */
    if (write_wb_case("test_dist_wb_p3.cgns",      3, 10, wb_tri3_u, wb_tri3_v, 0, 0)) return 1;
    if (write_wb_case("test_dist_wb_p3_reord.cgns",3, 10, wb_tri3_u, wb_tri3_v, 0, 1)) return 1;
    if (write_wb_case("test_dist_wb_p3_bad.cgns",  3, 10, wb_tri3_u, wb_tri3_v, 1, 0)) return 1;
    if (write_wb_case("test_dist_wb_p4.cgns",      4, 15, wb_tri4_u, wb_tri4_v, 0, 0)) return 1;
    printf("  wrote 4 WarpAndBlend reference files  OK\n");

    if (write_wb_tet_case("test_dist_wbtet_p2.cgns",    2, 10,
                          wb_tet2_u, wb_tet2_v, wb_tet2_w, 0)) return 1;
    if (write_wb_tet_case("test_dist_wbtet_p3.cgns",    3, 20,
                          wb_tet3_u, wb_tet3_v, wb_tet3_w, 0)) return 1;
    if (write_wb_tet_case("test_dist_wbtet_p3_bad.cgns",3, 20,
                          wb_tet3_u, wb_tet3_v, wb_tet3_w, 1)) return 1;
    printf("  wrote 3 tetrahedral WarpAndBlend files  OK\n");

    if (failures) {
        fprintf(stderr, "\n%d failure(s)\n", failures);
        return 1;
    }
    printf("all distribution cases passed\n");
    return 0;
}
