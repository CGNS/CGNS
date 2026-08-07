/*
 * test_degree_zero.c -- CPEX-0045 v3: degree zero is valid.
 *
 * The standard states that SpatialDegree = 0 denotes a single spatial degree of
 * freedom per element -- a solution constant over the element -- and "must be
 * accepted"; that TemporalDegree > 0 with SpatialDegree = 0 is also valid and
 * describes a per-element value constant in space and varying in time; and that
 * "neither the writer nor cgnscheck may reject these configurations".
 *
 * Nothing in the suite exercised any of this.  The library used to reject it in
 * two independent places: an explicit coupling check in the degree writer, and
 * a cg_npe_ho() lower bound of 1 that every sizing path funnelled through --
 * which made a degree-0 file unwritable AND unreadable (the field-size check
 * runs at file open).
 *
 * Cases covered:
 *   A  (p,q) = (0,0) modal   -- N_DOFs = C(0+2,2) = 1 per element
 *   B  (p,q) = (0,0) Lagrange-- N_DOFs = 1 control point, unisolvent for P0
 *   C  (p,q) = (0,2) modal   -- N_DOFs = 1 * (q+1) = 3 per element
 * Each is written, closed, reopened (exercising the reader's size check) and
 * the field length verified against the spec rule.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"

#define NELEM 4
#define NVERT 9

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

/* Build a 2x2 QUAD_4 grid, a family carrying one SolutionInterpolation_t at
 * (spatialDegree, temporalDegree), and a high-order FlowSolution_t whose field
 * has the length the spec prescribes.  Returns the field length written. */
static cgsize_t write_case(const char *filename, int p, int q,
                           CGNS_ENUMT(InterpolationType_t) it,
                           cgsize_t expect_ndofs)
{
    int fn, B, Z, F, S, si, ci, sec, fi;
    cgsize_t size[3], conn[NELEM * 4];
    double x[NVERT], y[NVERT];
    double *fld;
    cgsize_t len;
    int i, j, k;

    for (j = 0, k = 0; j < 3; j++)
        for (i = 0; i < 3; i++, k++) { x[k] = (double)i; y[k] = (double)j; }
    for (j = 0, k = 0; j < 2; j++)
        for (i = 0; i < 2; i++) {
            int v = j * 3 + i + 1;
            conn[k++] = v; conn[k++] = v + 1; conn[k++] = v + 4; conn[k++] = v + 3;
        }

    if (check(cg_open(filename, CG_MODE_WRITE, &fn), "open")) return -1;
    if (check(cg_base_write(fn, "Base", 2, 2, &B), "base")) return -1;
    size[0] = NVERT; size[1] = NELEM; size[2] = 0;
    if (check(cg_zone_write(fn, B, "Zone", size, CGNS_ENUMV(Unstructured), &Z), "zone")) return -1;
    if (check(cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", x, &ci), "coordX")) return -1;
    if (check(cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", y, &ci), "coordY")) return -1;
    if (check(cg_section_write(fn, B, Z, "Elem", CGNS_ENUMV(QUAD_4), 1, NELEM, 0, conn, &sec), "section")) return -1;

    if (check(cg_family_write(fn, B, "Fam", &F), "family")) return -1;
    if (check(cg_goto(fn, B, "Zone_t", Z, NULL), "goto zone")) return -1;
    if (check(cg_famname_write("Fam"), "famname")) return -1;

    if (check(cg_solution_interpolation_write(fn, B, F, "QUAD_deg0",
              CGNS_ENUMV(QUAD_4), p, q, it, &si), "solution interpolation")) return -1;

    if (it == CGNS_ENUMV(ParametricLagrange)) {
        /* One control point: its nodal function is identically one, and a
         * single point is unisolvent for P0. */
        int npts = 0;
        double pu[8], pv[8];
        if (check(cg_solution_lagrange_interpolation_size(CGNS_ENUMV(QUAD_4), p, q, &npts),
                  "lagrange size at degree 0")) return -1;
        if (npts != (q + 1)) {
            fprintf(stderr, "ERROR: lagrange size at (p=%d,q=%d) = %d, expected %d\n",
                    p, q, (int)npts, q + 1);
            failures++;
            return -1;
        }
        for (i = 0; i < (int)npts; i++) { pu[i] = 0.0; pv[i] = 0.0; }
        if (check(cg_solution_interpolation_points_write(fn, B, F, si, pu, pv, NULL,
                  q > 0 ? pv : NULL), "lagrange points at degree 0")) return -1;
    } else {
        int nmodal = 0;
        if (check(cg_solution_monomial_size(CGNS_ENUMV(QUAD_4), p, q, &nmodal),
                  "monomial size at degree 0")) return -1;
        if (nmodal != (q + 1)) {
            fprintf(stderr, "ERROR: monomial size at (p=%d,q=%d) = %d, expected %d\n",
                    p, q, (int)nmodal, q + 1);
            failures++;
            return -1;
        }
        /* A modal basis stores no array (CPEX-0045 withdraws
         * MonomialCoefficients): the cardinality checked above is the whole of
         * what the degree-0 modal case has to assert. */
    }

    if (check(cg_sol_write(fn, B, Z, "FS", CGNS_ENUMV(InterpolationPoints), &S), "sol")) return -1;
    /* The writer must accept degree 0, including with a non-zero temporal
     * degree -- no constraint couples the two. */
    if (check(cg_sol_interpolation_degree_write(fn, B, Z, S, p, q), "degree write")) return -1;

    len = (cgsize_t)NELEM * expect_ndofs;
    fld = (double *)malloc((size_t)len * sizeof(double));
    for (i = 0; i < (int)len; i++) fld[i] = (double)i;
    if (check(cg_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "Density", fld, &fi),
              "field write")) { free(fld); return -1; }
    free(fld);
    if (check(cg_close(fn), "close")) return -1;
    return len;
}

/* Reopen (runs the reader's field-size check) and confirm the stored length. */
static void verify_case(const char *filename, const char *label, cgsize_t expect_len,
                        int p, int q)
{
    int fn, nf, sd, td, nd;
    char nm[33];
    cgsize_t dv[3];
    CGNS_ENUMT(DataType_t) dt;

    if (check(cg_open(filename, CG_MODE_READ, &fn), "reopen")) return;

    if (check(cg_sol_interpolation_degree_read(fn, 1, 1, 1, &sd, &td), "degree read")) {
        cg_close(fn); return;
    }
    if (sd != p || td != q) {
        fprintf(stderr, "ERROR [%s]: degrees read (%d,%d), expected (%d,%d)\n",
                label, sd, td, p, q);
        failures++;
    }

    if (check(cg_goto(fn, 1, "Zone_t", 1, "FlowSolution_t", 1, NULL), "goto FS")) {
        cg_close(fn); return;
    }
    if (check(cg_narrays(&nf), "narrays")) { cg_close(fn); return; }
    if (check(cg_array_info(1, nm, &dt, &nd, dv), "array info")) { cg_close(fn); return; }

    if (dv[0] != expect_len) {
        fprintf(stderr, "ERROR [%s]: field length %d on disk, spec requires %d\n",
                label, (int)dv[0], (int)expect_len);
        failures++;
    } else {
        printf("  %-28s field length %d  OK\n", label, (int)dv[0]);
    }
    cg_close(fn);
}

int main(void)
{
    cgsize_t len;

    printf("CPEX-0045 degree-zero validity\n");

    /* A: modal, (p,q) = (0,0) -> C(0+2,2) = 1 DOF per element */
    len = write_case("test_degree_zero_modal.cgns", 0, 0,
                     CGNS_ENUMV(ParametricMonomialsPascal), 1);
    if (len > 0) verify_case("test_degree_zero_modal.cgns",
                             "modal (p=0,q=0)", len, 0, 0);

    /* B: Lagrange, (p,q) = (0,0) -> 1 control point */
    len = write_case("test_degree_zero_lagrange.cgns", 0, 0,
                     CGNS_ENUMV(ParametricLagrange), 1);
    if (len > 0) verify_case("test_degree_zero_lagrange.cgns",
                             "lagrange (p=0,q=0)", len, 0, 0);

    /* C: modal, (p,q) = (0,2) -> 1 * (2+1) = 3 DOFs per element.
     * Constant in space, varying in time: an unsteady finite-volume solution. */
    len = write_case("test_degree_zero_spacetime.cgns", 0, 2,
                     CGNS_ENUMV(ParametricMonomialsPascal), 3);
    if (len > 0) verify_case("test_degree_zero_spacetime.cgns",
                             "modal (p=0,q=2)", len, 0, 2);

    if (failures) {
        fprintf(stderr, "\n%d failure(s)\n", failures);
        return 1;
    }
    printf("all degree-zero cases passed\n");
    return 0;
}
