/*
 * CPEX-0045 v4 CharacteristicLength tests.
 *
 * CharacteristicLength records the per-element coordinate normalisation used
 * for CartesianMonomialsPascal interpolation. It is stored inside the
 * InterpolationMetadata UserDefinedData_t child of the FlowSolution_t, never
 * as a DataArray_t child of the FlowSolution_t itself: those are the solution
 * fields, the library enumerates them by label, and an array stored there is
 * counted by cg_nfields and rejected by the field size check.
 *
 * Two encodings are normative and are distinguished by array rank:
 *
 *   isotropic : R8, 1-D, [numElements]           (nscale == 1)
 *   per-axis  : R8, 2-D, [nscale, numElements]   (nscale == PhysDim)
 *
 * The per-axis form matches the directional non-dimensionalisation used by
 * Taylor-basis DG solvers and is what keeps the modal mass matrix well
 * conditioned on high-aspect-ratio cells.
 *
 * Coverage:
 *   A - isotropic round-trip
 *   B - per-axis round-trip, including a deliberately high-aspect-ratio cell
 *   C - shape-only query (h_e == NULL)
 *   D - absent node returns CG_NODE_NOT_FOUND
 *   E - input validation (bad nscale, non-positive factors)
 *   F - re-write rejected in CG_MODE_WRITE, accepted in CG_MODE_MODIFY
 *   G - the node is not a FlowSolution field and is not even a DataArray_t
 *       child of it; the file reopens under field size checking
 *   H - the superseded v3 layout (array directly under FlowSolution_t) is
 *       rejected on reopen
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cgnslib.h"

#define N_ELEM  4
#define N_VERT  (N_ELEM * 8)
#define PHYSDIM 3
/* This block uses CartesianMonomialsPascal, so N_DOFs is the Pascal-space
 * cardinality C(p+d,d) = C(2+3,3) = 10 -- NOT the nodal count 27.  The two
 * differ for every modal basis, and the field-array length rule is defined in
 * terms of the interpolation type actually declared. */
#define N_DOF   10              /* C(2+3,3), HEXA modal at degree 2 */

static int check(int err, const char *what)
{
    if (err) {
        fprintf(stderr, "ERROR in %s: %s\n", what, cg_get_error());
        return 1;
    }
    return 0;
}

/* Create an unstructured base/zone/family carrying one Cartesian-modal
 * SolutionInterpolation_t and one high-order FlowSolution_t.
 *
 * The zone deliberately contains grid coordinates, a real element section and
 * a correctly sized field array. Those are what enable the library's
 * FlowSolution field size checking on reopen: without an element section the
 * check is silently skipped, which would let a CharacteristicLength node that
 * is wrongly treated as a solution field go unnoticed.
 */
static int make_file(const char *filename, int *fn, int *B, int *Z, int *S)
{
    int F, si, sec, ci, fi;
    cgsize_t size[3];
    cgsize_t conn[N_ELEM * 8];
    double coord[N_VERT];
    double fld[N_ELEM * N_DOF];
    int i;

    for (i = 0; i < N_VERT; i++)        coord[i] = (double)i;
    for (i = 0; i < N_ELEM * 8; i++)    conn[i]  = (i % N_VERT) + 1;
    for (i = 0; i < N_ELEM * N_DOF; i++) fld[i]  = (double)i;

    if (check(cg_open(filename, CG_MODE_WRITE, fn), "open W"))  return 1;
    if (check(cg_base_write(*fn, "Base", 3, PHYSDIM, B), "base")) return 1;
    size[0] = N_VERT; size[1] = N_ELEM; size[2] = 0;
    if (check(cg_zone_write(*fn, *B, "Zone", size,
                            CGNS_ENUMV(Unstructured), Z), "zone")) return 1;
    if (check(cg_coord_write(*fn, *B, *Z, CGNS_ENUMV(RealDouble),
              "CoordinateX", coord, &ci), "coordX")) return 1;
    if (check(cg_coord_write(*fn, *B, *Z, CGNS_ENUMV(RealDouble),
              "CoordinateY", coord, &ci), "coordY")) return 1;
    if (check(cg_coord_write(*fn, *B, *Z, CGNS_ENUMV(RealDouble),
              "CoordinateZ", coord, &ci), "coordZ")) return 1;
    if (check(cg_section_write(*fn, *B, *Z, "Hexas", CGNS_ENUMV(HEXA_8),
              1, N_ELEM, 0, conn, &sec), "section")) return 1;
    if (check(cg_family_write(*fn, *B, "CartFam", &F), "family")) return 1;
    /* The zone must name the family: the high-order field length is derived
     * from the SolutionInterpolation_t reached through FamilyName_t. */
    if (check(cg_goto(*fn, *B, "Zone_t", *Z, NULL), "goto zone")) return 1;
    if (check(cg_famname_write("CartFam"), "famname")) return 1;
    if (check(cg_solution_interpolation_write(*fn, *B, F, "Hex_P2",
              CGNS_ENUMV(HEXA_8), 2, 0,
              CGNS_ENUMV(CartesianMonomialsPascal), &si), "SI")) return 1;
    /* Whole-zone high-order block: |E| is every element of the zone, so the
     * CharacteristicLength length equals N_ELEM. Subset (PointRange /
     * PointList) blocks are covered by test_variable_order. */
    if (check(cg_sol_write(*fn, *B, *Z, "FS",
              CGNS_ENUMV(InterpolationPoints), S), "sol")) return 1;
    if (check(cg_sol_interpolation_degree_write(*fn, *B, *Z, *S, 2, 0),
              "order")) return 1;
    if (check(cg_field_write(*fn, *B, *Z, *S, CGNS_ENUMV(RealDouble),
              "Density", fld, &fi), "field")) return 1;
    return 0;
}

/* ------------------------------------------------------------------ */
/* G - CharacteristicLength must not be exposed as a solution field    */
/* ------------------------------------------------------------------ */
static int test_not_a_field(void)
{
    const char *filename = "test_charlen_field.cgns";
    int fn, B, Z, S, nf, i, narr;
    double h[PHYSDIM * N_ELEM];

    printf("\n--- G: CharacteristicLength is not a solution field ---\n");

    for (i = 0; i < PHYSDIM * N_ELEM; i++) h[i] = 1.0 + i;

    if (make_file(filename, &fn, &B, &Z, &S)) return 1;
    if (check(cg_sol_characteristic_length_write(fn, B, Z, S, PHYSDIM,
                                                 N_ELEM, h), "write")) return 1;
    if (check(cg_close(fn), "close W")) return 1;

    /* Reopening exercises the field size check. A CharacteristicLength node
     * mistaken for a field fails that check and makes the file unreadable. */
    if (check(cg_open(filename, CG_MODE_READ, &fn), "reopen with elements"))
        return 1;

    if (check(cg_nfields(fn, B, Z, 1, &nf), "nfields")) return 1;
    if (nf != 1) {
        fprintf(stderr, "ERROR: expected exactly 1 solution field, got %d "
                        "(CharacteristicLength must not be one)\n", nf);
        return 1;
    }
    {
        char fname[33];
        CGNS_ENUMT(DataType_t) dt;
        if (check(cg_field_info(fn, B, Z, 1, 1, &dt, fname), "field_info"))
            return 1;
        if (strcmp(fname, "Density") != 0) {
            fprintf(stderr, "ERROR: field 1 is \"%s\", expected \"Density\"\n",
                    fname);
            return 1;
        }
    }

    /* The v4 guarantee is structural, not a filtered field count: the array
     * must not be a DataArray_t child of the FlowSolution_t at all. Checking
     * cg_nfields alone would still pass if the library were filtering by
     * name, so walk the raw children too. */
    if (check(cg_goto(fn, B, "Zone_t", Z, "FlowSolution_t", 1, "end"),
              "goto sol")) return 1;
    if (check(cg_narrays(&narr), "narrays")) return 1;
    if (narr != 1) {
        fprintf(stderr, "ERROR: FlowSolution_t has %d DataArray_t children, "
                        "expected 1 (Density only)\n", narr);
        return 1;
    }

    /* ...and it must be present in the container, not merely missing.
     * cg_user_data_read reads a child of the current node, so it is called
     * at the FlowSolution_t; the goto below then descends into it. */
    {
        char uname[33];
        int nud;
        if (check(cg_nuser_data(&nud), "nuser_data")) return 1;
        if (nud != 1) {
            fprintf(stderr, "ERROR: FlowSolution_t has %d UserDefinedData_t "
                            "children, expected 1\n", nud);
            return 1;
        }
        if (check(cg_user_data_read(1, uname), "user_data_read")) return 1;
        if (strcmp(uname, "InterpolationMetadata") != 0) {
            fprintf(stderr, "ERROR: container is \"%s\", expected "
                            "\"InterpolationMetadata\"\n", uname);
            return 1;
        }
    }
    if (check(cg_goto(fn, B, "Zone_t", Z, "FlowSolution_t", 1,
                      "UserDefinedData_t", 1, "end"), "goto container"))
        return 1;
    if (check(cg_narrays(&narr), "narrays in container")) return 1;
    if (narr != 1) {
        fprintf(stderr, "ERROR: InterpolationMetadata has %d arrays, "
                        "expected 1\n", narr);
        return 1;
    }
    {
        char aname[33];
        CGNS_ENUMT(DataType_t) adt;
        int andim;
        cgsize_t adims[12];
        if (check(cg_array_info(1, aname, &adt, &andim, adims), "array_info"))
            return 1;
        if (strcmp(aname, "CharacteristicLength") != 0) {
            fprintf(stderr, "ERROR: container holds \"%s\", expected "
                            "\"CharacteristicLength\"\n", aname);
            return 1;
        }
    }
    if (check(cg_close(fn), "close R")) return 1;

    printf("  file with element sections reopens cleanly\n");
    printf("  cg_nfields reports 1 field (Density)\n");
    printf("  FlowSolution_t has 1 DataArray_t child; metadata is in the "
           "container\n");
    return 0;
}

/* ------------------------------------------------------------------ */
/* H - the superseded v3 layout must be rejected                       */
/* ------------------------------------------------------------------ */
static int test_v3_layout_rejected(void)
{
    const char *filename = "test_charlen_v3layout.cgns";
    int fn, B, Z, S, i;
    cgsize_t dims[2];
    double h[PHYSDIM * N_ELEM];

    printf("\n--- H: v3 layout (array under FlowSolution_t) is rejected ---\n");

    for (i = 0; i < PHYSDIM * N_ELEM; i++) h[i] = 1.0 + i;

    /* Hand-build the superseded layout: the accessor will not produce it, so
     * write the array directly under the FlowSolution_t via cg_array_write. */
    if (make_file(filename, &fn, &B, &Z, &S)) return 1;
    if (check(cg_goto(fn, B, "Zone_t", Z, "FlowSolution_t", S, "end"),
              "goto sol")) return 1;
    dims[0] = PHYSDIM;
    dims[1] = N_ELEM;
    if (check(cg_array_write("CharacteristicLength", CGNS_ENUMV(RealDouble),
                             2, dims, h), "array_write")) return 1;
    if (check(cg_close(fn), "close W")) return 1;

    /* The array is now one of the FlowSolution_t's DataArray_t children, so
     * the field size check must reject it. Under v3 this file was accepted
     * only because the library filtered the name out first. */
    if (cg_open(filename, CG_MODE_READ, &fn) == CG_OK) {
        fprintf(stderr, "ERROR: file with the v3 CharacteristicLength layout "
                        "opened successfully; it must be rejected\n");
        cg_close(fn);
        return 1;
    }

    printf("  rejected on reopen: %s\n", cg_get_error());
    return 0;
}

/* ------------------------------------------------------------------ */
/* A - isotropic round-trip                                            */
/* ------------------------------------------------------------------ */
static int test_isotropic(void)
{
    const char *filename = "test_charlen_iso.cgns";
    int fn, B, Z, S, nscale;
    cgsize_t n;
    double h[N_ELEM]  = {1.0, 2.5, 0.125, 1000.0};
    double back[N_ELEM];
    int i;

    printf("\n--- A: isotropic CharacteristicLength ---\n");

    if (make_file(filename, &fn, &B, &Z, &S)) return 1;
    if (check(cg_sol_characteristic_length_write(fn, B, Z, S, 1, N_ELEM, h),
              "write iso")) return 1;
    if (check(cg_close(fn), "close W")) return 1;

    if (check(cg_open(filename, CG_MODE_READ, &fn), "open R")) return 1;
    if (check(cg_sol_characteristic_length_read(fn, B, Z, S, &nscale, &n, back),
              "read iso")) return 1;

    if (nscale != 1) {
        fprintf(stderr, "ERROR: expected nscale=1, got %d\n", nscale);
        return 1;
    }
    if (n != N_ELEM) {
        fprintf(stderr, "ERROR: expected %d elements, got %ld\n",
                N_ELEM, (long)n);
        return 1;
    }
    for (i = 0; i < N_ELEM; i++) {
        if (fabs(h[i] - back[i]) > 1.e-14) {
            fprintf(stderr, "ERROR: h[%d] %g != %g\n", i, h[i], back[i]);
            return 1;
        }
    }
    if (check(cg_close(fn), "close R")) return 1;

    printf("  isotropic round-trip OK (nscale=1, %d elements)\n", N_ELEM);
    return 0;
}

/* ------------------------------------------------------------------ */
/* B - per-axis round-trip                                             */
/* ------------------------------------------------------------------ */
static int test_per_axis(void)
{
    const char *filename = "test_charlen_axis.cgns";
    int fn, B, Z, S, nscale;
    cgsize_t n;
    int i;
    /* Layout is [nscale, numElements] with nscale fast-varying, so each
     * element's factors are contiguous. Element 2 is a 1e4:1 aspect-ratio
     * boundary-layer-like cell, which is exactly the case an isotropic
     * scalar cannot condition. */
    double h[PHYSDIM * N_ELEM] = {
        1.0,    1.0,    1.0,      /* elem 1: unit cube          */
        1.0e+4, 1.0,    1.0e+4,   /* elem 2: thin in y          */
        2.0,    3.0,    4.0,      /* elem 3: mildly anisotropic */
        1.0e-6, 1.0e-6, 1.0e-6    /* elem 4: micro-scale        */
    };
    double back[PHYSDIM * N_ELEM];

    printf("\n--- B: per-axis CharacteristicLength ---\n");

    if (make_file(filename, &fn, &B, &Z, &S)) return 1;
    if (check(cg_sol_characteristic_length_write(fn, B, Z, S, PHYSDIM,
                                                 N_ELEM, h),
              "write per-axis")) return 1;
    if (check(cg_close(fn), "close W")) return 1;

    if (check(cg_open(filename, CG_MODE_READ, &fn), "open R")) return 1;
    if (check(cg_sol_characteristic_length_read(fn, B, Z, S, &nscale, &n, back),
              "read per-axis")) return 1;

    if (nscale != PHYSDIM) {
        fprintf(stderr, "ERROR: expected nscale=%d, got %d\n", PHYSDIM, nscale);
        return 1;
    }
    if (n != N_ELEM) {
        fprintf(stderr, "ERROR: expected %d elements, got %ld\n",
                N_ELEM, (long)n);
        return 1;
    }
    for (i = 0; i < PHYSDIM * N_ELEM; i++) {
        /* Relative comparison: values span 1e-6 to 1e4. */
        if (fabs(h[i] - back[i]) > 1.e-12 * fabs(h[i])) {
            fprintf(stderr, "ERROR: h[%d] %g != %g\n", i, h[i], back[i]);
            return 1;
        }
    }
    if (check(cg_close(fn), "close R")) return 1;

    printf("  per-axis round-trip OK (nscale=%d, %d elements,\n", PHYSDIM, N_ELEM);
    printf("  including a 1e4:1 aspect-ratio cell and a 1e-6 micro-scale cell)\n");
    return 0;
}

/* ------------------------------------------------------------------ */
/* C - shape-only query, D - absent node                               */
/* ------------------------------------------------------------------ */
static int test_query_and_absent(void)
{
    const char *filename = "test_charlen_query.cgns";
    int fn, B, Z, S, nscale, ierr;
    cgsize_t n;
    double h[PHYSDIM * N_ELEM];
    int i;

    printf("\n--- C/D: shape-only query and absent node ---\n");

    for (i = 0; i < PHYSDIM * N_ELEM; i++) h[i] = 1.0 + i;

    /* Build the file without a CharacteristicLength child. Read accessors
     * require READ or MODIFY mode, so the absent-node probe runs after the
     * write-mode handle is closed. */
    if (make_file(filename, &fn, &B, &Z, &S)) return 1;
    if (check(cg_close(fn), "close W")) return 1;

    /* D: node absent before it is written. */
    if (check(cg_open(filename, CG_MODE_READ, &fn), "open R (absent)")) return 1;
    nscale = -1; n = -1;
    ierr = cg_sol_characteristic_length_read(fn, B, Z, S, &nscale, &n, NULL);
    if (ierr != CG_NODE_NOT_FOUND) {
        fprintf(stderr, "ERROR: expected CG_NODE_NOT_FOUND, got %d\n", ierr);
        return 1;
    }
    if (nscale != 0 || n != 0) {
        fprintf(stderr, "ERROR: absent node should zero outputs, got "
                        "nscale=%d n=%ld\n", nscale, (long)n);
        return 1;
    }
    if (check(cg_close(fn), "close R (absent)")) return 1;
    printf("  absent node returns CG_NODE_NOT_FOUND with zeroed outputs\n");

    if (check(cg_open(filename, CG_MODE_MODIFY, &fn), "open M")) return 1;
    if (check(cg_sol_characteristic_length_write(fn, B, Z, S, PHYSDIM,
                                                 N_ELEM, h), "write")) return 1;
    if (check(cg_close(fn), "close M")) return 1;

    /* C: shape-only query with h_e == NULL. */
    if (check(cg_open(filename, CG_MODE_READ, &fn), "open R")) return 1;
    nscale = -1; n = -1;
    if (check(cg_sol_characteristic_length_read(fn, B, Z, S, &nscale, &n, NULL),
              "shape query")) return 1;
    if (nscale != PHYSDIM || n != N_ELEM) {
        fprintf(stderr, "ERROR: shape query gave nscale=%d n=%ld\n",
                nscale, (long)n);
        return 1;
    }
    if (check(cg_close(fn), "close R")) return 1;
    printf("  shape-only query returns nscale=%d, numElements=%d\n",
           PHYSDIM, N_ELEM);
    return 0;
}

/* ------------------------------------------------------------------ */
/* E - input validation                                                */
/* ------------------------------------------------------------------ */
static int test_validation(void)
{
    const char *filename = "test_charlen_bad.cgns";
    int fn, B, Z, S;
    double good[PHYSDIM * N_ELEM];
    double bad[N_ELEM] = {1.0, -2.0, 3.0, 4.0};
    int i;

    printf("\n--- E: input validation ---\n");

    for (i = 0; i < PHYSDIM * N_ELEM; i++) good[i] = 1.0;

    if (make_file(filename, &fn, &B, &Z, &S)) return 1;

    /* nscale must be 1 or PhysDim. */
    if (cg_sol_characteristic_length_write(fn, B, Z, S, 2, N_ELEM, good)
        == CG_OK) {
        fprintf(stderr, "ERROR: nscale=2 should be rejected (PhysDim=%d)\n",
                PHYSDIM);
        return 1;
    }
    printf("  nscale=2 rejected for PhysDim=%d\n", PHYSDIM);

    /* numElements must be positive. */
    if (cg_sol_characteristic_length_write(fn, B, Z, S, 1, 0, good) == CG_OK) {
        fprintf(stderr, "ERROR: numElements=0 should be rejected\n");
        return 1;
    }
    printf("  numElements=0 rejected\n");

    /* Scale factors divide coordinates, so they must be strictly positive. */
    if (cg_sol_characteristic_length_write(fn, B, Z, S, 1, N_ELEM, bad)
        == CG_OK) {
        fprintf(stderr, "ERROR: negative scale factor should be rejected\n");
        return 1;
    }
    printf("  negative scale factor rejected\n");

    /* NULL data pointer. */
    if (cg_sol_characteristic_length_write(fn, B, Z, S, 1, N_ELEM, NULL)
        == CG_OK) {
        fprintf(stderr, "ERROR: NULL h_e should be rejected\n");
        return 1;
    }
    printf("  NULL h_e rejected\n");

    if (check(cg_close(fn), "close")) return 1;
    return 0;
}

/* ------------------------------------------------------------------ */
/* F - re-write guard: rejected in WRITE, replaced in MODIFY           */
/* ------------------------------------------------------------------ */
static int test_rewrite_guard(void)
{
    const char *filename = "test_charlen_rewrite.cgns";
    int fn, B, Z, S, nscale;
    cgsize_t n;
    double first[N_ELEM]  = {1.0, 1.0, 1.0, 1.0};
    double second[N_ELEM] = {9.0, 9.0, 9.0, 9.0};
    double back[N_ELEM];
    int i;

    printf("\n--- F: re-write guard ---\n");

    if (make_file(filename, &fn, &B, &Z, &S)) return 1;
    if (check(cg_sol_characteristic_length_write(fn, B, Z, S, 1, N_ELEM, first),
              "write 1")) return 1;

    /* Second write in CG_MODE_WRITE must be rejected. */
    if (cg_sol_characteristic_length_write(fn, B, Z, S, 1, N_ELEM, second)
        == CG_OK) {
        fprintf(stderr, "ERROR: duplicate write in CG_MODE_WRITE should fail\n");
        return 1;
    }
    printf("  duplicate write rejected in CG_MODE_WRITE\n");
    if (check(cg_close(fn), "close W")) return 1;

    /* In CG_MODE_MODIFY the existing node is replaced. */
    if (check(cg_open(filename, CG_MODE_MODIFY, &fn), "open M")) return 1;
    if (check(cg_sol_characteristic_length_write(fn, B, Z, S, 1, N_ELEM, second),
              "write in MODIFY")) return 1;
    if (check(cg_close(fn), "close M")) return 1;

    if (check(cg_open(filename, CG_MODE_READ, &fn), "open R")) return 1;
    if (check(cg_sol_characteristic_length_read(fn, B, Z, S, &nscale, &n, back),
              "read back")) return 1;
    for (i = 0; i < N_ELEM; i++) {
        if (fabs(back[i] - second[i]) > 1.e-14) {
            fprintf(stderr, "ERROR: MODIFY did not replace: back[%d]=%g\n",
                    i, back[i]);
            return 1;
        }
    }
    if (check(cg_close(fn), "close R")) return 1;
    printf("  CG_MODE_MODIFY replaced the existing node\n");

    /* Changing encoding rank in MODIFY must also work. */
    {
        double axis[PHYSDIM * N_ELEM];
        double axis_back[PHYSDIM * N_ELEM];
        for (i = 0; i < PHYSDIM * N_ELEM; i++) axis[i] = 2.0 + i;

        if (check(cg_open(filename, CG_MODE_MODIFY, &fn), "open M2")) return 1;
        if (check(cg_sol_characteristic_length_write(fn, B, Z, S, PHYSDIM,
                                                     N_ELEM, axis),
                  "switch to per-axis")) return 1;
        if (check(cg_close(fn), "close M2")) return 1;

        if (check(cg_open(filename, CG_MODE_READ, &fn), "open R2")) return 1;
        if (check(cg_sol_characteristic_length_read(fn, B, Z, S, &nscale, &n,
                                                    axis_back),
                  "read per-axis")) return 1;
        if (nscale != PHYSDIM || n != N_ELEM) {
            fprintf(stderr, "ERROR: after switch nscale=%d n=%ld\n",
                    nscale, (long)n);
            return 1;
        }
        for (i = 0; i < PHYSDIM * N_ELEM; i++) {
            if (fabs(axis_back[i] - axis[i]) > 1.e-12) {
                fprintf(stderr, "ERROR: per-axis switch value %d\n", i);
                return 1;
            }
        }
        if (check(cg_close(fn), "close R2")) return 1;
        printf("  isotropic -> per-axis switch in MODIFY OK\n");
    }
    return 0;
}

/* Write the factors in disjoint element ranges, the way a distributed writer
 * would, and confirm the result is identical to a single whole-array write.
 * This is the case cg_sol_characteristic_length_write cannot serve: it takes
 * the entire array, which no rank of a partitioned run holds. */
static int test_partial(void)
{
    int fn, B, Z, S, F, sec, ci, fam, si, nscale = 0;
    cgsize_t sz[3], conn[8*N_ELEM], numElements = 0;
    double coord[N_VERT], fld[N_ELEM], h[3*N_ELEM], back[3*N_ELEM];
    cgsize_t i;
    int k;

    for (i = 0; i < N_VERT; i++) coord[i] = (double)i;
    for (i = 0; i < 8*N_ELEM; i++) conn[i] = (cgsize_t)(i % N_VERT) + 1;
    for (i = 0; i < N_ELEM; i++) fld[i] = (double)i;
    for (i = 0; i < 3*N_ELEM; i++) h[i] = 1.0 + (double)i;

    if (check(cg_open("test_charlen_partial.cgns", CG_MODE_WRITE, &fn), "open")) return 1;
    if (check(cg_base_write(fn, "Base", 3, 3, &B), "base")) return 1;
    sz[0] = N_VERT; sz[1] = N_ELEM; sz[2] = 0;
    if (check(cg_zone_write(fn, B, "Zone", sz, CGNS_ENUMV(Unstructured), &Z), "zone")) return 1;
    if (check(cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", coord, &ci), "cx")) return 1;
    if (check(cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", coord, &ci), "cy")) return 1;
    if (check(cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateZ", coord, &ci), "cz")) return 1;
    if (check(cg_section_write(fn, B, Z, "Hexas", CGNS_ENUMV(HEXA_8),
              1, N_ELEM, 0, conn, &sec), "section")) return 1;
    if (check(cg_family_write(fn, B, "CartFam", &fam), "family")) return 1;
    if (check(cg_goto(fn, B, "Zone_t", Z, NULL), "goto")) return 1;
    if (check(cg_famname_write("CartFam"), "famname")) return 1;
    if (check(cg_solution_interpolation_write(fn, B, fam, "Hex_P0",
              CGNS_ENUMV(HEXA_8), 0, 0,
              CGNS_ENUMV(CartesianMonomialsPascal), &si), "si")) return 1;
    if (check(cg_sol_write(fn, B, Z, "FS", CGNS_ENUMV(InterpolationPoints), &S), "sol")) return 1;
    if (check(cg_sol_interpolation_degree_write(fn, B, Z, S, 0, 0), "degree")) return 1;
    if (check(cg_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "Density", fld, &F), "field")) return 1;

    /* Per-axis encoding, written as N_ELEM separate single-element ranges --
     * the most fragmented pattern a partitioning could produce. */
    for (k = 0; k < N_ELEM; k++) {
        if (check(cg_sol_characteristic_length_partial_write(fn, B, Z, S, 3,
                  (cgsize_t)N_ELEM, (cgsize_t)k+1, (cgsize_t)k+1, &h[3*k]),
                  "partial write")) return 1;
    }
    if (check(cg_close(fn), "close")) return 1;

    if (check(cg_open("test_charlen_partial.cgns", CG_MODE_READ, &fn), "reopen")) return 1;
    if (check(cg_sol_characteristic_length_read(fn, 1, 1, 1, &nscale, &numElements, back),
              "read back")) return 1;
    if (nscale != 3 || numElements != N_ELEM) {
        fprintf(stderr, "ERROR: partial write gave nscale=%d numElements=%ld, "
                "expected 3 and %d\n", nscale, (long)numElements, N_ELEM);
        cg_close(fn);
        return 1;
    }
    for (i = 0; i < 3*N_ELEM; i++) {
        if (back[i] != h[i]) {
            fprintf(stderr, "ERROR: factor %ld is %g, expected %g\n",
                    (long)i, back[i], h[i]);
            cg_close(fn);
            return 1;
        }
    }
    if (check(cg_close(fn), "close2")) return 1;
    printf("  partial write in %d disjoint ranges round-trips\n", N_ELEM);
    return 0;
}

int main(void)
{
    int errors = 0;

    printf("\n");
    printf("##################################################\n");
    printf("#  CPEX-0045 Test: CharacteristicLength          #\n");
    printf("##################################################\n");

    if (test_isotropic())         errors++;
    if (test_per_axis())          errors++;
    if (test_query_and_absent())  errors++;
    if (test_validation())        errors++;
    if (test_rewrite_guard())     errors++;
    if (test_not_a_field())       errors++;
    if (test_v3_layout_rejected()) errors++;
    if (test_partial())           errors++;

    printf("\n");
    printf("##################################################\n");
    if (errors == 0)
        printf("#  ALL CHARACTERISTIC LENGTH TESTS PASSED        #\n");
    else
        printf("#  FAILURES: %d test(s) failed                    #\n", errors);
    printf("##################################################\n\n");

    return errors;
}
