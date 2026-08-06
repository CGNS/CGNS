/*
 * @file test_high_order_parallel.c
 * @section DESCRIPTION
 * Parallel test for high-order polynomial elements
 *
 * Tests:
 * 1. Homogeneous high-order element sections (HEXA_125)
 * 2. MIXED element sections with high-order elements (TETRA_35, PENTA_75, PYRA_55)
 * 3. Section metadata and NPE for high-order element types
 * 4. Parallel solution field I/O with a GridLocation=InterpolationPoints solution
 *
 * Every write phase is followed by a read phase that compares the data against
 * the generator that produced it.  The comparison is deliberately cross-rank:
 * each rank reads back the slab written by its neighbour, never its own.  A rank
 * that read its own slab back would still agree with itself if write and read
 * shared the same wrong global offset, so it would not test the hyperslab
 * arithmetic this file exists to cover.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pcgnslib.h"
#include "mpi.h"

#define CHECK_ERROR(fn_call, msg) \
    if ((fn_call) != CG_OK) { \
        printf("[rank %d] ERROR at %s:%d - %s\n", comm_rank, __FILE__, __LINE__, msg); \
        printf("[rank %d] CGNS Error: %s\n", comm_rank, cg_get_error()); \
        cgp_error_exit(); \
    }

/* Global MPI variables */
int comm_size, comm_rank;

/* Data mismatches are counted rather than aborted, so one run reports every
 * discrepancy on every rank instead of the first one to trip. */
static int mismatches = 0;

static void mismatch(const char *what, long long index,
                     double got, double expected)
{
    if (mismatches < 10)   /* a systematic offset error would print millions */
        printf("[rank %d] MISMATCH %s[%lld]: got %.17g, expected %.17g\n",
               comm_rank, what, index, got, expected);
    mismatches++;
}

/* Reduce one test's outcome across the communicator before announcing it.  Each
 * rank only checks its neighbour's slab, so a verdict printed from local state
 * alone would report PASSED on the ranks that happened to look elsewhere. */
static int verdict(int entry_mismatches, const char *label)
{
    int local = (mismatches != entry_mismatches) ? 1 : 0;
    int total = 0;

    if (local)
        printf("[rank %d] %s: %d mismatch(es)\n", comm_rank, label,
               mismatches - entry_mismatches);

    MPI_Allreduce(&local, &total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    if (comm_rank == 0) {
        if (total)
            printf("%s FAILED on %d rank(s)\n", label, total);
        else
            printf("%s PASSED\n", label);
    }
    return total ? 1 : 0;
}

/* ---------------------------------------------------------------------------
 * Generators, all written as pure functions of a *global* index.  The verify
 * phase evaluates them for a slab it did not write, so the expected values are
 * independent of which rank produced them.
 * ------------------------------------------------------------------------ */

#define NODES_PER_RANK_HEX  500
#define ELEMS_PER_RANK_HEX  4
#define HEXA125_NPE         125

/* gnode is 0-based: the node at file position gnode+1 */
static double gen_coord_x(cgsize_t gnode) { return (double)gnode / 100.0; }
static double gen_coord_y(cgsize_t gnode)
{
    cgsize_t local = gnode % NODES_PER_RANK_HEX;
    return (double)(local % 25) / 5.0;
}
static double gen_coord_z(cgsize_t gnode)
{
    cgsize_t local = gnode % NODES_PER_RANK_HEX;
    return (double)(local / 25) / 5.0;
}

/* gelem is 0-based; j is the node slot within the element */
static cgsize_t gen_hexa_conn(cgsize_t gelem, int j)
{
    cgsize_t owner = gelem / ELEMS_PER_RANK_HEX;
    cgsize_t local = gelem % ELEMS_PER_RANK_HEX;
    return owner * NODES_PER_RANK_HEX + local * HEXA125_NPE + j + 1;
}

static double gen_field(cgsize_t gelem)
{
    cgsize_t owner = gelem / ELEMS_PER_RANK_HEX;
    cgsize_t local = gelem % ELEMS_PER_RANK_HEX;
    return 1.0 + (double)owner * 0.1 + (double)local * 0.01;
}

/* MIXED section: each rank contributes one TETRA_35, one PENTA_75, one PYRA_55 */
#define NODES_PER_RANK_MIX  180
#define ELEMS_PER_RANK_MIX  3
#define CONN_PER_RANK_MIX   168   /* (1+35) + (1+75) + (1+55) */

static const CGNS_ENUMT(ElementType_t) mixed_types[ELEMS_PER_RANK_MIX] = {
    CGNS_ENUMV(TETRA_35), CGNS_ENUMV(PENTA_75), CGNS_ENUMV(PYRA_55)
};
static const int mixed_npe[ELEMS_PER_RANK_MIX]        = { 35, 75, 55 };
static const int mixed_node_base[ELEMS_PER_RANK_MIX]  = {  0, 35, 110 };

/* First node id of element `slot` on `owner`, 1-based */
static cgsize_t gen_mixed_node(cgsize_t owner, int slot, int j)
{
    return owner * NODES_PER_RANK_MIX + mixed_node_base[slot] + j + 1;
}

/*
 * Test 1: Homogeneous high-order element section
 * Each process writes a portion of a HEXA_125 mesh, then verifies its
 * neighbour's portion.
 */
int test_homogeneous_hexa125(const char *filename)
{
    int fn, B, Z, S, C[3];
    cgsize_t sizes[3];
    cgsize_t elem_start, elem_end;
    cgsize_t *connectivity;
    double *coords_x, *coords_y, *coords_z;
    cgsize_t coord_start, coord_end;
    cgsize_t total_elems = (cgsize_t)ELEMS_PER_RANK_HEX * comm_size;
    cgsize_t total_nodes = (cgsize_t)NODES_PER_RANK_HEX * comm_size;
    int i, j;
    int entry_mismatches = mismatches;

    if (comm_rank == 0) {
        printf("\n=== Test 1: Homogeneous HEXA_125 Section ===\n");
    }

    CHECK_ERROR(cgp_open(filename, CG_MODE_WRITE, &fn),
                "Opening file for homogeneous HEXA_125 test");
    CHECK_ERROR(cg_base_write(fn, "Base", 3, 3, &B),
                "Creating base");

    sizes[0] = total_nodes;
    sizes[1] = total_elems;
    sizes[2] = 0;

    CHECK_ERROR(cg_zone_write(fn, B, "Zone1", sizes, CGNS_ENUMV(Unstructured), &Z),
                "Creating zone");

    /* Each process writes NODES_PER_RANK_HEX nodes */
    coord_start = (cgsize_t)comm_rank * NODES_PER_RANK_HEX + 1;
    coord_end   = coord_start + NODES_PER_RANK_HEX - 1;

    coords_x = (double *)malloc(NODES_PER_RANK_HEX * sizeof(double));
    coords_y = (double *)malloc(NODES_PER_RANK_HEX * sizeof(double));
    coords_z = (double *)malloc(NODES_PER_RANK_HEX * sizeof(double));

    for (i = 0; i < NODES_PER_RANK_HEX; i++) {
        cgsize_t gnode = coord_start - 1 + i;
        coords_x[i] = gen_coord_x(gnode);
        coords_y[i] = gen_coord_y(gnode);
        coords_z[i] = gen_coord_z(gnode);
    }

    CHECK_ERROR(cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", &C[0]),
                "Creating CoordinateX");
    CHECK_ERROR(cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", &C[1]),
                "Creating CoordinateY");
    CHECK_ERROR(cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateZ", &C[2]),
                "Creating CoordinateZ");

    CHECK_ERROR(cgp_coord_write_data(fn, B, Z, C[0], &coord_start, &coord_end, coords_x),
                "Writing CoordinateX data");
    CHECK_ERROR(cgp_coord_write_data(fn, B, Z, C[1], &coord_start, &coord_end, coords_y),
                "Writing CoordinateY data");
    CHECK_ERROR(cgp_coord_write_data(fn, B, Z, C[2], &coord_start, &coord_end, coords_z),
                "Writing CoordinateZ data");

    free(coords_x);
    free(coords_y);
    free(coords_z);

    /* Each process writes ELEMS_PER_RANK_HEX HEXA_125 elements */
    elem_start = (cgsize_t)comm_rank * ELEMS_PER_RANK_HEX + 1;
    elem_end   = elem_start + ELEMS_PER_RANK_HEX - 1;

    CHECK_ERROR(cgp_section_write(fn, B, Z, "HEXA_125_Elements",
                                  CGNS_ENUMV(HEXA_125), 1, total_elems, 0, &S),
                "Creating HEXA_125 section");

    connectivity = (cgsize_t *)malloc(ELEMS_PER_RANK_HEX * HEXA125_NPE * sizeof(cgsize_t));
    for (i = 0; i < ELEMS_PER_RANK_HEX; i++) {
        cgsize_t gelem = elem_start - 1 + i;
        for (j = 0; j < HEXA125_NPE; j++)
            connectivity[i * HEXA125_NPE + j] = gen_hexa_conn(gelem, j);
    }

    CHECK_ERROR(cgp_elements_write_data(fn, B, Z, S, elem_start, elem_end, connectivity),
                "Writing HEXA_125 connectivity data");

    free(connectivity);
    CHECK_ERROR(cgp_close(fn), "Closing file");

    /* --- verify: read back the neighbour's slab, not our own --- */
    if (comm_rank == 0) {
        printf("Verifying (each rank reads its neighbour's slab)...\n");
    }

    {
        int peer = (comm_rank + 1) % comm_size;
        cgsize_t pnode_start = (cgsize_t)peer * NODES_PER_RANK_HEX + 1;
        cgsize_t pnode_end   = pnode_start + NODES_PER_RANK_HEX - 1;
        cgsize_t pelem_start = (cgsize_t)peer * ELEMS_PER_RANK_HEX + 1;
        cgsize_t pelem_end   = pelem_start + ELEMS_PER_RANK_HEX - 1;

        CHECK_ERROR(cgp_open(filename, CG_MODE_READ, &fn), "Reopening for verify");

        coords_x = (double *)malloc(NODES_PER_RANK_HEX * sizeof(double));
        coords_y = (double *)malloc(NODES_PER_RANK_HEX * sizeof(double));
        coords_z = (double *)malloc(NODES_PER_RANK_HEX * sizeof(double));

        CHECK_ERROR(cgp_coord_read_data(fn, B, Z, 1, &pnode_start, &pnode_end, coords_x),
                    "Reading CoordinateX data");
        CHECK_ERROR(cgp_coord_read_data(fn, B, Z, 2, &pnode_start, &pnode_end, coords_y),
                    "Reading CoordinateY data");
        CHECK_ERROR(cgp_coord_read_data(fn, B, Z, 3, &pnode_start, &pnode_end, coords_z),
                    "Reading CoordinateZ data");

        for (i = 0; i < NODES_PER_RANK_HEX; i++) {
            cgsize_t gnode = pnode_start - 1 + i;
            if (coords_x[i] != gen_coord_x(gnode))
                mismatch("CoordinateX", (long long)gnode, coords_x[i], gen_coord_x(gnode));
            if (coords_y[i] != gen_coord_y(gnode))
                mismatch("CoordinateY", (long long)gnode, coords_y[i], gen_coord_y(gnode));
            if (coords_z[i] != gen_coord_z(gnode))
                mismatch("CoordinateZ", (long long)gnode, coords_z[i], gen_coord_z(gnode));
        }

        free(coords_x);
        free(coords_y);
        free(coords_z);

        connectivity = (cgsize_t *)malloc(ELEMS_PER_RANK_HEX * HEXA125_NPE * sizeof(cgsize_t));
        CHECK_ERROR(cgp_elements_read_data(fn, B, Z, 1, pelem_start, pelem_end, connectivity),
                    "Reading HEXA_125 connectivity data");

        for (i = 0; i < ELEMS_PER_RANK_HEX; i++) {
            cgsize_t gelem = pelem_start - 1 + i;
            for (j = 0; j < HEXA125_NPE; j++) {
                cgsize_t got = connectivity[i * HEXA125_NPE + j];
                cgsize_t exp = gen_hexa_conn(gelem, j);
                if (got != exp)
                    mismatch("Connectivity", (long long)(gelem * HEXA125_NPE + j),
                             (double)got, (double)exp);
            }
        }

        free(connectivity);
        CHECK_ERROR(cgp_close(fn), "Closing file after verify");
    }

    if (comm_rank == 0)
        printf("  %lld HEXA_125 elements written and read back\n",
               (long long)total_elems);
    return verdict(entry_mismatches, "Test 1");
}

/*
 * Test 2: MIXED element section with high-order elements
 * Each process writes a TETRA_35, a PENTA_75 and a PYRA_55, then verifies its
 * neighbour's three elements: the type tags, the node lists, and the offsets
 * that make a variable-size section readable at all.
 */
int test_mixed_high_order(const char *filename)
{
    int fn, B, Z, S, C[3];
    cgsize_t sizes[3];
    cgsize_t elem_start, elem_end;
    cgsize_t *elements;
    cgsize_t *offsets;
    double *coords_x, *coords_y, *coords_z;
    cgsize_t coord_start, coord_end, total_nodes, maxoffset;
    int i, slot, elem_idx, offset_idx;
    int entry_mismatches = mismatches;

    if (comm_rank == 0) {
        printf("\n=== Test 2: MIXED Section with TETRA_35, PENTA_75, PYRA_55 ===\n");
    }

    CHECK_ERROR(cgp_open(filename, CG_MODE_WRITE, &fn),
                "Opening file for MIXED high-order test");
    CHECK_ERROR(cg_base_write(fn, "Base", 3, 3, &B),
                "Creating base");

    total_nodes = (cgsize_t)NODES_PER_RANK_MIX * comm_size;
    sizes[0] = total_nodes;
    sizes[1] = (cgsize_t)ELEMS_PER_RANK_MIX * comm_size;
    sizes[2] = 0;

    CHECK_ERROR(cg_zone_write(fn, B, "Zone1", sizes, CGNS_ENUMV(Unstructured), &Z),
                "Creating zone");

    coord_start = (cgsize_t)comm_rank * NODES_PER_RANK_MIX + 1;
    coord_end   = coord_start + NODES_PER_RANK_MIX - 1;

    coords_x = (double *)malloc(NODES_PER_RANK_MIX * sizeof(double));
    coords_y = (double *)malloc(NODES_PER_RANK_MIX * sizeof(double));
    coords_z = (double *)malloc(NODES_PER_RANK_MIX * sizeof(double));

    for (i = 0; i < NODES_PER_RANK_MIX; i++) {
        cgsize_t gnode = coord_start - 1 + i;
        coords_x[i] = (double)gnode / 50.0;
        coords_y[i] = (double)(i % 10) / 2.0;
        coords_z[i] = (double)(i / 10) / 2.0;
    }

    CHECK_ERROR(cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", &C[0]),
                "Creating CoordinateX");
    CHECK_ERROR(cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", &C[1]),
                "Creating CoordinateY");
    CHECK_ERROR(cgp_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateZ", &C[2]),
                "Creating CoordinateZ");

    CHECK_ERROR(cgp_coord_write_data(fn, B, Z, C[0], &coord_start, &coord_end, coords_x),
                "Writing CoordinateX data");
    CHECK_ERROR(cgp_coord_write_data(fn, B, Z, C[1], &coord_start, &coord_end, coords_y),
                "Writing CoordinateY data");
    CHECK_ERROR(cgp_coord_write_data(fn, B, Z, C[2], &coord_start, &coord_end, coords_z),
                "Writing CoordinateZ data");

    free(coords_x);
    free(coords_y);
    free(coords_z);

    elem_start = (cgsize_t)comm_rank * ELEMS_PER_RANK_MIX + 1;
    elem_end   = elem_start + ELEMS_PER_RANK_MIX - 1;
    maxoffset  = (cgsize_t)CONN_PER_RANK_MIX * comm_size;

    CHECK_ERROR(cgp_poly_section_write(fn, B, Z, "MIXED_HighOrder",
                                       CGNS_ENUMV(MIXED), 1,
                                       (cgsize_t)ELEMS_PER_RANK_MIX * comm_size,
                                       maxoffset, 0, &S),
                "Creating MIXED high-order section");

    elements = (cgsize_t *)malloc(CONN_PER_RANK_MIX * sizeof(cgsize_t));
    offsets  = (cgsize_t *)malloc((ELEMS_PER_RANK_MIX + 1) * sizeof(cgsize_t));

    elem_idx = 0;
    offset_idx = 0;
    offsets[offset_idx++] = (cgsize_t)comm_rank * CONN_PER_RANK_MIX;

    for (slot = 0; slot < ELEMS_PER_RANK_MIX; slot++) {
        elements[elem_idx++] = mixed_types[slot];
        for (i = 0; i < mixed_npe[slot]; i++)
            elements[elem_idx++] = gen_mixed_node(comm_rank, slot, i);
        offsets[offset_idx] = offsets[offset_idx - 1] + 1 + mixed_npe[slot];
        offset_idx++;
    }

    CHECK_ERROR(cgp_poly_elements_write_data(fn, B, Z, S, elem_start, elem_end,
                                             elements, offsets),
                "Writing MIXED high-order connectivity data");

    free(elements);
    free(offsets);
    CHECK_ERROR(cgp_close(fn), "Closing file");

    /* --- verify the neighbour's three elements --- */
    if (comm_rank == 0) {
        printf("Verifying (each rank reads its neighbour's elements)...\n");
    }

    {
        int peer = (comm_rank + 1) % comm_size;
        cgsize_t pelem_start = (cgsize_t)peer * ELEMS_PER_RANK_MIX + 1;
        cgsize_t pelem_end   = pelem_start + ELEMS_PER_RANK_MIX - 1;

        CHECK_ERROR(cgp_open(filename, CG_MODE_READ, &fn), "Reopening for verify");

        offsets  = (cgsize_t *)malloc((ELEMS_PER_RANK_MIX + 1) * sizeof(cgsize_t));
        elements = (cgsize_t *)malloc(CONN_PER_RANK_MIX * sizeof(cgsize_t));

        CHECK_ERROR(cgp_poly_elements_read_data_offsets(fn, B, Z, 1,
                                                        pelem_start, pelem_end, offsets),
                    "Reading MIXED offsets");
        CHECK_ERROR(cgp_poly_elements_read_data_elements(fn, B, Z, 1,
                                                         pelem_start, pelem_end,
                                                         offsets, elements),
                    "Reading MIXED connectivity");

        /* The offsets must place this rank's block where the global layout says,
         * and each span must be exactly 1 tag + NPE nodes.  A variable-size
         * section with plausible connectivity but wrong offsets decodes as
         * garbage, so the spans are the load-bearing check. */
        if (offsets[0] != (cgsize_t)peer * CONN_PER_RANK_MIX)
            mismatch("MixedOffsetBase", 0, (double)offsets[0],
                     (double)((cgsize_t)peer * CONN_PER_RANK_MIX));

        elem_idx = 0;
        for (slot = 0; slot < ELEMS_PER_RANK_MIX; slot++) {
            cgsize_t span = offsets[slot + 1] - offsets[slot];
            cgsize_t want = 1 + mixed_npe[slot];

            if (span != want) {
                mismatch("MixedOffsetSpan", slot, (double)span, (double)want);
                break;    /* the element decode below would be meaningless */
            }
            if (elements[elem_idx] != (cgsize_t)mixed_types[slot])
                mismatch("MixedElementType", slot,
                         (double)elements[elem_idx], (double)mixed_types[slot]);
            elem_idx++;
            for (i = 0; i < mixed_npe[slot]; i++, elem_idx++) {
                cgsize_t exp = gen_mixed_node(peer, slot, i);
                if (elements[elem_idx] != exp)
                    mismatch("MixedConnectivity", (long long)elem_idx,
                             (double)elements[elem_idx], (double)exp);
            }
        }

        free(offsets);
        free(elements);
        CHECK_ERROR(cgp_close(fn), "Closing file after verify");
    }

    if (comm_rank == 0)
        printf("  MIXED section with %lld high-order elements read back\n",
               (long long)((cgsize_t)ELEMS_PER_RANK_MIX * comm_size));
    return verdict(entry_mismatches, "Test 2");
}

/*
 * Test 3: Section metadata and NPE for high-order element types.
 * Checked on every rank: a reader that resolved NPE differently on different
 * ranks would corrupt collective I/O, so agreement is part of the contract.
 */
int test_read_verify(const char *filename)
{
    int fn, B, Z, S, nsections;
    char sectname[33];
    CGNS_ENUMT(ElementType_t) elemtype;
    cgsize_t elem_start, elem_end;
    int nbndry, parent_flag;
    int npe;
    cgsize_t total_elems = (cgsize_t)ELEMS_PER_RANK_HEX * comm_size;
    int entry_mismatches = mismatches;

    if (comm_rank == 0) {
        printf("\n=== Test 3: Section Metadata and NPE ===\n");
    }

    CHECK_ERROR(cgp_open(filename, CG_MODE_READ, &fn),
                "Opening file for read verification");

    B = 1;
    Z = 1;

    CHECK_ERROR(cg_nsections(fn, B, Z, &nsections),
                "Getting number of sections");

    if (nsections != 1)
        mismatch("nsections", 0, (double)nsections, 1.0);

    for (S = 1; S <= nsections; S++) {
        CHECK_ERROR(cg_section_read(fn, B, Z, S, sectname, &elemtype,
                                    &elem_start, &elem_end, &nbndry, &parent_flag),
                    "Reading section info");

        if (comm_rank == 0) {
            printf("Section %d: %s, type %s, range %lld..%lld\n", S, sectname,
                   cg_ElementTypeName(elemtype),
                   (long long)elem_start, (long long)elem_end);
        }

        if (strcmp(sectname, "HEXA_125_Elements")) {
            printf("[rank %d] MISMATCH section name: got '%s', expected "
                   "'HEXA_125_Elements'\n", comm_rank, sectname);
            mismatches++;
        }
        if (elemtype != CGNS_ENUMV(HEXA_125))
            mismatch("ElementType", S, (double)elemtype, (double)CGNS_ENUMV(HEXA_125));
        if (elem_start != 1)
            mismatch("SectionStart", S, (double)elem_start, 1.0);
        if (elem_end != total_elems)
            mismatch("SectionEnd", S, (double)elem_end, (double)total_elems);

        CHECK_ERROR(cg_npe(elemtype, &npe), "Getting NPE for element type");
        if (npe != HEXA125_NPE)
            mismatch("NPE(HEXA_125)", S, (double)npe, (double)HEXA125_NPE);
    }

    /* The high-order tags used by the MIXED section must size the same way */
    {
        int slot;
        for (slot = 0; slot < ELEMS_PER_RANK_MIX; slot++) {
            CHECK_ERROR(cg_npe(mixed_types[slot], &npe), "Getting NPE for mixed type");
            if (npe != mixed_npe[slot])
                mismatch("NPE(mixed)", slot, (double)npe, (double)mixed_npe[slot]);
            else if (comm_rank == 0)
                printf("  NPE(%s) = %d\n", cg_ElementTypeName(mixed_types[slot]), npe);
        }
    }

    CHECK_ERROR(cgp_close(fn), "Closing file");

    return verdict(entry_mismatches, "Test 3");
}

/*
 * Test 4: Solution field I/O with high-order elements
 */
int test_solution_field(const char *filename)
{
    int fn, B, Z, S, Sol, F;
    cgsize_t sizes[3];
    cgsize_t elem_start, elem_end;
    cgsize_t *connectivity;
    double *field_data;
    cgsize_t field_start, field_end;
    cgsize_t total_elems = (cgsize_t)ELEMS_PER_RANK_HEX * comm_size;
    int i, j;
    int entry_mismatches = mismatches;

    if (comm_rank == 0) {
        printf("\n=== Test 4: Solution Field I/O with High-Order Elements ===\n");
    }

    CHECK_ERROR(cgp_open(filename, CG_MODE_WRITE, &fn),
                "Opening file for solution field test");
    CHECK_ERROR(cg_base_write(fn, "Base", 3, 3, &B),
                "Creating base");

    sizes[0] = (cgsize_t)NODES_PER_RANK_HEX * comm_size;
    sizes[1] = total_elems;
    sizes[2] = 0;

    CHECK_ERROR(cg_zone_write(fn, B, "Zone1", sizes, CGNS_ENUMV(Unstructured), &Z),
                "Creating zone");

    /* A GridLocation=InterpolationPoints field is sized as sum_e N_DOFs(e), and
     * N_DOFs comes from the SolutionInterpolation_t matching each element,
     * reached through the zone's FamilyName_t.  Both must therefore exist before
     * the field is written.
     *
     * This test wants one value per element, which is a degree-0 solution: a
     * single coefficient per element, the cell average.  (Degree 1 would give
     * (1+1)^3 = 8 degrees of freedom per HEXA, not 1.) */
    {
        int Fam, Si;
        CHECK_ERROR(cg_family_write(fn, B, "HOFamily", &Fam), "Creating family");
        CHECK_ERROR(cg_goto(fn, B, "Zone_t", Z, NULL), "goto zone");
        CHECK_ERROR(cg_famname_write("HOFamily"), "Attaching family to zone");
        CHECK_ERROR(cg_solution_interpolation_write(fn, B, Fam, "Hex_P0",
                        CGNS_ENUMV(HEXA_8), 0, 0,
                        CGNS_ENUMV(ParametricMonomialsPascal), &Si),
                    "Creating degree-0 solution interpolation");
    }

    /* Write element section - required for InterpolationPoints solutions */
    elem_start = (cgsize_t)comm_rank * ELEMS_PER_RANK_HEX + 1;
    elem_end   = elem_start + ELEMS_PER_RANK_HEX - 1;

    CHECK_ERROR(cgp_section_write(fn, B, Z, "HEXA_125_Elements",
                                  CGNS_ENUMV(HEXA_125), 1, total_elems, 0, &S),
                "Creating HEXA_125 section");

    connectivity = (cgsize_t *)malloc(ELEMS_PER_RANK_HEX * HEXA125_NPE * sizeof(cgsize_t));
    for (i = 0; i < ELEMS_PER_RANK_HEX; i++) {
        cgsize_t gelem = elem_start - 1 + i;
        for (j = 0; j < HEXA125_NPE; j++)
            connectivity[i * HEXA125_NPE + j] = gen_hexa_conn(gelem, j);
    }
    CHECK_ERROR(cgp_elements_write_data(fn, B, Z, S, elem_start, elem_end, connectivity),
                "Writing HEXA_125 connectivity");
    free(connectivity);

    CHECK_ERROR(cg_sol_write(fn, B, Z, "Solution", CGNS_ENUMV(InterpolationPoints), &Sol),
                "Creating element-based solution");

    /* Degree 0: one degree of freedom per element, matching the one value per
     * element written below. */
    CHECK_ERROR(cg_sol_interpolation_degree_write(fn, B, Z, Sol, 0, 0),
                "Setting solution interpolation degree");

    CHECK_ERROR(cgp_field_write(fn, B, Z, Sol, CGNS_ENUMV(RealDouble), "Density", &F),
                "Creating density field");

    field_data = (double *)malloc(ELEMS_PER_RANK_HEX * sizeof(double));
    field_start = (cgsize_t)comm_rank * ELEMS_PER_RANK_HEX + 1;
    field_end   = field_start + ELEMS_PER_RANK_HEX - 1;
    for (i = 0; i < ELEMS_PER_RANK_HEX; i++)
        field_data[i] = gen_field(field_start - 1 + i);

    CHECK_ERROR(cgp_field_write_data(fn, B, Z, Sol, F, &field_start, &field_end, field_data),
                "Writing solution field data");

    free(field_data);
    CHECK_ERROR(cgp_close(fn), "Closing file");

    /* --- verify: the field array is one value per element, and the values in
     * the neighbour's slab are the ones the neighbour wrote --- */
    if (comm_rank == 0) {
        printf("Verifying field length and values...\n");
    }

    {
        int peer = (comm_rank + 1) % comm_size;
        cgsize_t pstart = (cgsize_t)peer * ELEMS_PER_RANK_HEX + 1;
        cgsize_t pend   = pstart + ELEMS_PER_RANK_HEX - 1;
        char aname[33];
        int ndim;
        cgsize_t dimv[3];
        CGNS_ENUMT(DataType_t) dt;

        CHECK_ERROR(cgp_open(filename, CG_MODE_READ, &fn), "Reopening for verify");

        /* The point of a degree-0 InterpolationPoints solution is that the
         * library derives the array length from the basis, so check the length
         * on disk rather than assuming the write implied it. */
        CHECK_ERROR(cg_goto(fn, B, "Zone_t", Z, "FlowSolution_t", 1, NULL),
                    "goto FlowSolution_t");
        CHECK_ERROR(cg_array_info(1, aname, &dt, &ndim, dimv), "Reading field array info");
        if (ndim != 1 || dimv[0] != total_elems)
            mismatch("FieldLength", 0, (double)dimv[0], (double)total_elems);

        field_data = (double *)malloc(ELEMS_PER_RANK_HEX * sizeof(double));
        CHECK_ERROR(cgp_field_read_data(fn, B, Z, 1, 1, &pstart, &pend, field_data),
                    "Reading solution field data");

        for (i = 0; i < ELEMS_PER_RANK_HEX; i++) {
            cgsize_t gelem = pstart - 1 + i;
            if (field_data[i] != gen_field(gelem))
                mismatch("Density", (long long)gelem, field_data[i], gen_field(gelem));
        }

        free(field_data);
        CHECK_ERROR(cgp_close(fn), "Closing file after verify");
    }

    return verdict(entry_mismatches, "Test 4");
}

int main(int argc, char **argv)
{
    int err;
    int failures = 0, total_failures = 0;

    err = MPI_Init(&argc, &argv);
    if (err != MPI_SUCCESS) {
        printf("MPI_Init failed\n");
        return 1;
    }

    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);

    /* Require 4 processes for this test */
    if (comm_size != 4) {
        if (comm_rank == 0) {
            printf("ERROR: This test requires exactly 4 MPI processes\n");
            printf("Usage: mpiexec -n 4 ./test_high_order_parallel\n");
        }
        MPI_Finalize();
        return 1;
    }

    if (comm_rank == 0) {
        printf("\n");
        printf("========================================\n");
        printf("High-Order Elements Parallel I/O Test\n");
        printf("========================================\n");
        printf("Running with %d MPI processes\n", comm_size);
    }

    CHECK_ERROR(cgp_mpi_comm(MPI_COMM_WORLD), "Setting MPI communicator");

    failures += test_homogeneous_hexa125("test_high_order_hexa125.cgns");
    failures += test_mixed_high_order("test_high_order_mixed.cgns");
    failures += test_read_verify("test_high_order_hexa125.cgns");
    failures += test_solution_field("test_high_order_solution.cgns");

    /* verdict() already reduced each test, so `failures` agrees on every rank */
    total_failures = failures;

    if (comm_rank == 0) {
        printf("\n========================================\n");
        if (total_failures == 0)
            printf("All high-order element tests PASSED!\n");
        else
            printf("FAILED: %d of 4 tests reported mismatches\n", total_failures);
        printf("========================================\n\n");
    }

    MPI_Finalize();
    return total_failures == 0 ? 0 : 1;
}
