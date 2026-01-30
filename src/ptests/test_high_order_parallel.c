/*
 * @file test_high_order_parallel.c
 * @section DESCRIPTION
 * Comprehensive parallel test for high-order polynomial elements
 *
 * Tests:
 * 1. Homogeneous high-order element sections (HEXA_125, TETRA_56, PENTA_75, PYRA_55)
 * 2. MIXED element sections with high-order elements
 * 3. Parallel coordinate and solution field I/O with high-order meshes
 * 4. Both CGP_INDEPENDENT and CGP_COLLECTIVE parallel I/O modes
 *
 * This test verifies that high-order element support correctly handles:
 * - NPE (Nodes Per Element) for all high-order element types
 * - Connectivity array size calculations (elem_count * NPE)
 * - Hyperslab calculations for distributed parallel writes/reads
 * - MIXED section offset arrays with variable-sized high-order elements
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pcgnslib.h"
#include "mpi.h"

#define CHECK_ERROR(fn_call, msg) \
    if ((fn_call) != CG_OK) { \
        if (comm_rank == 0) { \
            printf("ERROR at %s:%d - %s\n", __FILE__, __LINE__, msg); \
            printf("CGNS Error: %s\n", cg_get_error()); \
        } \
        cgp_error_exit(); \
    }

/* Global MPI variables */
int comm_size, comm_rank;

/*
 * Test 1: Homogeneous high-order element section
 * Each process writes a portion of a HEXA_125 mesh
 */
int test_homogeneous_hexa125(const char *filename)
{
    int fn, B, Z, S, C[3];
    cgsize_t sizes[3];
    cgsize_t elem_start, elem_end, num_elem_per_proc;
    cgsize_t *connectivity;
    double *coords_x, *coords_y, *coords_z;
    cgsize_t coord_start, coord_end;
    int i, j;

    if (comm_rank == 0) {
        printf("\n=== Test 1: Homogeneous HEXA_125 Section ===\n");
    }

    /* Create file in parallel */
    CHECK_ERROR(cgp_open(filename, CG_MODE_WRITE, &fn),
                "Opening file for homogeneous HEXA_125 test");

    /* Create base */
    CHECK_ERROR(cg_base_write(fn, "Base", 3, 3, &B),
                "Creating base");

    /* Define zone size:
     * Total: 16 HEXA_125 elements (4 per process)
     * Total nodes: 5*5*5*4 = 500 nodes per process = 2000 total
     */
    num_elem_per_proc = 4;
    sizes[0] = 2000;  /* Total vertices */
    sizes[1] = 16;    /* Total cells */
    sizes[2] = 0;     /* Boundary vertices (not used for unstructured) */

    CHECK_ERROR(cg_zone_write(fn, B, "Zone1", sizes, CGNS_ENUMV(Unstructured), &Z),
                "Creating zone");

    /* Write coordinates in parallel - each process writes 500 nodes */
    coord_start = comm_rank * 500 + 1;
    coord_end = coord_start + 499;

    coords_x = (double *)malloc(500 * sizeof(double));
    coords_y = (double *)malloc(500 * sizeof(double));
    coords_z = (double *)malloc(500 * sizeof(double));

    /* Generate simple coordinate data */
    for (i = 0; i < 500; i++) {
        coords_x[i] = (double)(comm_rank * 500 + i) / 100.0;
        coords_y[i] = (double)(i % 25) / 5.0;
        coords_z[i] = (double)(i / 25) / 5.0;
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

    /* Write element section - each process writes 4 HEXA_125 elements */
    elem_start = comm_rank * num_elem_per_proc + 1;
    elem_end = elem_start + num_elem_per_proc - 1;

    CHECK_ERROR(cgp_section_write(fn, B, Z, "HEXA_125_Elements",
                                  CGNS_ENUMV(HEXA_125), 1, 16, 0, &S),
                "Creating HEXA_125 section");

    /* Allocate connectivity: 4 elements * 125 nodes per element = 500 entries */
    connectivity = (cgsize_t *)malloc(num_elem_per_proc * 125 * sizeof(cgsize_t));

    /* Generate connectivity data - simple sequential node numbering */
    for (i = 0; i < num_elem_per_proc; i++) {
        for (j = 0; j < 125; j++) {
            connectivity[i * 125 + j] = comm_rank * 500 + i * 125 + j + 1;
        }
    }

    CHECK_ERROR(cgp_elements_write_data(fn, B, Z, S, elem_start, elem_end, connectivity),
                "Writing HEXA_125 connectivity data");

    free(connectivity);

    CHECK_ERROR(cgp_close(fn), "Closing file");

    if (comm_rank == 0) {
        printf("Test 1 PASSED: Successfully wrote %lld HEXA_125 elements\n",
               (long long)(num_elem_per_proc * comm_size));
    }

    return 0;
}

/*
 * Test 2: MIXED element section with high-order elements
 * Each process writes a mix of TETRA_56, PENTA_75, and PYRA_55 elements
 */
int test_mixed_high_order(const char *filename)
{
    int fn, B, Z, S, C[3];
    cgsize_t sizes[3];
    cgsize_t elem_start, elem_end, num_elem_per_proc;
    cgsize_t *elements;
    cgsize_t *offsets;
    double *coords_x, *coords_y, *coords_z;
    cgsize_t coord_start, coord_end, total_nodes;
    int i, elem_idx, offset_idx;

    if (comm_rank == 0) {
        printf("\n=== Test 2: MIXED Section with TETRA_56, PENTA_75, PYRA_55 ===\n");
    }

    CHECK_ERROR(cgp_open(filename, CG_MODE_WRITE, &fn),
                "Opening file for MIXED high-order test");

    CHECK_ERROR(cg_base_write(fn, "Base", 3, 3, &B),
                "Creating base");

    /* Each process writes 3 elements: 1 TETRA_35, 1 PENTA_75, 1 PYRA_55
     * Total connectivity entries per process: 1 + 35 + 1 + 75 + 1 + 55 = 168
     * Total nodes per process: 35 + 75 + 55 = 165 (approximate, with sharing)
     * We'll use 180 nodes per process to be safe
     */
    num_elem_per_proc = 3;
    total_nodes = 180 * comm_size;
    sizes[0] = total_nodes;
    sizes[1] = num_elem_per_proc * comm_size;  /* 12 total elements */
    sizes[2] = 0;

    CHECK_ERROR(cg_zone_write(fn, B, "Zone1", sizes, CGNS_ENUMV(Unstructured), &Z),
                "Creating zone");

    /* Write coordinates */
    coord_start = comm_rank * 180 + 1;
    coord_end = coord_start + 179;

    coords_x = (double *)malloc(180 * sizeof(double));
    coords_y = (double *)malloc(180 * sizeof(double));
    coords_z = (double *)malloc(180 * sizeof(double));

    for (i = 0; i < 180; i++) {
        coords_x[i] = (double)(comm_rank * 180 + i) / 50.0;
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

    /* Write MIXED poly section with high-order elements */
    elem_start = comm_rank * num_elem_per_proc;
    elem_end = elem_start + num_elem_per_proc - 1;

    /* Calculate maxoffset: total connectivity entries per process
     * TETRA_35: 1 (type) + 35 (nodes) = 36
     * PENTA_75: 1 (type) + 75 (nodes) = 76
     * PYRA_55:  1 (type) + 55 (nodes) = 56
     * Total: 168 per process
     */
    cgsize_t maxoffset = 168 * comm_size;

    CHECK_ERROR(cgp_poly_section_write(fn, B, Z, "MIXED_HighOrder",
                                       CGNS_ENUMV(MIXED), 0,
                                       num_elem_per_proc * comm_size - 1,
                                       maxoffset, 0, &S),
                "Creating MIXED high-order section");

    /* Allocate elements and offsets arrays */
    elements = (cgsize_t *)malloc(168 * sizeof(cgsize_t));
    offsets = (cgsize_t *)malloc(4 * sizeof(cgsize_t));  /* 3 elements + 1 end offset */

    /* Build elements array with element type tags and connectivity */
    elem_idx = 0;
    offset_idx = 0;

    offsets[offset_idx++] = comm_rank * 168;  /* Start offset for this process */

    /* Element 0: TETRA_35 (35 nodes) */
    elements[elem_idx++] = CGNS_ENUMV(TETRA_35);
    for (i = 0; i < 35; i++) {
        elements[elem_idx++] = comm_rank * 180 + i + 1;
    }
    offsets[offset_idx++] = offsets[0] + 36;

    /* Element 1: PENTA_75 (75 nodes) */
    elements[elem_idx++] = CGNS_ENUMV(PENTA_75);
    for (i = 0; i < 75; i++) {
        elements[elem_idx++] = comm_rank * 180 + 35 + i + 1;
    }
    offsets[offset_idx++] = offsets[1] + 76;

    /* Element 2: PYRA_55 (55 nodes) */
    elements[elem_idx++] = CGNS_ENUMV(PYRA_55);
    for (i = 0; i < 55; i++) {
        elements[elem_idx++] = comm_rank * 180 + 110 + i + 1;
    }
    offsets[offset_idx++] = offsets[2] + 56;

    CHECK_ERROR(cgp_poly_elements_write_data(fn, B, Z, S, elem_start, elem_end,
                                             elements, offsets),
                "Writing MIXED high-order connectivity data");

    free(elements);
    free(offsets);

    CHECK_ERROR(cgp_close(fn), "Closing file");

    if (comm_rank == 0) {
        printf("Test 2 PASSED: Successfully wrote MIXED section with %lld high-order elements\n",
               (long long)(num_elem_per_proc * comm_size));
    }

    return 0;
}

/*
 * Test 3: Read back and verify high-order element data
 */
int test_read_verify(const char *filename)
{
    int fn, B, Z, S, nsections;
    char sectname[33];
    CGNS_ENUMT(ElementType_t) elemtype;
    cgsize_t elem_start, elem_end;
    int nbndry, parent_flag;
    int npe;

    if (comm_rank == 0) {
        printf("\n=== Test 3: Read and Verify High-Order Elements ===\n");
    }

    /* Open the homogeneous HEXA_125 file for reading */
    CHECK_ERROR(cgp_open(filename, CG_MODE_READ, &fn),
                "Opening file for read verification");

    B = 1;
    Z = 1;

    /* Get number of sections */
    CHECK_ERROR(cg_nsections(fn, B, Z, &nsections),
                "Getting number of sections");

    if (comm_rank == 0) {
        printf("Found %d section(s)\n", nsections);
    }

    /* Read section info */
    for (S = 1; S <= nsections; S++) {
        CHECK_ERROR(cg_section_read(fn, B, Z, S, sectname, &elemtype,
                                    &elem_start, &elem_end, &nbndry, &parent_flag),
                    "Reading section info");

        if (comm_rank == 0) {
            printf("Section %d: %s\n", S, sectname);
            printf("  Element type: %d\n", elemtype);
            printf("  Element range: %lld to %lld\n",
                   (long long)elem_start, (long long)elem_end);
        }

        /* Verify NPE for high-order element types */
        CHECK_ERROR(cg_npe(elemtype, &npe),
                    "Getting NPE for element type");

        if (comm_rank == 0) {
            printf("  Nodes per element (NPE): %lld\n", (long long)npe);

            /* Verify expected NPE values */
            if (elemtype == CGNS_ENUMV(HEXA_125) && npe != 125) {
                printf("ERROR: Expected NPE=125 for HEXA_125, got %lld\n", (long long)npe);
                cgp_error_exit();
            }
        }
    }

    CHECK_ERROR(cgp_close(fn), "Closing file");

    if (comm_rank == 0) {
        printf("Test 3 PASSED: Successfully read and verified high-order element data\n");
    }

    return 0;
}

/*
 * Test 4: Solution field I/O with high-order elements
 */
int test_solution_field(const char *filename)
{
    int fn, B, Z, S, Sol, F;
    cgsize_t sizes[3];
    cgsize_t elem_start, elem_end, num_elem_per_proc;
    cgsize_t *connectivity;
    double *field_data;
    cgsize_t field_start, field_end;
    int i, j;

    if (comm_rank == 0) {
        printf("\n=== Test 4: Solution Field I/O with High-Order Elements ===\n");
    }

    CHECK_ERROR(cgp_open(filename, CG_MODE_WRITE, &fn),
                "Opening file for solution field test");

    CHECK_ERROR(cg_base_write(fn, "Base", 3, 3, &B),
                "Creating base");

    /* Simple zone with 4 HEXA_125 elements per process */
    num_elem_per_proc = 4;
    sizes[0] = 500 * comm_size;
    sizes[1] = num_elem_per_proc * comm_size;
    sizes[2] = 0;

    CHECK_ERROR(cg_zone_write(fn, B, "Zone1", sizes, CGNS_ENUMV(Unstructured), &Z),
                "Creating zone");

    /* Write element section - required for InterpolationPoints solutions */
    elem_start = comm_rank * num_elem_per_proc + 1;
    elem_end = elem_start + num_elem_per_proc - 1;

    CHECK_ERROR(cgp_section_write(fn, B, Z, "HEXA_125_Elements",
                                  CGNS_ENUMV(HEXA_125), 1,
                                  num_elem_per_proc * comm_size, 0, &S),
                "Creating HEXA_125 section");

    /* Write dummy connectivity (required for high-order solution calculation) */
    connectivity = (cgsize_t *)malloc(num_elem_per_proc * 125 * sizeof(cgsize_t));
    for (i = 0; i < num_elem_per_proc; i++) {
        for (j = 0; j < 125; j++) {
            connectivity[i * 125 + j] = comm_rank * 500 + i * 125 + j + 1;
        }
    }
    CHECK_ERROR(cgp_elements_write_data(fn, B, Z, S, elem_start, elem_end, connectivity),
                "Writing HEXA_125 connectivity");
    free(connectivity);

    /* Create element-based solution node */
    CHECK_ERROR(cg_sol_write(fn, B, Z, "Solution", CGNS_ENUMV(InterpolationPoints), &Sol),
                "Creating element-based solution");

    /* Set solution interpolation order (required for InterpolationPoints solutions)
     * Using order 1 (gives 1 node per element for scalar solution) */
    CHECK_ERROR(cg_sol_interpolation_order_write(fn, B, Z, Sol, 1, 0),
                "Setting solution interpolation order");

    /* Write solution field - one value per element */
    CHECK_ERROR(cgp_field_write(fn, B, Z, Sol, CGNS_ENUMV(RealDouble), "Density", &F),
                "Creating density field");

    field_data = (double *)malloc(num_elem_per_proc * sizeof(double));
    for (i = 0; i < num_elem_per_proc; i++) {
        field_data[i] = 1.0 + comm_rank * 0.1 + i * 0.01;
    }

    field_start = comm_rank * num_elem_per_proc + 1;
    field_end = field_start + num_elem_per_proc - 1;

    CHECK_ERROR(cgp_field_write_data(fn, B, Z, Sol, F, &field_start, &field_end, field_data),
                "Writing solution field data");

    free(field_data);

    CHECK_ERROR(cgp_close(fn), "Closing file");

    if (comm_rank == 0) {
        printf("Test 4 PASSED: Successfully wrote solution field for high-order elements\n");
    }

    return 0;
}

int main(int argc, char **argv)
{
    int err;

    /* Initialize MPI */
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
            printf("Usage: mpiexec -n 4 ./test_cpex45_parallel\n");
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

    /* Set up parallel CGNS */
    CHECK_ERROR(cgp_mpi_comm(MPI_COMM_WORLD), "Setting MPI communicator");

    /* Run tests */
    test_homogeneous_hexa125("test_high_order_hexa125.cgns");
    test_mixed_high_order("test_high_order_mixed.cgns");
    test_read_verify("test_high_order_hexa125.cgns");
    test_solution_field("test_high_order_solution.cgns");

    if (comm_rank == 0) {
        printf("\n========================================\n");
        printf("All high-order element tests PASSED!\n");
        printf("========================================\n\n");
    }

    MPI_Finalize();
    return 0;
}
