/*
 * test_charlen_parallel.c -- distributed write of CharacteristicLength.
 *
 * cg_sol_characteristic_length_partial_write() exists for exactly one reason:
 * the normalisation factors of a Cartesian modal solution are per-element data,
 * of the same order as a solution field, and no rank of a partitioned run holds
 * the whole array that cg_sol_characteristic_length_write() demands.  The ranged
 * writer takes rmin/rmax over the element axis so a rank writes only what it
 * owns.
 *
 * That is the distributed case, and it had no parallel coverage: the serial
 * suite exercises the ranged writer from one process, which cannot catch a
 * collective-participation or global-offset error.
 *
 * Structure mirrors test_high_order_parallel.c:
 *   - node creation is collective, the range writes are independent;
 *   - every rank writes its own contiguous run of elements, together covering
 *     [1, N] with no gap and no overlap;
 *   - the verify phase is deliberately cross-rank -- each rank checks the slab
 *     its NEIGHBOUR wrote.  A rank re-reading its own slab would agree with
 *     itself even if write and read shared the same wrong offset, which is the
 *     error this test exists to catch.
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

int comm_size, comm_rank;

#define ELEMS_PER_RANK 5
#define NSCALE         3      /* per-axis encoding: PhysDim factors per cell */
#define NVERT          8

/* Pure function of the GLOBAL element index and axis, so the verify phase can
 * evaluate it for a slab it did not write. */
static double charlen_value(cgsize_t gelem, int axis)
{
    return 1.0 + (double)gelem * 0.25 + (double)axis * 0.0625;
}

int main(int argc, char *argv[])
{
    int fn, B, Z, S, F, sec, ci, fam, si;
    int nscale = 0, i, k, axis;
    int local_bad = 0, total_bad = 0;
    cgsize_t sz[3], numElements = 0, total_elems;
    cgsize_t rmin, rmax, gelem;
    cgsize_t conn[8 * ELEMS_PER_RANK];
    double coord[NVERT], fld[ELEMS_PER_RANK];
    double h[NSCALE * ELEMS_PER_RANK];
    double *back = NULL;

    if (MPI_Init(&argc, &argv) != MPI_SUCCESS) {
        printf("MPI_Init failed\n");
        return 1;
    }
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);

    if (comm_size != 4) {
        if (comm_rank == 0) {
            printf("ERROR: This test requires exactly 4 MPI processes\n");
            printf("Usage: mpiexec -n 4 ./test_charlen_parallel\n");
        }
        MPI_Finalize();
        return 1;
    }

    if (comm_rank == 0) {
        printf("\n========================================\n");
        printf("CharacteristicLength distributed write\n");
        printf("========================================\n");
        printf("Running with %d MPI processes\n", comm_size);
    }

    CHECK_ERROR(cgp_mpi_comm(MPI_COMM_WORLD), "Setting MPI communicator");

    total_elems = (cgsize_t)comm_size * ELEMS_PER_RANK;

    /* This rank's contiguous run, 1-based and inclusive. */
    rmin = (cgsize_t)comm_rank * ELEMS_PER_RANK + 1;
    rmax = rmin + ELEMS_PER_RANK - 1;

    for (i = 0; i < NVERT; i++)                coord[i] = (double)i;
    for (i = 0; i < 8 * ELEMS_PER_RANK; i++)   conn[i]  = (cgsize_t)(i % NVERT) + 1;
    for (i = 0; i < ELEMS_PER_RANK; i++)       fld[i]   = (double)(rmin + i);

    for (k = 0; k < ELEMS_PER_RANK; k++) {
        gelem = rmin + k;
        for (axis = 0; axis < NSCALE; axis++)
            h[NSCALE * k + axis] = charlen_value(gelem, axis);
    }

    /* ---------------------------------------------------------------- write */
    /* Everything that creates a node is collective with identical arguments,
     * per the CPEX-0045 parallel contract.  CartesianMonomialsPascal is what
     * makes CharacteristicLength meaningful, and it requires CellDim == PhysDim. */
    CHECK_ERROR(cgp_open("test_charlen_parallel.cgns", CG_MODE_WRITE, &fn), "cgp_open");
    CHECK_ERROR(cg_base_write(fn, "Base", 3, 3, &B), "base");

    sz[0] = NVERT; sz[1] = total_elems; sz[2] = 0;
    CHECK_ERROR(cg_zone_write(fn, B, "Zone", sz, CGNS_ENUMV(Unstructured), &Z), "zone");
    CHECK_ERROR(cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", coord, &ci), "cx");
    CHECK_ERROR(cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", coord, &ci), "cy");
    CHECK_ERROR(cg_coord_write(fn, B, Z, CGNS_ENUMV(RealDouble), "CoordinateZ", coord, &ci), "cz");

    CHECK_ERROR(cgp_section_write(fn, B, Z, "Hexas", CGNS_ENUMV(HEXA_8),
                                  1, total_elems, 0, &sec), "section");
    CHECK_ERROR(cgp_elements_write_data(fn, B, Z, sec, rmin, rmax, conn), "conn");

    CHECK_ERROR(cg_family_write(fn, B, "CartFam", &fam), "family");
    CHECK_ERROR(cg_goto(fn, B, "Zone_t", Z, NULL), "goto zone");
    CHECK_ERROR(cg_famname_write("CartFam"), "famname");
    CHECK_ERROR(cg_solution_interpolation_write(fn, B, fam, "Hex_P0",
                    CGNS_ENUMV(HEXA_8), 0, 0,
                    CGNS_ENUMV(CartesianMonomialsPascal), &si), "solution interpolation");

    CHECK_ERROR(cg_sol_write(fn, B, Z, "FS", CGNS_ENUMV(InterpolationPoints), &S), "sol");
    CHECK_ERROR(cg_sol_interpolation_degree_write(fn, B, Z, S, 0, 0), "degree");
    CHECK_ERROR(cgp_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "Density", &F), "field create");
    CHECK_ERROR(cgp_field_write_data(fn, B, Z, S, F, &rmin, &rmax, fld), "field data");

    /* The call under test: this rank writes only its own element range, while
     * numElements still declares the full extent so every rank agrees on the
     * array's shape. */
    if (comm_rank == 0)
        printf("Each rank writing %d of %ld elements via the ranged writer...\n",
               ELEMS_PER_RANK, (long)total_elems);

    /* Collective: every rank creates, with identical arguments, including a
     * rank that owns nothing.  This must precede any range write -- creating a
     * node is collective, so a create issued after another rank's write would
     * discard it. */
    CHECK_ERROR(cg_sol_characteristic_length_create(fn, B, Z, S, NSCALE,
                    total_elems), "characteristic length create");

    MPI_Barrier(MPI_COMM_WORLD);   /* DIAGNOSTIC: all creates before any write */

    /* Independent: each rank writes only the elements it owns, through the
     * MPI-IO path.  The serial ranged writer cannot be used here -- see the
     * note on cgp_sol_characteristic_length_write_data(). */
    CHECK_ERROR(cgp_sol_characteristic_length_write_data(fn, B, Z, S, NSCALE,
                    rmin, rmax, h), "characteristic length parallel write");

    CHECK_ERROR(cgp_close(fn), "cgp_close");

    /* ----------------------------------------------------------------- read */
    /* The reader takes the whole array; each rank then checks its NEIGHBOUR's
     * slab, so a shared wrong offset cannot pass. */
    CHECK_ERROR(cgp_open("test_charlen_parallel.cgns", CG_MODE_READ, &fn), "reopen");

    back = (double *) malloc((size_t)(NSCALE * total_elems) * sizeof(double));
    if (back == NULL) {
        printf("[rank %d] ERROR: malloc failed\n", comm_rank);
        cgp_error_exit();
    }

    CHECK_ERROR(cg_sol_characteristic_length_read(fn, B, Z, S, &nscale,
                                                  &numElements, back), "read back");

    if (nscale != NSCALE || numElements != total_elems) {
        printf("[rank %d] ERROR: got nscale=%d numElements=%ld, expected %d and %ld\n",
               comm_rank, nscale, (long)numElements, NSCALE, (long)total_elems);
        local_bad++;
    }
    else {
        int peer = (comm_rank + 1) % comm_size;
        cgsize_t pmin = (cgsize_t)peer * ELEMS_PER_RANK + 1;

        for (k = 0; k < ELEMS_PER_RANK; k++) {
            gelem = pmin + k;
            for (axis = 0; axis < NSCALE; axis++) {
                /* nscale is the fast axis, so element g starts at (g-1)*nscale */
                cgsize_t idx = (gelem - 1) * NSCALE + axis;
                double want = charlen_value(gelem, axis);
                if (back[idx] != want) {
                    if (local_bad < 10)
                        printf("[rank %d] MISMATCH peer elem %ld axis %d: "
                               "got %.17g, expected %.17g\n",
                               comm_rank, (long)gelem, axis, back[idx], want);
                    local_bad++;
                }
            }
        }
    }

    free(back);
    CHECK_ERROR(cgp_close(fn), "close read");

    MPI_Allreduce(&local_bad, &total_bad, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    if (comm_rank == 0) {
        printf("\n========================================\n");
        if (total_bad == 0)
            printf("CharacteristicLength parallel test PASSED\n");
        else
            printf("FAILED: %d mismatch(es) across all ranks\n", total_bad);
        printf("========================================\n");
    }

    MPI_Finalize();
    return total_bad == 0 ? 0 : 1;
}
