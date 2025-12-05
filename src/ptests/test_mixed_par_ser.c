/*
 * @file test_mixed_par_ser.c
 * @version 1.0
 *
 * @section DESCRIPTION
 * Test program for mixing parallel and serial CGNS I/O operations.
 * Tests fix for issue #836 where PR #906 caused hangs when mixing parallel
 * and serial file operations.
 *
 * @section USAGE
 * Launch with:
 *    mpirun -np <#> test_mixed_par_ser
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <io.h>
#define unlink _unlink
#else
#include <unistd.h>
#endif

#include "mpi.h"
#include "pcgnslib.h"
#include "cgnstypes.h"

static int commsize, commrank;

/* Forward declarations */
void test_serial(const char *fname);
void test_parallel(const char *fname);
void test_multiple_parallel_open(void);

int main(int argc, char *argv[])
{
    int ierr;
    const char *FNAME1 = "fname1.cgns";
    const char *FNAME2 = "fname2.cgns";
    const char *FNAME3 = "fname3.cgns";

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &commsize);
    MPI_Comm_rank(MPI_COMM_WORLD, &commrank);

    /* *******************************
     * TEST S-P-S, all different files
     * ******************************* */
    if (commrank == 0) printf("TEST S-P-S, all different files\n");
    if (commrank == 0) test_serial(FNAME1);
    MPI_Barrier(MPI_COMM_WORLD);
    test_parallel(FNAME2);
    if (commrank == 0) test_serial(FNAME3);
    MPI_Barrier(MPI_COMM_WORLD);

    /* *******************************
     * TEST S-P-S, all same files
     * ******************************* */
    if (commrank == 0) printf("TEST S-P-S, all same files\n");
    if (commrank == 0) test_serial(FNAME1);
    MPI_Barrier(MPI_COMM_WORLD);
    test_parallel(FNAME1);
    if (commrank == 0) test_serial(FNAME1);
    MPI_Barrier(MPI_COMM_WORLD);

    /* *******************************
     * TEST P-S-P, all different files
     * ******************************* */
    if (commrank == 0) printf("TEST P-S-P, all different files\n");
    test_parallel(FNAME1);
    if (commrank == 0) test_serial(FNAME2);
    MPI_Barrier(MPI_COMM_WORLD);
    test_parallel(FNAME3);

    /* *******************************
     * TEST P-S-P, all same files
     * ******************************* */
    if (commrank == 0) printf("TEST P-S-P, all same files\n");
    test_parallel(FNAME1);
    if (commrank == 0) test_serial(FNAME1);
    MPI_Barrier(MPI_COMM_WORLD);
    test_parallel(FNAME1);
    MPI_Barrier(MPI_COMM_WORLD);

    /* *******************************
     * TEST P-P: Multiple consecutive parallel opens
     * Tests that multiple parallel files work correctly
     * ******************************* */
    if (commrank == 0) printf("TEST P-P, consecutive parallel\n");
    test_parallel(FNAME1);
    test_parallel(FNAME2);
    test_parallel(FNAME3);
    MPI_Barrier(MPI_COMM_WORLD);

    /* *******************************
     * TEST S-S: Multiple consecutive serial opens
     * Tests that serial mode doesn't get corrupted
     * ******************************* */
    if (commrank == 0) printf("TEST S-S, consecutive serial\n");
    if (commrank == 0) test_serial(FNAME1);
    MPI_Barrier(MPI_COMM_WORLD);
    if (commrank == 0) test_serial(FNAME2);
    MPI_Barrier(MPI_COMM_WORLD);
    if (commrank == 0) test_serial(FNAME3);
    MPI_Barrier(MPI_COMM_WORLD);

    /* *******************************
     * TEST Issue #836: Multiple parallel files open simultaneously
     * This is the core scenario that PR #906 fixed.
     * Open file1 parallel, open file2 parallel (while file1 still open),
     * close file1, then write to file2 (should still work in parallel mode)
     * ******************************* */
    if (commrank == 0) printf("TEST Issue #836, simultaneous parallel files\n");
    test_multiple_parallel_open();
    MPI_Barrier(MPI_COMM_WORLD);

    /* *******************************
     * TEST: Parallel then immediate serial (no barrier)
     * Tests that mode handling works without synchronization delays
     * ******************************* */
    if (commrank == 0) printf("TEST P-S immediate (no barrier)\n");
    test_parallel(FNAME1);
    /* No barrier here - immediate serial open */
    if (commrank == 0) test_serial(FNAME2);
    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();
    return 0;
}

void test_serial(const char *fname)
{
    int ierr, cg, base, zone, iCoor;
    cgsize_t sizes[9];
    double *Dxyz;
    cgsize_t ii, jj, kk, pos;
    cgsize_t total_size;

    printf("   SERIAL CREATE.....");
    fflush(stdout);

    /* Make a serial file */
    ierr = cg_open(fname, CG_MODE_WRITE, &cg);
    if (ierr) cg_error_exit();

    ierr = cg_base_write(cg, "Base#1", 3, 3, &base);
    if (ierr) cg_error_exit();

    memset(sizes, 0, sizeof(sizes));
    sizes[0] = 10;
    sizes[1] = 10;
    sizes[2] = 10;
    sizes[3] = sizes[0] - 1;
    sizes[4] = sizes[1] - 1;
    sizes[5] = sizes[2] - 1;

    total_size = sizes[0] * sizes[1] * sizes[2];
    Dxyz = (double *)malloc(total_size * sizeof(double));

    /* Make up some dummy coordinates just for the test */
    for (kk = 0; kk < sizes[2]; kk++) {
        for (jj = 0; jj < sizes[1]; jj++) {
            for (ii = 0; ii < sizes[0]; ii++) {
                pos = ii + jj * sizes[0] + kk * sizes[0] * sizes[1];
                Dxyz[pos] = (double)ii;
            }
        }
    }

    ierr = cg_zone_write(cg, base, "zone1", sizes, CGNS_ENUMV(Structured), &zone);
    if (ierr) cg_error_exit();

    ierr = cg_coord_write(cg, base, zone, CGNS_ENUMV(RealDouble), "CoordinateX", Dxyz, &iCoor);
    if (ierr) cg_error_exit();

    ierr = cg_coord_write(cg, base, zone, CGNS_ENUMV(RealDouble), "CoordinateY", Dxyz, &iCoor);
    if (ierr) cg_error_exit();

    ierr = cg_coord_write(cg, base, zone, CGNS_ENUMV(RealDouble), "CoordinateZ", Dxyz, &iCoor);
    if (ierr) cg_error_exit();

    ierr = cg_close(cg);
    if (ierr) cg_error_exit();

    free(Dxyz);

    printf("PASS\n");
}

void test_parallel(const char *fname)
{
    int ierr, cg, base, zone, iCoor;
    cgsize_t sizes[9];

    if (commrank == 0) {
        printf("   PARALLEL CREATE...");
        fflush(stdout);
    }

    ierr = cgp_mpi_comm(MPI_COMM_WORLD);
    if (ierr) cgp_error_exit();

    ierr = cgp_pio_mode(CGP_COLLECTIVE);
    if (ierr) cgp_error_exit();

    ierr = cgp_open(fname, CG_MODE_WRITE, &cg);
    if (ierr) cgp_error_exit();

    ierr = cg_base_write(cg, "Base#1", 3, 3, &base);
    if (ierr) cgp_error_exit();

    memset(sizes, 0, sizeof(sizes));
    sizes[0] = 10;
    sizes[1] = 10;
    sizes[2] = 10;
    sizes[3] = sizes[0] - 1;
    sizes[4] = sizes[1] - 1;
    sizes[5] = sizes[2] - 1;

    ierr = cg_zone_write(cg, base, "zone1", sizes, CGNS_ENUMV(Structured), &zone);
    if (ierr) cgp_error_exit();

    ierr = cgp_coord_write(cg, base, zone, CGNS_ENUMV(RealDouble), "CoordinateX", &iCoor);
    if (ierr) cgp_error_exit();

    ierr = cgp_coord_write(cg, base, zone, CGNS_ENUMV(RealDouble), "CoordinateY", &iCoor);
    if (ierr) cgp_error_exit();

    ierr = cgp_coord_write(cg, base, zone, CGNS_ENUMV(RealDouble), "CoordinateZ", &iCoor);
    if (ierr) cgp_error_exit();

    ierr = cgp_close(cg);
    if (ierr) cgp_error_exit();

    if (commrank == 0) printf("PASS\n");
}

void test_multiple_parallel_open(void)
{
    /* Tests issue #836: Multiple parallel files open at the same time
     * This verifies that closing one parallel file doesn't corrupt the
     * access mode for other open parallel files. */

    int ierr, cg1, cg2, base1, base2, zone1, zone2;
    int iCoor1, iCoor2;
    cgsize_t sizes[9];

    if (commrank == 0) {
        printf("   MULTI PARALLEL OPEN...");
        fflush(stdout);
    }

    /* Setup MPI for parallel I/O */
    ierr = cgp_mpi_comm(MPI_COMM_WORLD);
    if (ierr) cgp_error_exit();
    ierr = cgp_pio_mode(CGP_COLLECTIVE);
    if (ierr) cgp_error_exit();

    /* Open first parallel file */
    ierr = cgp_open("multi_file1.cgns", CG_MODE_WRITE, &cg1);
    if (ierr) cgp_error_exit();

    /* Open second parallel file (while first is still open) */
    ierr = cgp_open("multi_file2.cgns", CG_MODE_WRITE, &cg2);
    if (ierr) cgp_error_exit();

    /* Create content in file 1 */
    memset(sizes, 0, sizeof(sizes));
    sizes[0] = 10;
    sizes[1] = 10;
    sizes[2] = 10;
    sizes[3] = sizes[0] - 1;
    sizes[4] = sizes[1] - 1;
    sizes[5] = sizes[2] - 1;

    ierr = cg_base_write(cg1, "Base", 3, 3, &base1);
    if (ierr) cgp_error_exit();
    ierr = cg_zone_write(cg1, base1, "Zone", sizes, CGNS_ENUMV(Structured), &zone1);
    if (ierr) cgp_error_exit();
    ierr = cgp_coord_write(cg1, base1, zone1, CGNS_ENUMV(RealDouble), "CoordinateX", &iCoor1);
    if (ierr) cgp_error_exit();

    /* Close first file (this previously would reset mode to NATIVE, breaking file2) */
    ierr = cgp_close(cg1);
    if (ierr) cgp_error_exit();

    /* Create content in file 2 - should still work in parallel mode
     * This would fail before PR #906 fix */
    ierr = cg_base_write(cg2, "Base", 3, 3, &base2);
    if (ierr) cgp_error_exit();
    ierr = cg_zone_write(cg2, base2, "Zone", sizes, CGNS_ENUMV(Structured), &zone2);
    if (ierr) cgp_error_exit();
    ierr = cgp_coord_write(cg2, base2, zone2, CGNS_ENUMV(RealDouble), "CoordinateY", &iCoor2);
    if (ierr) cgp_error_exit();

    /* Close second file */
    ierr = cgp_close(cg2);
    if (ierr) cgp_error_exit();

    if (commrank == 0) printf("PASS\n");
}
