/*
 * Tests Unicode (UTF-8) filepath support for parallel CGNS (cgp_open).
 * Requires HDF5 >= 2.0.0 (enforced by CMake; this test is only built
 * when the HDF5 version requirement is met).
 *
 * Exercises cgp_open write/read/modify with non-ASCII (Unicode)
 * directory and filenames, including parallel coordinate I/O.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>

/* windows.h must be included before utils.h because winerror.h defines
 * FAILED as a function-like macro, which would shadow the FAILED/PASSED
 * integer constants defined in utils.h. */
#if defined(_WIN32)
# include <direct.h>
# include <windows.h>
# undef FAILED
#endif

#include "pcgnslib.h"
#include "utils.h"
#include "mpi.h"

/* UTF-8 encoded filenames with CFD-related non-ASCII characters:
 *   "écoulement" = French for "flow"
 *   "résolu"     = French for "resolved"
 * These exercise multi-byte UTF-8 sequences (é = 0xC3 0xA9)
 */
#define UNICODE_DIR    "\xc3\xa9""coulement_par"
#define UNICODE_FILE   UNICODE_DIR "/r\xc3\xa9solu_par.cgns"

#define BASENAME   "Base"
#define ZONENAME   "Zone"
#define NUM_NODES  10

#if defined(_WIN32)

static wchar_t *utf8_to_wide(const char *utf8)
{
    int len = MultiByteToWideChar(CP_UTF8, 0, utf8, -1, NULL, 0);
    if (len <= 0) return NULL;
    wchar_t *wide = (wchar_t *)malloc(len * sizeof(wchar_t));
    if (wide == NULL) { errno = ENOMEM; return NULL; }
    if (MultiByteToWideChar(CP_UTF8, 0, utf8, -1, wide, len) == 0) {
        free(wide);
        return NULL;
    }
    return wide;
}

static int utf8_mkdir(const char *path)
{
    wchar_t *wpath = utf8_to_wide(path);
    int ret;
    if (wpath == NULL) { errno = ENOMEM; return -1; }
    ret = _wmkdir(wpath);
    free(wpath);
    return ret;
}

static int utf8_unlink(const char *path)
{
    wchar_t *wpath = utf8_to_wide(path);
    int ret;
    if (wpath == NULL) { errno = ENOMEM; return -1; }
    ret = _wunlink(wpath);
    free(wpath);
    return ret;
}

static int utf8_rmdir(const char *path)
{
    wchar_t *wpath = utf8_to_wide(path);
    int ret;
    if (wpath == NULL) { errno = ENOMEM; return -1; }
    ret = _wrmdir(wpath);
    free(wpath);
    return ret;
}

# define MKDIR(d)       utf8_mkdir(d)
# define UTF8_UNLINK(f) utf8_unlink(f)
# define UTF8_RMDIR(d)  utf8_rmdir(d)
#else
# define MKDIR(d)       mkdir((d), 0755)
# define UTF8_UNLINK(f) unlink(f)
# define UTF8_RMDIR(d)  rmdir(d)
#endif

static int comm_rank, comm_size;

static int test_parallel_write(void)
{
    int cgfile, B, Z, C;
    int cell_dim = 3, phys_dim = 3;
    cgsize_t sizes[9];
    cgsize_t rmin[3], rmax[3];
    double coord[NUM_NODES];
    int i;

    if (comm_rank == 0) {
        printf("  Parallel write to Unicode path...");
        fflush(stdout);
    }

    /* 1-D structured zone: total nodes = comm_size * NUM_NODES */
    sizes[0] = comm_size * NUM_NODES;
    sizes[1] = 1;
    sizes[2] = 1;
    sizes[3] = sizes[0] - 1;
    sizes[4] = 0;
    sizes[5] = 0;
    sizes[6] = 0;
    sizes[7] = 0;
    sizes[8] = 0;

    if (cgp_open(UNICODE_FILE, CG_MODE_WRITE, &cgfile)) {
        if (comm_rank == 0) {
            printf("*FAILED* cgp_open for write\n");
            cg_error_print();
        }
        return FAILED;
    }
    if (cg_base_write(cgfile, BASENAME, cell_dim, phys_dim, &B) ||
        cg_zone_write(cgfile, B, ZONENAME, sizes,
                      CGNS_ENUMV(Structured), &Z) ||
        cgp_coord_write(cgfile, B, Z, CGNS_ENUMV(RealDouble),
                        "CoordinateX", &C)) {
        if (comm_rank == 0) {
            printf("*FAILED* writing metadata\n");
            cg_error_print();
        }
        cgp_close(cgfile);
        return FAILED;
    }

    /* Each rank writes its portion of coordinate data */
    rmin[0] = comm_rank * NUM_NODES + 1;
    rmin[1] = 1;
    rmin[2] = 1;
    rmax[0] = (comm_rank + 1) * NUM_NODES;
    rmax[1] = 1;
    rmax[2] = 1;
    for (i = 0; i < NUM_NODES; i++)
        coord[i] = (double)(comm_rank * NUM_NODES + i);

    if (cgp_coord_write_data(cgfile, B, Z, C, rmin, rmax, coord)) {
        if (comm_rank == 0) {
            printf("*FAILED* cgp_coord_write_data\n");
            cg_error_print();
        }
        cgp_close(cgfile);
        return FAILED;
    }

    if (cgp_close(cgfile)) {
        if (comm_rank == 0) {
            printf("*FAILED* cgp_close\n");
            cg_error_print();
        }
        return FAILED;
    }
    if (comm_rank == 0) printf("PASSED\n");
    return PASSED;
}

static int test_parallel_read(void)
{
    int cgfile, ncoords;
    int cell_dim, phys_dim;
    char name[33];
    cgsize_t sizes[9];
    cgsize_t rmin[3], rmax[3];
    double coord[NUM_NODES];
    int i;

    if (comm_rank == 0) {
        printf("  Parallel read from Unicode path...");
        fflush(stdout);
    }

    if (cgp_open(UNICODE_FILE, CG_MODE_READ, &cgfile)) {
        if (comm_rank == 0) {
            printf("*FAILED* cgp_open for read\n");
            cg_error_print();
        }
        return FAILED;
    }

    /* Verify base */
    if (cg_base_read(cgfile, 1, name, &cell_dim, &phys_dim) != CG_OK) {
        if (comm_rank == 0) {
            printf("*FAILED* cg_base_read\n");
            cg_error_print();
        }
        cgp_close(cgfile);
        return FAILED;
    }
    if (strcmp(name, BASENAME) != 0 || cell_dim != 3 || phys_dim != 3) {
        if (comm_rank == 0) printf("*FAILED* base data mismatch\n");
        cgp_close(cgfile);
        return FAILED;
    }

    /* Verify zone */
    if (cg_zone_read(cgfile, 1, 1, name, sizes) != CG_OK) {
        if (comm_rank == 0) {
            printf("*FAILED* cg_zone_read\n");
            cg_error_print();
        }
        cgp_close(cgfile);
        return FAILED;
    }
    if (strcmp(name, ZONENAME) != 0) {
        if (comm_rank == 0) printf("*FAILED* zone name mismatch\n");
        cgp_close(cgfile);
        return FAILED;
    }
    if (sizes[0] != comm_size * NUM_NODES) {
        if (comm_rank == 0)
            printf("*FAILED* zone size mismatch: expected %d, got %d\n",
                   comm_size * NUM_NODES, (int)sizes[0]);
        cgp_close(cgfile);
        return FAILED;
    }

    /* Read back coordinate data for this rank's portion */
    if (cg_ncoords(cgfile, 1, 1, &ncoords) != CG_OK || ncoords != 1) {
        if (comm_rank == 0) {
            printf("*FAILED* cg_ncoords\n");
            cg_error_print();
        }
        cgp_close(cgfile);
        return FAILED;
    }

    rmin[0] = comm_rank * NUM_NODES + 1;
    rmin[1] = 1;
    rmin[2] = 1;
    rmax[0] = (comm_rank + 1) * NUM_NODES;
    rmax[1] = 1;
    rmax[2] = 1;

    if (cgp_coord_read_data(cgfile, 1, 1, 1, rmin, rmax, coord)) {
        if (comm_rank == 0) {
            printf("*FAILED* cgp_coord_read_data\n");
            cg_error_print();
        }
        cgp_close(cgfile);
        return FAILED;
    }

    /* Verify coordinate values */
    for (i = 0; i < NUM_NODES; i++) {
        double expected = (double)(comm_rank * NUM_NODES + i);
        if (coord[i] != expected) {
            if (comm_rank == 0)
                printf("*FAILED* coord[%d] = %g, expected %g\n",
                       i, coord[i], expected);
            cgp_close(cgfile);
            return FAILED;
        }
    }

    if (cgp_close(cgfile)) {
        if (comm_rank == 0) {
            printf("*FAILED* cgp_close\n");
            cg_error_print();
        }
        return FAILED;
    }
    if (comm_rank == 0) printf("PASSED\n");
    return PASSED;
}

static int test_parallel_modify(void)
{
    int cgfile;

    if (comm_rank == 0) {
        printf("  Parallel modify with Unicode path...");
        fflush(stdout);
    }

    if (cgp_open(UNICODE_FILE, CG_MODE_MODIFY, &cgfile)) {
        if (comm_rank == 0) {
            printf("*FAILED* cgp_open for modify\n");
            cg_error_print();
        }
        return FAILED;
    }
    if (cgp_close(cgfile)) {
        if (comm_rank == 0) {
            printf("*FAILED* cgp_close\n");
            cg_error_print();
        }
        return FAILED;
    }
    if (comm_rank == 0) printf("PASSED\n");
    return PASSED;
}

int main(int argc, char **argv)
{
    int result = PASSED;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);

    if (comm_rank == 0) {
        printf("*******************************************\n");
        printf("* TEST PARALLEL UNICODE FILEPATH SUPPORT  *\n");
        printf("*******************************************\n");
        fflush(stdout);
    }

    /* Rank 0 creates the Unicode-named directory */
    if (comm_rank == 0) {
        if (MKDIR(UNICODE_DIR) != 0 && errno != EEXIST) {
            printf("*FAILED* cannot create directory: %s\n",
                   strerror(errno));
            result = FAILED;
        }
    }
    MPI_Bcast(&result, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (result != PASSED) {
        MPI_Finalize();
        return 1;
    }
    MPI_Barrier(MPI_COMM_WORLD);

    /* Test parallel write, read, and modify with Unicode filepath */
    if (test_parallel_write() != PASSED) result = FAILED;
    MPI_Barrier(MPI_COMM_WORLD);
    if (result == PASSED && test_parallel_read() != PASSED) result = FAILED;
    MPI_Barrier(MPI_COMM_WORLD);
    if (result == PASSED && test_parallel_modify() != PASSED) result = FAILED;
    MPI_Barrier(MPI_COMM_WORLD);

    /* Cleanup - rank 0 only */
    if (comm_rank == 0) {
        UTF8_UNLINK(UNICODE_FILE);
        UTF8_RMDIR(UNICODE_DIR);
    }

    MPI_Finalize();
    return result;
}
