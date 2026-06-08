/*
 * Tests Unicode (UTF-8) filepath support for CGNS files.
 * Requires HDF5 >= 2.0.0 (enforced by CMake; this test is only built
 * when the HDF5 version requirement is met).
 *
 * Creates, writes, reads, and verifies a CGNS file whose path
 * contains non-ASCII (Unicode) characters.
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

#include "utils.h"

/* UTF-8 encoded filenames with CFD-related non-ASCII characters:
 *   "écoulement" = French for "flow"
 *   "maillage_résolu" = French for "resolved mesh"
 *   "vélocité" = French for "velocity"
 * These exercise multi-byte UTF-8 sequences (é = 0xC3 0xA9)
 */
#define UNICODE_DIR    "\xc3\xa9""coulement"
#define UNICODE_FILE   UNICODE_DIR "/maillage_r\xc3\xa9solu.cgns"
#define LINK_FILE      "link_top.cgns"
#define LINK_TARGET    UNICODE_DIR "/v\xc3\xa9locit\xc3\xa9.cgns"

#define BASENAME   "Base"
#define ZONENAME   "Zone"
#define NUM_SIDE   3

#if defined(_WIN32)
# include "utils_unicode_win.h"
# define MKDIR(d)       utf8_mkdir(d)
# define UTF8_UNLINK(f) utf8_unlink(f)
# define UTF8_RMDIR(d)  utf8_rmdir(d)
#else
# define MKDIR(d) mkdir((d), 0755)
# define UTF8_UNLINK(f) unlink(f)
# define UTF8_RMDIR(d)  rmdir(d)
#endif

static int test_unicode_write(void)
{
    int cgfile, B, Z;
    int cell_dim = 3, phys_dim = 3;
    cgsize_t sizes[9] = {3, 3, 3, 2, 2, 2, 0, 0, 0};

    printf("  Writing CGNS file with Unicode path...");
    fflush(stdout);

    if (cg_open(UNICODE_FILE, CG_MODE_WRITE, &cgfile)) {
        printf("*FAILED* cg_open for write\n");
        cg_error_print();
        return FAILED;
    }
    if (cg_base_write(cgfile, BASENAME, cell_dim, phys_dim, &B) != CG_OK) {
        printf("*FAILED* cg_base_write\n");
        cg_error_print();
        cg_close(cgfile);
        return FAILED;
    }
    if (cg_zone_write(cgfile, B, ZONENAME, sizes,
                      CGNS_ENUMV(Structured), &Z) != CG_OK) {
        printf("*FAILED* cg_zone_write\n");
        cg_error_print();
        cg_close(cgfile);
        return FAILED;
    }
    if (cg_close(cgfile)) {
        printf("*FAILED* cg_close\n");
        cg_error_print();
        return FAILED;
    }
    printf("PASSED\n");
    return PASSED;
}

static int test_unicode_read(void)
{
    int cgfile;
    int cell_dim, phys_dim;
    char name[33];
    cgsize_t sizes[9];

    printf("  Reading CGNS file with Unicode path...");
    fflush(stdout);

    if (cg_open(UNICODE_FILE, CG_MODE_READ, &cgfile)) {
        printf("*FAILED* cg_open for read\n");
        cg_error_print();
        return FAILED;
    }

    /* Verify base */
    if (cg_base_read(cgfile, 1, name, &cell_dim, &phys_dim) != CG_OK) {
        printf("*FAILED* cg_base_read\n");
        cg_error_print();
        cg_close(cgfile);
        return FAILED;
    }
    if (strcmp(name, BASENAME) != 0 || cell_dim != 3 || phys_dim != 3) {
        printf("*FAILED* base data mismatch\n");
        cg_close(cgfile);
        return FAILED;
    }

    /* Verify zone */
    if (cg_zone_read(cgfile, 1, 1, name, sizes) != CG_OK) {
        printf("*FAILED* cg_zone_read\n");
        cg_error_print();
        cg_close(cgfile);
        return FAILED;
    }
    if (strcmp(name, ZONENAME) != 0) {
        printf("*FAILED* zone name mismatch\n");
        cg_close(cgfile);
        return FAILED;
    }
    if (sizes[0] != 3 || sizes[1] != 3 || sizes[2] != 3 ||
        sizes[3] != 2 || sizes[4] != 2 || sizes[5] != 2) {
        printf("*FAILED* zone sizes mismatch\n");
        cg_close(cgfile);
        return FAILED;
    }

    if (cg_close(cgfile)) {
        printf("*FAILED* cg_close\n");
        cg_error_print();
        return FAILED;
    }
    printf("PASSED\n");
    return PASSED;
}

static int test_unicode_modify(void)
{
    int cgfile;

    printf("  Modifying CGNS file with Unicode path...");
    fflush(stdout);

    if (cg_open(UNICODE_FILE, CG_MODE_MODIFY, &cgfile)) {
        printf("*FAILED* cg_open for modify\n");
        cg_error_print();
        return FAILED;
    }
    if (cg_close(cgfile)) {
        printf("*FAILED* cg_close\n");
        cg_error_print();
        return FAILED;
    }
    printf("PASSED\n");
    return PASSED;
}

static int test_unicode_link_write(void)
{
    int cgfile, B, Z, cgcoord;
    int cell_dim = 3, phys_dim = 3;
    cgsize_t sizes[9] = {3, 3, 3, 2, 2, 2, 0, 0, 0};
    float coord[NUM_SIDE*NUM_SIDE*NUM_SIDE];
    int cglinkfile, cglinkbase;
    int i;

    printf("  Writing external link target with Unicode path...");
    fflush(stdout);

    for (i = 0; i < NUM_SIDE*NUM_SIDE*NUM_SIDE; i++)
        coord[i] = (float)i;

    /* Create the target file at a Unicode path */
    if (cg_open(LINK_TARGET, CG_MODE_WRITE, &cgfile)) {
        printf("*FAILED* cg_open target for write\n");
        cg_error_print();
        return FAILED;
    }
    if (cg_base_write(cgfile, BASENAME, cell_dim, phys_dim, &B) != CG_OK ||
        cg_zone_write(cgfile, B, ZONENAME, sizes,
                      CGNS_ENUMV(Structured), &Z) != CG_OK ||
        cg_coord_write(cgfile, B, Z, CGNS_ENUMV(RealSingle),
                       "CoordinateX", coord, &cgcoord) != CG_OK) {
        printf("*FAILED* writing target data\n");
        cg_error_print();
        cg_close(cgfile);
        return FAILED;
    }
    if (cg_close(cgfile)) {
        printf("*FAILED* cg_close target\n");
        cg_error_print();
        return FAILED;
    }

    /* Create the link file that references the Unicode-path target */
    if (cg_open(LINK_FILE, CG_MODE_WRITE, &cglinkfile)) {
        printf("*FAILED* cg_open link file for write\n");
        cg_error_print();
        return FAILED;
    }
    if (cg_base_write(cglinkfile, BASENAME, cell_dim, phys_dim,
                      &cglinkbase) != CG_OK) {
        printf("*FAILED* cg_base_write link file\n");
        cg_error_print();
        cg_close(cglinkfile);
        return FAILED;
    }
    /* Create an external link: Zone in link file -> /Base/Zone in target file */
    if (cg_goto(cglinkfile, cglinkbase, "end") != CG_OK ||
        cg_link_write(ZONENAME, LINK_TARGET, "/Base/Zone") != CG_OK) {
        printf("*FAILED* cg_link_write\n");
        cg_error_print();
        cg_close(cglinkfile);
        return FAILED;
    }
    if (cg_close(cglinkfile)) {
        printf("*FAILED* cg_close link file\n");
        cg_error_print();
        return FAILED;
    }
    printf("PASSED\n");
    return PASSED;
}

static int test_unicode_link_read(void)
{
    int cgfile;
    int cell_dim, phys_dim;
    int nzones;
    char name[33];
    cgsize_t sizes[9];

    printf("  Reading through external link to Unicode path...");
    fflush(stdout);

    /* Open the link file and read through the external link */
    if (cg_open(LINK_FILE, CG_MODE_READ, &cgfile)) {
        printf("*FAILED* cg_open link file for read\n");
        cg_error_print();
        return FAILED;
    }

    /* Verify zones are accessible through the link */
    if (cg_nzones(cgfile, 1, &nzones) != CG_OK) {
        printf("*FAILED* cg_nzones\n");
        cg_error_print();
        cg_close(cgfile);
        return FAILED;
    }
    if (nzones != 1) {
        printf("*FAILED* expected 1 zone, got %d\n", nzones);
        cg_close(cgfile);
        return FAILED;
    }

    /* Read zone data through the external link */
    if (cg_zone_read(cgfile, 1, 1, name, sizes) != CG_OK) {
        printf("*FAILED* cg_zone_read through link\n");
        cg_error_print();
        cg_close(cgfile);
        return FAILED;
    }
    if (strcmp(name, ZONENAME) != 0) {
        printf("*FAILED* zone name mismatch through link\n");
        cg_close(cgfile);
        return FAILED;
    }
    if (sizes[0] != 3 || sizes[1] != 3 || sizes[2] != 3 ||
        sizes[3] != 2 || sizes[4] != 2 || sizes[5] != 2) {
        printf("*FAILED* zone sizes mismatch through link\n");
        cg_close(cgfile);
        return FAILED;
    }

    if (cg_close(cgfile)) {
        printf("*FAILED* cg_close\n");
        cg_error_print();
        return FAILED;
    }
    printf("PASSED\n");
    return PASSED;
}

int main(int argc, char **argv)
{
    int result = PASSED;

    printf("**********************************\n");
    printf("* TEST UNICODE FILEPATH SUPPORT  *\n");
    printf("**********************************\n");
    fflush(stdout);

    /* Create the Unicode-named directory */
    if (MKDIR(UNICODE_DIR) != 0 && errno != EEXIST) {
        printf("*FAILED* cannot create directory '%s': %s\n",
               UNICODE_DIR, strerror(errno));
        return 1;
    }

    /* Test write, read, and modify with Unicode filepath */
    if (test_unicode_write() != PASSED) result = FAILED;
    if (result == PASSED && test_unicode_read() != PASSED) result = FAILED;
    if (result == PASSED && test_unicode_modify() != PASSED) result = FAILED;

    /* Test external links to files with Unicode paths */
    if (result == PASSED && test_unicode_link_write() != PASSED) result = FAILED;
    if (result == PASSED && test_unicode_link_read() != PASSED) result = FAILED;

    /* Cleanup */
    UTF8_UNLINK(UNICODE_FILE);
    UTF8_UNLINK(LINK_TARGET);
    UNLINK(LINK_FILE);
    UTF8_RMDIR(UNICODE_DIR);

    return result;
}
