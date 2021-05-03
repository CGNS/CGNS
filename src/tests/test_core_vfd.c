/*
 * Tests HDF5 core file driver use in CGNS. Valid only when CGNS is built with
 * HDF5 support.
 */
#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

#define FILENAME "file_ops.cgns"
#define BASENAME "Base 1"
#define BASENAME_PATH "/Base 1"

int main (int argc, char **argv)
{
#if CG_BUILD_HDF5
    double start, finish;
    int cgfile;

    int B;
    int cell_dim = 3;
    int phys_dim = 3;

    int file_type;
    int ier;

    printf ("**********************************\n");
    printf ("* TEST CGNS WITH HDF5 CORE FD    *\n");
    printf ("**********************************\n");
    fflush (stdout);

    /*
     * INITIAL STATE: FILE DOES NOT EXIST
     * +------+-------+--------+----------------+
     * |        CG_MODE_       | DISKLESS MODE  |
     * +------+-------+--------+----------------+
     * | READ | WRITE | MODIFY | MEMORY TO FILE |
     * +------+-------+--------+----------------+
     * |  N   |  Y    |  N     |        N       |
     * +------+-------+--------+----------------+
    */

    printf ("  Testing...MODE_WRITE/MEM_ONLY...");
    /* enable diskless mode */
    if (cg_configure (CG_CONFIG_HDF5_DISKLESS, (void *)1))
      cg_error_exit();
    /* disable committing to disk */
    if (cg_configure (CG_CONFIG_HDF5_DISKLESS_WRITE, (void *)0))
      cg_error_exit();

    if (cg_open (FILENAME, CG_MODE_WRITE, &cgfile)) cg_error_exit();
    if (cg_close (cgfile)) cg_error_exit();

    /* verify CGNS file was not created */
    if (ACCESS(FILENAME, F_OK) == 0) cg_error_exit();
    printf ("PASSED\n");
 
    /*
     * INITIAL STATE: FILE DOES NOT EXIST
     * +------+-------+--------+----------------+
     * |        CG_MODE_       | DISKLESS MODE  |
     * +------+-------+--------+----------------+
     * | READ | WRITE | MODIFY | MEMORY TO FILE |
     * +------+-------+--------+----------------+
     * |  N   |  Y    |  N     |        Y       |
     * +------+-------+--------+----------------+
    */
    printf ("  Testing...MODE_WRITE/MEMORY_TO_FILE...");
    /* enable diskless mode */
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS, (void *)1))
      cg_error_exit();
    /* enable committing to disk */
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS_WRITE, (void *)1))
      cg_error_exit();
    
    if (cg_open (FILENAME, CG_MODE_WRITE, &cgfile)) cg_error_exit();
    if (cg_close (cgfile)) cg_error_exit();

    /* verify CGNS file was created */
    if (ACCESS(FILENAME, F_OK) != 0) cg_error_exit();

    /* verify valid CGNS file
     *  - expected to fail since cg_is_cgns opens file read-only,
     *    and diskless mode is set to write */
    ier = cg_is_cgns(FILENAME, &file_type);
    if(ier != CG_ERROR) cg_error_exit();
    printf ("PASSED\n");

    /*
     * INITIAL STATE: FILE EXISTS -- MODIFY DATA
     * +------+-------+--------+----------------+
     * |        CG_MODE_       | DISKLESS MODE  |
     * +------+-------+--------+----------------+
     * | READ | WRITE | MODIFY | MEMORY TO FILE |
     * +------+-------+--------+----------------+
     * |  N   |  N    |  Y     |        N       |
     * +------+-------+--------+----------------+
    */
    printf ("  Testing...MODE_WRITE/MEMORY_ONLY -- DATA MODIFY...");
    /* enable diskless mode */
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS, (void *)1))
      cg_error_exit();
    /* disable committing to disk */
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS_WRITE, (void *)0))
      cg_error_exit();

    ier = cg_open (FILENAME, CG_MODE_MODIFY, &cgfile);
    if(ier == CG_ERROR)  cg_error_exit();
    /* write some data to the memory state */
    if(cg_base_write(cgfile, BASENAME, cell_dim, phys_dim, &B) != CG_OK) {
      printf("*FAILED* cg_base_write \n");
      cg_error_exit();
    }
    if (cg_close (cgfile)) cg_error_exit();

    /* verify memory state did not get written to disk */
    if( cg_open(FILENAME, CG_MODE_MODIFY, &cgfile) == CG_ERROR)
      cg_error_exit();
    if(cg_gopath(cgfile, BASENAME_PATH) == CG_OK)
      printf("*FAILED* memory state written to disk \n");
    if( cg_close(cgfile)) cg_error_exit();
    printf ("PASSED\n");
    /*
     * INITIAL STATE: FILE EXISTS -- MODIFY DATA
     * +------+-------+--------+----------------+
     * |        CG_MODE_       | DISKLESS MODE  |
     * +------+-------+--------+----------------+
     * | READ | WRITE | MODIFY | MEMORY TO FILE |
     * +------+-------+--------+----------------+
     * |  N   |  N    |  Y     |        Y       |
     * +------+-------+--------+----------------+
    */
    printf ("  Testing...MODE_WRITE/MEMORY_TO_FILE -- DATA MODIFY...");
    /* enable committing to disk */
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS, (void *)1))
      cg_error_exit();
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS_WRITE, (void *)1))
      cg_error_exit();
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS_INCR, (void *)(1*1024*1024)))
        cg_error_exit();

    if( cg_open (FILENAME, CG_MODE_MODIFY, &cgfile) == CG_ERROR)
      cg_error_exit();

    /* write some data to the memory state */
    if(cg_base_write(cgfile, BASENAME, cell_dim, phys_dim, &B) != CG_OK) {
      printf("*FAILED* cg_base_write \n");
      cg_error_exit();
    }
    if (cg_close (cgfile)) cg_error_exit();

    /* verify memory state was written to disk */
    if( cg_open(FILENAME, CG_MODE_MODIFY, &cgfile) == CG_ERROR)
      cg_error_exit();
    if(cg_gopath(cgfile,BASENAME_PATH) != CG_OK)
      printf("*FAILED* memory state not written to disk \n");
    if( cg_close(cgfile)) cg_error_exit();
    printf ("PASSED\n");
#endif
    return 0;
}

