#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

int main (int argc, char **argv)
{
    double start, finish;
    int cgfile, mode = CG_MODE_WRITE;
    const char *filename = "very_long_filename_to_test_buffer_overflow_handling_that_could_potentially_cause_issues_in_string_operation_and_memory_management.cgns";

    if (argc > 3) {
        fprintf (stderr, "open_cgns [-m] CGNSfile\n");
        exit (1);
    }
    if (argc > 2) {
        mode = CG_MODE_MODIFY;
#if 0
        if (cg_configure (CG_CONFIG_COMPRESS, (void *)1))
          cg_error_exit();
#endif
        filename = argv[2];
    }
    else if (argc == 2) {
      filename = argv[1];
    }

    int maxnum_files = 99;
    if (cg_configure(CG_CONFIG_GET_MAXIMUM_FILES,  &maxnum_files)) cg_error_exit();
#if CG_BUILD_HDF5
    /* HDF5 backend supports 1024 files */
    if (maxnum_files != 1024) cg_error_exit();
#else
    /* ADF backend supports more files (4095 or 16383 depending on NEW_ID_MAPPING) */
#ifdef NEW_ID_MAPPING
    if (maxnum_files != 0xfff) cg_error_exit();  /* 4095 */
#else
    if (maxnum_files != 0x3fff) cg_error_exit(); /* 16383 */
#endif
#endif

    printf ("opening cgns file <%s> ...", filename);
    fflush (stdout);
    start = elapsed_time ();
    if (cg_open (filename, mode, &cgfile)) cg_error_exit();
    finish = elapsed_time ();
    printf (" %g secs\n", finish - start);

    printf ("closing cgns file ...");
    fflush (stdout);
    start = elapsed_time ();
    cg_close (cgfile);
    finish = elapsed_time ();
    printf (" %g secs\n", finish - start);

    return 0;
}

