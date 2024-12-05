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

