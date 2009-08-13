#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
# include <unistd.h>
#endif
#include "utils.h"

int main (int argc, char **argv)
{
    int na, narrays = 100, arraysize = 1024;
    int cgfile, cgbase;
    char name[33];
    float *array;
    double start, end;
    static char *fname = "array.cgns";

    if (argc > 1) {
        narrays = atoi (argv[1]);
        if (argc > 2)
            arraysize = atoi (argv[2]);
    }
    printf ("writing %d arrays of size %d\n", narrays, arraysize);
    array = (float *) malloc (arraysize * sizeof(float));
    if (NULL == array) {
        fprintf (stderr, "malloc failed\n");
        exit (1);
    }
    for (na = 0; na < arraysize; na++)
        array[na] = (float)na;

    unlink (fname);
    start = elapsed_time ();
    if (cg_open (fname, CG_MODE_WRITE, &cgfile) ||
        cg_base_write (cgfile, "Base", 3, 3, &cgbase) ||
        cg_goto (cgfile, cgbase, NULL) ||
        cg_user_data_write ("Data") ||
        cg_goto (cgfile, cgbase, "UserDefinedData_t", 1, NULL))
        cg_error_exit();

    for (na = 1; na <= narrays; na++) {
        sprintf (name, "Array%d", na);
        if (cg_array_write (name, RealSingle, 1, &arraysize, array))
            cg_error_exit ();
    }
    if (cg_close(cgfile)) cg_error_exit();
    end = elapsed_time ();
    printf ("time = %g secs, size = %g Mb\n",
        end - start, file_size(fname));
#if 0
    puts ("rewriting the file");
    fflush (stdout);
    start = elapsed_time ();
    if (cg_open (fname, CG_MODE_MODIFY, &cgfile)) cg_error_exit();
    cgbase = 1;

    if (cg_goto (cgfile, cgbase, "UserDefinedData_t", 1, NULL))
        cg_error_exit();
    for (na = 1; na <= narrays; na++) {
        sprintf (name, "Array%d", na);
        if (cg_array_write (name, RealSingle, 1, &arraysize, array))
            cg_error_exit ();
    }
    if (cg_close(cgfile)) cg_error_exit();
    end = elapsed_time ();
    printf ("time = %g secs, size = %g Mb\n",
        end - start, file_size(fname));
#endif
    return 0;
}

