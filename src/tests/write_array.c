#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef _WIN32
# include <io.h>
# define unlink _unlink
#else
# include <unistd.h>
#endif
#include "utils.h"

int main (int argc, char **argv)
{
    int na, narrays = 100;
    cgsize_t arraysize = 1024;
    int cgfile, cgbase;
    char name[33];
    float *array, exps[5];
    double start, end;
    static char *fname = "array.cgns";
    char *endptr;

    if (argc > 1) {
        /* Get safely input values */
        long input_value;
        errno = 0;
        input_value = strtol(argv[1], &endptr, 10);
        if (errno == ERANGE){
            fprintf (stderr, "overflow when converting narrays input to int\n");
            exit(1);
        }
        if (endptr == argv[1]){
            fprintf (stderr, "impossible to convert narrays input to int\n");
            exit(1);
        }
        else {
            narrays = (int) input_value;
        }
        if (argc > 2){
            errno = 0;
            input_value = strtol(argv[2], &endptr, 10);
            if (errno == ERANGE){
                fprintf (stderr, "overflow when converting array_size input to int\n");
                exit(1);
            }
            if (endptr == argv[2]){
                fprintf (stderr, "impossible to convert array_size input to int\n");
                exit(1);
            }
            else {
                arraysize = (cgsize_t) input_value;
            }
	}
    }
    /* validate input */
    if (narrays < 1 || arraysize < 1){
        fprintf (stderr, "Invalid array size\n");
        exit(1);
    }
    printf ("writing %d arrays of size %d\n", narrays, (int)arraysize);
    array = (float *) malloc ((size_t)(arraysize * sizeof(float)));
    if (NULL == array) {
        fprintf (stderr, "malloc failed\n");
        exit (1);
    }
    for (na = 0; na < arraysize; na++)
        array[na] = (float)na;
    exps[0] = 1.0;
    for (na = 1; na < 5; na++)
        exps[na] = 0.0;

    unlink (fname);
    start = elapsed_time ();
    if (cg_open (fname, CG_MODE_WRITE, &cgfile) ||
        cg_base_write (cgfile, "Base", 3, 3, &cgbase) ||
        cg_goto (cgfile, cgbase, NULL) ||
        cg_dataclass_write(CGNS_ENUMV(Dimensional)) ||
        cg_units_write(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter),
            CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), CGNS_ENUMV(Radian)) ||
        cg_user_data_write ("Data") ||
        cg_goto (cgfile, cgbase, "UserDefinedData_t", 1, NULL))
        cg_error_exit();

    for (na = 1; na <= narrays; na++) {
        sprintf (name, "Array%d", na);
        if (cg_array_write (name, CGNS_ENUMV(RealSingle),1, &arraysize, array) ||
        cg_gopath(cgfile, name) ||
        cg_exponents_write(CGNS_ENUMV(RealSingle), exps) ||
        cg_gopath(cgfile, ".."))
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
        if (cg_array_write (name, CGNS_ENUMV( RealSingle ), 1, &arraysize, array))
            cg_error_exit ();
    }
    if (cg_close(cgfile)) cg_error_exit();
    end = elapsed_time ();
    printf ("time = %g secs, size = %g Mb\n",
        end - start, file_size(fname));
#endif

    cg_free(array);

    return 0;
}

