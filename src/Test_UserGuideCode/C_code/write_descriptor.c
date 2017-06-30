/*   Program write_descriptor   */
/*
Adds descriptor node to an existing CGNS file (under the
CGNSBase_t node).

The CGNS grid file 'grid_c.cgns' must already exist.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_descriptor.c
cc -o write_descriptor_c write_descriptor.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
#include <string.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#endif

int main()
{
    int index_file,index_base;
    char textstring[74];

    printf("\nProgram write_descriptor\n");

/* WRITE DESCRIPTOR NODE AT BASE LEVEL */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* go to base node */
    cg_goto(index_file,index_base,"end");
/* write descriptor node (user can give any name) */
    strcpy(textstring,"Supersonic vehicle with landing gear\n");
    strcat(textstring,"M=4.6, Re=6 million");
    cg_descriptor_write("Information",textstring);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully wrote descriptor node to file grid_c.cgns\n");
    return 0;
}
