/*   Program read_descriptor   */
/*
Reads descriptor node (under CGNSBase_t) from an existing CGNS file.

The CGNS grid file 'grid_c.cgns' must already exist.
and a descriptor node should be in it (using write_descriptor.c).

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_descriptor.c
cc -o read_descriptor_c read_descriptor.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#endif

int main()
{
    int index_file,index_base,ndescriptors,n;
    char *text,name[33];

/* READ DESCRIPTOR FROM EXISTING CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* go to base node */
    cg_goto(index_file,index_base,"end");
/* find out how many descriptors are here: */
    cg_ndescriptors(&ndescriptors);
    for (n=1; n <= ndescriptors; n++)
    {
/* read descriptor */
      cg_descriptor_read(n,name,&text);
      printf("\nThe descriptor is:\n\n%s\n",text);
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read descriptors from file grid_c.cgns\n");
    return 0;
}
