/*   Program write_convergence   */
/*
Adds convergence history to an existing CGNS file.

The CGNS grid file 'grid_c.cgns' must already exist.

Example compilation for this program is (change paths!):

cc -I ../CGNS_CVS/cgnslib_2.5 -c write_convergence.c
cc -o write_convergence_c write_convergence.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns

(../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#else
# if CG_BUILD_SCOPE
#  error enumeration scoping needs to be off
# endif
#endif

#define ntt 20

int main()
{
    double cl[ntt];
    int n,index_file,index_base;
    cgsize_t nuse;

/* create history array simple example: */
    for (n=0; n < ntt; n++)
    {
      cl[n]=(float)n+1.;
    }
    printf("\ncreated simple cl history\n");

/* WRITE CONVERGENCE HISTORY INFORMATION TO EXISTING CGNS FILE */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* go to base node */
    cg_goto(index_file,index_base,"end");
/* create history node (SIDS names it GlobalConvergenceHistory at base level) */
    cg_convergence_write(ntt,"");
/* go to new history node */
    cg_goto(index_file,index_base,"ConvergenceHistory_t",1,"end");
/* write lift coefficient array (user must use SIDS-standard name here) */
    nuse=ntt;
    cg_array_write("CoefLift",RealDouble,1,&nuse,&cl);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully wrote cl history to file grid_c.cgns\n");
    return 0;
}
