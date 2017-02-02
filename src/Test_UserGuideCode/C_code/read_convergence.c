/*   Program read_convergence   */
/*
Reads convergence history from an existing CGNS file.

The CGNS grid file 'grid_c.cgns' must already exist
and a convergence history should be in it (using write_convergence.c).

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_convergence.c
cc -o read_convergence_c read_convergence.o -L ../../lib -lcgns

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

#define ntt 20

int main()
{
    float cl[ntt];
    int index_file,index_base,narrays,index_array,ndim;
    char arrayname[33];
    CGNS_ENUMT(DataType_t) itype;
    cgsize_t idim;

/* READ CONVERGENCE HISTORY INFORMATION FROM EXISTING CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* go to base node */
    cg_goto(index_file,index_base,"end");
/* go to history node (we assume it exists and that there is only one -  */
/* real working code would check!) */
    cg_goto(index_file,index_base,"ConvergenceHistory_t",1,"end");
/* find out how many arrays are here (there should be only one!): */
    cg_narrays(&narrays);
    index_array=narrays;
/* some checks: */
    if (narrays != 1)
    {
      printf("\nError!  Expecting only one array, read %i\n",narrays);
      return 1;
    }
    cg_array_info(index_array,arrayname,&itype,&ndim,&idim);
    if (idim > ntt)
    {
      printf("\nError! must increase ntt to at least %i\n",(int)idim);
      return 1;
    }
    if (strcmp(arrayname,"CoefLift") != 0)
    {
      printf("\nError!  expecting CoefLift, read %s\n",arrayname);
      return 1;
    }
/* read lift coefficient array */
    cg_array_read_as(index_array,CGNS_ENUMV(RealSingle),cl);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read cl history from file grid_c.cgns\n");
    printf("   values are: %f, %f, %f, %f, %f\n",cl[0],cl[1],cl[2],cl[3],cl[4]);
    printf("               %f, %f, %f, %f, %f\n",cl[5],cl[6],cl[7],cl[8],cl[9]);
    return 0;
}
