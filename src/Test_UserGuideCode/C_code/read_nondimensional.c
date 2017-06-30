/*   Program read_nondimensional   */
/*
Opens an existing CGNS file and reads the DataClass and
ReferenceState appropriate for a completely
NONDIMENSIONAL data set.

The CGNS grid file 'grid_c.cgns' must already exist,
processed further using write_nondimensional.c.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_nondimensional.c
cc -o read_nondimensional_c read_nondimensional.o -L ../../lib -lcgns

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
    double data;
    int index_file,index_base,narrays,n,idim;
    char *state,arrayname[33];
    CGNS_ENUMT(DataClass_t) id;
    CGNS_ENUMT(DataType_t) idata;
    cgsize_t idimvec;

/* READ NONDIMENSIONAL INFO */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* read DataClass under Base */
    cg_goto(index_file,index_base,"end");
    cg_dataclass_read(&id);
    printf("\nDataClass = %s\n",DataClassName[id]);
    if (id != CGNS_ENUMV(NormalizedByUnknownDimensional))
    {
      printf("\nError!  Expecting NormalizedByUnknownDimensional\n");
      return 1;
    }
/* read ReferenceState under Base */
    cg_state_read(&state);
    printf("\nReferenceState = %s\n",state);
/* Go to ReferenceState node, read Mach array and its dataclass */
    cg_goto(index_file,index_base,"ReferenceState_t",1,"end");
/* find out how many data arrays */
    cg_narrays(&narrays);
    for (n=1; n <= narrays; n++)
    {
      cg_array_info(n,arrayname,&idata,&idim,&idimvec);
      if (idim != 1 || idimvec != 1)
      {
        printf("\nError! expecting idim,idimvec=1,1\n");
        printf("   they are idim,idimvec= %i, %i\n",idim,(int)idimvec);
        return 1;
      }
      cg_array_read_as(n,CGNS_ENUMV(RealDouble),&data);
      printf("Variable=%s\n",arrayname);
      printf("    data=%18.8f\n",data);
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read nondimensional info from file grid_c.cgns\n");
    return 0;
}
