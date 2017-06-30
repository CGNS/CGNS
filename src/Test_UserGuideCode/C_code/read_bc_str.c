/*   Program read_bc_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid + BCs (in PointRange format), and reads the BCs

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c), and the BCs must also
already have been written (using write_bc_str.c).  Note: whether the
existing CGNS file has a flow solution in it already or
not is irrelevant.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_bc_str.c
cc -o read_bc_str_c read_bc_str.o -L ../../lib -lcgns

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
    int index_file,index_base,index_zone,nbocos,ib;
    int normalindex[3],ndataset;
    int normallist;
    char boconame[33];
    CGNS_ENUMT(BCType_t) ibocotype;
    CGNS_ENUMT(PointSetType_t) iptset;
    CGNS_ENUMT(DataType_t) normaldatatype;
    cgsize_t ipnts[2][3];
    cgsize_t npts,normallistflag;

/* READ BOUNDARY CONDITIONS FROM EXISTING CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* find out number of BCs that exist under this zone */
    cg_nbocos(index_file,index_base,index_zone,&nbocos);
/* do loop over the total number of BCs */
    for (ib=1; ib <= nbocos; ib++)
    {
/* get BC info */
      cg_boco_info(index_file,index_base,index_zone,ib,boconame,&ibocotype,
                   &iptset,&npts,normalindex,&normallistflag,&normaldatatype,&ndataset);
      if (iptset != CGNS_ENUMV(PointRange))
      {
        printf("\nError.  For this program, BCs must be set up as PointRange type %s\n",
            PointSetTypeName[iptset]);
        return 1;
      }
      printf("\nBC number: %i\n",ib);
      printf("   name= %s\n",boconame);
      printf("   type= %s\n",BCTypeName[ibocotype]);
/* read point range in here */
      cg_boco_read(index_file,index_base,index_zone,ib,ipnts[0],&normallist);
      printf("   i-range= %i,%i\n",(int)ipnts[0][0],(int)ipnts[1][0]);
      printf("   j-range= %i,%i\n",(int)ipnts[0][1],(int)ipnts[1][1]);
      printf("   k-range= %i,%i\n",(int)ipnts[0][2],(int)ipnts[1][2]);
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read BCs (PointRange format) from file grid_c.cgns\n");
    return 0;
}
