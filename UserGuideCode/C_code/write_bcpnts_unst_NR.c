/*   Program write_bcpnts_unst_NR   */
/*

c   *** This method, although accepted, is not recommended ***
c   *** As of V3.1.3, use of ElementRange and ElementList
c   *** has been deprecated.
c   *** See program write_bcpnts_unst instead ***

Opens an existing CGNS file that contains a simple 3-D
unstructured grid, and adds BC definitions (defined
as individual elements = ElementList, associated with
face elements (QUAD_4)

For the following, be sure you are using Version 2.0 or 
later release of the API

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_unst.c).  Note: whether the 
existing CGNS file has a flow solution in it already or
not is irrelevant.

Example compilation for this program is (change paths!):

cc -I ../CGNS_CVS/cgnslib_2.5 -c write_bcpnts_unst_NR.c
cc -o write_bcpnts_unst_NR_c write_bcpnts_unst_NR.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns

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

#define maxcount 960

int main()
{
    int index_file,index_base,index_zone;
    int nelem_start,nelem_end,icount,index_bc,n;
    cgsize_t ipnts[maxcount];

/* WRITE BOUNDARY CONDITIONS TO EXISTING CGNS FILE */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* we know that for the unstructured zone, the following face elements */
/* have been defined as inflow (real working code would check!): */
    nelem_start=2561;
    nelem_end=2688;
    icount=0;
    for (n=nelem_start; n <= nelem_end; n++)
    {
      ipnts[icount]=n;
      icount=icount+1;
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
/* write boundary conditions for ilo face */
    cg_boco_write(index_file,index_base,index_zone,"Ilo",BCTunnelInflow,
        ElementList,icount,ipnts,&index_bc);
/* we know that for the unstructured zone, the following face elements */
/* have been defined as outflow (real working code would check!): */
    nelem_start=2689;
    nelem_end=2816;
    icount=0;
    for (n=nelem_start; n <= nelem_end; n++)
    {
      ipnts[icount]=n;
      icount=icount+1;
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
/* write boundary conditions for ihi face */
    cg_boco_write(index_file,index_base,index_zone,"Ihi",BCExtrapolate,
        ElementList,icount,ipnts,&index_bc);
/* we know that for the unstructured zone, the following face elements */
/* have been defined as walls (real working code would check!): */
    nelem_start=2817;
    nelem_end=3776;
    icount=0;
    for (n=nelem_start; n <= nelem_end; n++)
    {
      ipnts[icount]=n;
      icount=icount+1;
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
/* write boundary conditions for wall faces */
    cg_boco_write(index_file,index_base,index_zone,"Walls",BCWallInviscid,
        ElementList,icount,ipnts,&index_bc);

/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added FaceCenter BCs (ElementList) to unstructured grid file grid_c.cgns\n");
    return 0;
}
