/*   Program write_bcpnts_unst   */
/*
Opens an existing CGNS file that contains a simple 3-D
unstructured grid, and adds BC definitions (defined
as individual FaceCenter "points" = PointList+GridLocation=FaceCenter)
The BCs are added as FaceCenter points, associated with
face elements (QUAD_4), rather than associated to nodes

For the following, be sure you are using Version 2.0 or
later release of the API

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_unst.c).  Note: whether the
existing CGNS file has a flow solution in it already or
not is irrelevant.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_bcpnts_unst.c
cc -o write_bcpnts_unst_c write_bcpnts_unst.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#endif

#define maxcount 960

int main()
{
    int index_file,index_base,index_zone;
    int nelem_start,nelem_end,icount,index_bc,ibc,n;
    cgsize_t ipnts[maxcount],icounts;

    printf("\nProgram write_bcpnts_unst\n");

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
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Ilo",CGNS_ENUMV(BCTunnelInflow),
        CGNS_ENUMV(PointList),icounts,ipnts,&index_bc);
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
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Ihi",CGNS_ENUMV(BCExtrapolate),
        CGNS_ENUMV(PointList),icounts,ipnts,&index_bc);
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
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Walls",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointList),icounts,ipnts,&index_bc);

/* the above are all face-center locations for the BCs - must indicate this, */
/* otherwise Vertices will be assumed! */
    for (ibc=1; ibc <= index_bc; ibc++)
    {
/*    (the following call positions you in BC_t - it assumes there */
/*    is only one Zone_t and one ZoneBC_t - real working code would check!) */
      cg_goto(index_file,index_base,"Zone_t",1,"ZoneBC_t",1,"BC_t",ibc,"end");
      cg_gridlocation_write(CGNS_ENUMV(FaceCenter));
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added FaceCenter BCs (PointList) to unstructured grid file grid_c.cgns\n");
    return 0;
}
