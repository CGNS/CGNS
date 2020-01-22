/*   Program read_flowvert_ho_unst   */
/*
Opens an existing CGNS file that contains a simple 3-D
unstructured grid plus a flow solution (at VERTICES) and corresponding statial and temporal orders,
and reads it.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_unst.c followed by
write_flowvert_ho_unst.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_flowvert_ho_unst.c
cc -o read_flowvert_ho_unst read_flowvert_ho_unst.o -L ../../lib -lcgns

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
    float r[9*17*21],p[9*17*21];
    cgsize_t isize[3][1],irmin,irmax;
    int index_file,index_base,index_zone,index_flow;
    char zonename[33],solname[33];
    CGNS_ENUMT(GridLocation_t) loc;
    int so,to;

/* READ FLOW SOLUTION FROM CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_ho_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* we know there is only one FlowSolution_t (real working code would check!) */
    index_flow=1;
/* get zone size (and name - although not needed here) */
    cg_zone_read(index_file,index_base,index_zone,zonename,isize[0]);
/* lower range index */
    irmin=1;
/* upper range index - use vertex dimensions */
/* checking GridLocation first (real working code would check */
/* to make sure there are no Rind cells also!): */
    cg_sol_info(index_file,index_base,index_zone,index_flow,solname,&loc);
    if (loc != CGNS_ENUMV(ElementBased))
    {
      printf("\nError, GridLocation must be ElementBased! Currently: %s\n",
          GridLocationName[loc]);
      return 1;
    }
/* read flow solution spatial and temporal orders*/
    cg_sol_interpolation_order_read(index_file,index_base,index_zone,index_flow,&so,&to);
    if (so != 3 || to != 0)
    {
        fprintf( stderr, "ERROR: wrong values for solution interpolation orders. Needed (3,0) get(%d,%d)", so,to);
        return -1;
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read flow solution orders (spatial and temporal) from file grid_ho_c.cgns\n");
    printf("  spatial  Order = %d (should be 3)\n",so);
    printf("  temporal Order = %d (should be 0)\n",to);
    printf("\nProgram successful... ending now\n");
    return 0;
}
