/*   Program read_flowvert_unst   */
/*
Opens an existing CGNS file that contains a simple 3-D
unstructured grid plus a flow solution (at VERTICES),
and reads it.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_unst.c followed by
write_flowvert_unst.c)
Note that, other than the dimensions of the variables
r and p, this program is very similar to that for
reading flow solutions from a structured zone:
read_flowvert_str.c

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_flowvert_unst.c
cc -o read_flowvert_unst_c read_flowvert_unst.o -L ../../lib -lcgns

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

/* READ FLOW SOLUTION FROM CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
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
    if (loc != CGNS_ENUMV(Vertex))
    {
      printf("\nError, GridLocation must be Vertex! Currently: %s\n",
          GridLocationName[loc]);
      return 1;
    }
    irmax=isize[0][0];
/* read flow solution */
    cg_field_read(index_file,index_base,index_zone,index_flow,"Density", \
                  CGNS_ENUMV(RealSingle),&irmin,&irmax,r);
    cg_field_read(index_file,index_base,index_zone,index_flow,"Pressure", \
                  CGNS_ENUMV(RealSingle),&irmin,&irmax,p);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read flow solution from file grid_c.cgns\n");
    printf("  For example, r,p[379] = %f, %f\n",r[379],p[379]);
    printf("               r,p[3212]= %f, %f\n",r[3212],p[3212]);
    printf("\nProgram successful... ending now\n");
    return 0;
}
