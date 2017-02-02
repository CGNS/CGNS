/*    Program write_flowvert_unst   */
/*
Opens an existing CGNS file that contains a simple 3-D
unstructured grid, and adds a flow solution (at VERTICES)
to it.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_unst.c)
Note that, other than the dimensions of the variables
r and p, this program is essentially identical to that for
writing flow solutions to a structured zone:
write_flowvert_str.c

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_flowvert_unst.c
cc -o write_flowvert_unst_c write_flowvert_unst.o -L ../../lib -lcgns

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
    double r[9*17*21],p[9*17*21];
    int ni,nj,nk,i,j,k,index_file,index_base,index_zone,index_flow,index_field,iset;
    char solname[33];

    printf("\nProgram write_flowvert_unst\n");

/* create fake flow solution AT CELL CENTERS for simple example: */
    ni=21;
    nj=17;
    nk=9;
    iset=0;
    for (k=0; k < nk; k++)
    {
      for (j=0; j < nj; j++)
      {
        for (i=0; i < ni; i++)
        {
          r[iset]=(float)i;
          p[iset]=(float)j;
          iset=iset+1;
        }
      }
    }
    printf("\ncreated simple 3-D rho and p flow solution\n");

/* WRITE FLOW SOLUTION TO EXISTING CGNS FILE */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* define flow solution node name (user can give any name) */
    strcpy(solname,"FlowSolution");
/* create flow solution node */
    cg_sol_write(index_file,index_base,index_zone,solname,CGNS_ENUMV(Vertex),&index_flow);
/* write flow solution (user must use SIDS-standard names here) */
    cg_field_write(index_file,index_base,index_zone,index_flow,
        CGNS_ENUMV(RealDouble),"Density",r,&index_field);
    cg_field_write(index_file,index_base,index_zone,index_flow,
        CGNS_ENUMV(RealDouble),"Pressure",p,&index_field);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added Vertex flow solution data to file grid_c.cgns (unstructured)\n");
    printf("\nNote:  if the original CGNS file already had a FlowSolution_t node,");
    printf("\n          it has been overwritten\n");
    return 0;
}
