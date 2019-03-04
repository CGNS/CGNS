/*    Program write_flowcent_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid, and adds a flow solution (at CELL CENTERS)
to it.  (Compare this program with write_flowvert_str)

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_flowcent_str.c
cc -o write_flowcent_str_c write_flowcent_str.o -L ../../lib -lcgns

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
/*
  dimension statements (note that tri-dimensional arrays
  r and p must be dimensioned exactly as [N-1][17-1][21-1] (N>=9)
  for this particular case or else they will be written to
  the CGNS file incorrectly!  Other options are to use
  cg_field_general_write, use 1-D arrays, use dynamic memory, or
  pass index values to a subroutine and dimension exactly there):
*/
    double r[8][16][20],p[8][16][20];
    int ni,nj,nk,i,j,k,index_file,index_base,index_zone,index_flow,index_field;
    char solname[33];

    printf("\nProgram write_flowcent_str\n");

/* create fake flow solution AT CELL CENTERS for simple example: */
    ni=20;
    nj=16;
    nk=8;
    for (k=0; k < nk; k++)
    {
      for (j=0; j < nj; j++)
      {
        for (i=0; i < ni; i++)
        {
          r[k][j][i]=(float)i;
          p[k][j][i]=(float)j;
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
/* create flow solution node (NOTE USE OF CGNS_ENUMV(CellCenter) HERE) */
    cg_sol_write(index_file,index_base,index_zone,solname,CGNS_ENUMV(CellCenter),&index_flow);
/* write flow solution (user must use SIDS-standard names here) */
    cg_field_write(index_file,index_base,index_zone,index_flow,
        CGNS_ENUMV(RealDouble),"Density",r[0][0],&index_field);
    cg_field_write(index_file,index_base,index_zone,index_flow,
        CGNS_ENUMV(RealDouble),"Pressure",p[0][0],&index_field);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added CellCenter flow solution data to file grid_c.cgns\n");
    printf("\nNote:  if the original CGNS file already had a FlowSolution_t node,");
    printf("\n          it has been overwritten\n");
    return 0;
}
