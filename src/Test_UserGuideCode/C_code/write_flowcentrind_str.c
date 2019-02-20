/*    Program write_flowcentrind_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid, and adds a flow solution (at CELL CENTERS
PLUS RIND CELLS IN I AND J DIRECTIONS) to it.
(Compare this program with write_flowcent_str)

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_flowcentrind_str.c
cc -o write_flowcentrind_str_c write_flowcentrind_str.o -L ../../lib -lcgns

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
  r and p must be dimensioned exactly as [N-1][17-1+2][21-1+2] (N>=9)
  for this particular case or else they will be written to
  the CGNS file incorrectly!  Other options are to use
  cg_field_general_write, use 1-D arrays, use dynamic memory, or
  pass index values to a subroutine and dimension exactly there):
  Rind cells are stored in array locations [k][0][i], [k][17][i], [k][j][0], [k][j][21]
*/
    double r[8][18][22],p[8][18][22];
    int ni,nj,nk,i,j,k,kk,index_file,index_base,index_zone,index_flow,index_field;
    int irinddata[6];
    char solname[33];

    printf("\nProgram write_flowcentrind_str\n");

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
          r[k][j+1][i+1]=(float)i;
          p[k][j+1][i+1]=(float)j;
        }
      }
    }
/* create rind cell data: */
    for (k=0; k < nk; k++)
    {
      kk=k+1;
      for (j=0; j <= nj+1; j++)
      {
        r[k][j][0]=999.+(float)j+(5.*(float)kk);
        p[k][j][0]=999.+(float)j+(5.*(float)kk)+1.;
        r[k][j][ni+1]=-999.-(float)j-(5.*(float)kk);
        p[k][j][ni+1]=-999.-(float)j-(5.*(float)kk)-1.;
      }
    }
    for (k=0; k < nk; k++)
    {
      kk=k+1;
      for (i=0; i <= ni+1; i++)
      {
        r[k][0][i]=888.+(float)i+(5.*(float)kk);
        p[k][0][i]=888.+(float)i+(5.*(float)kk)+1.;
        r[k][nj+1][i]=-888.-(float)i-(5.*(float)kk);
        p[k][nj+1][i]=-888.-(float)i-(5.*(float)kk)-1.;
      }
    }
    printf("\ncreated simple 3-D rho and p flow solution with rind data\n");

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
/* go to position within tree at FlowSolution_t node */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowSolution_t",index_flow,"end");
/* write rind information under FlowSolution_t node (ilo,ihi,jlo,jhi,klo,khi) */
    irinddata[0]=1;
    irinddata[1]=1;
    irinddata[2]=1;
    irinddata[3]=1;
    irinddata[4]=0;
    irinddata[5]=0;
    cg_rind_write(irinddata);
/* write flow solution (user must use SIDS-standard names here) */
    cg_field_write(index_file,index_base,index_zone,index_flow,
        CGNS_ENUMV(RealDouble),"Density",r[0][0],&index_field);
    cg_field_write(index_file,index_base,index_zone,index_flow,
        CGNS_ENUMV(RealDouble),"Pressure",p[0][0],&index_field);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added flow solution data to file grid_c.cgns\n");
    printf("\nNote:  if the original CGNS file already had a FlowSolution_t node,");
    printf("\n          it has been overwritten\n");
    return 0;
}
