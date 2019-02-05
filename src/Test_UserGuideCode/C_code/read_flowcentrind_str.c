/*   Program read_flowcentrind_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid plus a flow solution (at CELL CENTERS PLUS
RIND CELLS IN THE I AND J DIRECTIONS), and reads it.
(Compare this program with read_flowcent_str)

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c followed by
write_flowcentrind_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_flowcentrind_str.c
cc -o read_flowcentrind_str_c read_flowcentrind_str.o -L ../../lib -lcgns

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
/*
  dimension statements (note that tri-dimensional arrays
  r and p must be dimensioned exactly as [N-1][17-1+2][21-1+2] (N>=9)
  for this particular case or else they will be read from
  the CGNS file incorrectly!  Other options are to use 1-D
  arrays, use dynamic memory, or pass index values to a
  subroutine and dimension exactly there):
  Rind cells are stored in array locations [k][0][i], [k][17][i], [k][j][0], [k][j][21]
*/
    float r[8][18][22],p[8][18][22];
    int irinddata[6];
    int index_file,index_base,index_zone,index_flow;
    char zonename[33],solname[33];
    CGNS_ENUMT(GridLocation_t) loc;
    cgsize_t isize[3][3],irmin[3],irmax[3];

/* READ FLOW SOLUTION FROM CGNS FILE */
/* open CGNS file for read-only */
    cg_open("grid_c.cgns",CG_MODE_READ,&index_file);
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* we know there is only one FlowSolution_t (real working code would check!) */
    index_flow=1;
/* get zone size (and name - although not needed here) */
    cg_zone_read(index_file,index_base,index_zone,zonename,isize[0]);
/* go to position within tree at FlowSolution\_t node */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowSolution_t",index_flow,"end");
/* read rind data */
    cg_rind_read(irinddata);
/* lower range index */
    irmin[0]=1-irinddata[0];
    irmin[1]=1-irinddata[2];
    irmin[2]=1-irinddata[4];
/* upper range index - use cell dimensions */
/* checking GridLocation first: */
    cg_sol_info(index_file,index_base,index_zone,index_flow,solname,&loc);
    if (loc != CGNS_ENUMV(CellCenter))
    {
      printf("\nError, GridLocation must be CellCenter! Currently: %s\n",
          GridLocationName[loc]);
      return 1;
    }
    irmax[0]=isize[1][0]+irinddata[1];
    irmax[1]=isize[1][1]+irinddata[3];
    irmax[2]=isize[1][2]+irinddata[5];
/* read flow solution */
    cg_field_read(index_file,index_base,index_zone,index_flow,"Density",
                  CGNS_ENUMV(RealSingle),irmin,irmax,r[0][0]);
    cg_field_read(index_file,index_base,index_zone,index_flow,"Pressure",
                  CGNS_ENUMV(RealSingle),irmin,irmax,p[0][0]);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read flow solution from file grid_c.cgns\n");
    printf("  For example, r,p[7][16][20]= %f, %f\n",r[7][16][20],p[7][16][20]);
    printf("         rind: r,p[7][16][0]= %f, %f\n",r[7][16][0],p[7][16][0]);
    printf("         rind: r,p[7][16][21]= %f, %f\n",r[7][16][21],p[7][16][21]);
    printf("\nProgram successful... ending now\n");
    return 0;
}
