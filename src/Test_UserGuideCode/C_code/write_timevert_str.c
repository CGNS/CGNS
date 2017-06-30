/*   Program write_timevert_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid, and adds 3 different flow solutions
(at VERTICES) to it, along with time-accurate info.
In this example, r1 & p1, r2 & p2, r3 & p3 correspond
with solutions at 3 different time steps.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_timevert_str.c
cc -o write_timevert_str_c write_timevert_str.o -L ../../lib -lcgns

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
r1, r2, r3 and p1, p2, p3
must be dimensioned exactly as [N][17][21] (N>=9)
for this particular case or else they will be written to
the CGNS file incorrectly!  Other options are to use 1-D
arrays, use dynamic memory, or pass index values to a
subroutine and dimension exactly there):
*/
    double r1[9][17][21],p1[9][17][21];
    double r2[9][17][21],p2[9][17][21];
    double r3[9][17][21],p3[9][17][21];
    double time[3];
    int ni,nj,nk,i,j,k,index_file,index_base,index_zone;
    cgsize_t idata[2],nuse;
    int index_flow,index_field,nsteps,n;
    char sn[3][33];
    char solname[97];  /* need an extra byte for the terminating 0 */

    printf("\nProgram write_timevert_str\n");

/* set up the times corresponding to the 3 solutions to be stored: */
    time[0]=10.;
    time[1]=20.;
    time[2]=50.;
/* create fake flow solution AT VERTICES for simple example: */
    ni=21;
    nj=17;
    nk=9;
    for (k=0; k < nk; k++)
    {
      for (j=0; j < nj; j++)
      {
        for (i=0; i < ni; i++)
        {
/*        soln at time 1: */
          r1[k][j][i]=(float)i;
          p1[k][j][i]=(float)j;
/*        soln at time 2: */
          r2[k][j][i]=r1[k][j][i]+1.;
          p2[k][j][i]=p1[k][j][i]+1.;
/*        soln at time 3: */
          r3[k][j][i]=r2[k][j][i]+1.;
          p3[k][j][i]=p2[k][j][i]+1.;
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
/* define 3 different solution names (user can give any names) */
    strcpy(sn[0],"FlowSolution1");
    strcpy(sn[1],"FlowSolution2");
    strcpy(sn[2],"FlowSolution3");
/*    sprintf(solname, "%-32s%-32s%-32s", "FlowSolution1", "FlowSolution2", "FlowSolution3"); */
    sprintf(solname,"%-32s%-32s%-32s",sn[0],sn[1],sn[2]);
/* do loop for the 3 solutions: */
    for (n=0; n < 3; n++)
    {
/* create flow solution node */
      cg_sol_write(index_file,index_base,index_zone,sn[n],CGNS_ENUMV(Vertex),&index_flow);
      printf("\n ...writing solution number %d\n",index_flow);
/* write flow solution (user must use SIDS-standard names here) */
      if (n == 0)
      {
        cg_field_write(index_file,index_base,index_zone,index_flow,CGNS_ENUMV(RealDouble),"Density",
                       r1[0][0],&index_field);
        cg_field_write(index_file,index_base,index_zone,index_flow,CGNS_ENUMV(RealDouble),"Pressure",
                       p1[0][0],&index_field);
      }
      else if (n == 1)
      {
        cg_field_write(index_file,index_base,index_zone,index_flow,CGNS_ENUMV(RealDouble),"Density",
                       r2[0][0],&index_field);
        cg_field_write(index_file,index_base,index_zone,index_flow,CGNS_ENUMV(RealDouble),"Pressure",
                       p2[0][0],&index_field);
      }
      else
      {
        cg_field_write(index_file,index_base,index_zone,index_flow,CGNS_ENUMV(RealDouble),"Density",
                       r3[0][0],&index_field);
        cg_field_write(index_file,index_base,index_zone,index_flow,CGNS_ENUMV(RealDouble),"Pressure",
                       p3[0][0],&index_field);
      }
    }
/* create BaseIterativeData */
    nsteps=3;
    cg_biter_write(index_file,index_base,"TimeIterValues",nsteps);
/* go to BaseIterativeData level and write time values */
    cg_goto(index_file,index_base,"BaseIterativeData_t",1,"end");
    nuse=3;
    cg_array_write("TimeValues",CGNS_ENUMV(RealDouble),1,&nuse,&time);
/* create ZoneIterativeData */
    cg_ziter_write(index_file,index_base,index_zone,"ZoneIterativeData");
/* go to ZoneIterativeData level and give info telling which */
/* flow solution corresponds with which time (solname(1) corresponds */
/* with time(1), solname(2) with time(2), and solname(3) with time(3)) */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"ZoneIterativeData_t",1,"end");
    idata[0]=32;
    idata[1]=3;
    cg_array_write("FlowSolutionPointers",CGNS_ENUMV(Character),2,idata,solname);
/* add SimulationType */
    cg_simulation_type_write(index_file,index_base,CGNS_ENUMV(TimeAccurate));
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added 3 times of flow solution data and time info to file grid_c.cgns\n");
    printf("   Note:  if the original CGNS file already had a FlowSolution_t node,\n");
    printf("          it has been overwritten\n");
    return 0;
}
