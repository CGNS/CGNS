/*   Program read_timevert_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid plus 3 different flow solutions (at VERTICES),
along with time-accurate info, and reads it.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c followed by
write_timevert_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_timevert_str.c
cc -o read_timevert_str_c read_timevert_str.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <string.h>
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
  r1, r2, r3 and p1, p2, p3
  must be dimensioned exactly as [N][17][21] (N>=9)
  for this particular case or else they will be written to
  the CGNS file incorrectly!  Other options are to use 1-D
  arrays, use dynamic memory, or pass index values to a
  subroutine and dimension exactly there):
*/
    float r1[9][17][21],p1[9][17][21];
    float r2[9][17][21],p2[9][17][21];
    float r3[9][17][21],p3[9][17][21];
    float time[3];
    cgsize_t isize[3][3],irmin[3],irmax[3];
    int index_file,index_base,index_zone,nsteps,narrays;
    int id1,n;
    cgsize_t idims[2],id2;
    char zonename[33],bitername[33],zitername[33];
    char arrayname[33];
    char solname[97],solname2[33];
    char sn[3][33];
    CGNS_ENUMT(DataType_t) idatatype;
    CGNS_ENUMT(SimulationType_t) isim;
    CGNS_ENUMT(GridLocation_t) loc;

/* READ FLOW SOLUTION FROM CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* get zone size (and name - although not needed here) */
    cg_zone_read(index_file,index_base,index_zone,zonename,isize[0]);
/* lower range index */
    irmin[0]=1;
    irmin[1]=1;
    irmin[2]=1;
/* upper range index - use vertex dimensions */
    irmax[0]=isize[0][0];
    irmax[1]=isize[0][1];
    irmax[2]=isize[0][2];
/* read BaseIterativeData */
    cg_biter_read(index_file,index_base,bitername,&nsteps);
    printf("\nnumber of time steps stored = %i\n",nsteps);
    if (nsteps != 3)
    {
      printf("\nError, expecting nsteps=3!\n");
      return 1;
    }
    cg_goto(index_file,index_base,"BaseIterativeData_t",1,"end");
    cg_narrays(&narrays);
    if (narrays != 1)
    {
      printf("\nError, expecting 1 array in BaseIterativeData... there are %i\n",narrays);
      return 1;
    }
    cg_array_info(1,arrayname,&idatatype,&id1,&id2);
    if (id1 != 1 || id2 != 3)
    {
      printf("\nError, expecting data dimension and vector to be 1 and 3 in "
              "BaseIterativeData... read %i, %i\n",id1,(int)id2);
      return 1;
    }
    cg_array_read_as(1,CGNS_ENUMV(RealSingle),time);
    printf("\nTimes stored are:\n");
    for (n=0; n < nsteps; n++)
    {
      printf(" %f\n",time[n]);
    }
/* read ZoneIterativeData */
    cg_ziter_read(index_file,index_base,index_zone,zitername);
    cg_goto(index_file,index_base,"Zone_t",index_zone,"ZoneIterativeData_t",1,"end");
    cg_narrays(&narrays);
    if (narrays != 1)
    {
      printf("\nError, expecting 1 array in ZoneIterativeData... there are %i\n",narrays);
      return 1;
    }
    cg_array_info(1,arrayname,&idatatype,&id1,idims);
    if (id1 != 2 || idims[0] != 32)
    {
      printf("\nError, expecting data dimension and vector to  be 2 and 32 in "
             "ZoneIterativeData... read %i, %i\n",id1,(int)idims[0]);
      return 1;
    }
    cg_array_read_as(1,CGNS_ENUMV(Character),solname);
    strncpy(sn[0],&solname[0],32);
    strncpy(sn[1],&solname[32],32);
    strncpy(sn[2],&solname[64],32);
    printf("\nFlow solution names corresponding to each are:\n");
    for (n=0; n < nsteps; n++)
    {
      printf(" %.32s\n",sn[n]);
    }
/* read SimulationType */
    cg_simulation_type_read(index_file,index_base,&isim);
    printf("\nSimulation type is: %s\n",SimulationTypeName[isim]);
/* do loop to read flow solutions */
    for (n=1; n <= nsteps; n++)
    {
/* check that soln names match, and also check GridLocation (real  */
/* working code would check to make sure there are no Rind cells  */
/* also!): */
      cg_sol_info(index_file,index_base,index_zone,n,solname2,&loc);
      if (strncmp(solname2,sn[n-1],strlen(solname2)) != 0)
      {
        printf("\nError, soln names do not match\n");
        printf(" solname2=%s\n",solname2);
        printf(" sn[%i]=%s\n",n-1,sn[n-1]);
        return 1;
      }
      if (loc != CGNS_ENUMV(Vertex))
      {
        printf("\nError, GridLocation must be Vertex!  Currently: %s\n",
               GridLocationName[loc]);
        return 1;
      }
      if (n == 1)
      {
        cg_field_read(index_file,index_base,index_zone,n,"Density",
                      CGNS_ENUMV(RealSingle),irmin,irmax,r1[0][0]);
        cg_field_read(index_file,index_base,index_zone,n,"Pressure",
                      CGNS_ENUMV(RealSingle),irmin,irmax,p1[0][0]);
      }
      else if (n == 2)
      {
        cg_field_read(index_file,index_base,index_zone,n,"Density",
                      CGNS_ENUMV(RealSingle),irmin,irmax,r2[0][0]);
        cg_field_read(index_file,index_base,index_zone,n,"Pressure",
                      CGNS_ENUMV(RealSingle),irmin,irmax,p2[0][0]);
      }
      else
      {
        cg_field_read(index_file,index_base,index_zone,n,"Density",
                      CGNS_ENUMV(RealSingle),irmin,irmax,r3[0][0]);
        cg_field_read(index_file,index_base,index_zone,n,"Pressure",
                      CGNS_ENUMV(RealSingle),irmin,irmax,p3[0][0]);
      }
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read 3 flow solutions from file grid_c.cgns\n");
    printf("   For example, r1,p1[0][0][0]=%f, %f\n",r1[0][0][0],p1[0][0][0]);
    printf("                r2,p2[0][0][0]=%f, %f\n",r2[0][0][0],p2[0][0][0]);
    printf("                r3,p3[0][0][0]=%f, %f\n",r3[0][0][0],p3[0][0][0]);
    printf("   For example, r1,p1[8][16][20]=%f, %f\n",r1[8][16][20],p1[8][16][20]);
    printf("                r2,p2[8][16][20]=%f, %f\n",r2[8][16][20],p2[8][16][20]);
    printf("                r3,p3[8][16][20]=%f, %f\n",r3[8][16][20],p3[8][16][20]);

    printf("\nProgram successful... ending now\n");
    return 0;
}
