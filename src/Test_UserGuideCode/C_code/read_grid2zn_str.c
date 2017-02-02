/*   Program read_grid2zn_str   */
/*
Reads simple 3-D structured 2-zone grid from CGNS file
(companion program to write_grid2zn_str.c)

The CGNS grid file 'grid_c.cgns' must already exist.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_grid2zn_str.c
cc -o read_grid2zn_str_c read_grid2zn_str.o -L ../../lib -lcgns

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
  x,y,z must be dimensioned exactly as [N][17][21] (N>=9)
  for this particular case or else they will be read from
  the CGNS file incorrectly!  Other options are to use 1-D
  arrays, use dynamic memory, or pass index values to a
  subroutine and dimension exactly there):
*/
    float x[9][17][21],y[9][17][21],z[9][17][21];
    float xsav[2][9][17][21],ysav[2][9][17][21],zsav[2][9][17][21];
    cgsize_t isize[3][3],irmin[3],irmax[3];
    int index_file,index_base,nzone,index_zone,i,j,k;
    char zonename[33];

/* READ X, Y, Z GRID POINTS FROM CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* get number of zones (should be 2 for our case) */
    cg_nzones(index_file,index_base,&nzone);
    if (nzone != 2)
    {
      printf("\nError.  This program expects 2 zones. %i read\n",nzone);
      return 1;
    }
/* do loop over the zones */
    for (index_zone=1; index_zone <= nzone; index_zone++)
    {
/* get zone size (and name - although not needed here) */
      cg_zone_read(index_file,index_base,index_zone,zonename,isize[0]);
/* lower range index */
      irmin[0]=1;
      irmin[1]=1;
      irmin[2]=1;
/* upper range index of vertices */
      irmax[0]=isize[0][0];
      irmax[1]=isize[0][1];
      irmax[2]=isize[0][2];
/* read grid coordinates */
      cg_coord_read(index_file,index_base,index_zone,"CoordinateX", \
                    CGNS_ENUMV(RealSingle),irmin,irmax,x[0][0]);
      cg_coord_read(index_file,index_base,index_zone,"CoordinateY", \
                    CGNS_ENUMV(RealSingle),irmin,irmax,y[0][0]);
      cg_coord_read(index_file,index_base,index_zone,"CoordinateZ", \
                    CGNS_ENUMV(RealSingle),irmin,irmax,z[0][0]);
/* store grid coordinates in xsav,ysav,zsav array: */
      for (i=0; i < isize[0][0]; i++)
      {
        for (j=0; j < isize[0][1]; j++)
        {
          for (k=0; k < isize[0][2]; k++)
          {
            xsav[index_zone-1][k][j][i]=x[k][j][i];
            ysav[index_zone-1][k][j][i]=y[k][j][i];
            zsav[index_zone-1][k][j][i]=z[k][j][i];
          }
        }
      }
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read grid from file grid_c.cgns\n");
    printf("  For example, zone 1 x,y,z[8][16][20]= %f, %f, %f\n", \
           xsav[0][8][16][20],ysav[0][8][16][20],zsav[0][8][16][20]);
    printf("               zone 2 x,y,z[8][16][20]= %f, %f, %f\n", \
           xsav[1][8][16][20],ysav[1][8][16][20],zsav[1][8][16][20]);
    printf("\nProgram successful... ending now\n");
    return 0;
}
