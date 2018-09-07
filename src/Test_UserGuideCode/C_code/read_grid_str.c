/*   Program read_grid_str   */
/*
Reads simple 3-D structured grid from CGNS file
(companion program to write_grid_str.c)

The CGNS grid file 'grid_c.cgns' must already exist.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_grid_str.c
cc -o read_grid_str_c read_grid_str.o -L ../../lib -lcgns

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
    cgsize_t isize[3][3],irmin[3],irmax[3];
    int index_file,index_base,index_zone;
    char zonename[33];

/* READ X, Y, Z GRID POINTS FROM CGNS FILE */
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
/* upper range index of vertices */
    irmax[0]=isize[0][0];
    irmax[1]=isize[0][1];
    irmax[2]=isize[0][2];
/* read grid coordinates */
    cg_coord_read(index_file,index_base,index_zone,"CoordinateX",
                  CGNS_ENUMV(RealSingle),irmin,irmax,x[0][0]);
    cg_coord_read(index_file,index_base,index_zone,"CoordinateY",
                  CGNS_ENUMV(RealSingle),irmin,irmax,y[0][0]);
    cg_coord_read(index_file,index_base,index_zone,"CoordinateZ",
                  CGNS_ENUMV(RealSingle),irmin,irmax,z[0][0]);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read grid from file grid_c.cgns\n");
    printf("  For example, zone 1 x,y,z[8][16][20]= %f, %f, %f\n", \
           x[8][16][20],y[8][16][20],z[8][16][20]);
    printf("\nProgram successful... ending now\n");
    return 0;
}
