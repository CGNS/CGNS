/*   Program read_discreteface_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid plus discrete data (at K-DIR FACE CENTERS PLUS
RIND CELLS IN THE I AND J DIRECTIONS), and reads it.
In memory, rind cells also exist in the K direction, necessitating
use of cg_array_general_read.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c followed by
write_discreteface_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_discreteface_str.c
cc -o read_discreteface_str_c read_discreteface_str.o -L ../../lib -lcgns

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
  dimension statements (note that there are no restrictions on
  tri-dimensional array f because because a general write interface
  is used).
  In the file, rind cells are stored in array locations [k][0][i],
  [k][17][i], [k][j][0], [k][j][21]
  In memory, rind cells are stored in array locations [0][j][i],
  [10][j][i], [k][0][i], [k][17][i], [k][j][0], [k][j][21]
*/
    float f[9+2][16+2][20+2];
    int irinddata[3][2];
    int n,index_file,index_base,index_zone,index_discrete;
    char zonename[33];
    cgsize_t isize[3][3],irmin[3],irmax[3],m_dims[3],m_rmin[3],m_rmax[3];

/* clear f since only partial read */
    for (n=0; n<(9+2)*(16+2)*(20+2); n++)
    {
      (&f[0][0][0])[n] = -0.123456f;
    }

/* READ FLOW SOLUTION FROM CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* we know there is only one DiscreteData_t (real working code would check!) */
    index_discrete=1;
/* get zone size (and name - although not needed here) */
    cg_zone_read(index_file,index_base,index_zone,zonename,isize[0]);
/* go to position within tree at DiscreteData_t node */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"DiscreteData_t",index_discrete,"end");
/* read rind data */
    cg_rind_read(irinddata[0]);
/* dimensions of array in memory */
    m_dims[0] = 20+2;
    m_dims[1] = 16+2;
    m_dims[2] =  9+2;
/* in file space the core cells ALWAYS start at index 1 */
/* lower range index in file */
    irmin[0]=1-irinddata[0][0];
    irmin[1]=1-irinddata[1][0];
    irmin[2]=1-irinddata[2][0];
/* upper range index in file - use vertex dimensions for k */
    irmax[0]=isize[1][0]+irinddata[0][1];
    irmax[1]=isize[1][1]+irinddata[1][1];
    irmax[2]=isize[0][2]+irinddata[2][1];
/* in memory space, the lower bound ALWAYS starts at index 1 (in this example,
 * core cells start at 2) */
/* lower range index in memory */
    m_rmin[0] = 2-irinddata[0][0];
    m_rmin[1] = 2-irinddata[1][0];
    m_rmin[2] = 2-irinddata[2][0];
/* upper range index in memory */
    m_rmax[0] = 1+20+irinddata[0][1];
    m_rmax[1] = 1+16+irinddata[1][1];
    m_rmax[2] = 1+ 9+irinddata[2][1];
/* read discrete data */
    if (cg_array_general_read(1, irmin, irmax, CGNS_ENUMV(RealSingle),
                              3, m_dims, m_rmin, m_rmax, f[0][0]))
      cg_error_exit();
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read discrete data from file grid_c.cgns\n");
    printf("  For example, f[ 9][16][20]= %f\n",f[ 9][16][20]);
    printf("         rind: f[ 9][16][ 0]= %f\n",f[ 9][16][ 0]);
    printf("         rind: f[ 9][16][21]= %f\n",f[ 9][16][21]);
    printf("   unmodified: f[10][16][ 0]= %f\n",f[10][16][ 0]);
    printf("\nProgram successful... ending now\n");
    return 0;
}
