/*    Program write_discreteface_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid, and adds discrete data (at K-DIR FACE CENTERS
PLUS RIND CELLS IN I AND J DIRECTIONS) to it.
In memory, rind cells also exist in the K direction, necessitating
use of cg_array_general_write.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_discreteface_str.c
cc -o write_discreteface_str_c write_discreteface_str.o -L ../../lib -lcgns

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
  dimension statements (note that there are no restrictions on
  tri-dimensional array f because because a general write interface
  is used).
  Rind cells are stored in array locations [0][j][i], [10][j][i],
  [k][0][i], [k][17][i], [k][j][0], [k][j][21]
*/
    double f[9+2][16+2][20+2];
    int ni,nj,nk,i,j,k,kk,index_file,index_base,index_zone,index_discrete;
    int irinddata[3][2];
    cgsize_t dims[3], rmin[3], rmax[3], m_dims[3], m_rmin[3], m_rmax[3];

    printf("\nProgram write_discreteface_str\n");

/* create fake discrete data AT I-DIR CELL FACES for simple example: */
    ni=20;
    nj=16;
    nk=9;
    for (k=0; k < nk; k++)
    {
      for (j=0; j < nj; j++)
      {
        for (i=0; i < ni; i++)
        {
          f[k+1][j+1][i+1]=(double)i;
        }
      }
    }
/* create rind cell data: */
    for (j=0; j < nj+2; j++)
    {
      for (i=0; i < ni+2; i++)
      {
        f[0   ][j][i]= 999.;
        f[nk+1][j][i]=-999.;
      }
    }
    for (k=0; k < nk+2; k++)
    {
      kk=k+1;
      for (j=0; j < nj+2; j++)
      {
        f[k][j][0   ]= 999.+(float)j+(5.*(float)kk);
        f[k][j][ni+1]=-999.-(float)j-(5.*(float)kk);
      }
    }
    for (k=0; k < nk+2; k++)
    {
      kk=k+1;
      for (i=0; i < ni+2; i++)
      {
        f[k][0   ][i]= 888.+(float)i+(5.*(float)kk);
        f[k][nj+1][i]=-888.-(float)i-(5.*(float)kk);
      }
    }
    printf("\ncreated simple 3-D flux with rind data\n");

/* WRITE DISCRETE DATA TO EXISTING CGNS FILE */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* create discrete data node */
    cg_discrete_write(index_file,index_base,index_zone,"DiscreteData",&index_discrete);
/* go to position within tree at DiscreteData_t node */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"DiscreteData_t",index_discrete,"end");
/* write data location under DiscreteData_t node */
    cg_gridlocation_write(CGNS_ENUMV(KFaceCenter));
/* write rind information under DiscreteData_t node (ilo,ihi,jlo,jhi,klo,khi) */
    irinddata[0][0]=1;
    irinddata[0][1]=1;
    irinddata[1][0]=1;
    irinddata[1][1]=1;
    irinddata[2][0]=0;  /* note that we are not writing k-direction rind */
    irinddata[2][1]=0;  /* planes */
    cg_rind_write(irinddata[0]);
/* write array data (use of cg_array_general_write allows for defining a
   hyperslab in memory that excludes the k-direction rind planes) */
/* dimensions of array in file */
    dims[0]   = ni+2;
    dims[1]   = nj+2;
    dims[2]   = nk;     /* no k-direction rind planes in file */
/* dimensions of array in memory */
    m_dims[0] = ni+2;
    m_dims[1] = nj+2;
    m_dims[2] = nk+2;
/* in file space the core cells ALWAYS start at index 1 */
/* lower range index in file */
    rmin[0] = 1-irinddata[0][0];
    rmin[1] = 1-irinddata[1][0];
    rmin[2] = 1-irinddata[2][0];
/* upper range index in file */
    rmax[0] = ni+irinddata[0][1];
    rmax[1] = nj+irinddata[1][1];
    rmax[2] = nk+irinddata[2][1];
/* in memory space, the lower bound ALWAYS starts at index 1 (in this example,
 * core cells start at 2) */
/* lower range index in memory */
    m_rmin[0] = 2-irinddata[0][0];
    m_rmin[1] = 2-irinddata[1][0];
    m_rmin[2] = 2-irinddata[2][0];
/* upper range index in memory */
    m_rmax[0] = 1+ni+irinddata[0][0];
    m_rmax[1] = 1+nj+irinddata[1][0];
    m_rmax[2] = 1+nk+irinddata[2][0];
    if (cg_array_general_write("K-Flux",
                               CGNS_ENUMV(RealDouble),
                               3,   dims,   rmin,   rmax,
                               CGNS_ENUMV(RealDouble),
                               3, m_dims, m_rmin, m_rmax, f))
      cg_error_exit();
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added discrete data to file grid_c.cgns\n");
    printf("\nNote:  if the original CGNS file already had a DiscreteData_t node,");
    printf("\n       it has been overwritten\n");
    return 0;
}
