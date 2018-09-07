/*   Program write_bcpnts_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid, and adds BC definitions (defined
as individual points = PointList)

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c).  Note: whether the
existing CGNS file has a flow solution in it already or
not is irrelevant.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_bcpnts_str.c
cc -o write_bcpnts_str_c write_bcpnts_str.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#endif

#define maxcount 400

int main()
{
    cgsize_t isize[3][3],ipnts[maxcount][3],icounts;
    int index_file,index_base,index_zone,index_bc;
    int ilo,ihi,jlo,jhi,klo,khi;
    int icount,i,j,k;
    char zonename[33];

    printf("\nProgram write_bcpnts_str\n");

/* WRITE BOUNDARY CONDITIONS TO EXISTING CGNS FILE */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* get zone size (and name - although not needed here) */
    cg_zone_read(index_file,index_base,index_zone,zonename,isize[0]);
    printf("zonename=%s\n",zonename);
    ilo=1;
    ihi=isize[0][0];
    jlo=1;
    jhi=isize[0][1];
    klo=1;
    khi=isize[0][2];
/* write boundary conditions for ilo face, defining pointlist first */
/* (user can give any name) */
    icount=0;
    for (j=jlo; j <= jhi; j++)
    {
      for (k=klo; k <= khi; k++)
      {
        ipnts[icount][0]=ilo;
        ipnts[icount][1]=j;
        ipnts[icount][2]=k;
        icount=icount+1;
      }
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Ilo",CGNS_ENUMV(BCTunnelInflow),
        CGNS_ENUMV(PointList),icounts,ipnts[0],&index_bc);
/* write boundary conditions for ihi face, defining pointlist first */
/* (user can give any name) */
    icount=0;
    for (j=jlo; j <= jhi; j++)
    {
      for (k=klo; k <= khi; k++)
      {
        ipnts[icount][0]=ihi;
        ipnts[icount][1]=j;
        ipnts[icount][2]=k;
        icount=icount+1;
      }
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Ihi",CGNS_ENUMV(BCExtrapolate),
        CGNS_ENUMV(PointList),icounts,ipnts[0],&index_bc);
/* write boundary conditions for jlo face, defining pointlist first */
/* (user can give any name) */
    icount=0;
    for (i=ilo; i <= ihi; i++)
    {
      for (k=klo; k <= khi; k++)
      {
        ipnts[icount][0]=i;
        ipnts[icount][1]=jlo;
        ipnts[icount][2]=k;
        icount=icount+1;
      }
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Jlo",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointList),icounts,ipnts[0],&index_bc);
/* write boundary conditions for jhi face, defining pointlist first */
/* (user can give any name) */
    icount=0;
    for (i=ilo; i <= ihi; i++)
    {
      for (k=klo; k <= khi; k++)
      {
        ipnts[icount][0]=i;
        ipnts[icount][1]=jhi;
        ipnts[icount][2]=k;
        icount=icount+1;
      }
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Jhi",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointList),icounts,ipnts[0],&index_bc);
/* write boundary conditions for klo face, defining pointlist first */
/* (user can give any name) */
    icount=0;
    for (i=ilo; i <= ihi; i++)
    {
      for (j=jlo; j <= jhi; j++)
      {
        ipnts[icount][0]=i;
        ipnts[icount][1]=j;
        ipnts[icount][2]=klo;
        icount=icount+1;
      }
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Klo",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointList),icounts,ipnts[0],&index_bc);
/* write boundary conditions for khi face, defining pointlist first */
/* (user can give any name) */
    icount=0;
    for (i=ilo; i <= ihi; i++)
    {
      for (j=jlo; j <= jhi; j++)
      {
        ipnts[icount][0]=i;
        ipnts[icount][1]=j;
        ipnts[icount][2]=khi;
        icount=icount+1;
      }
    }
    if (icount > maxcount)
    {
      printf("\nError. Need to increase maxcount to at least %i\n",icount);
      return 1;
    }
    icounts=icount;
    cg_boco_write(index_file,index_base,index_zone,"Khi",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointList),icounts,ipnts[0],&index_bc);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added BCs (PointList) to file grid_c.cgns\n");
    return 0;
}
