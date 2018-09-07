/*   Program write_bc_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid, and adds BC definitions (defined
over a range of points = PointRange)

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c).  Note: whether the
existing CGNS file has a flow solution in it already or
not is irrelevant.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_bc_str.c
cc -o write_bc_str_c write_bc_str.o -L ../../lib -lcgns

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
    cgsize_t isize[3][3],ipnts[2][3];
    int index_file,index_base,index_zone,index_bc;
    int ilo,ihi,jlo,jhi,klo,khi;
    char zonename[33];

    printf("\nProgram write_bc_str\n");

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
/* write boundary conditions for ilo face, defining range first */
/* (user can give any name) */
/* lower point of range */
    ipnts[0][0]=ilo;
    ipnts[0][1]=jlo;
    ipnts[0][2]=klo;
/* upper point of range */
    ipnts[1][0]=ilo;
    ipnts[1][1]=jhi;
    ipnts[1][2]=khi;
    cg_boco_write(index_file,index_base,index_zone,"Ilo",CGNS_ENUMV(BCTunnelInflow),
        CGNS_ENUMV(PointRange),2,ipnts[0],&index_bc);
/* write boundary conditions for ihi face, defining range first */
/* (user can give any name) */
/* lower point of range */
    ipnts[0][0]=ihi;
    ipnts[0][1]=jlo;
    ipnts[0][2]=klo;
/* upper point of range */
    ipnts[1][0]=ihi;
    ipnts[1][1]=jhi;
    ipnts[1][2]=khi;
    cg_boco_write(index_file,index_base,index_zone,"Ihi",CGNS_ENUMV(BCExtrapolate),
        CGNS_ENUMV(PointRange),2,ipnts[0],&index_bc);
/* write boundary conditions for jlo face, defining range first */
/* (user can give any name) */
/* lower point of range */
    ipnts[0][0]=ilo;
    ipnts[0][1]=jlo;
    ipnts[0][2]=klo;
/* upper point of range */
    ipnts[1][0]=ihi;
    ipnts[1][1]=jlo;
    ipnts[1][2]=khi;
    cg_boco_write(index_file,index_base,index_zone,"Jlo",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointRange),2,ipnts[0],&index_bc);
/* write boundary conditions for jhi face, defining range first */
/* (user can give any name) */
/* lower point of range */
    ipnts[0][0]=ilo;
    ipnts[0][1]=jhi;
    ipnts[0][2]=klo;
/* upper point of range */
    ipnts[1][0]=ihi;
    ipnts[1][1]=jhi;
    ipnts[1][2]=khi;
    cg_boco_write(index_file,index_base,index_zone,"Jhi",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointRange),2,ipnts[0],&index_bc);
/* write boundary conditions for klo face, defining range first */
/* (user can give any name) */
/* lower point of range */
    ipnts[0][0]=ilo;
    ipnts[0][1]=jlo;
    ipnts[0][2]=klo;
/* upper point of range */
    ipnts[1][0]=ihi;
    ipnts[1][1]=jhi;
    ipnts[1][2]=klo;
    cg_boco_write(index_file,index_base,index_zone,"Klo",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointRange),2,ipnts[0],&index_bc);
/* write boundary conditions for khi face, defining range first */
/* (user can give any name) */
/* lower point of range */
    ipnts[0][0]=ilo;
    ipnts[0][1]=jlo;
    ipnts[0][2]=khi;
/* upper point of range */
    ipnts[1][0]=ihi;
    ipnts[1][1]=jhi;
    ipnts[1][2]=khi;
    cg_boco_write(index_file,index_base,index_zone,"Khi",CGNS_ENUMV(BCWallInviscid),
        CGNS_ENUMV(PointRange),2,ipnts[0],&index_bc);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added BCs (PointRange) to file grid_c.cgns\n");
    return 0;
}
