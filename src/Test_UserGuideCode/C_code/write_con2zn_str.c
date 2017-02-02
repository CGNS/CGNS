/*   Program write_con2zn_str.c    */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid (2 zones), and adds 1-to-1
connectivity information to it.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid2zn_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_con2zn_str.c
cc -o write_con2zn_str_c write_con2zn_str.o -L ../../lib -lcgns

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
    int ilo[2],ihi[2],jlo[2],jhi[2],klo[2],khi[2];
    int itranfrm[3];
    int index_file,index_base,nzone,index_zone,nconns,n1to1,index_conn,iz;
    char donorname[33],zonename0[33],zonename1[33],zn[33];
    cgsize_t isize[3][3],ipnts[2][3],ipntsdonor[2][3];

    printf("\nProgram write_con2zn_str\n");

/* WRITE 1-TO-1 CONNECTIVITY INFORMATION TO EXISTING CGNS FILE */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* get number of zones (should be 2 for our case) */
    cg_nzones(index_file,index_base,&nzone);
    if (nzone != 2)
    {
      printf("\nError. This program expects 2 zones. %d read.\n",nzone);
      return 1;
    }

/* loop over zones to get zone sizes and names */
    for (index_zone=0; index_zone < nzone; ++index_zone)
    {
      iz=index_zone+1;
      cg_zone_read(index_file,index_base,iz,zn,isize[0]);
      if (index_zone == 0)
      {
        strcpy(zonename0,zn);
      }
      else
      {
        strcpy(zonename1,zn);
      }
      ilo[index_zone]=1;
      ihi[index_zone]=isize[0][0];
      jlo[index_zone]=1;
      jhi[index_zone]=isize[0][1];
      klo[index_zone]=1;
      khi[index_zone]=isize[0][2];
    }
/* loop over zones again */
    for (index_zone=0; index_zone < nzone; ++index_zone)
    {
      iz=index_zone+1;
/* for this program, there should be no existing connectivity info: */
      cg_nconns(index_file,index_base,iz,&nconns);
      if (nconns != 0)
      {
        printf("\nError. This program expects no interfaces yet. %d read.\n",nconns);
        return 1;
      }
      cg_n1to1(index_file,index_base,iz,&n1to1);
      if (n1to1 != 0)
      {
        printf("\nError. This program expects no interfaces yet. %d read.\n",n1to1);
        return 1;
      }
/* set up index ranges */
      if (index_zone == 0)
      {
        strcpy(donorname,zonename1);
/* lower point of receiver range */
        ipnts[0][0]=ihi[0];
        ipnts[0][1]=jlo[0];
        ipnts[0][2]=klo[0];
/* upper point of receiver range */
        ipnts[1][0]=ihi[0];
        ipnts[1][1]=jhi[0];
        ipnts[1][2]=khi[0];
/* lower point of donor range */
        ipntsdonor[0][0]=ilo[1];
        ipntsdonor[0][1]=jlo[1];
        ipntsdonor[0][2]=klo[1];
/* upper point of donor range */
        ipntsdonor[1][0]=ilo[1];
        ipntsdonor[1][1]=jhi[1];
        ipntsdonor[1][2]=khi[1];
      }
      else
      {
        strcpy(donorname,zonename0);
/* lower point of receiver range */
        ipnts[0][0]=ilo[1];
        ipnts[0][1]=jlo[1];
        ipnts[0][2]=klo[1];
/* upper point of receiver range */
        ipnts[1][0]=ilo[1];
        ipnts[1][1]=jhi[1];
        ipnts[1][2]=khi[1];
/* lower point of donor range */
        ipntsdonor[0][0]=ihi[0];
        ipntsdonor[0][1]=jlo[0];
        ipntsdonor[0][2]=klo[0];
/* upper point of donor range */
        ipntsdonor[1][0]=ihi[0];
        ipntsdonor[1][1]=jhi[0];
        ipntsdonor[1][2]=khi[0];
      }

/* set up Transform */
      itranfrm[0]=1;
      itranfrm[1]=2;
      itranfrm[2]=3;
/* write 1-to-1 info (user can give any name) */
      cg_1to1_write(index_file,index_base,iz,"Interface",donorname,
          ipnts[0],ipntsdonor[0],itranfrm,&index_conn);
    }

/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added 1-to-1 connectivity info to file grid_c.cgns\n");
    return 0;
}
