/*   Program write_con2zn_genrl_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid (2 zones), and adds 1-to-1
connectivity information to it (using GENERAL
method, as opposed to specific 1-to-1 method).

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid2zn_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_con2zn_genrl_str.c
cc -o write_con2zn_genrl_str_c write_con2zn_genrl_str.o -L ../../lib -lcgns

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

#define maxcount 400

int main()
{
    int ilo[2],ihi[2],jlo[2],jhi[2],klo[2],khi[2];
    int icount,j,k;
    int index_file,index_base,nzone,index_zone,nconns,n1to1,index_conn,iz;
    char donorname[33],zonename0[33],zonename1[33],zn[33];
    cgsize_t isize[3][3],ipnts[maxcount][3],ipntsdonor[maxcount][3];
    cgsize_t icounts;

    printf("\nProgram write_con2zn_genrl_str\n");

/* WRITE GENERAL CONNECTIVITY INFORMATION TO EXISTING CGNS FILE */
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
/* set up point lists */
      if (index_zone == 0)
      {
        strcpy(donorname,zonename1);
        icount=0;
        for (j=jlo[index_zone]; j <= jhi[index_zone]; j++)
        {
          for (k=klo[index_zone]; k <= khi[index_zone]; k++)
          {
            ipnts[icount][0]=ihi[0];
            ipnts[icount][1]=j;
            ipnts[icount][2]=k;
            ipntsdonor[icount][0]=ilo[1];
            ipntsdonor[icount][1]=j;
            ipntsdonor[icount][2]=k;
            icount=icount+1;
          }
        }
        if (icount > maxcount)
        {
          printf("\nError. Need to increase maxcount to at least %i\n",icount);
          return 1;
        }
      }
      else
      {
        strcpy(donorname,zonename0);
        icount=0;
        for (j=jlo[index_zone]; j <= jhi[index_zone]; j++)
        {
          for (k=klo[index_zone]; k <= khi[index_zone]; k++)
          {
            ipnts[icount][0]=ilo[1];
            ipnts[icount][1]=j;
            ipnts[icount][2]=k;
            ipntsdonor[icount][0]=ihi[0];
            ipntsdonor[icount][1]=j;
            ipntsdonor[icount][2]=k;
            icount=icount+1;
          }
        }
        if (icount > maxcount)
        {
          printf("\nError. Need to increase maxcount to at least %i\n",icount);
          return 1;
        }
      }
/* write integer connectivity info (user can give any name) */
      icounts=icount;
      cg_conn_write(index_file,index_base,iz,"GenInterface",CGNS_ENUMV(Vertex),CGNS_ENUMV(Abutting1to1),
          CGNS_ENUMV(PointList),icounts,ipnts[0],donorname,CGNS_ENUMV(Structured),
          CGNS_ENUMV(PointListDonor),CGNS_ENUMV(Integer),icounts,ipntsdonor[0],&index_conn);
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully added 1-to-1 connectivity info to file grid_c.cgns (using GENERAL method)\n");
    return 0;
}
