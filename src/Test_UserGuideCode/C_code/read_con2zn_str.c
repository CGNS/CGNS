/*   Program read_con2zn_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid (2 zones) plus 1-to-1 connectivity
information, and reads the connectivity info.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid2zn_str.c plus write_con2zn_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_con2zn_str.c
cc -o read_con2zn_str_c read_con2zn_str.o -L ../../lib -lcgns

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
    int itranfrm[3];
    int index_file,index_base,nzone,index_zone,n1to1,index_conn;
    char donorname[33],connectname[33];
    cgsize_t ipnts[2][3],ipntsdonor[2][3];

/* READ 1-TO-1 CONNECTIVITY INFORMATION FROM EXISTING CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* get number of zones (should be 2 for our case) */
    cg_nzones(index_file,index_base,&nzone);
    if (nzone != 2)
    {
      printf("\nError. This program expects 2 zones. %d read.\n",nzone);
      return 1;
    }
/* loop over zones */
    for (index_zone=1; index_zone <= nzone; ++index_zone)
    {
/* find out how many 1-to-1 interfaces there are in this zone */
/* (for this program, there should only be one) */
      cg_n1to1(index_file,index_base,index_zone,&n1to1);
      if (n1to1 != 1)
      {
        printf("\nError.  Expecting one 1-to-1 interface. %i read\n",n1to1);
        return 1;
      }
      index_conn=n1to1;
/* read 1-to-1 info */
      cg_1to1_read(index_file,index_base,index_zone,index_conn,connectname,donorname,
                   ipnts[0],ipntsdonor[0],itranfrm);
      printf("\nIn zone %i:\n",index_zone);
      printf("   donor name=%s\n",donorname);
      printf("   range (this zone)=%i,%i,%i,%i,%i,%i\n",(int)ipnts[0][0],
              (int)ipnts[0][1],(int)ipnts[0][2],(int)ipnts[1][0],
              (int)ipnts[1][1],(int)ipnts[1][2]);
      printf("   range (donor zone)=%i,%i,%i,%i,%i,%i\n",(int)ipntsdonor[0][0],
              (int)ipntsdonor[0][1],(int)ipntsdonor[0][2],(int)ipntsdonor[1][0],
              (int)ipntsdonor[1][1],(int)ipntsdonor[1][2]);
      printf("   transform=%i,%i,%i\n",itranfrm[0],itranfrm[1],itranfrm[2]);
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read 1-to-1 connectivity info from file grid_c.cgns\n");
    return 0;
}
