/*   Program read_con2zn_genrl_str   */
/*
Opens an existing CGNS file that contains a simple 3-D
structured grid (2 zones) plus 1-to-1 connectivity
information (written in GENERAL form), and reads the
connectivity info.

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid2zn_str.c plus write_con2zn_genrl_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_con2zn_genrl_str.c
cc -o read_con2zn_genrl_str_c read_con2zn_genrl_str.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#endif

#define maxpnts 400

int main()
{
    int i,index_file,index_base,nzone,index_zone,nconns,index_conn;
    char donorname[33],connectname[33];
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(GridConnectivityType_t) iconnect_type;
    CGNS_ENUMT(PointSetType_t) iptset_type,idonor_ptset_type;
    CGNS_ENUMT(ZoneType_t) idonor_zonetype;
    CGNS_ENUMT(DataType_t) idonor_datatype;
    cgsize_t npts,ndata_donor;
    cgsize_t ipnts[maxpnts][3],ipntsdonor[maxpnts][3];

/* READ GENERAL CONNECTIVITY INFORMATION FROM EXISTING CGNS FILE */
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
/* find out how many general interfaces there are in this zone */
/* (for this program, there should only be one) */
      cg_nconns(index_file,index_base,index_zone,&nconns);
      if (nconns != 1)
      {
        printf("\nError.  Expecting one general interface. %i read\n",nconns);
        return 1;
      }
      index_conn=nconns;
/* read general connectivity info */
      cg_conn_info(index_file,index_base,index_zone,index_conn,connectname,&location,
                   &iconnect_type,&iptset_type,&npts,donorname,&idonor_zonetype,
                   &idonor_ptset_type,&idonor_datatype,&ndata_donor);
      if (npts > maxpnts)
      {
        printf("\nError.  Must increase maxpnts to at least %i\n",(int)npts);
        return 1;
      }
      cg_conn_read(index_file,index_base,index_zone,index_conn,ipnts[0],
                   idonor_datatype,ipntsdonor[0]);
      printf("\nIn zone %i:\n",index_zone);
      printf("   donor name=%s\n",donorname);
      printf("   number of connectivity pts=%i\n",(int)npts);
      printf("   grid location=%s\n",GridLocationName[location]);
      printf("   connectivity type=%s\n",GridConnectivityTypeName[iconnect_type]);
      printf("   pointset type=%s\n",PointSetTypeName[iptset_type]);
      printf("   donor zonetype=%s\n",ZoneTypeName[idonor_zonetype]);
      printf("   donor pointset type=%s\n",PointSetTypeName[idonor_ptset_type]);
/*    printf("   data type=%s\n",DataTypeName[idonor_datatype]); */
      printf("   ipnts and ipntsdonor arrays read, only some written out here:\n");
      for (i=0; i < 10; i++)
      {
        printf("    ipnts[%i][0], [%i][1], [%i][2]=%i,%i,%i",
               i,i,i,(int)ipnts[i][0],(int)ipnts[i][1],(int)ipnts[i][2]);
        printf("    ipntsdonor[%i][0], [%i][1], [%i][2]=%i,%i,%i\n",
               i,i,i,(int)ipntsdonor[i][0],(int)ipntsdonor[i][1],(int)ipntsdonor[i][2]);
      }
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read general 1-to-1 connectivity info from file grid_c.cgns\n");
    return 0;
}
