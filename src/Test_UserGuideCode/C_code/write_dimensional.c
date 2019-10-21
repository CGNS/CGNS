/*    Program write_dimensional   */
/*
Opens an existing CGNS file that contains a simple 3-D
grid plus a flow solution and adds its dimensionality
(dimensional data).

The CGNS grid file 'grid_c.cgns' must already exist
(for example, created using
    write_grid_str.c followed by write_flowcent_str.c or
    write_grid_str.c followed by write_flowvert_str.c or
    write_grid_str.c followed by write_flowcentrind_str.c or
    write_grid_unst.c followed by write_flowvert_unst.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_dimensional.c
cc -o write_dimensional_c write_dimensional.o -L ../../lib -lcgns

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
    float exponents[5];
    int index_file,index_base,index_zone,index_flow;
    int nfields,iff,icc,ncoords;
    char fieldname[33];
    CGNS_ENUMT(DataType_t) idatatype;

    printf("\nProgram write_dimensional\n");

/* WRITE DIMENSIONAL INFO FOR GRID AND FLOW SOLN */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* we know there is only one FlowSolution_t (real working code would check!) */
    index_flow=1;
/* put DataClass and DimensionalUnits under Base */
    cg_goto(index_file,index_base,"end");
    cg_dataclass_write(CGNS_ENUMV(Dimensional));
    cg_units_write(CGNS_ENUMV(Kilogram),CGNS_ENUMV(Meter),CGNS_ENUMV(Second),CGNS_ENUMV(Kelvin),CGNS_ENUMV(Degree));
/* read fields */
    cg_nfields(index_file,index_base,index_zone,index_flow,&nfields);
    if (nfields != 2)
    {
      printf("\nError! expecting 2 fields, read %d\n",nfields);
      return 1;
    }
    for (iff=1; iff <= nfields; iff++)
    {
      cg_field_info(index_file,index_base,index_zone,index_flow,iff,&idatatype,fieldname);
      printf("fieldname=%s\n",fieldname);
      if (strcmp(fieldname,"Density") == 0)
      {
        exponents[0]=1.;
        exponents[1]=-3.;
        exponents[2]=0.;
        exponents[3]=0.;
        exponents[4]=0.;
      }
      else if (strcmp(fieldname,"Pressure") == 0)
      {
        exponents[0]=1.;
        exponents[1]=-1.;
        exponents[2]=-2.;
        exponents[3]=0.;
        exponents[4]=0.;
      }
      else
      {
        printf("\nError! this fieldname not expected: %s\n",fieldname);
        return 1;
      }
/* write DimensionalExponents */
      cg_goto(index_file,index_base,"Zone_t",1,"FlowSolution_t",1,"DataArray_t",iff,"end");
      cg_exponents_write(CGNS_ENUMV(RealSingle),exponents);
    }
/* read grid */
    cg_ncoords(index_file,index_base,index_zone,&ncoords);
    exponents[0]=0.;
    exponents[1]=1.;
    exponents[2]=0.;
    exponents[3]=0.;
    exponents[4]=0.;
    for (icc=1; icc <= ncoords; icc++)
    {
/* write DimensionalExponents */
      cg_goto(index_file,index_base,"Zone_t",1,"GridCoordinates_t",1,"DataArray_t",icc,"end");
      cg_exponents_write(CGNS_ENUMV(RealSingle),exponents);
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully wrote dimensional data to file grid_c.cgns\n");
    return 0;
}
