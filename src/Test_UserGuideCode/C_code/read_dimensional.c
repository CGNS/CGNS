/*   Program read_dimensional  */
/*
Opens an existing CGNS file that contains a simple 3-D
grid plus a flow solution WITH DIMENSIONALITY, and reads
the dimensionality.

The CGNS grid file 'grid_c.cgns' must already exist
(for example, created using
    write_grid_str.f followed by write_flowcent_str.f or
    write_grid_str.f followed by write_flowvert_str.f or
    write_grid_str.f followed by write_flowcentrind_str.f or
    write_grid_unst.f followed by write_flowvert_unst.f
followed by write_dimensional.f)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_dimensional.c
cc -o read_dimensional_c read_dimensional.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
#include <string.h>
/* cgnslib_f.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#endif

int main()
{
    int index_file,index_base,index_zone,index_flow,nfields,ncoords;
    int iff,ic;
    float exponents[5];
    char fieldname[33],coordname[33];
    CGNS_ENUMT(DataClass_t) id;
    CGNS_ENUMT(MassUnits_t) im;
    CGNS_ENUMT(LengthUnits_t) il;
    CGNS_ENUMT(TimeUnits_t) it;
    CGNS_ENUMT(TemperatureUnits_t) ix;
    CGNS_ENUMT(AngleUnits_t) ia;
    CGNS_ENUMT(DataType_t) idatatype;

/* READ DIMENSIONAL INFO FOR GRID AND FLOW SOLN */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* we know there is only one FlowSolution_t (real working code would check!) */
    index_flow=1;
/* read DataClass and DimensionalUnits under Base */
    cg_goto(index_file,index_base,"end");
    cg_dataclass_read(&id);
    printf("\nDataClass = %s\n",DataClassName[id]);
    if (strcmp(DataClassName[id],"Dimensional") != 0)
    {
      printf("\nError!  Expecting Dimensional\n");
      return 1;
    }
    cg_units_read(&im,&il,&it,&ix,&ia);
    printf("\nUnits=\n    %s\n    %s\n    %s\n    %s\n    %s\n",
           MassUnitsName[im],LengthUnitsName[il],TimeUnitsName[it],
           TemperatureUnitsName[ix],AngleUnitsName[ia]);
/* read fields */
    cg_nfields(index_file,index_base,index_zone,index_flow,&nfields);
    if (nfields != 2)
    {
      printf("\nError! expecting 2 fields, read %i\n",nfields);
      return 1;
    }
    for (iff=1; iff <= nfields; iff++)
    {
/* read DimensionalExponents */
      cg_goto(index_file,index_base,"Zone_t",1,"FlowSolution_t",1,"DataArray_t",iff,"end");
      cg_exponents_read(exponents);
/* get field name */
      cg_field_info(index_file,index_base,index_zone,index_flow,iff,&idatatype,fieldname);
      printf("\nFor %s, exponents are:\n    %f\n    %f\n    %f\n    %f\n    %f\n",
             fieldname,exponents[0],exponents[1],exponents[2],exponents[3],exponents[4]);
    }
/* read grid */
    cg_ncoords(index_file,index_base,index_zone,&ncoords);
    for (ic=1; ic <=ncoords; ic++)
    {
/* read DimensionalExponents */
      cg_goto(index_file,index_base,"Zone_t",1,"GridCoordinates_t",1,
              "DataArray_t",ic,"end");
      cg_exponents_read(&exponents);
/* get coord name */
      cg_coord_info(index_file,index_base,index_zone,ic,&idatatype,coordname);
      printf("\nFor %s, exponents are:\n    %f\n    %f\n    %f\n    %f\n    %f\n",
             coordname,exponents[0],exponents[1],exponents[2],exponents[3],exponents[4]);
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read dimensional data from file grid_c.cgns\n");
    return 0;
}
