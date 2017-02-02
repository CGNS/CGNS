/*   Program read_floweqn_str   */
/*
Opens an existing CGNS file and reads flow equation info

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c followed by write_floweqn_str.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_floweqn_str.c
cc -o read_floweqn_str_c read_floweqn_str.o -L ../../lib -lcgns

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
    int idata[6];
    int index_file,index_base,index_zone;
    int id,ige,igm,ivm,itcm,itc,itm;
    float gamma,prandtl;
    CGNS_ENUMT(GoverningEquationsType_t) itype;
    CGNS_ENUMT(ModelType_t) mtype;

/* READ FLOW EQUATION SET INFO */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* existing file must be 3D structured (real working code would check!) */
/* Read info from 'FlowEquationSet' node under 'Zone_t' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"end");
    if (cg_equationset_read(&id,&ige,&igm,&ivm,&itcm,&itc,&itm))
    {
      printf("\nError!  FlowEquationSet node does not exist.\n");
      return 1;
    }
    printf("\nEqn dimension = %i\n",id);
/* Read 'GoverningEquations' node */
    if (ige == 1)
    {
      cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,"end");
      cg_governing_read(&itype);
      printf(" Gov eqn = %s\n",GoverningEquationsTypeName[itype]);
/* Read 'DiffusionModel' node */
      cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1, \
              "GoverningEquations_t",1,"end");
      cg_diffusion_read(idata);
      printf("     diffusion= %i, %i, %i, %i, %i, %i\n",idata[0],idata[1], \
             idata[2],idata[3],idata[4],idata[5]);
    }
/* Read gas model */
    if (igm == 1)
    {
      cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,"end");
      cg_model_read("GasModel_t",&mtype);
      printf(" Gas model type = %s\n",ModelTypeName[mtype]);
      cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1, \
              "GasModel_t",1,"end");
      cg_array_read_as(1,CGNS_ENUMV(RealSingle),&gamma);
      printf("     gamma = %f\n",gamma);
    }
/* Read turbulence closure */
    if (itc == 1)
    {
      cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,"end");
      cg_model_read("TurbulenceClosure_t",&mtype);
      printf(" Turbulence closure type = %s\n",ModelTypeName[mtype]);
      cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1, \
              "TurbulenceClosure_t",1,"end");
      cg_array_read_as(1,CGNS_ENUMV(RealSingle),&prandtl);
      printf("     turb prandtl number = %f\n",prandtl);
    }
    if (itm == 1)
    {
      cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,"end");
      cg_model_read("TurbulenceModel_t",&mtype);
      printf(" Turbulence model type = %s\n",ModelTypeName[mtype]);
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read equation set info from file grid_c.cgns\n");
    return 0;
}
