/*    Program write_floweqn_str   */
/*
Opens an existing CGNS file and writes flow eqn info

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.f)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_floweqn_str.c
cc -o write_floweqn_str_c write_floweqn_str.o -L ../../lib -lcgns

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
    int idata[6],index_file,index_base,index_zone,ieq_dim;
    float gamma,prandtl;
    cgsize_t nuse;

    printf("\nProgram write_floweqn_str\n");

/* data for writing */
    gamma=1.4;
    prandtl=0.90;
/* WRITE FLOW EQUATION SET INFO */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* existing file must be 3D structured (real working code would check!) */
/* Create 'FlowEquationSet' node under 'Zone_t' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"end");
/* equation dimension = 3 */
    ieq_dim=3;
    cg_equationset_write(ieq_dim);

/* Create 'GoverningEquations' node under 'FlowEquationSet' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,"end");
    cg_governing_write(CGNS_ENUMV(NSTurbulent));
/* Create 'DiffusionModel' node under 'GoverningEquations' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,
        "GoverningEquations_t",1,"end");
    idata[0]=0;
    idata[1]=1;
    idata[2]=0;
    idata[3]=0;
    idata[4]=0;
    idata[5]=0;
    cg_diffusion_write(idata);

    nuse=1;
/* Create 'GasModel' under 'FlowEquationSet' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,"end");
    cg_model_write("GasModel_t",CGNS_ENUMV(Ideal));
/* Create 'SpecificHeatRatio' under GasModel */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,
        "GasModel_t",1,"end");
    cg_array_write("SpecificHeatRatio",CGNS_ENUMV(RealSingle),1,&nuse,&gamma);
/* Create 'DataClass' under 'SpecificHeatRatio' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,
        "GasModel_t",1,"DataArray_t",1,"end");
    cg_dataclass_write(CGNS_ENUMV(NondimensionalParameter));

/* Create 'TurbulenceClosure' under 'FlowEquationSet' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,"end");
    cg_model_write("TurbulenceClosure_t",CGNS_ENUMV(EddyViscosity));
/* Create 'PrandtlTurbulent' under 'TurbulenceClosure' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,
        "TurbulenceClosure_t",1,"end");
    cg_array_write("PrandtlTurbulent",CGNS_ENUMV(RealSingle),1,&nuse,&prandtl);
/* Create 'DataClass' under 'PrandtlTurbulent' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,
        "TurbulenceClosure_t",1,"DataArray_t",1,"end");
    cg_dataclass_write(CGNS_ENUMV(NondimensionalParameter));

/* Create 'TurbulenceModel' under 'FlowEquationSet' */
    cg_goto(index_file,index_base,"Zone_t",index_zone,"FlowEquationSet_t",1,"end");
    cg_model_write("TurbulenceModel_t",CGNS_ENUMV(OneEquation_SpalartAllmaras));
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully wrote equation set info to file grid_c.cgns\n");
    return 0;
}
