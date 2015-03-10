/*   Program write_nondimensional   */
/*
Opens an existing CGNS file and adds the DataClass and
ReferenceState appropriate for a completely
NONDIMENSIONAL data set.

The CGNS grid file 'grid_c.cgns' must already exist
(for example, created using write_grid_str.c or
write_grid_unst.c).  In this case, the flow solution does
not need to be present.

Example compilation for this program is (change paths!):

cc -I ../CGNS_CVS/cgnslib_2.5 -c write_nondimensional.c
cc -o write_nondimensional_c write_nondimensional.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns

(../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
library libcgns.a is located)
*/
 
#include <stdio.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#else
# if CG_BUILD_SCOPE
#  error enumeration scoping needs to be off
# endif
#endif

int main()
{
    double xmach,reue,xmv,xmc,rev,rel,renu,rho0;
    double p0,c0,vm0,xlength0,vx,vy,vz;
    double gamma;
    int index_file,index_base;
    DataClass_t idata;
    cgsize_t nuse;

/* define nondimensional parameters */
    xmach=4.6;
    reue=6000000.;
    xmv=xmach;
    xmc=1.;
    rev=xmach;
    rel=1.;
    renu=xmach/reue;
    rho0=1.;
    gamma=1.4;
    p0=1./gamma;
    c0=1.;
    vm0=xmach/reue;
    xlength0=1.;
    vx=xmach;
    vy=0.;
    vz=0.;
    nuse=1;
/* WRITE NONDIMENSIONAL INFO */
/* open CGNS file for modify */
    if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* put DataClass under Base */
    cg_goto(index_file,index_base,"end");
/* check first if a dataclass has already been written */
    if (CG_OK == cg_dataclass_read(&idata))
    {
      printf("\nError! DataClass already exists!\n");
      printf("  Re-make CGNS file without it and related info, then try again\n");
      return 1;
    }
    cg_dataclass_write(NormalizedByUnknownDimensional);
/* put ReferenceState under Base */
    cg_state_write("ReferenceQuantities");
/* Go to ReferenceState node, write Mach array and its dataclass */
    cg_goto(index_file,index_base,"ReferenceState_t",1,"end");
    cg_array_write("Mach",RealDouble,1,&nuse,&xmach);
    cg_goto(index_file,index_base,"ReferenceState_t",1,"DataArray_t",1,"end");
    cg_dataclass_write(NondimensionalParameter);
/* Go to ReferenceState node, write Reynolds array and its dataclass */
    cg_goto(index_file,index_base,"ReferenceState_t",1,"end");
    cg_array_write("Reynolds",RealDouble,1,&nuse,&reue);
    cg_goto(index_file,index_base,"ReferenceState_t",1,"DataArray_t",2,"end");
    cg_dataclass_write(NondimensionalParameter);
/* Go to ReferenceState node to write reference quantities: */
    cg_goto(index_file,index_base,"ReferenceState_t",1,"end");
/* First, write reference quantities that make up Mach and Reynolds: */
/* Mach_Velocity */
    cg_array_write("Mach_Velocity",RealDouble,1,&nuse,&xmv);
/* Mach_VelocitySound */
    cg_array_write("Mach_VelocitySound",RealDouble,1,&nuse,&xmc);
/* Reynolds_Velocity */
    cg_array_write("Reynolds_Velocity",RealDouble,1,&nuse,&rev);
/* Reynolds_Length */
    cg_array_write("Reynolds_Length",RealDouble,1,&nuse,&rel);
/* Reynolds_ViscosityKinematic */
    cg_array_write("Reynolds_ViscosityKinematic",RealDouble,1,&nuse,&renu);

/* Next, write flow field reference quantities: */
/* Density */
    cg_array_write("Density",RealDouble,1,&nuse,&rho0);
/* Pressure */
    cg_array_write("Pressure",RealDouble,1,&nuse,&p0);
/* VelocitySound */
    cg_array_write("VelocitySound",RealDouble,1,&nuse,&c0);
/* ViscosityMolecular */
    cg_array_write("ViscosityMolecular",RealDouble,1,&nuse,&vm0);
/* LengthReference */
    cg_array_write("LengthReference",RealDouble,1,&nuse,&xlength0);
/* VelocityX */
    cg_array_write("VelocityX",RealDouble,1,&nuse,&vx);
/* VelocityY */
    cg_array_write("VelocityY",RealDouble,1,&nuse,&vy);
/* VelocityZ */
    cg_array_write("VelocityZ",RealDouble,1,&nuse,&vz);
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully wrote nondimensional info to file grid_c.cgns\n");
    return 0;
}
