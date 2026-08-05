/*    Program write_flowcell_ho_unst   */
/*
Opens an existing CGNS file that contains a simple 3-D
unstructured grid, and adds a flow solution (at VERTICES)
to it.

The CGNS grid file 'grid_ho_c.cgns' must already exist
(created using write_grid_ho_unst.c)

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_flowcell_ho_unst.c
cc -o write_flowcell_ho_unst write_flowcell_ho_unst.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

double *createField(size_t nbelements);

void solutionLagrangeBasis(double **u, double **v);

void solutionCtrlPoints(int i, double *x, double *y);

int main()
{
    double *rho,*pres;
    double *pu,*pv;
    int ni,nj,index_file,index_base,index_zone,index_flow,index_field,index_family;
    int index_interp;
    char solname[33];
    
    /* NbCells per direction */
    ni = 4;
    nj = 5;
    
    printf("\nProgram write_flowcell_ho_unst\n");
    printf("\ncreated simple 3-D rho and p flow solution\n");

/* WRITE FLOW SOLUTION TO EXISTING CGNS FILE */
/* open CGNS file for modify */
    if (cg_open("grid_ho_c.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* we know there is only one family (real working code would check!) */
    index_family=1;
/* define flow solution node name (user can give any name) */
    snprintf(solname, sizeof(solname), "%s", "FlowSolution");
/* create flow solution node */
    if (cg_sol_write(index_file,index_base,index_zone,solname,CGNS_ENUMV(InterpolationPoints),&index_flow)) cg_error_exit();
/* add Corresponding spatial and temporal order --> 3rd and 0th order */
    if (cg_sol_interpolation_degree_write(index_file,index_base,index_zone,index_flow,3,0)) cg_error_exit();
/* write Lagrange Basis in Family.
 *
 * ORDER MATTERS: this must precede the cg_field_write calls below.  The length
 * of a GridLocation=InterpolationPoints field array is sum_e N_DOFs(e), and
 * N_DOFs(e) is defined by the SolutionInterpolation_t that matches element e
 * (reached through the zone's FamilyName_t, written by write_grid_ho_unst.c).
 * If the basis does not exist yet, the field length is undefined and
 * cg_field_write fails rather than assuming a cardinality. */
    if (cg_solution_interpolation_write(index_file,index_base,index_family,"3rdOrderQUADsolution",
                                        CGNS_ENUMV(QUAD_4),3,0,CGNS_ENUMV(ParametricLagrange),&index_interp)) cg_error_exit();
/* get Lagrange Basis for 3rd order QUAD solution */
    solutionLagrangeBasis(&pu,&pv);
/* write Lagrange Control Points */
    if (cg_solution_interpolation_points_write(index_file,index_base,index_family,
                                               index_interp,pu,pv,NULL,NULL)) cg_error_exit();
/* allocate fields */
    rho  = createField(ni*nj);
    pres = createField(ni*nj);
/* write flow solution (user must use SIDS-standard names here) */
    if (cg_field_write(index_file,index_base,index_zone,index_flow,
                       CGNS_ENUMV(RealDouble),"Density",rho,&index_field)) cg_error_exit();
    if (cg_field_write(index_file,index_base,index_zone,index_flow,
                       CGNS_ENUMV(RealDouble),"Pressure",pres,&index_field)) cg_error_exit();
/* close CGNS file */
    if (cg_close(index_file)) cg_error_exit();
    printf("\nSuccessfully added InterpolationPoints flow solution data to file grid_ho_c.cgns (unstructured)\n");
    printf("\nNote:  if the original CGNS file already had a FlowSolution_t node,");
    printf("\n          it has been overwritten\n");
    
    free(rho);
    free(pres);
    free(pu);
    free(pv);
    return 0;
}


double *createField(size_t nbelements)
{
  // Allocate field for (3rd order in space and 0th in time) solution on Quadrangle
  double *f;
  int sz;
  int error;
  error = cg_solution_lagrange_interpolation_size(CGNS_ENUMV(QUAD_4),3,0,&sz);
  if (error)
  {
      fprintf(stderr,"ERROR: an error occurred during Solution Lagrange interpolation size count !\n");
      cg_error_exit();
  }
  f = (double*) malloc(nbelements*sz*sizeof(double));
  memset(f,0,nbelements*sz*sizeof(double));
  return f;
}

void solutionLagrangeBasis(double **u, double **v)
{
  cgsize_t i;
  int sz;
  cg_solution_lagrange_interpolation_size(CGNS_ENUMV(QUAD_4),3,0,&sz);
  
  u[0] = (double*)malloc(sz*sizeof(double));
  v[0] = (double*)malloc(sz*sizeof(double));
  
  for( i = 0 ; i < sz ; i++)
  {
    solutionCtrlPoints(i,&(u[0][i]),&(v[0][i]));
  }
}

void solutionCtrlPoints(int i, double *x, double *y)
{
  /* return solution control points for 3rd order QUAD */
  double U[16] = {-1    , 1    , 1    ,-1    ,
                  -1./3., 1./3., 1    , 1    ,
                   1./3.,-1./3.,-1    ,-1    ,
                  -1./3., 1./3., 1./3.,-1./3.};
  double V[16] = {-1    ,-1    , 1    , 1    ,
                  -1    ,-1    ,-1./3., 1./3.,
                   1    , 1    , 1./3.,-1./3.,
                  -1./3.,-1./3., 1./3., 1./3.};
  
  *x = U[i];
  *y = V[i];
}
