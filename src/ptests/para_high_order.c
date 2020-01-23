/*
! @file para_high_order.c

! @section DESCRIPTION
! Test program for pcgns library
! -- Create a CGNS file to exploit the high order implementation from CPEX045
! -- Write an unstructured QUAD_9 based mesh with 3rd order ElementBased solution (ex: DGM)
! 
! To visualize, the solution field, you could use GMSH with high order CGNS feature
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "pcgnslib.h"
#include "mpi.h"

#define cgp_doError {printf("Error at %s:%u\n",__FILE__, __LINE__); return 1;}

double solutionField(double x, double y);

double solutionCtrlPoints(int i, double *x, double *y);

int main (int argc, char **argv)
{
  int err;
  int comm_size;
  int comm_rank;
  int cell_dim = 2;
  int phys_dim = 3;
  int nzones   = 4;
  int fn,B,Z,S,Sol,Fld,Cx,Cy,Cz,Fam,Ei,Si;
  cgsize_t nijk[3];
  cgsize_t start, end, min, max, emin, emax, *elements;
  char ZoneName[33];
  
  double *pu, *pv, *field;
  double *x, *y, *z;
  double xloc,yloc,r;
  int i,j,k,e,n,iset;
  
  
  // MPI Stuff
  err = MPI_Init(&argc,&argv);
  if(err!=MPI_SUCCESS) cgp_doError;
  err = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
  if(err!=MPI_SUCCESS) cgp_doError;
  err = MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
  if(err!=MPI_SUCCESS) cgp_doError;
  
  // 4 process required for this test !
  if( comm_size != nzones ) cgp_doError;
  
  // Each process will write 3x3 quadratic QUADS (2nd order QUAD -> QUAD_9)
  
  // Per Zone Infos ( 49 points , 9 Elements )
  nijk[0] = 7*7;
  nijk[1] = 3*3;
  nijk[2] = 0;
  
  // Open File in parallel
  if (cgp_open("test_high_order.cgns", CG_MODE_WRITE, &fn))
    cgp_error_exit();
  
  // Create the base in parallel
  if (cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B))
    cgp_error_exit();
  
  // [1] Create the Nodes (all processes have to be part of it)
  for (i = 1 ; i <= nzones ; i++)
  {
    // [1.1] Create the corresponding Zone
    sprintf(ZoneName,"Zone %d",i);
    if (cg_zone_write(fn, B, ZoneName, nijk, CGNS_ENUMV(Unstructured), &Z))
      cgp_error_exit();
    
    // [1.2] Write the Related Family Name (here only one for all Zones)
    if (cg_goto(fn, B, "Zone_t", Z, NULL) || cg_famname_write("Family"))
      cgp_error_exit();
    
    // [1.3] Write the Ordinal of the zone (Optional)
    if (cg_goto(fn, B, "Zone_t", Z, NULL) || cg_ordinal_write(i) )
      cgp_error_exit();
    
    // [1.4] Create the coordinates nodes
    if (cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX",&Cx) ||
        cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY",&Cy) ||
        cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ",&Cz))
        cgp_error_exit();
    
    // [1.5] Create the Element Section
    start = 1;
    end   = 9;
    if (cgp_section_write(fn,B,Z,"Elements",CGNS_ENUMV(QUAD_9),start,end,0,&S))
      cgp_error_exit();
    
    // [1.6] Write the Solution Node
    if (cg_sol_write(fn,B,Z,"solution",ElementBased,&Sol) )
      cgp_error_exit();
      
    // [1.7] Write the Solution Order (3rd Order in space, 0th Order in time)
    if (cg_sol_interpolation_order_write(fn,B,Z,Sol,3,0) )
      cgp_error_exit();
    
    // Note : It is MANDATORY to set solution interpolation Order BEFORE creating and writing the 
    //        field datas. CGNS will internally deduce the size of the array based on the 
    //        interpolation orders.
    
    // [1.8] Write the solution Field 
    if (cgp_field_write(fn,B,Z,Sol,CGNS_ENUMV(RealDouble), "Density", &Fld))
        cgp_error_exit();
    
  }
  
  // [2] Create the Family and its ElementInterpolation_t and SolutionInterpolation_t
  //     nodes
  {
    // [2.1] Create Family
    if (cg_family_write(fn,B,"Family",&Fam) )
      cgp_error_exit();
    
    // [2.2] Write ElementInterpolation_t node
    {
      // [2.2.1] Write ElementInterpolation_t node
      if (cg_element_interpolation_write(fn,B,Fam,"2ndOrderQUAD",QUAD_9,&Ei) )
        cgp_error_exit();
      
      // [2.2.2] Write LagrangeControlPoints (optional)
      {
        // [2.2.2.1] Allocate the control points
        pu = (double*)malloc(9*sizeof(double));
        pv = (double*)malloc(9*sizeof(double));
        memset(pu,0,9*sizeof(double));
        memset(pv,0,9*sizeof(double));
        
        // [2.2.2.2] Fill the Control Points
        pu[0] = pu[3] = pu[7] = -1.;
        pu[1] = pu[2] = pu[5] =  1.;
        pv[0] = pv[1] = pv[4] = -1.;
        pv[2] = pv[3] = pv[6] =  1.;
        
        // [2.2.2.3] Write the Lagrange Control Points (2D, thus only u and v are required)
        if (cg_element_interpolation_points_write(fn,B,Fam,Ei,pu,pv,NULL))
          cgp_error_exit();
        
        free(pu);
        free(pv);
      }
    }
    
    // [2.3] Write SolutionInterpolation_t node
    {
      // [2.3.1] Write SolutionInterpolation_t node (3rd order in space, 0th in time for QUAD)
      if (cg_solution_interpolation_write(fn,B,Fam,"3rdOrderQUADsolution",QUAD_4,3,0,
                                          ParametricLagrange,&Si) )
        cgp_error_exit();
      
      // [2.3.2] Write LagrangeControlPoints (optional)
      {
        // [2.3.2.1] Allocate the control points
        pu = (double*)malloc(16*sizeof(double));
        pv = (double*)malloc(16*sizeof(double));
        memset(pu,0,16*sizeof(double));
        memset(pv,0,16*sizeof(double));
        
        // [2.3.2.2] Fill the Control Points
        for ( i = 0 ; i < 16 ; i++)
          solutionCtrlPoints(i,&pu[i],&pv[i]);
        
        // [2.3.2.3] Write the Lagrange Control Points (2D, thus only u and v are required)
        if (cg_solution_interpolation_points_write(fn,B,Fam,Si,pu,pv,NULL,NULL))
          cgp_error_exit();
        
        free(pu);
        free(pv);
      }
    }
    
  }
  
  // [3] Each MPI process will write its own datas
  {
    Z = comm_rank + 1;
  
    // [3.0] Allocate fields
    {
      // [3.0.1] Allocate Coordinates Array
      x = (double*)malloc(7*7*sizeof(double));
      y = (double*)malloc(7*7*sizeof(double));
      z = (double*)malloc(7*7*sizeof(double));
      
      // [3.0.2] Allocate Element Connectivities (9 QUAD 2nd order -> 9x9 nodes)
      elements = (cgsize_t *)malloc(9*9*sizeof(cgsize_t));
      
      // [3.0.3] Allocate Field array (9 QUAD 3rd order in space and 0 in time -> 9*16 slots)
      field = (double*)malloc(9*16*sizeof(double));
    }
    
    // [3.1] Fill Coordinates
    {
      
      // [3.1.1] Fill Coordinates (shift for each process)
      iset = 0;
      for (j=1; j <= 7; j++)
      {
        for (i=1; i <= 7; i++)
        {
          x[iset]=(double)i-1. + (comm_rank%2)*(7-1);
          y[iset]=(double)j-1. + (comm_rank/2)*(7-1);
          z[iset]= 0.;
          iset=iset+1;
        }
      }
      
      // [3.1.2] Write Coordinates
      min = 1;
      max = 7*7;
      if (cgp_coord_write_data(fn,B,Z,Cx,&min,&max,x) ||
          cgp_coord_write_data(fn,B,Z,Cy,&min,&max,y) ||
          cgp_coord_write_data(fn,B,Z,Cz,&min,&max,z))
        cgp_error_exit();
      
    }
    
    // [3.2] Fill Element Section
    {
      
      // [3.2.1] Fill Connectivities
      e = 0; n = 0;
      for (j = 0; j < 3; j++)
      {
        n = j*7*2;
        for (i = 0; i < 3; i++)
        {
          elements[ e++ ] = 0  + n + 1;
          elements[ e++ ] = 2  + n + 1;
          elements[ e++ ] = 16 + n + 1;
          elements[ e++ ] = 14 + n + 1;
          elements[ e++ ] = 1  + n + 1;
          elements[ e++ ] = 9  + n + 1;
          elements[ e++ ] = 15 + n + 1;
          elements[ e++ ] = 7  + n + 1;
          elements[ e++ ] = 8  + n + 1;
          n = n + 2;
        }
      }
      
      // [3.2.2] Write Connectivities
      emin = 1;
      emax = 9;
      if (cgp_elements_write_data(fn,B,Z,S,emin,emax,elements))
        cgp_error_exit();
      
      
    }
    
    // [3.3] Fill Solution 
    {
      
      // [3.3.1] Fill Solution field (dummy values)
      memset(field,0,9*16*sizeof(double));
      
      for( e = 0 ; e < 9 ; e++)
      {
        for( i = 0 ; i < 16 ; i++)
        {
          n = i + 16*e;
          
          solutionCtrlPoints(i,&xloc,&yloc);
          
          xloc += 2.0 + (e%3)*2. + (comm_rank%2)*(7-1);
          yloc += 2.0 + (e/3)*2. + (comm_rank/2)*(7-1);
          
          field[ n ] = solutionField(xloc,yloc);
        }
      }
      
      // [3.3.2] Write Field
      min = 1;
      max = 9*16;
      if (cgp_field_write_data(fn,B,Z,Sol,Fld,&min,&max,field))
        cgp_error_exit();
      
    }
    
    // Free memory
    free(x);
    free(y);
    free(z);
    free(elements);
    free(field);
  }
  
  // Close the file
  if (cgp_close(fn)) 
    cgp_error_exit();
  
  // Finalize MPI
  err = MPI_Finalize();
  if(err!=MPI_SUCCESS) cgp_doError;
  return 0;
}

double solutionField(double x, double y)
{
  double r = sqrt(pow(x-7.,2.)+pow(y-7.,2.));
  return sin(r);
}

double solutionCtrlPoints(int i, double *x, double *y)
{
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


