/*
! @file para_high_order.c

! @section DESCRIPTION
! Test program for pcgns library
! -- Create a CGNS file to exploit the high order implementation from CPEX045
! -- Write an unstructured QUAD_9 based mesh with 3rd order InterpolationPoints solution (ex: DGM)
! -- Reopen it and check that what was written is what comes back
!
! To visualize, the solution field, you could use GMSH with high order CGNS feature
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "pcgnslib.h"
#include "mpi.h"
#include "utils.h"

#define cgp_doError {printf("Error at %s:%d\n",__FILE__, __LINE__); return 1;}

double solutionField(double x, double y);

void solutionCtrlPoints(int i, double *x, double *y);

/* Generators expressed in terms of the owning rank, so the verify phase can
 * reproduce a zone it did not write. */
static void zoneCoord(int owner, int iset, double *x, double *y, double *z);
static double zoneFieldValue(int owner, int e, int i);

static int mismatches = 0;

static void mismatch(int rank, const char *what, long long index,
                     double got, double expected)
{
  if (mismatches < 10)
    printf("[rank %d] MISMATCH %s[%lld]: got %.17g, expected %.17g\n",
           rank, what, index, got, expected);
  mismatches++;
}

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
  cgsize_t start, end, min, max, emin, emax;
  char ZoneName[33];
  int zn;
  double *pu, *pv;
  
  
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
  
  // NOTE: the Family and its interpolation nodes must be created BEFORE the
  //       fields, not after.  A GridLocation=InterpolationPoints field array is
  //       sized as sum_e N_DOFs(e), and N_DOFs comes from the
  //       SolutionInterpolation_t matching each element, reached through the
  //       zone's FamilyName_t.  With the family written afterwards the basis
  //       cannot be resolved and cgp_field_write fails.
  // [2] Create the Family and its ElementInterpolation_t and SolutionInterpolation_t
  //     nodes
  {
    // [2.1] Create Family
    if (cg_family_write(fn,B,"Family",&Fam) )
      cgp_error_exit();
    
    // [2.2] Write ElementInterpolation_t node
    {
      // [2.2.1] Write ElementInterpolation_t node
      if (cg_element_interpolation_write(fn,B,Fam,"2ndOrderQUAD",CGNS_ENUMV(QUAD_9),&Ei) )
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
      if (cg_solution_interpolation_write(fn,B,Fam,"3rdOrderQUADsolution",CGNS_ENUMV(QUAD_4),3,0,
                                          CGNS_ENUMV(ParametricLagrange),&Si) )
        cgp_error_exit();
      
      // [2.3.2] Write LagrangeControlPoints (optional)
      {
 	int i;
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

  // [1] Create the Nodes (all processes have to be part of it)
  for (zn = 1 ; zn <= nzones ; zn++)
  {
    // [1.1] Create the corresponding Zone
    sprintf(ZoneName,"Zone %d",zn);
    if (cg_zone_write(fn, B, ZoneName, nijk, CGNS_ENUMV(Unstructured), &Z))
      cgp_error_exit();
    
    // [1.2] Write the Related Family Name (here only one for all Zones)
    if (cg_goto(fn, B, "Zone_t", Z, NULL) || cg_famname_write("Family"))
      cgp_error_exit();
    
    // [1.3] Write the Ordinal of the zone (Optional)
    if (cg_goto(fn, B, "Zone_t", Z, NULL) || cg_ordinal_write(zn) )
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
    if (cg_sol_write(fn,B,Z,"solution",CGNS_ENUMV(InterpolationPoints),&Sol) )
      cgp_error_exit();

    // [1.7] Write the Solution Order (3rd Order in space, 0th Order in time)
    if (cg_sol_interpolation_degree_write(fn,B,Z,Sol,3,0) )
      cgp_error_exit();
    
    // Note : It is MANDATORY to set solution interpolation Order BEFORE creating and writing the 
    //        field data. CGNS will internally deduce the size of the array based on the 
    //        interpolation orders.
    
    // [1.8] Write the solution Field 
    if (cgp_field_write(fn,B,Z,Sol,CGNS_ENUMV(RealDouble), "Density", &Fld))
        cgp_error_exit();
    
  }
  
  // [3] Each MPI process will write its own data
  {
    cgsize_t *elements;
    double *field, *x, *y, *z;
    int e,n;
    
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
      int iset;
      // [3.1.1] Fill Coordinates (shift for each process)
      for (iset = 0; iset < 7*7; iset++)
        zoneCoord(comm_rank, iset, &x[iset], &y[iset], &z[iset]);

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
      int i,j;
      // [3.2.1] Fill Connectivities
      e = 0;
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
      int i;
      // [3.3.1] Fill Solution field (dummy values)
      memset(field,0,9*16*sizeof(double));
      
      for( e = 0 ; e < 9 ; e++)
      {
        for( i = 0 ; i < 16 ; i++)
        {
          n = i + 16*e;
          field[ n ] = zoneFieldValue(comm_rank, e, i);
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

  // [4] Read the file back and check it against the generators.
  //     Each rank verifies its neighbour's zone rather than its own: a rank that
  //     re-read what it just wrote would agree with itself even if both sides
  //     shared the same wrong offset, which is exactly the failure a parallel
  //     test needs to catch.
  {
    int peer = (comm_rank + 1) % nzones;
    double *x, *y, *z, *field;
    cgsize_t *elements;
    int i, e;

    if (cgp_open("test_high_order.cgns", CG_MODE_READ, &fn))
      cgp_error_exit();

    Z = peer + 1;

    // [4.1] The field length must be the one the library derived from the
    //       interpolation degree: 9 elements x 16 DOFs at degree 3.  This is
    //       the whole point of writing InterpolationDegrees before the field,
    //       so it is checked on disk rather than assumed from the write.
    {
      char aname[33];
      int ndim;
      cgsize_t dimv[3];
      CGNS_ENUMT(DataType_t) dt;

      if (cg_goto(fn, B, "Zone_t", Z, "FlowSolution_t", 1, NULL) ||
          cg_array_info(1, aname, &dt, &ndim, dimv))
        cgp_error_exit();
      if (ndim != 1 || dimv[0] != 9*16)
        mismatch(comm_rank, "FieldLength", 0, (double)dimv[0], (double)(9*16));
    }

    // [4.2] Coordinates
    x = (double*)malloc(7*7*sizeof(double));
    y = (double*)malloc(7*7*sizeof(double));
    z = (double*)malloc(7*7*sizeof(double));

    min = 1;
    max = 7*7;
    if (cgp_coord_read_data(fn,B,Z,Cx,&min,&max,x) ||
        cgp_coord_read_data(fn,B,Z,Cy,&min,&max,y) ||
        cgp_coord_read_data(fn,B,Z,Cz,&min,&max,z))
      cgp_error_exit();

    for (i = 0; i < 7*7; i++)
    {
      double ex, ey, ez;
      zoneCoord(peer, i, &ex, &ey, &ez);
      if (x[i] != ex) mismatch(comm_rank, "CoordinateX", i, x[i], ex);
      if (y[i] != ey) mismatch(comm_rank, "CoordinateY", i, y[i], ey);
      if (z[i] != ez) mismatch(comm_rank, "CoordinateZ", i, z[i], ez);
    }

    free(x);
    free(y);
    free(z);

    // [4.3] Connectivity.  The same 9x9 pattern in every zone, so the expected
    //       values are rebuilt here the same way the writer built them.
    elements = (cgsize_t *)malloc(9*9*sizeof(cgsize_t));
    emin = 1;
    emax = 9;
    if (cgp_elements_read_data(fn,B,Z,S,emin,emax,elements))
      cgp_error_exit();

    {
      int ii,jj,ee = 0;
      for (jj = 0; jj < 3; jj++)
      {
        int nn = jj*7*2;
        for (ii = 0; ii < 3; ii++)
        {
          cgsize_t expect[9];
          int k;
          expect[0] = 0  + nn + 1;
          expect[1] = 2  + nn + 1;
          expect[2] = 16 + nn + 1;
          expect[3] = 14 + nn + 1;
          expect[4] = 1  + nn + 1;
          expect[5] = 9  + nn + 1;
          expect[6] = 15 + nn + 1;
          expect[7] = 7  + nn + 1;
          expect[8] = 8  + nn + 1;
          for (k = 0; k < 9; k++, ee++)
            if (elements[ee] != expect[k])
              mismatch(comm_rank, "Connectivity", ee,
                       (double)elements[ee], (double)expect[k]);
          nn = nn + 2;
        }
      }
    }

    free(elements);

    // [4.4] Solution field
    field = (double*)malloc(9*16*sizeof(double));
    min = 1;
    max = 9*16;
    if (cgp_field_read_data(fn,B,Z,Sol,Fld,&min,&max,field))
      cgp_error_exit();

    for (e = 0; e < 9; e++)
      for (i = 0; i < 16; i++)
      {
        int n = i + 16*e;
        double expect = zoneFieldValue(peer, e, i);
        /* zoneFieldValue runs through sin()/sqrt(): the write-time call and
         * this recomputation are the same source expression but different
         * call sites, and compilers are free to contract a*b+c to a single
         * FMA at one site and not the other, so bit-exact equality is not a
         * guarantee IEEE 754 makes here.  compareValuesDouble (utils.h) is
         * the tolerance the rest of the suite already uses for this. */
        if (!compareValuesDouble(field[n], expect))
          mismatch(comm_rank, "Density", n, field[n], expect);
      }

    free(field);

    if (cgp_close(fn))
      cgp_error_exit();
  }

  // [5] A mismatch anywhere fails the run everywhere
  {
    int total = 0;
    err = MPI_Allreduce(&mismatches, &total, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (err != MPI_SUCCESS) cgp_doError;

    if (comm_rank == 0)
    {
      if (total == 0)
        printf("para_high_order: PASSED (mesh and solution verified on read-back)\n");
      else
        printf("para_high_order: FAILED with %d mismatch(es)\n", total);
    }

    err = MPI_Finalize();
    if(err!=MPI_SUCCESS) cgp_doError;
    return total == 0 ? 0 : 1;
  }
}

/* Zone coordinates as a function of the owning rank: a 7x7 lattice shifted onto
 * the rank's quadrant of the 2x2 zone layout. */
static void zoneCoord(int owner, int iset, double *x, double *y, double *z)
{
  int i = iset % 7;
  int j = iset / 7;
  *x = (double)i + (owner%2)*(7-1);
  *y = (double)j + (owner/2)*(7-1);
  *z = 0.;
}

/* Value of the analytic solution at control point i of element e in the zone
 * owned by `owner`. */
static double zoneFieldValue(int owner, int e, int i)
{
  double xloc, yloc;
  solutionCtrlPoints(i, &xloc, &yloc);
  xloc += 2.0 + (e%3)*2. + (owner%2)*(7-1);
  yloc += 2.0 + (e/3)*2. + (owner/2)*(7-1);
  return solutionField(xloc, yloc);
}

double solutionField(double x, double y)
{
  double r = sqrt(pow(x-7.,2.)+pow(y-7.,2.));
  return sin(r);
}

void solutionCtrlPoints(int i, double *x, double *y)
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


