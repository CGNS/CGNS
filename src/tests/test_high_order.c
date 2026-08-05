#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"

void fillQuadLagrangePoints(int order, double *u, double *v)
{
  // Grid like sorted
  int i, j;
    for (j=0; j < (order+1); j++)
    {
      for (i=0; i < (order+1); i++)
      {
        u[i + j*(order+1)] = -1. + i*2./order;
        v[i + j*(order+1)] = -1. + j*2./order;
      }
    }
}

int main (int argc, char **argv)
{
    double start, finish;
    int error, i, j, iset, ii, jj, ni, nj, ifirstnode, ielem_no, nbdyelem, n;
    cgsize_t nsize;
    int ncount;
    double *x;
    double *y;
    double *pu, *pv, *puu, *pvv, *r;
    cgsize_t *ielem;
    cgsize_t size[9],sizeread[9], dimvals[1];
    cgsize_t nelem_start,nelem_end;
    cgsize_t rmin[1],rmax[1];
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(ElementType_t) type, etyperead;
    CGNS_ENUMT(InterpolationType_t) itype, ityperead;
    int cgfile, cgbase, cgzone, cgsection, cgfamily, cgsol, cgcoord, cgeinterp, cgsinterp;
    char einterpName[33],sinterpname[33],fieldname[33],zonename[33],familyname[33],sectionname[33],solname[33];
    int os,ot,cntneinterp,neinterp,nsinterp;
    cgsize_t nbsolpts;
    
    
    int order = 2;
    type = CGNS_ENUMV(QUAD_9);
    
    int ncellI = 6;
    int ncellJ = 4;
    ni=ncellI*order+1;
    nj=ncellJ*order+1;
    
    /* vertex size */
    size[0]=ni*nj;
    /* cell size */
    size[1]=ncellI*ncellJ;
    /* boundary vertex size (zero if elements not sorted) */
    size[2]=0;
    
    x = (double*) malloc( (size_t) (ni*nj) * sizeof(double));
    y = (double*) malloc( (size_t) (ni*nj) * sizeof(double));
    pu = (double*) malloc( (size_t) (order+1)*(order+1) * sizeof(double));
    pv = (double*) malloc( (size_t) (order+1)*(order+1) * sizeof(double));
    ielem = (cgsize_t*) malloc( (order+1)*(order+1)*(ncellI*ncellJ)*sizeof(cgsize_t));
    
    /* create 2nd order gridpoints for simple example: */
    iset=0;
    for (j=1; j <=nj; j++)
    {
      for (i=1; i <= ni; i++)
      {
        x[iset]=(double)i-1.;
        y[iset]=(double)j-1.;
        iset=iset+1;
      }
    }
    
    /* Fill (U,V) */
    fillQuadLagrangePoints(order,pu,pv);
    
    printf ("Writing cgns file high_order.cgns ...\n");
    if (cg_open ("high_order.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write (cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write (cgfile, cgbase, "zone", size, CGNS_ENUMV(Unstructured), 
                       &cgzone)) 
      cg_error_exit();
    
    printf ("Writing coordinates ...\n");

    if ( cg_coord_write(cgfile, cgbase, cgzone,CGNS_ENUMV(RealDouble),"CoordinateX",
                        x,&cgcoord) ||
         cg_coord_write(cgfile, cgbase, cgzone,CGNS_ENUMV(RealDouble),"CoordinateY",
                        y,&cgcoord) )
      cg_error_exit();
    
    
    printf ("Writing indices ...\n");
    
    /*
do HO QUAD elements (NOT following standard SIDS ordering)
*/
    ielem_no=0;
/* index no of first element */
    nelem_start=1;
    for (j=0; j < ncellJ; j++)
    {
      for (i=0; i < ncellI; i++)
      {
        ifirstnode=1+ (i*(order)) +  (j)*(order)*ni;
        // Grid like sorted
        for (jj = 0 ; jj < order+1 ; jj++)
          for (ii = 0 ; ii < order+1 ; ii++)
          {
              ielem[ielem_no++] = ifirstnode + ii + jj*ni;
          }
      }
    }
/* index no of last element */
    nelem_end=ncellI*ncellJ;
    nbdyelem = 0;
/* write QUAD element connectivity for inflow face (user can give any name) */
    error = cg_section_write(cgfile, cgbase, cgzone,"Domain",type,nelem_start,
                             nelem_end,nbdyelem,ielem,&cgsection);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during section writing !\n");
        cg_error_exit();
    }
    
    free(ielem); free(x); free(y);
    
    printf ("Writing family  ...\n");
    error = cg_family_write(cgfile, cgbase,"family",&cgfamily);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during family writing !\n");
        cg_error_exit();
    }

    /* Attach the family to the zone.  A high-order FlowSolution_t field is
     * sized as sum_e N_DOFs(e), and N_DOFs(e) comes from the
     * SolutionInterpolation_t matching element e -- which is reachable only
     * through the zone's FamilyName_t.  Without this the library cannot size
     * the field and refuses the write rather than assuming a cardinality. */
    error = cg_goto(cgfile, cgbase, "Zone_t", cgzone, NULL);
    if (!error) error = cg_famname_write("family");
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred attaching the family to the zone !\n");
        cg_error_exit();
    }


    printf ("Writing ElementInterpolation_t node  ...\n");
    
    error = cg_element_interpolation_write(cgfile, cgbase,cgfamily,"QuadInterpolation",type,&cgeinterp);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during Mesh element interpolation writing !\n");
        cg_error_exit();
    }
    error = cg_element_interpolation_points_write(cgfile, cgbase,cgfamily,cgeinterp,pu,pv,NULL);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during Mesh Lagrange points writing !\n");
        cg_error_exit();
    }
    
    /* Test if possible to modify an existing ElementInterpolation_t for the same Element Type --> HAS TO FAIL IN WRITE MODE ! */
    {
        int ntemp;
        error = cg_element_interpolation_write(cgfile, cgbase,cgfamily,"QuadInterpolation",
                                               type,&ntemp);
        if (error != CG_ERROR)
        {
            fprintf(stderr,"ERROR CODE %d : Should not be able to modify an existing ElementInterpolation_t node for the same ElementType_t in WRITE MODE !\n",error);
            cg_error_exit();
        }
        fflush(stderr);fflush(stdout);
    }

    /* Test if possible to add another ElementInterpolation_t for the same Element Type --> HAS TO FAIL ! */
    {
        int ntemp;
        error = cg_element_interpolation_write(cgfile, cgbase,cgfamily,"QuadInterpolation_2",
                                               type,&ntemp);
        if (error != CG_ERROR)
        {
            fprintf(stderr,"ERROR CODE %d : Should Not be able to write 2 ElementInterpolation_t node for the same ElementType_t. An error should occurs !\n",error);
            cg_error_exit();
        }
        fflush(stderr);fflush(stdout);
    }
    
    printf ("Writing SolutionInterpolation_t node  ...\n");

    free(pu); free(pv);

    /* Degree 3 solution basis.  The FlowSolution_t below declares
     * InterpolationDegrees = (3,0), and those degrees *select* the family-level
     * SolutionInterpolation_t that defines N_DOFs for the field arrays -- so a
     * matching degree-3 block has to exist.  Without it the field length is
     * undefined and the write is refused. */
    order = 3;
    pu = (double*) malloc( (size_t) (order+1)*(order+1) * sizeof(double));
    pv = (double*) malloc( (size_t) (order+1)*(order+1) * sizeof(double));
    fillQuadLagrangePoints(order,pu,pv);

    error = cg_solution_interpolation_write(cgfile, cgbase,cgfamily,"3rdOrderQuadSolution",
                                            CGNS_ENUMV(QUAD_4),order,0,
                                            CGNS_ENUMV(ParametricLagrange),&cgsinterp);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during degree-3 Solution interpolation writing !\n");
        cg_error_exit();
    }
    error = cg_solution_interpolation_points_write(cgfile, cgbase,cgfamily,cgsinterp,
                                                   pu,pv,NULL,NULL);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during degree-3 Lagrange points writing !\n");
        cg_error_exit();
    }

    free(pu); free(pv);
    // Order 4 solution
    order = 4;
    pu = (double*) malloc( (size_t) (order+1)*(order+1) * sizeof(double));
    pv = (double*) malloc( (size_t) (order+1)*(order+1) * sizeof(double));
    
    // Fill (U,V)
    fillQuadLagrangePoints(order,pu,pv);
    
    error = cg_solution_interpolation_write(cgfile, cgbase,cgfamily,"4rdOrderQuadSolution",
                                            CGNS_ENUMV(QUAD_4),order,0,CGNS_ENUMV(ParametricLagrange),&cgsinterp);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during Solution interpolation writing !\n");
        cg_error_exit();
    }
    error = cg_solution_interpolation_points_write(cgfile, cgbase,cgfamily,cgsinterp,
                                                   pu,pv,NULL,NULL);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during Solution Lagrange interpolation points writing !\n");
        cg_error_exit();
    }
    
    /* Test if possible to add another SolutionInterpolation_t for the same (ElementType,sorder,torder) --> HAS TO FAIL ! */
    {
        int ntemp;
        error = cg_solution_interpolation_write(cgfile, cgbase,cgfamily,"4rdOrderQuadSolution_2",
                                            CGNS_ENUMV(QUAD_4),order,0,CGNS_ENUMV(ParametricLagrange),&ntemp);

        if (error != CG_ERROR)
        {
            fprintf(stderr,"Should Not be able to write 2 SolutionInterpolation_t node for the same ElementType_t. An error should occurs !");
            cg_error_exit();
        }
        fflush(stderr);fflush(stdout);
    }
    
    /* Get Node Count */
    if (cg_nelement_interpolation_read(cgfile, cgbase,cgfamily,&neinterp) ||
        cg_nsolution_interpolation_read(cgfile, cgbase,cgfamily,&nsinterp) )
    {
        fprintf(stderr,"ERROR: Impossible to get the interpolation node count.\n");
        cg_error_exit();
    }
    
    /* One ElementInterpolation_t (QUAD) and two SolutionInterpolation_t
     * blocks: degree 3 (used by the FlowSolution_t below) and degree 4. */
    if (neinterp != 1 || nsinterp != 2)
    {
        fprintf(stderr,"ERROR: wrong interpolation node count.\n");
        fprintf(stderr,"       cg_nelement_interpolation_read = %d, should be 1.\n",neinterp);
        fprintf(stderr,"       cg_nsolution_interpolation_read = %d, should be 2.\n",nsinterp);
        cg_error_exit();
    }
    free(pu); free(pv);
    


    /* Creating Solution */
    error = cg_sol_write(cgfile,cgbase,cgzone,"FlowSolution",CGNS_ENUMV(InterpolationPoints),&cgsol);

    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during Solution node writing !\n");
        cg_error_exit();
    }
    
    /* add Corresponding spatial and temporal order --> 3rd and 0th order */
    error = cg_sol_interpolation_degree_write(cgfile,cgbase,cgzone,cgsol,3,0);
    
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during solution order writing !\n");
        cg_error_exit();
    }
    
    /* Get number of pts required */
    error = cg_solution_lagrange_interpolation_size(type,3,0,&nbsolpts);
    
    if (error)
    {
        fprintf(stderr,"ERROR: an error occurred during Solution Lagrange interpolation size count !\n");
        cg_error_exit();
    }
    
    if (nbsolpts != 4*4)
    {
        fprintf(stderr,"ERROR: Wrong number of points given !\n");
        cg_error_exit();
    }
    
    /* add dummy solution field */
    r = (double*) malloc( (size_t) (ncellI*ncellJ*nbsolpts) * sizeof(double));
    
    {
        int nfield;
        error = cg_field_write(cgfile,cgbase,cgzone,cgsol,CGNS_ENUMV(RealDouble),
                       "Density",r,&nfield);
        free(r);

        if (error)
        {
            fprintf(stderr,"ERROR: an error occurred during Density Field writing !\n");
            cg_error_exit();
        }
    }
    
    fflush (stdout);
    printf ("closing cgns file ...\n");
    cg_close (cgfile);
    
    /* ************************************************************************** 
     * 
     *                             READ BACK
     * 
     * ************************************************************************** */
    
    printf ("reading cgns file high_order.cgns in READ mode ...\n");
    if (cg_open ("high_order.cgns", CG_MODE_READ, &cgfile)) cg_error_exit();

    // Get Information from Current Zone
    if (cg_zone_read(cgfile, cgbase,cgzone,zonename,sizeread) ) cg_error_exit();
    
    /* Check Zone */
    if (strcmp(zonename,"zone"))
    {
        fprintf(stderr,"ERROR: Wrong Zone Name !\n");
        cg_error_exit();
    }
    
    if (sizeread[0] != size[0] || sizeread[1] != size[1] || sizeread[2] != size[2])
    {
        fprintf(stderr,"ERROR: Wrong Zone Size !\n");
        cg_error_exit();
    }
    
    /* Read Family_t */
    if(cg_family_read(cgfile, cgbase,cgfamily,familyname,&i,&j))  cg_error_exit();
    
    /* Check Family */
    if (strcmp(familyname,"family"))
    {
        fprintf(stderr,"ERROR: Wrong Family Name !\n");
        cg_error_exit();
    }
    
    /* Check Existency */
    printf("Validating ElementInterpolation_t node count...\n");
    error = cg_element_lagrange_interpolation_count(cgfile, cgbase,cgfamily,type,&ncount);

    if (ncount != 1 || error)
    {
        fprintf(stderr,"ERROR: Wrong Number of ElementInterpolation_t node of type %s !\n",
                cg_ElementTypeName(type));
        cg_error_exit();
    }
    printf("Found %d ElementInterpolation_t node for %s\n", (int)ncount, cg_ElementTypeName(type));
    
    /* Read Element interpolation Node */
    printf("Reading ElementInterpolation_t node properties...\n");
    if (cg_element_interpolation_read(cgfile, cgbase,cgfamily,cgeinterp,einterpName,&etyperead))
    {
        fprintf(stderr,"ERROR: Cannot Read The ElementInterpolation_t node !\n");
        cg_error_exit();
    }

    /* Check Element interpolation Node */
    if (strcmp(einterpName,"QuadInterpolation"))
    {
        fprintf(stderr,"ERROR: Wrong Element Interpolation Name !\n");
        cg_error_exit();
    }
    printf("Element interpolation name: %s\n", einterpName);

    if (etyperead != type)
    {
        fprintf(stderr,"ERROR: Wrong Element Interpolation Type !\n");
        cg_error_exit();
    }
    printf("Element type: %s\n", cg_ElementTypeName(etyperead));
    
    printf("Validating Lagrange control point dimensions...\n");
    error = cg_element_lagrange_interpolation_size(etyperead,&nsize);

    if (nsize != 9)
    {
        fprintf(stderr,"ERROR: cg_element_lagrange_interpolation_size returns wrong number !\n");
        cg_error_exit();
    }
    printf("Expected %d control points for QUAD_9\n", (int)nsize);

    pu = (double*) malloc( (size_t) (nsize) * sizeof(double));
    pv = (double*) malloc( (size_t) (nsize) * sizeof(double));
    puu = (double*) malloc( (size_t) (nsize) * sizeof(double));
    pvv = (double*) malloc( (size_t) (nsize) * sizeof(double));
    
    // Fill (U,V)
    fillQuadLagrangePoints(2,pu,pv);

    /* Read Element interpolation Points Node */
    printf("Reading Lagrange control points...\n");
    if (cg_element_interpolation_points_read(cgfile, cgbase,cgfamily,cgeinterp,puu,pvv,NULL))
    {
        fprintf(stderr,"ERROR CODE %d : Cannot Read The Element Interpolation points !\n",error);
        cg_error_exit();
    }
    /* Check UV Points */
    printf("Validating control point coordinates...\n");
    int failed_points = 0;
    for(i = 0 ; i < nsize; i++)
    {
        if ( fabs(pu[i] - puu[i]) > 1.e-06 || fabs(pv[i] - pvv[i]) > 1.e-06 )
        {
            fprintf(stderr,"ERROR: Element Interpolation points are not in tolerance !\n");
            fprintf(stderr,"  given at indice i=%d : (%f,%f) --> required (%f,%f)\n",i,
                    puu[i],pvv[i],pu[i],pv[i]);
            fprintf(stderr,"errors : (%e,%e)\n",fabs(pu[i] - puu[i]),fabs(pv[i] - pvv[i]));
            failed_points++;
        }
    }
    if (failed_points > 0)
    {
        fprintf(stderr,"ERROR: %d element interpolation points failed validation\n", failed_points);
        cg_error_exit();
    }
    printf("All %d element interpolation control points validated successfully\n", (int)nsize);
    free(puu);
    free(pvv);
    free(pu);
    free(pv);
    
    /* Check SolutionInterpolation_t Existency */
    printf("\nValidating SolutionInterpolation_t node count...\n");
    error = cg_solution_lagrange_interpolation_count(cgfile, cgbase,cgfamily,CGNS_ENUMV(QUAD_4),
                                                     4,0,&ncount);

    if (ncount != 1 || error)
    {
        fprintf(stderr,"ERROR: Wrong Number of SolutionInterpolation_t node (%s,%d,%d) !\n",
                cg_ElementTypeName(type),4,0);
        cg_error_exit();
    }
    printf("Found %d SolutionInterpolation_t node for QUAD_4 (order 4)\n", (int)ncount);
    
    /* Read Solution interpolation Node */
    printf("Reading SolutionInterpolation_t node properties...\n");
    if (cg_solution_interpolation_read(cgfile, cgbase,cgfamily,cgsinterp,sinterpname,
        &etyperead, &os,&ot,&ityperead))
    {
        fprintf(stderr,"ERROR: Cannot Read The SolutionInterpolation_t node !\n");
        cg_error_exit();
    }

    /* Check Solution interpolation Node */
    if (strcmp(sinterpname,"4rdOrderQuadSolution"))
    {
        fprintf(stderr,"ERROR: Wrong Solution Interpolation Name !\n");
        cg_error_exit();
    }
    printf("Solution interpolation name: %s\n", sinterpname);

    if (etyperead != CGNS_ENUMV(QUAD_4))
    {
        fprintf(stderr,"ERROR: Wrong Solution Interpolation ElementType_t !\n");
        cg_error_exit();
    }
    printf("Element type: %s\n", cg_ElementTypeName(etyperead));

    if (ityperead != CGNS_ENUMV(ParametricLagrange))
    {
        fprintf(stderr,"ERROR: Wrong Solution Interpolation Type !\n");
        cg_error_exit();
    }
    printf("Interpolation type: ParametricLagrange\n");

    if (os != 4 || ot != 0)
    {
        fprintf(stderr,"ERROR: Wrong Solution Interpolation Orders !\n");
        cg_error_exit();
    }
    printf("Interpolation orders: spatial=%d, temporal=%d\n", os, ot);
    
    printf("Validating solution interpolation dimensions...\n");
    error = cg_solution_lagrange_interpolation_size(etyperead,os,ot,&nsize);

    if (nsize != (4+1)*(4+1))
    {
        fprintf(stderr,"ERROR: cg_solution_lagrange_interpolation_size returns wrong number !\n");
        cg_error_exit();
    }
    printf("Expected %d control points for order %d solution\n", (int)nsize, os);

    pu = (double*) malloc( (size_t) (nsize) * sizeof(double));
    pv = (double*) malloc( (size_t) (nsize) * sizeof(double));
    puu = (double*) malloc( (size_t) (nsize) * sizeof(double));
    pvv = (double*) malloc( (size_t) (nsize) * sizeof(double));
    
    // Fill (U,V)
    fillQuadLagrangePoints(4,pu,pv);

    /* Read Solution interpolation Points Node */
    printf("Reading solution interpolation control points...\n");
    if (cg_solution_interpolation_points_read(cgfile, cgbase,cgfamily,cgsinterp,puu,pvv,NULL,NULL))
    {
        fprintf(stderr,"ERROR CODE %d : Cannot Read The Solution Interpolation points !\n",error);
        cg_error_exit();
    }
    /* Check UV Points */
    printf("Validating solution control point coordinates...\n");
    failed_points = 0;
    for(i = 0 ; i < nsize; i++)
    {
        if ( fabs(pu[i] - puu[i]) > 1.e-06 || fabs(pv[i] - pvv[i]) > 1.e-06 )
        {
            fprintf(stderr,"ERROR: Solution Interpolation points are not in tolerance !\n");
            fprintf(stderr,"  given at indice i=%d : (%f,%f) --> required (%f,%f)\n",i,
                    puu[i],pvv[i],pu[i],pv[i]);
            fprintf(stderr,"errors : (%e,%e)\n",fabs(pu[i] - puu[i]),fabs(pv[i] - pvv[i]));
            failed_points++;
        }
    }
    if (failed_points > 0)
    {
        fprintf(stderr,"ERROR: %d solution interpolation points failed validation\n", failed_points);
        cg_error_exit();
    }
    printf("All %d solution interpolation control points validated successfully\n", (int)nsize);
    free(puu);
    free(pvv);
    free(pu);
    free(pv);
    
    
    /* Check Solution Field */
    
    if (cg_sol_info(cgfile,cgbase,cgzone,cgsol,solname,&location))
    {
        fprintf(stderr,"ERROR: Cannot Read The FlowSolution_t node !\n");
        cg_error_exit();
    }

    if (location != CGNS_ENUMV(InterpolationPoints) )
    {
      fprintf(stderr,"ERROR: Wrong solution GridLocation_t !\n");
        cg_error_exit();
    }
    
    if (cg_nfields(cgfile,cgbase,cgzone,cgsol,&n))
    {
        fprintf(stderr,"ERROR: Cannot get number of fields !\n");
        cg_error_exit();
    }
    
    if (n != 1 ) 
    {
      fprintf(stderr,"ERROR: Wrong count of solution field !\n");
        cg_error_exit();
    }
    
    /* read dummy solution field */
    int ddim;
    error = cg_sol_size(cgfile,cgbase,cgzone,cgsol,&ddim,dimvals);
    
    if (error || ddim != 1 || dimvals[0] != ncellI*ncellJ*nbsolpts )
    {
        fprintf(stderr,"ERROR: An error occurred during cg_sol_size  !\n");
        cg_error_exit();
    }
    
    r = (double*) malloc( (size_t) (dimvals[0]) * sizeof(double));
    rmin[0] = 1;
    rmax[0] = dimvals[0];  
    if (cg_field_read(cgfile,cgbase,cgzone,cgsol,"Density",CGNS_ENUMV(RealDouble),
      rmin,rmax,r))
    {
        fprintf(stderr,"ERROR: Cannot Read The SolutionField_t node !\n");
        cg_error_exit();
    }
    free(r);
    
    fflush (stdout);
    printf ("closing cgns file ...\n");
    cg_close(cgfile);
    /* ************************************************************************** 
     * 
     *                             MODIFY
     * 
     * ************************************************************************** */
    
    printf ("reading cgns file high_order.cgns in MODIFY mode ...\n");
    if (cg_open ("high_order.cgns", CG_MODE_MODIFY, &cgfile) ) cg_error_exit();
    
    printf("Adding New Nodes\n");
    /* adding new Node */
    error = cg_element_interpolation_write(cgfile, cgbase,cgfamily,"TriInterpolation",
                                            CGNS_ENUMV(TRI_10),&cgeinterp);
    if (error)
    {
        fprintf(stderr,"ERROR CODE %d : Cannot add a new ElementInterpolation_t node for the same ElementType_t in MODIFY MODE !\n",error);
        cg_error_exit();
    }
    
    error = cg_solution_interpolation_write(cgfile, cgbase,cgfamily,"5thOrderTriSolution",
                                            CGNS_ENUMV(TRI_3),5,0,CGNS_ENUMV(ParametricLagrange),&cgsinterp);
    
    if (error)
    {
        fprintf(stderr,"ERROR CODE %d : Cannot add a new SolutionInterpolation_t node for the same ElementType_t in MODIFY MODE !\n",error);
        cg_error_exit();
    }
    
    /* Get Node Count */
    {
        if (cg_nelement_interpolation_read(cgfile, cgbase,cgfamily,&neinterp) ||
            cg_nsolution_interpolation_read(cgfile, cgbase,cgfamily,&nsinterp) )
        {
            fprintf(stderr,"ERROR: Impossible to get the interpolation node count.\n");
            cg_error_exit();
        }
        
        /* 2 ElementInterpolation_t (QUAD, TRI); 3 SolutionInterpolation_t
         * (QUAD degree 3, QUAD degree 4, TRI degree 5). */
        if (neinterp != 2 || nsinterp != 3)
        {
            fprintf(stderr,"ERROR: wrong interpolation node count.\n");
            fprintf(stderr,"       cg_nelement_interpolation_read = %d, should be 2.\n",neinterp);
            fprintf(stderr,"       cg_nsolution_interpolation_read = %d, should be 3.\n",nsinterp);
            cg_error_exit();
        }
    }
    
    /* Try to Modify Nodes */
    {
        printf("Modifying Old Nodes\n");
        
        error = cg_element_interpolation_write(cgfile, cgbase,cgfamily,"QuadInterpolation",
                                               type,&cgeinterp);
        if (error)
        {
            fprintf(stderr,"ERROR CODE %d : Cannot modify an existing ElementInterpolation_t node for the same ElementType_t in MODIFY MODE !\n",error);
            cg_error_exit();
        }
        
        error = cg_solution_interpolation_write(cgfile, cgbase,cgfamily,"4rdOrderQuadSolution_2",
                                            CGNS_ENUMV(QUAD_4),order,0,CGNS_ENUMV(ParametricLagrange),&cgsinterp);
        
        if (error)
        {
            fprintf(stderr,"ERROR CODE %d : Cannot modify an existing SolutionInterpolation_t node for the same ElementType_t in MODIFY MODE !\n",error);
            cg_error_exit();
        }
        
    }
    
    /* Get Node Count */
    {
        if (cg_nelement_interpolation_read(cgfile, cgbase,cgfamily,&neinterp) ||
            cg_nsolution_interpolation_read(cgfile, cgbase,cgfamily,&nsinterp) )
        {
            fprintf(stderr,"ERROR: Impossible to get the interpolation node count.\n");
            cg_error_exit();
        }
        
        /* 2 ElementInterpolation_t (QUAD, TRI); 3 SolutionInterpolation_t
         * (QUAD degree 3, QUAD degree 4, TRI degree 5). */
        if (neinterp != 2 || nsinterp != 3)
        {
            fprintf(stderr,"ERROR: wrong interpolation node count.\n");
            fprintf(stderr,"       cg_nelement_interpolation_read = %d, should be 2.\n",neinterp);
            fprintf(stderr,"       cg_nsolution_interpolation_read = %d, should be 3.\n",nsinterp);
            cg_error_exit();
        }
    }
    
    /* Try to delete Nodes */
    if(1)
    {
        printf("Deleting Old Nodes\n");
      
        cg_goto(cgfile, cgbase, "Family_t", cgfamily, NULL);
        if (cg_delete_node("QuadInterpolation") ) 
        {
            fprintf(stderr,"ERROR: Impossible to delete the node QuadInterpolation !");
            cg_error_exit();
        }
        if (cg_delete_node("4rdOrderQuadSolution_2") ) 
        {
            fprintf(stderr,"ERROR: Impossible to delete the node 4rdOrderQuadSolution !");
            cg_error_exit();
        }
    }
    
    /* Get Node Count */
    {
        if (cg_nelement_interpolation_read(cgfile, cgbase,cgfamily,&neinterp) ||
            cg_nsolution_interpolation_read(cgfile, cgbase,cgfamily,&nsinterp) )
        {
            fprintf(stderr,"ERROR: Impossible to get the interpolation node count.\n");
            cg_error_exit();
        }
        
        /* After deleting QuadInterpolation and the degree-4 quad solution:
         * 1 ElementInterpolation_t (TRI) and 2 SolutionInterpolation_t
         * (QUAD degree 3, TRI degree 5) remain. */
        if (neinterp != 1 || nsinterp != 2)
        {
            fprintf(stderr,"ERROR: wrong interpolation node count.\n");
            fprintf(stderr,"       cg_nelement_interpolation_read = %d, should be 1.\n",neinterp);
            fprintf(stderr,"       cg_nsolution_interpolation_read = %d, should be 2.\n",nsinterp);
            cg_error_exit();
        }
    }
    
    
    
    
    fflush (stdout);
    printf ("closing cgns file ...\n");
    cg_close (cgfile);

    return 0;
}

