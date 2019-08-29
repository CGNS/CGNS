#include <stdio.h>
#include <stdlib.h>
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
    int error, i, j, ii, jj, n, ni, nj, ifirstnode, ielem_no, nbdyelem;
    double *x;
    double *y;
    double *pu, *pv, *puu, *pvv;
    cgsize_t *ielem;
    cgsize_t size[9],sizeread[9];
    cgsize_t nelem_start,nelem_end;
    CGNS_ENUMT(ElementType_t) type, etyperead;
    int cgfile, cgbase, cgzone, cgsection, cgfamily, cgsol, cgcoord, cgeinterp, cgsinterp;
    char einterpName[33],sinterpname[33],zonename[33],familyname[33],sectionname[33],solname[33];
    int neinterp,nsinterp;
    
    
    int order = 2;
    type = QUAD_9;
    
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
    
    // Fill (U,V)
    fillQuadLagrangePoints(order,pu,pv);
    
    printf ("Writing cgns file high_order.cgns ...\n");
    if (cg_open ("high_order.cgns", CG_MODE_WRITE, &cgfile) ||
        cg_base_write (cgfile, "Base", 3, 3, &cgbase) ||
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
        fprintf(stderr,"ERROR: an error occured during section writing !\n");
        cg_error_exit();
    }
    
    free(ielem); free(x); free(y);
    
    printf ("Writing family  ...\n");
    error = cg_family_write(cgfile, cgbase,"family",&cgfamily);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occured during family writing !\n");
        cg_error_exit();
    }
    
    
    printf ("Writing ElementInterpolation_t node  ...\n");
    
    error = cg_element_interpolation_write(cgfile, cgbase,cgfamily,"QuadInterpolation",type,&cgeinterp);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occured during Mesh element interpolation writing !\n");
        cg_error_exit();
    }
    error = cg_element_interpolation_points_write(cgfile, cgbase,cgfamily,cgeinterp,pu,pv,NULL);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occured during Mesh Lagrange points writing !\n");
        cg_error_exit();
    }
    
    /* Test if possible to modify an existing ElementInterpolation_t for the same Element Type --> HAS TO FAIL IN WRITE MODE ! */
    {
        error = cg_element_interpolation_write(cgfile, cgbase,cgfamily,"QuadInterpolation",
                                               type,&n);
        if (error != CG_ERROR)
        {
            fprintf(stderr,"ERROR CODE %d : Should not be able to modify an existing ElementInterpolation_t node for the same ElementType_t in WRITE MODE !\n",error);
            cg_error_exit();
        }
    }
    
    /* Test if possible to add another ElementInterpolation_t for the same Element Type --> HAS TO FAIL ! */
    {
        error = cg_element_interpolation_write(cgfile, cgbase,cgfamily,"QuadInterpolation_2",
                                               type,&n);
        if (error != CG_ERROR)
        {
            fprintf(stderr,"ERROR CODE %d : Should Not be able to write 2 ElementInterpolation_t node for the same ElementType_t. An error should occurs !\n",error);
            cg_error_exit();
        }
    }
    
    printf ("Writing SolutionInterpolation_t node  ...\n");
    
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
        fprintf(stderr,"ERROR: an error occured during Solution interpolation writing !\n");
        cg_error_exit();
    }
    error = cg_solution_interpolation_points_write(cgfile, cgbase,cgfamily,cgsinterp,
                                                   pu,pv,NULL,NULL);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occured during Solution Lagrange interpolation points writing !\n");
        cg_error_exit();
    }
    
    /* Test if possible to add another SolutionInterpolation_t for the same (ElementType,sorder,torder) --> HAS TO FAIL ! */
    {
        error = cg_solution_interpolation_write(cgfile, cgbase,cgfamily,"4rdOrderQuadSolution_2",
                                            CGNS_ENUMV(QUAD_4),order,0,CGNS_ENUMV(ParametricLagrange),&n);
        
        if (error != CG_ERROR)
        {
            fprintf(stderr,"Should Not be able to write 2 SolutionInterpolation_t node for the same ElementType_t. An error should occurs !");
            cg_error_exit();
        }
    }
    
    /* Get Node Count */
    if (cg_nelement_interpolation_read(cgfile, cgbase,cgfamily,&neinterp) ||
        cg_nsolution_interpolation_read(cgfile, cgbase,cgfamily,&nsinterp) )
    {
        fprintf(stderr,"ERROR: Impossible to get the interpolation node count.\n");
        cg_error_exit();
    }
    
    if (neinterp != 1 || nsinterp != 1)
    {
        fprintf(stderr,"ERROR: wrong interpolation node count.\n");
        fprintf(stderr,"       cg_nelement_interpolation_read = %d, should be 1.\n",neinterp);
        fprintf(stderr,"       cg_nsolution_interpolation_read = %d, should be 1.\n",nsinterp);
        cg_error_exit();
    }
    
    
    free(pu); free(pv);
    
    fflush (stdout);
    printf ("closing cgns file ...\n");
    cg_close (cgfile);
    
    /* ************************************************************************** 
     * 
     *                             READ BACK
     * 
     * ************************************************************************** */
    
    printf ("reading cgns file high_order.cgns in READ mode ...\n");
    if (cg_open ("high_order.cgns", CG_MODE_MODIFY, &cgfile) ) cg_error_exit();
    
    // Get Informations from Current Zone
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
    cg_element_lagrange_interpolation_count(cgfile, cgbase,cgfamily,type,&n);
    
    if (n != 1)
    {
        fprintf(stderr,"ERROR: Wrong Number of ElementInterpolation_t node of type %s !\n",
                cg_ElementTypeName(type));
        cg_error_exit();
    }
    
    /* Read Element interpolation Node */
    if (cg_element_interpolation_read(cgfile, cgbase,cgfamily,cgeinterp,einterpName,&etyperead)) 
    {
        fprintf(stderr,"ERROR: Cannot Read The ElementInterpolation_t node !\n",error);
        cg_error_exit();
    }
    
    /* Check Element interpolation Node */
    if (strcmp(einterpName,"QuadInterpolation"))
    {
        fprintf(stderr,"ERROR: Wrong Element Interpolation Name !\n");
        cg_error_exit();
    }
    
    if (etyperead != type)
    {
        fprintf(stderr,"ERROR: Wrong Element Interpolation Type !\n");
        cg_error_exit();
    }
    
    error = cg_element_lagrange_interpolation_size(etyperead,&n);
    
    if (n != 9)
    {
        fprintf(stderr,"ERROR: cg_element_lagrange_interpolation_size returns wrong number !\n");
        cg_error_exit();
    }
    
    pu = (double*) malloc( (size_t) (n) * sizeof(double));
    pv = (double*) malloc( (size_t) (n) * sizeof(double));
    puu = (double*) malloc( (size_t) (n) * sizeof(double));
    pvv = (double*) malloc( (size_t) (n) * sizeof(double));
    
    // Fill (U,V)
    fillQuadLagrangePoints(2,pu,pv);
    
    /* Read Element interpolation Points Node */
    if (cg_element_interpolation_points_read(cgfile, cgbase,cgfamily,cgeinterp,puu,pvv,NULL))
    {
        fprintf(stderr,"ERROR CODE %d : Cannot Read The Element Interpolation points !\n",error);
        cg_error_exit();
    }
    /* Check UV Points */
    for(i = 0 ; i < n; i++)
    {
        if ( fabs(pu[i] - puu[i]) > 1.e-06 || fabs(pv[i] - pvv[i]) > 1.e-06 )
        {
            fprintf(stderr,"ERROR: Element Interpolation points are not in tolerance !\n");
            fprintf(stderr,"  given at indice i=%d : (%f,%f) --> required (%f,%f)\n",i,
                    puu[i],pvv[i],pu[i],pv[i]);
            fprintf(stderr,"errors : (%e,%e)\n",fabs(pu[i] - puu[i]),fabs(pv[i] - pvv[i]));
            cg_error_exit();
        }
    }
    free(puu);
    free(pvv);
    free(pu);
    free(pv);
    
    
    
    
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
                                            TRI_9,&cgeinterp);
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
        
        if (neinterp != 2 || nsinterp != 2)
        {
            fprintf(stderr,"ERROR: wrong interpolation node count.\n");
            fprintf(stderr,"       cg_nelement_interpolation_read = %d, should be 2.\n",neinterp);
            fprintf(stderr,"       cg_nsolution_interpolation_read = %d, should be 2.\n",nsinterp);
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
        
        if (neinterp != 2 || nsinterp != 2)
        {
            fprintf(stderr,"ERROR: wrong interpolation node count.\n");
            fprintf(stderr,"       cg_nelement_interpolation_read = %d, should be 2.\n",neinterp);
            fprintf(stderr,"       cg_nsolution_interpolation_read = %d, should be 2.\n",nsinterp);
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
        
        if (neinterp != 1 || nsinterp != 1)
        {
            fprintf(stderr,"ERROR: wrong interpolation node count.\n");
            fprintf(stderr,"       cg_nelement_interpolation_read = %d, should be 1.\n",neinterp);
            fprintf(stderr,"       cg_nsolution_interpolation_read = %d, should be 1.\n",nsinterp);
            cg_error_exit();
        }
    }
    
    
    
    
    fflush (stdout);
    printf ("closing cgns file ...\n");
    cg_close (cgfile);

    return 0;
}

