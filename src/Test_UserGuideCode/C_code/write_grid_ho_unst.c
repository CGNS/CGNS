/*    Program write_grid_unst   */
/*
Creates simple 3-D HO unstructured grid and writes it to a
CGNS file.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_grid_ho_unst.c
cc -o write_grid_ho_unst write_grid_ho_unst.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#endif

int main()
{
    int error;
    double *x;
    double *y;
    double *pu, *pv;
    cgsize_t isize[3][1],*ielem;
    cgsize_t nelem_start,nelem_end;
    int order,ncellI,ncellJ,ni,nj,iset,i,j,ii,jj,index_file,icelldim,iphysdim;
    int index_base,index_interpolation,index_family,index_zone,index_coord,ielem_no;
    int index_solutionInterp;
    int ifirstnode,nbdyelem,index_section;
    char basename[33],zonename[33];
    CGNS_ENUMT(ElementType_t) type;

    printf("\nProgram write_grid_ho_unst\n");

/* Define Sizes */
    order = 2;
    switch(order)
    {
      case 1:type = CGNS_ENUMV(QUAD_4);break;
      case 2:type = CGNS_ENUMV(QUAD_9);break;
      case 3:type = CGNS_ENUMV(QUAD_16);break;
      case 4:type = CGNS_ENUMV(QUAD_25);break;
      default:
      {
          fprintf(stderr,"ERROR: order has to be 0 < order < 5. %d given",order);
          return -1;
      }
    }
    ncellI = 4;
    ncellJ = 5;
    ni=ncellI*order+1;
    nj=ncellJ*order+1;
    
    x = (double*) malloc( (size_t) (ni*nj) * sizeof(double));
    y = (double*) malloc( (size_t) (ni*nj) * sizeof(double));
    pu = (double*) malloc( (size_t) (order+1)*(order+1) * sizeof(double));
    pv = (double*) malloc( (size_t) (order+1)*(order+1) * sizeof(double));
    ielem = (cgsize_t*) malloc( (order+1)*(order+1)*(ncellI*ncellJ)*sizeof(cgsize_t));
    
/* Initialise the element's Lagrange Points coordinates*/
    iset = 0;
    for (jj = 0 ; jj < order+1 ; jj++)
      for (ii = 0 ; ii < order+1 ; ii++)
      {
        pu[iset] = -1.0 + ii*2./(order);
        pv[iset] = -1.0 + jj*2./(order);
        iset++;
      }
    
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
    printf("\ncreated simple 2-D HO grid points (order %d)\n",order);

/* WRITE X, Y, Z GRID POINTS TO CGNS FILE */
/* open CGNS file for write */
    if (cg_open("grid_ho_c.cgns",CG_MODE_WRITE,&index_file)) cg_error_exit();
/* create base (user can give any name) */
    strcpy(basename,"Base");
    icelldim=2;
    iphysdim=2;
    cg_base_write(index_file,basename,icelldim,iphysdim,&index_base);
/* define zone name (user can give any name) */
    strcpy(zonename,"Zone  1");
/* vertex size */
    isize[0][0]=ni*nj;
/* cell size */
    isize[1][0]=ncellI*ncellJ;
/* boundary vertex size (zero if elements not sorted) */
    isize[2][0]=0;
/* create zone */
    cg_zone_write(index_file,index_base,zonename,isize[0],CGNS_ENUMV(Unstructured),&index_zone);
/* write grid coordinates (user must use SIDS-standard names here) */
    cg_coord_write(index_file,index_base,index_zone,CGNS_ENUMV(RealDouble),"CoordinateX",
        x,&index_coord);
    cg_coord_write(index_file,index_base,index_zone,CGNS_ENUMV(RealDouble),"CoordinateY",
        y,&index_coord);
/* set element connectivity: */
/* ---------------------------------------------------------- */
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
    error = cg_section_write(index_file,index_base,index_zone,"Domain",type,nelem_start,
                             nelem_end,nbdyelem,ielem,&index_section);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occured during section writing !");
        return -1;
    }
/* set Family: */
/* ---------------------------------------------------------- */
    error = cg_family_write(index_file,index_base,"family",&index_family);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occured during family writing !");
        return -1;
    }
    
/* set FamilyName for the Zone (not mandatory) */
    cg_goto(index_file,index_base, "Zone_t", index_zone, NULL);
    cg_famname_write ("family");
    
/* set ElementInterpolation Node: */
/* ---------------------------------------------------------- */
    error = cg_element_interpolation_write(index_file,index_base,index_family,"QuadInterpolation",type,&index_interpolation);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occured during Mesh element interpolation writing !");
        return -1;
    }
    error = cg_element_interpolation_points_write(index_file,index_base,index_family,index_interpolation,pu,pv,NULL);
    if (error)
    {
        fprintf(stderr,"ERROR: an error occured during Mesh Lagrange points writing !");
        return -1;
    }
/* ---------------------------------------------------------- */
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully wrote unstructured grid to file grid_c.cgns\n");
    
    free(x);
    free(y);
    free(pu);
    free(pv);
    free(ielem);
    return 0;
}
