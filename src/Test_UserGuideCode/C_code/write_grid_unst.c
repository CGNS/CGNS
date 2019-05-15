/*    Program write_grid_unst   */
/*
Creates simple 3-D unstructured grid and writes it to a
CGNS file.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c write_grid_unst.c
cc -o write_grid_unst_c write_grid_unst.o -L ../../lib -lcgns

(../../lib is the location where the compiled
library libcgns.a is located)
*/

#include <stdio.h>
#include <string.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "cgnslib.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#endif

#define maxelemi 20*16*8
#define maxelemj 1216

int main()
{
    double x[21*17*9],y[21*17*9],z[21*17*9];
    cgsize_t isize[3][1],ielem[maxelemi][8],jelem[maxelemj][4];
    cgsize_t nelem_start,nelem_end;
    int ni,nj,nk,iset,i,j,k,index_file,icelldim,iphysdim;
    int index_base,index_zone,index_coord,ielem_no;
    int ifirstnode,nbdyelem,index_section;
    char basename[33],zonename[33];

    printf("\nProgram write_grid_unst\n");

/* create gridpoints for simple example: */
    ni=21;
    nj=17;
    nk=9;
    iset=0;
    for (k=1; k <= nk; k++)
    {
      for (j=1; j <=nj; j++)
      {
        for (i=1; i <= ni; i++)
        {
          x[iset]=(float)i-1.;
          y[iset]=(float)j-1.;
          z[iset]=(float)k-1.;
          iset=iset+1;
        }
      }
    }
    printf("\ncreated simple 3-D grid points\n");

/* WRITE X, Y, Z GRID POINTS TO CGNS FILE */
/* open CGNS file for write */
    if (cg_open("grid_c.cgns",CG_MODE_WRITE,&index_file)) cg_error_exit();
/* create base (user can give any name) */
    strcpy(basename,"Base");
    icelldim=3;
    iphysdim=3;
    cg_base_write(index_file,basename,icelldim,iphysdim,&index_base);
/* define zone name (user can give any name) */
    strcpy(zonename,"Zone  1");
/* vertex size */
    isize[0][0]=ni*nj*nk;
/* cell size */
    isize[1][0]=(ni-1)*(nj-1)*(nk-1);
/* boundary vertex size (zero if elements not sorted) */
    isize[2][0]=0;
/* create zone */
    cg_zone_write(index_file,index_base,zonename,isize[0],CGNS_ENUMV(Unstructured),&index_zone);
/* write grid coordinates (user must use SIDS-standard names here) */
    cg_coord_write(index_file,index_base,index_zone,CGNS_ENUMV(RealDouble),"CoordinateX",
        x,&index_coord);
    cg_coord_write(index_file,index_base,index_zone,CGNS_ENUMV(RealDouble),"CoordinateY",
        y,&index_coord);
    cg_coord_write(index_file,index_base,index_zone,CGNS_ENUMV(RealDouble),"CoordinateZ",
        z,&index_coord);
/* set element connectivity: */
/* ---------------------------------------------------------- */
/* do all the HEXA_8 elements (this part is mandatory): */
/* maintain SIDS-standard ordering */
    ielem_no=0;
/* index no of first element */
    nelem_start=1;
    for (k=1; k < nk; k++)
    {
      for (j=1; j < nj; j++)
      {
        for (i=1; i < ni; i++)
        {
/*
in this example, due to the order in the node numbering, the
hexahedral elements can be reconstructed using the following
relationships:
*/
          ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
          ielem[ielem_no][0]=ifirstnode;
          ielem[ielem_no][1]=ifirstnode+1;
          ielem[ielem_no][2]=ifirstnode+1+ni;
          ielem[ielem_no][3]=ifirstnode+ni;
          ielem[ielem_no][4]=ifirstnode+ni*nj;
          ielem[ielem_no][5]=ifirstnode+ni*nj+1;
          ielem[ielem_no][6]=ifirstnode+ni*nj+1+ni;
          ielem[ielem_no][7]=ifirstnode+ni*nj+ni;
          ielem_no=ielem_no+1;
        }
      }
    }
/* index no of last element (=2560) */
    nelem_end=ielem_no;
    if (nelem_end > maxelemi)
    {
      printf("\nError, must increase maxelemi to at least %lu\n",(unsigned long)nelem_end);
      return 1;
    }
/* unsorted boundary elements */
    nbdyelem=0;
/* write CGNS_ENUMV(HEXA_8) element connectivity (user can give any name) */
    cg_section_write(index_file,index_base,index_zone,"Elem",CGNS_ENUMV(HEXA_8),nelem_start,
                     nelem_end,nbdyelem,ielem[0],&index_section);
/* ---------------------------------------------------------- */
/*
do boundary (QUAD) elements (this part is optional,
but you must do it if you eventually want to define BCs
at element faces rather than at nodes):
maintain SIDS-standard ordering
*/
/* INFLOW: */
    ielem_no=0;
/* index no of first element */
    nelem_start=nelem_end+1;
    i=1;
    for (k=1; k < nk; k++)
    {
      for (j=1; j < nj; j++)
      {
        ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
        jelem[ielem_no][0]=ifirstnode;
        jelem[ielem_no][1]=ifirstnode+ni*nj;
        jelem[ielem_no][2]=ifirstnode+ni*nj+ni;
        jelem[ielem_no][3]=ifirstnode+ni;
        ielem_no=ielem_no+1;
      }
    }
/* index no of last element */
    nelem_end=nelem_start+ielem_no-1;
    if (ielem_no > maxelemj)
    {
      printf("\nError, must increase maxelemj to at least %d\n",ielem_no);
      return 1;
    }
/* write QUAD element connectivity for inflow face (user can give any name) */
    cg_section_write(index_file,index_base,index_zone,"InflowElem",CGNS_ENUMV(QUAD_4),nelem_start,
                     nelem_end,nbdyelem,jelem[0],&index_section);
/* OUTFLOW: */
    ielem_no=0;
/* index no of first element */
    nelem_start=nelem_end+1;
    i=ni-1;
    for (k=1; k < nk; k++)
    {
      for (j=1; j < nj; j++)
      {
        ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
        jelem[ielem_no][0]=ifirstnode+1;
        jelem[ielem_no][1]=ifirstnode+1+ni;
        jelem[ielem_no][2]=ifirstnode+ni*nj+1+ni;
        jelem[ielem_no][3]=ifirstnode+ni*nj+1;
        ielem_no=ielem_no+1;
      }
    }
/* index no of last element */
    nelem_end=nelem_start+ielem_no-1;
    if (ielem_no > maxelemj)
    {
      printf("\nError, must increase maxelemj to at least %d\n",ielem_no);
      return 1;
    }
/* write QUAD element connectivity for outflow face (user can give any name) */
    cg_section_write(index_file,index_base,index_zone,"OutflowElem",CGNS_ENUMV(QUAD_4),nelem_start,
                     nelem_end,nbdyelem,jelem[0],&index_section);
/* SIDEWALLS: */
    ielem_no=0;
/* index no of first element */
    nelem_start=nelem_end+1;
    j=1;
    for (k=1; k < nk; k++)
    {
      for (i=1; i < ni; i++)
      {
        ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
        jelem[ielem_no][0]=ifirstnode;
        jelem[ielem_no][1]=ifirstnode+ni*nj;
        jelem[ielem_no][2]=ifirstnode+ni*nj+1;
        jelem[ielem_no][3]=ifirstnode+1;
        ielem_no=ielem_no+1;
      }
    }
    j=nj-1;
    for (k=1; k < nk; k++)
    {
      for (i=1; i < ni; i++)
      {
        ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
        jelem[ielem_no][0]=ifirstnode+1+ni;
        jelem[ielem_no][1]=ifirstnode+ni;
        jelem[ielem_no][2]=ifirstnode+ni*nj+ni;
        jelem[ielem_no][3]=ifirstnode+ni*nj+1+ni;
        ielem_no=ielem_no+1;
      }
    }
    k=1;
    for (j=1; j < nj; j++)
    {
      for (i=1; i < ni; i++)
      {
        ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
        jelem[ielem_no][0]=ifirstnode;
        jelem[ielem_no][1]=ifirstnode+1;
        jelem[ielem_no][2]=ifirstnode+1+ni;
        jelem[ielem_no][3]=ifirstnode+ni;
        ielem_no=ielem_no+1;
      }
    }
    k=nk-1;
    for (j=1; j < nj; j++)
    {
      for (i=1; i < ni; i++)
      {
        ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
        jelem[ielem_no][0]=ifirstnode+ni*nj;
        jelem[ielem_no][1]=ifirstnode+ni*nj+ni;
        jelem[ielem_no][2]=ifirstnode+ni*nj+1+ni;
        jelem[ielem_no][3]=ifirstnode+ni*nj+1;
        ielem_no=ielem_no+1;
      }
    }
/* index no of last element */
    nelem_end=nelem_start+ielem_no-1;
    if (ielem_no > maxelemj)
    {
      printf("\nError, must increase maxelemj to at least %d\n",ielem_no);
      return 1;
    }
/* write QUAD element connectivity for sidewall face (user can give any name) */
    cg_section_write(index_file,index_base,index_zone,"SidewallElem",CGNS_ENUMV(QUAD_4),nelem_start,
                     nelem_end,nbdyelem,jelem[0],&index_section);
/* ---------------------------------------------------------- */
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully wrote unstructured grid to file grid_c.cgns\n");
    return 0;
}
