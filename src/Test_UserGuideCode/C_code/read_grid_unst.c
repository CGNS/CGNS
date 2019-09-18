/*   Program read_grid_unst   */
/*
Reads simple 3-D unstructured grid from a CGNS file
(created using write_grid_unst.c).

The CGNS grid file 'grid_c.cgns' must already exist.

Example compilation for this program is (change paths if needed!):

cc -I ../.. -c read_grid_unst.c
cc -o read_grid_unst_c read_grid_unst.o -L ../../lib -lcgns

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
    float x[21*17*9],y[21*17*9],z[21*17*9];
    cgsize_t isize[3][1],ielem[20*16*8][8];
    int index_file,index_base,index_zone;
    cgsize_t irmin,irmax,istart,iend;
    int nsections,index_sect,nbndry,iparent_flag;
    cgsize_t iparentdata;
    char zonename[33],sectionname[33];
    CGNS_ENUMT(ElementType_t) itype;

/* READ X, Y, Z GRID POINTS FROM CGNS FILE */
/* open CGNS file for read-only */
    if (cg_open("grid_c.cgns",CG_MODE_READ,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;
/* we know there is only one zone (real working code would check!) */
    index_zone=1;
/* get zone size (and name - although not needed here) */
    cg_zone_read(index_file,index_base,index_zone,zonename,isize[0]);
/* lower range index */
    irmin=1;
/* upper range index of vertices */
    irmax=isize[0][0];
/* read grid coordinates */
    cg_coord_read(index_file,index_base,index_zone,"CoordinateX",
                  CGNS_ENUMV(RealSingle),&irmin,&irmax,x);
    cg_coord_read(index_file,index_base,index_zone,"CoordinateY",
                  CGNS_ENUMV(RealSingle),&irmin,&irmax,y);
    cg_coord_read(index_file,index_base,index_zone,"CoordinateZ",
                  CGNS_ENUMV(RealSingle),&irmin,&irmax,z);
/* find out how many sections */
    cg_nsections(index_file,index_base,index_zone,&nsections);
    printf("\nnumber of sections=%i\n",nsections);
/* read element connectivity */
    for (index_sect=1; index_sect <= nsections; index_sect++)
    {
      cg_section_read(index_file,index_base,index_zone,index_sect,sectionname,
                      &itype,&istart,&iend,&nbndry,&iparent_flag);
      printf("\nReading section data...\n");
      printf("   section name=%s\n",sectionname);
      printf("   section type=%s\n",ElementTypeName[itype]);
      printf("   istart,iend=%i, %i\n",(int)istart,(int)iend);
      if (itype == CGNS_ENUMV(HEXA_8))
      {
        printf("   reading element data for this element\n");
        cg_elements_read(index_file,index_base,index_zone,index_sect,ielem[0], \
                         &iparentdata);
      }
      else
      {
        printf("   not reading element data for this element\n");
      }
    }
/* close CGNS file */
    cg_close(index_file);
    printf("\nSuccessfully read unstructured grid from file grid_c.cgns\n");
    printf("   for example, element 1 is made up of nodes: %i, %i, %i, %i, %i, %i, %i, %i\n",
            (int)ielem[0][0],(int)ielem[0][1],(int)ielem[0][2],(int)ielem[0][3],
            (int)ielem[0][4],(int)ielem[0][5],(int)ielem[0][6],(int)ielem[0][7]);
    printf("   x,y,z of node 357 are: %f, %f, %f\n",x[357],y[357],z[357]);
    printf("   x,y,z of node 1357 are: %f, %f, %f\n",x[1357],y[1357],z[1357]);
    return 0;
}
