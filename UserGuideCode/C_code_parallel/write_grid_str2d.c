/*   Program par_write_grid_str.c    */
/*
Creates simple 3-D structured grid and writes it to a 
CGNS file.

Example compilation for this program is (change paths!):
 
cc -I ../CGNS_CVS/cgnslib_2.5 -c write_grid_str.c
cc -o write_grid_str_c write_grid_str.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns

mpicxx write_grid_str2d.c -lcgns -lhdf5 -lsz -lz -o write_grid_str2d
mpirun -np 2 write_grid_str2d
 
(../CGNS_CVS/cgnslib_2.5/LINUX/ is the location where the compiled
library libcgns.a is located)
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* cgnslib.h file must be located in directory specified by -I during compile: */
#include "pcgnslib.h"
#include "mpi.h"

int main(int argc, const char* argv[])
{
/*
   dimension statements (note that tri-dimensional arrays
   x,y,z must be dimensioned exactly as [N][17][21] (N>=9) 
   for this particular case or else they will be written to 
   the CGNS file incorrectly!  Other options are to use 1-D 
   arrays, use dynamic memory, or pass index values to a 
   subroutine and dimension exactly there):
*/
   const int nZone = 2;
   int nProcZone, idxProcZone;
   int nCelli[nZone];
   nCelli[0] = 20;
   nCelli[1] = 18;
   int nCellj[nZone];
   nCellj[0] = 16;
   nCellj[1] = 16;
   int nVerti[nZone];
   nVerti[0] = nCelli[0] + 1;
   nVerti[1] = nCelli[1] + 1;
   int nVertj[nZone];
   nVertj[0] = nCellj[0] + 1;
   nVertj[1] = nCellj[1] + 1;
   const int nRind  = 1;
   double* x;
   double* y;
   cgsize_t isize[3][2];
   int ni,nj,i,j, numProc, procID;
   int irinddata[4];
   int index_file,icelldim,iphysdim,index_base;
   int index_zone,index_grid,index_coordx,index_coordy;
   int proc_index_zone[nZone];
   int proc_index_coordx[nZone],proc_index_coordy[nZone];
   char basename[33],zonename[33];

   MPI_Init(&argc, (char***)(&argv));
   MPI_Comm_size(MPI_COMM_WORLD, &numProc);
   MPI_Comm_rank(MPI_COMM_WORLD, &procID);
   nProcZone = nZone/numProc;
   nProcZone += (procID < (nZone - nProcZone*numProc));

   cgp_mpi_comm(MPI_COMM_WORLD);

/* open CGNS file for write */
   if (cgp_open("grid_c2d.cgns",CG_MODE_WRITE,&index_file)) cg_error_exit();
/* create base (user can give any name) */
   strcpy(basename,"Base");
   icelldim=2;
   iphysdim=2;
   cg_base_write(index_file,basename,icelldim,iphysdim,&index_base);

/* COLLECTIVE CREATION OF FILE STRUCTURE -- all processors write the same
 * information */

   int idxZone;
   idxProcZone = 0;
   for (idxZone = 0; idxZone != nZone; ++idxZone)
     {
/* define zone name (user can give any name) */
       sprintf(zonename, "Zone %d", idxZone + 1);
       printf("proc %d is writing meta-data for %s\n", procID, zonename);
       fflush(stdout);
/* vertex size */
       isize[0][0]=nVerti[idxZone];
       isize[0][1]=nVertj[idxZone];
/* cell size */
       isize[1][0]=nCelli[idxZone];
       isize[1][1]=nCellj[idxZone];
/* boundary vertex size (always zero for structured grids) */
       isize[2][0]=0;
       isize[2][1]=0;
/* create zone */
       cg_zone_write(index_file,index_base,
                     zonename,*isize,Structured,&index_zone);
       cg_grid_write(index_file,index_base,index_zone,
                     "GridCoordinates",&index_grid);
/* go to position within tree at GridCoordinates_t node */
       /* printf("\nOther %d %d\n", index_base, index_zone); */
       int ier;
       ier = cg_goto(index_file,index_base,"Zone_t",index_zone,
                     "GridCoordinates_t",index_grid,"end");
       /* printf("Goto status: %d\n", ier); */
       /* cg_error_print(); */
/* write rind information under GridCoordinates_t node
 * (ilo,ihi,jlo,jhi,klo,khi) */
       irinddata[0]=nRind;
       irinddata[1]=nRind;
       irinddata[2]=nRind;
       irinddata[3]=nRind;
       ier = cg_rind_write(irinddata);
       /* printf("Rind status: %d\n", ier); */
       /* cg_error_print(); */
/* collectivelly construct the grid coordinates nodes (user must use
   SIDS-standard names here) */
       cgp_coord_write(index_file,index_base,index_zone,
                       RealDouble,"CoordinateX",&index_coordx);
       cgp_coord_write(index_file,index_base,index_zone,
                       RealDouble,"CoordinateY",&index_coordy);
       if (nProcZone > 0 && (idxZone/nProcZone) == procID)
         {
           proc_index_zone[idxProcZone] = index_zone;
           proc_index_coordx[idxProcZone] = index_coordx;
           proc_index_coordy[idxProcZone] = index_coordy;
           ++idxProcZone;
         }
     }

/* COLLECTIVE WRITING OF FILE DATA -- each processor writes to a separate
   zone */

   for (idxProcZone = 0; idxProcZone < nProcZone; ++idxProcZone)
     {
       int idxGlobalZone = proc_index_zone[idxProcZone] - 1;
       /* create gridpoints for simple example: */
       ni=nVerti[idxGlobalZone] + 2*nRind;
       nj=nVertj[idxGlobalZone] + 2*nRind;
       x = (double*)malloc(ni*nj*sizeof(double));
       y = (double*)malloc(ni*nj*sizeof(double));
       double xOffset = idxGlobalZone*nCelli[0] - nRind;
       double yOffset = -nRind;
       for (j=0; j < nj; ++j)
         {
           for (i=0; i < ni; ++i)
             {
               x[j*ni + i] = i + xOffset;
               y[j*ni + i] = j + yOffset;
             }
         }
       cgsize_t irmin[2];
       irmin[0] = 1;
       irmin[1] = 1;
       cgsize_t irmax[2];
       irmax[0] = ni;
       irmax[1] = nj;
       printf("proc %d is writing data for Zone %d\n",
              procID, idxGlobalZone + 1);
       fflush(stdout);
       cgp_coord_write_data(index_file,index_base,proc_index_zone[idxProcZone],
                            proc_index_coordx[idxProcZone], irmin, irmax, x);
       cgp_coord_write_data(index_file,index_base,proc_index_zone[idxProcZone],
                            proc_index_coordy[idxProcZone], irmin, irmax, y);
       free(y);
       free(x);
     }
/* close CGNS file */
   cgp_close(index_file);
   MPI_Finalize();
   if (procID == 0)
     {
       printf("\nSuccessfully wrote grid to file grid_c2d.cgns\n");
     }
   return 0;
}
