/*   Program write_grid_str_paroverzone.c    */
/*
Creates simple 3-D structured grid and writes it to a CGNS file.  Each processor
write data to one zone (parallelism over zones)

mpicxx write_grid_str_paroverzone.c -lcgns -lhdf5 -lsz -lz -o write_grid_str_paroverzone
mpirun -np 2 write_grid_str_paroverzone
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
   const int numZone = 2;
   const cgsize_t zoneSize[2][3][3] = {
     /* zone 1 */
     /* vertices     cells        boundary vertex */
     { {21, 19, 9}, {20, 18, 8}, {0, 0, 0} },
     /* zone 2 */
     /* vertices     cells        boundary vertex */
     { {17, 19, 9}, {16, 18, 8}, {0, 0, 0} } 
   };

   int i, j, k, n, comm_size, comm_rank;
   int index_file, index_base;
   int index_zone, index_grid, index_coordx, index_coordy, index_coordz;
   char basename[33],zonename[33];

   MPI_Init(&argc, (char***)(&argv));
   MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
   MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
   /* number of zones on this processor and global index of the first one. */
   int maxLocalZone;
   int idxGlobalZoneBeg;
   int numLocalZone = numZone/comm_size;
   {
     int numUnevenZone = numZone - numLocalZone*comm_size;
     maxLocalZone = numLocalZone + (numUnevenZone > 0);
     if (comm_rank < numUnevenZone)
       {
         ++numLocalZone;
         idxGlobalZoneBeg = comm_rank*numLocalZone;
       }
     else
       {
         idxGlobalZoneBeg = numUnevenZone*(numLocalZone + 1);
         idxGlobalZoneBeg += (comm_rank - numUnevenZone)*numLocalZone;
       }
   }

   cgp_mpi_comm(MPI_COMM_WORLD);

/* open CGNS file for write */
   if (cgp_open("grid_c.cgns", CG_MODE_WRITE, &index_file)) cg_error_exit();
/* create base (user can give any name) */
   strcpy(basename, "Base");
   int icelldim = 3;
   int iphysdim = 3;
   cg_base_write(index_file, basename, icelldim, iphysdim, &index_base);

/* CREATION OF FILE STRUCTURE -- all processors write the same information */

   int idxZone;
   int idxLocalZone = 0;
   for (idxZone = 0; idxZone != numZone; ++idxZone)
     {
/* define zone name (user can give any name) */
       sprintf(zonename, "Zone %d", idxZone + 1);
/* create zone */
       if (cg_zone_write(index_file, index_base,
                         zonename, (cgsize_t*)zoneSize[idxZone], Structured,
                         &index_zone)) cg_error_exit();
       if (cg_grid_write(index_file, index_base, index_zone,
                         "GridCoordinates", &index_grid)) cg_error_exit();
/* construct the grid coordinates nodes (user must use SIDS-standard names
   here).  No data is written */
       cgp_coord_write(index_file, index_base, index_zone,
                       CGNS_ENUMV(RealSingle), "CoordinateX", &index_coordx);
       cgp_coord_write(index_file, index_base, index_zone,
                       CGNS_ENUMV(RealSingle), "CoordinateY", &index_coordy);
       cgp_coord_write(index_file, index_base, index_zone,
                       CGNS_ENUMV(RealSingle), "CoordinateZ", &index_coordz);
     }

/* COLLECTIVE WRITING OF FILE DATA -- each processor writes to a separate
   zone */

   for (idxLocalZone = 0; idxLocalZone < maxLocalZone; ++idxLocalZone)
     {
       const int idxGlobalZone = idxGlobalZoneBeg + idxLocalZone;
       int s_rmin[3], s_rmax[3], m_dimvals[3], m_rmin[3], m_rmax[3];
       double *x = NULL;
       double *y = NULL;
       double *z = NULL;
       if (idxGlobalZone < numZone)
         {
           /* create gridpoints for simple example: */
           const int ni = zoneSize[idxGlobalZone][0][0];
           const int nj = zoneSize[idxGlobalZone][0][1];
           const int nk = zoneSize[idxGlobalZone][0][2];
           const int num_vertex = ni*nj*nk;
           const double xOffset = idxGlobalZone*zoneSize[0][1][0];
           x = (double*)malloc(3*num_vertex*sizeof(double));
           y = x + num_vertex;
           z = y + num_vertex;
           for (k = 0; k < nk; ++k)
             {
               for (j = 0; j < nj; ++j)
                 {
                   for (i = 0; i < ni; ++i)
                     {
                       int idx = (k*nj + j)*ni + i;
                       x[idx] = i + xOffset;
                       y[idx] = j;
                       z[idx] = k;
                     }
                 }
             }
           // Shape in file space
           for (n = 0; n < 3; ++n)
             {
               s_rmin[n] = 1;
               s_rmax[n] = zoneSize[idxGlobalZone][0][n];
             }
           // Shape in memory
           for (n = 0; n < 3; ++n)
             {
               m_dimvals[n] = zoneSize[idxGlobalZone][0][n];
               m_rmin[n]    = 1;
               m_rmax[n]    = zoneSize[idxGlobalZone][0][n];
             }
         }
       const int cgns_index_zone =
         (idxGlobalZone < numZone) ? idxGlobalZone + 1 : 1;
       cgp_coord_general_write_data(index_file, index_base, cgns_index_zone, 1,
                                    s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                    3, m_dimvals, m_rmin, m_rmax, x);
       cgp_coord_general_write_data(index_file, index_base, cgns_index_zone, 2,
                                    s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                    3, m_dimvals, m_rmin, m_rmax, y);
       cgp_coord_general_write_data(index_file, index_base, cgns_index_zone, 3,
                                    s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                    3, m_dimvals, m_rmin, m_rmax, z);
       if (idxGlobalZone < numZone)
         {
           free(x);
         }
     }
/* close CGNS file */
   cgp_close(index_file);
   MPI_Finalize();
   if (comm_rank == 0)
     {
       printf("\nSuccessfully wrote grid to file grid_c.cgns\n");
     }
   return 0;
}
