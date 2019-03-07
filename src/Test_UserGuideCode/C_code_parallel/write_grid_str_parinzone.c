/*   Program write_grid_str_parinzone.c    */
/*
Creates simple 3-D structured grid and writes it to a CGNS file.  Each processor
writes a slab of data to one zone (parallelism within a zone)

mpicxx write_grid_str_parinzone.c -lcgns -lhdf5 -lsz -lz -o write_grid_str_parinzone
mpirun -np 2 write_grid_str_parinzone
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pcgnslib.h"
#include "mpi.h"

int main(int argc, const char* argv[])
{
/* dimension statements for the zone are given here */
   const cgsize_t zoneSize[3][3] =
     /* vertices     cells        boundary vertex */
     { {21, 19, 9}, {20, 18, 8}, {0, 0, 0} };

   int i, j, k, n, comm_size, comm_rank;
   int index_file, index_base;
   int index_zone, index_grid, index_coordx, index_coordy, index_coordz;
   char basename[33], zonename[33];

   MPI_Init(&argc, (char***)(&argv));
   MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
   MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
   cgp_mpi_comm(MPI_COMM_WORLD);

/* open CGNS file for write */
   if (cgp_open("grid_piz_c.cgns", CG_MODE_WRITE, &index_file)) cg_error_exit();
/* create base (user can give any name) */
   strcpy(basename, "Base");
   int icelldim = 3;
   int iphysdim = 3;
   cg_base_write(index_file, basename, icelldim, iphysdim, &index_base);

/* CREATION OF FILE STRUCTURE -- all processors write the same information.
   Only zone meta-data is written to the library at this stage */

/* define zone name (user can give any name) */
   sprintf(zonename, "Zone %d", 1);
/* create zone */
   if (cg_zone_write(index_file, index_base, zonename, (cgsize_t*)zoneSize,
                     CGNS_ENUMV(Structured), &index_zone)) cg_error_exit();
   if (cg_grid_write(index_file, index_base, index_zone, "GridCoordinates",
                     &index_grid)) cg_error_exit();
/* construct the grid coordinates nodes (user must use SIDS-standard names
   here) */
   if (cgp_coord_write(index_file, index_base, index_zone,
                       CGNS_ENUMV(RealSingle), "CoordinateX",
                       &index_coordx)) cgp_error_exit();
   if (cgp_coord_write(index_file, index_base, index_zone,
                       CGNS_ENUMV(RealSingle), "CoordinateY",
                       &index_coordy)) cgp_error_exit();
   if (cgp_coord_write(index_file, index_base, index_zone,
                       CGNS_ENUMV(RealSingle), "CoordinateZ",
                       &index_coordz)) cgp_error_exit();

/* COLLECTIVE WRITING OF FILE DATA -- each processor writes a part of the
   zone */

   /* partition the k-index of the zone among the processes */
   int kIdxBeg;  /* beginning k-index for this processor */
   int numLocalkIdx = zoneSize[0][2]/comm_size;  /* the number of k-indices
                                                    local to this process */
   {
     int numUnevenkIdx = zoneSize[0][2] - numLocalkIdx*comm_size;
     if (comm_rank < numUnevenkIdx)
       {
         ++numLocalkIdx;
         kIdxBeg = comm_rank*numLocalkIdx;
       }
     else
       {
         kIdxBeg = numUnevenkIdx*(numLocalkIdx + 1);
         kIdxBeg += (comm_rank - numUnevenkIdx)*numLocalkIdx;
       }
   }

   cgsize_t s_rmin[3], s_rmax[3], m_dimvals[3], m_rmin[3], m_rmax[3];
   double *x = NULL;
   double *y = NULL;
   double *z = NULL;
   if (numLocalkIdx > 0)
     {
       /* create gridpoints for simple example: */
       const int ni = zoneSize[0][0];
       const int nj = zoneSize[0][1];
       const int nk = numLocalkIdx;
       const int num_vertex = ni*nj*nk;
       x = (double*)malloc(3*num_vertex*sizeof(double));
       y = x + num_vertex;
       z = y + num_vertex;
       for (k = kIdxBeg; k < kIdxBeg + numLocalkIdx; ++k)
         {
           for (j = 0; j < nj; ++j)
             {
               for (i = 0; i < ni; ++i)
                 {
                   int idx = ((k - kIdxBeg)*nj + j)*ni + i;
                   x[idx] = i;
                   y[idx] = j;
                   z[idx] = k;
                 }
             }
         }
       // shape in file space
       for (n = 0; n < 2; ++n)
         {
           s_rmin[n] = 1;
           s_rmax[n] = zoneSize[0][n];
         }
       s_rmin[2] = kIdxBeg + 1;
       s_rmax[2] = kIdxBeg + numLocalkIdx;
       // shape in memory
       for (n = 0; n < 2; ++n)
         {
           m_dimvals[n] = zoneSize[0][n];
           m_rmin[n]    = 1;
           m_rmax[n]    = zoneSize[0][n];
         }
       m_dimvals[2] = numLocalkIdx;
       m_rmin[2]    = 1;
       m_rmax[2]    = numLocalkIdx;
     }
   /* if there is nothing for this process to write, a valid zone index must be
      provided and data array = NULL */
   /* the data is defined as double but will be written as single */
   if (cgp_coord_general_write_data(index_file, index_base, index_zone, 1,
                                    s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                    3, m_dimvals, m_rmin, m_rmax,
                                    x)) cgp_error_exit();
   if (cgp_coord_general_write_data(index_file, index_base, index_zone, 2,
                                    s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                    3, m_dimvals, m_rmin, m_rmax,
                                    y)) cgp_error_exit();
   if (cgp_coord_general_write_data(index_file, index_base, index_zone, 3,
                                    s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                    3, m_dimvals, m_rmin, m_rmax,
                                    z)) cgp_error_exit();
   if (numLocalkIdx > 0)
     {
       free(x);
     }
/* close CGNS file */
   cgp_close(index_file);
   MPI_Finalize();
   if (comm_rank == 0)
     {
       printf("\nSuccessfully wrote grid to file grid_piz_c.cgns\n");
     }
   return 0;
}
