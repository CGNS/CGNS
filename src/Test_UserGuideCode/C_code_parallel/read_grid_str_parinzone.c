/*   Program read_grid_str_parinzone.c    */
/*
Reads a simple 3-D structured grid from a CGNS file.  Each processor reads a
slab of data from one zone (parallelism within a zone).

The CGNS grid file 'grid_piz_c.cgns' must already exist.

mpicxx read_grid_str_parinzone.c -lcgns -lhdf5 -lsz -lz -o read_grid_str_parinzone
mpirun -np 2 write_grid_str_parinzone
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pcgnslib.h"
#include "mpi.h"

int main(int argc, const char* argv[])
{
   cgsize_t zoneSize[3][3];
   int n, comm_size, comm_rank;
   int index_file, index_base, index_zone;
   char zonename[33];

   MPI_Init(&argc, (char***)(&argv));
   MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
   MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
   cgp_mpi_comm(MPI_COMM_WORLD);

/* open CGNS file for read-only */
   if (cgp_open("grid_piz_c.cgns", CG_MODE_READ, &index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base = 1;
/* we know there is only one zone (real working code would check!) */
    index_zone = 1;
/* get zone size (and name - although not needed here) */
    if (cg_zone_read(index_file, index_base, index_zone, zonename,
                     (cgsize_t*)zoneSize)) cg_error_exit();

/* COLLECTIVE READING OF FILE DATA -- each processor reads a part of the zone */

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
   int ni, nj, nk;
   if (numLocalkIdx > 0)
     {
       /* allocate memory for reading */
       ni = zoneSize[0][0];
       nj = zoneSize[0][1];
       nk = numLocalkIdx;
       const int num_vertex = ni*nj*nk;
       x = (double*)malloc(3*num_vertex*sizeof(double));
       y = x + num_vertex;
       z = y + num_vertex;
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
   /* the file data is defined as single but will be read as double */
   if (cgp_coord_general_read_data(index_file, index_base, index_zone, 1,
                                   s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                   3, m_dimvals, m_rmin, m_rmax,
                                   x)) cgp_error_exit();
   if (cgp_coord_general_read_data(index_file, index_base, index_zone, 2,
                                   s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                   3, m_dimvals, m_rmin, m_rmax,
                                   y)) cgp_error_exit();
   if (cgp_coord_general_read_data(index_file, index_base, index_zone, 3,
                                   s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                   3, m_dimvals, m_rmin, m_rmax,
                                   z)) cgp_error_exit();
   if (8 >= kIdxBeg && 8 < kIdxBeg + numLocalkIdx)
     {
       printf("\nSuccessfully read grid from file grid_piz_c.cgns\n");
       printf("  For example, from process %d, zone 1 x,y,z[8][18][16]= "
              "%f, %f, %f\n", comm_rank,
              x[((8 - kIdxBeg)*nj + 18)*ni + 16],
              y[((8 - kIdxBeg)*nj + 18)*ni + 16],
              z[((8 - kIdxBeg)*nj + 18)*ni + 16]);
     }
   if (numLocalkIdx > 0)
     {
       free(x);
     }
/* close CGNS file */
   cgp_close(index_file);
   MPI_Finalize();
   if (comm_rank == 0)
     {
       printf("\nProgram successful... ending now\n");
     }
   return 0;
}
