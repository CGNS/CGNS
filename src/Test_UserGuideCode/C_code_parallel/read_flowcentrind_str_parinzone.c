/*    Program read_flowcentrind_str_parinzone   */
/*
Opens an existing CGNS file that contains a simple
3-D structured grid plus a flow solution (at CELL
CENTERS PLUS RIND CELLS IN I AND J DIRECTIONS) and reads it.
Each processor reads a slab of data from one zone
(parallelism within a zone).

The CGNS grid file 'grid_piz_c.cgns' must already exist
(created using write_grid_str_parinzone.c followed by
write_flowcentrind_str_parinzone.c))

Example compilation for this program is (change paths!):

mpicxx read_flowcentrind_str_parinzone.c -lcgns -lhdf5 -lsz -lz -o read_flowcentrind_str_parinzone
mpirun -np 2 read_flowcentrind_str_parinzone
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pcgnslib.h"
#include "mpi.h"

int main(int argc, const char* argv[])
{
   int n, comm_size, comm_rank;
   int index_file, index_base, index_zone, index_flow;
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
   index_zone=1;
/* we know there is only one FlowSolution_t (real working code would check!) */
   index_flow=1;

/* COLLECTIVE READING OF FILE DATA -- each processor reads a part of the zone */

   /* get zone size (and name - although not needed here) */
   cgsize_t zoneSize[3][3];
   if (cg_zone_read(index_file, index_base, index_zone, zonename,
                    (cgsize_t*)zoneSize)) cg_error_exit();

   /* partition the k-index of the zone among the processes.  Note that the
      partitions do not include the k-direction rind planes. */
   int kIdxBeg;  /* beginning k-index for this processor */
   int numLocalkIdx = zoneSize[1][2]/comm_size;  /* the number of k-indices
                                                    local to this process */
   {
     int numUnevenkIdx = zoneSize[1][2] - numLocalkIdx*comm_size;
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
   double *r = NULL;
   double *p = NULL;
   int ni, nj, nk;
   if (numLocalkIdx > 0)
     {
       /* create fake flow solution AT CELL CENTERS for simple example.  In
          memory, there are two rind cells in each direction: */
       ni = zoneSize[1][0] + 2;
       nj = zoneSize[1][1] + 2;
       nk = numLocalkIdx;
       const int num_vertex = ni*nj*nk;
       r = (double*)malloc(2*num_vertex*sizeof(double));
       p = r + num_vertex;
       /* shape in file space (core cells start at 1) */
       for (n = 0; n < 2; ++n)
         {
           s_rmin[n] = 0;
           s_rmax[n] = zoneSize[1][n] + 1;
         }
       s_rmin[2] = kIdxBeg + 1;  /* but no rind cells in k-direction */
       s_rmax[2] = kIdxBeg + numLocalkIdx;
       /* shape in memory (array starts at 1)*/
       for (n = 0; n < 2; ++n)
         {
           m_dimvals[n] = zoneSize[1][n] + 2;
           m_rmin[n]    = 1;
           m_rmax[n]    = zoneSize[1][n] + 2;
         }
       m_dimvals[2] = numLocalkIdx;
       m_rmin[2]    = 1;
       m_rmax[2]    = numLocalkIdx;
     }
   /* if there is nothing for this process to write, a valid zone index must be
      provided and data array = NULL */
   if (cgp_field_general_read_data(index_file, index_base, index_zone,
                                   index_flow, 1,
                                   s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                   3, m_dimvals, m_rmin, m_rmax,
                                   r)) cgp_error_exit();
   if (cgp_field_general_read_data(index_file, index_base, index_zone,
                                   index_flow, 2,
                                   s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                   3, m_dimvals, m_rmin, m_rmax,
                                   p)) cgp_error_exit();
   if (7 >= kIdxBeg && 7 < kIdxBeg + numLocalkIdx)
     {
       printf("\nSuccessfully read flow solution from file grid_piz_c.cgns\n");
       printf("  For example, r,p[7][18][20]= %f, %f\n",
              r[((7 - kIdxBeg)*nj + 18)*ni + 20],
              p[((7 - kIdxBeg)*nj + 18)*ni + 20]);
       printf("         rind: r,p[7][18][ 0]= %f, %f\n",
              r[((7 - kIdxBeg)*nj + 18)*ni +  0],
              p[((7 - kIdxBeg)*nj + 18)*ni +  0]);
       printf("         rind: r,p[7][18][21]= %f, %f\n",
              r[((7 - kIdxBeg)*nj + 18)*ni + 21],
              p[((7 - kIdxBeg)*nj + 18)*ni + 21]);
     }
   if (numLocalkIdx > 0)
     {
       free(r);
     }
/* close CGNS file */
   cg_close(index_file);
   MPI_Finalize();
   if (comm_rank == 0)
     {
       printf("\nProgram successful... ending now\n");
     }
   return 0;
}
