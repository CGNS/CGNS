/*    Program read_flowcentrind_str_paroverzone   */
/*
Opens an existing CGNS file that contains a simple two-zone
3-D structured grid plus a flow solution (at CELL
CENTERS PLUS RIND CELLS IN I AND J DIRECTIONS) and reads it.
Each processor reads data from one zone (parallelism over zones).

The CGNS grid file 'grid_poz_c.cgns' must already exist
(created using write_grid_str_paroverzone.c followed by
write_flowcentrind_str_paroverzone.c)

Example compilation for this program is (change paths!):

mpicxx read_flowcentrind_str_paroverzone.c -lcgns -lhdf5 -lsz -lz -o read_flowcentrind_str_paroverzone
mpirun -np 2 read_flowcentrind_str_paroverzone
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
   if (cgp_open("grid_poz_c.cgns", CG_MODE_READ, &index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
   index_base = 1;
/* get the number of zones (should be 2) */
   int numZone;
   if (cg_nzones(index_file, index_base, &numZone)) cg_error_exit();
/* we know there is only one FlowSolution_t (real working code would check!) */
   index_flow=1;

/* COLLECTIVE READING OF FILE DATA -- each processor reads from a separate
   zone */

   /* partition the zones among the processes */
   int maxLocalZone;      /* the maximum number of zones on any process */
   int idxGlobalZoneBeg;  /* the global index of the first zone on this
                             process */
   int numLocalZone = numZone/comm_size;  /* the number of zones local to this
                                             process */
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

   int idxLocalZone;
   for (idxLocalZone = 0; idxLocalZone < maxLocalZone; ++idxLocalZone)
     {
       const int idxGlobalZone = idxGlobalZoneBeg + idxLocalZone;
       cgsize_t zoneSize[3][3];
       cgsize_t s_rmin[3], s_rmax[3], m_dimvals[3], m_rmin[3], m_rmax[3];
       double *r = NULL;
       double *p = NULL;
       int ni, nj, nk;
       /* if there is nothing for this process to read, a valid zone index must
          be provided and data array = NULL */
       index_zone = (idxGlobalZone < numZone) ? idxGlobalZone + 1 : 1;
       if (idxGlobalZone < numZone)
         {
           /* get zone size (and name - although not needed here) */
           if (cg_zone_read(index_file, index_base, index_zone, zonename,
                            (cgsize_t*)zoneSize)) cg_error_exit();

           /* allocate memory for reading (all rind planes) */
           ni = zoneSize[1][0] + 2;
           nj = zoneSize[1][1] + 2;
           nk = zoneSize[1][2] + 2;
           const int num_vertex = ni*nj*nk;
           r = (double*)malloc(2*num_vertex*sizeof(double));
           p = r + num_vertex;
           /* shape in file space (core cells start at 1) */
           for (n = 0; n < 2; ++n)
             {
               s_rmin[n] = 0;
               s_rmax[n] = zoneSize[1][n] + 1;
             }
           s_rmin[2] = 1;  /* but no rind cells in k-direction */
           s_rmax[2] = zoneSize[1][n];
           /* shape in memory (array starts at 1)*/
           for (n = 0; n < 2; ++n)
             {
               m_dimvals[n] = zoneSize[1][n] + 2;
               m_rmin[n]    = 1;
               m_rmax[n]    = zoneSize[1][n] + 2;
             }
           m_dimvals[2] = zoneSize[1][n] + 2;
           m_rmin[2]    = 2;  /* but no rind cells in k-direction */
           m_rmax[2]    = zoneSize[1][n] + 1;
         }
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
       for (n = 0; n < comm_size; ++n)
         {
           if (n == comm_rank)
             {
               if (idxGlobalZone < numZone)
                 {
                   int imax = zoneSize[1][0];
                   printf("\nProcess %d successfully read flow solution from "
                          "zone %d in file grid_poz_c.cgns\n", comm_rank,
                          index_zone);
                   printf("  For example, r,p[8][18][%d]= %f, %f\n", imax,
                          r[(8*nj + 18)*ni + imax], p[(8*nj + 18)*ni + imax]);
                   printf("         rind: r,p[8][18][ 0]= %f, %f\n",
                          r[(8*nj + 18)*ni +  0], p[(8*nj + 18)*ni +  0]);
                   printf("         rind: r,p[8][18][%d]= %f, %f\n", imax+1,
                          r[(8*nj + 18)*ni + imax+1],
                          p[(8*nj + 18)*ni + imax+1]);
                   fflush(stdout);
                 }
             }
           /* this does not guarantee output is written in order but may help */
           MPI_Barrier(MPI_COMM_WORLD);
         }
       if (idxGlobalZone < numZone)
         {
           free(r);
         }
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
