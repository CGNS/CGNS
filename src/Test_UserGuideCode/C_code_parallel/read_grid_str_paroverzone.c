/*   Program read_grid_str_paroverzone.c    */
/*
Reads a simple 3-D structured grid from a CGNS file.  Each processor reads data
from one zone (parallelism over zones).

The CGNS grid file 'grid_poz_c.cgns' must already exist.

mpicxx read_grid_str_paroverzone.c -lcgns -lhdf5 -lsz -lz -o read_grid_str_paroverzone
mpirun -np 2 write_grid_str_paroverzone
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pcgnslib.h"
#include "mpi.h"

int main(int argc, const char* argv[])
{
   int n, comm_size, comm_rank;
   int index_file, index_base, index_zone;
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
       double *x = NULL;
       double *y = NULL;
       double *z = NULL;
       int ni, nj, nk;
       /* if there is nothing for this process to read, a valid zone index must
          be provided and data array = NULL */
       index_zone = (idxGlobalZone < numZone) ? idxGlobalZone + 1 : 1;
       if (idxGlobalZone < numZone)
         {
           /* get zone size (and name - although not needed here) */
           if (cg_zone_read(index_file, index_base, index_zone, zonename,
                            (cgsize_t*)zoneSize)) cg_error_exit();

           /* allocate memory for reading */
           ni = zoneSize[0][0];
           nj = zoneSize[0][1];
           nk = zoneSize[0][2];
           const int num_vertex = ni*nj*nk;
           x = (double*)malloc(3*num_vertex*sizeof(double));
           y = x + num_vertex;
           z = y + num_vertex;
           /* shape in file space */
           for (n = 0; n < 3; ++n)
             {
               s_rmin[n] = 1;
               s_rmax[n] = zoneSize[0][n];
             }
           /* shape in memory */
           for (n = 0; n < 3; ++n)
             {
               m_dimvals[n] = zoneSize[0][n];
               m_rmin[n]    = 1;
               m_rmax[n]    = zoneSize[0][n];
             }
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
       for (n = 0; n < comm_size; ++n)
         {
           if (n == comm_rank)
             {
               if (idxGlobalZone < numZone)
                 {
                   printf("\nProcess %d successfully read grid from file "
                          "grid_poz_c.cgns\n", comm_rank);
                   printf("  For example, zone %d x,y,z[8][18][16]= "
                          "%f, %f, %f\n", index_zone,
                          x[(8*nj + 18)*ni + 16],
                          y[(8*nj + 18)*ni + 16],
                          z[(8*nj + 18)*ni + 16]);
                   fflush(stdout);
                 }
             }
           /* this does not guarantee output is written in order but may help */
           MPI_Barrier(MPI_COMM_WORLD);
         }
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
       printf("\nProgram successful... ending now\n");
     }
   return 0;
}
