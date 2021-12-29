/*    Program write_flowcentrind_str_parinzone   */
/*
Opens an existing CGNS file that contains a simple
3-D structured grid, and adds a flow solution (at CELL
CENTERS PLUS RIND CELLS IN I AND J DIRECTIONS) to it.

The CGNS grid file 'grid_piz_c.cgns' must already exist
(created using write_grid_str_parinzone.c)

Example compilation for this program is (change paths!):

mpicxx write_flowcentrind_str_parinzone.c -lcgns -lhdf5 -lsz -lz -o write_flowcentrind_str_parinzone
mpirun -np 2 write_flowcentrind_str_parinzone
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pcgnslib.h"
#include "mpi.h"

int main(int argc, const char* argv[])
{
   int i, j, k, n, comm_size, comm_rank;
   int index_file, index_base, index_zone, index_flow, index_field;
   char solname[33], zonename[33];

   MPI_Init(&argc, (char***)(&argv));
   MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
   MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
   cgp_mpi_comm(MPI_COMM_WORLD);

/* open CGNS file for modify */
   if (cgp_open("grid_piz_c.cgns", CG_MODE_MODIFY, &index_file))
     cg_error_exit();
/* we know there is only one base (real working code would check!) */
   index_base = 1;
/* we know there is only one zone (real working code would check!) */
   index_zone=1;

/* CREATION OF FILE STRUCTURE -- all processors write the same information.
   Only meta-data is written to the library at this stage */

/* define flow solution node name (user can give any name) */
   strcpy(solname, "FlowSolution");
/* create flow solution node (NOTE USE OF CGNS_ENUMV(CellCenter) HERE) */
   cg_sol_write(index_file, index_base, index_zone, solname,
                CGNS_ENUMV(CellCenter), &index_flow);
/* go to position within tree at FlowSolution_t node */
   cg_goto(index_file, index_base, "Zone_t", index_zone, "FlowSolution_t",
           index_flow, "end");
/* write rind information under FlowSolution_t node (ilo,ihi,jlo,jhi,klo,khi) */
   int irinddata[6] = { 1, 1, 1, 1, 0, 0 };
   cg_rind_write(irinddata);
   if (cgp_field_write(index_file, index_base, index_zone, index_flow,
                       CGNS_ENUMV(RealDouble), "Density",
                       &index_field)) cgp_error_exit();
   if (cgp_field_write(index_file, index_base, index_zone, index_flow,
                       CGNS_ENUMV(RealDouble), "Pressure",
                       &index_field)) cgp_error_exit();

/* COLLECTIVE WRITING OF FILE DATA -- each processor writes to a separate
   zone */

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
       for (k = kIdxBeg; k < kIdxBeg + numLocalkIdx; ++k)
         {
           for (j = 1; j < nj-1; ++j)
             {
               for (i = 1; i < ni-1; ++i)
                 {
                   int idx = ((k - kIdxBeg)*nj + j)*ni + i;
                   r[idx] = (float)(i - 1);
                   p[idx] = (float)(j - 1);
                 }
             }
         }
       /* create rind cell data: */
       /* k rind planes are not even allocated */
       for (k = kIdxBeg; k < kIdxBeg + numLocalkIdx; ++k)  /* i planes */
         {
           for (j = 0; j < nj; ++j)
             {
               int idx = ((k - kIdxBeg)*nj + j)*ni + 0;
               r[idx] = 999. + (float)j + (5.*(float)k);
               p[idx] = 999. + (float)j + (5.*(float)k) + 1.;
               idx = ((k - kIdxBeg)*nj + j)*ni + (ni-1);
               r[idx] = -999. - (float)j - (5.*(float)k);
               p[idx] = -999. - (float)j - (5.*(float)k) - 1.;
             }
         }
       for (k = kIdxBeg; k < kIdxBeg + numLocalkIdx; ++k)  /* j planes */
         {
           for (i = 0; i < ni; ++i)
             {
               int idx = ((k - kIdxBeg)*nj + 0)*ni + i;
               r[idx] = 888. + (float)i + (5.*(float)k);
               p[idx] = 888. + (float)i + (5.*(float)k) + 1.;
               idx = ((k - kIdxBeg)*nj + (nj-1))*ni + i;
               r[idx] = -888. - (float)i - (5.*(float)k);
               p[idx] = -888. - (float)i - (5.*(float)k) - 1.;
             }
         }
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
   if (cgp_field_general_write_data(index_file, index_base, index_zone,
                                    index_flow, 1,
                                    s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                    3, m_dimvals, m_rmin, m_rmax,
                                    r)) cgp_error_exit();
   if (cgp_field_general_write_data(index_file, index_base, index_zone,
                                    index_flow, 2,
                                    s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                    3, m_dimvals, m_rmin, m_rmax,
                                    p)) cgp_error_exit();
   if (numLocalkIdx > 0)
     {
       free(r);
     }
/* close CGNS file */
   cg_close(index_file);
   MPI_Finalize();
   if (comm_rank == 0)
     {
       printf("\nSuccessfully added flow solution data to file "
              "grid_piz_c.cgns\n");
       printf("\nNote:  if the original CGNS file already had a FlowSolution_t "
              "node,");
       printf("\n          it has been overwritten\n");
     }
   return 0;
}
