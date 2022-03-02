/*    Program write_flowcentrind_str_paroverzone   */
/*
Opens an existing CGNS file that contains a simple two-zone
3-D  structured grid, and adds a flow solution (at CELL
CENTERS PLUS RIND CELLS IN I AND J DIRECTIONS) to it.

The CGNS grid file 'grid_poz_c.cgns' must already exist
(created using write_grid_str_paroverzone.c)

Example compilation for this program is (change paths!):

mpicxx write_flowcentrind_str_paroverzone.c -lcgns -lhdf5 -lsz -lz -o write_flowcentrind_str_paroverzone
mpirun -np 2 write_flowcentrind_str_paroverzone
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
   if (cgp_open("grid_poz_c.cgns", CG_MODE_MODIFY, &index_file))
     cg_error_exit();
/* we know there is only one base (real working code would check!) */
   index_base = 1;
/* set the number of zones (should be 2, real working code would check!) */
   int numZone = 2;

/* CREATION OF FILE STRUCTURE -- all processors write the same information.
   Only meta-data is written to the library at this stage */

   int idxZone;
   for (idxZone = 0; idxZone != numZone; ++idxZone)
     {
       index_zone = idxZone + 1;
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
     }

/* COLLECTIVE WRITING OF FILE DATA -- each processor writes to a separate
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
           /* create fake flow solution AT CELL CENTERS for simple example.  In
              memory, there are two rind cells in each direction: */
           ni = zoneSize[1][0] + 2;
           nj = zoneSize[1][1] + 2;
           nk = zoneSize[1][2] + 2;
           const int num_vertex = ni*nj*nk;
           const int iOffset = idxGlobalZone*20;
           r = (double*)malloc(2*num_vertex*sizeof(double));
           p = r + num_vertex;
           for (k = 1; k < nk-1; ++k)
             {
               for (j = 1; j < nj-1; ++j)
                 {
                   for (i = 1; i < ni-1; ++i)
                     {
                       int idx = (k*nj + j)*ni + i;
                       r[idx] = (float)(i - 1 + iOffset);
                       p[idx] = (float)(j - 1);
                     }
                 }
             }
           /* create rind cell data: */
           for (j = 0; j < nj; ++j)    /* k planes */
             {
               for (i = 0; i < ni; ++i)
                 {
                   int idx = (0*nj + j)*ni + i;
                   r[idx] = 777.;
                   p[idx] = 777.;
                   idx = ((nk-1)*nj + j)*ni + i;
                   r[idx] = 777.;
                   p[idx] = 777.;
                 }
             }
           for (k = 1; k < nk-1; ++k)  /* i planes */
             {
               for (j = 0; j < nj; ++j)
                 {
                   int idx = (k*nj + j)*ni + 0;
                   r[idx] = 999. + (float)j + (5.*(float)k);
                   p[idx] = 999. + (float)j + (5.*(float)k) + 1.;
                   idx = (k*nj + j)*ni + (ni-1);
                   r[idx] = -999. - (float)j - (5.*(float)k);
                   p[idx] = -999. - (float)j - (5.*(float)k) - 1.;
                 }
             }
           for (k = 1; k < nk-1; ++k)  /* j planes */
             {
               for (i = 0; i < ni; ++i)
                 {
                   int idx = (k*nj + 0)*ni + i;
                   r[idx] = 888. + (float)i + (5.*(float)k);
                   p[idx] = 888. + (float)i + (5.*(float)k) + 1.;
                   idx = (k*nj + (nj-1))*ni + i;
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
       /* if there is nothing for this process to write, a valid zone index must
          be provided and data array = NULL */
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
       printf("\nSuccessfully added flow solution data to file "
              "grid_poz_c.cgns\n");
       printf("\nNote:  if the original CGNS file already had a FlowSolution_t "
              "node,");
       printf("\n          it has been overwritten\n");
     }
   return 0;
}
