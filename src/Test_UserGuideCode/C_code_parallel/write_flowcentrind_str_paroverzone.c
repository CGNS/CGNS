/*    Program write_flowcentrind_str   */
/*
Opens an existing CGNS file that contains a simple 3-D 
structured grid, and adds a flow solution (at CELL CENTERS
PLUS RIND CELLS IN I AND J DIRECTIONS) to it.
(Compare this program with write_flowcent_str)

The CGNS grid file 'grid_c.cgns' must already exist
(created using write_grid_str.c)

Example compilation for this program is (change paths!):

cc -I ../CGNS_CVS/cgnslib_2.5 -c write_flowcentrind_str.c
cc -o write_flowcentrind_str_c write_flowcentrind_str.o -L ../CGNS_CVS/cgnslib_2.5/LINUX -lcgns

mpicxx write_flowcentrind_str2d.c -lcgns -lhdf5 -lsz -lz -o write_flowcentrind_str2d
mpirun -np 2 write_flowcentrind_str2d

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
  r and p must be dimensioned exactly as [N-1][17-1+2][21-1+2] (N>=9)
  for this particular case or else they will be written to 
  the CGNS file incorrectly!  Other options are to use 1-D 
  arrays, use dynamic memory, or pass index values to a 
  subroutine and dimension exactly there):
  Rind cells are stored in array locations [k][0][i], [k][17][i], [k][j][0], [k][j][21]
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
   double* r;
   double* p;
   int ni,nj,i,j, numProc, procID;
   int irinddata[4];
   int index_file,index_base;
   int index_zone,index_flow,index_fieldr,index_fieldp;
   int proc_index_zone[nZone],proc_index_flow[nZone];
   int proc_index_fieldr[nZone],proc_index_fieldp[nZone];
   char solname[33];

   MPI_Init(&argc, (char***)(&argv));
   MPI_Comm_size(MPI_COMM_WORLD, &numProc);
   MPI_Comm_rank(MPI_COMM_WORLD, &procID);
   nProcZone = nZone/numProc;
   nProcZone += (procID < (nZone - nProcZone*numProc));

   cgp_mpi_comm(MPI_COMM_WORLD);

/* open CGNS file for modify */
    if (cgp_open("grid_c2d.cgns",CG_MODE_MODIFY,&index_file)) cg_error_exit();
/* we know there is only one base (real working code would check!) */
    index_base=1;

/* COLLECTIVE CREATION OF FILE STRUCTURE -- all processors write the same
 * information */

/* we know there are two zones (real working code would check!) */
   int idxZone;
   idxProcZone = 0;
   for (idxZone = 0; idxZone != nZone; ++idxZone)
     {
       index_zone = idxZone + 1;
       printf("proc %d is writing meta-data for Zone %d\n", procID, index_zone);
       fflush(stdout);
/* define flow solution node name (user can give any name) */
       strcpy(solname,"FlowSolution");
/* create flow solution node (NOTE USE OF CellCenter HERE) */
       cg_sol_write(index_file,index_base,index_zone,solname,CellCenter,
                    &index_flow);
/* go to position within tree at FlowSolution_t node */
       cg_goto(index_file,index_base,"Zone_t",index_zone,
               "FlowSolution_t",index_flow,"end");
/* write rind information under FlowSolution_t node (ilo,ihi,jlo,jhi,klo,khi) */
       irinddata[0]=1;
       irinddata[1]=1;
       irinddata[2]=1;
       irinddata[3]=1;
       cg_rind_write(irinddata);
/* write flow solution nodes (user must use SIDS-standard names here) */
       cgp_field_write(index_file,index_base,index_zone,index_flow,
                       RealDouble,"Density",&index_fieldr);
       cgp_field_write(index_file,index_base,index_zone,index_flow,
                       RealDouble,"Pressure",&index_fieldp);
       if (nProcZone > 0 && (idxZone/nProcZone) == procID)
         {
           proc_index_zone[idxProcZone] = index_zone;
           proc_index_flow[idxProcZone] = index_flow;
           proc_index_fieldr[idxProcZone] = index_fieldr;
           proc_index_fieldp[idxProcZone] = index_fieldp;
           ++idxProcZone;
         }
     }

/* COLLECTIVE WRITING OF FILE DATA -- each processor writes to a separate
 * zone */

   for (idxProcZone = 0; idxProcZone < nProcZone; ++idxProcZone)
     {
       int idxGlobalZone = proc_index_zone[idxProcZone] - 1;
       /* storage in memory */
       ni=nCelli[idxGlobalZone] + 2*nRind;
       nj=nCellj[idxGlobalZone] + 2*nRind;
       r = (double*)malloc(ni*nj*sizeof(double));
       p = (double*)malloc(ni*nj*sizeof(double));

       /* write dummy information */
       for (j=0; j < nj; ++j)
         {
           for (i=0; i < ni; ++i)
             {
               r[j*ni + i] = 3333.;
               p[j*ni + i] = 3333.;
             }
         }
       cgsize_t irmin[2];
       irmin[0] = 1;
       irmin[1] = 1;
       cgsize_t irmax[2];
       irmax[0] = ni;
       irmax[1] = nj;
       cgp_field_write_data(index_file,index_base,
                            proc_index_zone[idxProcZone],
                            proc_index_flow[idxProcZone],
                            proc_index_fieldr[idxProcZone], irmin, irmax, r);
       cgp_field_write_data(index_file,index_base,
                            proc_index_zone[idxProcZone],
                            proc_index_flow[idxProcZone],
                            proc_index_fieldp[idxProcZone], irmin, irmax, p);

       /* write core grid information */
       for (j=0; j < nj; ++j)
         {
           for (i=0; i < ni; ++i)
             {
               r[j*ni + i] = 4444.;
               p[j*ni + i] = 4444.;
             }
         }
       int c = -607 + idxGlobalZone*320;
       for (j=1; j <= nCellj[idxGlobalZone]; j++)
         {
           for (i=1; i <= nCelli[idxGlobalZone]; i++)
             {
               r[j*ni + i] = (float)(c++);
               p[j*ni + i] = (float)j;
             }
         }
/* the core range (shape of data in file) */
       irmin[0] = 1;
       irmax[0] = nCelli[idxGlobalZone];
       irmin[1] = 1;
       irmax[1] = nCellj[idxGlobalZone];
/* the core range in memory (shape of date in memory).  Note that the lower
 * bound is defined to be 1. */
       int mnumdim = 2;
       cgsize_t mdim[2],mrmin[2],mrmax[2];
       mdim[0] = ni;
       mdim[1] = nj;
       mrmin[0] = 1 + nRind;
       mrmin[1] = 1 + nRind;
       mrmax[0] = mdim[0] - nRind;
       mrmax[1] = mdim[1] - nRind;
       printf("proc %d is writing data for Zone %d\n",
              procID, idxGlobalZone + 1);
       fflush(stdout);
       cgp_field_general_write_data(index_file,index_base,
                                    proc_index_zone[idxProcZone],
                                    proc_index_flow[idxProcZone],
                                    proc_index_fieldr[idxProcZone],
                                    irmin, irmax,
                                    mnumdim, mdim, mrmin, mrmax, r);
       cgp_field_general_write_data(index_file,index_base,
                                    proc_index_zone[idxProcZone],
                                    proc_index_flow[idxProcZone],
                                    proc_index_fieldp[idxProcZone],
                                    irmin, irmax,
                                    mnumdim, mdim, mrmin, mrmax, p);
   /* cgp_field_general_write_data(index_file,index_base,proc_index_zone, */
   /*                              proc_index_flow,proc_index_fieldp, */
   /*                              irmin, irmax, mnumdim, mdim, mrmin, mrmax, p); */
       free(p);
       free(r);
     }
/* close CGNS file */
   cg_close(index_file);
   MPI_Finalize();
   if (procID == 0)
     {
       printf("\nSuccessfully added flow solution data to file grid_c2d.cgns\n");
       printf("\nNote:  if the original CGNS file already had a FlowSolution_t node,");
       printf("\n          it has been overwritten\n");
     }
   return 0;
}
