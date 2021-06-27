/*
! @file test_poly_unstructured.c
! @version 0.1
!
! @section DESCRIPTION
! Test program for pcgns library
! -- created to test cgp_poly_section_write (and cg_section_general_write)
! -- created to test cgp_poly_element_write_data
! -- based on a test file of David.Gutzwiller from Numeca
!
! @section USAGE
! Launch with:
!    mpirun -np <#> test_unstructured
!
*/

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <io.h>
#define unlink _unlink
#else
#include <unistd.h>
#endif

#include "mpi.h"
#include "pcgnslib.h"

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

void compareValuesDouble(double val1, double val2) {
  if (fabs(val1 - val2) > 1e-10) {
    printf("ERROR - value comparison failed %f, %f\n", val1, val2);
    MPI_Abort(MPI_COMM_WORLD, 2);
  }
}

void compareValuescgSize_t(cgsize_t val1, cgsize_t val2) {
  if (val1 != val2) {
    printf("ERROR - value comparison failed %d, %d\n", val1, val2);
    MPI_Abort(MPI_COMM_WORLD, 2);
  }
}

void callCGNS(int error) {
  if (error != CG_OK)
    cgp_error_exit();
}

int main(int argc, char **argv) {
  // set up MPI
  int comm_size;
  int comm_rank;
  int ierr = 0;

  // parallel/serial read communicators
  MPI_Comm comm_parallel = MPI_COMM_WORLD;
  MPI_Comm comm_serial = MPI_COMM_SELF;

  MPI_Init(NULL, NULL);
  MPI_Comm_size(comm_parallel, &comm_size);
  MPI_Comm_rank(comm_parallel, &comm_rank);

  // various CGNS handles
  int cgfile = 0;
  int cgbase = 0;
  int cgzone = 0;
  int cgcoord = 0;
  int cgelem = 0;

  // default test values
  int cell_dim = 3;
  int phys_dim = 3;
  int nb_zones = 5;
  cgsize_t nbCellSide = 20;

  char fileName[] = "test_par_poly.cgns";
  char baseName[] = "Unstructured3D";

  if (comm_size != 2 && comm_rank == 0) {
    printf("WARNING: you are supposed to run this test with two "
           "processes\n");
  }

  if (comm_rank == 0) {
    printf("Unstructured CGNS mesh write test with %d ranks\n", comm_size);
    printf("nbCellSide %d\n", nbCellSide);
    printf("nbZones %d\n", nb_zones);
  }

  // basic mesh information
  cgsize_t nbNodeSide = nbCellSide + 1;
  cgsize_t nbNodeSlice = nbNodeSide * nbNodeSide;
  cgsize_t nbNodeTotal = nbNodeSide * nbNodeSlice;
  cgsize_t nbCellSlice = nbCellSide * nbCellSide;
  cgsize_t nbCellTotal = nbCellSide * nbCellSlice;
  // Sizing of Section and
  cgsize_t offsetsTotalSize = 0;
  cgsize_t startOffset = 0;
  // MPI arrays
  long *offsets_sizes = NULL;
  long local_size = 0;

  CGNS_ENUMT(ElementType_t) eType = CGNS_ENUMV(MIXED);

  // distribute the elements and nodes hosted by each rank
  cgsize_t nbNodeIdeal = (nbNodeTotal / comm_size) + 1;
  cgsize_t nbCellIdeal = (nbCellTotal / comm_size) + 1;
  cgsize_t cellOnProcStart = (comm_rank) * (nbCellIdeal);
  cgsize_t cellOnProcEnd = (comm_rank + 1) * (nbCellIdeal);
  cgsize_t nodeOnProcStart = (comm_rank) * (nbNodeIdeal);
  cgsize_t nodeOnProcEnd = (comm_rank + 1) * (nbNodeIdeal);
  cellOnProcEnd = MIN(cellOnProcEnd, nbCellTotal);
  nodeOnProcEnd = MIN(nodeOnProcEnd, nbNodeTotal);
  cgsize_t nbCellWrite = cellOnProcEnd - cellOnProcStart;
  cgsize_t nbNodeWrite = nodeOnProcEnd - nodeOnProcStart;

  printf("rank %d hosts %d cells in range [%d,%d]\n", comm_rank, nbCellWrite,
         cellOnProcStart, cellOnProcEnd);
  printf("rank %d hosts %d nodes in range [%d,%d]\n", comm_rank, nbNodeWrite,
         nodeOnProcStart, nodeOnProcEnd);

  // create a simple cube mesh
  double *nodeX = (double *)malloc(sizeof(double) * nbNodeWrite);
  double *nodeY = (double *)malloc(sizeof(double) * nbNodeWrite);
  double *nodeZ = (double *)malloc(sizeof(double) * nbNodeWrite);
  double spacing = 1.0 / (nbNodeSide - 1);
  int count = 0;
  for (cgsize_t iNode = nodeOnProcStart; iNode < nodeOnProcEnd; iNode++) {
    cgsize_t i = floor(iNode / nbNodeSlice);
    cgsize_t j = floor((iNode - i * nbNodeSlice) / nbNodeSide);
    cgsize_t k = floor(iNode - i * nbNodeSlice - j * nbNodeSide);
    nodeX[count] = (i * spacing);
    nodeY[count] = (j * spacing);
    nodeZ[count] = (k * spacing);
    count++;
  }

  cgsize_t *cells = (cgsize_t *)malloc(sizeof(cgsize_t) * nbCellWrite * 9);
  cgsize_t *offsets = (cgsize_t *)malloc(sizeof(cgsize_t) * (nbCellWrite + 1));
  count = 0;
  offsets[0] = 0;
  for (cgsize_t iCell = cellOnProcStart; iCell < cellOnProcEnd; iCell++) {
    cgsize_t i = floor(iCell / nbCellSlice);
    cgsize_t j = floor((iCell - i * nbCellSlice) / nbCellSide);
    cgsize_t k = floor(iCell - i * nbCellSlice - j * nbCellSide);
    cells[count + 0] = (cgsize_t)(CGNS_ENUMV(HEXA_8));
    cells[count + 1] =
        (i + 0) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 0) + 1;
    cells[count + 2] =
        (i + 1) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 0) + 1;
    cells[count + 3] =
        (i + 1) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 0) + 1;
    cells[count + 4] =
        (i + 0) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 0) + 1;
    cells[count + 5] =
        (i + 0) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 1) + 1;
    cells[count + 6] =
        (i + 1) * nbNodeSlice + (j + 0) * nbNodeSide + (k + 1) + 1;
    cells[count + 7] =
        (i + 1) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 1) + 1;
    cells[count + 8] =
        (i + 0) * nbNodeSlice + (j + 1) * nbNodeSide + (k + 1) + 1;
    count = count + 9;
    offsets[iCell - cellOnProcStart + 1] = count;
  }

  offsets_sizes = (long *)malloc(sizeof(long) * comm_size);
  local_size = offsets[cellOnProcEnd - cellOnProcStart];

  MPI_Allgather(&local_size, 1, MPI_LONG, offsets_sizes, 1, MPI_LONG,
                comm_parallel);

  for (int iProc = 0; iProc < comm_size; iProc++) {
    if (iProc < comm_rank) {
      startOffset += (cgsize_t) offsets_sizes[iProc];
    }
    offsetsTotalSize += (cgsize_t) offsets_sizes[iProc];
  }
  free(offsets_sizes);

  // Update local offset to global
  for (cgsize_t iCell = 0; iCell < cellOnProcEnd - cellOnProcStart + 1;
       iCell++) {
    offsets[iCell] += startOffset;
  }

  if (comm_rank == 0) {
    unlink(fileName);
  }

  double writeTimerStart = MPI_Wtime();
  MPI_Barrier(comm_parallel);

  callCGNS(cg_configure(CG_CONFIG_COMPRESS, 0));
  callCGNS(cgp_mpi_comm(comm_parallel));
  callCGNS(cgp_open(fileName, CG_MODE_WRITE, &cgfile));
  callCGNS(cg_base_write(cgfile, baseName, cell_dim, phys_dim, &cgbase));

  if (comm_rank == 0)
    printf("writing node data in parallel\n");
  for (int iZone = 0; iZone < nb_zones; iZone++) {
    // offset the nodes for each zone
    for (cgsize_t iNode = 0; iZone != 0 && iNode < nbNodeWrite; iNode++) {
      nodeX[iNode] = nodeX[iNode] + 1.0;
    }

    // create the zone
    char zoneName[10];
    sprintf(zoneName, "domain%d", iZone);
    cgsize_t zoneSize[9];
    zoneSize[0] = nbNodeTotal;
    zoneSize[1] = nbCellTotal;
    zoneSize[2] = 0;
    callCGNS(cg_zone_write(cgfile, cgbase, zoneName, zoneSize, CGNS_ENUMV(Unstructured),
                           &cgzone));

    // write the nodes in parallel
    CGNS_ENUMT(DataType_t) precision = CGNS_ENUMV(RealDouble);
    cgsize_t start = nodeOnProcStart + 1;
    cgsize_t end = nodeOnProcEnd;
    callCGNS(cgp_coord_write(cgfile, cgbase, cgzone, precision, "CoordinateX",
                             &cgcoord));
    callCGNS(cg_coord_partial_write(cgfile, cgbase, cgzone, precision,
                                    "CoordinateX", &start, &end, &nodeX[0],
                                    &cgcoord));
    callCGNS(cgp_coord_write(cgfile, cgbase, cgzone, precision, "CoordinateY",
                             &cgcoord));
    callCGNS(cg_coord_partial_write(cgfile, cgbase, cgzone, precision,
                                    "CoordinateY", &start, &end, &nodeY[0],
                                    &cgcoord));
    callCGNS(cgp_coord_write(cgfile, cgbase, cgzone, precision, "CoordinateZ",
                             &cgcoord));
    callCGNS(cg_coord_partial_write(cgfile, cgbase, cgzone, precision,
                                    "CoordinateZ", &start, &end, &nodeZ[0],
                                    &cgcoord));
  }
  MPI_Barrier(MPI_COMM_WORLD);

  // write elements, mixed
  for (int iZone = 0; iZone < nb_zones; iZone++) {
    cgzone = iZone + 1;
    // create element node
    cgsize_t start = 1;
    cgsize_t end = nbCellTotal;

    callCGNS(cgp_poly_section_write(cgfile, cgbase, cgzone, "Elements 3D",
                                    eType, start, end, offsetsTotalSize, 0,
                                    &cgelem));

    start = cellOnProcStart + 1;
    end = cellOnProcEnd;
    callCGNS(cgp_poly_elements_write_data(cgfile, cgbase, cgzone, cgelem, start,
                                          end, cells, offsets));
  }

  callCGNS(cgp_close(cgfile));

  MPI_Barrier(MPI_COMM_WORLD);
  double elapsedTime = MPI_Wtime() - writeTimerStart;
  if (comm_rank == 0) {
    printf("\ndone, write time = %f\n\n", elapsedTime);
  }

  // now read the data and compare it with the expected values
  if (comm_rank == 0) {
    printf("reading node data in parallel\n");
  }
  for (int iZone = 0; iZone < nb_zones; iZone++) {
    // zero out the node coordinates
    for (cgsize_t iNode = 0; iNode < nbNodeWrite; iNode++) {
      nodeX[iNode] = 0.0;
      nodeY[iNode] = 0.0;
      nodeZ[iNode] = 0.0;
    }

    // open the file in parallel
    callCGNS(cgp_mpi_comm(comm_parallel));
    callCGNS(cgp_open(fileName, CG_MODE_READ, &cgfile));

    // read the nodes and compare
    CGNS_ENUMT(DataType_t) precision = CGNS_ENUMV(RealDouble);
    cgsize_t start = nodeOnProcStart + 1;
    cgsize_t end = nodeOnProcEnd;
    cgzone = iZone + 1;
    cgcoord = 1;
    callCGNS(cgp_coord_read_data(cgfile, cgbase, cgzone, cgcoord, &start, &end,
                                 &nodeX[0]));
    cgcoord = 2;
    callCGNS(cgp_coord_read_data(cgfile, cgbase, cgzone, cgcoord, &start, &end,
                                 &nodeY[0]));
    cgcoord = 3;
    callCGNS(cgp_coord_read_data(cgfile, cgbase, cgzone, cgcoord, &start, &end,
                                 &nodeZ[0]));

    int ipos = 0;
    for (cgsize_t iNode = nodeOnProcStart; iNode < nodeOnProcEnd; iNode++) {
      cgsize_t i = floor(iNode / nbNodeSlice);
      cgsize_t j = floor((iNode - i * nbNodeSlice) / nbNodeSide);
      cgsize_t k = floor(iNode - i * nbNodeSlice - j * nbNodeSide);
      compareValuesDouble(nodeX[ipos], ((i * spacing) + iZone * 1.0));
      compareValuesDouble(nodeY[ipos], (j * spacing));
      compareValuesDouble(nodeZ[ipos], (k * spacing));
      ipos++;
    }
    callCGNS(cgp_close(cgfile));
  }
  free(nodeX);
  free(nodeY);
  free(nodeZ);

  for (int iZone = 0; iZone < nb_zones; iZone++) {
    // read the elements
    if (comm_rank == 0)
      printf("reading mixed element data in serial\n");

    // do the comparison on rank 0 only, read for all procs...
    if (comm_rank == 0) {
      callCGNS(cgp_mpi_comm(comm_serial));
      callCGNS(cgp_open(fileName, CG_MODE_READ, &cgfile));
      cgzone = iZone + 1;
      cgelem = 1;
      long start  = (long) (cellOnProcStart + 1);
      long end    = (long) cellOnProcEnd;
      long nbRead = (long) (cellOnProcEnd - cellOnProcStart);
      for (int iProc = 0; iProc < comm_size; iProc++) {
        if (iProc != comm_rank) {
          MPI_Recv(&nbRead, 1, MPI_LONG, iProc, 1, MPI_COMM_WORLD,
                   MPI_STATUS_IGNORE);
          MPI_Recv(&start, 1, MPI_LONG, iProc, 2, MPI_COMM_WORLD,
                   MPI_STATUS_IGNORE);
          MPI_Recv(&end, 1, MPI_LONG, iProc, 3, MPI_COMM_WORLD,
                   MPI_STATUS_IGNORE);
        }
        cgsize_t sizeRead;
        callCGNS(cg_ElementPartialSize(cgfile, cgbase, cgzone, cgelem, start,
                                       end, &sizeRead));
        cgsize_t *cellsRead = (cgsize_t *)malloc(sizeof(cgsize_t) * sizeRead);
        cgsize_t *offsetsRead =
            (cgsize_t *)malloc(sizeof(cgsize_t) * (nbRead + 1));
        callCGNS(cg_poly_elements_partial_read(cgfile, cgbase, cgzone, cgelem,
                                               start, end, cellsRead,
                                               offsetsRead, NULL));

        cgsize_t count = 0;
        for (cgsize_t iCell = (start - 1); iCell < end; iCell++) {
          cgsize_t i = floor(iCell / nbCellSlice);
          cgsize_t j = floor((iCell - i * nbCellSlice) / nbCellSide);
          cgsize_t k = floor(iCell - i * nbCellSlice - j * nbCellSide);
          compareValuescgSize_t(cellsRead[count + 0], (cgsize_t)(CGNS_ENUMV(HEXA_8)));
          compareValuescgSize_t(cellsRead[count + 1], (i + 0) * nbNodeSlice +
                                                          (j + 0) * nbNodeSide +
                                                          (k + 0) + 1);
          compareValuescgSize_t(cellsRead[count + 2], (i + 1) * nbNodeSlice +
                                                          (j + 0) * nbNodeSide +
                                                          (k + 0) + 1);
          compareValuescgSize_t(cellsRead[count + 3], (i + 1) * nbNodeSlice +
                                                          (j + 1) * nbNodeSide +
                                                          (k + 0) + 1);
          compareValuescgSize_t(cellsRead[count + 4], (i + 0) * nbNodeSlice +
                                                          (j + 1) * nbNodeSide +
                                                          (k + 0) + 1);
          compareValuescgSize_t(cellsRead[count + 5], (i + 0) * nbNodeSlice +
                                                          (j + 0) * nbNodeSide +
                                                          (k + 1) + 1);
          compareValuescgSize_t(cellsRead[count + 6], (i + 1) * nbNodeSlice +
                                                          (j + 0) * nbNodeSide +
                                                          (k + 1) + 1);
          compareValuescgSize_t(cellsRead[count + 7], (i + 1) * nbNodeSlice +
                                                          (j + 1) * nbNodeSide +
                                                          (k + 1) + 1);
          compareValuescgSize_t(cellsRead[count + 8], (i + 0) * nbNodeSlice +
                                                          (j + 1) * nbNodeSide +
                                                          (k + 1) + 1);
          count = count + 9;
        }
        free(cellsRead);
        free(offsetsRead);
      }
      callCGNS(cgp_close(cgfile));
    } else {
      long start = (long)(cellOnProcStart + 1);
      long end = (long) cellOnProcEnd;
      long nbRead = (long)(cellOnProcEnd - cellOnProcStart);
      MPI_Send(&nbRead, 1, MPI_LONG, 0, 1, MPI_COMM_WORLD);
      MPI_Send(&start, 1, MPI_LONG, 0, 2, MPI_COMM_WORLD);
      MPI_Send(&end, 1, MPI_LONG, 0, 3, MPI_COMM_WORLD);
    }
  }
  free(cells);
  free(offsets);

  ierr = MPI_Finalize();
  if (ierr != MPI_SUCCESS)
    return 1;
  return 0;
}
