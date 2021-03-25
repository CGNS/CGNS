/* This test is intended to monitor the 
 * parallel performance of sizeable metadata 
 * usage by a CGNS application. Its 
 * main focus is not regression functionality
 * testing of CGNS parallel APIs.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "pcgnslib.h"
#include "mpi.h"

// Number of cells in x, y, z
#define N 32

//Total number of blocks
//uint32_t NBLOCKS = 8192;
//uint32_t NBLOCKS = 16384;
//uint32_t NBLOCKS = 32768;
//uint32_t NBLOCKS = 4096;
uint32_t NBLOCKS = 512;

int whoami  = 0;
int num_mpi = 0;

int main ( int argc, char *argv[] );

// Overall domain size
const float L[6] = {0., 20., 0., 1., 0., 1.};

// All zones have the same size
const cgsize_t zoneSize[3][3] = {{N + 1, N + 1, N + 1}, {N, N, N}, {0, 0, 0}};

const int ROOT = 0;

int read_inputs(int* argc, char*** argv) {

  if(whoami==0) {
    for(int k = 1; k < *argc; k++) {
      if(strcmp((*argv)[k],"-nblocks")==0) {
        k++;
        sscanf((*argv)[k],"%u",&NBLOCKS);
      }
    }
  }
  MPI_Bcast(&NBLOCKS, 1, MPI_INT, 0, MPI_COMM_WORLD);
  return 0;
}
int initialize(int* argc, char** argv[]) {
  MPI_Init(argc,argv);
  MPI_Comm_size(MPI_COMM_WORLD, &num_mpi);
  MPI_Comm_rank(MPI_COMM_WORLD, &whoami);

  if(*argc > 2)
    read_inputs(argc,argv);

  return 0;
}

int main(int argc, char* argv[]) {

  int exit = 0;
  int* local_blocks;
  int *zone;
  char fname[17];
  int index_file;
  int index_base;
  int index_grid;
  int index_coordx, index_coordy, index_coordz;
  double t0, t1;
  char zonename[12];

  initialize(&argc,&argv);

  // ---- Partitioning -------------------------------------------------------------------
  if (num_mpi > NBLOCKS) {
    if (whoami == ROOT) {
      printf("Number of processes > NBLOCKS.\n");
    }
    MPI_Finalize();
    return exit;
  }

  local_blocks = (int *)malloc(num_mpi*sizeof(int));
  for (int i = 0; i < num_mpi; ++i) {
    if (i == ROOT) {
      // Remainder goes to root
      local_blocks[i] = NBLOCKS / num_mpi + NBLOCKS % num_mpi;
    } else {
      local_blocks[i] = NBLOCKS / num_mpi;
    }
  }

  if (whoami == ROOT) {
    for (int i = 0; i < num_mpi; ++i) {
      printf("Process %d holds %d blocks\n", i, local_blocks[i]);
    }
  }

  // ---- Set MPI communicator for parallel operations  ----------------------------------
  //cgp_mpi_comm(MPI_COMM_WORLD);

  snprintf(fname, 16, "grid_%u.cgns", NBLOCKS);

  if (whoami == ROOT) {
  // ---- Open CGNS file -----------------------------------------------------------------

    t0 = MPI_Wtime();
    if (cg_open(fname, CG_MODE_WRITE, &index_file)) cg_error_exit();
    t1 = MPI_Wtime();
    if (whoami == ROOT) {
      printf("cg_open: %lf s \n", (t1 - t0));
    }
    //cgns_filetype = old_type;

  // ---- Create base  -------------------------------------------------------------------
    char *basename = "Base";
    int icelldim = 3;
    int iphysdim = 3;
    if(cg_base_write(index_file, basename, icelldim, iphysdim, &index_base) != CG_OK) {
      printf("*FAILED* cg_base_write\n");
      cg_error_exit();
    };

  // ---- Create zones (done by all processes) -------------------------------------------
    t0 = MPI_Wtime();
    zone = (int *)malloc(NBLOCKS*sizeof(int));
    for (uint32_t b = 0; b < NBLOCKS; ++b) {
      // Zone name
      snprintf(zonename, 11, "Zone_%u", (b + 1) );
      // Create zone
      if (cg_zone_write(index_file, index_base, zonename, (cgsize_t *)zoneSize,
                        CGNS_ENUMV(Structured), &(zone[b])) != CG_OK)
        cg_error_exit();
    }
    t1 = MPI_Wtime();
    if (whoami == ROOT) {
      printf("cg_zone_write: %lf s\n", (t1 - t0));
    }

    t0 = MPI_Wtime();
    for (uint32_t b = 0; b < NBLOCKS; ++b) {
      if (cg_grid_write(index_file, index_base, zone[b], "GridCoordinates", &index_grid) != CG_OK) {
        printf("*FAILED* cg_grid_write \n");
        cg_error_exit();
      }
    }
    t1 = MPI_Wtime();
    if (whoami == ROOT) {
      printf("cg_grid_write: %lf s \n", (t1 - t0));
    }
#if 0
    t0 = MPI_Wtime();
    for (uint32_t b = 0; b < NBLOCKS; ++b) {
      if (cgp_coord_write(index_file, index_base, zone[b], CGNS_ENUMV(RealSingle), "CoordinateX",
                          &index_coordx))
        cg_error_exit();
      if (cgp_coord_write(index_file, index_base, zone[b], CGNS_ENUMV(RealSingle), "CoordinateY",
                          &index_coordy))
        cg_error_exit();
      if (cgp_coord_write(index_file, index_base, zone[b], CGNS_ENUMV(RealSingle), "CoordinateZ",
                          &index_coordz))
        cg_error_exit();
    }
    t1 = MPI_Wtime();
    if (whoami == ROOT) {
      printf("cgp_coord_write: %lf s\n", (t1 - t0));
    }
#endif
    t0 = MPI_Wtime();
    cg_close(index_file);
    t1 = MPI_Wtime();
    if (whoami == ROOT) {
      printf("cg_close: %lf s\n", (t1 - t0));
    }
  }

  MPI_Barrier(MPI_COMM_WORLD);

  t0 = MPI_Wtime();
  if(cgp_open(fname, CG_MODE_MODIFY, &index_file) != CG_OK) {
    printf("*FAILED* cgp_open \n");
    cgp_error_exit();
  }
  MPI_Barrier(MPI_COMM_WORLD);
  t1 = MPI_Wtime();
  if (whoami == ROOT) {
    printf("cgp_open: %lf s\n", (t1 - t0));
  }

  index_base = 1;

  t0 = MPI_Wtime();
  for (uint32_t b = 0; b < NBLOCKS; ++b) {
    if (cgp_coord_write(index_file, index_base, b + 1, CGNS_ENUMV(RealSingle), "CoordinateX",
                        &index_coordx))
      cg_error_exit();
    if (cgp_coord_write(index_file, index_base, b + 1, CGNS_ENUMV(RealSingle), "CoordinateY",
                        &index_coordy))
      cg_error_exit();
    if (cgp_coord_write(index_file, index_base, b + 1, CGNS_ENUMV(RealSingle), "CoordinateZ",
                        &index_coordz))
      cg_error_exit();
  }
  MPI_Barrier(MPI_COMM_WORLD);
  t1 = MPI_Wtime();
  if (whoami == ROOT) {
    printf("cgp_coord_write: %lf s\n", (t1 - t0));
  }

  for (uint32_t b = 0; b < NBLOCKS; ++b) {
    snprintf(zonename, 11, "Zone_%u", (b + 1) );
    if(cg_goto(index_file, index_base, zonename, 0, "end") != CG_OK) {
      printf("*FAILED* cg_goto\n");
      cgp_error_exit();
    }
  }
  for (uint32_t b = 0; b < NBLOCKS; ++b) {
    char path[100];
    snprintf(path, 99, "/Base/Zone_%u/GridCoordinates/CoordinateX", (b + 1) );
    if(cg_gopath(index_file, path) != CG_OK) {
      printf("*FAILED* cg_goto\n");
      cgp_error_exit();
    }
  }

  t0 = MPI_Wtime();
  cgp_close(index_file);
  MPI_Barrier(MPI_COMM_WORLD);
  t1 = MPI_Wtime();
  if (whoami == ROOT) {
    printf("cgp_close: %lf s\n", (t1 - t0));
  }

  free(zone);
  free(local_blocks);

  MPI_Finalize();

  return exit;
}
