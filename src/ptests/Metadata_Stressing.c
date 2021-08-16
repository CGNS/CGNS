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
uint32_t NBLOCKS = 256;

int whoami  = 0;
int num_mpi = 0;
int core = 1; // Use core VFD for file creation(-sec2 uses the SEC2 VFD in HDF5)
int allranks = 0; // Use all ranks for file creation
int skipread = 0; // skip opening and reading the file parallel

int main ( int argc, char *argv[] );

// Overall domain size
const float L[6] = {0., 20., 0., 1., 0., 1.};

// All zones have the same size
const cgsize_t zoneSize[3][3] = {{N + 1, N + 1, N + 1}, {N, N, N}, {0, 0, 0}};

const int ROOT = 0;

int read_inputs(int* argc, char*** argv) {

  int bcast[4] = {NBLOCKS,allranks,core,skipread};

  if(whoami==0) {
    for(int k = 1; k < *argc; k++) {
      if(strcmp((*argv)[k],"-nblocks")==0) {
        k++;
        sscanf((*argv)[k],"%d",&bcast[0]);
        continue;
      }
      if(strcmp((*argv)[k],"-all")==0) {
        allranks = 1;
        bcast[1] = allranks;
        continue;
      }
      if(strcmp((*argv)[k],"-sec2")==0) {
        core = 0;
        bcast[2] = core;
        continue;
      }
      if(strcmp((*argv)[k],"-skipread")==0) {
        skipread = 1;
        bcast[3] = skipread;
        continue;
      }
      printf("ERROR: invalid argument option %s\n", (*argv)[k]);
      MPI_Abort(MPI_COMM_WORLD,-1);
    }
    if (core == 1 && allranks == 1) {
      printf("WARNING: diskless in parallel not supported, disabling diskless\n");
      core = 0;
      bcast[2] = core;
    }
  }
  MPI_Bcast(bcast, 4, MPI_INT, 0, MPI_COMM_WORLD);
  NBLOCKS  = (uint32_t)bcast[0];
  allranks = bcast[1];
  core     = bcast[2];
  skipread = bcast[3];
  return 0;
}
int initialize(int* argc, char** argv[]) {
  MPI_Init(argc,argv);
  MPI_Comm_size(MPI_COMM_WORLD, &num_mpi);
  MPI_Comm_rank(MPI_COMM_WORLD, &whoami);

  if(*argc > 1)
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

  if (whoami == ROOT) {
    printf("Number of blocks = %u\n", NBLOCKS);
    if(core == 1)
      printf("Diskless enabled = true\n");
    else
      printf("Diskless enabled = false\n");
    if(skipread == 0)
      printf("Skip opening and reading the file = false\n");
    else
      printf("Skip opening and reading the file = true\n");
    if(allranks == 0)
      printf("All ranks create file = false\n\n");
    else
      printf("All ranks create file = true\n\n");
  }
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
    /* Only print for a reasonable number of ranks */
    if ( num_mpi <= 1000) {
      for (int i = 0; i < num_mpi; ++i) {
        printf("Process %d holds %d blocks\n", i, local_blocks[i]);
      }
    }
  }

  // ---- Set MPI communicator for parallel operations  ----------------------------------

  snprintf(fname, 16, "grid_%u.cgns", NBLOCKS);

  // ---- Create CGNS file ---------------------------------------------------------------

  /* use the core file driver, diskless option */
  if(core == 1) {
    /* enable core file driver committing to disk */
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS, (void *)1))
      cg_error_exit();
    /* enable committing to disk */
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS_WRITE, (void *)1))
      cg_error_exit();
    /* set initial/increimental memory increases to 125 MiB */
    size_t incr = 125L*1024L*1024L;
    if (cg_configure(CG_CONFIG_HDF5_DISKLESS_INCR, (void *)(incr)))
      cg_error_exit();
  }
  t0 = MPI_Wtime();
  if(allranks == 0) {
    if (whoami == ROOT) {
      if (cg_open(fname, CG_MODE_WRITE, &index_file)) cg_error_exit();
    }
  } else {
    cgp_mpi_comm(MPI_COMM_WORLD);
    if (cgp_open(fname, CG_MODE_WRITE, &index_file)) cg_error_exit();
  }
  t1 = MPI_Wtime();
  if (whoami == ROOT) {
    printf("CREATE:cg_open: %lf s \n", (t1 - t0));
  }

  // ---- Create base  -------------------------------------------------------------------
  char *basename = "Base";
  int icelldim = 3;
  int iphysdim = 3; 
  if(allranks == 0) {
    if (whoami == ROOT) {
      if(cg_base_write(index_file, basename, icelldim, iphysdim, &index_base) != CG_OK) {
        printf("*FAILED* cg_base_write\n");
        cg_error_exit();
      };
    }
  } else {
    if(cg_base_write(index_file, basename, icelldim, iphysdim, &index_base) != CG_OK) {
      printf("*FAILED* cg_base_write\n");
      cg_error_exit();
    };
  }
  // ---- Create zones -------------------------------------------
  t0 = MPI_Wtime();
  if(allranks == 0) {
    if (whoami == ROOT) {
      zone = (int *)malloc(NBLOCKS*sizeof(int));
      for (uint32_t b = 0; b < NBLOCKS; ++b) {
        // Zone name
        snprintf(zonename, 11, "Zone_%u", (b + 1) );
        // Create zone
        if (cg_zone_write(index_file, index_base, zonename, (cgsize_t *)zoneSize,
                          CGNS_ENUMV(Structured), &(zone[b])) != CG_OK)
          cg_error_exit();
      }
    }
  } else {
    zone = (int *)malloc(NBLOCKS*sizeof(int));
    for (uint32_t b = 0; b < NBLOCKS; ++b) {
      // Zone name
      snprintf(zonename, 11, "Zone_%u", (b + 1) );
      // Create zone
      if (cg_zone_write(index_file, index_base, zonename, (cgsize_t *)zoneSize,
                        CGNS_ENUMV(Structured), &(zone[b])) != CG_OK)
        cg_error_exit();
    }
  }
  t1 = MPI_Wtime();
  if (whoami == ROOT) {
    printf("CREATE:cg_zone_write: %lf s\n", (t1 - t0));
  }
  
  t0 = MPI_Wtime();
  if(allranks == 0) {
    if (whoami == ROOT) {
      for (uint32_t b = 0; b < NBLOCKS; ++b) {
        if (cg_grid_write(index_file, index_base, zone[b], "GridCoordinates", &index_grid) != CG_OK) {
          printf("*FAILED* cg_grid_write \n");
          cg_error_exit();
        }
      }
    }
  } else {
    for (uint32_t b = 0; b < NBLOCKS; ++b) {
      if (cg_grid_write(index_file, index_base, zone[b], "GridCoordinates", &index_grid) != CG_OK) {
        printf("*FAILED* cg_grid_write \n");
        cg_error_exit();
      }
    }
  }
  t1 = MPI_Wtime();
  if (whoami == ROOT) {
    printf("CREATE:cg_grid_write: %lf s \n", (t1 - t0));
  }

  t0 = MPI_Wtime();
  if(allranks == 0) {
    if (whoami == ROOT) {
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
    }
  } else {
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
  }

  t1 = MPI_Wtime();
  if (whoami == ROOT) {
    printf("CREATE:cgp_coord_write: %lf s\n", (t1 - t0));
  }

  t0 = MPI_Wtime();
  if(allranks == 0) {
    if (whoami == ROOT) {
      cg_close(index_file);
      free(zone);
    }
  } else {
    cgp_close(index_file);
    free(zone);
  }
  t1 = MPI_Wtime();
  if (whoami == ROOT) {
    printf("CREATE:cg_close: %lf s\n", (t1 - t0));
  }

  MPI_Barrier(MPI_COMM_WORLD);

  /* disable core file driver */
  if (cg_configure(CG_CONFIG_HDF5_DISKLESS, (void *)0))
    cg_error_exit();

  cgp_mpi_comm(MPI_COMM_WORLD);

  if( skipread == 0) {
    t0 = MPI_Wtime();
    if(cgp_open(fname, CG_MODE_MODIFY, &index_file) != CG_OK) {
      printf("*FAILED* cgp_open \n");
      cgp_error_exit();
    }
    MPI_Barrier(MPI_COMM_WORLD);
    t1 = MPI_Wtime();
    if (whoami == ROOT) {
      printf("OPEN:cgp_open: %lf s\n", (t1 - t0));
    }

    index_base = 1;

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
      printf("OPEN:cgp_close: %lf s\n", (t1 - t0));
    }
  }
  free(local_blocks);
  MPI_Finalize();

  return exit;
}
