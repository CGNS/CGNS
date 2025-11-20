#ifndef CGIO_INTERNAL_TYPE_H
#define CGIO_INTERNAL_TYPE_H

typedef enum {
  CGIO_NATIVE_MODE = 0,
  CGIO_PARALLEL_MODE = 1
} access_mode_t;

typedef struct _cgns_io_ctx_t {
    /* Flag indicating if HDF5 file accesses is PARALLEL or NATIVE */
    access_mode_t hdf5_access_mode;
#if CG_BUILD_PARALLEL
    /* MPI-2 info object */
    MPI_Comm pcg_mpi_comm;
    int pcg_mpi_comm_size;
    int pcg_mpi_comm_rank;
    /* flag indicating if mpi_initialized was called */
    int pcg_mpi_initialized;
    MPI_Info pcg_mpi_info;
    hid_t default_pio_mode;
#endif
} cgns_io_ctx_t;

#endif /* CGIO_INTERNAL_TYPE_H */
