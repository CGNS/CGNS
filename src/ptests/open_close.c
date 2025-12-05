/*
! @file open_close.c
! @author Kyle Horne <horne.kyle@gmail.com>
! @version 0.2
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Test program for pcgns library
*/

#include <stdio.h>
#include <stdlib.h>

#include "pcgnslib.h"
#include "mpi.h"

#define cgp_doError {printf("Error at %s:%u\n",__FILE__, __LINE__); return 1;}

int main(int argc, char* argv[]) {
	int err;
	int comm_size;
	int comm_rank;
	MPI_Info info;
        MPI_Comm comm_self = MPI_COMM_SELF;
	int fn;
        size_t value[2];

	err = MPI_Init(&argc,&argv);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
	if(err!=MPI_SUCCESS) cgp_doError;
	err = MPI_Info_create(&(info));
	if(err!=MPI_SUCCESS) cgp_doError;

        if(comm_rank == 0) {
          if (cg_open("open_close.cgns", CG_MODE_WRITE, &fn))
            cg_error_exit();

          if (cg_close(fn))
            cg_error_exit();
        }

        MPI_Barrier(MPI_COMM_WORLD);
        if(cgp_open("open_close.cgns", CG_MODE_MODIFY, &fn))
          cgp_error_exit();

        if(cgp_close(fn))
          cgp_error_exit();

       if(cgp_open("open_close.cgns", CG_MODE_READ, &fn))
          cgp_error_exit();

        if(cgp_close(fn))
          cgp_error_exit();

        MPI_Barrier(MPI_COMM_WORLD);

        cgp_mpi_comm(comm_self);

        if(comm_rank == 0) {
          if (cgp_open("open_close.cgns", CG_MODE_READ, &fn))
            cg_error_exit();

          if (cgp_close(fn))
            cg_error_exit();
        }

        cgp_mpi_comm(MPI_COMM_WORLD);

#ifdef DEBUG_MPI
        printf("[%d]cgp_open\n",comm_rank);
        fflush(stdout);
	if (cgp_open("open_close_p.cgns", CG_MODE_WRITE, &fn))
	    cgp_error_exit();
        printf("[%d]cgp_close\n",comm_rank);
        fflush(stdout);
	if (cgp_close(fn))
	    cgp_error_exit();
#else

	if (cgp_open("open_close_p.cgns", CG_MODE_WRITE, &fn))
	    cgp_error_exit();
	if (cgp_close(fn))
	    cgp_error_exit();
#endif

        /* test setting COMM via configure API */
        if (cg_configure(CG_CONFIG_HDF5_MPI_COMM, &comm_self))
            cgp_error_exit();

        if (comm_rank == 0) {
           if (cgp_open("open_close.cgns", CG_MODE_READ, &fn))
               cg_error_exit();
           if (cgp_close(fn))
               cg_error_exit();
        }
        MPI_Barrier(MPI_COMM_WORLD);

        cgp_mpi_comm(MPI_COMM_WORLD);

        value[0] = 0; /* threshold for H5Pset_alignment */
        value[1] = 2*1024*1024; /* alignment for H5Pset_alignment */

        if (cg_configure(CG_CONFIG_HDF5_ALIGNMENT, value))
            cgp_error_exit();

        if (cg_configure(CG_CONFIG_HDF5_MD_BLOCK_SIZE, (void *)(8*1024)))
            cgp_error_exit();

        if (cg_configure(CG_CONFIG_HDF5_BUFFER, (void *)(4*1024*1024)))
            cgp_error_exit();

        if (cg_configure(CG_CONFIG_HDF5_SIEVE_BUF_SIZE, (void *)(2*1024*1024)))
            cgp_error_exit();

        if (cg_configure(CG_CONFIG_HDF5_ELINK_CACHE_SIZE, (void *)(10)))
            cgp_error_exit();

        if (cgp_open("test_cg_conf.cgns", CG_MODE_WRITE, &fn))
          cgp_error_exit();
        if (cgp_close(fn))
          cgp_error_exit();

        if (cg_configure(CG_CONFIG_RESET, (void *)CG_CONFIG_RESET_HDF5))
            cgp_error_exit();

        if (cgp_open("test_cg_conf.cgns", CG_MODE_WRITE, &fn))
          cgp_error_exit();
        if (cgp_close(fn))
          cgp_error_exit();

        /* Test for GitHub issue #836: multiple parallel file operations
         * Verify that closing one file doesn't corrupt parallel access mode
         * for other open files */
        {
            int fn1, fn2, B, Z, C;
            cgsize_t sizes[9];
            cgsize_t rmin[3], rmax[3];
            double coord_array[10];
            int i;

            if (comm_rank == 0)
                printf("Testing multiple parallel file operations\n");

            /* Open first file in parallel mode */
            if (cgp_open("test_multi_file1.cgns", CG_MODE_WRITE, &fn1))
                cgp_error_exit();

            /* Create base and zone in file 1 */
            sizes[0] = comm_size * 10;
            sizes[1] = 1;
            sizes[2] = 1;
            sizes[3] = sizes[0] - 1;
            sizes[4] = 0;
            sizes[5] = 0;
            sizes[6] = 0;
            sizes[7] = 0;
            sizes[8] = 0;

            if (cg_base_write(fn1, "Base", 3, 3, &B) ||
                cg_zone_write(fn1, B, "Zone", sizes, CGNS_ENUMV(Structured), &Z) ||
                cgp_coord_write(fn1, B, Z, CGNS_ENUMV(RealDouble), "CoordinateX", &C))
                cgp_error_exit();

            /* Open second file in parallel mode */
            if (cgp_open("test_multi_file2.cgns", CG_MODE_WRITE, &fn2))
                cgp_error_exit();

            /* Create base and zone in file 2 */
            if (cg_base_write(fn2, "Base", 3, 3, &B) ||
                cg_zone_write(fn2, B, "Zone", sizes, CGNS_ENUMV(Structured), &Z) ||
                cgp_coord_write(fn2, B, Z, CGNS_ENUMV(RealDouble), "CoordinateY", &C))
                cgp_error_exit();

            /* Write data to file 1 */
            rmin[0] = comm_rank * 10 + 1;
            rmin[1] = 1;
            rmin[2] = 1;
            rmax[0] = (comm_rank + 1) * 10;
            rmax[1] = 1;
            rmax[2] = 1;
            for (i = 0; i < 10; i++)
                coord_array[i] = (double)(comm_rank * 10 + i);

            if (cgp_coord_write_data(fn1, B, Z, C, rmin, rmax, coord_array))
                cgp_error_exit();

            /* Close the first file
             * This would reset hdf5_access_mode to NATIVE before the fix */
            if (cgp_close(fn1))
                cgp_error_exit();

            /* Write to file 2 - should still work in parallel mode
             * This would fail previously */
            for (i = 0; i < 10; i++)
                coord_array[i] = (double)(comm_rank * 10 + i + 100);

            if (cgp_coord_write_data(fn2, B, Z, C, rmin, rmax, coord_array))
                cgp_error_exit();

            if (cgp_close(fn2))
                cgp_error_exit();

            if (comm_rank == 0)
                printf("  Multiple parallel file test passed\n");
        }

	err = MPI_Finalize();
	if(err!=MPI_SUCCESS) cgp_doError;
	return err;
	}

