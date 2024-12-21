#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "pcgnslib.h"
#include "utils.h"
#include "mpi.h"

#define MIN_COUNT 40320 /* 8! */

#ifdef DEBUG_MPI
# define DEBUG_PRINT(A) printf A;fflush(stdout);
#else
# define DEBUG_PRINT(A)
#endif

MPI_Comm comm = MPI_COMM_WORLD;
int comm_size, comm_rank;
/*    int scale_factor = 10; */
int scale_factor = 400;

static int pcgns_ctest()
{
    int F, B, Z, E, S;
    int Cx, Cy, Cz, Fx, Fy, Fz, Ax, Ay, Az;
    int i, j, n, nb, nz, n_per_proc, errs;
    double *x, *y, *z;
    cgsize_t sizes[3], *e, start, end;
    char name[33];
    double ts, tt, data_size;
    int total_count;
    static char *piomode[2] = {"independent", "collective"};

    total_count = scale_factor * MIN_COUNT;
    n_per_proc = total_count / comm_size;
    x = (double *)malloc(n_per_proc * sizeof(double));
    y = (double *)malloc(n_per_proc * sizeof(double));
    z = (double *)malloc(n_per_proc * sizeof(double));
    e = (cgsize_t *)malloc(4 * n_per_proc * sizeof(cgsize_t));

    start = comm_rank * n_per_proc + 1;
    end   = start + n_per_proc - 1;
    for (j = 0, n = 0; n < n_per_proc; n++) {
        x[n] = start + n;
        y[n] = comm_rank + 1;
        z[n] = n + 1;
        for (i = 0; i < 4; i++, j++)
            e[j] = start + n;
    }
    sizes[0] = total_count;
    sizes[1] = total_count;
    sizes[2] = 0;

    data_size = (9.0 * total_count * sizeof(double) +
                 4.0 * total_count * sizeof(cgsize_t)) /
                 (1024.0 * 1024.0);
    if (comm_rank == 0) {
        printf("number processes       = %d\n", comm_size);
        printf("array size per process = %d\n", n_per_proc);
        printf("total array size       = %d\n", total_count);
        printf("total Mb for all data  = %lf\n", data_size);
#ifdef DEBUG_MPI
        fflush(stdout);
#endif
    }

    /* the default here is to use MPI_COMM_WORLD,
       but this allows assigning of any communicator */
    cgp_mpi_comm(comm);

    DEBUG_PRINT(("[%d]cgp_open(write)\n",comm_rank))
    if (cgp_open("pcgns_ctest.cgns", CG_MODE_WRITE, &F))
        cgp_error_exit();

    for (nb = 0; nb < 2; nb++) {
        sprintf(name, "Base %d", nb + 1);
        DEBUG_PRINT(("[%d]cg_base_write(%s)\n",comm_rank,name))
        if (cg_base_write(F,name,3,3,&B)) cgp_error_exit();
        DEBUG_PRINT(("[%d]cgp_pio_mode(%s)\n",comm_rank,piomode[nb]))
	if (cgp_pio_mode((CGNS_ENUMT(PIOmode_t))nb))
            cgp_error_exit();
        for (nz = 0; nz < 2; nz++) {
            sprintf(name, "Zone %d", nz + 1);
            DEBUG_PRINT(("[%d]cg_zone_write(%s)\n",comm_rank,name))
            if (cg_zone_write(F,B,name,sizes,CGNS_ENUMV(Unstructured),&Z))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cgp_coord_write\n",comm_rank))
            if (cgp_coord_write(F,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX",&Cx) ||
                cgp_coord_write(F,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY",&Cy) ||
                cgp_coord_write(F,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ",&Cz))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cgp_section_write\n",comm_rank))
            if (cgp_section_write(F,B,Z,"Tets",CGNS_ENUMV(TETRA_4),1,total_count,0,&E))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cg_sol_write\n",comm_rank))
            if (cg_sol_write(F,B,Z,"Solution",CGNS_ENUMV(Vertex),&S))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cgp_field_write\n",comm_rank))
            if (cgp_field_write(F,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumX",&Fx) ||
                cgp_field_write(F,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumY",&Fy) ||
                cgp_field_write(F,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumZ",&Fz))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cg_goto(%s)\n",comm_rank,name))
            if (cg_goto(F,B,name,0,NULL))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cg_user_data_write\n",comm_rank))
            if (cg_user_data_write("User Data"))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cg_gorel(user data)\n",comm_rank))
            if (cg_gorel(F, "User Data", 0, NULL))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cgp_array_write\n",comm_rank))
            if (cgp_array_write("ArrayX",CGNS_ENUMV(RealDouble),1,sizes,&Ax) ||
                cgp_array_write("ArrayY",CGNS_ENUMV(RealDouble),1,sizes,&Ay) ||
                cgp_array_write("ArrayZ",CGNS_ENUMV(RealDouble),1,sizes,&Az))
                cgp_error_exit();

            MPI_Barrier(comm);
            ts = MPI_Wtime();

            DEBUG_PRINT(("[%d]cgp_coord_write_data\n",comm_rank))
            if (cgp_coord_write_data(F,B,Z,Cx,&start,&end,x) ||
                cgp_coord_write_data(F,B,Z,Cy,&start,&end,y) ||
                cgp_coord_write_data(F,B,Z,Cz,&start,&end,z))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cgp_elements_write_data\n",comm_rank))
            if (cgp_elements_write_data(F,B,Z,E,start,end,e))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cgp_field_write_data\n",comm_rank))
            if (cgp_field_write_data(F,B,Z,S,Fx,&start,&end,x) ||
                cgp_field_write_data(F,B,Z,S,Fy,&start,&end,y) ||
                cgp_field_write_data(F,B,Z,S,Fz,&start,&end,z))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cg_goto(UserDefinedData_t)\n",comm_rank))
            if (cg_goto(F,B,"Zone_t",Z,"UserDefinedData_t",1,NULL))
                cgp_error_exit();
            DEBUG_PRINT(("[%d]cgp_array_write_data\n",comm_rank))
            if (cgp_array_write_data(Ax,&start,&end,x) ||
                cgp_array_write_data(Ay,&start,&end,y) ||
                cgp_array_write_data(Az,&start,&end,z))
                cgp_error_exit();


            MPI_Barrier(comm);
            tt = MPI_Wtime() - ts;
            if (comm_rank == 0) {
                printf("write: %lf secs, %lf Mb/sec (%s)\n",
                    tt, data_size / tt, piomode[nb]);
#ifdef DEBUG_MPI
                fflush(stdout);
#endif
            }
        }
    }

    DEBUG_PRINT(("[%d]cgp_close\n",comm_rank))
    if (cgp_close(F)) cgp_error_exit();
    DEBUG_PRINT(("[%d]cgp_open(read)\n",comm_rank))
    if (cgp_open("pcgns_ctest.cgns", CG_MODE_READ, &F)) cgp_error_exit();

    Z = S = E = 1;
    for (nb = 0; nb < 2; nb++) {
        B = nb + 1;
        DEBUG_PRINT(("[%d]cgp_pio_mode(%s)\n",comm_rank,piomode[nb]))
        if (cgp_pio_mode((CGNS_ENUMT(PIOmode_t))nb))
            cgp_error_exit();

        MPI_Barrier(comm);
        ts = MPI_Wtime();

        DEBUG_PRINT(("[%d]cgp_coord_read_data\n",comm_rank))
        if (cgp_coord_read_data(F,B,Z,1,&start,&end,x) ||
            cgp_coord_read_data(F,B,Z,2,&start,&end,y) ||
            cgp_coord_read_data(F,B,Z,3,&start,&end,z) ||
            cgp_elements_read_data(F,B,Z,E,start,end,e)) cgp_error_exit();

        MPI_Barrier(comm);
        tt = MPI_Wtime() - ts;

        errs = 0;
        for (j = 0, n = 0; n < n_per_proc; n++) {
            if (x[n] != (double)(start + n)) errs++;
            if (y[n] != (double)(comm_rank + 1)) errs++;
            if (z[n] != (double)(n + 1)) errs++;
            for (i = 0; i < 4; i++, j++) {
                if (e[j] != (start + n)) errs++;
            }
        }

        MPI_Barrier(comm);
        ts = MPI_Wtime();

        DEBUG_PRINT(("[%d]cgp_field_read_data\n",comm_rank))
        if (cgp_field_read_data(F,B,Z,S,1,&start,&end,x) ||
            cgp_field_read_data(F,B,Z,S,2,&start,&end,y) ||
            cgp_field_read_data(F,B,Z,S,3,&start,&end,z)) cgp_error_exit();

        MPI_Barrier(comm);
        tt += (MPI_Wtime() - ts);

        for (n = 0; n < n_per_proc; n++) {
            if (x[n] != (double)(start + n)) errs++;
            if (y[n] != (double)(comm_rank + 1)) errs++;
            if (z[n] != (double)(n + 1)) errs++;
        }

        MPI_Barrier(comm);
        ts = MPI_Wtime();

        DEBUG_PRINT(("[%d]cg_goto(UserDefinedData_t)\n",comm_rank))
        if (cg_goto(F,B,"Zone_t",Z,"UserDefinedData_t",1,NULL))
            cgp_error_exit();
        DEBUG_PRINT(("[%d]cgp_array_read_data\n",comm_rank))
        if (cgp_array_read_data(1,&start,&end,x) ||
            cgp_array_read_data(2,&start,&end,y) ||
            cgp_array_read_data(3,&start,&end,z)) cgp_error_exit();

        MPI_Barrier(comm);
        tt += (MPI_Wtime() - ts);

        for (n = 0; n < n_per_proc; n++) {
            if (x[n] != (double)(start + n)) errs++;
            if (y[n] != (double)(comm_rank + 1)) errs++;
            if (z[n] != (double)(n + 1)) errs++;
        }

        if (comm_rank == 0) {
            printf("read : %lf secs, %lf Mb/sec (%s) errors=%d\n",
                tt, data_size / tt, piomode[nb], errs);
#ifdef DEBUG_MPI
            fflush(stdout);
#endif
        }
    }

    DEBUG_PRINT(("[%d]cgp_close\n",comm_rank))
    cgp_close(F);

    return 0;

}

static int multisets()
{
  char fname[32];

  void **buf;
  int Cvec[3];
  int Fvec[3];
  int Avec[2];

  cgsize_t Nelem = 1024; /* DEFAULT */
  cgsize_t NodePerElem = 6;

  cgsize_t Nnodes;

  int fn;
  int B;
  int Z;
  int S;
  int Cx,Cy,Cz, Fx, Fy, Fz, Ar, Ai;
  int cell_dim = 3;
  int phys_dim = 3;
  int r_cell_dim = 0;
  int r_phys_dim = 0;
  cgsize_t nijk[3], sizes[3];
  cgsize_t size_1D[1];
  cgsize_t min, max;
  cgsize_t k, count;
  /* For writing and reading data*/
  double* Coor_x;
  double* Coor_y;
  double* Coor_z;
  double* Data_Fx;
  double* Data_Fy;
  double* Data_Fz;
  double* Array_r;
  cgsize_t* Array_i;
  cgsize_t start, end, emin, emax;
  cgsize_t* elements;
  char name[33];
  int piomode = CGP_COLLECTIVE; /* DEFAULT */
  int err;

  err = (int)cgp_pio_mode((CGNS_ENUMT(PIOmode_t))piomode);
  if(err != CG_OK) {
    printf("*FAILED* cgp_pio_mode \n");
    cgp_error_exit();
  }

  Nnodes = Nelem*NodePerElem;

  nijk[0] = Nnodes; /* Number of vertices */
  nijk[1] = Nelem; /* Number of cells */
  nijk[2] = 0; /* Number of boundary vertices */

  /* ====================================== */
  /* ==    **WRITE THE CGNS FILE **      == */
  /* ====================================== */

  sprintf(fname, "cmultiset_%06d.cgns", comm_size);

  if(cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
    printf("*FAILED* cgp_open \n");
    cgp_error_exit();
  }

  if(cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) != CG_OK) {
    printf("*FAILED* cg_base_write \n");
    cgp_error_exit();
  }
  if(cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z) != CG_OK) {
    printf("*FAILED* cg_zone_write \n");
    cgp_error_exit();
  }

  /* ====================================== */
  /* == (A) WRITE THE NODAL COORDINATES  == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_x \n");
    cgp_error_exit();
  }

  if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_y \n");
    cgp_error_exit();
  }

  if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_z \n");
    cgp_error_exit();
  }

  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  for (k=0; k < count; k++) {
    Coor_x[k] = comm_rank*count + k + 1.1;
    Coor_y[k] = Coor_x[k] + 0.1;
    Coor_z[k] = Coor_y[k] + 0.1;
  }

  if(cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX",&Cx) != CG_OK) {
    printf("*FAILED* cgp_coord_write (Coor_x) \n");
    cgp_error_exit();
  }
  if(cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY",&Cy) != CG_OK) {
    printf("*FAILED* cgp_coord_write (Coor_y) \n");
    cgp_error_exit();
  }
  if(cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ",&Cz) != CG_OK) {
    printf("*FAILED* cgp_coord_write (Coor_z) \n");
    cgp_error_exit();
  }

  Cvec[0] = Cx;
  Cvec[1] = Cy;
  Cvec[2] = Cz;

  buf = (void *)malloc(3*sizeof(void *));

  buf[0] =&Coor_x[0];
  buf[1] =&Coor_y[0];
  buf[2] =&Coor_z[0];

  if(cgp_coord_multi_write_data(fn, B, Z, Cvec, &min,&max,3,(const void **)buf)!= CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_coord_multi_write_data", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_coord_multi_write_data", NULL);
  }

  free(buf);

  free(Coor_x);
  free(Coor_y);
  free(Coor_z);

  /* ====================================== */
  /* == (B) WRITE THE CONNECTIVITY TABLE == */
  /* ====================================== */

  start = 1;
  end = nijk[1];

  if(cgp_section_write(fn,B,Z,"Elements",CGNS_ENUMV(PENTA_6),start,end,0,&S) != CG_OK) {
    printf("*FAILED* cgp_section_write \n");
    cgp_error_exit();
  }

  count = nijk[1]/comm_size;

  if( !(elements = malloc(count*NodePerElem*sizeof(cgsize_t)) )) {
    printf("*FAILED* allocation of elements \n");
    cgp_error_exit();
  }

  /* Create ridiculous connectivity table ... */
  for ( k = 0; k < count*NodePerElem; k++) {
    elements[k] = comm_rank*count*NodePerElem + k + 1;
  }

  emin = count*comm_rank+1;
  emax = count*(comm_rank+1);

  if(cgp_elements_write_data(fn, B, Z, S, emin, emax, elements) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_elements_write_data (elements)", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_elements_write_data (elements)", NULL);

  }
  free(elements);

  /* ====================================== */
  /* == (C) WRITE THE FIELD DATA         == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fx \n");
    cgp_error_exit();
  }

  if( !(Data_Fy= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fy \n");
    cgp_error_exit();
  }

  if( !(Data_Fz= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Data_Fz \n");
    cgp_error_exit();
  }

  for ( k = 0; k < count; k++) {
     Data_Fx[k] = comm_rank*count+k + 1.01;
     Data_Fy[k] = comm_rank*count+k + 1.02;
     Data_Fz[k] = comm_rank*count+k + 1.03;
  }

  if(cg_sol_write(fn, B, Z, "Solution", CGNS_ENUMV(Vertex), &S) != CG_OK) {
    printf("*FAILED* cg_sol_write \n");
    cgp_error_exit();
  }

  if(cgp_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumX",&Fx) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumX) \n");
    cgp_error_exit();
  }
  if(cgp_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumY",&Fy) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumY) \n");
    cgp_error_exit();
  }
  if(cgp_field_write(fn,B,Z,S,CGNS_ENUMV(RealDouble),"MomentumZ",&Fz) != CG_OK) {
    printf("*FAILED* cgp_field_write (MomentumZ) \n");
    cgp_error_exit();
  }

  Fvec[0] = Fx;
  Fvec[1] = Fy;
  Fvec[2] = Fz;

  buf = (void *)malloc(3*sizeof(void *));
  buf[0] = &Data_Fx[0];
  buf[1] = &Data_Fy[0];
  buf[2] = &Data_Fz[0];

  if(cgp_field_multi_write_data(fn,B,Z,S,Fvec,&min,&max,3,(const void **)buf) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_field_multi_write_data (Momentum)", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_field_multi_write_data (Momentum)", NULL);
  }
  free(buf);

  free(Data_Fx);
  free(Data_Fy);
  free(Data_Fz);

  /* ====================================== */
  /* == (D) WRITE THE ARRAY DATA         == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Array_r = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Array_r \n");
    cgp_error_exit();
  }

  if( !(Array_i= (cgsize_t*) malloc(count*sizeof(cgsize_t))) ) {
    printf("*FAILED* allocation of Array_i  \n");
    cgp_error_exit();
  }

  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  for ( k = 0; k < count; k++) {
    Array_r[k] = comm_rank*count + k + 1.001;
    Array_i[k] = comm_rank*count + k + 1;
  }

  if(cg_goto(fn, B, "Zone 1", 0, "end") != CG_OK) {
    printf("*FAILED* cg_goto\n");
    cgp_error_exit();
  }

  if(cg_user_data_write("User Data") != CG_OK) {
    printf("*FAILED* cg_user_data_write \n");
    cgp_error_exit();
  }

  if(cg_gorel(fn,"User Data",0,"end") != CG_OK) {
    printf("*FAILED* cg_gorel\n");
    cgp_error_exit();
  }

  size_1D[0] = nijk[0];
  if(cgp_array_write("ArrayR",CGNS_ENUMV(RealDouble),1,size_1D,&Ar) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array_Ar)\n");
    cgp_error_exit();
  }

#if CG_BUILD_64BIT
  if(cgp_array_write("ArrayI",CGNS_ENUMV(LongInteger),1,size_1D,&Ai) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array_Ai)\n");
    cgp_error_exit();
  }
#else
  if(cgp_array_write("ArrayI",CGNS_ENUMV(Integer),1,size_1D,&Ai) != CG_OK) {
    printf("*FAILED* cgp_array_write (Array Ai)\n");
    cgp_error_exit();
  }
#endif

  Avec[0] = Ai;
  Avec[1] = Ar;

  buf = (void *)malloc(2*sizeof(void *));
  buf[0] = &Array_i[0];
  buf[1] = &Array_r[0];

  if(cgp_array_multi_write_data(fn, Avec,&min,&max, 2, (const void **)buf) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_field_array_data (Array_A)", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_field_array_data (Array_A)", NULL);
  }
  free(buf);
  free(Array_r);
  free(Array_i);

  if(cgp_close(fn) != CG_OK) {
    printf("*FAILED* cgp_close \n");
    cgp_error_exit();
  };

  /* ====================================== */
  /* ==    **  READ THE CGNS FILE **     == */
  /* ====================================== */
  MPI_Barrier(MPI_COMM_WORLD);

  /* Open the cgns file for reading */
  if(cgp_open(fname, CG_MODE_MODIFY, &fn) != CG_OK) {
    printf("*FAILED* cgp_open \n");
    cgp_error_exit();
  }

  /* Read the base information */
  if(cg_base_read(fn, B, name, &r_cell_dim, &r_phys_dim) != CG_OK) {
    printf("*FAILED* cg_base_read\n");
    cgp_error_exit();
  }

  if(r_cell_dim != cell_dim || r_phys_dim != phys_dim) {
    printf("*FAILED* bad cell dim=%d or phy dim=%d\n", r_cell_dim, r_phys_dim);
    cgp_error_exit();
  }

  if (strcmp (name, "Base 1")) {
    printf("*FAILED* bad base name=%s\n", name);
    cgp_error_exit();
  }
  /* Read the zone information */
  if(cg_zone_read(fn, B, Z, name, sizes) != CG_OK) {
    printf("*FAILED* cg_zone_read\n");
    cgp_error_exit();
  }

  /* Check the read zone information is correct */
  if(sizes[0] != Nnodes) {
    printf("bad num points=%ld\n", (long)sizes[0]);
    cgp_error_exit();
  }

  if(sizes[1] != Nelem) {
    printf("bad num points=%ld\n", (long)sizes[1]);
    cgp_error_exit();
  }

  if(sizes[2] != 0) {
    printf("bad num points=%ld\n", (long)sizes[2]);
    cgp_error_exit();
  }

  if (strcmp (name, "Zone 1")) {
    printf("bad zone name=%s\n", name);
    cgp_error_exit();
  }
  /* ====================================== */
  /* ==  (A) READ THE NODAL COORDINATES  == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_x \n");
    cgp_error_exit();
  }

  if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_y \n");
    cgp_error_exit();
  }

  if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Coor_z \n");
    cgp_error_exit();
  }
  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  Cvec[0] = Cx;
  Cvec[1] = Cy;
  Cvec[2] = Cz;

  buf = (void *)malloc(3*sizeof(void *));
  buf[0] = &Coor_x[0];
  buf[1] = &Coor_y[0];
  buf[2] = &Coor_z[0];

  if (cgp_coord_multi_read_data(fn, B, Z, Cvec, &min,&max, 3, buf)!= CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_coord_multi_read_data", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_coord_multi_read_data", NULL);
  }
  free(buf);

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if( !compareValuesDouble(Coor_x[k], comm_rank*count + k + 1.1) ||
        !compareValuesDouble(Coor_y[k], Coor_x[k] + 0.1) ||
        !compareValuesDouble(Coor_z[k], Coor_y[k] + 0.1) ) {
      if(comm_rank == 0) write_test_status(FAILED, "Check cgp_coord_multi_read_data values", NULL);
      cgp_error_exit();
    }
  }
  if(comm_rank == 0) write_test_status(PASSED, "Check cgp_coord_multi_read_data values", NULL);

  free(Coor_x);
  free(Coor_y);
  free(Coor_z);

/* ====================================== */
/* == (B) READ THE CONNECTIVITY TABLE  == */
/* ====================================== */

  count = nijk[1]/comm_size;
  if( !(elements = malloc(count*NodePerElem*sizeof(cgsize_t)) )) {
    printf("*FAILED* allocation of elements \n");
    cgp_error_exit();
  }

  emin = count*comm_rank+1;
  emax = count*(comm_rank+1);

  if( cgp_elements_read_data(fn, B, Z, S, emin, emax, elements) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_elements_read_data ( Reading elements)", NULL);
    cgp_error_exit();
  }
  if(comm_rank == 0) write_test_status(PASSED, "Test cgp_elements_read_data ( Reading elements)", NULL);

  for ( k = 0; k < count; k++) {
    if(elements[k] != comm_rank*count*NodePerElem + k + 1) {
      if(comm_rank == 0) write_test_status(FAILED, "Check cgp_elements_read_data values", NULL);
      cgp_error_exit();
    }
  }
  if(comm_rank == 0) write_test_status(PASSED, "Check cgp_elements_read_data values", NULL);
  free(elements);

  /* ====================================== */
  /* == (C) READ THE FIELD DATA          == */
  /* ====================================== */
  count = nijk[0]/comm_size;

  if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Reading Data_Fx \n");
    cgp_error_exit();
  }

  if( !(Data_Fy = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Reading Data_Fy \n");
    cgp_error_exit();
  }

  if( !(Data_Fz = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of Reading Data_Fz \n");
    cgp_error_exit();
  }

  Fvec[0] = Fx;
  Fvec[1] = Fy;
  Fvec[2] = Fz;

  buf = (void *)malloc(3*sizeof(void *));
  buf[0] = &Data_Fx[0];
  buf[1] = &Data_Fy[0];
  buf[2] = &Data_Fz[0];

  if(cgp_field_multi_read_data(fn,B,Z,S,Fvec,&min,&max,3,buf) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_field_multi_read_data", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_field_multi_read_data", NULL);
  }
  free(buf);

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if(!compareValuesDouble(Data_Fx[k], comm_rank*count + k + 1.01) ||
       !compareValuesDouble(Data_Fy[k], comm_rank*count + k + 1.02) ||
       !compareValuesDouble(Data_Fz[k], comm_rank*count + k + 1.03) ) {
      if(comm_rank == 0) write_test_status(FAILED, "Check cgp_field_multi_read_data values", NULL);
      cgp_error_exit();
    }
  }
  if(comm_rank == 0) write_test_status(PASSED, "Check cgp_field_multi_read_data values", NULL);

  free(Data_Fx);
  free(Data_Fy);
  free(Data_Fz);

  /* ====================================== */
  /* == (D) READ THE ARRAY DATA          == */
  /* ====================================== */

  count = nijk[0]/comm_size;

  if( !(Array_r = (double*) malloc(count*sizeof(double))) ) {
    printf("*FAILED* allocation of  Reading Array_r \n");
    cgp_error_exit();
  }

  if( !(Array_i= (cgsize_t*) malloc(count*sizeof(cgsize_t))) ) {
    printf("*FAILED* allocation of  Reading Array_i  \n");
    cgp_error_exit();
  }

  min = count*comm_rank+1;
  max = count*(comm_rank+1);

  if(cg_goto(fn,B,"Zone_t",Z,"UserDefinedData_t",1,"end") != CG_OK) {
    printf("*FAILED* cg_goto (User Defined Data)\n");
    cgp_error_exit();
  }

  Avec[0] = Ar;
  Avec[1] = Ai;

  buf = (void *)malloc(3*sizeof(void *));
  buf[0] = &Array_r[0];
  buf[1] = &Array_i[0];

  if( cgp_array_multi_read_data(fn, Avec, &min, &max, 2, buf) != CG_OK) {
    if(comm_rank == 0) write_test_status(FAILED, "Test cgp_array_multi_read_data", NULL);
    cgp_error_exit();
  } else {
    if(comm_rank == 0) write_test_status(PASSED, "Test cgp_array_multi_read_data", NULL);
  }
  free(buf);

  /* Check if read the data back correctly */
  for ( k = 0; k < count; k++) {
    if(!compareValuesDouble(Array_r[k], comm_rank*count + k + 1.001) ||
       Array_i[k] != comm_rank*count + k +1) {
      if(comm_rank == 0) write_test_status(FAILED, "Check cgp_array_multi_read_data values", NULL);
      cgp_error_exit();
    }
  }
  if(comm_rank == 0) write_test_status(PASSED, "Check cgp_array_multi_read_data values", NULL);

  free(Array_r);
  free(Array_i);

  if(cgp_close(fn) !=CG_OK) {
     printf("*FAILED* cgp_close\n");
     cgp_error_exit();
  }

  return 0;

}

int particle_test()
{
   char fname[32];
   int fn;
   int B;
   int P;
   int S;
   int Cx,Cy,Cz, Fx, Fy, Fz;
   int cell_dim = 3;
   int phys_dim = 3;
   int r_cell_dim = 0;
   int r_phys_dim = 0;
   cgsize_t num_particles = 1024, size;
   cgsize_t min, max;
   cgsize_t k, count;
   /* For writing and reading data*/
   double* Coor_x;
   double* Coor_y;
   double* Coor_z;
   double* Data_Fx;
   double* Data_Fy;
   double* Data_Fz;
   char name[33];
   int piomode = CGP_COLLECTIVE; /* DEFAULT */
   int err;

   err = (int)cgp_pio_mode((CGNS_ENUMT(PIOmode_t))piomode);
   if(err != CG_OK) {
     printf("*FAILED* cgp_pio_mode \n");
     cgp_error_exit();
   }

   /* ====================================== */
   /* ==    **WRITE THE CGNS FILE **      == */
   /* ====================================== */

   sprintf(fname, "particle_test_%06d.cgns", comm_size);

   if(cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
     printf("*FAILED* cgp_open \n");
     cgp_error_exit();
   }

   if(cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) != CG_OK) {
     printf("*FAILED* cg_base_write \n");
     cgp_error_exit();
   }
   if(cg_particle_write(fn, B, "Particle 1", num_particles, &P) != CG_OK) {
     printf("*FAILED* cg_particle_write \n");
     cgp_error_exit();
   }

   /* ======================================== */
   /* == (A) WRITE THE PARTICLE COORDINATES == */
   /* ======================================== */

   count = num_particles/comm_size;

   if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_x \n");
     cgp_error_exit();
   }

   if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_y \n");
     cgp_error_exit();
   }

   if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_z \n");
     cgp_error_exit();
   }

   min = count*comm_rank+1;
   max = count*(comm_rank+1);

   for (k=0; k < count; k++) {
     Coor_x[k] = comm_rank*count + k + 1.1;
     Coor_y[k] = Coor_x[k] + 0.1;
     Coor_z[k] = Coor_y[k] + 0.1;
   }

   if(cgp_particle_coord_write(fn,B,P,CGNS_ENUMV(RealDouble),"CoordinateX",&Cx) != CG_OK) {
     printf("*FAILED* cgp_particle_coord_write (Coor_x) \n");
     cgp_error_exit();
   }
   if(cgp_particle_coord_write(fn,B,P,CGNS_ENUMV(RealDouble),"CoordinateY",&Cy) != CG_OK) {
     printf("*FAILED* cgp_particle_coord_write (Coor_y) \n");
     cgp_error_exit();
   }
   if(cgp_particle_coord_write(fn,B,P,CGNS_ENUMV(RealDouble),"CoordinateZ",&Cz) != CG_OK) {
     printf("*FAILED* cgp_particle_coord_write (Coor_z) \n");
     cgp_error_exit();
   }

   if(cgp_particle_coord_write_data(fn, B, P, Cx, &min, &max, Coor_x) != CG_OK) {
      printf("*FAILED* cgp_particle_coord_write_data (Coor_x) \n");
      cgp_error_exit();
   }
   if(cgp_particle_coord_write_data(fn, B, P, Cy, &min, &max, Coor_y) != CG_OK) {
      printf("*FAILED* cgp_particle_coord_write_data (Coor_y) \n");
      cgp_error_exit();
   }

   int m_numdim = 1;
   cgsize_t s_rmin[1], s_rmax[1]; // Indices in the file; dim = m_numdim
   cgsize_t m_rmin[1], m_rmax[1]; // Indices in memory corresponding to Coor_z; dim = m_numdim
   cgsize_t m_arg_dimvals = count;

   s_rmin[0] = min ;
   s_rmax[0] = max;
   m_rmin[0] = 1;
   m_rmax[0] = count;

   if(cgp_particle_coord_general_write_data(fn, B, P, Cz, s_rmin, s_rmax, CGNS_ENUMV(RealDouble),
                                            m_numdim, &m_arg_dimvals, m_rmin, m_rmax, Coor_z) != CG_OK) {
      printf("*FAILED* cgp_particle_coord_general_write_data (Coor_z) \n");
      cgp_error_exit();
   }

   free(Coor_x);
   free(Coor_y);
   free(Coor_z);

   /* ======================================= */
   /* == (B) WRITE THE PARTICLE FIELD DATA == */
   /* ======================================= */

   count = num_particles/comm_size;

   if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Data_Fx \n");
     cgp_error_exit();
   }

   if( !(Data_Fy= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Data_Fy \n");
     cgp_error_exit();
   }

   if( !(Data_Fz= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Data_Fz \n");
     cgp_error_exit();
   }

   for ( k = 0; k < count; k++) {
      Data_Fx[k] = comm_rank*count+k + 1.01;
      Data_Fy[k] = comm_rank*count+k + 1.02;
      Data_Fz[k] = comm_rank*count+k + 1.03;
   }

   if(cg_particle_sol_write(fn, B, P, "Solution", &S) != CG_OK) {
     printf("*FAILED* cg_particle_sol_write \n");
     cgp_error_exit();
   }

   if(cgp_particle_field_write(fn,B,P,S,CGNS_ENUMV(RealDouble),"VelocityX",&Fx) != CG_OK) {
     printf("*FAILED* cgp_particle_field_write (VelocityX) \n");
     cgp_error_exit();
   }
   if(cgp_particle_field_write(fn,B,P,S,CGNS_ENUMV(RealDouble),"VelocityY",&Fy) != CG_OK) {
     printf("*FAILED* cgp_particle_field_write (VelocityY) \n");
     cgp_error_exit();
   }
   if(cgp_particle_field_write(fn,B,P,S,CGNS_ENUMV(RealDouble),"VelocityZ",&Fz) != CG_OK) {
     printf("*FAILED* cgp_particle_field_write (VelocityZ) \n");
     cgp_error_exit();
   }

   if(cgp_particle_field_write_data(fn, B, P, S, Fx, &min, &max, Data_Fx) != CG_OK) {
      printf("*FAILED* cgp_particle_field_write_data (VelocityX) \n");
      cgp_error_exit();
   }
   if(cgp_particle_field_write_data(fn, B, P, S, Fy, &min, &max, Data_Fy) != CG_OK) {
      printf("*FAILED* cgp_particle_field_write_data (VelocityY) \n");
      cgp_error_exit();
   }

   if(cgp_particle_field_general_write_data(fn, B, P, S, Fz, s_rmin, s_rmax, CGNS_ENUMV(RealDouble), m_numdim, &m_arg_dimvals, m_rmin, m_rmax, Data_Fz) != CG_OK) {
      printf("*FAILED* cgp_particle_field_general_write_data (VelocityZ) \n");
      cgp_error_exit();
   }

   free(Data_Fx);
   free(Data_Fy);
   free(Data_Fz);

   if(cgp_close(fn) !=CG_OK) {
      printf("*FAILED* cgp_close\n");
      cgp_error_exit();
   }

   /* ====================================== */
   /* ==    **  READ THE CGNS FILE **     == */
   /* ====================================== */
   MPI_Barrier(MPI_COMM_WORLD);

   /* Open the cgns file for reading */
   if(cgp_open(fname, CG_MODE_MODIFY, &fn) != CG_OK) {
     printf("*FAILED* cgp_open \n");
     cgp_error_exit();
   }

   /* Read the base information */
   if(cg_base_read(fn, B, name, &r_cell_dim, &r_phys_dim) != CG_OK) {
     printf("*FAILED* cg_base_read\n");
     cgp_error_exit();
   }

   if(r_cell_dim != cell_dim || r_phys_dim != phys_dim) {
     printf("*FAILED* bad cell dim=%d or phy dim=%d\n", r_cell_dim, r_phys_dim);
     cgp_error_exit();
   }

   if (strcmp (name, "Base 1")) {
     printf("*FAILED* bad base name=%s\n", name);
     cgp_error_exit();
   }
   /* Read the particle zone information */
   if(cg_particle_read(fn, B, P, name, &size) != CG_OK) {
     printf("*FAILED* cg_particle_read\n");
     cgp_error_exit();
   }

   /* Check the read zone information is correct */
   if(size != num_particles) {
     printf("bad num points=%ld\n", (long)size);
     cgp_error_exit();
   }

   if (strcmp (name, "Particle 1")) {
     printf("bad zone name=%s\n", name);
     cgp_error_exit();
   }
   /* ========================================= */
   /* ==  (A) READ THE PARTICLE COORDINATES  == */
   /* ========================================= */

   count = num_particles/comm_size;

   if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_x \n");
     cgp_error_exit();
   }

   if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_y \n");
     cgp_error_exit();
   }

   float* Coor_z_float;
   if( !(Coor_z_float= (float*) malloc(count*sizeof(float))) ) {
     printf("*FAILED* allocation of particle Coor_z_float \n");
     cgp_error_exit();
   }
   min = count*comm_rank+1;
   max = count*(comm_rank+1);

   if (cgp_particle_coord_read_data(fn, B, P, Cx, &min, &max, Coor_x)!= CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_coord_read_data (Coor_x)", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_coord_read_data (Coor_x)", NULL);
   }

   if (cgp_particle_coord_read_data(fn, B, P, Cy, &min, &max, Coor_y)!= CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_coord_read_data (Coor_y)", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_coord_read_data (Coor_y)", NULL);
   }

   /* Have the MLL convert the data to float while reading */
   if (cgp_particle_coord_general_read_data(fn, B, P, Cz, s_rmin, s_rmax, CGNS_ENUMV(RealSingle), m_numdim, &m_arg_dimvals, m_rmin, m_rmax, Coor_z_float)!= CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_coord_general_read_data (Coor_z_float)", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_coord_general_read_data (Coor_z_float)", NULL);
   }

   /* Check if read the data back correctly */
   for ( k = 0; k < count; k++) {
     if( !compareValuesDouble(Coor_x[k], comm_rank*count + k + 1.1) ||
         !compareValuesDouble(Coor_y[k], Coor_x[k] + 0.1) ||
         !compareValuesFloat(Coor_z_float[k], (float)(Coor_y[k] + 0.1)) ) {
       if(comm_rank == 0) write_test_status(FAILED, "Check cgp_particle_coord_read_data values", NULL);
       cgp_error_exit();
     }
   }
   if(comm_rank == 0) write_test_status(PASSED, "Check cgp_particle_coord_read_data values", NULL);

   free(Coor_x);
   free(Coor_y);
   free(Coor_z_float);

   /* ====================================== */
   /* == (B) READ THE PARTICLE FIELD DATA == */
   /* ====================================== */
   count = num_particles/comm_size;

   if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Reading Data_Fx \n");
     cgp_error_exit();
   }

   if( !(Data_Fy = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Reading Data_Fy \n");
     cgp_error_exit();
   }

   float* Data_Fz_float;
   if( !(Data_Fz_float = (float*) malloc(count*sizeof(float))) ) {
     printf("*FAILED* allocation of Reading Data_Fz_float \n");
     cgp_error_exit();
   }

   if(cgp_particle_field_read_data(fn,B,P,S,Fx,&min,&max,Data_Fx) != CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_field_read_data (Data_Fx)", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_field_read_data (Data_Fx)", NULL);
   }

   if(cgp_particle_field_read_data(fn,B,P,S,Fy,&min,&max,Data_Fy) != CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_field_read_data (Data_Fy)", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_field_read_data (Data_Fy)", NULL);
   }

   if(cgp_particle_field_general_read_data(fn,B,P,S,Fz, s_rmin, s_rmax, CGNS_ENUMV(RealSingle), m_numdim, &m_arg_dimvals, m_rmin, m_rmax, Data_Fz_float) != CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_field_general_read_data (Data_Fz_float)", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_field_general_read_data (Data_Fz_float)", NULL);
   }

   /* Check if read the data back correctly */
   for ( k = 0; k < count; k++) {
     if(!compareValuesDouble(Data_Fx[k], comm_rank*count + k + 1.01) ||
        !compareValuesDouble(Data_Fy[k], comm_rank*count + k + 1.02) ||
        !compareValuesFloat(Data_Fz_float[k], (float)(comm_rank*count + k + 1.03)) ) {
       if(comm_rank == 0) write_test_status(FAILED, "Check cgp_particle_field_read_data values", NULL);
       cgp_error_exit();
     }
   }
   if(comm_rank == 0) write_test_status(PASSED, "Check cgp_particle_field_read_data values", NULL);

   free(Data_Fx);
   free(Data_Fy);
   free(Data_Fz_float);

   if(cgp_close(fn) !=CG_OK) {
      printf("*FAILED* cgp_close\n");
      cgp_error_exit();
   }

   return 0;
}

int particle_multisets()
{
   char fname[32];

   void **buf;
   int Cvec[3];
   int Fvec[3];
   int fn;
   int B;
   int P;
   int S;
   int Cx,Cy,Cz, Fx, Fy, Fz;
   int cell_dim = 3;
   int phys_dim = 3;
   int r_cell_dim = 0;
   int r_phys_dim = 0;
   cgsize_t num_particles = 1024, size;
   cgsize_t min, max;
   cgsize_t k, count;
   /* For writing and reading data*/
   double* Coor_x;
   double* Coor_y;
   double* Coor_z;
   double* Data_Fx;
   double* Data_Fy;
   double* Data_Fz;
   char name[33];
   int piomode = CGP_COLLECTIVE; /* DEFAULT */
   int err;

   err = (int)cgp_pio_mode((CGNS_ENUMT(PIOmode_t))piomode);
   if(err != CG_OK) {
     printf("*FAILED* cgp_pio_mode \n");
     cgp_error_exit();
   }

   /* ====================================== */
   /* ==    **WRITE THE CGNS FILE **      == */
   /* ====================================== */

   sprintf(fname, "particle_multiset_%06d.cgns", comm_size);

   if(cgp_open(fname, CG_MODE_WRITE, &fn) != CG_OK) {
     printf("*FAILED* cgp_open \n");
     cgp_error_exit();
   }

   if(cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) != CG_OK) {
     printf("*FAILED* cg_base_write \n");
     cgp_error_exit();
   }
   if(cg_particle_write(fn, B, "Particle 1", num_particles, &P) != CG_OK) {
     printf("*FAILED* cg_particle_write \n");
     cgp_error_exit();
   }

   /* ======================================== */
   /* == (A) WRITE THE PARTICLE COORDINATES == */
   /* ======================================== */

   count = num_particles/comm_size;

   if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_x \n");
     cgp_error_exit();
   }

   if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_y \n");
     cgp_error_exit();
   }

   if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_z \n");
     cgp_error_exit();
   }

   min = count*comm_rank+1;
   max = count*(comm_rank+1);

   for (k=0; k < count; k++) {
     Coor_x[k] = comm_rank*count + k + 1.1;
     Coor_y[k] = Coor_x[k] + 0.1;
     Coor_z[k] = Coor_y[k] + 0.1;
   }

   if(cgp_particle_coord_write(fn,B,P,CGNS_ENUMV(RealDouble),"CoordinateX",&Cx) != CG_OK) {
     printf("*FAILED* cgp_particle_coord_write (Coor_x) \n");
     cgp_error_exit();
   }
   if(cgp_particle_coord_write(fn,B,P,CGNS_ENUMV(RealDouble),"CoordinateY",&Cy) != CG_OK) {
     printf("*FAILED* cgp_particle_coord_write (Coor_y) \n");
     cgp_error_exit();
   }
   if(cgp_particle_coord_write(fn,B,P,CGNS_ENUMV(RealDouble),"CoordinateZ",&Cz) != CG_OK) {
     printf("*FAILED* cgp_particle_coord_write (Coor_z) \n");
     cgp_error_exit();
   }

   Cvec[0] = Cx;
   Cvec[1] = Cy;
   Cvec[2] = Cz;

   buf = (void *)malloc(3*sizeof(void *));

   buf[0] =&Coor_x[0];
   buf[1] =&Coor_y[0];
   buf[2] =&Coor_z[0];

   if(cgp_particle_coord_multi_write_data(fn, B, P, Cvec, &min,&max,3,(const void **)buf)!= CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_coord_multi_write_data", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_coord_multi_write_data", NULL);
   }

   free(buf);

   free(Coor_x);
   free(Coor_y);
   free(Coor_z);

   /* ======================================= */
   /* == (B) WRITE THE PARTICLE FIELD DATA == */
   /* ======================================= */

   count = num_particles/comm_size;

   if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Data_Fx \n");
     cgp_error_exit();
   }

   if( !(Data_Fy= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Data_Fy \n");
     cgp_error_exit();
   }

   if( !(Data_Fz= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Data_Fz \n");
     cgp_error_exit();
   }

   for ( k = 0; k < count; k++) {
      Data_Fx[k] = comm_rank*count+k + 1.01;
      Data_Fy[k] = comm_rank*count+k + 1.02;
      Data_Fz[k] = comm_rank*count+k + 1.03;
   }

   if(cg_particle_sol_write(fn, B, P, "Solution", &S) != CG_OK) {
     printf("*FAILED* cg_particle_sol_write \n");
     cgp_error_exit();
   }

   if(cgp_particle_field_write(fn,B,P,S,CGNS_ENUMV(RealDouble),"VelocityX",&Fx) != CG_OK) {
     printf("*FAILED* cgp_particle_field_write (VelocityX) \n");
     cgp_error_exit();
   }
   if(cgp_particle_field_write(fn,B,P,S,CGNS_ENUMV(RealDouble),"VelocityY",&Fy) != CG_OK) {
     printf("*FAILED* cgp_particle_field_write (VelocityY) \n");
     cgp_error_exit();
   }
   if(cgp_particle_field_write(fn,B,P,S,CGNS_ENUMV(RealDouble),"VelocityZ",&Fz) != CG_OK) {
     printf("*FAILED* cgp_particle_field_write (VelocityZ) \n");
     cgp_error_exit();
   }

   Fvec[0] = Fx;
   Fvec[1] = Fy;
   Fvec[2] = Fz;

   buf = (void *)malloc(3*sizeof(void *));
   buf[0] = &Data_Fx[0];
   buf[1] = &Data_Fy[0];
   buf[2] = &Data_Fz[0];

   if(cgp_particle_field_multi_write_data(fn,B,P,S,Fvec,&min,&max,3,(const void **)buf) != CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_field_multi_write_data", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_field_multi_write_data", NULL);
   }
   free(buf);

   free(Data_Fx);
   free(Data_Fy);
   free(Data_Fz);

   if(cgp_close(fn) !=CG_OK) {
      printf("*FAILED* cgp_close\n");
      cgp_error_exit();
   }

   /* ====================================== */
   /* ==    **  READ THE CGNS FILE **     == */
   /* ====================================== */
   MPI_Barrier(MPI_COMM_WORLD);

   /* Open the cgns file for reading */
   if(cgp_open(fname, CG_MODE_MODIFY, &fn) != CG_OK) {
     printf("*FAILED* cgp_open \n");
     cgp_error_exit();
   }

   /* Read the base information */
   if(cg_base_read(fn, B, name, &r_cell_dim, &r_phys_dim) != CG_OK) {
     printf("*FAILED* cg_base_read\n");
     cgp_error_exit();
   }

   if(r_cell_dim != cell_dim || r_phys_dim != phys_dim) {
     printf("*FAILED* bad cell dim=%d or phy dim=%d\n", r_cell_dim, r_phys_dim);
     cgp_error_exit();
   }

   if (strcmp (name, "Base 1")) {
     printf("*FAILED* bad base name=%s\n", name);
     cgp_error_exit();
   }
   /* Read the particle zone information */
   if(cg_particle_read(fn, B, P, name, &size) != CG_OK) {
     printf("*FAILED* cg_particle_read\n");
     cgp_error_exit();
   }

   /* Check the read zone information is correct */
   if(size != num_particles) {
     printf("bad num points=%ld\n", (long)size);
     cgp_error_exit();
   }

   if (strcmp (name, "Particle 1")) {
     printf("bad zone name=%s\n", name);
     cgp_error_exit();
   }
   /* ========================================= */
   /* ==  (A) READ THE PARTICLE COORDINATES  == */
   /* ========================================= */

   count = num_particles/comm_size;

   if( !(Coor_x = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_x \n");
     cgp_error_exit();
   }

   if( !(Coor_y= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_y \n");
     cgp_error_exit();
   }

   if( !(Coor_z= (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of particle Coor_z \n");
     cgp_error_exit();
   }
   min = count*comm_rank+1;
   max = count*(comm_rank+1);

   Cvec[0] = Cx;
   Cvec[1] = Cy;
   Cvec[2] = Cz;

   buf = (void *)malloc(3*sizeof(void *));
   buf[0] = &Coor_x[0];
   buf[1] = &Coor_y[0];
   buf[2] = &Coor_z[0];

   if (cgp_particle_coord_multi_read_data(fn, B, P, Cvec, &min,&max, 3, buf)!= CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_coord_multi_read_data", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_coord_multi_read_data", NULL);
   }
   free(buf);

   /* Check if read the data back correctly */
   for ( k = 0; k < count; k++) {
     if( !compareValuesDouble(Coor_x[k], comm_rank*count + k + 1.1) ||
         !compareValuesDouble(Coor_y[k], Coor_x[k] + 0.1) ||
         !compareValuesDouble(Coor_z[k], Coor_y[k] + 0.1) ) {
       if(comm_rank == 0) write_test_status(FAILED, "Check cgp_particle_coord_multi_read_data values", NULL);
       cgp_error_exit();
     }
   }
   if(comm_rank == 0) write_test_status(PASSED, "Check cgp_particle_coord_multi_read_data values", NULL);

   free(Coor_x);
   free(Coor_y);
   free(Coor_z);

   /* ====================================== */
   /* == (B) READ THE PARTICLE FIELD DATA == */
   /* ====================================== */
   count = num_particles/comm_size;

   if( !(Data_Fx = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Reading Data_Fx \n");
     cgp_error_exit();
   }

   if( !(Data_Fy = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Reading Data_Fy \n");
     cgp_error_exit();
   }

   if( !(Data_Fz = (double*) malloc(count*sizeof(double))) ) {
     printf("*FAILED* allocation of Reading Data_Fz \n");
     cgp_error_exit();
   }

   Fvec[0] = Fx;
   Fvec[1] = Fy;
   Fvec[2] = Fz;

   buf = (void *)malloc(3*sizeof(void *));
   buf[0] = &Data_Fx[0];
   buf[1] = &Data_Fy[0];
   buf[2] = &Data_Fz[0];

   if(cgp_particle_field_multi_read_data(fn,B,P,S,Fvec,&min,&max,3,buf) != CG_OK) {
     if(comm_rank == 0) write_test_status(FAILED, "Test cgp_particle_field_multi_read_data", NULL);
     cgp_error_exit();
   } else {
     if(comm_rank == 0) write_test_status(PASSED, "Test cgp_particle_field_multi_read_data", NULL);
   }
   free(buf);

   /* Check if read the data back correctly */
   for ( k = 0; k < count; k++) {
     if(!compareValuesDouble(Data_Fx[k], comm_rank*count + k + 1.01) ||
        !compareValuesDouble(Data_Fy[k], comm_rank*count + k + 1.02) ||
        !compareValuesDouble(Data_Fz[k], comm_rank*count + k + 1.03) ) {
       if(comm_rank == 0) write_test_status(FAILED, "Check cgp_particle_field_multi_read_data values", NULL);
       cgp_error_exit();
     }
   }
   if(comm_rank == 0) write_test_status(PASSED, "Check cgp_particle_field_multi_read_data values", NULL);

   free(Data_Fx);
   free(Data_Fy);
   free(Data_Fz);

   if(cgp_close(fn) !=CG_OK) {
      printf("*FAILED* cgp_close\n");
      cgp_error_exit();
   }

   return 0;
}

int main (int argc, char *argv[])
{

  char test_str[TAB_SPACE];

  MPI_Init(&argc,&argv);
  MPI_Comm_size(comm, &comm_size);
  MPI_Comm_rank(comm, &comm_rank);

  if (comm_size > 8) {
    if (comm_rank == 0)
      fprintf(stderr, "number of processes must be 8 or less\n");
    cgp_error_exit();
  }
  if (argc > 1) {
    scale_factor = atoi(argv[1]);
    if (scale_factor < 1) {
      if (comm_rank == 0)
        fprintf(stderr, "invalid scale factor: %s\n", argv[1]);
      cgp_error_exit();
    }
  }

  strcpy(test_str,"General Parallel CGNS Testing");
  if ( comm_rank == 0) write_test_header(test_str, strlen(test_str));
  pcgns_ctest();

  strcpy(test_str,"Multi-sets API Testing");
  if ( comm_rank == 0) write_test_header(test_str, strlen(test_str));
  multisets();

  strcpy(test_str,"Particle Parallel I/O Testing");
  if ( comm_rank == 0) write_test_header(test_str, strlen(test_str));
  particle_test();

  strcpy(test_str,"Particle Multi-sets Testing");
  if ( comm_rank == 0) write_test_header(test_str, strlen(test_str));
  particle_multisets();

  MPI_Finalize();
  return 0;
}

