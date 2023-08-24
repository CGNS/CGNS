/*
! @file test_unstruc_quad.c
! @author Greg Sjaardema <gsjaardema@gmail.com>
! @version 0.1
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Test program for pcgns library
! -- Created to test cgp_parent_data_write function
! -- Based on test_unstructured.c
*/

/*

2....4....6....8 8...10...12...14 14...16...18...20 L+1.L+3...L+5...L+7
|    |    |    | |    |    |    | |     |    |    | |     |     |     |
1....3....5....7 7....9...11...13 13...15...17...19 L...L+2...L+4...L+6
   1    2   3      4    5    6       7    8    9      M     M+1   M+2
       P0               P1                P2             PN

L = P*6+1   M = P*3+1
The BC "Bottom" is on the bottom (y=0) edge of the mesh.
The BC "Left"   is on the left (x=0) edge of the mesh.
 -- included to test whether works with 0 entries on some procs.
*/


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "pcgnslib.h"
#include "mpi.h"

#define cgp_doError {printf("Error at %s:%d\n",__FILE__, __LINE__); return 1;}

int main(int argc, char* argv[]) {
  int err;
  int comm_size;
  int comm_rank;
  MPI_Info info;
  int fn;
  int B;
  int Z;
  int S;
  int BC;
  int Cx,Cy,Cz;
  int cell_dim = 3;
  int phys_dim = 3;
  cgsize_t nijk[3], min, max;
  int k, vert_proc, i;
  double *x, *y, *z;
  int nelem, nvert;
  cgsize_t start, end, emin, emax, *elements;
  cgsize_t *el_ptr = NULL;
  cgsize_t n_boco_elems;
  cgsize_t start_local, end_local;

  err = MPI_Init(&argc,&argv);
  if(err!=MPI_SUCCESS) cgp_doError;
  err = MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
  if(err!=MPI_SUCCESS) cgp_doError;
  err = MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
  if(err!=MPI_SUCCESS) cgp_doError;
  err = MPI_Info_create(&(info));
  if(err!=MPI_SUCCESS) cgp_doError;

  nelem = 3 * comm_size;
  nvert = 2 * nelem + 2;
  nijk[0] = nvert;
  nijk[1] = nelem;
  nijk[2] = 0;


  if (cgp_open("test_unstruc_quad.cgns", CG_MODE_WRITE, &fn) ||
      cg_base_write(fn, "Base 1", cell_dim, phys_dim, &B) ||
      cg_zone_write(fn, B, "Zone 1", nijk, CGNS_ENUMV(Unstructured), &Z))
    cgp_error_exit();

  vert_proc = 8;
  x = (double *)malloc(vert_proc*sizeof(double));
  y = (double *)malloc(vert_proc*sizeof(double));
  z = (double *)malloc(vert_proc*sizeof(double));

  min = 6*comm_rank+1;
  max = min+7;

  for(k=0;k<vert_proc;k+=2) {
    x[k+0] = (double) (min+k);
    x[k+1] = (double) (min+k);
    y[k+0] = 0.0;
    y[k+1] = 1.0;
    z[k+0] = 0.0;
    z[k+1] = 0.0;
  }

  if (cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateX",&Cx) ||
      cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateY",&Cy) ||
      cgp_coord_write(fn,B,Z,CGNS_ENUMV(RealDouble),"CoordinateZ",&Cz))
    cgp_error_exit();

  if (cgp_coord_write_data(fn,B,Z,Cx,&min,&max,x) ||
      cgp_coord_write_data(fn,B,Z,Cy,&min,&max,y) ||
      cgp_coord_write_data(fn,B,Z,Cz,&min,&max,z))
    cgp_error_exit();

  start = 1;
  end = comm_size*3;
  if (cgp_section_write(fn,B,Z,"Elements",CGNS_ENUMV(QUAD_4),start,end,0,&S))
    cgp_error_exit();

  nelem = 3;
  printf("%d:%d\n",comm_rank,nelem);
  emin = comm_rank*3+1;
  emax = emin+2;

  elements = (cgsize_t *)malloc(nelem*4*sizeof(cgsize_t));
  for(k=0;k<nelem;k++) {
    elements[4*k+0] = 2*(emin+k)-1;
    elements[4*k+1] = 2*(emin+k)+1;
    elements[4*k+2] = 2*(emin+k)+2;
    elements[4*k+3] = 2*(emin+k)+0;
  }
  printf("%d:%d %d %d\n",comm_rank,nelem,(int)emin,(int)emax);

  if (cgp_elements_write_data(fn,B,Z,S,emin,emax,elements))
    cgp_error_exit();

  start = 3*comm_size + 1;
  end   = start + 3*comm_size - 1;
  if (cgp_section_write(fn,B,Z,"Bottom",CGNS_ENUMV(BAR_2),start,end,0,&S))
    cgp_error_exit();

  /* Parent Element/Side data */
  for(k=0; k < 3; k++) {
    elements[3*0 + k] = comm_rank*3+1+k; /* Element */
    elements[3*1 + k] = 0;
    elements[3*2 + k] = 1; /* Side */
    elements[3*3 + k] = 0;
  }

  emin = comm_rank*3+start;
  emax = emin+2;
  printf("%d:%d %d\n",comm_rank,(int)emin,(int)emax);
  if (cgp_parent_data_write(fn,B,Z,S,emin,emax,elements))
    cgp_error_exit();

  /* Side Connectivity */
  for(k=0; k < 3; k++) {
    elements[2*k+0] = 2*(comm_rank*3+k)+1;
    elements[2*k+1] = 2*(comm_rank*3+k)+3;
  }
  if (cgp_elements_write_data(fn,B,Z,S,emin,emax,elements))
    cgp_error_exit();

  n_boco_elems = end - start + 1;
  if (cg_boco_write(fn, B, Z, "Bottom BC", CGNS_ENUMV(BCTypeUserDefined), CGNS_ENUMV(PointList), n_boco_elems, NULL, &BC))
    cgp_error_exit();

  for(i=0, k=emin; k <= emax; k++, i++) {
    elements[i] = k;
  }

  start_local = comm_rank * 3 + 1;
  end_local   = start_local + 2;
  printf("%d: %d %d\n", comm_rank, (int)start_local, (int)end_local);
  {
    const int depth    = 4;
    char *labels[]     = {"Zone_t", "ZoneBC_t", "BC_t", "PointList"};
    int indices[] = {Z, 1, BC, 0};
    if (cg_golist(fn, B, depth, labels, indices)) cgp_error_exit();
  }
  if (cgp_ptlist_write_data(fn, start_local, end_local, elements))
    cgp_error_exit();

  if (cg_boco_gridlocation_write(fn, B, Z, BC, CGNS_ENUMV(EdgeCenter)))
    cgp_error_exit();

  /* Left BC */
  start = end + 1;
  end   = start;
  if (cgp_section_write(fn,B,Z,"Left",CGNS_ENUMV(BAR_2),start,end,0,&S))
    cgp_error_exit();

  if (comm_rank == 0) {
    emin = start;
    emax = end;

    /* Parent Element/Side data */
    elements[0] = 1; /* Element */
    elements[1] = 0;
    elements[2] = 4; /* Side */
    elements[3] = 0;
    el_ptr = elements;
  }
  else {
    emin = 0;
    emax = 0;
    el_ptr = NULL;
  }
  printf("%d:%d %d\n",comm_rank,(int)emin,(int)emax);
  if (cgp_parent_data_write(fn,B,Z,S,emin,emax,el_ptr))
    cgp_error_exit();

  /* Side Connectivity */
  if (comm_rank == 0) {
    elements[0] = 1;
    elements[1] = 2;
  }
  if (cgp_elements_write_data(fn,B,Z,S,emin,emax,el_ptr))
    cgp_error_exit();

  n_boco_elems = 1;
  if (cg_boco_write(fn, B, Z, "Left BC", CGNS_ENUMV(BCTypeUserDefined), CGNS_ENUMV(PointList), n_boco_elems, NULL, &BC))
    cgp_error_exit();

  if (comm_rank == 0) {
    start_local = 1;
    end_local   = 1;

    elements[0] = start;
    el_ptr      = elements;
  } else {
    start_local = 0;
    end_local   = 0;

    el_ptr = NULL;
  }

  printf("%d: %d %d\n", comm_rank, (int)start_local, (int)end_local);
  {
    const int depth    = 4;
    char *labels[]     = {"Zone_t", "ZoneBC_t", "BC_t", "PointList"};
    int indices[] = {Z, 1, BC, 0};
    if (cg_golist(fn, B, depth, labels, indices)) cgp_error_exit();
  }
  if (cgp_ptlist_write_data(fn, start_local, end_local, el_ptr))
    cgp_error_exit();
  if (cg_boco_gridlocation_write(fn, B, Z, BC, CGNS_ENUMV(EdgeCenter)))
    cgp_error_exit();

  if (cgp_close(fn)) cgp_error_exit();

  // Test file reading
  if (cgp_open("test_unstruc_quad.cgns", CG_MODE_READ, &fn)) cgp_error_exit();

  for (int i = 0; i < nelem*4; i++) elements[i] = 0;
  start_local = comm_rank * 3 + 1;
  end_local = start_local + 2;
  {
    const int depth    = 4;
    char *labels[]     = {"Zone_t", "ZoneBC_t", "BC_t", "PointList"};
    int indices[] = {Z, 1, 1, 0};
    if (cg_golist(fn, B, depth, labels, indices)) cgp_error_exit();
  }
  if (cgp_ptlist_read_data(fn, start_local, end_local, elements)) cgp_error_exit();
  printf("%d: %d, %d, %d\n", comm_rank, (int)elements[0], (int)elements[1], (int)elements[2]);

  { // Test read values
    bool found_point;
    cgsize_t *point_list;
    int global_num_quads = nelem * comm_size;

    point_list = (cgsize_t *)malloc(global_num_quads*sizeof(cgsize_t));
    for (int i = 0; i < global_num_quads; i++) {
      point_list[i] = global_num_quads + i + 1;
    }

    for (int i = 0; i < 3; i++) {
      found_point = false;
      for (int j = 0; j < global_num_quads; j++) {
        if (elements[i] == point_list[j]) {
          found_point = true;
          break;
        }
      }
      if (!found_point) {
        printf("Error at %s:%d\n",__FILE__, __LINE__);
        printf("Could not find point %d in boundary list [%d,..., %d]\n", (int)elements[i], (int)point_list[0], (int)point_list[global_num_quads-1]);
        MPI_Abort(MPI_COMM_WORLD, 1);
      }
    }
    free(point_list);
  }

  if (cgp_close(fn)) cgp_error_exit();

  err = MPI_Finalize();
  if(err!=MPI_SUCCESS) cgp_doError;
  free(elements);
  return 0;
}
