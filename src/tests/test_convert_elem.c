#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <io.h>
#define unlink _unlink
#else
#include <unistd.h>
#endif
#include "cgnslib.h"

#ifndef CGNS_ENUMT
#define CGNS_ENUMT(e) e
#define CGNS_ENUMV(e) e
#endif

#define NUM_SIDE 5
#define NUM_RANDOM 10

float *xcoord;
float *ycoord;
float *zcoord;
float *fbuf;

int64_t *elements;
int64_t *faces;
int64_t *parent;
int64_t *ptmp;
int64_t *ibuf, *pbuf;
int64_t *offsets;
cgsize_t *parent_buffer;

#define NODE_INDEX(I, J, K) ((I) + NUM_SIDE * (((J)-1) + NUM_SIDE * ((K)-1)))
#define CELL_INDEX(I, J, K)                                                    \
  ((I) + (NUM_SIDE - 1) * (((J)-1) + (NUM_SIDE - 1) * ((K)-1)))

void get_parent(int rmin, int rmax, int nelems, int nfaces) {
  int i, j, k, n, nn, np;

  np = rmax - rmin + 1;
  nn = rmin - nelems - 1;
  for (n = 0, j = 0; j < 4; j++) {
    k = j * nfaces + nn;
    for (i = 0; i < np; i++) {
      parent_buffer[n++] = (cgsize_t)parent[k++];
    }
  }
}

int mixed_offset(int num, int nelems) {
  int offset;
  int nmixed = nelems << 1;

  if (--num < nmixed) {
    int i = num >> 1;
    offset = 14 * i;
    if (num != (i << 1))
      offset += 5;
  } else
    offset = 14 * nelems + 5 * (num - nmixed);
  return offset;
}

int main(int argc, char **argv) {
  int n, i, j, k, l, nn, nf, np;
  int nnodes, nelems, nfaces;
  int cgfile, cgbase, cgzone, cgcoord, cgsol, cgfld;
  int cgsect, cgelems, cgfaces;
  cgsize_t size[3], rmin, rmax, nr;
  cgsize_t is, ie;
  char name[33];
  CGNS_ENUMT(ElementType_t) type;
  static char *fname = "convert_datatype_elem.cgns";

  nnodes = NUM_SIDE * NUM_SIDE * NUM_SIDE;
  xcoord = (float *)malloc(4 * nnodes * sizeof(float));
  ycoord = xcoord + nnodes;
  zcoord = ycoord + nnodes;
  fbuf = zcoord + nnodes;

  for (n = 0, k = 1; k <= NUM_SIDE; k++) {
    for (j = 1; j <= NUM_SIDE; j++) {
      for (i = 1; i <= NUM_SIDE; i++) {
        xcoord[n] = (float)i;
        ycoord[n] = (float)j;
        zcoord[n] = (float)k;
        n++;
      }
    }
  }

  nelems = (NUM_SIDE - 1) * (NUM_SIDE - 1) * (NUM_SIDE - 1);
  elements = (int64_t *)malloc(16 * nelems * sizeof(int64_t));
  ibuf = elements + 8 * nelems;

  for (n = 0, k = 1; k < NUM_SIDE; k++) {
    for (j = 1; j < NUM_SIDE; j++) {
      for (i = 1; i < NUM_SIDE; i++) {
        nn = NODE_INDEX(i, j, k);
        elements[n++] = nn;
        elements[n++] = nn + 1;
        elements[n++] = nn + 1 + NUM_SIDE;
        elements[n++] = nn + NUM_SIDE;
        nn += NUM_SIDE * NUM_SIDE;
        elements[n++] = nn;
        elements[n++] = nn + 1;
        elements[n++] = nn + 1 + NUM_SIDE;
        elements[n++] = nn + NUM_SIDE;
      }
    }
  }

  nfaces = 6 * (NUM_SIDE - 1) * (NUM_SIDE - 1);
  faces = (int64_t *)malloc(18 * nfaces * sizeof(int64_t));
  parent_buffer = (cgsize_t *)malloc(4 * nfaces * sizeof(cgsize_t));
  parent = faces + 4 * nfaces;
  ptmp = parent + 4 * nfaces;
  pbuf = ptmp + 4 * nfaces;

  for (n = 0; n < 4 * nfaces; n++)
    parent[n] = 0;
  np = nf = 0;
  n = 2 * nfaces;

  i = 1;
  for (k = 1; k < NUM_SIDE; k++) {
    for (j = 1; j < NUM_SIDE; j++) {
      nn = NODE_INDEX(i, j, k);
      faces[nf++] = nn;
      faces[nf++] = nn + NUM_SIDE * NUM_SIDE;
      faces[nf++] = nn + NUM_SIDE * (NUM_SIDE + 1);
      faces[nf++] = nn + NUM_SIDE;
      parent[np] = CELL_INDEX(i, j, k);
      parent[np + n] = 5;
      np++;
    }
  }
  i = NUM_SIDE;
  for (k = 1; k < NUM_SIDE; k++) {
    for (j = 1; j < NUM_SIDE; j++) {
      nn = NODE_INDEX(i, j, k);
      faces[nf++] = nn;
      faces[nf++] = nn + NUM_SIDE;
      faces[nf++] = nn + NUM_SIDE * (NUM_SIDE + 1);
      faces[nf++] = nn + NUM_SIDE * NUM_SIDE;
      parent[np] = CELL_INDEX(i - 1, j, k);
      parent[np + n] = 3;
      np++;
    }
  }
  j = 1;
  for (k = 1; k < NUM_SIDE; k++) {
    for (i = 1; i < NUM_SIDE; i++) {
      nn = NODE_INDEX(i, j, k);
      faces[nf++] = nn;
      faces[nf++] = nn + 1;
      faces[nf++] = nn + 1 + NUM_SIDE * NUM_SIDE;
      faces[nf++] = nn + NUM_SIDE * NUM_SIDE;
      parent[np] = CELL_INDEX(i, j, k);
      parent[np + n] = 2;
      np++;
    }
  }
  j = NUM_SIDE;
  for (k = 1; k < NUM_SIDE; k++) {
    for (i = 1; i < NUM_SIDE; i++) {
      nn = NODE_INDEX(i, j, k);
      faces[nf++] = nn;
      faces[nf++] = nn + NUM_SIDE * NUM_SIDE;
      faces[nf++] = nn + 1 + NUM_SIDE * NUM_SIDE;
      faces[nf++] = nn + 1;
      parent[np] = CELL_INDEX(i, j - 1, k);
      parent[np + n] = 4;
      np++;
    }
  }
  k = 1;
  for (j = 1; j < NUM_SIDE; j++) {
    for (i = 1; i < NUM_SIDE; i++) {
      nn = NODE_INDEX(i, j, k);
      faces[nf++] = nn;
      faces[nf++] = nn + NUM_SIDE;
      faces[nf++] = nn + NUM_SIDE + 1;
      faces[nf++] = nn + 1;
      parent[np] = CELL_INDEX(i, j, k);
      parent[np + n] = 1;
      np++;
    }
  }
  k = NUM_SIDE;
  for (j = 1; j < NUM_SIDE; j++) {
    for (i = 1; i < NUM_SIDE; i++) {
      nn = NODE_INDEX(i, j, k);
      faces[nf++] = nn;
      faces[nf++] = nn + 1;
      faces[nf++] = nn + NUM_SIDE + 1;
      faces[nf++] = nn + NUM_SIDE;
      parent[np] = CELL_INDEX(i, j, k - 1);
      parent[np + n] = 6;
      np++;
    }
  }

  unlink(fname);
  printf("creating CGNS file %s\n", fname);
  if (cg_open(fname, CG_MODE_WRITE, &cgfile) ||
      cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
      cg_gopath(cgfile, "/Base") ||
      cg_dataclass_write(CGNS_ENUMV(NormalizedByDimensional)))
    cg_error_exit();

  /* create zone and sections */

  size[0] = nnodes;
  size[1] = nelems;
  size[2] = 0;

  puts("\nwriting a 32bit section");

  if (cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured),
                    &cgzone))
    cg_error_exit();

  /* Create section using 32-bit elements */

  if (cg_section_general_write(cgfile, cgbase, cgzone, "Elements",
                               CGNS_ENUMV(HEXA_8), CGNS_ENUMV(Integer),
                               (cgsize_t)1, (cgsize_t)nelems,
                               (cgsize_t)(nelems * 8), 0, &cgelems) ||
      cg_section_general_write(
          cgfile, cgbase, cgzone, "Faces", CGNS_ENUMV(QUAD_4),
          CGNS_ENUMV(Integer), (cgsize_t)(nelems + 1),
          (cgsize_t)(nelems + nfaces), (cgsize_t)(nfaces * 4), 0, &cgfaces))
    cg_error_exit();

  /* write zone with partial write */

  puts("\nwriting zone");

  /* write every other coordinate plane */

  np = NUM_SIDE * NUM_SIDE;
  for (k = 0; k < NUM_SIDE; k += 2) {
    n = k * np;
    rmin = n + 1;
    rmax = n + np;
    printf("coordinates %d -> %d\n", (int)rmin, (int)rmax);
    if (cg_coord_partial_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                               "CoordinateX", &rmin, &rmax, &xcoord[n],
                               &cgcoord) ||
        cg_coord_partial_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                               "CoordinateY", &rmin, &rmax, &ycoord[n],
                               &cgcoord) ||
        cg_coord_partial_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                               "CoordinateZ", &rmin, &rmax, &zcoord[n],
                               &cgcoord))
      cg_error_exit();
  }

  /* write every other cell plane */

  np = (NUM_SIDE - 1) * (NUM_SIDE - 1);
  for (k = 1; k < NUM_SIDE; k += 2) {
    nn = (k - 1) * np;
    n = nn << 3;
    rmin = nn + 1;
    rmax = nn + np;
    printf("elements %d -> %d\n", (int)rmin, (int)rmax);
    /* LongInteger == 64bit */
    if (cg_elements_general_write(cgfile, cgbase, cgzone, cgelems, rmin, rmax,
                                  CGNS_ENUMV(LongInteger), &elements[n]))
      cg_error_exit();
  }

  /* write every other face */

  for (k = 0; k < 6; k += 2) {
    nn = k * np;
    n = nn << 2;
    rmin = nn + 1 + nelems;
    rmax = nn + np + nelems;
    get_parent((int)rmin, (int)rmax, nelems, nfaces);
    printf("faces %d -> %d\n", (int)rmin, (int)rmax);
    if (cg_elements_general_write(cgfile, cgbase, cgzone, cgfaces, rmin, rmax,
                                  CGNS_ENUMV(LongInteger), &faces[n]) ||
        cg_parent_data_partial_write(cgfile, cgbase, cgzone, cgfaces, rmin,
                                     rmax, (cgsize_t *)parent_buffer))
      cg_error_exit();
  }

  puts("closing");
  cg_close(cgfile);

  if (cg_open(fname, CG_MODE_MODIFY, &cgfile))
    cg_error_exit();

  /* fill in missing coordinate planes */

  np = NUM_SIDE * NUM_SIDE;
  for (k = 1; k < NUM_SIDE; k += 2) {
    n = k * np;
    rmin = n + 1;
    rmax = n + np;
    printf("coordinates %d -> %d\n", (int)rmin, (int)rmax);
    if (cg_coord_partial_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                               "CoordinateX", &rmin, &rmax, &xcoord[n],
                               &cgcoord) ||
        cg_coord_partial_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                               "CoordinateY", &rmin, &rmax, &ycoord[n],
                               &cgcoord) ||
        cg_coord_partial_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
                               "CoordinateZ", &rmin, &rmax, &zcoord[n],
                               &cgcoord))
      cg_error_exit();
  }

  /* fill in missing cell planes */

  np = (NUM_SIDE - 1) * (NUM_SIDE - 1);
  for (k = 2; k < NUM_SIDE; k += 2) {
    nn = (k - 1) * np;
    n = nn << 3;
    rmin = nn + 1;
    rmax = nn + np;
    printf("elements %d -> %d\n", (int)rmin, (int)rmax);
    if (cg_elements_general_write(cgfile, cgbase, cgzone, cgelems, rmin, rmax,
                                  CGNS_ENUMV(LongInteger), &elements[n]))
      cg_error_exit();
  }

  /* fill in missing faces */

  for (k = 1; k < 6; k += 2) {
    nn = k * np;
    n = nn << 2;
    rmin = nn + 1 + nelems;
    rmax = nn + np + nelems;
    get_parent((int)rmin, (int)rmax, nelems, nfaces);
    printf("faces %d -> %d\n", (int)rmin, (int)rmax);
    if (cg_elements_general_write(cgfile, cgbase, cgzone, cgfaces, rmin, rmax,
                                  CGNS_ENUMV(LongInteger), &faces[n]) ||
        cg_parent_data_partial_write(cgfile, cgbase, cgzone, cgfaces, rmin,
                                     rmax, (cgsize_t *)parent_buffer))
      cg_error_exit();
  }

  /* check the data */

  puts("checking the data");

  nn = 0;
  /* check coordinates */

  rmin = 1;
  rmax = nelems;

  /* check elements */

  if (cg_section_read(cgfile, cgbase, cgzone, 1, name, &type, &is, &ie, &k,
                      &n) ||
      cg_elements_general_read(cgfile, cgbase, cgzone, 1, rmin, rmax,
                               CGNS_ENUMV(LongInteger), ibuf))
    cg_error_exit();
  if (strcmp(name, "Elements") || type != CGNS_ENUMV(HEXA_8) || is != 1 ||
      ie != nelems || k != 0 || n != 0) {
    nn++;
    puts("differences in Elements");
  }
  for (np = 0, n = 0; n < 8 * nelems; n++) {
    if (elements[n] != ibuf[n])
      np++;
  }
  nn += np;
  if (np)
    printf("%d differences in Elements connectivity\n", np);

  /* check faces */

  if (cg_section_read(cgfile, cgbase, cgzone, 2, name, &type, &is, &ie, &k,
                      &n) ||
      cg_elements_general_read(cgfile, cgbase, cgzone, 2, is, ie,
                               CGNS_ENUMV(LongInteger), ibuf))
    cg_error_exit();
  if (cg_parent_elements_general_read(cgfile, cgbase, cgzone, 2, is, ie,
                                      CGNS_ENUMV(LongInteger), pbuf) ||
      cg_parent_elements_position_general_read(cgfile, cgbase, cgzone, 2, is,
                                               ie, CGNS_ENUMV(LongInteger),
                                               &pbuf[(ie - is + 1) << 1])) {
    cg_error_exit();
  }

  if (strcmp(name, "Faces") || type != CGNS_ENUMV(QUAD_4) ||
      is != (nelems + 1) || ie != (nelems + nfaces) || k != 0 || n != 1) {
    nn++;
    puts("differences in Faces");
  }
  for (np = 0, n = 0; n < 4 * nfaces; n++) {
    if (faces[n] != ibuf[n])
      np++;
  }
  nn += np;
  if (np)
    printf("%d differences in Faces connectivity\n", np);

  for (np = 0, n = 0; n < 4 * nfaces; n++) {
    if (parent[n] != pbuf[n]) {
      np++;
    }
  }
  nn += np;
  if (np)
    printf("%d differences in Faces parent data\n", np);

  if (nn == 0)
    puts("no diferences");

  free(xcoord);

  puts("deleting Elements and Faces and creating Mixed 32bit section");
  /* delete sections and create as mixed */

  if (cg_goto(cgfile, cgbase, "Zone_t", 1, NULL) ||
      cg_delete_node("Elements") || cg_delete_node("Faces"))
    cg_error_exit();

  /* create the 32bit section */

  if (cg_section_general_write(cgfile, cgbase, cgzone, "Mixed",
                               CGNS_ENUMV(MIXED), CGNS_ENUMV(Integer),
                               (cgsize_t)1, (cgsize_t)(nelems + nfaces),
                               (nelems * 9 + nfaces * 5), 0, &cgsect))
    cg_error_exit();

  /* before any partial writing it is important to initialize the section or at
   * least the StartElementOffset */

  if (cg_section_initialize(cgfile, cgbase, cgzone, cgsect))
    cg_error_exit();

  /* create mixed element connectivity */

  nn = (nelems << 3) + nelems + (nfaces << 2) + nfaces;
  ptmp = (int64_t *)malloc(2 * nn * sizeof(int64_t));

  offsets = (int64_t *)malloc((nelems + nfaces + 1) * sizeof(int64_t));
  offsets[0] = 0;

  i = j = n = l = 0;
  for (nf = 0; nf < nelems; nf++) {
    ptmp[n++] = CGNS_ENUMV(QUAD_4);
    for (k = 0; k < 4; k++)
      ptmp[n++] = faces[j++];
    offsets[l + 1] = offsets[l] + 5;
    l++;
    ptmp[n++] = CGNS_ENUMV(HEXA_8);
    for (k = 0; k < 8; k++)
      ptmp[n++] = elements[i++];
    offsets[l + 1] = offsets[l] + 9;
    l++;
  }
  while (nf++ < nfaces) {
    ptmp[n++] = CGNS_ENUMV(QUAD_4);
    for (k = 0; k < 4; k++)
      ptmp[n++] = faces[j++];
    offsets[l + 1] = offsets[l] + 5;
    l++;
  }

  free(elements);
  free(parent_buffer);
  elements = ptmp;
  ibuf = elements + nn;

  /* create parent data */

  np = nelems + nfaces;
  nn = np << 2;
  ptmp = (int64_t *)malloc(3 * nn * sizeof(int64_t));
  parent_buffer = (cgsize_t *)malloc(3 * nn * sizeof(cgsize_t));

  for (n = 0; n < nfaces; n++)
    parent[n] <<= 1;

  for (n = 0; n < nn; n++) {
    ptmp[n] = 0;
    parent_buffer[n] = 0;
  }
  for (n = 0, j = 0; j < 4; j++) {
    k = j * np;
    for (i = 0; i < nelems; i++) {
      ptmp[k] = parent[n++];
      k += 2;
    }
    while (i++ < nfaces) {
      ptmp[k++] = parent[n++];
    }
  }

  free(faces);
  parent = ptmp;
  ptmp = parent + nn;
  pbuf = ptmp + nn;

  rmin = 2 * nelems + 1;
  rmax = np;
  n = mixed_offset((int)rmin, nelems);
  get_parent((int)rmin, (int)rmax, 0, np);

  printf("mixed %d -> %d\n", (int)rmin, (int)rmax);
  if (cg_poly_elements_general_write(cgfile, cgbase, cgzone, cgsect, rmin, rmax,
                                     CGNS_ENUMV(LongInteger), &elements[n],
                                     &offsets[rmin - 1]) ||
      cg_parent_data_partial_write(cgfile, cgbase, cgzone, cgsect, rmin, rmax,
                                   (cgsize_t *)parent_buffer))
    cg_error_exit();

  printf("mixed %d -> %d (2 at a time)\n", 1, nelems << 1);
  for (i = 0; i < nelems; i++) {
    rmin = (i << 1) + 1;
    rmax = rmin + 1;
    n = mixed_offset((int)rmin, nelems);
    get_parent((int)rmin, (int)rmax, 0, np);
    if (cg_poly_elements_general_write(cgfile, cgbase, cgzone, cgsect, rmin,
                                       rmax, CGNS_ENUMV(LongInteger),
                                       &elements[n], &offsets[rmin - 1]))
      cg_error_exit();
    if (cg_parent_data_partial_write(cgfile, cgbase, cgzone, cgsect, rmin, rmax,
                                     (cgsize_t *)parent_buffer))
      cg_error_exit();
  }

  puts("checking the data");

  if (cg_section_read(cgfile, cgbase, cgzone, cgsect, name, &type, &is, &ie, &k,
                      &n) ||
      cg_poly_elements_general_read(cgfile, cgbase, cgzone, cgsect, is, ie,
                                    CGNS_ENUMV(LongInteger), ibuf, offsets))
    cg_error_exit();
  if (cg_parent_elements_general_read(cgfile, cgbase, cgzone, cgsect, is, ie,
                                      CGNS_ENUMV(LongInteger), pbuf) ||
      cg_parent_elements_position_general_read(cgfile, cgbase, cgzone, cgsect,
                                               is, ie, CGNS_ENUMV(LongInteger),
                                               &pbuf[(ie - is + 1) << 1])) {
    cg_error_exit();
  }

  if (strcmp(name, "Mixed") || type != CGNS_ENUMV(MIXED) || is != 1 ||
      ie != np || k != 0 || n != 1) {
    puts("differences in Mixed");
  }

  nn = mixed_offset(np, nelems);
  for (i = 0, n = 0; n < nn; n++) {
    if (elements[n] != ibuf[n])
      i++;
  }
  if (i)
    printf("%d differences in Mixed connectivity\n", i);

  nn = (nelems + nfaces) << 2;
  for (i = 0, n = 0; n < nn; n++) {
    if (parent[n] != pbuf[n])
      i++;
  }
  if (i)
    printf("%d differences in Mixed parent data\n", i);

  puts("closing file");
  cg_close(cgfile);
  free(elements);
  free(offsets);
  free(parent);

  return 0;
}
