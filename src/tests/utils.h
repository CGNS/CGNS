#ifndef UTILS_H
#define UTILS_H

#include "cgnslib.h"
#include <math.h>
#include <string.h>


#ifndef CGNS_ENUMT
# define CGNS_ENUMT(e) e
# define CGNS_ENUMV(e) e
#endif

/* Return code indicating a skipped test */
#define SKIP_RETURN_CODE 125

#if defined(_WIN32) && !defined(__NUTC__)
# include <io.h>     /* suggested by MTI */
# ifndef F_OK
#  define R_OK    004 /* Test for Read permission */
#  define W_OK    002 /* Test for Write permission */
#  define X_OK    001 /* Test for eXecute permission */
#  define F_OK    000 /* Test for existence of File */
# endif
# define ACCESS _access
# define UNLINK _unlink
#else
# include <unistd.h>
# define ACCESS access
# define UNLINK unlink
#endif

#define false 0
#define true 1
#define TAB_SPACE 90

#define SKIP  -1
#define PASSED 0
#define FAILED 1

double elapsed_time (void);
double file_size (char *fname);

int compareValuesDouble(double val1, double val2) {

  int ret = 1;
  if (fabs(val1 - val2) > 1e-10) {
    ret = 0;
    printf("ERROR - value comparison failed (double)\n");
  }
  return ret;
}

int compareValuesFloat(float val1, float val2) {

  int ret = 1;
  if (fabs((double)val1 - (double)val2) > 1e-6) {
    ret = 0;
    printf("ERROR - value comparison failed (float)\n");
  }
  return ret;
}

int compareValuesInt(int val1, int val2) {
  int ret = 1;
  if (val1 != val2) {
    ret = 0;
    printf("ERROR - value comparison failed (int)\n");
  }
  return ret;
}

int compareValuescgSize_t(cgsize_t val1, cgsize_t val2) {
  int ret = 1;
  if (val1 != val2) {
    ret = 0;
    printf("ERROR - value comparison failed (cgsize_t)\n");
  }
  return ret;
}

int compareValuesChr(const char *val1, const char *val2) {
  int ret = 1;
  if (strcmp(val1, val2)) {
    printf("ERROR - value comparison failed (char)\n");
    ret = 0;
  }
  return ret;
}

int write_test_header(char *title_header, int len)
{

  /* Writes the test header */

  int width;
  int i;

  width = TAB_SPACE+10;

  char *title_centered = (char*)malloc((4*width+1)*sizeof(char));
  char *str = (char*)malloc((2*width+2)*sizeof(char));

  memcpy(str,title_header,len);
  str[len] = '\0';
  unsigned short lpad = (width-len)/2-3;
  unsigned short rpad = width-5 - (lpad + len);
  sprintf(title_centered,"%s%*s%s%*s%s", "| |",lpad, " ", str, rpad, " ","| |");

  printf(" ");
  for( i = 0; i < width-1; i++)
    printf("_");
  printf("\n");

  printf("|  ");
  for( i = 0; i < width-5; i++)
    printf("_");
  printf("  |\n");

  printf("| |");
  for( i = 0; i < width-5; i++)
    printf(" ");
  printf("| |\n");

  printf("%s\n",title_centered);

  printf("| |");
  for( i = 0; i < width-5; i++)
    printf(" ");
  printf("| |\n");

  printf("| |");
  for( i = 0; i < width-5; i++)
    printf("_");
  printf("| |\n");

  printf("|");
  for( i = 0; i < width-1; i++)
    printf("_");
  printf("|\n\n");

  free(title_centered);
  free(str);

  return 0;
}

/* CPEX-0045 S3.2.2: the first points of an ElementInterpolation_t's
 * LagrangeControlPoints must be the principal vertices of the corresponding
 * linear element, in Figure 1 order.  A generator that emits a plain
 * lexicographic lattice does not satisfy that, so it must permute the set
 * before writing.  The corner tables below are the same ones cgnscheck's
 * check_element_nodes_ordering() validates against.
 *
 * Returns 0 on success, or 1 if the point set does not contain every corner
 * (which means the generator, not the ordering, is wrong -- callers should
 * fail loudly rather than write a non-conforming file).
 * v and w may be NULL for 1-D and 2-D element types. */
int ho_reorder_corners_first(CGNS_ENUMT(ElementType_t) basic, int npts,
                             double *u, double *v, double *w)
{
    /* Figure 1 principal-vertex coordinates, indexed as in the linear tag. */
    static const double bar_u[]  = {-1., 1.};
    static const double tri_u[]  = {-1., 1.,-1.}, tri_v[]  = {-1.,-1., 1.};
    static const double quad_u[] = {-1., 1., 1.,-1.}, quad_v[] = {-1.,-1., 1., 1.};
    static const double tet_u[]  = {-1., 1.,-1.,-1.};
    static const double tet_v[]  = {-1.,-1., 1.,-1.};
    static const double tet_w[]  = {-1.,-1.,-1., 1.};
    static const double hex_u[]  = {-1., 1., 1.,-1.,-1., 1., 1.,-1.};
    static const double hex_v[]  = {-1.,-1., 1., 1.,-1.,-1., 1., 1.};
    static const double hex_w[]  = {-1.,-1.,-1.,-1., 1., 1., 1., 1.};
    static const double pen_u[]  = {-1., 1.,-1.,-1., 1.,-1.};
    static const double pen_v[]  = {-1.,-1., 1.,-1.,-1., 1.};
    static const double pen_w[]  = {-1.,-1.,-1., 1., 1., 1.};
    static const double pyr_u[]  = {-1., 1., 1.,-1., 0.};
    static const double pyr_v[]  = {-1.,-1., 1., 1., 0.};
    static const double pyr_w[]  = {-1.,-1.,-1.,-1., 1.};

    const double *cu = NULL, *cv = NULL, *cw = NULL;
    int ncorner = 0, k, j;
    const double tol = 1.e-10;

    switch (basic) {
        case CGNS_ENUMV(BAR_2):
            cu = bar_u;  ncorner = 2; break;
        case CGNS_ENUMV(TRI_3):
            cu = tri_u;  cv = tri_v;  ncorner = 3; break;
        case CGNS_ENUMV(QUAD_4):
            cu = quad_u; cv = quad_v; ncorner = 4; break;
        case CGNS_ENUMV(TETRA_4):
            cu = tet_u;  cv = tet_v;  cw = tet_w;  ncorner = 4; break;
        case CGNS_ENUMV(HEXA_8):
            cu = hex_u;  cv = hex_v;  cw = hex_w;  ncorner = 8; break;
        case CGNS_ENUMV(PENTA_6):
            cu = pen_u;  cv = pen_v;  cw = pen_w;  ncorner = 6; break;
        case CGNS_ENUMV(PYRA_5):
            cu = pyr_u;  cv = pyr_v;  cw = pyr_w;  ncorner = 5; break;
        default:
            return 1;
    }
    if (npts < ncorner) return 1;

    for (k = 0; k < ncorner; k++) {
        int found = -1;
        for (j = k; j < npts; j++) {
            if (fabs(u[j] - cu[k]) > tol) continue;
            if (cv && v && fabs(v[j] - cv[k]) > tol) continue;
            if (cw && w && fabs(w[j] - cw[k]) > tol) continue;
            found = j;
            break;
        }
        if (found < 0) return 1;
        if (found != k) {
            double t;
            t = u[k]; u[k] = u[found]; u[found] = t;
            if (v) { t = v[k]; v[k] = v[found]; v[found] = t; }
            if (w) { t = w[k]; w[k] = w[found]; w[found] = t; }
        }
    }
    return 0;
}

int write_test_status( int test_result, char *test_title, char *cause)
{

  /* Writes the results of the tests

  test_result: negative,  --skip --
               0       ,   passed
               positive,   failed
  */

  char error_string[9];
  char passed[] = " PASSED ";
  char failed[] = "*FAILED*";
  char skip[]   = "--SKIP--";

  strcpy(error_string,failed);
  if(test_result == PASSED) {
    strcpy(error_string, passed);
  } else if (test_result == SKIP) {
    strcpy(error_string,skip);
  }
  printf("%s %*s\n",test_title,(int)(TAB_SPACE-strlen(test_title)),error_string);

  if(cause)
    printf("  FAILURE REPORTED -- %s\n", cause);

  return 0;
}

#endif
