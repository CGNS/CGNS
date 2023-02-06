#include "cgnslib.h"
#include <math.h>
#include <string.h>


#ifndef CGNSTYPES_H
# define cgsize_t int
#endif
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

double elapsed_time (void);
double file_size (char *fname);

int compareValuesDouble(double val1, double val2) {

  int ret = 1;
  if (fabs(val1 - val2) > 1e-10) {
    ret = 0;
    printf("ERROR - value comparison failed %f, %f\n", val1, val2);
  }
  return ret;
}

int compareValuesFloat(float val1, float val2) {

  int ret = 1;
  if (fabs((double)val1 - (double)val2) > 1e-7) {
    ret = 0;
    printf("ERROR - value comparison failed %f, %f\n", val1, val2);
  }
  return ret;
}

int compareValuesInt(int val1, int val2) {
  int ret = 1;
  if (val1 != val2) {
    ret = 0;
    printf("ERROR - value comparison failed %d, %d\n", val1, val2);
  }
  return ret;
}

int compareValuescgSize_t(cgsize_t val1, cgsize_t val2) {
  int ret = 1;
  if (val1 != val2) {
    ret = 0;
    if (sizeof(cgsize_t) == 4)
      printf("ERROR - value comparison failed %d, %d\n", val1, val2);
    else
      printf("ERROR - value comparison failed %zu, %zu\n", val1, val2);
  }
  return ret;
}

int compareValuesChr(const char *val1, const char *val2) {
  int ret = 1;
  if (strcmp(val1, val2)) {
    ret = 0;
  }
  return ret;
}
