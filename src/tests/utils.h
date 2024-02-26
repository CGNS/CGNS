#ifndef UTILS_H
#define UTILS_H

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

  char *title_centered = (char*)malloc(4*width+1*sizeof(char));
  char *str = (char*)malloc(2*width+2*sizeof(char));

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
