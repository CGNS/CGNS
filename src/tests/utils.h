#include "cgnslib.h"

#ifndef CGNSTYPES_H
# define cgsize_t int
#endif
#ifndef CGNS_ENUMT
# define CGNS_ENUMT(e) e
# define CGNS_ENUMV(e) e
#endif

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
