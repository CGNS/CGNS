/*
 * binaryio.h - include for C and FORTRAN binary I/O
 */

#ifndef _BINARYIO_H_
#define _BINARYIO_H_

#include <stdio.h>

/*----- machines -----*/

#define MACH_DEFAULT    0x0000
#define MACH_SUN        0x0001  /* 32-bit Sun */
#define MACH_IRIS       0x0002  /* 32-bit SGI */
#define MACH_HP         0x0003  /* 32-bit HP */
#define MACH_IBM        0x0004  /* 32-bit IBM */
#define MACH_DEC        0x0005  /* 32-bit DEC */
#define MACH_ALPHA      0x0006  /* 64-bit DEC Alpha */
#define MACH_CRAY       0x0007  /* 64-bit Cray */
#define MACH_CONVEX     0x0008  /* 32-bit Convex (native fp) */
#define MACH_DOS16      0x0009  /* 16-bit DOS (real mode) */
#define MACH_DOS32      0x000A  /* 32-bit DOS (protected mode) */
#define MACH_WIN32      0x000B  /* 32-bit Windows */
#define MACH_LINUX      0x000C  /* 32-bit Linux */
#define MACH_BSIEEE     0x000D  /* 32-bit generic byteswapped IEEE */
#define MACH_IEEE       0x000E  /* 32-bit generic IEEE */
#define MACH_UNKNOWN    0x00FF  /* unkown machine */

/*----- architectures -----*/

#define ARCH_DEFAULT    0x0000
#define ARCH_BSIEEE     0x0100  /* byteswapped IEEE */
#define ARCH_CRAY       0x0200  /* 64-bit Cray */
#define ARCH_CONVEX     0x0300  /* native Convex */
#define ARCH_ALPHA      0x0400  /* 64-bit DEC Alpha */
#define ARCH_DOS        0x0500  /* 16-bit DOS */
#define ARCH_IEEE       0x0F00  /* IEEE */

/*----- determine local machine/architecture -----*/

#if defined(sun) || defined(sparc)
# define MACH_LOCAL     MACH_SUN
# define ARCH_LOCAL     ARCH_IEEE
#endif

#ifdef __sgi
# define MACH_LOCAL     MACH_IRIS
# define ARCH_LOCAL     ARCH_IEEE
#endif

#if defined(hpux) || defined(__hpux)
# define MACH_LOCAL     MACH_HP
# define ARCH_LOCAL     ARCH_IEEE
#endif

#ifdef _AIX
# define MACH_LOCAL     MACH_IBM
# define ARCH_LOCAL     ARCH_IEEE
#endif

#ifdef __ultrix
# define MACH_LOCAL     MACH_DEC
# define ARCH_LOCAL     ARCH_BSIEEE
#endif

#ifdef __alpha
# define MACH_LOCAL     MACH_ALPHA
# define ARCH_LOCAL     ARCH_ALPHA
#endif

#ifdef CRAY
# define MACH_LOCAL     MACH_CRAY
# define ARCH_LOCAL     ARCH_CRAY
#endif

#if defined(__convex__) || defined(__convexc__)
# if defined(__convex__) || defined(_IEEE_FLOAT_)
#  define MACH_LOCAL    MACH_IEEE
#  define ARCH_LOCAL    ARCH_IEEE
# else
#  define MACH_LOCAL    MACH_CONVEX
#  define ARCH_LOCAL    ARCH_CONVEX
# endif
#endif

#if defined(MSDOS) || defined(__MSDOS__)
# if defined(__GO32__) || defined(__WIN32__)
#  define MACH_LOCAL    MACH_DOS32
#  define ARCH_LOCAL    ARCH_BSIEEE
# else
#  define MACH_LOCAL    MACH_DOS16
#  define ARCH_LOCAL    ARCH_DOS
# endif
#endif

#ifdef _WIN32
# define ARCH_LOCAL     ARCH_BSIEEE
# define MACH_LOCAL     MACH_WIN32
#endif

#if defined(__linux) || defined(__CYGWIN__)
# define ARCH_LOCAL     ARCH_BSIEEE
# define MACH_LOCAL     MACH_LINUX
#endif

/* assume machine is 32-bit IEEE */

#ifndef MACH_LOCAL
# define MACH_LOCAL     MACH_UNKNOWN
# define ARCH_LOCAL     ARCH_IEEE
#endif

/*----- read/write flags -----*/

#define OPEN_READ       0x0000  /* open for reading */
#define OPEN_WRITE      0x1000  /* open for writing */
#define OPEN_FORTRAN    0x2000  /* FORTRAN reads */
#define OPEN_ASCII      0x4000  /* ASCII read/write */

/*----- prototyping -----*/

#if defined(__STDC__) || defined(__cplusplus)
#ifndef PROTOTYPE
#define PROTOTYPE
#endif
#endif

/*----- file pointer structure -----*/

typedef struct binaryio {
    FILE *fp;       /* file pointer from fopen() */
    int flags;      /* file I/O flags */
    int arch;       /* I/O file architecture */
    int did_open;   /* set if bf_open() called */
    long rec_num;   /* current record number */
    long rec_size;  /* current record size */
    long rec_read;  /* bytes read in current record */

    /* data sizes */

    int short_size;     /* size of short in bytes */
    int int_size;       /* size of int in bytes */
    int long_size;      /* size of long in bytes */
    int float_size;     /* size of float in bytes */
    int double_size;    /* size of double in bytes */

    /* conversion routines */

#ifdef PROTOTYPE
    unsigned char *(*fromshort)(unsigned char *data);
    unsigned char *(*fromint)(unsigned char *data);
    unsigned char *(*fromlong)(unsigned char *data);
    unsigned char *(*fromfloat)(unsigned char *data);
    unsigned char *(*fromdouble)(unsigned char *data);
    unsigned char *(*toshort)(unsigned char *data);
    unsigned char *(*toint)(unsigned char *data);
    unsigned char *(*tolong)(unsigned char *data);
    unsigned char *(*tofloat)(unsigned char *data);
    unsigned char *(*todouble)(unsigned char *data);
#else
    unsigned char *(*fromshort)();
    unsigned char *(*fromint)();
    unsigned char *(*fromlong)();
    unsigned char *(*fromfloat)();
    unsigned char *(*fromdouble)();
    unsigned char *(*toshort)();
    unsigned char *(*toint)();
    unsigned char *(*tolong)();
    unsigned char *(*tofloat)();
    unsigned char *(*todouble)();
#endif
} BINARYIO;

extern void (*binaryio_error)(  /* error handler */
#ifdef PROTOTYPE
    char *errmsg
#endif
);

/*----- function prototypes -----*/

#ifdef __cplusplus
extern "C" {
#endif

/* file manipulation */

BINARYIO *bf_open (
#ifdef PROTOTYPE
    char *filename,
    int flags
#endif
);

BINARYIO *bf_new (
#ifdef PROTOTYPE
    FILE *fp,
    int flags
#endif
);

void bf_close (
#ifdef PROTOTYPE
    BINARYIO *bf
#endif
);

void bf_rewind (
#ifdef PROTOTYPE
    BINARYIO *bf
#endif
);

long bf_tell (
#ifdef PROTOTYPE
    BINARYIO *bf
#endif
);

int bf_seek (
#ifdef PROTOTYPE
    BINARYIO *bf,
    long offset
#endif
);

int bf_unget (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int c
#endif
);

int bf_nextrec (
#ifdef PROTOTYPE
    BINARYIO *bf
#endif
);

int bf_reclen  (
#ifdef PROTOTYPE
    BINARYIO *bf
#endif
);

int bf_skipspace (
#ifdef PROTOTYPE
    BINARYIO *bf
#endif
);

/* information */

char *bf_machname (
#ifdef PROTOTYPE
    int mach
#endif
);

char *bf_archname (
#ifdef PROTOTYPE
    int mach
#endif
);

/* reads */

int bf_getbytes (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    unsigned char *data
#endif
);

int bf_getstring (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    char *data
#endif
);

int bf_getshorts (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    short *data
#endif
);

int bf_getints (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    int *data
#endif
);

int bf_getlongs (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    long *data
#endif
);

int bf_getfloats (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    float *data
#endif
);

int bf_getdoubles (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    double *data
#endif
);

/* writes */

int bf_putbytes (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    unsigned char *data
#endif
);

int bf_putshorts (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    short *data
#endif
);

int bf_putints (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    int *data
#endif
);

int bf_putlongs (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    long *data
#endif
);

int bf_putfloats (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    float *data
#endif
);

int bf_putdoubles (
#ifdef PROTOTYPE
    BINARYIO *bf,
    int count,
    double *data
#endif
);

#ifdef __cplusplus
}
#endif

/*----- macros -----*/

#define bf_flags(BF)        ((BF)->flags)
#define bf_is_fortran(BF)   (OPEN_FORTRAN == ((BF)->flags & OPEN_FORTRAN))
#define bf_is_ASCII(BF)     (OPEN_ASCII == ((BF)->flags & OPEN_ASCII))
#define bf_machtype(BF)     ((BF)->flags & MACH_IEEE)
#define bf_archtype(BF)     ((BF)->arch)
#define bf_reccount(BF)     ((BF)->rec_num)
#define bf_is_eof(BF)       (feof((BF)->fp))
#define bf_is_eor(BF)       (OPEN_FORTRAN != ((BF)->flags & OPEN_FORTRAN) \
                            || (BF)->rec_read == (BF)->rec_size);

#endif  /* _BINARYIO_H_ */
