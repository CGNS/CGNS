/*
 * vec.h
 */

#ifndef _VEC_H_
#define _VEC_H_

#define VEC_MAXCALLS    10  /* max recursive calls */

/*----- vector data structure -----*/

#define VEC_VALUE       0
#define VEC_VECTOR      1
#define VEC_COMPLEX     2

typedef double VECFLOAT;

typedef struct {
    int type;          /* type of data */
    int len;           /* length of vector */
    union {
        VECFLOAT val;  /* single data value */
        VECFLOAT *vec; /* vector data */
    } f;
} VECDATA;

/*--- function prototypes ---*/

#ifdef __STDC__
# ifndef PROTOTYPE
#  define PROTOTYPE
# endif
#endif

#ifdef __cplusplus
# ifndef PROTOTYPE
#  define PROTOTYPE
# endif
extern "C" {
#endif

#ifdef PROTOTYPE
typedef VECDATA *(*VECFUNC)(
    int checking,   /* set if checking string instead of parsing */
    int nv,         /* number of arguments */
    VECDATA **v,    /* arguments */
    char **errmsg   /* returned error message */
);
typedef VECDATA *(*VECCALL)(
    int checking,   /* set if checking string instead of parsing */
    char **pos,     /* pointer to position in equation string */
    char **errmsg   /* returned error message */
);
#else
typedef VECDATA *(*VECFUNC)();
typedef VECDATA *(*VECCALL)();
#endif

VECDATA *vec_create (   /* create a vector data structure */
#ifdef PROTOTYPE
    int type,           /* type of data */
    int len,            /* length of vector */
    int temp            /* set if temporary */
#endif
);

void vec_destroy (      /* destroy a vector data structure */
#ifdef PROTOTYPE
    VECDATA *vdata      /* structure to destroy */
#endif
);

int vec_add (           /* add vector to temporary list */
#ifdef PROTOTYPE
    VECDATA *vdata      /* data to add */
#endif
);

void vec_remove (       /* remove vector from temporary list */
#ifdef PROTOTYPE
    VECDATA *vdata      /* data to remove */
#endif
);

void vec_free (         /* free internal memory */
#ifdef PROTOTYPE
    void
#endif
);

void vec_maxcalls (     /* sets maximum number recursive calls */
#ifdef PROTOTYPE
    int maxcalls        /* max recursive calls */
#endif
);

char *vec_errmsg (      /* returns error messages */
#ifdef PROTOTYPE
    void
#endif
);

char **vec_list (       /* return list of intrinsics */
#ifdef PROTOTYPE
    void
#endif
);

VECDATA *vec_parse (    /* parse and process equation */
#ifdef PROTOTYPE
    char *str,          /* equation string */
    int min_len,        /* minimum length of vector for counter */
    VECCALL func        /* user call-back for unknown symbols */
#endif
);

int vec_check (         /* parse equation and check for errors */
#ifdef PROTOTYPE
    char *str,          /* equation string */
    int min_len,        /* minimum length of vector for counter */
    VECCALL func        /* user call-back for unknown symbols */
#endif
);

int vec_number (        /* get floating point number from string */
#ifdef PROTOTYPE
    double *dval,       /* returned number */
    char **sp           /* updated string pointer */
#endif
);

void vec_randinit (     /* initialize random number generator */
#ifdef PROTOTYPE
    int seed            /* initialization seed */
#endif
);

double vec_rand (       /* return random floating point number */
#ifdef PROTOTYPE        /* between 0 and 1 */
    void
#endif
);

#ifdef __cplusplus
}
#endif

#endif      /* _VEC_H_ */
