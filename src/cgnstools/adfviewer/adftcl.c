#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "tk.h"
#include "ADF.h"
#include "cgnslib.h" /* only needed for CGNS_VERSION */

/* these are the data types as used in ADF */

#define I4 int
#define U4 unsigned int
#define I8 long
#define U8 unsigned long
#define R4 float
#define R8 double
#define X4 float
#define X8 double
#define C1 char
#define B1 unsigned char

enum DataTags {
    MTdata = 0,
    I4data,
    I8data,
    U4data,
    U8data,
    R4data,
    R8data,
    X4data,
    X8data,
    C1data,
    B1data,
    LKdata
};

static struct DataType {
    char *name;
    int type;
    int bytes;
    int size;
} DataList[] = {
    {"MT", MTdata,  0, 0},
    {"I4", I4data,  4, sizeof(I4)},
    {"I8", I8data,  8, sizeof(I8)},
    {"U4", U4data,  4, sizeof(U4)},
    {"U8", U8data,  8, sizeof(U8)},
    {"R4", R4data,  4, sizeof(R4)},
    {"R8", R8data,  8, sizeof(R8)},
    {"X4", X4data,  8, sizeof(X4)*2},
    {"X8", X8data, 16, sizeof(X8)*2},
    {"C1", C1data,  1, 1},
    {"B1", B1data,  1, 1},
    {"LK", LKdata,  0, 0}
};

#define NumDataTypes (sizeof(DataList)/sizeof(struct DataType))

static double RootID = 0.0;

/*===================================================================
 *           local routines
 *===================================================================*/

/*---------- get_error ----------------------------------------------
 * save error message
 *-------------------------------------------------------------------*/

static int get_error (
#ifdef PROTOTYPE
    Tcl_Interp *interp, char *funcname, int errcode)
#else
    interp, funcname, errcode)
Tcl_Interp *interp;
char *funcname;
int errcode;
#endif
{
    char *p, errmsg[ADF_MAX_ERROR_STR_LENGTH+1];

    ADF_Error_Message (errcode, errmsg);
    if (NULL == (p = strchr (errmsg, ':')))
        p = errmsg;
    else
        p++;
    while (*p && isspace (*p))
        p++;
    Tcl_AppendResult (interp, /*funcname, ":",*/ p, NULL);
    return TCL_ERROR;
}

/*---------- get_node ----------------------------------------------
 * get the node ID for a node
 *------------------------------------------------------------------*/

static int get_node (
#ifdef PROTOTYPE
    Tcl_Interp *interp, char *nodename, double *nodeid)
#else
    interp, nodename, nodeid)
Tcl_Interp *interp;
char *nodename;
double *nodeid;
#endif
{
    int err;

    if (!*nodename || 0 == strcmp (nodename, "/"))
        *nodeid = RootID;
    else {
        ADF_Get_Node_ID (RootID, nodename, nodeid, &err);
        if (err > 0)
            return (get_error (interp, "ADF_Get_Node_ID", err));
    }
    return TCL_OK;
}

/*---------- get_parent ----------------------------------------------
 * get the parent node ID for a node
 *------------------------------------------------------------------*/

static char *get_parent (
#ifdef PROTOTYPE
    Tcl_Interp *interp, char *nodename, double *nodeid)
#else
    interp, nodename, nodeid)
Tcl_Interp *interp;
char *nodename;
double *nodeid;
#endif
{
    int err;
    char *node;

    if (!*nodename || 0 == strcmp (nodename, "/") ||
        NULL == (node = strrchr (nodename, '/'))) {
        Tcl_AppendResult (interp, "node does not have a parent", NULL);
        return NULL;
    }
    if (node == nodename)
        *nodeid = RootID;
    else {
        *node = 0;
        ADF_Get_Node_ID (RootID, nodename, nodeid, &err);
        if (err > 0) {
            get_error (interp, "ADF_Get_Node_ID", err);
            return NULL;
        }
        *node = '/';
    }
    return (++node);
}

/*---------- data_size ---------------------------------------------
 * return number of data values for a node
 *------------------------------------------------------------------*/

static int data_size (
#ifdef PROTOTYPE
    Tcl_Interp *interp, double nodeid)
#else
    interp, nodeid)
Tcl_Interp *interp;
double nodeid;
#endif
{
    int err, n, np;
    int ndim, dims[ADF_MAX_DIMENSIONS];

    ADF_Get_Number_of_Dimensions (nodeid, &ndim, &err);
    if (err > 0) {
        get_error (interp, "ADF_Get_Number_of_Dimensions", err);
        return -1;
    }
    if (ndim == 0)
        return 0;
    ADF_Get_Dimension_Values (nodeid, dims, &err);
    if (err > 0) {
        get_error (interp, "ADF_Get_Dimension_Values", err);
        return -1;
    }
    for (np = 1, n = 0; n < ndim; n++)
        np *= dims[n];
    return np;
}

/*---------- data_type ---------------------------------------------
 * return number of data values for a node
 *------------------------------------------------------------------*/

static struct DataType * data_type (
#ifdef PROTOTYPE
    Tcl_Interp *interp, double nodeid)
#else
    interp, nodeid)
Tcl_Interp *interp;
double nodeid;
#endif
{
    int err, n;
    char type[ADF_DATA_TYPE_LENGTH+1];

    ADF_Get_Data_Type (nodeid, type, &err);
    if (err > 0) {
        get_error (interp, "ADF_Get_Data_Type", err);
        return NULL;
    }
    for (n = 0; n < ADF_DATA_TYPE_LENGTH && type[n]; n++) {
        if (islower (type[n]))
            type[n] = toupper (type[n]);
    }
    for (n = 0; n < NumDataTypes; n++) {
        if (0 == strncmp (type, DataList[n].name, 2))
            return (&DataList[n]);
    }
    Tcl_AppendResult (interp, "unrecognized data type:", type, NULL);
    return NULL;
}

/*---------- convert_C1 --------------------------------------------
 * convert data from C1
 *------------------------------------------------------------------*/

static char *convert_C1 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    C1 *d = (C1 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == C1data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I4)*d++;
            break;
        }
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I8)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R4)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R8)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_B1 --------------------------------------------
 * convert data from B1
 *------------------------------------------------------------------*/

static char *convert_B1 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    B1 *d = (B1 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == B1data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I4)*d++;
            break;
        }
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I8)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R4)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R8)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++)
                *c++ = (C1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_I4 --------------------------------------------
 * convert data from I4
 *------------------------------------------------------------------*/

static char *convert_I4 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    I4 *d = (I4 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == I4data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I8)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R4)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R8)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++)
                *c++ = (C1)*d++;
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_U4 --------------------------------------------
 * convert data from U4
 *------------------------------------------------------------------*/

static char *convert_U4 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    U4 *d = (U4 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == U4data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I8)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R4)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R8)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++)
                *c++ = (C1)*d++;
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_I8 --------------------------------------------
 * convert data from I8
 *------------------------------------------------------------------*/

static char *convert_I8 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    I8 *d = (I8 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == I8data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I4)*d++;
            break;
        }
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U4)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R4)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R8)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++)
                *c++ = (C1)*d++;
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_U8 --------------------------------------------
 * convert data from U8
 *------------------------------------------------------------------*/

static char *convert_U8 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    U8 *d = (U8 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == U8data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I4)*d++;
            break;
        }
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R4)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R8)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++)
                *c++ = (C1)*d++;
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_R4 --------------------------------------------
 * convert data from R4
 *------------------------------------------------------------------*/

static char *convert_R4 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    R4 *d = (R4 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == R4data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I4)*d++;
            break;
        }
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I8)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U8)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R8)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++)
                *c++ = (C1)*d++;
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_R8 --------------------------------------------
 * convert data from R8
 *------------------------------------------------------------------*/

static char *convert_R8 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    R8 *d = (R8 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == R8data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I4)*d++;
            break;
        }
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++)
                *i++ = (I8)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++)
                *u++ = (U8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++)
                *r++ = (R4)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = 0.0;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++)
                *c++ = (C1)*d++;
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_X4 --------------------------------------------
 * convert data from X4
 *------------------------------------------------------------------*/

static char *convert_X4 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    X4 *d = (X4 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == X4data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++, d++)
                *i++ = (I4)*d++;
            break;
        }
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++, d++)
                *u++ = (U4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++, d++)
                *i++ = (I8)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++, d++)
                *u++ = (U8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++, d++)
                *r++ = (R4)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++, d++)
                *r++ = (R8)*d++;
            break;
        }
        case X8data: {
            X8 *x = (X8 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X8)*d++;
                *x++ = (X8)*d++;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++, d++)
                *c++ = (C1)*d++;
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++, d++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*---------- convert_X8 --------------------------------------------
 * convert data from C1
 *------------------------------------------------------------------*/

static char *convert_X8 (
#ifdef PROTOTYPE
    int np, char *olddata, struct DataType *dtnew)
#else
    np, olddata, dtnew)
int np;
char *olddata;
struct DataType *dtnew;
#endif
{
    int n;
    X8 *d = (X8 *)olddata;
    char *newdata;

    if (np < 1 || dtnew->size == 0) return NULL;
    if (dtnew->type == X8data) return olddata;
    newdata = (char *) calloc (np, dtnew->size);
    if (NULL == newdata) return NULL;

    switch (dtnew->type) {
        case I4data: {
            I4 *i = (I4 *)newdata;
            for (n = 0; n < np; n++, d++)
                *i++ = (I4)*d++;
            break;
        }
        case U4data: {
            U4 *u = (U4 *)newdata;
            for (n = 0; n < np; n++, d++)
                *u++ = (U4)*d++;
            break;
        }
        case I8data: {
            I8 *i = (I8 *)newdata;
            for (n = 0; n < np; n++, d++)
                *i++ = (I8)*d++;
            break;
        }
        case U8data: {
            U8 *u = (U8 *)newdata;
            for (n = 0; n < np; n++, d++)
                *u++ = (U8)*d++;
            break;
        }
        case R4data: {
            R4 *r = (R4 *)newdata;
            for (n = 0; n < np; n++, d++)
                *r++ = (R4)*d++;
            break;
        }
        case R8data: {
            R8 *r = (R8 *)newdata;
            for (n = 0; n < np; n++, d++)
                *r++ = (R8)*d++;
            break;
        }
        case X4data: {
            X4 *x = (X4 *)newdata;
            for (n = 0; n < np; n++) {
                *x++ = (X4)*d++;
                *x++ = (X4)*d++;
            }
            break;
        }
        case C1data: {
            C1 *c = (C1 *)newdata;
            for (n = 0; n < np; n++, d++)
                *c++ = (C1)*d++;
            break;
        }
        case B1data: {
            B1 *b = (B1 *)newdata;
            for (n = 0; n < np; n++, d++)
                *b++ = (B1)*d++;
            break;
        }
        default:
            free (newdata);
            newdata = NULL;
            break;
    }
    return newdata;
}

/*===================================================================
 *           tcl routines
 *===================================================================*/

/*---------- CGNSversion -----------------------------------------------
 * get CGNS library version
 *----------------------------------------------------------------------*/

static int CGNSversion (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char version[33];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], "\"", NULL);
        return TCL_ERROR;
    }
    sprintf (version, "%g", CGNS_DOTVERS);
    Tcl_AppendResult (interp, version, NULL);
    return TCL_OK;
}

/*---------- CGNSfile --------------------------------------------------
 * detect whether file is ADF or HDF5
 *----------------------------------------------------------------------*/

static int CGNSfile (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n;
    char buf[25];
    FILE *fp;
    static char *HDF5sig = "\211HDF\r\n\032\n";

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " filename\"", NULL);
        return TCL_ERROR;
    }
    if (NULL == (fp = fopen (argv[1], "rb"))) {
        Tcl_AppendResult (interp, "couldn't open \"",
            argv[1], "\" for reading", NULL);
        return TCL_ERROR;
    }
    fread (buf, 1, 24, fp);
    buf[24] = 0;
    fclose (fp);

    /* check for HDF5 */

    for (n = 0; n < 8; n++) {
        if (buf[n] != HDF5sig[n]) break;
    }
    if (n == 8) {
        Tcl_SetResult (interp, "HDF5", TCL_STATIC);
        return TCL_OK;
    }

    /* ADF file */

    for (n = 0; n < 4; n++)
        buf[n] &= 0x7F;
    if (0 == strcmp (buf, "@(#)ADF Database Version")) {
        Tcl_SetResult (interp, "ADF", TCL_STATIC);
        return TCL_OK;
    }

    /* unknown type */

    Tcl_SetResult (interp, "unknown file type", TCL_STATIC);
    return TCL_ERROR;
}

/*---------- ADFversion ------------------------------------------------
 * get ADF library version
 *----------------------------------------------------------------------*/

static int ADFversion (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char version[ADF_VERSION_LENGTH+1];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], "\"", NULL);
        return TCL_ERROR;
    }

    ADF_Library_Version (version, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Library_Version", err));
    Tcl_AppendResult (interp, version, NULL);
    return TCL_OK;
}

/*---------- ADFfile ---------------------------------------------------
 * checks if file can be opened
 *----------------------------------------------------------------------*/

static int ADFfile (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char *status = NULL, *format = NULL;
    char rootname[ADF_NAME_LENGTH+1];
    double root_id;

    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 4) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " filename ?status? ?format?\"", NULL);
        return TCL_ERROR;
    }

    if (argc > 2) {
        status = argv[2];
        if (argc > 3)
            format = argv[3];
    }
    if (status == NULL)
        status = "unknown";
    if (format == NULL)
        format = "native";

    ADF_Database_Open (argv[1], status, format, &root_id, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Database_Open", err));
    ADF_Get_Name (root_id, rootname, &err);
    if (err <= 0)
        Tcl_AppendResult (interp, rootname, NULL);
    ADF_Database_Close (root_id, &err);
    return TCL_OK;
}

/*---------- ADFopen ---------------------------------------------------
 * open ADF database
 *----------------------------------------------------------------------*/

static int ADFopen (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char *status = NULL, *format = NULL;
    char rootname[ADF_NAME_LENGTH+1];
    double root_id;

    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 4) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " filename ?status? ?format?\"", NULL);
        return TCL_ERROR;
    }

    if (RootID != 0.0) {
        ADF_Database_Close (RootID, &err);
        RootID = 0.0;
    }

    if (argc > 2) {
        status = argv[2];
        if (argc > 3)
            format = argv[3];
    }
    if (status == NULL)
        status = "unknown";
    if (format == NULL)
        format = "native";

    ADF_Database_Open (argv[1], status, format, &root_id, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Database_Open", err));

    RootID = root_id;
    ADF_Get_Name (root_id, rootname, &err);
    if (err <= 0)
        Tcl_AppendResult (interp, rootname, NULL);
    return TCL_OK;
}

/*---------- ADFclose --------------------------------------------------
 * close ADF database file
 *----------------------------------------------------------------------*/

static int ADFclose (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;

    Tcl_ResetResult (interp);
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no ADF database is open", NULL);
        return TCL_ERROR;
    }

    ADF_Database_Close (RootID, &err);
    RootID = 0.0;
    if (err > 0)
        return (get_error (interp, "ADF_Database_Close", err));
    return TCL_OK;
}

/*---------- ADFnode ---------------------------------------------------
 * get type of node
 *----------------------------------------------------------------------*/

static int ADFnode (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err, len;
    double node_id;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }
    if (0 == strcmp (argv[1], "/")) {
        Tcl_SetResult (interp, "root", TCL_STATIC);
        return TCL_OK;
    }
    ADF_Get_Node_ID (RootID, argv[1], &node_id, &err);
    if (err <= 0) {
        ADF_Is_Link (node_id, &len, &err);
        if (err <= 0 && len > 0)
            Tcl_SetResult (interp, "link", TCL_STATIC);
        else
            Tcl_SetResult (interp, "node", TCL_STATIC);
    }
    return TCL_OK;
}

/*---------- ADFformat -------------------------------------------------
 * get/set ADF database format
 *----------------------------------------------------------------------*/

static int ADFformat (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char format[ADF_FORMAT_LENGTH+1];

    Tcl_ResetResult (interp);
    if (argc > 2) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " ?newformat?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (argc == 2) {
        ADF_Database_Set_Format (RootID, argv[1], &err);
        if (err > 0)
            return (get_error (interp, "ADF_Database_Set_Format", err));
    }
    ADF_Database_Get_Format (RootID, format, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Database_Get_Format", err));
    Tcl_AppendResult (interp, format, NULL);
    return TCL_OK;
}

/*---------- ADFname ---------------------------------------------------
 * get/set name for a node
 *----------------------------------------------------------------------*/

static int ADFname (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char name[ADF_NAME_LENGTH+1];
    double parent_id, node_id;

    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 3) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node ?newname?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;
    if (argc == 3) {
        if (NULL == get_parent (interp, argv[1], &parent_id))
            return TCL_ERROR;
        ADF_Put_Name (parent_id, node_id, argv[2], &err);
        if (err > 0)
            return (get_error (interp, "ADF_Put_Name", err));
    }
    ADF_Get_Name (node_id, name, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Get_Name", err));
    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*---------- ADFlabel --------------------------------------------------
 * get/set label for a node
 *----------------------------------------------------------------------*/

static int ADFlabel (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char label[ADF_LABEL_LENGTH+1];
    double node_id;

    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 3) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node ?newlabel?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;
    if (argc == 3) {
        ADF_Set_Label (node_id, argv[2], &err);
        if (err > 0)
            return (get_error (interp, "ADF_Set_Label", err));
    }
    ADF_Get_Label (node_id, label, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Get_Label", err));
    Tcl_AppendResult (interp, label, NULL);
    return TCL_OK;
}

/*---------- ADFtype ---------------------------------------------------
 * get data type for a node
 *----------------------------------------------------------------------*/

static int ADFtype (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, np, err;
    int ndim, dims[ADF_MAX_DIMENSIONS];
    char type[ADF_DATA_TYPE_LENGTH+1];
    double node_id;
    struct DataType *dtnew, *dtold;
    char *olddata, *newdata;

    Tcl_ResetResult (interp);
    if (argc == 1) {
        for (ndim = 0; ndim < NumDataTypes; ndim++)
            Tcl_AppendElement (interp, DataList[ndim].name);
        return TCL_OK;
    }
    if (argc > 3) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node ?newtype?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;

    if (argc > 2) {

        /* get new data type */

        strncpy (type, argv[2], ADF_DATA_TYPE_LENGTH);
        type[ADF_DATA_TYPE_LENGTH] = 0;
        for (n = 0; n < ADF_DATA_TYPE_LENGTH && type[n]; n++) {
            if (islower (type[n]))
                type[n] = toupper (type[n]);
        }
        dtnew = NULL;
        for (n = 0; n < NumDataTypes; n++) {
            if (0 == strncmp (type, DataList[n].name, 2)) {
                dtnew = &DataList[n];
                break;
            }
        }
        if (NULL == dtnew) {
            Tcl_AppendResult (interp, "unrecognized data type", NULL);
            return TCL_ERROR;
        }

        /* get old data type */

        dtold = data_type (interp, node_id);
        if (NULL == dtold)
            return TCL_ERROR;

        /* convert the data */

        if (dtnew->type != dtold->type) {

            /* get current data size */

            np = 0;
            ADF_Get_Number_of_Dimensions (node_id, &ndim, &err);
            if (err > 0)
                return (get_error (interp,
                    "ADF_Get_Number_of_Dimensions", err));
            if (ndim > 0) {
                ADF_Get_Dimension_Values (node_id, dims, &err);
                if (err > 0)
                    return (get_error (interp,
                        "ADF_Get_Dimension_Values", err));
                for (np = 1, n = 0; n < ndim; n++)
                    np *= dims[n];
            }
            newdata = NULL;

            if (np > 0 && dtold->size > 0) {
                olddata = (char *) malloc (np * dtold->size);
                if (NULL == olddata) {
                    Tcl_AppendResult (interp, "malloc failed for data", NULL);
                    return TCL_ERROR;
                }
                ADF_Read_All_Data (node_id, olddata, &err);
                if (err > 0) {
                    free (olddata);
                    return (get_error (interp, "ADF_Read_All_Data", err));
                }
                switch (dtold->type) {
                    case C1data:
                        newdata = convert_C1 (np, olddata, dtnew);
                        break;
                    case B1data:
                        newdata = convert_B1 (np, olddata, dtnew);
                        break;
                    case I4data:
                        newdata = convert_I4 (np, olddata, dtnew);
                        break;
                    case U4data:
                        newdata = convert_U4 (np, olddata, dtnew);
                        break;
                    case I8data:
                        newdata = convert_I8 (np, olddata, dtnew);
                        break;
                    case U8data:
                        newdata = convert_U8 (np, olddata, dtnew);
                        break;
                    case R4data:
                        newdata = convert_R4 (np, olddata, dtnew);
                        break;
                    case R8data:
                        newdata = convert_R8 (np, olddata, dtnew);
                        break;
                    case X4data:
                        newdata = convert_X4 (np, olddata, dtnew);
                        break;
                    case X8data:
                        newdata = convert_X8 (np, olddata, dtnew);
                        break;
                }
                if (newdata != olddata)
                    free (olddata);
            }

            /* rewrite the data */

            if (NULL == newdata) ndim = 0;
            ADF_Put_Dimension_Information (node_id,
                dtnew->name, ndim, dims, &err);
            if (err > 0)
                return (get_error (interp,
                    "ADF_Put_Dimension_Information", err));
            if (NULL != newdata) {
                ADF_Write_All_Data (node_id, newdata, &err);
                free (newdata);
                if (err > 0)
                    return (get_error (interp,
                        "ADF_Write_All_Data", err));
            }
        }
    }
    ADF_Get_Data_Type (node_id, type, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Get_Data_Type", err));
    Tcl_AppendResult (interp, type, NULL);
    return TCL_OK;
}

/*---------- ADFdimensions ---------------------------------------------
 * get/set data dimensions for a node
 *----------------------------------------------------------------------*/

static int ADFdimensions (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, err;
    int ndim, dims[ADF_MAX_DIMENSIONS];
    double node_id;
    char **args, type[ADF_DATA_TYPE_LENGTH+1];

    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 3) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node ?newdimensions?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;
    if (argc > 2) {
        ADF_Get_Data_Type (node_id, type, &err);
        if (err > 0)
            return (get_error (interp, "ADF_Get_Data_Type", err));
        if (TCL_OK != Tcl_SplitList (interp, argv[2], &ndim, &args))
            return TCL_ERROR;
        if (ndim > ADF_MAX_DIMENSIONS) {
            Tcl_Free ((char *)args);
            Tcl_AppendResult (interp, "invalid number of dimensions", NULL);
            return TCL_ERROR;
        }
        if (ndim) {
            for (n = 0; n < ndim; n++)
                dims[n] = atoi (args[n]);
            Tcl_Free ((char *)args);
        }
        ADF_Put_Dimension_Information (node_id, type, ndim, dims, &err);
        if (err > 0)
            return (get_error (interp, "ADF_Put_Dimension_Information", err));
    }
    ADF_Get_Number_of_Dimensions (node_id, &ndim, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Get_Number_of_Dimensions", err));
    if (ndim > 0) {
        ADF_Get_Dimension_Values (node_id, dims, &err);
        if (err > 0)
            return (get_error (interp, "ADF_Get_Dimension_Values", err));
        for (n = 0; n < ndim; n++) {
            sprintf (type, "%d", dims[n]);
            Tcl_AppendElement (interp, type);
        }
    }
    return TCL_OK;
}

/*---------- ADFsize ---------------------------------------------------
 * get number of bytes of data for a node
 *----------------------------------------------------------------------*/

static int ADFsize (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int np;
    char str[65];
    double node_id;
    struct DataType *type;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;

    np = data_size (interp, node_id);
    if (np == -1)
        return TCL_ERROR;
    type = data_type (interp, node_id);
    if (NULL == type)
        return TCL_ERROR;
    sprintf (str, "%u", (unsigned)np * (unsigned)type->bytes);
    Tcl_AppendResult (interp, str, NULL);
    return TCL_OK;
}

/*---------- ADFread ---------------------------------------------------
 * read node data
 *----------------------------------------------------------------------*/

static int ADFread (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err, n, np;
    char *values, str[65];
    double node_id;
    struct DataType *dt;

    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 14) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node ?range1 range2 ...?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;
    np = data_size (interp, node_id);
    if (np == -1)
        return TCL_ERROR;
    dt = data_type (interp, node_id);
    if (NULL == dt)
        return TCL_ERROR;
    if (np == 0 || dt->size == 0)
        return TCL_OK;

    values = (char *) malloc ((unsigned)np * (unsigned)dt->size + 1);
    if (NULL == values) {
        Tcl_AppendResult (interp, "malloc failed for data", NULL);
        return TCL_ERROR;
    }

    ADF_Read_All_Data (node_id, values, &err);
    if (err > 0) {
        free (values);
        return (get_error (interp, "ADF_Read_All_Data", err));
    }

    if (dt->type == C1data) {
        values[np] = 0;
        Tcl_AppendResult (interp, values, NULL);
    }
    else if (dt->type == B1data) {
        B1 *u = (B1 *)values;
        for (n = 0; n < np; n++, u++) {
            sprintf (str, "%d", (int)*u);
            Tcl_AppendElement (interp, str);
        }
    }
    else if (dt->type == I4data) {
        I4 *i = (I4 *)values;
        for (n = 0; n < np; n++, i++) {
            sprintf (str, "%d", *i);
            Tcl_AppendElement (interp, str);
        }
    }
    else if (dt->type == U4data) {
        U4 *u = (U4 *)values;
        for (n = 0; n < np; n++, u++) {
            sprintf (str, "%u", *u);
            Tcl_AppendElement (interp, str);
        }
    }
    else if (dt->type == I8data) {
        I8 *i = (I8 *)values;
        for (n = 0; n < np; n++, i++) {
            sprintf (str, "%ld", *i);
            Tcl_AppendElement (interp, str);
        }
    }
    else if (dt->type == U8data) {
        U8 *u = (U8 *)values;
        for (n = 0; n < np; n++, u++) {
            sprintf (str, "%lu", *u);
            Tcl_AppendElement (interp, str);
        }
    }
    else if (dt->type == R4data) {
        R4 *r = (R4 *)values;
        for (n = 0; n < np; n++, r++) {
            sprintf (str, "%g", *r);
            Tcl_AppendElement (interp, str);
        }
    }
    else if (dt->type == R8data) {
        R8 *r = (R8 *)values;
        for (n = 0; n < np; n++, r++) {
            sprintf (str, "%g", *r);
            Tcl_AppendElement (interp, str);
        }
    }
    else if (dt->type == X4data) {
        X4 *r = (X4 *)values;
        for (n = 0; n < np; n++, r++) {
            sprintf (str, "%g %g", *r, *(r+1));
            Tcl_AppendElement (interp, str);
            r++;
        }
    }
    else if (dt->type == X8data) {
        X8 *r = (X8 *)values;
        for (n = 0; n < np; n++, r++) {
            sprintf (str, "%g %g", *r, *(r+1));
            Tcl_AppendElement (interp, str);
            r++;
        }
    }
    else {
        free (values);
        Tcl_AppendResult (interp, "internal error - should not happen", NULL);
        return TCL_ERROR;
    }

    free (values);
    return TCL_OK;
}

/*---------- ADFwrite --------------------------------------------------
 * write node data
 *----------------------------------------------------------------------*/

static int ADFwrite (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err, n, np, nv, ns;
    int ndim, dims[ADF_MAX_DIMENSIONS];
    double node_id;
    char **args, *values, type[ADF_DATA_TYPE_LENGTH+1];
    struct DataType *dt = NULL;

    Tcl_ResetResult (interp);
    if (argc < 3 || argc > 5) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node type ?dimensions? ?data?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    /* get node ID for node */

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;

    /* get data type */

    strncpy (type, argv[2], ADF_DATA_TYPE_LENGTH);
    type[ADF_DATA_TYPE_LENGTH] = 0;
    for (n = 0; n < ADF_DATA_TYPE_LENGTH && type[n]; n++) {
        if (islower (type[n]))
            type[n] = toupper (type[n]);
    }
    for (n = 0; n < NumDataTypes; n++) {
        if (0 == strncmp (type, DataList[n].name, 2)) {
            dt = &DataList[n];
            break;
        }
    }
    if (dt == NULL) {
        Tcl_AppendResult (interp, "data type not recognized", NULL);
        return TCL_ERROR;
    }

    /* get dimensions */

    ndim = 0;
    args = NULL;
    if (argc > 3 &&
        TCL_OK != Tcl_SplitList (interp, argv[3], &ndim, &args))
        return TCL_ERROR;
    if (ndim == 0) {
        ADF_Put_Dimension_Information (node_id, dt->name, ndim, dims, &err);
        if (err > 0)
            return (get_error (interp, "ADF_Put_Dimension_Information", err));
        return TCL_OK;
    }

    if (ndim > ADF_MAX_DIMENSIONS) {
        Tcl_Free ((char *)args);
        Tcl_AppendResult (interp, "invalid number of dimensions", NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < ndim; n++)
        dims[n] = atoi (args[n]);
    Tcl_Free ((char *)args);
    for (np = 1, n = 0; n < ndim; n++) {
        if (dims[n] < 1) {
            Tcl_AppendResult (interp, "invalid dimension", NULL);
            return TCL_ERROR;
        }
        np *= dims[n];
    }

    /* create data array */

    if (NULL == (values = (char *) calloc (np, dt->size))) {
        Tcl_AppendResult (interp, "malloc failed for data", NULL);
        return TCL_ERROR;
    }

    /* get data */

    if (argc > 4) {
        if (dt->type == C1data && ndim == 1) {
            strncpy (values, argv[4], np);
            for (ns = strlen(argv[4]); ns < np; ns++)
                values[ns] = ' ';
        }
        else {
            if (TCL_OK != Tcl_SplitList (interp, argv[4], &nv, &args)) {
                free (values);
                return TCL_ERROR;
            }
            if (nv) {
                if (dt->type == C1data) np /= dims[0];
                if (np > nv) np = nv;

                if (dt->type == C1data) {
                    char *p = values;
                    for (n = 0; n < np; n++) {
                        strncpy (p, args[n], dims[0]);
                        for (ns = strlen(args[n]); ns < dims[0]; ns++)
                            p[ns] = ' ';
                        p += dims[0];
                    }
                }
                else if (dt->type == B1data) {
                    B1 *u = (B1 *)values;
                    for (n = 0; n < np; n++, u++)
                        *u = (B1) atoi (args[n]);
                }
                else if (dt->type == I4data) {
                    I4 *i = (I4 *)values;
                    for (n = 0; n < np; n++, i++)
                        *i = (I4) atoi (args[n]);
                }
                else if (dt->type == U4data) {
                    U4 *u = (U4 *)values;
                    for (n = 0; n < np; n++, u++)
                        *u = (U4) atoi (args[n]);
                }
                else if (dt->type == I8data) {
                    I8 *i = (I8 *)values;
                    for (n = 0; n < np; n++, i++)
                        *i = (I8) atol (args[n]);
                }
                else if (dt->type == U8data) {
                    U8 *u = (U8 *)values;
                    for (n = 0; n < np; n++, u++)
                        *u = (U8) atol (args[n]);
                }
                else if (dt->type == R4data) {
                    R4 *r = (R4 *)values;
                    for (n = 0; n < np; n++, r++)
                        *r = (R4) atof (args[n]);
                }
                else if (dt->type == R8data) {
                    R8 *r = (R8 *)values;
                    for (n = 0; n < np; n++, r++)
                        *r = (R8) atof (args[n]);
                }
                else if (dt->type == X4data) {
                    float fr, fi;
                    X4 *r = (X4 *)values;
                    for (n = 0; n < np; n++, r++) {
                        if (2 != sscanf (args[n], "%f %f", &fr, &fi)) {
                            fr = (float) atof (args[n]);
                            fi = 0.0;
                        }
                        *r = (R4) fr;
                        *++r = (R4) fi;
                    }
                }
                else if (dt->type == X8data) {
                    double dr, di;
                    X8 *r = (X8 *)values;
                    for (n = 0; n < np; n++, r++) {
                        if (2 != sscanf (args[n], "%lf %lf", &dr, &di)) {
                            dr = atof (args[n]);
                            di = 0.0;
                        }
                        *r = (X8) dr;
                        *++r = (X8) di;
                    }
                }
                else {
                    Tcl_Free ((char *)args);
                    free (values);
                    Tcl_AppendResult (interp, "internal error - should not happen", NULL);
                    return TCL_ERROR;
                }

                Tcl_Free ((char *)args);
            }
        }
    }

    ADF_Put_Dimension_Information (node_id, dt->name, ndim, dims, &err);
    if (err > 0) {
        free (values);
        return (get_error (interp, "ADF_Put_Dimension_Information", err));
    }
    ADF_Write_All_Data (node_id, values, &err);
    free (values);
    if (err > 0)
        return (get_error (interp, "ADF_Write_All_Data", err));
    return TCL_OK;
}

/*---------- ADFlink ---------------------------------------------------
 * create/retrieve link of a node
 *----------------------------------------------------------------------*/

static int ADFlink (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err, len;
    char *node, name_in_file[ADF_MAX_LINK_DATA_SIZE+1];
    char file_name[ADF_FILENAME_LENGTH+1];
    double parent_id, node_id;

    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 4) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node ?linknode? ?linkfile?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (argc > 2) {
        if (NULL == (node = get_parent (interp, argv[1], &parent_id)))
            return TCL_ERROR;
        strcpy (name_in_file, argv[2]);
        if (argc > 3)
            strcpy (file_name, argv[3]);
        else
            file_name[0] = 0;
        ADF_Link (parent_id, node, file_name, name_in_file, &node_id, &err);
        if (err > 0)
            return (get_error (interp, "ADF_Link", err));
    }
    else {
        if (get_node (interp, argv[1], &node_id))
            return TCL_ERROR;
    }
    ADF_Is_Link (node_id, &len, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Is_Link", err));
    if (len > 0) {
        ADF_Get_Link_Path (node_id, file_name, name_in_file, &err);
        if (err > 0)
            return (get_error (interp, "ADF_Get_Link_Path", err));
        Tcl_AppendElement (interp, name_in_file);
        Tcl_AppendElement (interp, file_name);
    }
    return TCL_OK;
}

/*---------- ADFchildren -----------------------------------------------
 * retrieve children of a node
 *----------------------------------------------------------------------*/

static int ADFchildren (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err, n, nchildren, len;
    char name[ADF_NAME_LENGTH+1];
    double node_id;
#ifdef NULL_NODEID_POINTER
    double *ids;
#endif

    Tcl_ResetResult (interp);
    if (2 != argc) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;
    ADF_Number_of_Children (node_id, &nchildren, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Number_of_Children", err));
    if (nchildren < 1)
        return TCL_OK;
#ifdef NULL_NODEID_POINTER
    ids = (double *) malloc (nchildren * sizeof(double));
    if (NULL == ids) {
        Tcl_AppendResult (interp, "malloc failed for IDs", NULL);
        return TCL_ERROR;
    }
    ADF_Children_IDs (node_id, 1, nchildren, &len, ids, &err);
    if (err > 0) {
        free (ids);
        Tcl_ResetResult (interp);
        return (get_error (interp, "ADF_Children_IDs", err));
    }
    for (n = 0; n < nchildren; n++) {
        ADF_Get_Name (ids[n], name, &err);
        if (err > 0) {
            free (ids);
            Tcl_ResetResult (interp);
            return (get_error (interp, "ADF_Get_Name", err));
        }
        Tcl_AppendElement (interp, name);
    }
    free (ids);
#else
    for (n = 1; n <= nchildren; n++) {
        ADF_Children_Names (node_id, n, 1, ADF_NAME_LENGTH+1, &len, name, &err);
        if (err > 0) {
            Tcl_ResetResult (interp);
            return (get_error (interp, "ADF_Children_Names", err));
        }
        Tcl_AppendElement (interp, name);
    }
#endif
    return TCL_OK;
}

/*---------- ADFnumchild -----------------------------------------------
 * returns number of children for a node
 *----------------------------------------------------------------------*/

static int ADFnumchild (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err, nchildren;
    double node_id;
    char buf[33];

    Tcl_ResetResult (interp);
    if (2 != argc) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;
    ADF_Number_of_Children (node_id, &nchildren, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Number_of_Children", err));
    sprintf (buf, "%d", nchildren);
    Tcl_AppendResult (interp, buf, NULL);
    return TCL_OK;
}

/*---------- ADFchildname ----------------------------------------------
 * retrieve child name for child index
 *----------------------------------------------------------------------*/

static int ADFchildname (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err, n, len;
    char name[ADF_NAME_LENGTH+1];
    double node_id;

    Tcl_ResetResult (interp);
    if (3 != argc) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node childnum\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (get_node (interp, argv[1], &node_id))
        return TCL_ERROR;
    ADF_Children_Names (node_id, atoi(argv[2]), 1, ADF_NAME_LENGTH+1,
        &len, name, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Children_Names", err));
    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*---------- ADFcreate -------------------------------------------------
 * create a new node
 *----------------------------------------------------------------------*/

static int ADFcreate (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char *node, label[ADF_LABEL_LENGTH+1];
    double parent_id, node_id;

    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 3) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node ?label?\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (NULL == (node = get_parent (interp, argv[1], &parent_id)))
        return TCL_ERROR;
    ADF_Create (parent_id, node, &node_id, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Create", err));
    if (argc > 2) {
        ADF_Set_Label (node_id, argv[2], &err);
        if (err > 0)
            return (get_error (interp, "ADF_Set_Label", err));
    }
    ADF_Get_Label (node_id, label, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Get_Label", err));
    Tcl_AppendResult (interp, label, NULL);
    return TCL_OK;
}

/*---------- ADFdelete -------------------------------------------------
 * delete a node
 *----------------------------------------------------------------------*/

static int ADFdelete (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char *node;
    double parent_id, node_id;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (NULL == (node = get_parent (interp, argv[1], &parent_id)))
        return TCL_ERROR;
    ADF_Get_Node_ID (parent_id, node, &node_id, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Get_Node_ID", err));
    ADF_Delete (parent_id, node_id, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Delete", err));
    return TCL_OK;
}

/*---------- ADFmove ---------------------------------------------------
 * move a node
 *----------------------------------------------------------------------*/

static int ADFmove (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int err;
    char *node;
    double parent_id, node_id, new_parent_id;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "wrong # args: should be \"",
            argv[0], " node parent\"", NULL);
        return TCL_ERROR;
    }
    if (RootID == 0.0) {
        Tcl_AppendResult (interp, "no database is open", NULL);
        return TCL_ERROR;
    }

    if (NULL == (node = get_parent (interp, argv[1], &parent_id)))
        return TCL_ERROR;
    ADF_Get_Node_ID (parent_id, node, &node_id, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Get_Node_ID", err));
    if (get_node (interp, argv[2], &new_parent_id))
        return TCL_ERROR;
    ADF_Move_Child (parent_id, node_id, new_parent_id, &err);
    if (err > 0)
        return (get_error (interp, "ADF_Move_Child", err));
    return TCL_OK;
}

#ifdef SINGLE_COMMAND

/*---------- ADFcommand ----------------------------------------
 * process ADF command
 *--------------------------------------------------------------*/

static int ADFcommand (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    static char usg_msg[] =
        "ADF version\n"
        "ADF open filename ?status? ?format?\n"
        "ADF file filename ?status? ?format?\n"
        "ADF close\n"
        "ADF node node\n"
        "ADF format ?newformat?\n"
        "ADF name node ?newname?\n"
        "ADF label node ?newlabel?\n"
        "ADF type node ?newtype?\n"
        "ADF dimensions node ?newdimensions?\n"
        "ADF size node\n"
        "ADF read node ?range1 range2 ...?\n"
        "ADF write node type dimensions data\n"
        "ADF link node ?linknode? ?linkfile?\n"
        "ADF children node\n"
        "ADF numchild node\n"
        "ADF childname node childnum\n"
        "ADF create node ?label?\n"
        "ADF delete node\n"
        "ADF move node newparent\n";

    if (argc < 2) {
        Tcl_SetResult (interp, usg_msg, TCL_STATIC);
        return TCL_ERROR;
    }
    if (0 == strcmp (argv[1], "version"))
        return ADFversion (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "open"))
        return ADFopen (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "file"))
        return ADFfile (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "close"))
        return ADFclose (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "node"))
        return ADFnode (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "format"))
        return ADFformat (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "name"))
        return ADFname (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "label"))
        return ADFlabel (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "type"))
        return ADFtype (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "dimensions"))
        return ADFdimensions (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "size"))
        return ADFsize (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "read"))
        return ADFread (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "write"))
        return ADFwrite (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "link"))
        return ADFlink (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "children"))
        return ADFchildren (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "numchild"))
        return ADFnumchild (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "childname"))
        return ADFchildname (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "create"))
        return ADFcreate (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "delete"))
        return ADFdelete (data, interp, argc-1, argv+1);
    if (0 == strcmp (argv[1], "move"))
        return ADFmove (data, interp, argc-1, argv+1);

    Tcl_SetResult (interp, usg_msg, TCL_STATIC);
    return TCL_ERROR;
}

#endif

/*---------- ADFtcl_Init ---------------------------------------
 * Initialize and create the commands
 *--------------------------------------------------------------*/

#if defined(_WIN32) && defined(BUILD_DLL)
__declspec(dllexport)
#endif
int Adftcl_Init(Tcl_Interp *interp)
{
#ifdef SINGLE_COMMAND
    Tcl_CreateCommand (interp, "ADF", (Tcl_CmdProc *)ADFcommand,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
#else
    Tcl_CreateCommand (interp, "ADFversion", (Tcl_CmdProc *)ADFversion,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFfile", (Tcl_CmdProc *)ADFfile,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFopen", (Tcl_CmdProc *)ADFopen,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFclose", (Tcl_CmdProc *)ADFclose,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFnode", (Tcl_CmdProc *)ADFnode,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFformat", (Tcl_CmdProc *)ADFformat,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFname", (Tcl_CmdProc *)ADFname,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFlabel", (Tcl_CmdProc *)ADFlabel,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFtype", (Tcl_CmdProc *)ADFtype,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFdimensions", (Tcl_CmdProc *)ADFdimensions,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFsize", (Tcl_CmdProc *)ADFsize,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFread", (Tcl_CmdProc *)ADFread,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFwrite", (Tcl_CmdProc *)ADFwrite,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFlink", (Tcl_CmdProc *)ADFlink,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFchildren", (Tcl_CmdProc *)ADFchildren,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFnumchild", (Tcl_CmdProc *)ADFnumchild,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFchildname", (Tcl_CmdProc *)ADFchildname,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFcreate", (Tcl_CmdProc *)ADFcreate,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFdelete", (Tcl_CmdProc *)ADFdelete,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "ADFmove", (Tcl_CmdProc *)ADFmove,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
#endif
    Tcl_CreateCommand (interp, "CGNSversion", (Tcl_CmdProc *)CGNSversion,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSfile", (Tcl_CmdProc *)CGNSfile,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    return TCL_OK;
}

