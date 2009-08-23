#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#ifdef _WIN32
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
# undef ERROR
#endif
#include <GL/gl.h>

#include "tk.h"
#include "cgnslib.h"
#include "hash.h"
#include "cgnames.h"

#ifndef CONST
# define CONST
#endif

#ifndef CG_MODE_READ
# define CG_MODE_READ MODE_READ
#endif

/* define this to exclude structured mesh boundaries */
/* imin, imax, jmin, jmax, kmin, kmax */

/*#define NO_MESH_BOUNDARIES*/

/* define this to disable a cutting plane */
/* the cutting plane requires more memory since all
   nodes and elements are saved */

/*#define NO_CUTTING_PLANE*/

/* this is the cosine of the angle between faces to define an interior edge */
/* it's set to 60 degrees - comment to remove interior edges */

#define EDGE_ANGLE 0.5

typedef float Node[3];

typedef struct {
    int id;
    int nodes[2];
} Edge;

typedef struct {
    int id;
    int nnodes;
    int nodes[4];
    float normal[3];
} Face;

#ifndef NO_CUTTING_PLANE

typedef struct {
    int id;
    int nodes[3];
    float ratio;
} CutNode;

typedef struct {
    int nelems;
    int *elems;
    int nedges;
    Edge *edges;
} CutData;

typedef struct {
    float plane[4];
    int nelems;
    int nedges;
    int nnodes;
    Node *nodes;
} CutPlane;

static float cutcolor[4] = {0.8, 0.4, 0.8, 0.5};
static int usecutclr = 0;
static int ignorevis = 0;
static CutPlane cutplane;
static int CutDL = 0;
static int PlaneDL = 0;

#endif

typedef struct {
    char name[33];
    int type;
    int dim;
    int data[10];
    char d_name[33];
    int nedges;
    Edge *edges;
    int nfaces;
    Face *faces;
#ifndef NO_CUTTING_PLANE
    ElementType_t elemtype;
    int nelems;
    int *elems;
    CutData cut;
#endif
    float bbox[3][2];
    int dlist;
    int mode;
    float color[4];
    char errmsg[81];
} Regn;

typedef struct {
    char name[33];
    int nnodes;
    Node *nodes;
    int nregs;
    Regn *regs;
} Zone;

static int cgnsfn = 0;
static int cgnsbase = 0;
static int cgnszone = 0;
static int nbases = 0;
static int nzones = 0;
static Zone *zones;
static int AxisDL = 0;

static char BaseName[33];
static int CellDim, PhyDim;

static Tcl_Interp *global_interp;

enum {
    REG_MESH,
    REG_ELEM,
    REG_1TO1,
    REG_CONN,
    REG_BOCO,
    REG_BNDS
};

/* mapping from elements to faces */

static int facenodes[22][5] = {
    /* tri */
    {3, 0, 1, 2, 0},
    /* quad */
    {4, 0, 1, 2, 3},
    /* tet */
    {3, 0, 2, 1, 0},
    {3, 0, 1, 3, 0},
    {3, 1, 2, 3, 0},
    {3, 2, 0, 3, 0},
    /* pyramid */
    {4, 0, 3, 2, 1},
    {3, 0, 1, 4, 0},
    {3, 1, 2, 4, 0},
    {3, 2, 3, 4, 0},
    {3, 3, 0, 4, 0},
    /* wedge */
    {4, 0, 1, 4, 3},
    {4, 1, 2, 5, 4},
    {4, 2, 0, 3, 5},
    {3, 0, 2, 1, 0},
    {3, 3, 4, 5, 0},
    /* hex */
    {4, 0, 3, 2, 1},
    {4, 0, 1, 5, 4},
    {4, 1, 2, 6, 5},
    {4, 2, 3, 7, 6},
    {4, 0, 4, 7, 3},
    {4, 4, 5, 6, 7}
};

/*===================================================================
 *           local routines
 *===================================================================*/

/*-------------------------------------------------------------------*/

static void FATAL (
#ifdef PROTOTYPE
    char *errmsg)
#else
    errmsg)
char *errmsg;
#endif
{
    char cmd[129];

    sprintf (cmd, "error_exit {%s}", errmsg);
    Tcl_Eval (global_interp, cmd);
    exit (1);
}

/*-------------------------------------------------------------------*/

static void *MALLOC (
#ifdef PROTOTYPE
    char *funcname, int bytes)
#else
    funcname, bytes)
char *funcname;
int bytes;
#endif
{
    void *data = calloc (bytes, 1);
    if (NULL == data) {
        char msg[128];
        if (funcname != NULL)
            sprintf (msg, "%s:malloc failed for %d bytes", funcname, bytes);
        else
            sprintf (msg, "malloc failed for %d bytes", bytes);
        FATAL (msg);
    }
    return data;
}

/*-------------------------------------------------------------------*/

static void *REALLOC (
#ifdef PROTOTYPE
    char *funcname, int bytes, void *old_data)
#else
    funcname, bytes, old_data)
char *funcname;
int bytes;
void *old_data;
#endif
{
    void *new_data;

    if (NULL == old_data)
        return MALLOC (funcname, bytes);
    new_data = realloc (old_data, bytes);
    if (NULL == new_data) {
        char msg[128];
        if (funcname != NULL)
            sprintf (msg, "%s:realloc failed for %d bytes", funcname, bytes);
        else
            sprintf (msg, "realloc failed for %d bytes", bytes);
        FATAL (msg);
    }
    return new_data;
}

/*-------------------------------------------------------------------*/

static void zone_message (
#ifdef PROTOTYPE
    char *msg, char *name)
#else
    msg, name)
char *msg, *name;
#endif
{
    char cmd[129];

    if (name != NULL)
        sprintf (cmd, "display_message {Zone %d : %s %s...}",
            cgnszone, msg, name);
    else
        sprintf (cmd, "display_message {Zone %d : %s...}", cgnszone, msg);
    Tcl_Eval (global_interp, cmd);
}

/*-------------------------------------------------------------------*/

static void free_all (
#ifdef PROTOTYPE
    void
#endif
){
    int nz, nr;

    if (!nzones) return;
    for (nz = 0; nz < nzones; nz++) {
        for (nr = 0; nr < zones[nz].nregs; nr++) {
            if (zones[nz].regs[nr].dlist)
                glDeleteLists (zones[nz].regs[nr].dlist, 1);
            if (zones[nz].regs[nr].nedges)
                free (zones[nz].regs[nr].edges);
            if (zones[nz].regs[nr].nfaces)
                free (zones[nz].regs[nr].faces);
#ifndef NO_CUTTING_PLANE
            if (zones[nz].regs[nr].nelems)
                free (zones[nz].regs[nr].elems);
            if (zones[nz].regs[nr].cut.nelems)
                free (zones[nz].regs[nr].cut.elems);
            if (zones[nz].regs[nr].cut.nedges)
                free (zones[nz].regs[nr].cut.edges);
#endif
        }
        if (zones[nz].nregs) free (zones[nz].regs);
        if (zones[nz].nnodes) free(zones[nz].nodes);
    }
    free (zones);
    nzones = 0;
#ifndef NO_CUTTING_PLANE
    if (cutplane.nnodes)
        free (cutplane.nodes);
    cutplane.nelems = 0;
    cutplane.nedges = 0;
    cutplane.nnodes = 0;
#endif
}

/*-------------------------------------------------------------------*/

static int compare_ints (
#ifdef PROTOTYPE
    const void *v1, const void *v2)
#else
    v1, v2)
void *v1, *v2;
#endif
{
    return (*((int *)v1) - *((int *)v2));
}

/*-------------------------------------------------------------------*/

static int find_int (
#ifdef PROTOTYPE
    int value, int nlist, int *list)
#else
    value, nlist, list)
int value, nlist, *list;
#endif
{
    int lo = 0, hi = nlist - 1, mid;

    if (!nlist || value < list[0]) return 0;
    if (value == list[0]) return 1;
    if (!hi || value > list[hi]) return 0;
    if (value == list[hi]) return 1;

    while (lo <= hi) {
        mid = (lo + hi) >> 1;
        if (value == list[mid]) return 1;
        if (value < list[mid])
            hi = mid - 1;
        else
            lo = mid + 1;
    }
    return 0;
}

/*====================================================================
 * structured grid regions
 *====================================================================*/

#define NODE_INDEX(I,J,K) ((I)+dim[0]*((J)+dim[1]*(K)))

/*-------------------------------------------------------------------*/

static int structured_range (
#ifdef PROTOTYPE
    Regn *reg, int *dim, int *ptrng, GridLocation_t location)
#else
    reg, dim, ptrng, location)
Regn *reg;
int *dim, *ptrng;
GridLocation_t location;
#endif
{
    int n, i, j, k, nfaces, rng[3][2];
    Face *f;
    static char *funcname = "structured_range";

    if (location != Vertex && location != IFaceCenter &&
        location != JFaceCenter && location != KFaceCenter) {
        i = j = 0;
        for (n = 0; n < 3; n++) {
            if (ptrng[n] == ptrng[n+3] &&
               (ptrng[n] == 1 || ptrng[n] == dim[n])) {
                if (ptrng[n] == 1)
                    i++;
                else if (j) {
                    j = 4;
                    break;
                }
                else
                    j = n + 1;
            }
        }
        if (!j && i == 1) {
            for (n = 0; n < 3; n++) {
                if (ptrng[n] == ptrng[n+3] && ptrng[n] == 1) {
                    j = n + 1;
                    break;
                }
            }
        }
        if (j == 1)
            location = IFaceCenter;
        else if (j == 2)
            location = JFaceCenter;
        else if (j == 3)
            location = KFaceCenter;
        else {
            strcpy (reg->errmsg,
                "unable to determine boundary - use [IJK]FaceCenter");
            return 0;
        }
    }

    nfaces = 1;
    if (location == Vertex) {
        for (n = 0, i = 0; i < 3; i++) {
            if (ptrng[i] < 1 || ptrng[i] > dim[i]) return 0;
            if (ptrng[i] == ptrng[i+3]) {
                if (n || (ptrng[i] != 1 && ptrng[i] != dim[i]))
                    return 0;
                n = i + 1;
                rng[i][0] = rng[i][1] = ptrng[i] - 1;
            } else {
                if (ptrng[i] < ptrng[i+3]) {
                    rng[i][0] = ptrng[i] - 1;
                    rng[i][1] = ptrng[i+3] - 1;
                }
                else {
                    rng[i][0] = ptrng[i+3] - 1;
                    rng[i][1] = ptrng[i] - 1;
                }
                nfaces *= (rng[i][1] - rng[i][0]);
            }
        }
    }
    else {
        if (location == IFaceCenter)
            n = 0;
        else if (location == JFaceCenter)
            n = 1;
        else
            n = 2;
        for (i = 0; i < 3; i++) {
            if (i == n) {
                if (ptrng[i] != ptrng[i+3] ||
                   (ptrng[i] != 1 && ptrng[i] != dim[i])) return 0;
                rng[i][0] = rng[i][1] = ptrng[i] - 1;
            }
            else {
                if (ptrng[i] < ptrng[i+3]) {
                    rng[i][0] = ptrng[i] - 1;
                    rng[i][1] = ptrng[i+3];
                }
                else {
                    rng[i][0] = ptrng[i+3] - 1;
                    rng[i][1] = ptrng[i];
                }
                if (rng[i][0] < 0 || rng[i][1] >= dim[i]) return 0;
                nfaces *= (rng[i][1] - rng[i][0]);
            }
        }
        n++;
    }
    if (!nfaces || !n) {
        strcpy (reg->errmsg, "couldn't find any exterior faces");
        return 0;
    }

    reg->nfaces = nfaces;
    reg->faces = f = (Face *) MALLOC (funcname, nfaces * sizeof(Face));

    if (n == 1) {
        if ((i = rng[0][0]) == 0) {
            for (k = rng[2][0]; k < rng[2][1]; k++) {
                for (j = rng[1][0]; j < rng[1][1]; j++) {
                    f->nnodes = 4;
                    f->nodes[0] = NODE_INDEX (i, j, k);
                    f->nodes[1] = NODE_INDEX (i, j, k+1);
                    f->nodes[2] = NODE_INDEX (i, j+1, k+1);
                    f->nodes[3] = NODE_INDEX (i, j+1, k);
                    f++;
                }
            }
        }
        else {
            for (k = rng[2][0]; k < rng[2][1]; k++) {
                for (j = rng[1][0]; j < rng[1][1]; j++) {
                    f->nnodes = 4;
                    f->nodes[0] = NODE_INDEX (i, j, k);
                    f->nodes[1] = NODE_INDEX (i, j+1, k);
                    f->nodes[2] = NODE_INDEX (i, j+1, k+1);
                    f->nodes[3] = NODE_INDEX (i, j, k+1);
                    f++;
                }
            }
        }
    }
    else if (n == 2) {
        if ((j = rng[1][0]) == 0) {
            for (k = rng[2][0]; k < rng[2][1]; k++) {
                for (i = rng[0][0]; i < rng[0][1]; i++) {
                    f->nnodes = 4;
                    f->nodes[0] = NODE_INDEX (i, j, k);
                    f->nodes[1] = NODE_INDEX (i+1, j, k);
                    f->nodes[2] = NODE_INDEX (i+1, j, k+1);
                    f->nodes[3] = NODE_INDEX (i, j, k+1);
                    f++;
                }
            }
        }
        else {
            for (k = rng[2][0]; k < rng[2][1]; k++) {
                for (i = rng[0][0]; i < rng[0][1]; i++) {
                    f->nnodes = 4;
                    f->nodes[0] = NODE_INDEX (i, j, k);
                    f->nodes[1] = NODE_INDEX (i, j, k+1);
                    f->nodes[2] = NODE_INDEX (i+1, j, k+1);
                    f->nodes[3] = NODE_INDEX (i+1, j, k);
                    f++;
                }
            }
        }
    }
    else {
        if ((k = rng[2][0]) == 0) {
            for (j = rng[1][0]; j < rng[1][1]; j++) {
                for (i = rng[0][0]; i < rng[0][1]; i++) {
                    f->nnodes = 4;
                    f->nodes[0] = NODE_INDEX (i, j, k);
                    f->nodes[1] = NODE_INDEX (i, j+1, k);
                    f->nodes[2] = NODE_INDEX (i+1, j+1, k);
                    f->nodes[3] = NODE_INDEX (i+1, j, k);
                    f++;
                }
            }
        }
        else {
            for (j = rng[1][0]; j < rng[1][1]; j++) {
                for (i = rng[0][0]; i < rng[0][1]; i++) {
                    f->nnodes = 4;
                    f->nodes[0] = NODE_INDEX (i, j, k);
                    f->nodes[1] = NODE_INDEX (i+1, j, k);
                    f->nodes[2] = NODE_INDEX (i+1, j+1, k);
                    f->nodes[3] = NODE_INDEX (i, j+1, k);
                    f++;
                }
            }
        }
    }
    return nfaces;
}

/*-------------------------------------------------------------------*/

static int structured_list (
#ifdef PROTOTYPE
    Regn *mesh, Regn *reg, int *dim, int npts, int *pts,
    GridLocation_t location)
#else
    mesh, reg, dim, npts, pts, location)
Regn *mesh, *reg;
int *dim, npts, *pts;
GridLocation_t location;
#endif
{
    int n, nn, nf, nfaces = 0;
    Face **faces, *f;
    static char *funcname = "structured_list";

    if (location != Vertex && location != IFaceCenter &&
        location != JFaceCenter && location != KFaceCenter) {
        int rng[3][2];
        for (n = 0; n < 3; n++)
            rng[n][0] = rng[n][1] = pts[n];
        for (nf = 1; nf < npts; nf++) {
            nn = nf * 3;
            for (n = 0; n < 3; n++) {
                if (rng[n][0] > pts[nn+n]) rng[n][0] = pts[nn+n];
                if (rng[n][1] < pts[nn+n]) rng[n][1] = pts[nn+n];
            }
        }
        nf = nn = 0;
        for (n = 0; n < 3; n++) {
            if (rng[n][0] == rng[n][1] &&
                (rng[n][0] == 1 || rng[n][0] == dim[n])) {
                if (rng[n][0] == 1)
                    nf++;
                else if (nn) {
                    nn = 4;
                    break;
                }
                else
                    nn = n + 1;
            }
        }
        if (!nn && nf == 1) {
            for (n = 0; n < 3; n++) {
                if (rng[n][0] == rng[n][1] && rng[n][0] == 1) {
                    nn = n + 1;
                    break;
                }
            }
        }
        if (nn == 1)
            location = IFaceCenter;
        else if (nn == 2)
            location = JFaceCenter;
        else if (nn == 3)
            location = KFaceCenter;
        else {
            strcpy (reg->errmsg,
                "unable to determine boundary - use [IJK]FaceCenter");
            return 0;
        }
    }

    faces = (Face **) MALLOC (funcname, npts * sizeof(Face *));

    if (location == Vertex) {
        for (n = 0; n < npts; n++) {
            nn = 3 * n;
            pts[n] = NODE_INDEX (pts[nn]-1, pts[nn+1]-1, pts[nn+2]-1);
        }
        for (n = 1; n < npts; n++) {
            if (pts[n] < pts[n-1]) {
                qsort (pts, npts, sizeof(int), compare_ints);
                break;
            }
        }

        for (f = mesh->faces, nf = 0; nf < mesh->nfaces; nf++, f++) {
            for (nn = 0; nn < f->nnodes; nn++) {
                if (!find_int (f->nodes[nn], npts, pts))
                    break;
            }
            if (nn == f->nnodes) {
                if (nfaces == npts) {
                    npts += 100;
                    faces = (Face **) REALLOC (funcname,
                        npts * sizeof(Face *), faces);
                }
                faces[nfaces++] = f;
            }
        }
    }

    else if (location == IFaceCenter) {
        for (n = 0; n < npts; n++) {
            nn = 3 * n;
            if ((pts[nn] == 1 || pts[nn] == dim[0]) &&
                pts[nn+1] > 0 && pts[nn+1] < dim[1] &&
                pts[nn+2] > 0 && pts[nn+2] < dim[2]) {
                nf = pts[nn+1]-1 + (pts[nn+2]-1) * (dim[1]-1);
                if (pts[nn] == dim[0])
                    nf += (dim[1]-1) * (dim[2]-1);
                faces[nfaces++] = &(mesh->faces[nf]);
            }
        }
    }

    else if (location == JFaceCenter) {
        int noff = 2 * (dim[1]-1) * (dim[2]-1);
        for (n = 0; n < npts; n++) {
            nn = 3 * n;
            if ((pts[nn+1] == 1 || pts[nn+1] == dim[1]) &&
                pts[nn] > 0 && pts[nn] < dim[0] &&
                pts[nn+2] > 0 && pts[nn+2] < dim[2]) {
                nf = noff + pts[nn]-1 + (pts[nn+2]-1) * (dim[0]-1);
                if (pts[nn+1] == dim[1])
                    nf += (dim[0]-1) * (dim[2]-1);
                faces[nfaces++] = &(mesh->faces[nf]);
            }
        }
    }

    else  {
        int noff = 2 * ((dim[1]-1) * (dim[2]-1) + (dim[0]-1) * (dim[2]-1));
        for (n = 0; n < npts; n++) {
            nn = 3 * n;
            if ((pts[nn+2] == 1 || pts[nn+2] == dim[2]) &&
                pts[nn] > 0 && pts[nn] < dim[0] &&
                pts[nn+1] > 0 && pts[nn+1] < dim[1]) {
                nf = noff + pts[nn]-1 + (pts[nn+1]-1) * (dim[0]-1);
                if (pts[nn+2] == dim[2])
                    nf += (dim[0]-1) * (dim[1]-1);
                faces[nfaces++] = &(mesh->faces[nf]);
            }
        }
    }

    if (nfaces) {
        reg->nfaces = nfaces;
        reg->faces = (Face *) MALLOC (funcname, nfaces * sizeof(Face));
        for (nf = 0; nf < nfaces; nf++) {
            reg->faces[nf].nnodes = faces[nf]->nnodes;
            for (nn = 0; nn < faces[nf]->nnodes; nn++)
                reg->faces[nf].nodes[nn] = faces[nf]->nodes[nn];
        }
    }
    else
        strcpy (reg->errmsg, "couldn't find any exterior faces");
    free (faces);
    return nfaces;
}

/*-------------------------------------------------------------------*/

static int structured_zone (
#ifdef PROTOTYPE
    Tcl_Interp *interp, int *dim)
#else
    interp, dim)
Tcl_Interp *interp;
int *dim;
#endif
{
    char name[33], d_name[33];
    int ni, nj, nk, nr, nf, n, i, j, k;
    int nints, nconns, nbocos, *pts, *d_pts;
    int range[6], d_range[6], transform[3];
    GridLocation_t location;
    GridConnectivityType_t type;
    PointSetType_t ptype, d_ptype;
    ZoneType_t d_ztype;
    DataType_t datatype;
    BCType_t bctype;
    Face *f;
    Zone *z = &zones[cgnszone-1];
    static char *funcname = "structured_zone";

    zone_message ("finding exterior faces", NULL);
    if (CellDim == 3) {
        if (cg_n1to1 (cgnsfn, cgnsbase, cgnszone, &nints) ||
            cg_nconns (cgnsfn, cgnsbase, cgnszone, &nconns) ||
            cg_nbocos (cgnsfn, cgnsbase, cgnszone, &nbocos)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
#ifndef NO_MESH_BOUNDARIES
        z->nregs = nints + nconns + nbocos + 7;
#else
        z->nregs = nints + nconns + nbocos + 1;
#endif
    }
    else {
        nints = nconns = nbocos = 0;
        z->nregs = 1;
    }
    z->regs = (Regn *) MALLOC (funcname, z->nregs * sizeof(Regn));
    ni = dim[0] - 1;
    nj = dim[1] - 1;
    nk = dim[2] - 1;
    nr = 1;

    /* volume mesh boundaries */

    strcpy (z->regs[0].name, "<mesh>");
    z->regs[0].type = REG_MESH;
    for (n = 0; n < 3; n++)
        z->regs[0].data[n] = dim[n];

    if (CellDim == 2) {
        z->regs[0].dim = 2;
        z->regs[0].nfaces = ni * nj;
        z->regs[0].faces = f =
            (Face *) MALLOC (funcname, z->regs[0].nfaces * sizeof(Face));
        for (k = 0, j = 0; j < nj; j++) {
            for (i = 0; i < ni; i++) {
                f->nnodes = 4;
                f->nodes[0] = NODE_INDEX (i, j, k);
                f->nodes[1] = NODE_INDEX (i, j+1, k);
                f->nodes[2] = NODE_INDEX (i+1, j+1, k);
                f->nodes[3] = NODE_INDEX (i+1, j, k);
                f++;
            }
        }
#ifndef NO_CUTTING_PLANE
        nf = z->regs[0].nfaces;
        f = z->regs[0].faces;
        z->regs[0].elemtype = QUAD_4;
        z->regs[0].nelems = nf;
        z->regs[0].elems = (int *) MALLOC (funcname, 4 * nf * sizeof(int));
        for (n = 0, j = 0; j < nf; j++, f++) {
            for (i = 0; i < 4; i++)
                z->regs[0].elems[n++] = f->nodes[i];
        }
#endif
        return 0;
    }

    z->regs[0].dim = 3;

#ifndef NO_CUTTING_PLANE
    z->regs[0].elemtype = HEXA_8;
    z->regs[0].nelems = ni * nj * nk;
    z->regs[0].elems = pts =
        (int *) MALLOC (funcname, 8 * z->regs[0].nelems * sizeof(int));

    for (n = 0, k = 0; k < nk; k++) {
        for (j = 0; j < nj; j++) {
            for (i = 0; i < ni; i++) {
                pts[n++] = NODE_INDEX (i,   j,   k);
                pts[n++] = NODE_INDEX (i+1, j,   k);
                pts[n++] = NODE_INDEX (i+1, j+1, k);
                pts[n++] = NODE_INDEX (i,   j+1, k);
                pts[n++] = NODE_INDEX (i,   j,   k+1);
                pts[n++] = NODE_INDEX (i+1, j,   k+1);
                pts[n++] = NODE_INDEX (i+1, j+1, k+1);
                pts[n++] = NODE_INDEX (i,   j+1, k+1);
            }
        }
    }
#endif

    z->regs[0].nfaces = 2 * (nj * nk + ni * nk + ni * nj);
    z->regs[0].faces = f =
        (Face *) MALLOC (funcname, z->regs[0].nfaces * sizeof(Face));

    for (i = 0, k = 0; k < nk; k++) {
        for (j = 0; j < nj; j++) {
            f->nnodes = 4;
            f->nodes[0] = NODE_INDEX (i, j, k);
            f->nodes[1] = NODE_INDEX (i, j, k+1);
            f->nodes[2] = NODE_INDEX (i, j+1, k+1);
            f->nodes[3] = NODE_INDEX (i, j+1, k);
            f++;
        }
    }
    for (i = ni, k = 0; k < nk; k++) {
        for (j = 0; j < nj; j++) {
            f->nnodes = 4;
            f->nodes[0] = NODE_INDEX (i, j, k);
            f->nodes[1] = NODE_INDEX (i, j+1, k);
            f->nodes[2] = NODE_INDEX (i, j+1, k+1);
            f->nodes[3] = NODE_INDEX (i, j, k+1);
            f++;
        }
    }
    for (j = 0, k = 0; k < nk; k++) {
        for (i = 0; i < ni; i++) {
            f->nnodes = 4;
            f->nodes[0] = NODE_INDEX (i, j, k);
            f->nodes[1] = NODE_INDEX (i+1, j, k);
            f->nodes[2] = NODE_INDEX (i+1, j, k+1);
            f->nodes[3] = NODE_INDEX (i, j, k+1);
            f++;
        }
    }
    for (j = nj, k = 0; k < nk; k++) {
        for (i = 0; i < ni; i++) {
            f->nnodes = 4;
            f->nodes[0] = NODE_INDEX (i, j, k);
            f->nodes[1] = NODE_INDEX (i, j, k+1);
            f->nodes[2] = NODE_INDEX (i+1, j, k+1);
            f->nodes[3] = NODE_INDEX (i+1, j, k);
            f++;
        }
    }
    for (k = 0, j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
            f->nnodes = 4;
            f->nodes[0] = NODE_INDEX (i, j, k);
            f->nodes[1] = NODE_INDEX (i, j+1, k);
            f->nodes[2] = NODE_INDEX (i+1, j+1, k);
            f->nodes[3] = NODE_INDEX (i+1, j, k);
            f++;
        }
    }
    for (k = nk, j = 0; j < nj; j++) {
        for (i = 0; i < ni; i++) {
            f->nnodes = 4;
            f->nodes[0] = NODE_INDEX (i, j, k);
            f->nodes[1] = NODE_INDEX (i+1, j, k);
            f->nodes[2] = NODE_INDEX (i+1, j+1, k);
            f->nodes[3] = NODE_INDEX (i, j+1, k);
            f++;
        }
    }

#ifndef NO_MESH_BOUNDARIES
    for (n = 0; n < 3; n++) {
        range[n] = 1;
        range[n+3] = dim[n];
    }

    f = z->regs->faces;
    nf = nj * nk;
    strcpy (z->regs[nr].name, "<imin>");
    z->regs[nr].dim = 2;
    z->regs[nr].type = REG_BNDS;
    for (n = 0; n < 6; n++)
        z->regs[nr].data[n] = range[n];
    z->regs[nr].data[3] = 1;
    z->regs[nr].nfaces = nf;
    z->regs[nr].faces = (Face *) MALLOC (funcname, nf * sizeof(Face));
    for (n = 0; n < nf; n++, f++) {
        z->regs[nr].faces[n].nnodes = 4;
        for (i = 0; i < 4; i++)
            z->regs[nr].faces[n].nodes[i] = f->nodes[i];
    }
    nr++;

    strcpy (z->regs[nr].name, "<imax>");
    z->regs[nr].dim = 2;
    z->regs[nr].type = REG_BNDS;
    for (n = 0; n < 6; n++)
        z->regs[nr].data[n] = range[n];
    z->regs[nr].data[0] = dim[0];
    z->regs[nr].nfaces = nf;
    z->regs[nr].faces = (Face *) MALLOC (funcname, nf * sizeof(Face));
    for (n = 0; n < nf; n++, f++) {
        z->regs[nr].faces[n].nnodes = 4;
        for (i = 0; i < 4; i++)
            z->regs[nr].faces[n].nodes[i] = f->nodes[i];
    }
    nr++;

    nf = ni * nk;
    strcpy (z->regs[nr].name, "<jmin>");
    z->regs[nr].dim = 2;
    z->regs[nr].type = REG_BNDS;
    for (n = 0; n < 6; n++)
        z->regs[nr].data[n] = range[n];
    z->regs[nr].data[4] = 1;
    z->regs[nr].nfaces = nf;
    z->regs[nr].faces = (Face *) MALLOC (funcname, nf * sizeof(Face));
    for (n = 0; n < nf; n++, f++) {
        z->regs[nr].faces[n].nnodes = 4;
        for (i = 0; i < 4; i++)
            z->regs[nr].faces[n].nodes[i] = f->nodes[i];
    }
    nr++;

    strcpy (z->regs[nr].name, "<jmax>");
    z->regs[nr].dim = 2;
    z->regs[nr].type = REG_BNDS;
    for (n = 0; n < 6; n++)
        z->regs[nr].data[n] = range[n];
    z->regs[nr].data[1] = dim[1];
    z->regs[nr].nfaces = nf;
    z->regs[nr].faces = (Face *) MALLOC (funcname, nf * sizeof(Face));
    for (n = 0; n < nf; n++, f++) {
        z->regs[nr].faces[n].nnodes = 4;
        for (i = 0; i < 4; i++)
            z->regs[nr].faces[n].nodes[i] = f->nodes[i];
    }
    nr++;

    nf = ni * nj;
    strcpy (z->regs[nr].name, "<kmin>");
    z->regs[nr].dim = 2;
    z->regs[nr].type = REG_BNDS;
    for (n = 0; n < 6; n++)
        z->regs[nr].data[n] = range[n];
    z->regs[nr].data[5] = 1;
    z->regs[nr].nfaces = nf;
    z->regs[nr].faces = (Face *) MALLOC (funcname, nf * sizeof(Face));
    for (n = 0; n < nf; n++, f++) {
        z->regs[nr].faces[n].nnodes = 4;
        for (i = 0; i < 4; i++)
            z->regs[nr].faces[n].nodes[i] = f->nodes[i];
    }
    nr++;

    strcpy (z->regs[nr].name, "<kmax>");
    z->regs[nr].dim = 2;
    z->regs[nr].type = REG_BNDS;
    for (n = 0; n < 6; n++)
        z->regs[nr].data[n] = range[n];
    z->regs[nr].data[2] = dim[2];
    z->regs[nr].nfaces = nf;
    z->regs[nr].faces = (Face *) MALLOC (funcname, nf * sizeof(Face));
    for (n = 0; n < nf; n++, f++) {
        z->regs[nr].faces[n].nnodes = 4;
        for (i = 0; i < 4; i++)
            z->regs[nr].faces[n].nodes[i] = f->nodes[i];
    }
    nr++;
#endif

    /* 1 to 1 interfaces */

    for (n = 1; n <= nints; n++) {
        if (cg_1to1_read (cgnsfn, cgnsbase, cgnszone, n,
            name, d_name, range, d_range, transform)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        strcpy (z->regs[nr].name, name);
        z->regs[nr].type = REG_1TO1;
        z->regs[nr].data[0] = 6;
        for (i = 0; i < 6; i++)
            z->regs[nr].data[i+1] = range[i];
        strcpy (z->regs[nr].d_name, d_name);
        if (structured_range (&z->regs[nr], dim, range, Vertex))
            z->regs[nr].dim = 2;
        nr++;
    }

    /* general connectivities */

    for (n = 1; n <= nconns; n++) {
        if (cg_conn_info (cgnsfn, cgnsbase, cgnszone, n, name,
            &location, &type, &ptype, &i, d_name, &d_ztype,
            &d_ptype, &datatype, &j)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        pts = (int *) MALLOC (funcname, 3 * i * sizeof(int));
        d_pts = (int *) MALLOC (funcname, 3 * j * sizeof(int));
        k = cg_conn_read (cgnsfn, cgnsbase, cgnszone, n, pts, Integer, d_pts);
        free (d_pts);
        if (k) {
            free (pts);
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        strcpy (z->regs[nr].name, name);
        z->regs[nr].type = REG_CONN;
        z->regs[nr].data[0] = type;
        z->regs[nr].data[1] = location;
        z->regs[nr].data[2] = ptype;
        z->regs[nr].data[3] = i;
        if (ptype == PointRange) {
            z->regs[nr].data[3] = 6;
            for (j = 0; j < 6; j++)
                z->regs[nr].data[j+4] = pts[j];
        }
        strcpy (z->regs[nr].d_name, d_name);

        if ((type == Abutting1to1 || type == Abutting) &&
            (ptype == PointList || ptype == PointRange)) {
            if (ptype == PointRange)
                k = structured_range (&z->regs[nr], dim, pts, location);
            else
                k = structured_list (z->regs, &z->regs[nr], dim, i, pts,
                    location);
            if (k) z->regs[nr].dim = 2;
        }
        else
            strcpy (z->regs[nr].errmsg,
                "not Abutting or Abutting1to1 with PointList or PointRange");
        free (pts);
        nr++;
    }

    /* boundary conditions */

    for (n = 1; n <= nbocos; n++) {
        if (cg_boco_info (cgnsfn, cgnsbase, cgnszone, n, name,
            &bctype, &ptype, &i, transform, &j, &datatype, &k)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        if (cg_goto (cgnsfn, cgnsbase, "Zone_t", cgnszone,
           "ZoneBC_t", 1, "BC_t", n, "end")) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        if (cg_gridlocation_read (&location))
            location = Vertex;
        pts = (int *) MALLOC (funcname, 3 * i * sizeof(int));
        d_pts = NULL;
        if (j) {
            k = datatype == RealSingle ? sizeof(float) : sizeof(double);
            d_pts = (int *) MALLOC (funcname, j * k);
        }
        k = cg_boco_read (cgnsfn, cgnsbase, cgnszone, n, pts, d_pts);
        if (NULL != d_pts) free (d_pts);
        if (k) {
            free (pts);
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        strcpy (z->regs[nr].name, name);
        z->regs[nr].type = REG_BOCO;
        z->regs[nr].data[0] = bctype;
        z->regs[nr].data[1] = location;
        z->regs[nr].data[2] = ptype;
        z->regs[nr].data[3] = i;
        if (ptype == PointRange || ptype == ElementRange) {
            z->regs[nr].data[3] = 6;
            for (j = 0; j < 6; j++)
                z->regs[nr].data[j+4] = pts[j];
        }

        if (ptype == PointRange || ptype == ElementRange)
            k = structured_range (&z->regs[nr], dim, pts, location);
        else if (ptype == PointList || ptype == ElementList)
            k = structured_list (z->regs, &z->regs[nr], dim, i, pts,
                location);
        else {
            k = 0;
            strcpy (z->regs[nr].errmsg, "invalid point set type");
        }
        if (k) z->regs[nr].dim = 2;
        free (pts);
        nr++;
    }

    z->nregs = nr;

#ifndef NO_CUTTING_PLANE
    for (nr = 0; nr < z->nregs; nr++) {
        if (z->regs[nr].dim == 2 && z->regs[nr].nfaces) {
            nf = z->regs[nr].nfaces;
            f = z->regs[nr].faces;
            z->regs[nr].elemtype = QUAD_4;
            z->regs[nr].nelems = nf;
            z->regs[nr].elems = (int *) MALLOC (funcname, 4 * nf * sizeof(int));
            for (n = 0, j = 0; j < nf; j++, f++) {
                for (i = 0; i < 4; i++)
                    z->regs[nr].elems[n++] = f->nodes[i];
            }
        }
    }
#endif

    return 0;
}

/*====================================================================
 * unstructured grid regions
 *====================================================================*/

static HASH *facehash;

/*-------------------------------------------------------------------*/

static int compare_faces (
#ifdef PROTOTYPE
    void *v1, void *v2)
#else
    v1, v2)
void *v1, *v2;
#endif
{
    Face *f1 = (Face *)v1;
    Face *f2 = (Face *)v2;
    int i, id, k, nn, n1[4], n2[4];

    if (f1->nnodes != f2->nnodes)
        return (f1->nnodes - f2->nnodes);

    for (i = 0; i < f1->nnodes; i++) {
        id = f1->nodes[i];
        for (k = 0; k < i; k++) {
            if (n1[k] > id) {
                nn = n1[k];
                n1[k] = id;
                id = nn;
            }
        }
        n1[i] = id;
    }
    for (i = 0; i < f2->nnodes; i++) {
        id = f2->nodes[i];
        for (k = 0; k < i; k++) {
            if (n2[k] > id) {
                nn = n2[k];
                n2[k] = id;
                id = nn;
            }
        }
        n2[i] = id;
    }

    for (i = 0; i < f1->nnodes; i++) {
        if (n1[i] != n2[i])
            return (n1[i] - n2[i]);
    }
    return (0);
}

/*-------------------------------------------------------------------*/

static unsigned hash_face (
#ifdef PROTOTYPE
    void *v)
#else
    v)
void *v;
#endif
{
    Face *f = (Face *)v;
    int n;
    unsigned hash = 0;

    for (n = 0; n < f->nnodes; n++)
        hash += (unsigned)f->nodes[n];
    return (hash);
}

/*-------------------------------------------------------------------*/

static int get_faces (
#ifdef PROTOTYPE
    void *vf, void *vr)
#else
    vf, vr)
void *vf, *vr;
#endif
{
    Face *f = (Face *)vf;
    Regn *r = (Regn *)vr;
    int n;

    r->faces[r->nfaces].id = f->id;
    r->faces[r->nfaces].nnodes = f->nnodes;
    for (n = 0; n < f->nnodes; n++)
        r->faces[r->nfaces].nodes[n] = f->nodes[n];
    free (f);
    (r->nfaces)++;
    return 1;
}

/*-------------------------------------------------------------------*/

static void exterior_faces (
#ifdef PROTOTYPE
    int nelems, ElementType_t elemtype, int *conn, int rind[2])
#else
    nelems, elemtype, conn, rind)
int nelems, *conn, rind[2];
ElementType_t elemtype;
#endif
{
    int i, j, ne, nn, nf, ip, type = elemtype;
    Face face, *pf;

    for (nn = 0, ne = 0; ne < nelems - rind[1]; ne++) {
        if (elemtype == MIXED) type = conn[nn++];
        switch (type) {
            case TETRA_4:
            case TETRA_10:
                ip = 2;
                nf = 4;
                break;
            case PYRA_5:
            case PYRA_14:
                ip = 6;
                nf = 5;
                break;
            case PENTA_6:
            case PENTA_15:
            case PENTA_18:
                ip = 11;
                nf = 5;
                break;
            case HEXA_8:
            case HEXA_20:
            case HEXA_27:
                ip = 16;
                nf = 6;
                break;
            default:
                if (type < NODE || type > HEXA_27)
                    FATAL ("unknown element type int exterior_faces");
                ip = 0;
                nf = 0;
                break;
        }
        if (ne >= rind[0]) {
            for (j = 0; j < nf; j++) {
                face.nnodes = facenodes[ip+j][0];
                for (i = 0; i < face.nnodes; i++)
                    face.nodes[i] = conn[nn + facenodes[ip+j][i+1]] - 1;
                if (NULL == (pf = (Face *) HashFind (facehash, &face))) {
                    pf = (Face *) MALLOC ("exterior_faces", sizeof(Face));
                    pf->id = 0;
                    pf->nnodes = face.nnodes;
                    for (i = 0; i < face.nnodes; i++)
                        pf->nodes[i] = face.nodes[i];
                    (void) HashAdd (facehash, pf);
                }
                else {
                    HashDelete (facehash, pf);
                    free (pf);
                }
            }
        }
        cg_npe ((ElementType_t)type, &j);
        nn += j;
    }
}

/*-------------------------------------------------------------------*/

static void element_faces (
#ifdef PROTOTYPE
    int istart, int nelems, ElementType_t elemtype, int *conn, int rind[2])
#else
    istart, nelems, elemtype, conn, rind)
int istart, nelems, *conn, rind[2];
ElementType_t elemtype;
#endif
{
    int i, ne, nn, ip, type = elemtype;
    Face face, *pf;

    for (nn = 0, ne = 0; ne < nelems - rind[1]; ne++) {
        if (elemtype == MIXED) type = conn[nn++];
        switch (type) {
            case TRI_3:
            case TRI_6:
                ip = 3;
                break;
            case QUAD_4:
            case QUAD_8:
            case QUAD_9:
                ip = 4;
                break;
            default:
                if (type < NODE || type > HEXA_27)
                    FATAL ("unknown element type int element_faces");
                ip = 0;
                break;
        }
        if (ip && ne >= rind[0]) {
            face.nnodes = ip;
            for (i = 0; i < ip; i++)
                face.nodes[i] = conn[nn+i] - 1;

            if (NULL == (pf = (Face *) HashFind (facehash, &face))) {
                pf = (Face *) MALLOC ("element_faces", sizeof(Face));
                pf->id = istart + ne;
                pf->nnodes = face.nnodes;
                for (i = 0; i < face.nnodes; i++)
                    pf->nodes[i] = face.nodes[i];
                (void) HashAdd (facehash, pf);
            }
        }
        cg_npe ((ElementType_t)type, &i);
        nn += i;
    }
}

/*-------------------------------------------------------------------*/

static int unstructured_region (
#ifdef PROTOTYPE
    int nregs, Regn *regs, Regn *r, PointSetType_t ptype, int nlist, int *list)
#else
    nregs, regs, r, ptype, nlist, list)
int nregs, nlist, *list;
Regn *regs, *r;
PointSetType_t ptype;
#endif
{
    int nf, nr, nn, maxfaces, nfaces;
    Face **faces, *f;
    static char *funcname = "unstructured_region";

    if (ptype == PointList || ptype == ElementList) {
        for (nn = 1; nn < nlist; nn++) {
            if (list[nn] < list[nn-1]) {
                qsort (list, nlist, sizeof(int), compare_ints);
                break;
            }
        }
        maxfaces = nlist;
    }
    else if (ptype == PointRange || ptype == ElementRange) {
        if (list[0] > list[1]) {
            nn = list[0];
            list[0] = list[1];
            list[1] = nn;
        }
        maxfaces = list[1] - list[0] + 1;
    }
    else {
        strcpy (r->errmsg, "invalid point set type");
        maxfaces = 0;
    }

    if (maxfaces < 1) return 0;
    faces = (Face **) MALLOC (funcname, maxfaces * sizeof(Face *));

    nfaces = 0;
    for (nr = 0; nr < nregs; nr++) {
        if (!regs[nr].nfaces) continue;
        f = regs[nr].faces;
        for (nf = 0; nf < regs[nr].nfaces; nf++, f++) {
            switch (ptype) {
                case PointList:
                    if (f->id) continue;
                    for (nn = 0; nn < f->nnodes; nn++) {
                        if (!find_int (f->nodes[nn]+1, nlist, list))
                            break;
                    }
                    if (nn == f->nnodes) break;
                    continue;
                case PointRange:
                    if (f->id) continue;
                    for (nn = 0; nn < f->nnodes; nn++) {
                        if (f->nodes[nn]+1 < list[0] ||
                            f->nodes[nn]+1 > list[1])
                            break;
                    }
                    if (nn == f->nnodes) break;
                    continue;
                case ElementList:
                    if (find_int (f->id, nlist, list)) break;
                    continue;
                case ElementRange:
                    if (f->id >= list[0] && f->id <= list[1]) break;
                    continue;
                default:
                    continue;
            }
            if (nfaces == maxfaces) {
                maxfaces += 100;
                faces = (Face **) REALLOC (funcname,
                    maxfaces * sizeof(Face *), faces);
            }
            faces[nfaces++] = f;
        }
    }
    if (nfaces) {
        r->nfaces = nfaces;
        r->faces = (Face *) MALLOC (funcname, nfaces * sizeof(Face));
        for (nf = 0; nf < nfaces; nf++) {
            r->faces[nf].id = faces[nf]->id;
            r->faces[nf].nnodes = faces[nf]->nnodes;
            for (nn = 0; nn < faces[nf]->nnodes; nn++)
                r->faces[nf].nodes[nn] = faces[nf]->nodes[nn];
        }
    }
    else
        strcpy (r->errmsg, "couldn't find any exterior faces");
    free (faces);
    return nfaces;
}

/*-------------------------------------------------------------------*/

static int unstructured_zone (
#ifdef PROTOTYPE
    Tcl_Interp *interp)
#else
    interp)
Tcl_Interp *interp;
#endif
{
    int ns, is, ie, nb, ip, nr, hasvol;
    int nsect, nints, nconns, nbocos, nrmlindex[3];
    int nelem, elemsize, *conn, *par;
    int range[6], d_range[6], transform[3], rind[2];
    GridLocation_t location;
    GridConnectivityType_t type;
    PointSetType_t ptype, d_ptype;
    ZoneType_t d_ztype;
    DataType_t datatype;
    BCType_t bctype;
    ElementType_t elemtype;
    char name[33], d_name[33];
    Zone *z = &zones[cgnszone-1];
    static char *funcname = "unstructured_zone";
    static char *dspmsg = "finding exterior faces for";

    if (cg_nsections (cgnsfn, cgnsbase, cgnszone, &nsect) ||
        cg_n1to1 (cgnsfn, cgnsbase, cgnszone, &nints) ||
        cg_nconns (cgnsfn, cgnsbase, cgnszone, &nconns) ||
        cg_nbocos (cgnsfn, cgnsbase, cgnszone, &nbocos)) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        return 1;
    }
    z->nregs = nsect + nints + nconns + nbocos;
    z->regs = (Regn *) MALLOC (funcname, z->nregs * sizeof(Regn));
    nr = 0;

    /* element sets */

    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgnsfn, cgnsbase, cgnszone, ns,
            name, &elemtype, &is, &ie, &nb, &ip) ||
            cg_ElementDataSize (cgnsfn, cgnsbase, cgnszone, ns, &elemsize)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        zone_message (dspmsg, name);
        strcpy (z->regs[nr].name, name);
        z->regs[nr].type = REG_ELEM;
        z->regs[nr].data[0] = elemtype;
        z->regs[nr].data[1] = is;
        z->regs[nr].data[2] = ie;
        z->regs[nr].data[3] = 0;
        z->regs[nr].data[4] = 0;
        if (elemtype < TRI_3 || elemtype > MIXED)
            strcpy (z->regs[nr].errmsg, "invalid element type");
        else {
            int ne, n, i;
            nelem = ie - is + 1;
            conn = (int *) MALLOC (funcname, elemsize * sizeof(int));
            par = (int *) MALLOC (funcname, 4 * nelem * sizeof(int));
            if (cg_elements_read (cgnsfn, cgnsbase, cgnszone, ns, conn, par)) {
                free (conn);
                free (par);
                Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
                return 1;
            }
            free (par);

            if (cg_goto (cgnsfn, cgnsbase, "Zone_t", cgnszone,
                    "Elements_t", ns, "end") ||
                cg_rind_read (rind)) {
                rind[0] = rind[1] = 0;
            }
            z->regs[nr].data[3] = rind[0];
            z->regs[nr].data[4] = rind[1];

            /* check element indices */

            cg_npe (elemtype, &ip);
            for (n = 0, ne = 0; ne < nelem; ne++) {
                if (elemtype == MIXED) {
                    nb = conn[n++];
                    if (cg_npe ((ElementType_t)nb, &ip) || ip <= 0) {
                        strcpy(z->regs[nr].errmsg,
                            "unhandled element type found in MIXED");
                        break;
                    }
                }
                for (i = 0; i < ip; i++) {
                    if (conn[n] < 1 || conn[n] > z->nnodes) {
                        strcpy(z->regs[nr].errmsg, "invalid element index");
                        break;
                    }
                    n++;
                }
                if (i < ip) break;
            }
            hasvol = 0;
            if (ne == nelem) {
                facehash = HashCreate (nelem > 1024 ? nelem / 3 : 127,
                    compare_faces, hash_face);
                if (NULL == facehash)
                    FATAL ("face hash table creation failed");

                exterior_faces (nelem, elemtype, conn, rind);
                hasvol = HashSize (facehash);
                element_faces (is, nelem, elemtype, conn, rind);

                nb = HashSize (facehash);
                if (nb) {
                    z->regs[nr].dim = hasvol ? 3 : 2;
                    z->regs[nr].faces = (Face *) MALLOC (funcname,
                        nb * sizeof(Face));
                    HashList (facehash, get_faces, &z->regs[nr]);
                }
                else {
                    strcpy (z->regs[nr].errmsg,
                        "couldn't find any exterior faces");
                }
                HashDestroy (facehash, NULL);
            }
#ifndef NO_CUTTING_PLANE
            if (z->regs[nr].dim > 1) {
                z->regs[nr].elemtype = elemtype;
                z->regs[nr].nelems = nelem;
                z->regs[nr].elems = conn;
                /* fix element indexing */
                cg_npe (elemtype, &ip);
                for (n = 0, ne = 0; ne < nelem; ne++) {
                    if (elemtype == MIXED) {
                        nb = conn[n++];
                        cg_npe ((ElementType_t)nb, &ip);
                    }
                    for (i = 0; i < ip; i++) {
                        (conn[n])--;
                        n++;
                    }
                }
            }
            else
#endif
                free (conn);
        }
        nr++;
    }

    /* 1to1 connectivities */

    for (ns = 1; ns <= nints; ns++) {
        if (cg_1to1_read (cgnsfn, cgnsbase, cgnszone, ns,
            name, d_name, range, d_range, transform)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        zone_message (dspmsg, name);
        strcpy (z->regs[nr].name, name);
        z->regs[nr].type = REG_1TO1;
        z->regs[nr].data[0] = 2;
        z->regs[nr].data[1] = range[0];
        z->regs[nr].data[2] = range[1];
        strcpy (z->regs[nr].d_name, d_name);
        if (unstructured_region (nsect, z->regs, &z->regs[nr],
            PointRange, 2, range)) z->regs[nr].dim = 2;
        nr++;
    }

    /* general connectivities */

    for (ns = 1; ns <= nconns; ns++) {
        if (cg_conn_info (cgnsfn, cgnsbase, cgnszone, ns, name,
            &location, &type, &ptype, &ip, d_name, &d_ztype,
            &d_ptype, &datatype, &is)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        zone_message (dspmsg, name);
        conn = (int *) MALLOC (funcname, ip * sizeof(int));
        /* this should be donor index dim but a bug in versions prior to 2.2
           use cell dim instead */
        par = (int *) MALLOC (funcname, CellDim * is * sizeof(int));
        ie = cg_conn_read (cgnsfn, cgnsbase, cgnszone, ns, conn, Integer, par);
        free (par);
        if (ie) {
            free (conn);
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        strcpy (z->regs[nr].name, name);
        z->regs[nr].type = REG_CONN;
        z->regs[nr].data[0] = type;
        z->regs[nr].data[1] = location;
        z->regs[nr].data[2] = ptype;
        z->regs[nr].data[3] = ip;
        if (ptype == PointRange) {
            z->regs[nr].data[4] = conn[0];
            z->regs[nr].data[5] = conn[1];
        }
        strcpy (z->regs[nr].d_name, d_name);

        if (type != Abutting && type != Abutting1to1) {
            strcpy(z->regs[nr].errmsg,
               "can only handle Abutting or Abutting1to1 currently");
        }
        else if (ptype != PointList && ptype != PointRange) {
            strcpy(z->regs[nr].errmsg,
               "point set type not PointList or PointRange");
        }
        else if (location != Vertex && location != CellCenter &&
                 location != FaceCenter) {
            strcpy(z->regs[nr].errmsg,
               "location not Vertex, CellCenter or FaceCenter");
        }
        else {
            if (location != Vertex)
                ptype = (ptype == PointRange ? ElementRange : ElementList);
            if (unstructured_region (nsect, z->regs, &z->regs[nr],
                ptype, ip, conn)) z->regs[nr].dim = 2;
        }
        free (conn);
        nr++;
    }

    /* boundary conditions */

    for (ns = 1; ns <= nbocos; ns++) {
        if (cg_boco_info (cgnsfn, cgnsbase, cgnszone, ns, name,
            &bctype, &ptype, &ip, nrmlindex, &is, &datatype, &ie)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        zone_message (dspmsg, name);
        if (cg_goto (cgnsfn, cgnsbase, "Zone_t", cgnszone,
           "ZoneBC_t", 1, "BC_t", ns, "end")) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        if (cg_gridlocation_read (&location))
            location = Vertex;
        conn = (int *) MALLOC (funcname, ip * sizeof(int));
        par = NULL;
        if (is) {
            ie = datatype == RealSingle ? sizeof(float) : sizeof(double);
            par = (int *) MALLOC (funcname, is * ie);
        }
        ie = cg_boco_read (cgnsfn, cgnsbase, cgnszone, ns, conn, par);
        if (is) free (par);
        if (ie) {
            free (conn);
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return 1;
        }
        strcpy (z->regs[nr].name, name);
        z->regs[nr].type = REG_BOCO;
        z->regs[nr].data[0] = bctype;
        z->regs[nr].data[1] = location;
        z->regs[nr].data[2] = ptype;
        z->regs[nr].data[3] = ip;
        if (ptype == PointRange || ptype == ElementRange) {
            z->regs[nr].data[4] = conn[0];
            z->regs[nr].data[5] = conn[1];
        }

        if ((ptype == PointRange || ptype == PointList) &&
            (location == FaceCenter || location == CellCenter))
            ptype = (ptype == PointRange ? ElementRange : ElementList);
        if (unstructured_region (nsect, z->regs, &z->regs[nr],
            ptype, ip, conn)) z->regs[nr].dim = 2;
        free (conn);
        nr++;
    }

    z->nregs = nr;
    return 0;
}

/*====================================================================
 * find region edges
 *====================================================================*/

/*-------------------------------------------------------------------*/

static int compare_edges (
#ifdef PROTOTYPE
    void *v1, void *v2)
#else
    v1, v2)
void *v1, *v2;
#endif
{
    Edge *e1 = (Edge *)v1;
    Edge *e2 = (Edge *)v2;

    if (e1->nodes[0] < e1->nodes[1]) {
        if (e2->nodes[0] < e2->nodes[1])
            return (e1->nodes[0] - e2->nodes[0]);
        return (e1->nodes[0] - e2->nodes[1]);
    }
    if (e2->nodes[0] < e2->nodes[1])
        return (e1->nodes[1] - e2->nodes[0]);
    return (e1->nodes[1] - e2->nodes[1]);
}

/*-------------------------------------------------------------------*/

static unsigned hash_edge (
#ifdef PROTOTYPE
    void *v)
#else
    v)
void *v;
#endif
{
    Edge *e = (Edge *)v;

    return ((unsigned)(e->nodes[0] + e->nodes[1]));
}

/*-------------------------------------------------------------------*/

static int get_edges (
#ifdef PROTOTYPE
    void *ve, void *vr)
#else
    ve, vr)
void *ve, *vr;
#endif
{
    Edge *e = (Edge *)ve;
    Regn *r = (Regn *)vr;

    r->edges[r->nedges].nodes[0] = e->nodes[0];
    r->edges[r->nedges].nodes[1] = e->nodes[1];
    (r->nedges)++;
    return 1;
}

/*-------------------------------------------------------------------*/

static void extract_edges (
#ifdef PROTOTYPE
    Regn *r)
#else
    r)
Regn *r;
#endif
{
    int n, i, j, k;
    Face *f;
    Edge edge, *ep;
    HASH *edgehash;
    float dot;
    static char *funcname = "extract_edges";

    if (!r->nfaces) return;
    edgehash = HashCreate (r->nfaces, compare_edges, hash_edge);
    if (NULL == edgehash)
        FATAL ("edge hash table creation failed");
    for (f = r->faces, j = 0; j < r->nfaces; j++, f++) {
        for (i = 0, k = f->nnodes-1; i < f->nnodes; k = i++) {
            if (f->nodes[i] == f->nodes[k]) continue;
            if (f->nodes[i] < f->nodes[k]) {
                edge.nodes[0] = f->nodes[i];
                edge.nodes[1] = f->nodes[k];
            }
            else {
                edge.nodes[0] = f->nodes[k];
                edge.nodes[1] = f->nodes[i];
            }
            ep = (Edge *) HashFind (edgehash, &edge);
            if (NULL == ep) {
                ep = (Edge *) MALLOC (funcname, sizeof(Edge));
                ep->nodes[0] = edge.nodes[0];
                ep->nodes[1] = edge.nodes[1];
                ep->id = j;
                (void) HashAdd (edgehash, ep);
            }
            else {
                n = ep->id;
                dot = r->faces[n].normal[0] * f->normal[0] +
                      r->faces[n].normal[1] * f->normal[1] +
                      r->faces[n].normal[2] * f->normal[2];
                if (dot > EDGE_ANGLE) {
                    HashDelete (edgehash, ep);
                    free (ep);
                }
            }
        }
    }

    n = HashSize (edgehash);
    if (n) {
        r->nedges = 0;
        r->edges = (Edge *) MALLOC (funcname, n * sizeof(Edge));
        HashList (edgehash, get_edges, r);
    }
    HashDestroy (edgehash, NULL);
}

/*===================================================================
 * region manipulation
 *===================================================================*/

/*-------------------------------------------------------------------*/

static float *compute_normal (
#ifdef PROTOTYPE
    Node n0, Node n1, Node n2, Node n3)
#else
    n0, n1, n2, n3)
Node n0, n1, n2, n3;
#endif
{
    int j;
    double xn, yn, zn, sn;
    double d1[3], d2[3];
    static float norm[3];

    /* triangle */

    if (NULL == n3) {
        for (j = 0; j < 3; j++) {
            d1[j] = n1[j] - n0[j];
            d2[j] = n2[j] - n0[j];
        }
        sn = 0.5;
    }

    /* quadrilateral */

    else {
        for (j = 0; j < 3; j++) {
            d1[j] = n2[j] - n0[j];
            d2[j] = n3[j] - n1[j];
        }
        sn = 1.0;
    }
    xn = sn * (d1[1] * d2[2] - d2[1] * d1[2]);
    yn = sn * (d1[2] * d2[0] - d2[2] * d1[0]);
    zn = sn * (d1[0] * d2[1] - d2[0] * d1[1]);
    sn = sqrt (xn * xn + yn * yn + zn * zn);
    if (sn == 0.0) sn = 1.0;
    norm[0] = (float)(xn / sn);
    norm[1] = (float)(yn / sn);
    norm[2] = (float)(zn / sn);
    return norm;
}

/*-------------------------------------------------------------------*/

static void face_normals (
#ifdef PROTOTYPE
    Zone *z, Regn *r)
#else
    z, r)
Zone *z;
Regn *r;
#endif
{
    int i, j;
    Face *f;
    float *norm;

    for (f = r->faces, j = 0; j < r->nfaces; j++, f++) {
        norm = compute_normal(z->nodes[f->nodes[0]],
            z->nodes[f->nodes[1]], z->nodes[f->nodes[2]],
            f->nnodes == 4 ? z->nodes[f->nodes[3]] : NULL);
        for (i = 0; i < 3; i++)
            f->normal[i] = norm[i];
    }
}

/*-------------------------------------------------------------------*/

static void bounding_box (
#ifdef PROTOTYPE
    Zone *z, Regn *r)
#else
    z, r)
Zone *z;
Regn *r;
#endif
{
    int i, j, n;

    if (r->nfaces) {
        Face *f = r->faces;
        for (j = 0; j < 3; j++)
            r->bbox[j][0] = r->bbox[j][1] = z->nodes[f->nodes[0]][j];
        for (n = 0; n < r->nfaces; n++, f++) {
            for (i = 0; i < f->nnodes; i++) {
                for (j = 0; j < 3; j++) {
                    if (r->bbox[j][0] > z->nodes[f->nodes[i]][j])
                        r->bbox[j][0] = z->nodes[f->nodes[i]][j];
                    if (r->bbox[j][1] < z->nodes[f->nodes[i]][j])
                        r->bbox[j][1] = z->nodes[f->nodes[i]][j];
                }
            }
        }
    }
    else if (r->nedges) {
        Edge *e = r->edges;
        for (j = 0; j < 3; j++)
            r->bbox[j][0] = r->bbox[j][1] = z->nodes[e->nodes[0]][j];
        for (n = 0; n < r->nedges; n++, e++) {
            for (i = 0; i < 2; i++) {
                for (j = 0; j < 3; j++) {
                    if (r->bbox[j][0] > z->nodes[e->nodes[i]][j])
                        r->bbox[j][0] = z->nodes[e->nodes[i]][j];
                    if (r->bbox[j][1] < z->nodes[e->nodes[i]][j])
                        r->bbox[j][1] = z->nodes[e->nodes[i]][j];
                }
            }
        }
    }
    else {
        for (j = 0; j < 3; j++)
            r->bbox[j][0] = r->bbox[j][1] = 0.0;
    }
}

/*-------------------------------------------------------------------*/

static void get_bounds (
#ifdef PROTOTYPE
    int all, float bbox[3][2])
#else
    all, bbox)
int all;
float bbox[3][2];
#endif
{
    int nz, nr, n, first = 1;

    for (nz = 0; nz < nzones; nz++) {
        for (nr = 0; nr < zones[nz].nregs; nr++) {
            if (zones[nz].regs[nr].nfaces &&
               (all || zones[nz].regs[nr].mode)) {
                if (first) {
                    for (n = 0; n < 3; n++) {
                        bbox[n][0] = zones[nz].regs[nr].bbox[n][0];
                        bbox[n][1] = zones[nz].regs[nr].bbox[n][1];
                    }
                    first = 0;
                }
                else {
                    for (n = 0; n < 3; n++) {
                        if (bbox[n][0] > zones[nz].regs[nr].bbox[n][0])
                            bbox[n][0] = zones[nz].regs[nr].bbox[n][0];
                        if (bbox[n][1] < zones[nz].regs[nr].bbox[n][1])
                            bbox[n][1] = zones[nz].regs[nr].bbox[n][1];
                    }
                }
            }
        }
    }
    if (first) {
        for (n = 0; n < 3; n++) {
            bbox[n][0] = 0.0;
            bbox[n][1] = 1.0;
        }
    }
}

/*-------------------------------------------------------------------*/

static void draw_outlines (
#ifdef PROTOTYPE
    Zone *z, Regn *r)
#else
    z, r)
Zone *z;
Regn *r;
#endif
{
    glDisable (GL_LIGHTING);
    glShadeModel (GL_FLAT);
    glBegin (GL_LINES);
    if (r->nedges) {
        int ne;
        for (ne = 0; ne < r->nedges; ne++) {
            glVertex3fv (z->nodes[r->edges[ne].nodes[0]]);
            glVertex3fv (z->nodes[r->edges[ne].nodes[1]]);
        }
    }
    else {
        glVertex3f (r->bbox[0][0], r->bbox[1][0], r->bbox[2][0]);
        glVertex3f (r->bbox[0][1], r->bbox[1][0], r->bbox[2][0]);
        glVertex3f (r->bbox[0][1], r->bbox[1][0], r->bbox[2][0]);
        glVertex3f (r->bbox[0][1], r->bbox[1][1], r->bbox[2][0]);
        glVertex3f (r->bbox[0][1], r->bbox[1][1], r->bbox[2][0]);
        glVertex3f (r->bbox[0][0], r->bbox[1][1], r->bbox[2][0]);
        glVertex3f (r->bbox[0][0], r->bbox[1][1], r->bbox[2][0]);
        glVertex3f (r->bbox[0][0], r->bbox[1][0], r->bbox[2][0]);
        glVertex3f (r->bbox[0][0], r->bbox[1][0], r->bbox[2][1]);
        glVertex3f (r->bbox[0][1], r->bbox[1][0], r->bbox[2][1]);
        glVertex3f (r->bbox[0][1], r->bbox[1][0], r->bbox[2][1]);
        glVertex3f (r->bbox[0][1], r->bbox[1][1], r->bbox[2][1]);
        glVertex3f (r->bbox[0][1], r->bbox[1][1], r->bbox[2][1]);
        glVertex3f (r->bbox[0][0], r->bbox[1][1], r->bbox[2][1]);
        glVertex3f (r->bbox[0][0], r->bbox[1][1], r->bbox[2][1]);
        glVertex3f (r->bbox[0][0], r->bbox[1][0], r->bbox[2][1]);
        glVertex3f (r->bbox[0][0], r->bbox[1][0], r->bbox[2][0]);
        glVertex3f (r->bbox[0][0], r->bbox[1][0], r->bbox[2][1]);
        glVertex3f (r->bbox[0][1], r->bbox[1][0], r->bbox[2][0]);
        glVertex3f (r->bbox[0][1], r->bbox[1][0], r->bbox[2][1]);
        glVertex3f (r->bbox[0][1], r->bbox[1][1], r->bbox[2][0]);
        glVertex3f (r->bbox[0][1], r->bbox[1][1], r->bbox[2][1]);
        glVertex3f (r->bbox[0][0], r->bbox[1][1], r->bbox[2][0]);
        glVertex3f (r->bbox[0][0], r->bbox[1][1], r->bbox[2][1]);
    }
    glEnd ();
}

/*-------------------------------------------------------------------*/

static void draw_mesh (
#ifdef PROTOTYPE
    Zone *z, Regn *r)
#else
    z, r)
Zone *z;
Regn *r;
#endif
{
    int nf, nn;
    Face *f;

    glEnable (GL_LIGHTING);
    glShadeModel (GL_FLAT);
    glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
    for (f = r->faces, nf = 0; nf < r->nfaces; nf++, f++) {
        glBegin (f->nnodes == 3 ? GL_TRIANGLES : GL_QUADS);
        glNormal3fv (f->normal);
        for (nn = 0; nn < f->nnodes; nn++)
            glVertex3fv (z->nodes[f->nodes[nn]]);
        glEnd ();
    }
}

/*-------------------------------------------------------------------*/

static void draw_shaded (
#ifdef PROTOTYPE
    Zone *z, Regn *r)
#else
    z, r)
Zone *z;
Regn *r;
#endif
{
    int nf, nn;
    Face *f;

    glEnable (GL_LIGHTING);
    glShadeModel (GL_FLAT);
    glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
    for (f = r->faces, nf = 0; nf < r->nfaces; nf++, f++) {
        glBegin (f->nnodes == 3 ? GL_TRIANGLES : GL_QUADS);
        glNormal3fv (f->normal);
        for (nn = 0; nn < f->nnodes; nn++)
            glVertex3fv (z->nodes[f->nodes[nn]]);
        glEnd ();
    }
}

/*===================================================================
 *           tcl interface
 *===================================================================*/

/*---------- CGNSopen ----------------------------------------------
 * open a CGNS file - return bases
 *------------------------------------------------------------------*/

static int CGNSopen (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int fn, nb, idum;
    static char buff[33];

    if (argc != 2) {
        Tcl_SetResult (interp, "usage: CGNSopen filename", TCL_STATIC);
        return TCL_ERROR;
    }
    Tcl_ResetResult (interp);

    if (cgnsfn) {
        cg_close (cgnsfn);
        cgnsfn = 0;
    }
    if (cg_open (argv[1], CG_MODE_READ, &fn)) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        return TCL_ERROR;
    }
    if (cg_nbases (fn, &nb)) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        cg_close (fn);
        return TCL_ERROR;
    }
    if (nb < 1) {
        Tcl_SetResult (interp, "no bases defined", TCL_STATIC);
        cg_close (fn);
        return TCL_ERROR;
    }
    nbases = nb;
    for (nb = 1; nb <= nbases; nb++) {
        if (cg_base_read (fn, nb, buff, &idum, &idum)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            cg_close (fn);
            return TCL_ERROR;
        }
        Tcl_AppendElement (interp, buff);
    }
    cgnsfn = fn;
    return TCL_OK;
}

/*---------- CGNSclose ---------------------------------------------
 * close the open CGNS file
 *------------------------------------------------------------------*/

static int CGNSclose (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    if (cgnsfn && cg_close (cgnsfn)) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        return TCL_ERROR;
    }
    cgnsfn = 0;
    free_all ();
    Tcl_ResetResult (interp);
    return TCL_OK;
}

/*---------- CGNSbase ----------------------------------------------
 * set the CGNS base - return zones
 *------------------------------------------------------------------*/

static int CGNSbase (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int base, nz, sizes[9];
    char buff[33];

    if (!cgnsfn) {
        Tcl_SetResult (interp, "CGNS file not open", TCL_STATIC);
        return TCL_ERROR;
    }
    if (argc != 2) {
        Tcl_SetResult (interp, "usage: CGNSbase basenum", TCL_STATIC);
        return TCL_ERROR;
    }
    base = atoi (argv[1]) + 1;
    if (base < 1 || base > nbases) {
        Tcl_SetResult (interp, "base number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    Tcl_ResetResult (interp);
    cgnsbase = base;
    if (cg_base_read (cgnsfn, cgnsbase, BaseName, &CellDim, &PhyDim) ||
        cg_nzones (cgnsfn, cgnsbase, &nz)) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        return TCL_ERROR;
    }
    free_all ();
    if (CellDim < 2 || CellDim > 3 || PhyDim < 2 || PhyDim > 3) {
        Tcl_SetResult (interp, "CellDim and Phydim not 2 or 3", TCL_STATIC);
        return TCL_ERROR;
    }
    nzones = nz;
    zones = (Zone *) MALLOC ("CGNSbase", nzones * sizeof(Zone));
    for (nz = 1; nz <= nzones; nz++) {
        if (cg_zone_read (cgnsfn, cgnsbase, nz, buff, sizes)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return TCL_ERROR;
        }
        strcpy (zones[nz-1].name, buff);
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/*---------- CGNSzone ----------------------------------------------
 * set the CGNS zone - return regions
 *------------------------------------------------------------------*/

static int CGNSzone (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int i, j, n, zone, nr, nf, nn, ncoords;
    int sizes[9], rng[2][3], rind[6], nnodes;
    int *tag;
    DataType_t datatype;
    ZoneType_t zonetype;
    Node *nodes;
    float *xyz;
    double rad, theta, phi;
    char buff[65], coordtype[4];
    Zone *z;
    static char *funcname = "CGNSzone";

    if (!cgnsfn) {
        Tcl_SetResult (interp, "CGNS file not open", TCL_STATIC);
        return TCL_ERROR;
    }
    if (argc != 2) {
        Tcl_SetResult (interp, "usage: CGNSzone zonenum", TCL_STATIC);
        return TCL_ERROR;
    }
    zone = atoi (argv[1]) + 1;
    if (zone < 1 || zone > nzones) {
        Tcl_SetResult (interp, "zone number out of range", TCL_STATIC);
        return TCL_ERROR;
    }

    if (cg_zone_read (cgnsfn, cgnsbase, zone, buff, sizes) ||
        cg_zone_type (cgnsfn, cgnsbase, zone, &zonetype)) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        return TCL_ERROR;
    }
    if (zonetype == Structured) {
        for (n = 0; n < CellDim; n++) {
            if (sizes[n] < 2) {
                Tcl_SetResult (interp, "zone dimension < 2", TCL_STATIC);
                return TCL_ERROR;
            }
        }
    }
    else if (zonetype != Unstructured) {
        Tcl_SetResult (interp, "invalid zone type", TCL_STATIC);
        return TCL_ERROR;
    }
    cgnszone = zone;
    z = &zones[zone-1];

    /* get number of coordinates */

    if (cg_ncoords (cgnsfn, cgnsbase, cgnszone, &ncoords)) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        return TCL_ERROR;
    }
    if (ncoords < PhyDim) {
        Tcl_SetResult (interp, "less than PhyDim coordinates", TCL_STATIC);
        return TCL_ERROR;
    }

    /* check for rind */

    if (cg_goto (cgnsfn, cgnsbase, "Zone_t", zone,
        "GridCoordinates_t", 1, "end")) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        return TCL_ERROR;
    }
    if ((i = cg_rind_read (rind)) != CG_OK) {
        if (i != CG_NODE_NOT_FOUND) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return TCL_ERROR;
        }
        for (n = 0; n < 6; n++)
            rind[n] = 0;
    }

    /* get grid coordinate range */

    if (zonetype == Structured) {
        for (n = 0; n < 3; n++) {
            rng[0][n] = 1;
            rng[1][n] = 1;
        }
        nnodes = 1;
        for (n = 0; n < CellDim; n++) {
            rng[0][n] = rind[2*n] + 1;
            rng[1][n] = rind[2*n] + sizes[n];
            nnodes *= sizes[n];
        }
    }
    else {
        nnodes = sizes[0] + rind[0] + rind[1];
        rng[0][0] = 1;
        rng[1][0] = nnodes;
    }

    /* read the nodes */

    strcpy (coordtype, "   ");
    zone_message ("reading coordinates", NULL);
    xyz = (float *) MALLOC (funcname, nnodes * sizeof(float));
    nodes = (Node *) MALLOC (funcname, nnodes * sizeof(Node));
    for (i = 1; i <= ncoords; i++) {
        if (cg_coord_info (cgnsfn, cgnsbase, cgnszone, i, &datatype, buff) ||
            cg_coord_read (cgnsfn, cgnsbase, cgnszone, buff,
                RealSingle, rng[0], rng[1], xyz)) {
            free (xyz);
            free (nodes);
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return TCL_ERROR;
        }
        if (0 == strcmp (buff, "CoordinateX") ||
            0 == strcmp (buff, "CoordinateR"))
            j = 0;
        else if (0 == strcmp (buff, "CoordinateY") ||
                 0 == strcmp (buff, "CoordinateTheta"))
            j = 1;
        else if (0 == strcmp (buff, "CoordinateZ") ||
                 0 == strcmp (buff, "CoordinatePhi"))
            j = 2;
        else
            continue;
        if (coordtype[j] == ' ' || strchr ("XYZ", buff[10]) != NULL)
            coordtype[j] = buff[10];
        for (n = 0; n < nnodes; n++)
            nodes[n][j] = xyz[n];
    }
    free (xyz);
    if (0 == strncmp (coordtype, "RTZ", PhyDim)) {
        for (n = 0; n < nnodes; n++) {
            rad = nodes[n][0];
            theta = nodes[n][1];
            nodes[n][0] = (float)(rad * cos (theta));
            nodes[n][1] = (float)(rad * sin (theta));
        }
    }
    else if (0 == strcmp (coordtype, "RTP")) {
        for (n = 0; n < nnodes; n++) {
            rad = nodes[n][0];
            theta = nodes[n][1];
            phi = nodes[n][2];
            nodes[n][0] = (float)(rad * sin (theta) * cos (phi));
            nodes[n][1] = (float)(rad * sin (theta) * sin (phi));
            nodes[n][2] = (float)(rad * cos (theta));
        }
    }
    else if (strncmp (coordtype, "XYZ", PhyDim)) {
        free (nodes);
        Tcl_SetResult (interp, "unknown coordinate system", TCL_STATIC);
        return TCL_ERROR;
    }

    z->nnodes = nnodes;
    z->nodes = nodes;

    /* build regions */

    if (zonetype == Structured) {
        if (structured_zone (interp, sizes))
            return TCL_ERROR;
    }
    else {
        if (unstructured_zone (interp))
            return TCL_ERROR;
    }

#ifdef NO_CUTTING_PLANE

    tag = (int *) MALLOC (funcname, nnodes * sizeof(int));
    for (n = 0; n < nnodes; n++)
        tag[n] = -1;

    /* tag nodes which are actually used */

    for (nn = 0, nr = 0; nr < z->nregs; nr++) {
        for (nf = 0; nf < z->regs[nr].nfaces; nf++) {
            for (n = 0; n < z->regs[nr].faces[nf].nnodes; n++) {
                i = z->regs[nr].faces[nf].nodes[n];
                if (tag[i] < 0)
                    tag[i] = nn++;
            }
        }
    }

    nodes = (Node *) MALLOC (funcname, nn * sizeof(Node));
    for (n = 0; n < nnodes; n++) {
        if (tag[n] >= 0) {
            j = tag[n];
            for (i = 0; i < 3; i++)
                nodes[j][i] = z->nodes[n][i];
        }
    }

    free(z->nodes);
    z->nodes = nodes;
    z->nnodes = nn;

    /* re-index region faces */

    for (nr = 0; nr < z->nregs; nr++) {
        for (nf = 0; nf < z->regs[nr].nfaces; nf++) {
            for (n = 0; n < z->regs[nr].faces[nf].nnodes; n++) {
                i = z->regs[nr].faces[nf].nodes[n];
                z->regs[nr].faces[nf].nodes[n] = tag[i];
            }
        }
    }

    free(tag);

#endif

    /* find region bounding box, edges and normals */

    zone_message ("finding normals and edges", NULL);
    for (nr = 0; nr < z->nregs; nr++) {
        if (z->regs[nr].nfaces) {
            bounding_box (z, &z->regs[nr]);
            face_normals (z, &z->regs[nr]);
            extract_edges (&z->regs[nr]);
        }
    }

    Tcl_ResetResult (interp);
    for (nr = 0; nr < z->nregs; nr++) {
        switch (z->regs[nr].type) {
            case REG_MESH:
                strcpy(buff, z->regs[nr].name);
                break;
            case REG_ELEM:
                sprintf(buff, "<Element Sections>/%s", z->regs[nr].name);
                break;
            case REG_1TO1:
                sprintf(buff, "<1to1 Connections>/%s", z->regs[nr].name);
                break;
            case REG_CONN:
                sprintf(buff, "<General Connections>/%s", z->regs[nr].name);
                break;
            case REG_BOCO:
                sprintf(buff, "<Boundary Conditions>/%s", z->regs[nr].name);
                break;
            case REG_BNDS:
                sprintf(buff, "<Mesh Boundaries>/%s", z->regs[nr].name);
                break;
        }
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/*---------- CGNSsummary -------------------------------------------
 * return info summary string
 *------------------------------------------------------------------*/

static int CGNSsummary (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, nz;
    char *p, buff[128];
    Regn *r;

    if (!cgnsfn) {
        Tcl_SetResult (interp, "CGNS file not open", TCL_STATIC);
        return TCL_ERROR;
    }
    if (argc < 1 || argc > 3) {
        Tcl_SetResult (interp, "usage: CGNSsummary [zone [reg]]", TCL_STATIC);
        return TCL_ERROR;
    }
    if (argc == 1) {
        sprintf (buff, "Physical Dim = %d, Cell Dim = %d", PhyDim, CellDim);
        Tcl_SetResult (interp, buff, TCL_STATIC);
        return TCL_OK;
    }

    nz = atoi (argv[1]);
    if (nz < 0 || nz >= nzones) {
        Tcl_SetResult (interp, "zone number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    Tcl_ResetResult (interp);

    if (argc == 2) {
        int sizes[9];
        ZoneType_t zonetype;
        if (cg_zone_read (cgnsfn, cgnsbase, nz+1, buff, sizes) ||
            cg_zone_type (cgnsfn, cgnsbase, nz+1, &zonetype)) {
            Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
            return TCL_ERROR;
        }
        Tcl_AppendResult (interp, cg_ZoneTypeName(zonetype),
            " Zone : ", NULL);
        if (zonetype == Unstructured) {
            sprintf (buff, "%d vertices, %d elements", sizes[0], sizes[1]);
            Tcl_AppendResult (interp, buff, NULL);
        }
        else {
            sprintf (buff, "%d", sizes[0]);
            for (n = 1; n < CellDim; n++) {
                p = buff + strlen(buff);
                sprintf (p, " x %d", sizes[n]);
            }
            Tcl_AppendResult (interp, buff, " vertices", NULL);
        }
        return TCL_OK;
    }

    n = atoi (argv[2]);
    if (n < 0 || n >= zones[nz].nregs) {
        Tcl_SetResult (interp, "region number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    r = &zones[nz].regs[n];

    switch (r->type) {
        case REG_MESH:
            if (CellDim == 2)
                sprintf (buff, "%d x %d", r->data[0], r->data[1]);
            else
                sprintf (buff, "%d x %d x %d", r->data[0], r->data[1],
                    r->data[2]);
            Tcl_AppendResult (interp, "Structured Mesh : ",
                buff, " vertices", NULL);
            break;
        case REG_ELEM:
            sprintf (buff, "%d", r->data[2] - r->data[1] + 1);
            Tcl_AppendResult (interp, cg_ElementTypeName(r->data[0]),
                " Element Set : ", buff, " elements", NULL);
            break;
        case REG_1TO1:
            if (r->data[0] == 2)
                sprintf (buff, "%d", r->data[2] - r->data[1] + 1);
            else
                sprintf (buff, "%d x %d x %d",
                    r->data[4] - r->data[1] + 1,
                    r->data[5] - r->data[2] + 1,
                    r->data[6] - r->data[3] + 1);
            Tcl_AppendResult (interp, "1to1 Connection : PointRange ",
                buff, " -> ", r->d_name, NULL);
            break;
        case REG_CONN:
            if (r->data[2] == PointList || r->data[2] == ElementList)
                sprintf (buff, " %d", r->data[3]);
            else if (r->data[3] == 2)
                sprintf (buff, " %d", r->data[5] - r->data[4] + 1);
            else
                sprintf (buff, " %d x %d x %d",
                    r->data[7] - r->data[4] + 1,
                    r->data[8] - r->data[5] + 1,
                    r->data[9] - r->data[6] + 1);
            Tcl_AppendResult (interp,
                cg_GridConnectivityTypeName(r->data[0]),
                " Connection : ", cg_PointSetTypeName(r->data[2]),
                buff, " -> ", r->d_name, NULL);
            break;
        case REG_BOCO:
            if (r->data[2] == PointList || r->data[2] == ElementList)
                sprintf (buff, " %d", r->data[3]);
            else if (r->data[3] == 2)
                sprintf (buff, " %d", r->data[5] - r->data[4] + 1);
            else
                sprintf (buff, " %d x %d x %d",
                    r->data[7] - r->data[4] + 1,
                    r->data[8] - r->data[5] + 1,
                    r->data[9] - r->data[6] + 1);
            Tcl_AppendResult (interp, cg_BCTypeName(r->data[0]),
                " Boundary Condition : ", cg_PointSetTypeName(r->data[2]),
                buff, NULL);
            break;
        case REG_BNDS:
            sprintf (buff, "%d x %d x %d",
                r->data[3] - r->data[0] + 1,
                r->data[4] - r->data[1] + 1,
                r->data[5] - r->data[2] + 1);
            Tcl_AppendResult (interp, "Mesh Boundary : ", buff,
                " vertices", NULL);
            break;
    }
    return TCL_OK;
}

/*---------- CGNSgetbase -------------------------------------------
 * get base properties
 *------------------------------------------------------------------*/

static int CGNSgetbase (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char cd[16], pd[16], nz[16];

    if (!cgnsfn) {
        Tcl_SetResult (interp, "CGNS file not open", TCL_STATIC);
        return TCL_ERROR;
    }
    if (argc != 1) {
        Tcl_SetResult (interp, "usage: CGNSgetbase", TCL_STATIC);
        return TCL_ERROR;
    }
    Tcl_ResetResult (interp);

    sprintf (pd, "%d", PhyDim);
    sprintf (cd, "%d", CellDim);
    sprintf (nz, "%d", nzones);
    Tcl_AppendResult (interp,
          "Base Name   : ", BaseName,
        "\nPhysical Dim: ", pd,
        "\nCell Dim    : ", cd,
        "\nNumber Zones: ", nz, NULL);
    return TCL_OK;
}

/*---------- CGNSgetzone -------------------------------------------
 * get zone properties
 *------------------------------------------------------------------*/

static int CGNSgetzone (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, ndim, zone, sizes[9], cnts[4];
    ZoneType_t zonetype;
    char *p, buff[65];
    static char *cntname[] = {
        "Element Sections",
        "1to1 Connections",
        "General Connections",
        "Boundary Conditions"
    };

    if (!cgnsfn) {
        Tcl_SetResult (interp, "CGNS file not open", TCL_STATIC);
        return TCL_ERROR;
    }
    if (argc != 2) {
        Tcl_SetResult (interp, "usage: CGNSgetzone zonenum", TCL_STATIC);
        return TCL_ERROR;
    }
    zone = atoi (argv[1]) + 1;
    if (zone < 1 || zone > nzones) {
        Tcl_SetResult (interp, "zone number out of range", TCL_STATIC);
        return TCL_ERROR;
    }

    if (cg_zone_read (cgnsfn, cgnsbase, zone, buff, sizes) ||
        cg_zone_type (cgnsfn, cgnsbase, zone, &zonetype) ||
        cg_nsections (cgnsfn, cgnsbase, zone, &cnts[0]) ||
        cg_n1to1 (cgnsfn, cgnsbase, zone, &cnts[1]) ||
        cg_nconns (cgnsfn, cgnsbase, zone, &cnts[2]) ||
        cg_nbocos (cgnsfn, cgnsbase, zone, &cnts[3])) {
        Tcl_SetResult (interp, (char *)cg_get_error(), TCL_STATIC);
        return TCL_ERROR;
    }
    Tcl_ResetResult (interp);

    Tcl_AppendResult (interp, "Zone Name          : ", buff,
        "\nType of Zone       : ", cg_ZoneTypeName(zonetype),
        "\nVertex Dimensions  : ", NULL);

    ndim = zonetype == Unstructured ? 1 : CellDim;
    sprintf (buff, "%d", sizes[0]);
    for (n = 1; n < ndim; n++) {
        p = buff + strlen(buff);
        sprintf (p, " x %d", sizes[n]);
    }
    Tcl_AppendResult (interp, buff, "\nCell Dimensions    : ", NULL);

    sprintf (buff, "%d", sizes[ndim]);
    for (n = 1; n < ndim; n++) {
        p = buff + strlen(buff);
        sprintf (p, " x %d", sizes[n+CellDim]);
    }
    Tcl_AppendResult (interp, buff, NULL);

    for (n = 0; n < 4; n++) {
        sprintf (buff, "\n%-19s: %d", cntname[n], cnts[n]);
        Tcl_AppendResult (interp, buff, NULL);
    }
    return TCL_OK;
}

/*---------- CGNSgetregion -----------------------------------------
 * get region properties
 *------------------------------------------------------------------*/

static int CGNSgetregion (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n;
    char *p, buff[128];
    Zone *z;
    Regn *r;

    if (!cgnsfn) {
        Tcl_SetResult (interp, "CGNS file not open", TCL_STATIC);
        return TCL_ERROR;
    }
    if (argc != 3) {
        Tcl_SetResult (interp, "usage: CGNSgetregion zone reg", TCL_STATIC);
        return TCL_ERROR;
    }
    n = atoi (argv[1]);
    if (n < 0 || n >= nzones) {
        Tcl_SetResult (interp, "zone number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    z = &zones[n];
    n = atoi (argv[2]);
    if (n < 0 || n >= z->nregs) {
        Tcl_SetResult (interp, "region number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    r = &z->regs[n];

    Tcl_ResetResult (interp);
    switch (r->type) {
        case REG_MESH:
            if (CellDim == 2)
                sprintf (buff, "%d x %d", r->data[0], r->data[1]);
            else
                sprintf (buff, "%d x %d x %d", r->data[0], r->data[1],
                    r->data[2]);
            Tcl_AppendResult (interp,
                  "Region Name    : ", r->name,
                "\nType of Region : Structured Mesh",
                "\nMesh Dimensions: ", buff, NULL);
            break;
        case REG_ELEM:
            sprintf (buff, "%d -> %d", r->data[1], r->data[2]);
            Tcl_AppendResult (interp,
                  "Region Name     : ", r->name,
                "\nType of Region  : Element Set",
                "\nElement Set Type: ", cg_ElementTypeName(r->data[0]),
                "\nElement Range   : ", buff, NULL);
            if (r->data[3] || r->data[4]) {
                sprintf (buff, "%d %d", r->data[3], r->data[4]);
                Tcl_AppendResult (interp,
                    "\nRind Elements   : ", buff, NULL);
            }
            break;
        case REG_1TO1:
            Tcl_AppendResult (interp,
                  "Region Name   : ", r->name,
                "\nType of Region: 1to1 Connectivity",
                "\nPoint Set Type: PointRange", NULL);
            if (r->data[0] == 2) {
                sprintf (buff, "%d -> %d", r->data[1], r->data[2]);
                Tcl_AppendResult (interp,
                    "\nIndex Range   : ", buff, NULL);
            }
            else {
                sprintf (buff, "%d -> %d", r->data[1], r->data[4]);
                Tcl_AppendResult (interp,
                    "\nI Index Range : ", buff, NULL);
                sprintf (buff, "%d -> %d", r->data[2], r->data[5]);
                Tcl_AppendResult (interp,
                    "\nJ Index Range : ", buff, NULL);
                sprintf (buff, "%d -> %d", r->data[3], r->data[6]);
                Tcl_AppendResult (interp,
                    "\nK Index Range : ", buff, NULL);
            }
            Tcl_AppendResult (interp, "\nDonor Zone    : ", r->d_name, NULL);
            break;
        case REG_CONN:
            Tcl_AppendResult (interp,
                  "Region Name      : ", r->name,
                "\nType of Region   : General Connectivity",
                "\nConnectivity Type: ",
                    cg_GridConnectivityTypeName(r->data[0]),
                "\nGrid Location    : ", cg_GridLocationName(r->data[1]),
                "\nPoint Set Type   : ", cg_PointSetTypeName(r->data[2]),
                NULL);
            if (r->data[2] == PointList || r->data[2] == ElementList) {
                sprintf (buff, "%d", r->data[3]);
                Tcl_AppendResult (interp, "\nNumber of Points : ", buff, NULL);
            }
            else if (r->data[3] == 2) {
                sprintf (buff, "%d -> %d", r->data[4], r->data[5]);
                Tcl_AppendResult (interp, "\nIndex Range      : ", buff, NULL);
            }
            else {
                sprintf (buff, "%d -> %d", r->data[4], r->data[7]);
                Tcl_AppendResult (interp,
                    "\nI Index Range    : ", buff, NULL);
                sprintf (buff, "%d -> %d", r->data[5], r->data[8]);
                Tcl_AppendResult (interp,
                    "\nJ Index Range    : ", buff, NULL);
                sprintf (buff, "%d -> %d", r->data[6], r->data[9]);
                Tcl_AppendResult (interp,
                    "\nK Index Range    : ", buff, NULL);
            }
            Tcl_AppendResult (interp, "\nDonor Zone       : ", r->d_name, NULL);
            break;
        case REG_BOCO:
            Tcl_AppendResult (interp,
                  "Region Name     : ", r->name,
                "\nType of Region  : Boundary Condition",
                "\nType of BC      : ", cg_BCTypeName(r->data[0]),
                "\nGrid Location   : ", cg_GridLocationName(r->data[1]),
                "\nPoint Set Type  : ", cg_PointSetTypeName(r->data[2]),
                NULL);
            if (r->data[2] == PointList || r->data[2] == ElementList) {
                sprintf (buff, "%d", r->data[3]);
                Tcl_AppendResult (interp, "\nNumber of Points: ", buff, NULL);
            }
            else if (r->data[3] == 2) {
                sprintf (buff, "%d -> %d", r->data[4], r->data[5]);
                Tcl_AppendResult (interp, "\nIndex Range     : ", buff, NULL);
            }
            else {
                sprintf (buff, "%d -> %d", r->data[4], r->data[7]);
                Tcl_AppendResult (interp,
                    "\nI Index Range   : ", buff, NULL);
                sprintf (buff, "%d -> %d", r->data[5], r->data[8]);
                Tcl_AppendResult (interp,
                    "\nJ Index Range   : ", buff, NULL);
                sprintf (buff, "%d -> %d", r->data[6], r->data[9]);
                Tcl_AppendResult (interp,
                    "\nK Index Range   : ", buff, NULL);
            }
            break;
        case REG_BNDS:
            strcpy (buff, r->name);
            Tcl_AppendResult (interp,
                  "Region Name   : ", r->name,
                "\nType of Region: Mesh Boundary", NULL);
            sprintf (buff, "%d -> %d", r->data[0], r->data[3]);
            Tcl_AppendResult (interp,
                "\nI Index Range : ", buff, NULL);
            sprintf (buff, "%d -> %d", r->data[1], r->data[4]);
            Tcl_AppendResult (interp,
                "\nJ Index Range : ", buff, NULL);
            sprintf (buff, "%d -> %d", r->data[2], r->data[5]);
            Tcl_AppendResult (interp,
                "\nK Index Range : ", buff, NULL);
            break;
    }
    return TCL_OK;
}

/*---------- CGNSregiondim -----------------------------------------
 * return dimension of a region
 *------------------------------------------------------------------*/

static int CGNSregiondim (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n;
    char buff[16];
    Zone *z;

    if (!cgnsfn) {
        Tcl_SetResult (interp, "CGNS file not open", TCL_STATIC);
        return TCL_ERROR;
    }
    if (argc != 3) {
        Tcl_SetResult (interp, "usage: CGNSregtype zone reg", TCL_STATIC);
        return TCL_ERROR;
    }
    n = atoi (argv[1]);
    if (n < 0 || n >= nzones) {
        Tcl_SetResult (interp, "zone number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    z = &zones[n];
    n = atoi (argv[2]);
    if (n < 0 || n >= z->nregs) {
        Tcl_SetResult (interp, "region number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    Tcl_ResetResult (interp);
    if (!z->regs[n].dim) {
        if (*(z->regs[n].errmsg))
            Tcl_SetResult (interp, z->regs[n].errmsg, TCL_STATIC);
        return TCL_ERROR;
    }
    sprintf (buff, "%d", z->regs[n].dim);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*---------- CGNSbounds --------------------------------------------
 * get bounding box
 *------------------------------------------------------------------*/

static void transform_bounds (float m[16], float bb[3][2])
{
    int i, j;
    float x, y, z, r, bbox[3][2];

    x = m[0] * bb[0][0] + m[4] * bb[1][0] +  m[8] * bb[2][0] + m[12];
    y = m[1] * bb[0][0] + m[5] * bb[1][0] +  m[9] * bb[2][0] + m[13];
    z = m[2] * bb[0][0] + m[6] * bb[1][0] + m[10] * bb[2][0] + m[14];
    bbox[0][0] = bbox[0][1] = x;
    bbox[1][0] = bbox[1][1] = y;
    bbox[2][0] = bbox[2][1] = z;

    x = m[0] * bb[0][1] + m[4] * bb[1][0] +  m[8] * bb[2][0] + m[12];
    y = m[1] * bb[0][1] + m[5] * bb[1][0] +  m[9] * bb[2][0] + m[13];
    z = m[2] * bb[0][1] + m[6] * bb[1][0] + m[10] * bb[2][0] + m[14];
    if (bbox[0][0] > x) bbox[0][0] = x;
    if (bbox[0][1] < x) bbox[0][1] = x;
    if (bbox[1][0] > y) bbox[1][0] = y;
    if (bbox[1][1] < y) bbox[1][1] = y;
    if (bbox[2][0] > z) bbox[2][0] = z;
    if (bbox[2][1] < z) bbox[2][1] = z;

    x = m[0] * bb[0][0] + m[4] * bb[1][1] +  m[8] * bb[2][0] + m[12];
    y = m[1] * bb[0][0] + m[5] * bb[1][1] +  m[9] * bb[2][0] + m[13];
    z = m[2] * bb[0][0] + m[6] * bb[1][1] + m[10] * bb[2][0] + m[14];
    if (bbox[0][0] > x) bbox[0][0] = x;
    if (bbox[0][1] < x) bbox[0][1] = x;
    if (bbox[1][0] > y) bbox[1][0] = y;
    if (bbox[1][1] < y) bbox[1][1] = y;
    if (bbox[2][0] > z) bbox[2][0] = z;
    if (bbox[2][1] < z) bbox[2][1] = z;

    x = m[0] * bb[0][1] + m[4] * bb[1][1] +  m[8] * bb[2][0] + m[12];
    y = m[1] * bb[0][1] + m[5] * bb[1][1] +  m[9] * bb[2][0] + m[13];
    z = m[2] * bb[0][1] + m[6] * bb[1][1] + m[10] * bb[2][0] + m[14];
    if (bbox[0][0] > x) bbox[0][0] = x;
    if (bbox[0][1] < x) bbox[0][1] = x;
    if (bbox[1][0] > y) bbox[1][0] = y;
    if (bbox[1][1] < y) bbox[1][1] = y;
    if (bbox[2][0] > z) bbox[2][0] = z;
    if (bbox[2][1] < z) bbox[2][1] = z;

    x = m[0] * bb[0][0] + m[4] * bb[1][0] +  m[8] * bb[2][1] + m[12];
    y = m[1] * bb[0][0] + m[5] * bb[1][0] +  m[9] * bb[2][1] + m[13];
    z = m[2] * bb[0][0] + m[6] * bb[1][0] + m[10] * bb[2][1] + m[14];
    if (bbox[0][0] > x) bbox[0][0] = x;
    if (bbox[0][1] < x) bbox[0][1] = x;
    if (bbox[1][0] > y) bbox[1][0] = y;
    if (bbox[1][1] < y) bbox[1][1] = y;
    if (bbox[2][0] > z) bbox[2][0] = z;
    if (bbox[2][1] < z) bbox[2][1] = z;

    x = m[0] * bb[0][1] + m[4] * bb[1][0] +  m[8] * bb[2][1] + m[12];
    y = m[1] * bb[0][1] + m[5] * bb[1][0] +  m[9] * bb[2][1] + m[13];
    z = m[2] * bb[0][1] + m[6] * bb[1][0] + m[10] * bb[2][1] + m[14];
    if (bbox[0][0] > x) bbox[0][0] = x;
    if (bbox[0][1] < x) bbox[0][1] = x;
    if (bbox[1][0] > y) bbox[1][0] = y;
    if (bbox[1][1] < y) bbox[1][1] = y;
    if (bbox[2][0] > z) bbox[2][0] = z;
    if (bbox[2][1] < z) bbox[2][1] = z;

    x = m[0] * bb[0][0] + m[4] * bb[1][1] +  m[8] * bb[2][1] + m[12];
    y = m[1] * bb[0][0] + m[5] * bb[1][1] +  m[9] * bb[2][1] + m[13];
    z = m[2] * bb[0][0] + m[6] * bb[1][1] + m[10] * bb[2][1] + m[14];
    if (bbox[0][0] > x) bbox[0][0] = x;
    if (bbox[0][1] < x) bbox[0][1] = x;
    if (bbox[1][0] > y) bbox[1][0] = y;
    if (bbox[1][1] < y) bbox[1][1] = y;
    if (bbox[2][0] > z) bbox[2][0] = z;
    if (bbox[2][1] < z) bbox[2][1] = z;

    x = m[0] * bb[0][1] + m[4] * bb[1][1] +  m[8] * bb[2][1] + m[12];
    y = m[1] * bb[0][1] + m[5] * bb[1][1] +  m[9] * bb[2][1] + m[13];
    z = m[2] * bb[0][1] + m[6] * bb[1][1] + m[10] * bb[2][1] + m[14];
    if (bbox[0][0] > x) bbox[0][0] = x;
    if (bbox[0][1] < x) bbox[0][1] = x;
    if (bbox[1][0] > y) bbox[1][0] = y;
    if (bbox[1][1] < y) bbox[1][1] = y;
    if (bbox[2][0] > z) bbox[2][0] = z;
    if (bbox[2][1] < z) bbox[2][1] = z;

    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
            bb[i][j] = bbox[i][j];
}

static int CGNSbounds (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    float bbox[3][2], matrix[16];
    int n, all = 0;
    CONST char **args;
    char sbb[65];

    if (argc > 1) all = atoi(argv[1]);
    get_bounds (all, bbox);
    if (argc > 2) {
        if (TCL_OK != Tcl_SplitList (interp, argv[2], &n, &args))
            return TCL_ERROR;
        for (n = 0; n < 16; n++)
            matrix[n] = (float) atof (args[n]);
        Tcl_Free ((char *)args);
        transform_bounds (matrix, bbox);
    }
    Tcl_ResetResult (interp);
    for (n = 0; n < 3; n++) {
        sprintf (sbb, "%f %f", bbox[n][0], bbox[n][1]);
        Tcl_AppendElement (interp, sbb);
    }
    return TCL_OK;
}

/*---------- OGLregion ---------------------------------------------
 * create OGL display list for region
 *------------------------------------------------------------------*/

static int OGLregion (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int zone, regn, nc;
    CONST char **args;
    Zone *z;
    Regn *r;
    static char slist[17];

    if (argc != 5) {
        Tcl_SetResult (interp, "usage: OGLregion zone region mode color",
            TCL_STATIC);
        return TCL_ERROR;
    }
    zone = atoi (argv[1]);
    if (zone < 0 || zone >= nzones) {
        Tcl_SetResult (interp, "zone number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    z = &zones[zone];
    regn = atoi (argv[2]);
    if (regn < 0 || regn >= z->nregs) {
        Tcl_SetResult (interp, "region number out of range", TCL_STATIC);
        return TCL_ERROR;
    }
    r = &z->regs[regn];

    if (r->nfaces) {
        r->mode = atoi (argv[3]);
        if (TCL_OK != Tcl_SplitList (interp, argv[4], &nc, &args))
            return TCL_ERROR;
        if (nc != 3) {
            Tcl_Free ((char *)args);
            Tcl_SetResult (interp, "invalid color", TCL_STATIC);
            return TCL_ERROR;
        }
        for (nc = 0; nc < 3; nc++)
            r->color[nc] = atof (args[nc]);
        r->color[3] = 1.0;
        Tcl_Free ((char *)args);

        if (!r->dlist) r->dlist = glGenLists (1);
        glNewList (r->dlist, GL_COMPILE);
        glColor3fv (r->color);
        glMaterialfv (GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, r->color);
        switch (r->mode) {
            case 1:
                draw_outlines (z, r);
                break;
            case 2:
                draw_mesh (z, r);
                break;
            case 3:
                draw_shaded (z, r);
                break;
            default:
                r->mode = 0;
                break;
        }
        glEndList ();
    }

    sprintf (slist, "%d", r->dlist);
    Tcl_SetResult (interp, slist, TCL_STATIC);
    return TCL_OK;
}

/*---------- OGLaxis -----------------------------------------------
 * create OGL display list for axis
 *------------------------------------------------------------------*/

#define CHAR_W 8
#define CHAR_H 13

static GLubyte x_raster[] = {
    0x00, 0x00, 0xc3, 0x66, 0x66, 0x3c, 0x3c, 0x18,
    0x3c, 0x3c, 0x66, 0x66, 0xc3
};
static GLubyte y_raster[] = {
    0x00, 0x00, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
    0x3c, 0x3c, 0x66, 0x66, 0xc3
};
static GLubyte z_raster[] = {
    0x00, 0x00, 0xff, 0xc0, 0xc0, 0x60, 0x30, 0x7e,
    0x0c, 0x06, 0x03, 0x03, 0xff
};

static int OGLaxis (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int vis;
    float bbox[3][2];
    static char slist[17];

    if (argc < 2 || argc > 3) {
        Tcl_SetResult (interp, "usage: OGLaxis visible [bounds]",
            TCL_STATIC);
        return TCL_ERROR;
    }
    vis = atoi (argv[1]);
    if (!AxisDL) AxisDL = glGenLists (1);

    glNewList (AxisDL, GL_COMPILE);
    if (vis) {
        if (argc == 3) {
            int nb, n = 0;
            CONST char **args;
            if (TCL_OK != Tcl_SplitList (interp, argv[2], &nb, &args))
                return TCL_ERROR;
            if (nb == 3) {
                for (n = 0; n < nb; n++) {
                    if (sscanf (args[n], "%f %f", &bbox[n][0], &bbox[n][1]) != 2)
                        break;
                }
            }
            Tcl_Free ((char *)args);
            if (n != 3) {
                Tcl_SetResult (interp, "invalid bounding box", TCL_STATIC);
                return TCL_ERROR;
            }
        }
        else
            get_bounds (0, bbox);
        glLineWidth (3.0);
        glDisable (GL_LIGHTING);
        glShadeModel (GL_FLAT);
        glBegin (GL_LINES);
        glColor3f (1.0, 0.0, 0.0);
        glVertex3f (bbox[0][0], bbox[1][0], bbox[2][0]);
        glVertex3f (bbox[0][1], bbox[1][0], bbox[2][0]);
        glColor3f (1.0, 1.0, 0.0);
        glVertex3f (bbox[0][0], bbox[1][0], bbox[2][0]);
        glVertex3f (bbox[0][0], bbox[1][1], bbox[2][0]);
        glColor3f (0.0, 1.0, 0.0);
        glVertex3f (bbox[0][0], bbox[1][0], bbox[2][0]);
        glVertex3f (bbox[0][0], bbox[1][0], bbox[2][1]);
        glEnd ();
        glLineWidth (1.0);

        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
        glColor3f (1.0, 0.0, 0.0);
        glRasterPos3f (bbox[0][1], bbox[1][0], bbox[2][0]);
        glBitmap (CHAR_W, CHAR_H, -0.5 * (float)CHAR_W,
            0.5 * (float)CHAR_H, 0.0, 0.0, x_raster);
        glColor3f (1.0, 1.0, 0.0);
        glRasterPos3f (bbox[0][0], bbox[1][1], bbox[2][0]);
        glBitmap (CHAR_W, CHAR_H, -0.5 * (float)CHAR_W,
            0.5 * (float)CHAR_H, 0.0, 0.0, y_raster);
        glColor3f (0.0, 1.0, 0.0);
        glRasterPos3f (bbox[0][0], bbox[1][0], bbox[2][1]);
        glBitmap (CHAR_W, CHAR_H, -0.5 * (float)CHAR_W,
            0.5 * (float)CHAR_H, 0.0, 0.0, z_raster);
    }
    glEndList ();

    sprintf (slist, "%d", AxisDL);
    Tcl_SetResult (interp, slist, TCL_STATIC);
    return TCL_OK;
}

/*==============================================================
 * cutting plane routines
 *==============================================================*/

#ifndef NO_CUTTING_PLANE

/*------------------------------------------------------------------*/

static void init_cutplane (
#ifdef PROTOTYPE
    float plane[4])
#else
    plane)
float plane[4];
#endif
{
    int nz, nr;

    for (nz = 0; nz < nzones; nz++) {
        for (nr = 0; nr < zones[nz].nregs; nr++) {
            if (zones[nz].regs[nr].cut.nelems) {
                free (zones[nz].regs[nr].cut.elems);
                zones[nz].regs[nr].cut.nelems = 0;
            }
            if (zones[nz].regs[nr].cut.nedges) {
                free (zones[nz].regs[nr].cut.edges);
                zones[nz].regs[nr].cut.nedges = 0;
            }
        }
    }
    if (cutplane.nnodes) free (cutplane.nodes);
    cutplane.nelems = 0;
    cutplane.nedges = 0;
    cutplane.nnodes = 0;
    for (nr = 0; nr < 4; nr++)
        cutplane.plane[nr] = plane[nr];
}

/*------------------------------------------------------------------*/

static int classify_element (
#ifdef PROTOTYPE
    Zone *z, int nnodes, int *nodeid)
#else
    z, nnodes, nodeid)
Zone *z;
int nnodes, *nodeid;
#endif
{
    int n, index = 0;
    int mask = (1 << (nnodes - 1)) - 1;
    float *node;
    double s;

    for (n = 0; n < nnodes; n++) {
        node = z->nodes[nodeid[n]];
        s = node[0] * cutplane.plane[0] + node[1] * cutplane.plane[1] +
            node[2] * cutplane.plane[2] - cutplane.plane[3];
        if (s >= 0.0) index |= (1 << n);
    }
    if (index > mask) index ^= ((mask << 1) | 1);
    return index;
}

/*------------------------------------------------------------------*/

static int find_elements ()
{
#define ELEM_INC 50
    int nz, nr, n, ne, nnodes, nn;
    int maxelems, nelems, *elems;
    ElementType_t type;
    Zone *z;
    Regn *r;

    for (nz = 0; nz < nzones; nz++) {
        z = &zones[nz];
        for (nr = 0; nr < z->nregs; nr++) {
            r = &z->regs[nr];
            if (r->dim < 2 || (r->mode == 0 && !ignorevis)) continue;
            type = r->elemtype;
            cg_npe(type, &nnodes);
            maxelems = nelems = 0;
            elems = NULL;

            for (n = 0, ne = 0; ne < r->nelems; ne++) {
                if (r->elemtype == MIXED) {
                    type = (ElementType_t)r->elems[n++];
                    cg_npe(type, &nnodes);
                }
                switch (type) {
                    case TRI_3:
                    case TRI_6:
                        nn = 3;
                        break;
                    case QUAD_4:
                    case QUAD_8:
                    case QUAD_9:
                    case TETRA_4:
                    case TETRA_10:
                        nn = 4;
                        break;
                    case PYRA_5:
                    case PYRA_14:
                        nn = 5;
                        break;
                    case PENTA_6:
                    case PENTA_15:
                    case PENTA_18:
                        nn = 6;
                        break;
                    case HEXA_8:
                    case HEXA_20:
                    case HEXA_27:
                        nn = 8;
                        break;
                    default:
                        nn = 0;
                        break;
                }
                if (nn && classify_element(z, nn, &r->elems[n])) {
                    if (nelems >= maxelems) {
                        maxelems += ELEM_INC;
                        elems = (int *) REALLOC ("find_elements",
                            maxelems * sizeof(int), elems);
                    }
                    if (r->elemtype == MIXED)
                        elems[nelems] = n - 1;
                    else
                        elems[nelems] = n;
                    nelems++;
                }
                n += nnodes;
            }
            r->cut.nelems = nelems;
            r->cut.elems = elems;
            cutplane.nelems += nelems;
        }
    }

    return cutplane.nelems;
}

/*-------------------------------------------------------------------*/

static int compare_cut_node (
#ifdef PROTOTYPE
    void *v1, void *v2)
#else
    v1, v2)
void *v1, *v2;
#endif
{
    int i;
    CutNode *c1 = (CutNode *)v1;
    CutNode *c2 = (CutNode *)v2;

    for (i = 0; i < 3; i++) {
        if (c1->nodes[i] != c2->nodes[i])
            return (c1->nodes[i] - c2->nodes[i]);
    }
    return 0;
}

/*-------------------------------------------------------------------*/

static unsigned hash_cut_node (
#ifdef PROTOTYPE
    void *v)
#else
    v)
void *v;
#endif
{
    CutNode *c = (CutNode *)v;

    return ((unsigned)(c->nodes[0] + c->nodes[1] + c->nodes[2]));
}

/*-------------------------------------------------------------------*/

static void get_cut_node (
#ifdef PROTOTYPE
    void *v)
#else
    v)
void *v;
#endif
{
    int i;
    CutNode *c = (CutNode *)v;
    Zone *z = &zones[c->nodes[0]];
    float *n, *n1, *n2;

    n  = cutplane.nodes[c->id];
    n1 = z->nodes[c->nodes[1]];
    n2 = z->nodes[c->nodes[2]];

    for (i = 0; i < 3; i++)
        n[i] = n1[i] + c->ratio * (n2[i] - n1[i]);
    free (c);
}

/*-------------------------------------------------------------------*/

static int get_cut_edge (
#ifdef PROTOTYPE
    void *ve, void *vc)
#else
    ve, vc)
void *ve, *vc;
#endif
{
    Edge *e = (Edge *)ve;
    CutData *c = (CutData *)vc;

    c->edges[c->nedges].nodes[0] = e->nodes[0];
    c->edges[c->nedges].nodes[1] = e->nodes[1];
    (c->nedges)++;
    return 1;
}

/*----- tri elements -----*/

#define TRI_SIZE  3
#define TRI_EDGES 3

static int triCuts[TRI_SIZE+1][4] = {
    {0},
    {2,0,2, 0},
    {2,0,1, 0},
    {2,1,2, 0}
};
static int triEdges[TRI_EDGES][2] = {
    {0,1},
    {1,2},
    {2,0}
};

/*----- quad elements -----*/

#define QUAD_SIZE  7
#define QUAD_EDGES 4

static int quadCuts[QUAD_SIZE+1][4] = {
    {0},
    {2,0,3, 0},
    {2,0,1, 0},
    {2,1,3, 0},
    {2,1,2, 0},
    {2,0,3, 0},
    {2,0,2, 0},
    {2,2,3, 0}
};
static int quadEdges[QUAD_EDGES][2] = {
    {0,1},
    {1,2},
    {2,3},
    {3,0}
};

/*----- tet elements -----*/

#define TET_SIZE  7
#define TET_EDGES 6

static int tetCuts[TET_SIZE+1][6] = {
    {0},
    {3,0,3,2, 0},
    {3,0,1,4, 0},
    {4,1,4,3,2, 0},
    {3,1,2,5, 0},
    {4,0,3,5,1, 0},
    {4,0,2,5,4, 0},
    {3,3,5,4, 0}
};
static int tetEdges[TET_EDGES][2] = {
    {0,1},
    {1,2},
    {2,0},
    {0,3},
    {1,3},
    {2,3}
};

/*----- pyramid elements -----*/

#define PYR_SIZE  15
#define PYR_EDGES 8

static int pyrCuts[PYR_SIZE+1][9] = {
    {0},
    {3,0,4,3, 0},
    {3,0,1,5, 0},
    {4,1,5,4,3, 0},
    {3,1,2,6, 0},
    {3,0,4,3, 3,1,2,6, 0},
    {4,0,2,6,5, 0},
    {5,3,2,6,5,4, 0},
    {3,2,3,7, 0},
    {4,0,4,7,2, 0},
    {3,2,3,7, 3,0,1,5, 0},
    {5,1,5,4,7,2, 0},
    {4,1,3,7,6, 0},
    {5,0,4,7,6,1, 0},
    {5,0,3,7,6,5, 0},
    {4,4,7,6,5, 0},
};
static int pyrEdges[PYR_EDGES][2] = {
    {0,1},
    {1,2},
    {2,3},
    {3,0},
    {0,4},
    {1,4},
    {2,4},
    {3,4}
};

/*----- wedge elements -----*/

#define WDG_SIZE  31
#define WDG_EDGES 9

static int wdgCuts[WDG_SIZE+1][10] = {
    {0},
    {3,0,3,2, 0},
    {3,0,1,4, 0},
    {4,1,4,3,2, 0},
    {3,1,2,5, 0},
    {4,0,3,5,1, 0},
    {4,0,2,5,4, 0},
    {3,3,5,4, 0},
    {3,3,6,8, 0},
    {4,0,6,8,2, 0},
    {3,3,6,8, 3,0,1,4, 0},
    {5,1,4,6,8,2, 0},
    {3,3,6,8, 3,1,2,5, 0},
    {5,0,6,8,5,1, 0},
    {3,3,6,8, 4,0,2,5,4, 0},
    {4,4,6,8,5, 0},
    {3,4,7,6, 0},
    {3,4,7,6, 3,0,3,2, 0},
    {4,0,1,7,6, 0},
    {5,1,7,6,3,2, 0},
    {3,4,7,6, 3,1,2,5, 0},
    {3,4,7,6, 4,0,3,5,1, 0},
    {5,0,2,5,7,6, 0},
    {4,3,5,7,6, 0},
    {4,3,4,7,8, 0},
    {5,0,4,7,8,2, 0},
    {5,0,1,7,8,3, 0},
    {4,1,7,8,2, 0},
    {4,3,4,7,8, 3,1,2,5, 0},
    {3,0,4,1, 3,5,7,8, 0},
    {3,0,2,3, 3,5,7,8, 0},
    {3,5,7,8, 0}
};
static int wdgEdges[WDG_EDGES][2] = {
    {0,1},
    {1,2},
    {2,0},
    {0,3},
    {1,4},
    {2,5},
    {3,4},
    {4,5},
    {5,3}
};

/*----- hex elements -----*/

#define HEX_SIZE  127
#define HEX_EDGES 12

static int hexCuts[HEX_SIZE+1][17] = {
    {0},
    {3,3,0,4, 0},
    {3,0,1,5, 0},
    {4,3,1,5,4, 0},
    {3,1,2,6, 0},
    {3,3,0,4, 3,1,2,6, 0},
    {4,0,2,6,5, 0},
    {5,3,2,6,5,4, 0},
    {3,3,7,2, 0},
    {4,0,4,7,2, 0},
    {3,0,1,5, 3,3,7,2, 0},
    {5,4,7,2,1,5, 0},
    {4,3,7,6,1, 0},
    {5,0,4,7,6,1, 0},
    {5,0,3,7,6,5, 0},
    {4,7,6,5,4, 0},
    {3,4,11,8, 0},
    {4,0,8,11,3, 0},
    {3,0,1,5, 3,4,11,8, 0},
    {5,3,11,8,5,1, 0},
    {3,1,2,6, 3,4,11,8, 0},
    {4,0,8,11,3, 3,1,2,6, 0},
    {4,0,2,6,5, 3,4,11,8, 0},
    {6,3,2,6,5,8,11, 0},
    {3,3,7,2, 3,4,11,8, 0},
    {5,0,2,7,11,8, 0},
    {3,0,1,5, 3,3,7,2, 3,4,11,8, 0},
    {6,2,1,5,8,11,7, 0},
    {4,3,7,6,1, 3,4,11,8, 0},
    {6,0,8,11,7,6,1, 0},
    {5,0,3,7,6,5, 3,4,11,8, 0},
    {5,11,7,6,5,8, 0},
    {3,8,9,5, 0},
    {3,8,9,5, 3,3,0,4, 0},
    {4,0,8,9,1, 0},
    {5,4,3,1,9,8, 0},
    {3,8,9,5, 3,1,2,6, 0},
    {3,1,9,5, 3,3,0,4, 3,1,2,6, 0},
    {5,0,2,6,9,8, 0},
    {6,3,2,6,9,8,4, 0},
    {3,8,9,5, 3,3,7,2, 0},
    {4,0,4,7,2, 3,8,9,5, 0},
    {4,0,8,9,1, 3,3,7,2, 0},
    {6,4,7,2,1,9,8, 0},
    {4,3,7,6,1, 3,8,9,5, 0},
    {5,7,6,1,0,4, 3,8,9,5, 0},
    {6,0,3,7,6,9,8, 0},
    {5,4,7,6,9,8, 0},
    {4,4,11,9,5, 0},
    {5,0,3,11,9,5, 0},
    {5,0,4,11,9,1, 0},
    {4,3,1,9,11, 0},
    {4,4,11,9,5, 3,1,2,6, 0},
    {6,0,3,11,9,6,2, 0},
    {6,0,2,6,9,11,4, 0},
    {5,3,2,6,9,11, 0},
    {4,4,11,9,5, 3,3,7,2, 0},
    {6,0,2,7,11,9,5, 0},
    {5,11,9,1,0,4, 3,3,7,2, 0},
    {5,11,7,2,1,9, 0},
    {4,3,7,6,1, 4,4,11,9,5, 0},
    {4,11,7,6,9, 3,0,1,5, 0},
    {4,11,7,6,9, 3,3,0,4, 0},
    {4,11,7,6,9, 0},
    {3,9,10,6, 0},
    {3,9,10,6, 3,3,0,4, 0},
    {3,9,10,6, 3,0,1,5, 0},
    {4,4,3,1,5, 3,9,10,6, 0},
    {4,2,1,9,10, 0},
    {4,2,1,9,10, 3,3,0,4, 0},
    {5,0,2,10,9,5, 0},
    {6,3,2,10,9,5,4, 0},
    {3,3,7,2, 3,9,10,6, 0},
    {4,7,2,0,4, 3,9,10,6, 0},
    {3,0,1,5, 3,2,3,7, 3,9,10,6, 0},
    {4,4,7,10,9, 4,1,2,6,5, 0},
    {5,3,7,10,9,1, 0},
    {6,4,7,10,9,1,0, 0},
    {6,3,7,10,9,5,0, 0},
    {5,4,7,10,9,5, 0},
    {3,4,11,8, 3,9,10,6, 0},
    {4,0,8,11,3, 3,9,10,6, 0},
    {3,0,1,5, 3,4,11,8, 3,9,10,6, 0},
    {5,3,11,10,6,1, 3,1,9,5, 0},
    {4,1,2,10,9, 3,4,11,8, 0},
    {4,3,11,10,2, 4,0,8,9,1, 0},
    {5,4,11,10,2,0, 3,8,9,5, 0},
    {4,3,11,10,2, 3,8,9,5, 0},
    {3,3,7,2, 3,9,10,6, 3,4,11,8, 0},
    {5,0,2,6,9,8, 3,11,7,10, 0},
    {3,0,4,3, 3,11,7,10, 3,1,2,6, 3,8,9,5, 0},
    {3,1,9,5, 3,11,7,10, 3,1,2,6, 0},
    {5,3,1,9,8,4, 3,11,7,10, 0},
    {4,0,8,9,1, 3,11,7,10, 0},
    {3,11,7,10, 3,8,9,5, 3,3,0,4, 0},
    {3,11,7,10, 3,8,9,5, 0},
    {4,8,10,6,5, 0},
    {4,8,10,6,5, 3,3,0,4, 0},
    {5,0,8,10,6,1, 0},
    {6,3,4,8,10,6,1, 0},
    {5,10,2,1,5,8, 0},
    {5,3,2,10,8,4, 3,0,1,5, 0},
    {4,0,8,10,2, 0},
    {5,3,2,10,8,4, 0},
    {4,8,10,6,5, 3,3,7,2, 0},
    {4,4,7,10,8, 4,0,2,6,5, 0},
    {5,3,7,10,8,0, 3,1,2,6, 0},
    {4,4,7,10,8, 3,1,2,6, 0},
    {6,3,7,10,8,5,1, 0},
    {4,4,7,10,8, 3,0,1,5, 0},
    {5,3,7,10,8,0, 0},
    {4,4,7,10,8, 0},
    {5,4,11,10,6,5, 0},
    {6,0,3,11,10,6,5, 0},
    {6,4,11,10,6,1,0, 0},
    {5,3,11,10,6,1, 0},
    {6,4,11,10,2,1,5, 0},
    {4,3,11,10,2, 3,0,1,5, 0},
    {5,4,11,10,2,0, 0},
    {4,11,10,2,3, 0},
    {5,3,2,6,5,4, 3,11,7,10, 0},
    {4,0,2,6,5, 3,11,7,10, 0},
    {3,3,0,4, 3,1,2,6, 3,11,7,10, 0},
    {3,1,2,6, 3,11,7,10, 0},
    {4,4,3,1,5, 3,11,7,10, 0},
    {3,0,1,5, 3,11,7,10, 0},
    {3,11,7,10, 3,3,0,4, 0},
    {3,11,7,10, 0}
};
static int hexEdges[HEX_EDGES][2] = {
    {0,1},
    {1,2},
    {2,3},
    {3,0},
    {0,4},
    {1,5},
    {2,6},
    {3,7},
    {4,5},
    {5,6},
    {6,7},
    {7,4}
};

static int n_cut_nodes;
static HASH *cut_hash;

/*------------------------------------------------------------------*/

static void intersect_element (
#ifdef PROTOTYPE
    int zonenum, ElementType_t elemtype, int *nodeid, HASH *edgehash)
#else
    zonenum, elemtype, nodeid, edgehash)
int zonenum, *nodeid;
ElementType_t elemtype;
HASH *edgehash;
#endif
{
    int i, n, index, nn, nc, *cuts;
    int edgemap[HEX_EDGES][2], ids[7];
    double s1, s2;
    float *node;
    CutNode cnode, *cn;
    Edge edge, *ep;
    Zone *z = &zones[zonenum];
    static char *funcname = "intersect_element";

    /* get intersection lookup table */

    switch (elemtype) {
        case TRI_3:
        case TRI_6:
            index = classify_element(z, 3, nodeid);
            if (index < 1 || index > TRI_SIZE) return;
            cuts = triCuts[index];
            for (n = 0; n < TRI_EDGES; n++) {
                edgemap[n][0] = triEdges[n][0];
                edgemap[n][1] = triEdges[n][1];
            }
            break;
        case QUAD_4:
        case QUAD_8:
        case QUAD_9:
            index = classify_element(z, 4, nodeid);
            if (index < 1 || index > QUAD_SIZE) return;
            cuts = quadCuts[index];
            for (n = 0; n < QUAD_EDGES; n++) {
                edgemap[n][0] = quadEdges[n][0];
                edgemap[n][1] = quadEdges[n][1];
            }
            break;
        case TETRA_4:
        case TETRA_10:
            index = classify_element(z, 4, nodeid);
            if (index < 1 || index > TET_SIZE) return;
            cuts = tetCuts[index];
            for (n = 0; n < TET_EDGES; n++) {
                edgemap[n][0] = tetEdges[n][0];
                edgemap[n][1] = tetEdges[n][1];
            }
            break;
        case PYRA_5:
        case PYRA_14:
            index = classify_element(z, 5, nodeid);
            if (index < 1 || index > PYR_SIZE) return;
            cuts = pyrCuts[index];
            for (n = 0; n < PYR_EDGES; n++) {
                edgemap[n][0] = pyrEdges[n][0];
                edgemap[n][1] = pyrEdges[n][1];
            }
            break;
        case PENTA_6:
        case PENTA_15:
        case PENTA_18:
            index = classify_element(z, 6, nodeid);
            if (index < 1 || index > WDG_SIZE) return;
            cuts = wdgCuts[index];
            for (n = 0; n < WDG_EDGES; n++) {
                edgemap[n][0] = wdgEdges[n][0];
                edgemap[n][1] = wdgEdges[n][1];
            }
            break;
        case HEXA_8:
        case HEXA_20:
        case HEXA_27:
            index = classify_element(z, 8, nodeid);
            if (index < 1 || index > HEX_SIZE) return;
            cuts = hexCuts[index];
            for (n = 0; n < HEX_EDGES; n++) {
                edgemap[n][0] = hexEdges[n][0];
                edgemap[n][1] = hexEdges[n][1];
            }
            break;
        default:
            return;
    }

    /* get the edge intersections */

    for (nc = 0; cuts[nc];) {
        nn = cuts[nc];
        for (n = 1; n <= nn; n++) {

            /* get edge nodes */

            cnode.nodes[0] = zonenum;
            cnode.nodes[1] = nodeid[edgemap[cuts[nc+n]][0]];
            cnode.nodes[2] = nodeid[edgemap[cuts[nc+n]][1]];
            if (cnode.nodes[2] < cnode.nodes[1]) {
                index = cnode.nodes[1];
                cnode.nodes[1] = cnode.nodes[2];
                cnode.nodes[2] = index;
            }
            cn = (CutNode *) HashFind (cut_hash, &cnode);

            /* add node to hash table if not there */

            if (NULL == cn) {
                cn = (CutNode *) MALLOC (funcname, sizeof(CutNode));
                cn->id = n_cut_nodes++;
                for (i = 0; i < 3; i++)
                    cn->nodes[i] = cnode.nodes[i];
                node = z->nodes[cn->nodes[1]];
                for (s1 = 0.0, i = 0; i < 3; i++)
                    s1 += (node[i] * cutplane.plane[i]);
                node = z->nodes[cn->nodes[2]];
                for (s2 = 0.0, i = 0; i < 3; i++)
                    s2 += (node[i] * cutplane.plane[i]);
                if (s1 == s2)
                    cn->ratio = 0.0;
                else
                    cn->ratio = (cutplane.plane[3] - s1) / (s2 - s1);
                (void) HashAdd (cut_hash, cn);
            }
            ids[n-1] = cn->id;
        }
        ids[nn] = ids[0];

        /* add cutplane edge */

        for (n = 0; n < nn; n++) {
            edge.nodes[0] = ids[n];
            edge.nodes[1] = ids[n+1];
            ep = (Edge *) HashFind (edgehash, &edge);
            if (NULL == ep) {
                ep = (Edge *) MALLOC (funcname, sizeof(Edge));
                ep->nodes[0] = edge.nodes[0];
                ep->nodes[1] = edge.nodes[1];
                (void) HashAdd (edgehash, ep);
            }
        }

        /* next cut */

        nc += (nn + 1);
    }
}

/*------------------------------------------------------------------*/

static int find_intersects ()
{
    int nz, nr, n, ne, nnodes;
    ElementType_t type;
    Regn *r;
    HASH *edgehash;
    static char *funcname = "find_intersects";

    /* create hash table to store nodes at edge intersections */

    n_cut_nodes = 0;
    cut_hash = HashCreate (cutplane.nelems > 1024 ?
        cutplane.nelems / 3 : 127, compare_cut_node, hash_cut_node);
    cutplane.nedges = 0;

    for (nz = 0; nz < nzones; nz++) {
        for (nr = 0; nr < zones[nz].nregs; nr++) {
            r = &zones[nz].regs[nr];
            if (r->cut.nelems == 0) continue;
            type = r->elemtype;
            cg_npe(type, &nnodes);

            edgehash = HashCreate (r->cut.nelems > 1024 ?
                       r->cut.nelems / 3: 127, compare_edges, hash_edge);

            for (n = 0, ne = 0; ne < r->cut.nelems; ne++) {
                n = r->cut.elems[ne];
                if (r->elemtype == MIXED) {
                    type = (ElementType_t)r->elems[n++];
                    cg_npe(type, &nnodes);
                }
                intersect_element (nz, type, &r->elems[n], edgehash);
            }

            r->cut.nedges = 0;
            ne = HashSize (edgehash);
            if (ne) {
                r->cut.edges = (Edge *) MALLOC (funcname, ne * sizeof(Edge));
                HashList (edgehash, get_cut_edge, &r->cut);
            }
            HashDestroy (edgehash, NULL);
            cutplane.nedges += r->cut.nedges;
        }
    }

    nnodes = HashSize (cut_hash);
    cutplane.nnodes = nnodes;
    cutplane.nodes = (Node *) MALLOC (funcname, nnodes * sizeof(Node));
    HashDestroy (cut_hash, get_cut_node);

    return cutplane.nelems;
}

/*------------------------------------------------------------------*/

static void draw_edges ()
{
    int nz, nr, ne, nn;
    Zone *z;
    Regn *r;

    glDisable(GL_LIGHTING);
    glShadeModel(GL_FLAT);
    glBegin(GL_LINES);
    glColor3fv(cutcolor);

    for (nz = 0; nz < nzones; nz++) {
        z = &zones[nz];
        for (nr = 0; nr < z->nregs; nr++) {
            r = &z->regs[nr];
            if (r->cut.nedges == 0) continue;
            if (!usecutclr) glColor3fv(r->color);
            for (ne = 0; ne < r->cut.nedges; ne++) {
                nn = r->cut.edges[ne].nodes[0];
                glVertex3fv(cutplane.nodes[nn]);
                nn = r->cut.edges[ne].nodes[1];
                glVertex3fv(cutplane.nodes[nn]);
            }
        }
    }

    glEnd();
}

/*------------------------------------------------------------------*/

static void draw_elements (
#ifdef PROTOTYPE
    int mode)
#else
    mode)
int mode;
#endif
{
    int nz, nr, n, i, j, ne;
    int ip, nf, nn, nnodes;
    float *nodes[4];
    ElementType_t type;
    Zone *z;
    Regn *r;

    glEnable(GL_LIGHTING);
    glShadeModel(GL_FLAT);
    glPolygonMode(GL_FRONT_AND_BACK, mode);
    glColor3fv(cutcolor);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, cutcolor);

    for (nz = 0; nz < nzones; nz++) {
        z = &zones[nz];
        for (nr = 0; nr < z->nregs; nr++) {
            r = &z->regs[nr];
            if (r->cut.nelems == 0) continue;
            type = r->elemtype;

            if (!usecutclr) {
                glColor3fv(r->color);
                glMaterialfv(GL_FRONT_AND_BACK,
                    GL_AMBIENT_AND_DIFFUSE, r->color);
            }

            for (ne = 0; ne < r->cut.nelems; ne++) {
                n = r->cut.elems[ne];
                if (r->elemtype == MIXED)
                    type = (ElementType_t)r->elems[n++];
                switch (type) {
                    case TRI_3:
                    case TRI_6:
                        ip = 0;
                        nf = 1;
                        break;
                    case QUAD_4:
                    case QUAD_8:
                    case QUAD_9:
                        ip = 1;
                        nf = 1;
                        break;
                    case TETRA_4:
                    case TETRA_10:
                        ip = 2;
                        nf = 4;
                        break;
                    case PYRA_5:
                    case PYRA_14:
                        ip = 6;
                        nf = 5;
                        break;
                    case PENTA_6:
                    case PENTA_15:
                    case PENTA_18:
                        ip = 11;
                        nf = 5;
                        break;
                    case HEXA_8:
                    case HEXA_20:
                    case HEXA_27:
                        ip = 16;
                        nf = 6;
                        break;
                    default:
                        ip = 0;
                        nf = 0;
                        break;
                }
                for (j = 0; j < nf; j++) {
                    nnodes = facenodes[ip+j][0];
                    for (i = 0; i < nnodes; i++) {
                        nn = r->elems[n+facenodes[ip+j][i+1]];
                        nodes[i] = z->nodes[nn];
                    }
                    if (nnodes == 4) {
                        glBegin(GL_QUADS);
                        glNormal3fv(compute_normal(nodes[0], nodes[1],
                            nodes[2], nodes[3]));
                    }
                    else {
                        glBegin(GL_TRIANGLES);
                        glNormal3fv(compute_normal(nodes[0], nodes[1],
                            nodes[2], NULL));
                    }
                    for (i = 0; i < nnodes; i++)
                        glVertex3fv(nodes[i]);
                    glEnd();
                }
            }
        }
    }
}

/*---------- OGLcutplane -------------------------------------------
 * create OGL display list for a cutting plane
 *------------------------------------------------------------------*/

static int OGLcutplane (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int np, mode;
    float plane[4];
    static char slist[33];

    if (argc < 1 || argc > 3) {
        Tcl_SetResult (interp, "usage: OGLcutplane [mode] [plane]",
            TCL_STATIC);
        return TCL_ERROR;
    }

    /* create and return displaylist flag */

    if (argc == 1) {
        if (!CutDL) CutDL = glGenLists (1);
        sprintf (slist, "%d", CutDL);
        Tcl_SetResult (interp, slist, TCL_STATIC);
        return TCL_OK;
    }

    mode = atoi(argv[1]);

    if (argc == 3) {
        int np;
        CONST char **args;
        if (TCL_OK != Tcl_SplitList (interp, argv[2], &np, &args))
            return TCL_ERROR;
        if (np != 4) {
            Tcl_Free ((char *)args);
            Tcl_SetResult (interp, "invalid plane", TCL_STATIC);
            return TCL_ERROR;
        }
        for (np = 0; np < 4; np++)
            plane[np] = (float) atof (args[np]);
        Tcl_Free ((char *)args);
        init_cutplane(plane);
        find_elements();
    }

    if (!CutDL) CutDL = glGenLists (1);

    glNewList (CutDL, GL_COMPILE);
    if (mode && cutplane.nelems) {
        if (mode == 1) {
            if (cutplane.nedges == 0)
                find_intersects();
            draw_edges ();
        }
        else {
            draw_elements (mode > 2 ? GL_FILL : GL_LINE);
        }
    }
    glEndList ();

    sprintf (slist, "%d %d", cutplane.nelems, cutplane.nedges);
    Tcl_SetResult (interp, slist, TCL_STATIC);
    return TCL_OK;
}

/*---------- OGLdrawplane ------------------------------------------
 * draw the cutting plane
 *------------------------------------------------------------------*/

static int OGLdrawplane (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, np, i, j, k, index, n0, n1;
    CONST char **args;
    float plane[4], bbox[3][2], s[8], ds;
    float node[8][3], pnode[6][3];
    static char slist[17];

    if (argc < 1 || argc > 2) {
        Tcl_SetResult (interp, "usage: OGLdrawplane [plane]",
            TCL_STATIC);
        return TCL_ERROR;
    }

    if (!PlaneDL) PlaneDL = glGenLists (1);

    if (argc == 1) {
      glNewList (PlaneDL, GL_COMPILE);
      glEndList ();
      sprintf (slist, "%d", PlaneDL);
      Tcl_SetResult (interp, slist, TCL_STATIC);
      return TCL_OK;
    }

    if (TCL_OK != Tcl_SplitList (interp, argv[1], &np, &args))
        return TCL_ERROR;
    if (np != 4) {
        Tcl_Free ((char *)args);
        Tcl_SetResult (interp, "invalid plane", TCL_STATIC);
        return TCL_ERROR;
    }
    for (n = 0; n < np; n++)
        plane[n] = (float) atof (args[n]);
    Tcl_Free ((char *)args);

    get_bounds (ignorevis, bbox);
    index = n = 0;
    for (k = 0; k < 2; k++) {
        for (j = 0; j < 2; j++) {
            for (i = 0; i < 2; i++) {
                node[n][0] = bbox[0][(i+j)%2];
                node[n][1] = bbox[1][j];
                node[n][2] = bbox[2][k];
                s[n] = node[n][0] * plane[0] + node[n][1] * plane[1] +
                       node[n][2] * plane[2];
                if (s[n] >= plane[3]) index |= (1 << n);
                n++;
            }
        }
    }
    if (index > 0x7f) index ^= 0xff;
    if (index < 1 || index > HEX_SIZE) {
      Tcl_SetResult (interp, "plane doesn't intersect", TCL_STATIC);
      return TCL_ERROR;
    }

    np = hexCuts[index][0];
    for (n = 0; n < np; n++) {
        j = hexCuts[index][n+1];
        n0 = hexEdges[j][0];
        n1 = hexEdges[j][1];
        ds = s[n1] - s[n0];
        if (ds != 0.0)
            ds = (plane[3] - s[n0]) / ds;
        for (i = 0; i < 3; i++)
            pnode[n][i] = node[n0][i] + ds * (node[n1][i] - node[n0][i]);
    }

    glNewList (PlaneDL, GL_COMPILE);
    glEnable(GL_BLEND);
    glDepthMask(GL_FALSE);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_LIGHTING);
    glShadeModel(GL_FLAT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glColor4fv(cutcolor);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, cutcolor);

    glBegin(GL_TRIANGLE_FAN);
    glNormal3fv(plane);
    for (n = 0; n < np; n++)
        glVertex3fv(pnode[n]);
    glEnd();

    glDepthMask(GL_TRUE);
    glDisable(GL_BLEND);
    glEnable(GL_LIGHTING);
    glEndList ();

    sprintf (slist, "%d", PlaneDL);
    Tcl_SetResult (interp, slist, TCL_STATIC);
    return TCL_OK;
}

/*---------- OGLcutconfig ------------------------------------------
 * set the cutting plane color and operation
 *------------------------------------------------------------------*/

static int OGLcutconfig (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, np;
    CONST char **args;

    if (argc < 2 || argc > 4) {
        Tcl_SetResult (interp, "usage: OGLcutconfig color [usecutclr] [ignorevis]",
            TCL_STATIC);
        return TCL_ERROR;
    }

    if (TCL_OK != Tcl_SplitList (interp, argv[1], &np, &args))
        return TCL_ERROR;
    if (np < 3 || np > 4) {
        Tcl_Free ((char *)args);
        Tcl_SetResult (interp, "invalid color", TCL_STATIC);
        return TCL_ERROR;
    }
    for (n = 0; n < np; n++)
        cutcolor[n] = (float) atof (args[n]);
    Tcl_Free ((char *)args);

    if (argc > 2) {
        usecutclr = atoi (argv[2]);
        if (argc > 3)
            ignorevis = atoi (argv[3]);
    }
    return TCL_OK;
}

#endif

/*---------- Cgnstcl_Init --------------------------------------
 * Initialize and create the commands
 *--------------------------------------------------------------*/

#if defined(_WIN32) && defined(BUILD_DLL)
__declspec(dllexport)
#endif
int Cgnstcl_Init(
#ifdef PROTOTYPE
    Tcl_Interp *interp)
#else
    interp)
Tcl_Interp *interp;
#endif
{
    global_interp = interp;
    Tcl_CreateCommand (interp, "CGNSopen", (Tcl_CmdProc *)CGNSopen,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSclose", (Tcl_CmdProc *)CGNSclose,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSbase", (Tcl_CmdProc *)CGNSbase,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSzone", (Tcl_CmdProc *)CGNSzone,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSsummary", (Tcl_CmdProc *)CGNSsummary,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSgetbase", (Tcl_CmdProc *)CGNSgetbase,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSgetzone", (Tcl_CmdProc *)CGNSgetzone,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSgetregion", (Tcl_CmdProc *)CGNSgetregion,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSregiondim", (Tcl_CmdProc *)CGNSregiondim,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "CGNSbounds", (Tcl_CmdProc *)CGNSbounds,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "OGLregion", (Tcl_CmdProc *)OGLregion,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "OGLaxis", (Tcl_CmdProc *)OGLaxis,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
#ifndef NO_CUTTING_PLANE
    Tcl_CreateCommand (interp, "OGLcutplane", (Tcl_CmdProc *)OGLcutplane,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "OGLdrawplane", (Tcl_CmdProc *)OGLdrawplane,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
    Tcl_CreateCommand (interp, "OGLcutconfig", (Tcl_CmdProc *)OGLcutconfig,
        (ClientData)0, (Tcl_CmdDeleteProc *)0);
#endif
    return TCL_OK;
}

