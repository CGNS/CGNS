/*
 * import routines for CGNS
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "cgnslib.h"
#include "cgnsImport.h"
#include "hash.h"

#ifndef CG_MODE_READ
# define CG_MODE_READ   MODE_READ
# define CG_MODE_WRITE  MODE_WRITE
# define CG_MODE_MODIFY MODE_MODIFY
#endif

#ifndef DOUBLE      /* data type for coordinates */
#define DOUBLE      double
#endif

#ifndef TOLERANCE   /* tolerance for duplicate checking */
#define TOLERANCE   1.e-03
#endif

#ifndef REGION_BASE /* base name for unnamed regions */
#define REGION_BASE "Region"
#endif

/*--- variables ---*/

static int num_vars = 0;
static char **var_names = 0;

/*--- node bit flags ---*/

#define USED_BIT    1
#define REGN_BIT    2
#define REFS_BIT    4

/*--- node structure ---*/

typedef struct {
    int id;         /* node ID */
    int flags;      /* references to node */
    DOUBLE x, y, z; /* coordinates */
    DOUBLE dist;    /* distance from origin */
    DOUBLE *vars;   /* variables */
} cgnsNODE;

/*--- node mapping data ---*/

#define NODEMAP_INC 50  /* realloc this many at a time */

typedef struct {
    int nodeid;     /* node id */
    int mapped;     /* set when mapped */
    cgnsNODE *node;     /* pointer to node data */
} NODEMAP;

static int num_nodes = 0;   /* number of nodes */
static int max_nodes = 0;   /* number of nodes malloced */
static NODEMAP *nodemap;    /* list of nodes */

/*--- duplicate node checking ---*/

#define ENTRY_INC   50

static int no_check = 0;
static double def_tol = TOLERANCE;
static double tolerance = TOLERANCE;
DOUBLE xmin, xmax, ymin, ymax, zmin, zmax;

static int num_entries = 0;
static int max_entries = 0;
static cgnsNODE **node_list;

/*--- element data ---*/

#define ELEMENT_INC 50  /* realloc this many at a time */

typedef struct {
    int elemid;     /* element ID */
    int elemtype;   /* element type (number of nodes) */
    int *nodeid;    /* node ID's for element */
    char facemap[6];/* remapping of faces */
} cgnsELEM;

static int num_elements = 0;/* number of elements */
static int max_elements = 0;/* number of elements malloced */
static cgnsELEM *elemlist;   /* list of elements */

/*--- region data ---*/

#define REGION_INC  50          /* step increment for realloc */

static char region_name[33];    /* region name */
static int region_type;         /* type of region */
static int region_id = 0;       /* region ID */
static int region_max = 0;      /* malloced size of region_list */
static int region_nobjs = 0;    /* number objects in region */
static int *region_list;        /* list of nodes in region */

typedef struct {
    char name[33];  /* region name */
    int type;       /* region type */
    int nobjs;      /* number of objects */
    int *objid;     /* object ID's */
} cgnsREGN;

static int num_regions = 0; /* number of regions */
static cgnsREGN *reglist;     /* list of regions */

/*--- external faces */

typedef struct  {
    int faceid;
    int nnodes;
    int nodeid[4];
    int flags;
} cgnsFACE;

static int num_faces = 0;
static cgnsFACE **facelist;

/*--- CGNS data ---*/

int cgnsFile = 0;
int cgnsBase = 0;
int cgnsZone = 0;
char cgnsZoneName[33] = "";

/*--- error handling callback ---*/

static void (*errcallback)( /* callback pointer to user routine */
#ifdef PROTOTYPE
    char *errmsg            /* error message */
#endif
) = NULL;

/*======================================================================
 * Node routines
 *======================================================================*/

/*---------- NewNode ---------------------------------------------------
 * create a new node
 *----------------------------------------------------------------------*/

static cgnsNODE *NewNode (id, x, y, z)
int id;
double x, y, z;
{
    cgnsNODE *node = (cgnsNODE *) malloc (sizeof(cgnsNODE));

    if (NULL != node) {
        node->id = id;
        node->flags = 0;
        node->x = x;
        node->y = y;
        node->z = z;
        node->dist = 0.0;
        node->vars = 0;
        if (num_vars)
            node->vars = (DOUBLE *) calloc (num_vars, sizeof(DOUBLE));
    }
    return (node);
}

/*---------- GetNode ---------------------------------------------------
 * return the node for a given node id
 *----------------------------------------------------------------------*/

static cgnsNODE *GetNode (nodeid, pos)
int nodeid, *pos;
{
    int lo = 0, hi = num_nodes - 1;

    *pos = 0;
    if (!num_nodes || nodeid < nodemap[0].nodeid)
        return (NULL);
    if (nodeid == nodemap[0].nodeid)
        return (nodemap[0].node);
    if (!hi || nodeid > nodemap[hi].nodeid) {
        *pos = num_nodes;
        return (NULL);
    }
    if (nodeid == nodemap[hi].nodeid) {
        *pos = hi;
        return (nodemap[hi].node);
    }

    while (1) {
        *pos = (lo + hi) >> 1;
        if (nodeid == nodemap[*pos].nodeid)
            return (nodemap[*pos].node);
        if (hi - lo <= 1)
            break;
        if (nodeid < nodemap[*pos].nodeid)
            hi = *pos;
        else
            lo = *pos;
    }
    *pos = hi;
    return (NULL);
}

/*---------- CompareNodes ----------------------------------------------
 * compare two nodes, returns 0 if nodes are the same within
 * the specifed tolerance, else 1
 *----------------------------------------------------------------------*/

static int CompareNodes (node1, node2)
cgnsNODE *node1, *node2;
{
    double dist = (node2->x - node1->x) * (node2->x - node1->x) +
                  (node2->y - node1->y) * (node2->y - node1->y) +
                  (node2->z - node1->z) * (node2->z - node1->z);
    return (dist < (tolerance * tolerance) ? 0 : 1);
}

/*======================================================================
  duplicate node checking routines

The nodes are stored in a sorted list based on radius from the origin.
Once the position in the list is determined, then a scan backwards and
forwards in the list is done for a matching node.
========================================================================*/

/*---------- FindPosition ----------------------------------------------
 * bisection search to locate position for node
 *----------------------------------------------------------------------*/

static int FindPosition (node)
cgnsNODE *node;
{
    int mid, lo = 0, hi = num_entries - 1;

    if (!num_entries || node->dist <= node_list[0]->dist)
        return (0);

    if (!hi || node->dist > node_list[hi]->dist)
        return (num_entries);
    if (node->dist == node_list[hi]->dist)
        return (hi);

    while ((hi - lo) > 1) {
        mid = (lo + hi) >> 1;
        if (node->dist == node_list[mid]->dist)
            return (mid);
        if (node->dist < node_list[mid]->dist)
            hi = mid;
        else
            lo = mid;
    }
    return (hi);
}

/*---------- FindNode --------------------------------------------------
 * search for matching node in Node List
 *----------------------------------------------------------------------*/

static cgnsNODE *FindNode (node, pos)
cgnsNODE *node;
int *pos;
{
    int n;

    *pos = FindPosition (node);

    for (n = *pos - 1; n >= 0; n--) {
        if (fabs (node->dist - node_list[n]->dist) >= tolerance)
            break;
        if (!CompareNodes (node, node_list[n]))
            return (node_list[n]);
    }
    for (n = *pos; n < num_entries; n++) {
        if (fabs (node->dist - node_list[n]->dist) >= tolerance)
            break;
        if (!CompareNodes (node, node_list[n]))
            return (node_list[n]);
    }
    return (NULL);
}

/*---------- AddNode ---------------------------------------------------
 * add a new node to the duplicate node checking list
 *----------------------------------------------------------------------*/

static void AddNode (node, pos)
cgnsNODE *node;
int pos;
{
    int n;

    if (num_entries == max_entries) {
        if (!max_entries)
            node_list = (cgnsNODE **) malloc (ENTRY_INC * sizeof(cgnsNODE *));
        else
            node_list = (cgnsNODE **) realloc (node_list,
                (max_entries + ENTRY_INC) * sizeof(cgnsNODE *));
        if (NULL == node_list)
            cgnsImportFatal (
            "AddNode:malloc failed for new node entry in duplicate node list");
        max_entries += ENTRY_INC;
    }
    for (n = num_entries; n > pos; n--)
        node_list[n] = node_list[n-1];
    node_list[pos] = node;
    num_entries++;
}

/*======================================================================
 * Element routines
 *======================================================================*/

/*---------- GetElement ------------------------------------------------
 * return the element for a given element id
 *----------------------------------------------------------------------*/

static cgnsELEM *GetElement (elemid, pos)
int elemid, *pos;
{
    int lo = 0, hi = num_elements - 1;

    *pos = 0;
    if (!num_elements || elemid < elemlist[0].elemid)
        return (NULL);
    if (elemid == elemlist[0].elemid)
        return (&elemlist[0]);
    if (!hi || elemid > elemlist[hi].elemid) {
        *pos = num_elements;
        return (NULL);
    }
    if (elemid == elemlist[hi].elemid) {
        *pos = hi;
        return (&elemlist[hi]);
    }

    while (1) {
        *pos = (lo + hi) >> 1;
        if (elemid == elemlist[*pos].elemid)
            return (&elemlist[*pos]);
        if (hi - lo <= 1)
            break;
        if (elemid < elemlist[*pos].elemid)
            hi = *pos;
        else
            lo = *pos;
    }
    *pos = hi;
    return (NULL);
}

/*---------- NewElement ------------------------------------------------
 * add new element to list of elements
 *----------------------------------------------------------------------*/

static cgnsELEM *NewElement (pos)
int pos;
{
    int n, i;

    /* malloc/realloc if needed */

    if (num_elements == max_elements) {
        if (!max_elements)
            elemlist = (cgnsELEM *) malloc (ELEMENT_INC * sizeof(cgnsELEM));
        else
            elemlist = (cgnsELEM *) realloc (elemlist,
                (max_elements + ELEMENT_INC) * sizeof(cgnsELEM));
        if (NULL == elemlist)
            cgnsImportFatal ("AddElement:malloc failed for element list");
        max_elements += ELEMENT_INC;
    }

    /* insert new element */

    for (n = num_elements; n > pos; n--) {
        elemlist[n].elemid   = elemlist[n-1].elemid;
        elemlist[n].elemtype = elemlist[n-1].elemtype;
        elemlist[n].nodeid   = elemlist[n-1].nodeid;
        for (i = 0; i < 6; i++)
            elemlist[n].facemap[i] = elemlist[n-1].facemap[i];
    }
    num_elements++;
    return (&elemlist[pos]);
}

/*======================================================================
 * Region routines
 *======================================================================*/

/*---------- GetRegion -------------------------------------------------
 * return a region for a given region name
 *----------------------------------------------------------------------*/

static cgnsREGN *GetRegion (name, pos)
char *name;
int *pos;
{
    int cmp, lo = 0, hi = num_regions - 1;

    *pos = 0;
    if (!num_regions || (cmp = strcmp (name, reglist[0].name)) < 0)
        return (NULL);
    if (0 == cmp)
        return (&reglist[0]);
    if (!hi || (cmp = strcmp (name, reglist[hi].name)) > 0) {
        *pos = num_regions;
        return (NULL);
    }
    if (0 == cmp) {
        *pos = hi;
        return (&reglist[hi]);
    }

    while (1) {
        *pos = (lo + hi) >> 1;
        if (0 == (cmp = strcmp (name, reglist[*pos].name)))
            return (&reglist[*pos]);
        if (hi - lo <= 1)
            break;
        if (cmp < 0)
            hi = *pos;
        else
            lo = *pos;
    }
    *pos = hi;
    return (NULL);
}

/*---------- NewRegion -------------------------------------------------
 * add a new region to region list
 *----------------------------------------------------------------------*/

static cgnsREGN *NewRegion (name, pos)
char *name;
int pos;
{
    int n;
    static char *errmsg = "NewRegion:malloc failed for region list";

    if (!num_regions)
        reglist = (cgnsREGN *) malloc (sizeof(cgnsREGN));
    else
        reglist = (cgnsREGN *) realloc (reglist,
            (num_regions + 1) * sizeof(cgnsREGN));
    if (NULL == reglist)
        cgnsImportFatal (errmsg);
    for (n = num_regions; n > pos; n--) {
        strcpy (reglist[n].name, reglist[n-1].name);
        reglist[n].type  = reglist[n-1].type;
        reglist[n].nobjs = reglist[n-1].nobjs;
        reglist[n].objid = reglist[n-1].objid;
    }
    strncpy (reglist[pos].name, name, 32);
    reglist[pos].name[32] = 0;
    reglist[pos].type  = 0;
    reglist[pos].nobjs = 0;
    reglist[pos].objid = NULL;
    num_regions++;
    return (&reglist[pos]);
}

/*======================================================================
 * external face regions
 *======================================================================*/

/*---------- get_face_nodes -----------------------------------------
 * get nodes for an element face
 *-------------------------------------------------------------------*/

static int get_face_nodes (
#ifdef PROTOTYPE
    int faceid, int nodeid[4])
#else
    faceid, nodeid)
int faceid, nodeid[4];
#endif
{
    cgnsELEM *elem;
    int elemid = faceid >> 3;
    int facenum = faceid & 7;
    int n, nfaces, noff, nnodes;
    static int facenodes[20][5] = {
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

    if (elemid < 0 || elemid >= num_elements)
        cgnsImportFatal ("get_face_nodes:invalid element number");
    elem = &elemlist[elemid];
    switch (elem->elemtype) {
        case cgnsELEM_TET:
            noff = 0;
            nfaces = 4;
            break;
        case cgnsELEM_PYR:
            noff = 4;
            nfaces = 5;
            break;
        case cgnsELEM_WDG:
            noff = 9;
            nfaces = 5;
            break;
        case cgnsELEM_HEX:
            noff = 14;
            nfaces = 6;
            break;
        default:
            cgnsImportFatal ("get_face_nodes:invalid element type");
    }
    if (facenum < 0 || facenum >= nfaces)
        return (0);
    noff += (int)elem->facemap[facenum];
    nnodes = facenodes[noff][0];
    for (n = 0; n < nnodes; n++)
        nodeid[n] = elem->nodeid[facenodes[noff][n+1]];
    return (nnodes);
}

/*---------- compare_faces -------------------------------------------
 * face comparison routine
 *--------------------------------------------------------------------*/

static int compare_faces (
#ifdef PROTOTYPE
    void *v1, void *v2)
#else
    v1, v2)
void *v1, *v2;
#endif
{
    cgnsFACE *f1 = (cgnsFACE *)v1;
    cgnsFACE *f2 = (cgnsFACE *)v2;
    int n;

    if (f1->nnodes != f2->nnodes)
        return (f1->nnodes - f2->nnodes);

    /* the following assumes nodes have been sorted */

    for (n = 0; n < f1->nnodes; n++) {
        if (f1->nodeid[n] != f2->nodeid[n])
            return (f1->nodeid[n] - f2->nodeid[n]);
    }
    return (0);
}

/*---------- get_faces ----------------------------------------------
 * get the exterior faces
 *-------------------------------------------------------------------*/

static void get_faces (
#ifdef PROTOTYPE
    void *v)
#else
    v)
void *v;
#endif
{
    facelist[num_faces++] = (cgnsFACE *)v;
}

/*---------- hash_face -----------------------------------------------
 * face hash function
 *--------------------------------------------------------------------*/

static unsigned hash_face (
#ifdef PROTOTYPE
    void *v)
#else
    v)
void *v;
#endif
{
    cgnsFACE *face = (cgnsFACE *)v;
    int n;
    unsigned hash = 0;

    for (n = 0; n < face->nnodes; n++)
        hash += (unsigned)face->nodeid[n];
    return (hash);
}

/*---------- sortfaces -------------------------------------------------
 * called by qsort to sort the list of faces
 *----------------------------------------------------------------------*/

static int sortfaces (
#ifdef PROTOTYPE
    const void *f1, const void *f2)
#else
    f1, f2)
void *f1, *f2;
#endif
{
    return ((*((cgnsFACE **)f1))->faceid - (*((cgnsFACE **)f2))->faceid);
}

/*---------- exterior_faces -----------------------------------------
 * find exterior faces
 *-------------------------------------------------------------------*/

static void exterior_faces (
#ifdef PROTOTYPE
    void
#endif
){
    int i, j, k, nfaces, nodeid[4];
    int n, nn, id, faceid;
    HASH FaceList;
    cgnsFACE *pf, face;

    FaceList = HashCreate (2047, compare_faces, hash_face);
    if (NULL == FaceList)
        cgnsImportFatal ("exterior_faces:malloc failed for face hash table");

    for (n = 0; n < num_elements; n++) {
        switch (elemlist[n].elemtype) {
            case cgnsELEM_WDG:
                nfaces = 5;
                break;
            case cgnsELEM_HEX:
                nfaces = 6;
                break;
            default:
                nfaces = elemlist[n].elemtype;
                break;
        }

        /* loop over element faces */

        for (j = 0; j < nfaces; j++) {

            /* get face nodes and sort */

            faceid = (n << 3) | j;
            face.nnodes = get_face_nodes (faceid, nodeid);
            for (i = 0; i < face.nnodes; i++) {
                id = nodeid[i];
                for (k = 0; k < i; k++) {
                    if (face.nodeid[k] > id) {
                        nn = face.nodeid[k];
                        face.nodeid[k] = id;
                        id = nn;
                    }
                }
                face.nodeid[i] = id;
            }

            if (NULL == (pf = (cgnsFACE *) HashFind (FaceList, &face))) {

                /* create new face and add to list */

                if (NULL == (pf = (cgnsFACE *) malloc (sizeof(cgnsFACE))))
                    cgnsImportFatal ("exterior_faces:malloc failed for new face");
                pf->faceid = faceid;
                pf->flags = 0;
                pf->nnodes = face.nnodes;
                for (i = 0; i < face.nnodes; i++)
                    pf->nodeid[i] = face.nodeid[i];
                (void) HashAdd (FaceList, pf);
            }

            /* else already exists */

            else {
                HashDelete (FaceList, pf);
                free (pf);
            }
        }
    }

    facelist = (cgnsFACE **) malloc (HashSize (FaceList) * sizeof(cgnsFACE *));
    if (NULL == facelist)
        cgnsImportFatal ("exterior_faces:malloc failed for exterior face list");
    num_faces = 0;
    HashDestroy (FaceList, get_faces);

    /* check if faces need sorting */

    for (n = 1; n < num_faces; n++) {
        if (facelist[n]->faceid < facelist[n-1]->faceid)
            break;
    }
    if (n < num_faces)
        qsort (facelist, num_faces, sizeof(cgnsFACE *), sortfaces);

    /* get face nodes in the correct order */

    for (n = 0; n < num_faces; n++) {
        get_face_nodes (facelist[n]->faceid, nodeid);
        for (i = 0; i < 4; i++)
            facelist[n]->nodeid[i] = nodeid[i];
    }
}

/*===================================================================
 * write regions to cgns file
 *===================================================================*/

/*---------- sortnodes -------------------------------------------------
 * called by qsort to sort list of node ID mappings
 *----------------------------------------------------------------------*/

static int sortnodes (
#ifdef PROTOTYPE
    const void *n1, const void *n2)
#else
    n1, n2)
void *n1, *n2;
#endif
{
    return (*((int *)n1) - *((int *)n2));
}

/*---------- write_node_region --------------------------------------
 * write region from node list
 *-------------------------------------------------------------------*/

static int write_node_region (
#ifdef PROTOTYPE
    cgnsREGN *reg, int offset)
#else
    reg, offset)
cgnsREGN *reg;
int offset;
#endif
{
    int i, j, mid, lo, hi, pos;
    int nfaces, nc, *conns, isect;
    ElementType_t elemtype = ElementTypeNull;
    cgnsNODE *node;

    /* get exterior faces */

    if (num_faces == 0) exterior_faces ();
    for (j = 0; j < num_faces; j++)
        facelist[j]->flags = 0;

    /* sort region nodes */

    for (i = 1; i < reg->nobjs; i++) {
        if (reg->objid[i] < reg->objid[i-1])
            break;
    }
    if (i < reg->nobjs)
        qsort (reg->objid, reg->nobjs, sizeof(int), sortnodes);

    /* scan list of exterior faces */

    nfaces = nc = 0;
    for (j = 0; j < num_faces; j++) {
        if (facelist[j]->flags) continue;
        for (i = 0; i < facelist[j]->nnodes; i++) {
            lo = 0;
            hi = reg->nobjs - 1;
            while (lo <= hi) {
                mid = (lo + hi) >> 1;
                if (facelist[j]->nodeid[i] == reg->objid[mid])
                    break;
                if (facelist[j]->nodeid[i] < reg->objid[mid])
                    hi = mid - 1;
                else
                    lo = mid + 1;
            }
            if (lo > hi)
                break;
        }
        if (i == facelist[j]->nnodes) {
            nfaces++;
            facelist[j]->flags = 1;
            if (nc != i) {
                elemtype = nc ? MIXED : (i == 3 ? TRI_3 : QUAD_4);
                nc = i;
            }
        }
    }
    if (!nfaces) return 0;

    conns = (int *) malloc (5 * nfaces * sizeof(int));
    if (NULL == conns)
        cgnsImportFatal ("write_node_region:malloc failed for connectivity");

    /* write face connectivities */

    for (nc = 0, j = 0; j < num_faces; j++) {
        if (facelist[j]->flags) {
            if (elemtype == MIXED)
                conns[nc++] = facelist[j]->nnodes == 3 ? TRI_3 : QUAD_4;
            for (i = 0; i < facelist[j]->nnodes; i++) {
                if (NULL == (node = GetNode (facelist[j]->nodeid[i], &pos)))
                    cgnsImportFatal ("write_node_region:missing element node");
                conns[nc++] = pos + 1;
            }
        }
    }

    if (cg_section_write (cgnsFile, cgnsBase, cgnsZone, reg->name,
        elemtype, offset, offset + nfaces - 1, 0, conns, &isect))
        cgnsImportFatal ((char *)cg_get_error());

    /* create parent cell mapping */

    for (nc = 0, j = 0; j < num_faces; j++) {
        if (facelist[j]->flags)
            conns[nc++] = (facelist[j]->faceid >> 3) + 1;
    }
    for (j = 0; j < nfaces; j++)
        conns[nc++] = 0;
    for (j = 0; j < num_faces; j++) {
        if (facelist[j]->flags)
            conns[nc++] = (facelist[j]->faceid & 7) + 1;
    }
    for (j = 0; j < nfaces; j++)
        conns[nc++] = 0;
    if (cg_parent_data_write (cgnsFile, cgnsBase, cgnsZone, isect, conns))
        cgnsImportFatal ((char *)cg_get_error());

    free (conns);
    return nfaces;
}

/*---------- write_face_region --------------------------------------
 * write region from face list
 *-------------------------------------------------------------------*/

static int write_face_region (
#ifdef PROTOTYPE
    cgnsREGN *reg, int offset)
#else
    reg, offset)
cgnsREGN *reg;
int offset;
#endif
{
    int nn, n, elemid, facenum, nodeid[4];
    int i, nc, *conns, isect, pos;
    ElementType_t elemtype = ElementTypeNull;
    cgnsELEM *elem;
    cgnsNODE *node;

    if (!reg->nobjs) return 0;
    conns = (int *) malloc (5 * reg->nobjs * sizeof(int));
    if (NULL == conns)
        cgnsImportFatal ("write_face_region:malloc failed for connectivity");

    for (nc = 0, n = 0; n < reg->nobjs; n++) {
        elemid = reg->objid[n] >> 3;
        facenum = (reg->objid[n] & 7) - 1;
        if (NULL == (elem = GetElement (elemid, &pos)))
            cgnsImportFatal ("write_face_region:region element not found");
        nn = get_face_nodes ((pos << 3) | facenum, nodeid);
        if (nc != nn) {
            if (nc) {
                elemtype = MIXED;
                break;
            }
            nc = nn;
            elemtype = nn == 3 ? TRI_3 : QUAD_4;
        }
    }

    for (nc = 0, n = 0; n < reg->nobjs; n++) {
        elemid = reg->objid[n] >> 3;
        facenum = (reg->objid[n] & 7) - 1;
        elem = GetElement (elemid, &pos);
        nn = get_face_nodes ((pos << 3) | facenum, nodeid);
        if (elemtype == MIXED)
            conns[nc++] = nn == 3 ? TRI_3 : QUAD_4;
        for (i = 0; i < nn; i++) {
            if (NULL == (node = GetNode (nodeid[i], &pos)))
                cgnsImportFatal ("write_face_region:missing element node");
            conns[nc++] = pos + 1;
        }
    }

    if (cg_section_write (cgnsFile, cgnsBase, cgnsZone, reg->name,
        elemtype, offset, offset + reg->nobjs - 1, 0, conns, &isect))
        cgnsImportFatal ((char *)cg_get_error());

    free (conns);
    return reg->nobjs;
}

/*---------- write_elem_region --------------------------------------
 * write elements as region
 *-------------------------------------------------------------------*/

static int write_elem_region (
#ifdef PROTOTYPE
    cgnsREGN *reg, int offset)
#else
    reg)
cgnsREGN *reg;
#endif
{
    return 0;
}

/*======================================================================
 * API routines
 *======================================================================*/

/*---------- cgnsImportError -------------------------------------------
 * setup error handler call back
 *----------------------------------------------------------------------*/

void cgnsImportError (
#ifdef PROTOTYPE
    void (*callback)(char *msg))
#else
    callback)
void (*callback)();
#endif
{
    errcallback = callback;
}

/*---------- cgnsImportFatal -------------------------------------------
 * write error message and exit
 *----------------------------------------------------------------------*/

void cgnsImportFatal (
#ifdef PROTOTYPE
    char *errmsg)
#else
    errmsg)
char *errmsg;
#endif
{
    if (NULL != errcallback)
        (*errcallback) (errmsg);
    else if (NULL != errmsg && *errmsg)
        fprintf (stderr, "%s\n", errmsg);
    exit (-1);
}

/*---------- cgnsImportSetTol ------------------------------------------
 * setup tolerance for duplicate node checking
 *----------------------------------------------------------------------*/

double cgnsImportSetTol (
#ifdef PROTOTYPE
    double tol)
#else
    tol)
double tol;
#endif
{
    tolerance = tol >= 0.0 ? tol : TOLERANCE;
    tol = def_tol;
    def_tol = tolerance;
    return (tol);
}

/*---------- cgnsImportGetTol ------------------------------------------
 * return tolerance for duplicate node checking
 *----------------------------------------------------------------------*/

double cgnsImportGetTol (
#ifdef PROTOTYPE
    int rel)
#else
    rel)
int rel;
#endif
{
    double tol = def_tol;

    if (rel && num_nodes) {
        double avgvol = (xmax-xmin) * (ymax-ymin) * (zmax-zmin) /
            (DOUBLE)num_nodes;
        tol *= pow (avgvol, 0.33333);
    }
    return (tol);
}

/*---------- cgnsImportSetCheck ----------------------------------------
 * set duplicate node checking on/off
 *----------------------------------------------------------------------*/

void cgnsImportSetCheck (
#ifdef PROTOTYPE
    int set)
#else
    set)
int set;
#endif
{
    no_check = set;
}

/*---------- cgnsImportRange --------------------------------------------
 * gets bounding box of node coordinates
 *-----------------------------------------------------------------------*/

int cgnsImportRange (
#ifdef PROTOTYPE
    double *x1, double *y1, double *z1,
    double *x2, double *y2, double *z2)
#else
    x1, y1, z1, x2, y2, z2)
double *x1, *y1, *z1;
double *x2, *y2, *z2;
#endif
{
    *x1 = xmin; *y1 = ymin; *z1 = zmin;
    *x2 = xmax; *y2 = ymax; *z2 = zmax;
    return (num_nodes);
}

/*---------- cgnsImportCheck --------------------------------------------
 * check for and remove duplicate nodes
 *-----------------------------------------------------------------------*/

int cgnsImportCheck (
#ifdef PROTOTYPE
    int rel)
#else
    rel)
int rel;
#endif
{
    int n, pos, dup_cnt = 0;
    cgnsNODE *node;

    if (num_nodes < 2)
        return (0);

    /* set tolerance */

    tolerance = cgnsImportGetTol (rel);

    /* scan list of nodes, and remove duplicates */

    for (n = 0; n < num_nodes; n++) {
        if (!nodemap[n].mapped) {
            nodemap[n].node->dist = sqrt ((double)(
                (nodemap[n].node->x - xmin) * (nodemap[n].node->x - xmin) +
                (nodemap[n].node->y - ymin) * (nodemap[n].node->y - ymin) +
                (nodemap[n].node->z - zmin) * (nodemap[n].node->z - zmin)));
            node = FindNode (nodemap[n].node, &pos);
            if (NULL == node)
                AddNode (nodemap[n].node, pos);
            else if (node != nodemap[n].node) {
                if (REFS_BIT == (nodemap[n].node->flags & REFS_BIT)) {
                    for (pos = 0; pos < num_nodes; pos++) {
                        if (nodemap[pos].mapped &&
                            nodemap[pos].node == nodemap[n].node)
                            nodemap[pos].node = node;
                    }
                }
                node->flags |=
                    ((nodemap[n].node->flags & USED_BIT) | REFS_BIT);
                free (nodemap[n].node);
                nodemap[n].node = node;
                dup_cnt++;
            }
            nodemap[n].mapped = 1;
        }
    }

    /* free duplicate node checking list */

    free (node_list);
    num_entries = max_entries = 0;
    return (dup_cnt);
}

/*---------- cgnsImportMap ----------------------------------------------
 * map a node explictly to another
 *-----------------------------------------------------------------------*/

int cgnsImportMap (
#ifdef PROTOTYPE
    int nodeid, int mapid)
#else
    nodeid, mapid)
int nodeid, mapid;
#endif
{
    int p1, p2, ret;
    cgnsNODE *n1 = GetNode (nodeid, &p1);
    cgnsNODE *n2 = GetNode (mapid, &p2);

    if (NULL == n1 || NULL == n2)
        return (0);
    if (n1 == n2)
        return (n1->id);
    ret = CompareNodes (n1, n2) ? -1 : n1->id;
    if (REFS_BIT == (n2->flags & REFS_BIT)) {
        int n;
        for (n = 0; n < num_nodes; n++) {
            if (nodemap[n].node == n2) {
                nodemap[n].node = n1;
                nodemap[n].mapped = 1;
            }
        }
    }
    else {
        nodemap[p2].node = n1;
        nodemap[p2].mapped = 1;
    }
    n1->flags |= ((n2->flags & USED_BIT) | REFS_BIT);
    free (n2);
    return (ret);
}

/*---------- cgnsImportNode ---------------------------------------------
 * import a node
 *-----------------------------------------------------------------------*/

int cgnsImportNode (
#ifdef PROTOTYPE
    int nodeid, double x, double y, double z)
#else
    nodeid, x, y, z)
int nodeid;
double x, y, z;
#endif
{
    int n, pos;

    if (nodeid <= 0)
        return (0);

    /* find min/max bounds */

    if (!num_nodes) {
        xmin = xmax = x;
        ymin = ymax = y;
        zmin = zmax = z;
    }
    else {
        if (xmin > x) xmin = x;
        if (xmax < x) xmax = x;
        if (ymin > y) ymin = y;
        if (ymax < y) ymax = y;
        if (zmin > z) zmin = z;
        if (zmax < z) zmax = z;
    }

    /* find position to place node id */

    if (NULL != GetNode (nodeid, &pos)) {
        nodemap[pos].node->x = (DOUBLE)x;
        nodemap[pos].node->y = (DOUBLE)y;
        nodemap[pos].node->z = (DOUBLE)z;
        return (-1);
    }

    /* malloc/realloc if needed */

    if (num_nodes == max_nodes) {
        if (!max_nodes)
            nodemap = (NODEMAP *) malloc (NODEMAP_INC * sizeof(NODEMAP));
        else
            nodemap = (NODEMAP *) realloc (nodemap,
                (max_nodes + NODEMAP_INC) * sizeof(NODEMAP));
        if (NULL == nodemap)
            cgnsImportFatal (
                "cgnsImportNode:malloc failed for node mapping data");
        max_nodes += NODEMAP_INC;
    }

    /* insert new node */

    for (n = num_nodes; n > pos; n--) {
        nodemap[n].nodeid = nodemap[n-1].nodeid;
        nodemap[n].mapped = nodemap[n-1].mapped;
        nodemap[n].node = nodemap[n-1].node;
    }
    nodemap[pos].nodeid = nodeid;
    nodemap[pos].mapped = no_check;
    nodemap[pos].node = NewNode (nodeid, x, y, z);
    if (NULL == nodemap[pos].node)
        cgnsImportFatal ("cgnsImportNode:malloc failed for a new node");
    num_nodes++;
    return (nodeid);
}

/*---------- cgnsImportSetNode ------------------------------------------
 * set node coordinates for node ID
 *-----------------------------------------------------------------------*/

int cgnsImportSetNode (
#ifdef PROTOTYPE
    int nodeid, double x, double y, double z)
#else
    nodeid, x, y, z)
int nodeid;
double x, y, z;
#endif
{
    int n;
    cgnsNODE *node = GetNode (nodeid, &n);

    if (NULL != node) {
        node->x = (DOUBLE)x;
        node->y = (DOUBLE)y;
        node->z = (DOUBLE)z;
        return (node->id);
    }
    return (0);
}

/*---------- cgnsImportGetNode ------------------------------------------
 * return node coordinates for node ID
 *-----------------------------------------------------------------------*/

int cgnsImportGetNode (
#ifdef PROTOTYPE
    int nodeid, double *x, double *y, double *z)
#else
    nodeid, x, y, z)
int nodeid;
double *x, *y, *z;
#endif
{
    int n;
    cgnsNODE *node = GetNode (nodeid, &n);

    if (NULL != node) {
        *x = node->x;
        *y = node->y;
        *z = node->z;
        return (node->id);
    }
    return (0);
}

/*---------- cgnsImportNodeList -----------------------------------------
 * return list of all node ID's
 *-----------------------------------------------------------------------*/

int *cgnsImportNodeList (
#ifdef PROTOTYPE
    void
#endif
){
    int n, *nodeids = (int *) malloc ((num_nodes + 1) * sizeof(int));

    if (NULL == nodeids)
        cgnsImportFatal ("cgnsImportNodeList:malloc failed for node ID list");
    nodeids[0] = num_nodes;
    for (n = 0; n < num_nodes; n++)
        nodeids[n+1] = nodemap[n].nodeid;
    return (nodeids);
}

/*---------- cgnsImportAddVariable -------------------------------------
 * create a node variable
 *----------------------------------------------------------------------*/

int cgnsImportAddVariable (
#ifdef PROTOTYPE
    char *varname)
#else
    varname)
char *varname;
#endif
{
    int n;
    cgnsNODE *node;

    if (varname == NULL || !*varname) return -1;
    for (n = 0; n < num_vars; n++) {
        if (0 == strcmp(varname, var_names[n])) return n;
    }
    if (num_vars)
        var_names = (char **) realloc (var_names, (num_vars + 1) * sizeof(char *));
    else
        var_names = (char **) malloc (sizeof(char *));
    if (var_names == NULL)
        cgnsImportFatal ("AddVariable:malloc/realloc failed for variable name list");
    var_names[num_vars] = (char *) malloc (strlen(varname) + 1);
    if (var_names[num_vars] == NULL)
        cgnsImportFatal ("AddVariable:malloc failed for variable name");
    strcpy(var_names[num_vars++], varname);

    for (n = 0; n < num_entries; n++) {
        node = node_list[n];
        if (num_vars == 1)
            node->vars = (DOUBLE *) malloc (sizeof(DOUBLE));
        else
            node->vars = (DOUBLE *) realloc (node->vars, num_vars * sizeof(DOUBLE));
        if (node->vars == NULL)
            cgnsImportFatal ("AddVariable:malloc failed for node variables");
        node->vars[num_vars-1] = 0.0;
    }
    return num_vars-1;
}

/*---------- cgnsImportGetVariable -------------------------------------
 * get the variable number for a node variable
 *----------------------------------------------------------------------*/

int cgnsImportGetVariable (
#ifdef PROTOTYPE
    char *varname)
#else
    varname)
char *varname;
#endif
{
    int n;

    if (varname != NULL && *varname) {
        for (n = 0; n < num_vars; n++) {
            if (0 == strcmp(varname, var_names[n])) return n;
        }
    }
    return -1;
}

/*---------- cgnsImportVariable ----------------------------------------
 * set the value of a variable at a node
 *----------------------------------------------------------------------*/

int cgnsImportVariable (
#ifdef PROTOTYPE
    int nodeid, int varnum, double val)
#else
    nodeid, varnum, val)
int nodeid, varnum;
double val;
#endif
{
    int n;
    cgnsNODE *node = GetNode (nodeid, &n);

    if (NULL == node || varnum < 0 || varnum >= num_vars) return 0;
    node->vars[varnum] = (DOUBLE)val;
    return node->id;
}

/*---------- cgnsImportElement ------------------------------------------
 * import an element
 *-----------------------------------------------------------------------*/

int cgnsImportElement (
#ifdef PROTOTYPE
    int elemid, int elemtype, int *nodelist)
#else
    elemid, elemtype, nodelist)
int elemid, elemtype, *nodelist;
#endif
{
    int n, pos, ret;
    cgnsNODE *node;
    cgnsELEM *elem;

    if (elemid <= 0 ||
       (elemtype != cgnsELEM_TET && elemtype != cgnsELEM_PYR &&
        elemtype != cgnsELEM_WDG && elemtype != cgnsELEM_HEX))
        return (0);

    /* element not found */

    if (NULL == (elem = GetElement (elemid, &pos))) {
        ret = elemtype;
        elem = NewElement (pos);
    }

    /* element already exists */

    else {
        ret = -1;
        free (elem->nodeid);
    }

    /* set element values */

    elem->elemid   = elemid;
    elem->elemtype = elemtype;
    elem->nodeid   = (int *) malloc (elemtype * sizeof(int));
    if (NULL == elem->nodeid)
        cgnsImportFatal (
            "cgnsImportElement:malloc failed for a new element");

    for (n = 0; n < elemtype; n++) {
        if (NULL == (node = GetNode (nodelist[n], &pos))) {
            char errmsg[50];
            sprintf (errmsg,
                "cgnsImportElement:element node %d not found", nodelist[n]);
            cgnsImportFatal (errmsg);
        }
        elem->nodeid[n] = node->id;
        node->flags |= USED_BIT;
    }
    for (n = 0; n < 6; n++)
        elem->facemap[n] = n;

    return (ret);
}

/*---------- cgnsImportGetElement ---------------------------------------
 * return element for element ID
 *-----------------------------------------------------------------------*/

int cgnsImportGetElement (
#ifdef PROTOTYPE
    int elemid, int nodeid[])
#else
    elemid, nodeid)
int elemid, nodeid[];
#endif
{
    int n;
    cgnsELEM *elem = GetElement (elemid, &n);

    if (NULL != elem) {
        for (n = 0; n < elem->elemtype; n++)
            nodeid[n] = elem->nodeid[n];
        return (elem->elemtype);
    }
    return (0);
}

/*---------- cgnsImportElementList --------------------------------------
 * return list of all element ID's
 *-----------------------------------------------------------------------*/

int *cgnsImportElementList (
#ifdef PROTOTYPE
    void
#endif
){
    int n, *elemids = (int *) malloc ((num_elements + 1) * sizeof(int));

    if (NULL == elemids)
        cgnsImportFatal (
            "cgnsImportElementList:malloc failed for element ID list");
    elemids[0] = num_elements;
    for (n = 0; n < num_elements; n++)
        elemids[n+1] = elemlist[n].elemid;
    return (elemids);
}

/*---------- cgnsImportGetFace ------------------------------------------
 * return element face node ID's
 *-----------------------------------------------------------------------*/

int cgnsImportGetFace (
#ifdef PROTOTYPE
    int elemid, int facenum, int nodeid[])
#else
    elemid, facenum, nodeid)
int elemid, facenum, nodeid[];
#endif
{
    int n, nfaces;
    cgnsELEM *elem = GetElement (elemid, &n);

    if (NULL == elem)
        return (-1);
    switch (elem->elemtype) {
        case cgnsELEM_WDG:
            nfaces = 5;
            break;
        case cgnsELEM_HEX:
            nfaces = 6;
            break;
        default:
            nfaces = elem->elemtype;
            break;
    }
    if (--facenum < 0 || facenum >= nfaces)
        return (0);
    return get_face_nodes ((n << 3) | facenum, nodeid);
}

/*---------- cgnsImportFindFace -----------------------------------------
 * return element face number given face node ID's
 *-----------------------------------------------------------------------*/

int cgnsImportFindFace (
#ifdef PROTOTYPE
    int elemid, int nnodes, int nodeid[])
#else
    elemid, nnodes, nodeid)
int elemid, nnodes, nodeid[];
#endif
{
    int i, j, nfaces, noff, mask = 0;
    cgnsELEM *elem = GetElement (elemid, &i);
    static int facemask[4][6] = {
        /* tet */
        { 7,  11,  14,  13,   0,   0},
        /* pyramid */
        {15,  19,  22,  28,  25,   0},
        /* wedge */
        {27,  54,  45,   7,  56,   0},
        /* hex */
        {15,  51, 102, 204, 153, 240}
    };

    if (NULL == elem || NULL == nodeid)
        return (-1);

    switch (elem->elemtype) {
        case cgnsELEM_TET:
            if (nnodes != 3)
                return (-1);
            noff = 0;
            nfaces = 4;
            break;
        case cgnsELEM_PYR:
            if (nnodes < 3 || nnodes > 4)
                return (-1);
            noff = 1;
            nfaces = 5;
            break;
        case cgnsELEM_WDG:
            if (nnodes < 3 || nnodes > 4)
                return (-1);
            noff = 2;
            nfaces = 5;
            break;
        case cgnsELEM_HEX:
            if (nnodes != 4)
                return (-1);
            noff = 3;
            nfaces = 6;
            break;
        default:
            cgnsImportFatal ("cgnsImportFindFace:invalid element type");
    }

    for (j = 0; j < nnodes; j++) {
        for (i = 0; i < elem->elemtype; i++) {
            if (nodeid[j] == elem->nodeid[i])
                break;
        }
        if (i == elem->elemtype)
            return (0);
        mask |= (1 << i);
    }
    for (i = 0; i < nfaces; i++) {
        if (mask == facemask[noff][i]) {
            for (j = 0; j < 6; j++) {
                if (i == (int)elem->facemap[j])
                    return (j + 1);
            }
        }
    }
    return (0);
}

/*---------- cgnsImportBegReg --------------------------------------------
 * begin a region specification
 *-----------------------------------------------------------------------*/

int cgnsImportBegReg (
#ifdef PROTOTYPE
    char *regname, int regtype)
#else
    regname, regtype)
char *regname;
int regtype;
#endif
{
    int n;
    cgnsREGN *reg;

    if (region_id)
        cgnsImportEndReg ();

    /* initialize region node list */

    if (0 == region_max) {
        region_list = (int *) malloc (REGION_INC * sizeof(int));
        if (NULL == region_list)
            cgnsImportFatal (
                "cgnsImportBegReg:malloc failed for region node list");
        region_max = REGION_INC;
    }

    /* initialize region data */

    region_id = num_regions + 1;
    region_type = regtype;
    if (NULL == regname || !*regname)
        sprintf (region_name, "%s%d", REGION_BASE, region_id);
    else {
        strncpy (region_name, regname, sizeof(region_name));
        region_name[sizeof(region_name)-1] = 0;
    }
    region_nobjs = 0;

    if (NULL == (reg = GetRegion (region_name, &n)))
        return (0);
    if (reg->type != regtype)
        cgnsImportFatal ("cgnsImportBegReg:only 1 type allowed for a region");
    return (reg->nobjs);
}

/*---------- cgnsImportAddReg -------------------------------------------
 * add nodes to the region
 *-----------------------------------------------------------------------*/

int cgnsImportAddReg (
#ifdef PROTOTYPE
    int numobjs, int *objlist)
#else
    numobjs, objlist)
int numobjs, *objlist;
#endif
{
    int n, pos;
    char errmsg[50];

    if (!region_id)
        cgnsImportFatal ("cgnsImportAddReg:region not defined");

    /* realloc region list array if needed */

    if (region_nobjs + numobjs > region_max) {
        n = region_nobjs + numobjs - region_max;
        if (n < REGION_INC) n = REGION_INC;
        region_list = (int *) realloc (region_list,
            (region_max + n) * sizeof(int));
        if (NULL == region_list)
            cgnsImportFatal (
                "cgnsImportAddReg:malloc failed for region node list");
        region_max += n;
    }

    /* node region */

    if (region_type == cgnsREG_NODES) {
        cgnsNODE *node;
        for (n = 0; n < numobjs; n++) {
            if (NULL == (node = GetNode (objlist[n], &pos))) {
                sprintf (errmsg,
                    "cgnsImportAddReg:region node %d not found", objlist[n]);
                cgnsImportFatal (errmsg);
            }
            region_list[region_nobjs++] = node->id;
            node->flags |= USED_BIT;
        }
    }

    /* face region */

    else if (region_type == cgnsREG_FACES) {
        int elemid, facenum, nfaces;
        cgnsELEM *elem;
        for (n = 0; n < numobjs; n++) {
            elemid = objlist[n] >> 3;
            facenum = objlist[n] & 7;
            if (NULL == (elem = GetElement (elemid, &pos))) {
                sprintf (errmsg,
                    "cgnsImportAddReg:region element %d not found", elemid);
                cgnsImportFatal (errmsg);
            }
            if (elem->elemtype == cgnsELEM_WDG)
                nfaces = 5;
            else if (elem->elemtype == cgnsELEM_HEX)
                nfaces = 6;
            else
                nfaces = elem->elemtype;
            if (facenum < 1 || facenum > nfaces)
                cgnsImportFatal (
                    "cgnsImportAddReg:region face number out of range");
            region_list[region_nobjs++] = objlist[n];
        }
    }

    /* element region */

    else if (region_type == cgnsREG_ELEMS) {
        for (n = 0; n < numobjs; n++) {
            if (NULL == GetElement (objlist[n], &pos)) {
                sprintf (errmsg,
                    "cgnsImportAddReg:region element %d not found",
                    objlist[n]);
                cgnsImportFatal (errmsg);
            }
            region_list[region_nobjs++] = objlist[n];
        }
    }

    else
        cgnsImportFatal ("cgnsImportAddReg:undefined region type");

    return (region_nobjs);
}

/*---------- cgnsImportEndReg -------------------------------------------
 * end region definition and import region
 *-----------------------------------------------------------------------*/

int cgnsImportEndReg (
#ifdef PROTOTYPE
    void
#endif
){
    int n, pos;
    cgnsREGN *reg;

    if (!region_id || !region_nobjs)
        return (region_id = 0);
    region_id = 0;

    /* create a new region */

    if (NULL == (reg = GetRegion (region_name, &pos))) {
        reg = NewRegion (region_name, pos);
        reg->type = region_type;
    }
    if (0 == reg->nobjs)
        reg->objid = (int *) malloc (region_nobjs * sizeof(int));
    else
        reg->objid = (int *) realloc (reg->objid,
            (reg->nobjs + region_nobjs) * sizeof(int));
    if (NULL == reg->objid)
        cgnsImportFatal (
            "cgnsImportRegion:malloc failed for the region object list");

    for (n = 0; n < region_nobjs; n++)
        reg->objid[n+reg->nobjs] = region_list[n];
    reg->nobjs += region_nobjs;
    return (reg->nobjs);
}

/*---------- cgnsImportRegion -------------------------------------------
 * import a named region
 *-----------------------------------------------------------------------*/

int cgnsImportRegion (
#ifdef PROTOTYPE
    char *regname, int regtype, int numobjs, int *objlist)
#else
    regname, regtype, numobjs, objlist)
char *regname;
int regtype, numobjs, *objlist;
#endif
{
    cgnsImportBegReg (regname, regtype);
    cgnsImportAddReg (numobjs, objlist);
    return (cgnsImportEndReg ());
}

/*---------- cgnsImportRegionList ---------------------------------------
 * return a list of all region names
 *-----------------------------------------------------------------------*/

char **cgnsImportRegionList (
#ifdef PROTOTYPE
    void
#endif
){
    int n, len = 0;
    char **namelist, *names;

    for (n = 0; n < num_regions; n++)
        len += (strlen (reglist[n].name) + 1);
    n = num_regions + 1;
    namelist = (char **) malloc (len + n * sizeof(char *));
    if (NULL == namelist)
        cgnsImportFatal (
            "cgnsImportRegionList:malloc failed for region name list");
    names = (char *) (namelist + n);
    for (n = 0; n < num_regions; n++) {
        namelist[n] = names;
        strcpy (names, reglist[n].name);
        names += (strlen (reglist[n].name) + 1);
    }
    namelist[num_regions] = NULL;
    return (namelist);
}

/*---------- cgnsImportGetRegion ----------------------------------------
 * get node ID's for a region
 *-----------------------------------------------------------------------*/

int *cgnsImportGetRegion (
#ifdef PROTOTYPE
    char *regname)
#else
    regname)
char *regname;
#endif
{
    int n, *objlist;
    cgnsREGN *reg;

    if (NULL == regname || !*regname ||
        NULL == (reg = GetRegion (regname, &n)))
        return (NULL);
    objlist = (int *) malloc ((reg->nobjs + 2) * sizeof(int));
    if (NULL == objlist)
        cgnsImportFatal (
            "cgnsImportGetRegion:malloc failed for region object ID list");
    objlist[0] = reg->type;
    objlist[1] = reg->nobjs;
    for (n = 0; n < reg->nobjs; n++)
        objlist[n+2] = reg->objid[n];
    return (objlist);
}

/*---------- cgnsImportOpen ---------------------------------------------
 * open CGNS file
 *-----------------------------------------------------------------------*/

int cgnsImportOpen (
#ifdef PROTOTYPE
    char *filename)
#else
    filename)
char *filename;
#endif
{
    int fn;

    cgnsImportClose ();
    if (cg_open (filename, CG_MODE_MODIFY, &cgnsFile) &&
        cg_open (filename, CG_MODE_WRITE, &cgnsFile))
        cgnsImportFatal ((char *)cg_get_error());
    return cgnsFile;
}

/*---------- cgnsImportBase ---------------------------------------------
 * set CGNS base
 *-----------------------------------------------------------------------*/

int cgnsImportBase (
#ifdef PROTOTYPE
    char *basename)
#else
    basename)
char *basename;
#endif
{
    if (cg_base_write (cgnsFile, basename, 3, 3, &cgnsBase))
        cgnsImportFatal ((char *)cg_get_error());
    return cgnsBase;
}

/*---------- cgnsImportZone ---------------------------------------------
 * set CGNS zone
 *-----------------------------------------------------------------------*/

void cgnsImportZone (
#ifdef PROTOTYPE
    char *zonename)
#else
    zonename)
char *zonename;
#endif
{
    int n;

    for (n = 0; n < num_nodes; n++) {
        if (nodemap[n].node != NULL)
            free (nodemap[n].node);
    }
    for (n = 0; n < num_elements; n++) {
        if (elemlist[n].nodeid != NULL)
            free (elemlist[n].nodeid);
    }
    if (num_regions) {
        for (n = 0; n < num_regions; n++) {
            if (reglist[n].objid != NULL)
                free (reglist[n].objid);
        }
        free (reglist);
    }
    if (num_faces) {
        for (n = 0; n < num_faces; n++) {
            if (facelist[n] != NULL) free (facelist[n]);
        }
        free (facelist);
    }
    num_nodes = num_elements = num_regions = num_faces = 0;

    strncpy (cgnsZoneName, zonename, 32);
    cgnsZoneName[32] = 0;
}

/*---------- cgnsImportWrite --------------------------------------------
 * write data to the CGNS file
 *-----------------------------------------------------------------------*/

int cgnsImportWrite (
#ifdef PROTOTYPE
    void
#endif
){
    int n, nn, nnodes, icoord, sizes[3];
    int nc, nconn, *conns, pos, isect;
    ElementType_t elemtype;
#ifdef DOUBLE_PRECISION
    DataType_t datatype = RealDouble;
    double *xyz;
#else
    DataType_t datatype = RealSingle;
    float *xyz;
#endif
    cgnsNODE *node;
    cgnsELEM *elem;
    cgnsREGN *regn;

    if (!cgnsFile)
        cgnsImportFatal ("cgnsImportWrite:CGNS file not open");
    if (region_id)
        cgnsImportEndReg ();

    /* count the nodes */

    nnodes = 0;
    for (n = 0; n < num_nodes; n++) {
        if (nodemap[n].nodeid == nodemap[n].node->id &&
            USED_BIT == (nodemap[n].node->flags & USED_BIT))
            nnodes++;
    }
    if (!nnodes) return 0;

    if (!cgnsBase) cgnsImportBase ("Base");
    if (!*cgnsZoneName) strcpy (cgnsZoneName, "Zone");

    sizes[0] = nnodes;
    sizes[1] = num_elements;
    sizes[2] = 0;

    if (cg_zone_write (cgnsFile, cgnsBase, cgnsZoneName,
        sizes, Unstructured, &cgnsZone))
        cgnsImportFatal ((char *)cg_get_error());

    /* write the node list */

#ifdef DOUBLE_PRECISION
    xyz = (double *) malloc (nnodes * sizeof(double));
#else
    xyz = (float *) malloc (nnodes * sizeof(float));
#endif
    if (NULL == xyz)
        cgnsImportFatal ("cgnsImportWrite:malloc failed for nodes");

    for (nn = 0, n = 0; n < num_nodes; n++) {
        if (nodemap[n].nodeid == nodemap[n].node->id &&
            USED_BIT == (nodemap[n].node->flags & USED_BIT))
            xyz[nn++] = nodemap[n].node->x;
    }
    if (cg_coord_write (cgnsFile, cgnsBase, cgnsZone, datatype,
        "CoordinateX", (void *)xyz, &icoord))
        cgnsImportFatal ((char *)cg_get_error());

    for (nn = 0, n = 0; n < num_nodes; n++) {
        if (nodemap[n].nodeid == nodemap[n].node->id &&
            USED_BIT == (nodemap[n].node->flags & USED_BIT))
            xyz[nn++] = nodemap[n].node->y;
    }
    if (cg_coord_write (cgnsFile, cgnsBase, cgnsZone, datatype,
        "CoordinateY", (void *)xyz, &icoord))
        cgnsImportFatal ((char *)cg_get_error());

    for (nn = 0, n = 0; n < num_nodes; n++) {
        if (nodemap[n].nodeid == nodemap[n].node->id &&
            USED_BIT == (nodemap[n].node->flags & USED_BIT))
            xyz[nn++] = nodemap[n].node->z;
    }
    if (cg_coord_write (cgnsFile, cgnsBase, cgnsZone, datatype,
        "CoordinateZ", (void *)xyz, &icoord))
        cgnsImportFatal ((char *)cg_get_error());

    /* write variables */

    if (num_vars) {
        int isol, ifld, nv;
        if (cg_sol_write(cgnsFile, cgnsBase, cgnsZone,
            "NodeVariables", Vertex, &isol))
            cgnsImportFatal ((char *)cg_get_error());
        for (nv = 0; nv < num_vars; nv++) {
            for (nn = 0, n = 0; n < num_nodes; n++) {
                if (nodemap[n].nodeid == nodemap[n].node->id &&
                    USED_BIT == (nodemap[n].node->flags & USED_BIT))
                    xyz[nn++] = nodemap[n].node->vars[nv];
            }
            if (strlen(var_names[nv]) > 32) var_names[nv][32] = 0;
            if (cg_field_write(cgnsFile, cgnsBase, cgnsZone, isol,
                datatype, var_names[nv], xyz, &ifld))
                cgnsImportFatal ((char *)cg_get_error());
        }
    }

    free (xyz);

    /* write the element list */

    switch (elemlist->elemtype) {
        case cgnsELEM_TET:
            elemtype = TETRA_4;
            break;
        case cgnsELEM_PYR:
            elemtype = PYRA_5;
            break;
        case cgnsELEM_WDG:
            elemtype = PENTA_6;
            break;
        case cgnsELEM_HEX:
            elemtype = HEXA_8;
            break;
    }
    for (n = 0, elem = elemlist; n < num_elements; n++, elem++) {
        if (elem->elemtype != elemlist->elemtype) {
            elemtype = MIXED;
            break;
        }
    }

    if (elemtype == MIXED) {
        nconn = 0;
        for (n = 0, elem = elemlist; n < num_elements; n++, elem++)
            nconn += (1 + elem->elemtype);
    }
    else
        nconn = num_elements * elemlist->elemtype;
    conns = (int *) malloc (nconn * sizeof(int));
    if (NULL == conns)
        cgnsImportFatal ("cgnsImportWrite:malloc failed for element data");

    nc = 0;
    for (n = 0, elem = elemlist; n < num_elements; n++, elem++) {
        if (elemtype == MIXED) {
            switch (elem->elemtype) {
                case cgnsELEM_TET :
                    conns[nc] = TETRA_4;
                    break;
                case cgnsELEM_PYR:
                    conns[nc] = PYRA_5;
                    break;
                case cgnsELEM_WDG:
                    conns[nc] = PENTA_6;
                    break;
                case cgnsELEM_HEX:
                    conns[nc] = HEXA_8;
                    break;
            }
            nc++;
        }
        for (nn = 0; nn < elem->elemtype; nn++) {
            if (NULL == (node = GetNode (elem->nodeid[nn], &pos)))
                cgnsImportFatal ("cgnsImportWrite:missing element node");
            conns[nc++] = pos + 1;
        }
    }

    if (cg_section_write (cgnsFile, cgnsBase, cgnsZone, "GridElements",
        elemtype, 1, num_elements, 0, conns, &isect))
        cgnsImportFatal ((char *)cg_get_error());

    free (conns);

    /* write the regions */

    nn = num_elements + 1;
    for (n = 0, regn = reglist; n < num_regions; n++, regn++) {
        if (regn->type == cgnsREG_NODES)
            nn += write_node_region (regn, nn);
        else if (regn->type == cgnsREG_FACES)
            nn += write_face_region (regn, nn);
        else if (regn->type == cgnsREG_ELEMS)
            nn += write_elem_region (regn, nn);
    }

    return cgnsZone;
}

/*---------- cgnsImportClose --------------------------------------------
 * close the CGNS file
 *-----------------------------------------------------------------------*/

void cgnsImportClose (
#ifdef PROTOTYPE
    void
#endif
){
    if (cgnsFile) {
        cg_close (cgnsFile);
        cgnsFile = 0;
    }
}

