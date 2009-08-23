/* include file for CGNS imports */

#ifndef _cgnsIMPORT_H_
#define _cgnsIMPORT_H_

/*--- allowable element types ---*/

#define cgnsELEM_TET     4   /* tet element (4 nodes ) */
#define cgnsELEM_PYR     5   /* pyramid element (5 nodes ) */
#define cgnsELEM_WDG     6   /* wedge element (6 nodes) */
#define cgnsELEM_HEX     8   /* hex element (8 nodes) */

/*--- allowable region types ---*/

#define cgnsREG_NODES    1
#define cgnsREG_FACES    2
#define cgnsREG_ELEMS    3

/*--- convert element ID/face number to face ID */

#define cgnsFACEID(elemid,facenum) ((elemid << 3) | (facenum & 7))

/*--- function prototypes ---*/

#ifdef __STDC__
#ifndef PROTOTYPE
#define PROTOTYPE
#endif
#endif

#ifdef __cplusplus
#ifndef PROTOTYPE
#define PROTOTYPE
#endif
extern "C" {
#endif

void cgnsImportError (   /* define callback for errors */
#ifdef PROTOTYPE
    void (*callback)(    /* user-supplied call back routine */
        char *errmsg     /* error message */
    )
#endif
);

void cgnsImportFatal (   /* terminate with error message */
#ifdef PROTOTYPE
    char *errmsg         /* error message */
#endif
);

double cgnsImportSetTol (/* setup node checking tolerance */
#ifdef PROTOTYPE
    double tol           /* tolerance for comparisons */
#endif
);

double cgnsImportGetTol (/* get duplicate node tolerance */
#ifdef PROTOTYPE
    int rel_tol          /* 0 - absolute tol, else relative tol */
#endif
);

void cgnsImportSetCheck (/* set dup checking for new nodes */
#ifdef PROTOTYPE
    int set              /* 0 - allow dup checking, else disallow */
#endif
);

int cgnsImportRange (    /* returns bounding box of nodes */
#ifdef PROTOTYPE
    double *xmin,        /* lower x limit */
    double *ymin,        /* lower y limit */
    double *zmin,        /* lower z limit */
    double *xmax,        /* upper x limit */
    double *ymax,        /* upper y limit */
    double *zmax         /* upper z limit */
#endif
);

int cgnsImportCheck (    /* check for duplicate nodes */
#ifdef PROTOTYPE
    int rel_tol          /* 0 - absolute tol, else relative tol */
#endif
);

int cgnsImportMap (      /* explicitly map 2 nodes */
#ifdef PROTOTYPE
    int nodeid,          /* reference node id */
    int mapid            /* node id to map */
#endif
);

int cgnsImportNode (     /* import a node */
#ifdef PROTOTYPE
    int nodeid,          /* node number */
    double x,            /* x coordinate */
    double y,            /* y coordinate */
    double z             /* z coordinate */
#endif
);

int cgnsImportSetNode (  /* set node coordinates */
#ifdef PROTOTYPE
    int nodeid,          /* node ID */
    double x,            /* coordinates */
    double y,
    double z
#endif
);

int cgnsImportGetNode (  /* get node coordinates */
#ifdef PROTOTYPE
    int nodeid,          /* node ID */
    double *x,           /* returned coordinates */
    double *y,
    double *z
#endif
);

int *cgnsImportNodeList (/* return list of all node ID's */
#ifdef PROTOTYPE
    void
#endif
);

int cgnsImportAddVariable (/*add a variable to the nodes */
#ifdef PROTOTYPE
    char *varname        /* name of the variable */
#endif
);

int cgnsImportGetVariable (/* return variable number */
#ifdef PROTOTYPE
    char *varname        /* name of the variable */
#endif
);

int cgnsImportVariable ( /* set variable value at node */
#ifdef PROTOTYPE
    int nodeid,          /* node number */
    int varnum,          /* variable number */
    double val           /* variable value */
#endif
);

int cgnsImportElement (  /* import an element */
#ifdef PROTOTYPE
    int elemid,          /* element number */
    int elemtype,        /* element type - tet,pyr,wdg or hex */
    int *nodelist        /* node numbers defining element */
#endif
);

int cgnsImportGetElement (/* get element nodes */
#ifdef PROTOTYPE
    int elemid,          /* element ID */
    int nodeid[]         /* returned node IDs */
#endif
);

int *cgnsImportElementList (/* return list of all element ID's */
#ifdef PROTOTYPE
    void
#endif
);

int cgnsImportGetFace (  /* get element face nodes */
#ifdef PROTOTYPE
    int elemid,          /* element ID */
    int facenum,         /* element face number */
    int nodeid[]         /* face nodes */
#endif
);

int cgnsImportFindFace ( /* get element face number */
#ifdef PROTOTYPE
    int elemid,          /* element ID */
    int nnodes,          /* number of nodes */
    int nodeid[]         /* face nodes */
#endif
);

int cgnsImportBegReg (   /* begin a region specification */
#ifdef PROTOTYPE
    char *regname,       /* region name */
    int regtype          /* type of region (nodes,faces or elements) */
#endif
);

int cgnsImportAddReg (   /* add nodes to a region specification */
#ifdef PROTOTYPE
    int numobjs,         /* number of objects to add */
    int *objlist         /* object list for region */
#endif
);

int cgnsImportEndReg (   /* end region specification */
#ifdef PROTOTYPE
    void
#endif
);

int cgnsImportRegion (   /* import region of nodes */
#ifdef PROTOTYPE
    char *regname,       /* region name */
    int regtype,         /* region type */
    int numobjs,         /* number of objects in region */
    int *objlist         /* object IDs in region */
#endif
);

char **cgnsImportRegionList (/* get list of region names */
#ifdef PROTOTYPE
    void
#endif
);

int *cgnsImportGetRegion (/* get region object ID's */
#ifdef PROTOTYPE
    char *regname        /* region name */
#endif
);

int cgnsImportOpen (     /* open CGNS file */
#ifdef PROTOTYPE
    char *filename       /* name of the file */
#endif
);

int cgnsImportBase (     /* set CGNS base */
#ifdef PROTOTYPE
    char *basename       /* name for base */
#endif
);

void cgnsImportZone (    /* set CGNS zone */
#ifdef PROTOTYPE
    char *zonename       /* name for zone */
#endif
);

int cgnsImportWrite (    /* write data to CGNS file */
#ifdef PROTOTYPE
    void
#endif
);

void cgnsImportClose (   /* close the CGNS file */
#ifdef PROTOTYPE
    void
#endif
);

#ifdef __cplusplus
}
#endif

#endif  /* _cgnsIMPORT_H_ */
