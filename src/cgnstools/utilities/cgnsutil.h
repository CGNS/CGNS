/*
 * cgnsutil.h - CGNS utility header
 */

#ifndef _CGNSUTIL_H_
#define _CGNSUTIL_H_

#if defined(__STDC__) || defined(__cplusplus)
# ifndef PROTOTYPE
#  define PROTOTYPE
# endif
#endif

typedef struct _DESC {
    int id;
    char name[33];
    char *desc;
} DESC;

typedef struct _VERTEX {
    int id;
    double x, y, z, w;
} VERTEX;

typedef struct _ELEMSET {
    int id;
    char name[33];
    int type;
    int start;
    int end;
    int nbndry;
    int *conn;
    int *parent;
} ELEMSET;

typedef struct _INTERFACE {
    int id;
    char name[33];
    int range[3][2];
    char d_name[33];
    int d_range[3][2];
    int transform[3];
    int d_zone;
} INTERFACE;

typedef struct _CONNECT {
    int id;
    char name[33];
    int type;
    int location;
    int ptype;
    int npnts;
    int *pnts;
    char d_name[33];
    int d_ztype;
    int d_ptype;
    int d_npnts;
    int *d_pnts;
    int d_zone;
} CONNECT;

typedef struct _BOCO {
    int id;
    char name[33];
    int type;
    int ptype;
    int npnts;
    int *pnts;
    int n_index[3];
    int n_cnt;
    int n_type;
    double *n_list;
} BOCO;

typedef struct _FIELD {
    int id;
    char name[33];
    int datatype;
    int units[5];
    int dataclass;
    int convtype;
    double dataconv[2];
    int exptype;
    double exponent[5];
    double *data;
} FIELD;

typedef struct _SOLUTION {
    int id;
    char name[33];
    int location;
    int rind[3][2];
    int size;
    int units[5];
    int dataclass;
    int nflds;
    FIELD *flds;
    int ndesc;
    DESC *desc;
} SOLUTION;

typedef struct _ZONE {
    int id;
    char name[33];
    int type;
    int idim;
    int dim[3];
    int units[5];
    int dataclass;
    int datatype;
    int vertflags;
    int nverts;
    VERTEX *verts;
    int nesets;
    ELEMSET *esets;
    int nints;
    INTERFACE *ints;
    int nconns;
    CONNECT *conns;
    int nbocos;
    BOCO *bocos;
    int nsols;
    SOLUTION *sols;
    void *user;
    int ndesc;
    DESC *desc;
} ZONE;

extern int nZones;
extern ZONE *Zones;

extern int baseunits[5];
extern int baseclass;

extern int cgnsfn;
extern int cgnsbase;

extern int element_node_counts[];

#ifdef __cplusplus
extern "C" {
#endif

void FATAL (
#ifdef PROTOTYPE
    char *procname, char *errmsg
#endif
);

ZONE *new_zone (
#ifdef PROTOTYPE
    int count
#endif
);

VERTEX *new_vertex (
#ifdef PROTOTYPE
    int nverts
#endif
);

ELEMSET *new_elemset (
#ifdef PROTOTYPE
    int nsets
#endif
);

INTERFACE *new_interface (
#ifdef PROTOTYPE
    int nints
#endif
);

CONNECT *new_connect (
#ifdef PROTOTYPE
    int nconns
#endif
);

BOCO *new_boco (
#ifdef PROTOTYPE
    int nbocos
#endif
);

DESC *new_desc (
#ifdef PROTOTYPE
    int ndesc
#endif
);

SOLUTION *new_solution (
#ifdef PROTOTYPE
    int nsols
#endif
);

FIELD *new_field (
#ifdef PROTOTYPE
    int nfields, int size
#endif
);

int vertex_index (
#ifdef PROTOTYPE
    ZONE *z, int i, int j, int k
#endif
);

int cell_index (
#ifdef PROTOTYPE
    ZONE *z, int i, int j, int k
#endif
);

int solution_index (
#ifdef PROTOTYPE
    ZONE *z, SOLUTION *s, int i, int j, int k
#endif
);

int file_exists (
#ifdef PROTOTYPE
    char *file
#endif
);

int is_executable (
#ifdef PROTOTYPE
    char *file
#endif
);

char *find_executable (
#ifdef PROTOTYPE
    char *exename
#endif
);

char *find_file (
#ifdef PROTOTYPE
    char *filename, char *exename
#endif
);

int same_file (
#ifdef PROTOTYPE
    char *file1, char *file2
#endif
);

char *temporary_file (
#ifdef PROTOTYPE
    void
#endif
);

void copy_file (
#ifdef PROTOTYPE
    char *oldfile, char *newfile
#endif
);

int open_cgns (
#ifdef PROTOTYPE
    char *cgnsfile, int read_only
#endif
);

int find_base (
#ifdef PROTOTYPE
    char *basename
#endif
);

void read_cgns (
#ifdef PROTOTYPE
    void
#endif
);

int read_zones (
#ifdef PROTOTYPE
    void
#endif
);

void read_zone_data (
#ifdef PROTOTYPE
    int izone
#endif
);

int read_zone_grid (
#ifdef PROTOTYPE
    int izone
#endif
);

int read_zone_element (
#ifdef PROTOTYPE
    int izone
#endif
);

int structured_elements (
#ifdef PROTOTYPE
    int izone
#endif
);

int read_zone_interface (
#ifdef PROTOTYPE
    int izone
#endif
);

int read_zone_connect (
#ifdef PROTOTYPE
    int izone
#endif
);

int read_zone_boco (
#ifdef PROTOTYPE
    int izone
#endif
);

int read_zone_solution (
#ifdef PROTOTYPE
    int izone
#endif
);

int read_solution_field (
#ifdef PROTOTYPE
    int izone, int isol, int ifld
#endif
);

int read_units (
#ifdef PROTOTYPE
    int units[5]
#endif
);

void write_cgns (
#ifdef PROTOTYPE
    void
#endif
);

void write_zones (
#ifdef PROTOTYPE
    void
#endif
);

void write_zone_data (
#ifdef PROTOTYPE
    int izone
#endif
);

void write_zone_grid (
#ifdef PROTOTYPE
    int izone
#endif
);

void write_zone_element (
#ifdef PROTOTYPE
    int izone
#endif
);

void write_zone_interface (
#ifdef PROTOTYPE
    int izone
#endif
);

void write_zone_connect (
#ifdef PROTOTYPE
    int izone
#endif
);

void write_zone_boco (
#ifdef PROTOTYPE
    int izone
#endif
);

void write_zone_solution (
#ifdef PROTOTYPE
    int izone, int isol
#endif
);

void write_solution_field (
#ifdef PROTOTYPE
    int izone, int isol, int ifld
#endif
);

double volume_tet (
#ifdef PROTOTYPE
    VERTEX *v1, VERTEX *v2, VERTEX *v3, VERTEX *v4
#endif
);

double volume_pyr (
#ifdef PROTOTYPE
    VERTEX *v1, VERTEX *v2, VERTEX *v3,
    VERTEX *v4, VERTEX *v5
#endif
);

double volume_wdg (
#ifdef PROTOTYPE
    VERTEX *v1, VERTEX *v2, VERTEX *v3,
    VERTEX *v4, VERTEX *v5, VERTEX *v6
#endif
);

double volume_hex (
#ifdef PROTOTYPE
    VERTEX *v1, VERTEX *v2, VERTEX *v3, VERTEX *v4,
    VERTEX *v5, VERTEX *v6, VERTEX *v7, VERTEX *v8
#endif
);

double volume_element (
#ifdef PROTOTYPE
    int nnodes, VERTEX *v[]
#endif
);

double cell_volume (
#ifdef PROTOTYPE
    ZONE *z, int i, int j, int k
#endif
);

void vertex_volumes (
#ifdef PROTOTYPE
    int izone
#endif
);

void cell_vertex_solution (
#ifdef PROTOTYPE
    int izone, int isol, int weight
#endif
);

void cell_center_solution (
#ifdef PROTOTYPE
    int izone, int isol, int weight
#endif
);

#ifdef __cplusplus
}
#endif

#endif

