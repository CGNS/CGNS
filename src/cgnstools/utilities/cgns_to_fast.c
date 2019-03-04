#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#ifdef _WIN32
#include <io.h>
#define ACCESS _access
#else
#include <unistd.h>
#define ACCESS access
#endif

#include "cgnslib.h"
#include "getargs.h"
#include "hash.h"

#if defined(_WIN32) || defined(__BORLANDC__) || defined(__GO32__) || \
    defined(__alpha) || defined(ultrix) || defined(linux)
# ifndef LITTLE_ENDIAN
#  define LITTLE_ENDIAN
# endif
#endif

static char options[] = "48fslbxyzB:Z:";

static char *usgmsg[] = {
    "usage: cgns_to_fast [options] CGNSfile [FASTfile] [MAPBCfile]",
    "  options are:",
    "   -4       = write coordinates as real*4 (default Real*8)",
    "   -8       = write coordinates as real*8 (double - default)",
    "   -f       = formatted (ASCII) file format",
    "   -s       = Fortran stream format (binary - default)",
    "   -l       = little-endian format",
    "   -b       = big-endian format (default)",
    "   -x       = symmetric in X",
    "   -y       = symmetric in Y",
    "   -z       = symmetric in Z",
    "   -B<base> = use CGNS base number <base> (default 1)",
    "   -Z<zone> = zone number (default 1)",
    "default is big-endian binary (Fortran stream) FAST file",
    NULL
};

static int is_ascii = 0;
static int is_single = 0;
static int is_little = 0;
static int is_sym = 0;

static int cgFile = 0, cgBase, cgZone;
static cgsize_t sizes[9];

static cgsize_t nCoords = 0;
static cgsize_t nTris = 0;
static cgsize_t nTets = 0;

typedef struct {
    cgsize_t id;
    int bcnum;
    cgsize_t nodes[3];
} TRI;

static TRI **Tris;

static int tetfaces[4][3] = {
    {0, 2, 1}, {0, 1, 3}, {1, 2, 3}, {2, 0, 3}
};

typedef struct {
    CGNS_ENUMT(BCType_t) type;
    char name[33];
} BOCO;

static int nBocos = 0;
static BOCO *Bocos;

/*--------------------------------------------------------------------*/

static void err_exit (char *procname, char *errmsg)
{
    char *msg = errmsg;

    if (NULL == msg) {
        msg = (char *)cg_get_error ();
        if (NULL == msg || !*msg)
            msg = "unknown error";
    }
    fflush (stdout);
    if (NULL == procname || !*procname)
        fprintf (stderr, "%s\n", msg);
    else
        fprintf (stderr, "%s:%s\n", procname, msg);
    if (cgFile) cg_close (cgFile);
    exit (1);
}

/*--------------------------------------------------------------------*/

static void *swap_bytes (int bytes, void *data)
{
    static unsigned char buf[sizeof(double)];
    unsigned char *p = (unsigned char *)data;
    int n;

    p += bytes;
    for (n = 0; n < bytes; n++)
        buf[n] = *--p;
    return ((void *)buf);
}

/*--------------------------------------------------------------------*/

static void write_ints (FILE *fp, int cnt, int *data)
{
    if (is_ascii) {
        fprintf (fp, "%d", *data);
        while (--cnt > 0) {
            data++;
            fprintf (fp, " %d", *data);
        }
        putc ('\n', fp);
    }
#ifdef LITTLE_ENDIAN
    else if (!is_little) {
#else
    else if (is_little) {
#endif
        while (cnt-- > 0) {
            fwrite (swap_bytes (sizeof(int), (void *)data),
                sizeof(int), 1, fp);
            data++;
        }
    }
    else
        fwrite (data, sizeof(int), cnt, fp);
}

/*--------------------------------------------------------------------*/

static void write_floats (FILE *fp, int cnt, float *data)
{
    if (is_ascii) {
        fprintf (fp, "%g", *data);
        while (--cnt > 0) {
            data++;
            fprintf (fp, " %g", *data);
        }
        putc ('\n', fp);
    }
#ifdef LITTLE_ENDIAN
    else if (!is_little) {
#else
    else if (is_little) {
#endif
        while (cnt-- > 0) {
            fwrite (swap_bytes (sizeof(float), (void *)data),
                sizeof(float), 1, fp);
            data++;
        }
    }
    else
        fwrite (data, sizeof(float), cnt, fp);
}

/*--------------------------------------------------------------------*/

static void write_doubles (FILE *fp, int cnt, double *data)
{
    if (is_ascii) {
        fprintf (fp, "%g", *data);
        while (--cnt > 0) {
            data++;
            fprintf (fp, " %g", *data);
        }
        putc ('\n', fp);
    }
#ifdef LITTLE_ENDIAN
    else if (!is_little) {
#else
    else if (is_little) {
#endif
        while (cnt-- > 0) {
            fwrite (swap_bytes (sizeof(double), (void *)data),
                sizeof(double), 1, fp);
            data++;
        }
    }
    else
        fwrite (data, sizeof(double), cnt, fp);
}

/*--------------------------------------------------------------------*/

static void count_elements ()
{
    int ns, nsect, nn, ip;
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn, *conn_offset;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33], errmsg[128];

    if (cg_nsections (cgFile, cgBase, cgZone, &nsect))
        err_exit ("cg_nsections", NULL);
    if (nsect < 1) err_exit (NULL, "no sections defined");

    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &nn, &ip))
            err_exit ("cg_section_read", NULL);
        ne = ie - is + 1;

        if (elemtype == CGNS_ENUMV(MIXED)) {
            if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
                err_exit ("cg_ElementDataSize", NULL);
            conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
            if (conn == NULL)
                err_exit (NULL, "malloc failed for element connectivity");

            conn_offset = (cgsize_t *) malloc ((size_t)(ne+1) * sizeof(cgsize_t));
            if (conn_offset == NULL)
                err_exit (NULL, "malloc failed for element connectivity offset");

            if (cg_poly_elements_read (cgFile, cgBase, cgZone, ns, conn, conn_offset, NULL))
                err_exit ("cg_poly_elements_read", NULL);

            for (i = 0, n = 0; n < ne; n++) {
                et = (CGNS_ENUMT(ElementType_t))conn[i++];
                switch (et) {
                    case CGNS_ENUMV(TRI_3):
                    case CGNS_ENUMV(TRI_6):
                        nTris++;
                        break;
                    case CGNS_ENUMV(TETRA_4):
                    case CGNS_ENUMV(TETRA_10):
                        nTets++;
                        break;
                    /* ignore these */
                    case CGNS_ENUMV(NODE):
                    case CGNS_ENUMV(BAR_2):
                    case CGNS_ENUMV(BAR_3):
                        break;
                    /* invalid */
                    default:
                        sprintf(errmsg,
                            "element type %s not allowed for FAST",
                            cg_ElementTypeName(et));
                        err_exit(NULL, errmsg);
                        break;
                }
                if (cg_npe(et, &nn) || nn <= 0)
                    err_exit("cg_npe", NULL);
                i += nn;
            }
            free (conn);
            free (conn_offset);
        }
        else {
            switch (elemtype) {
                case CGNS_ENUMV(TRI_3):
                case CGNS_ENUMV(TRI_6):
                    nTris += ne;
                    break;
                case CGNS_ENUMV(TETRA_4):
                case CGNS_ENUMV(TETRA_10):
                    nTets += ne;
                    break;
                /* ignore these */
                case CGNS_ENUMV(NODE):
                case CGNS_ENUMV(BAR_2):
                case CGNS_ENUMV(BAR_3):
                    break;
                /* invalid */
                default:
                    sprintf(errmsg,
                        "element type %s not allowed for FAST",
                        cg_ElementTypeName(elemtype));
                    err_exit(NULL, errmsg);
                    break;
            }
        }
    }
}

/*-------------------------------------------------------------------*/

static int compare_tris (void *v1, void *v2)
{
    TRI *t1 = (TRI *)v1;
    TRI *t2 = (TRI *)v2;
    int i, k;
    cgsize_t id, nn, n1[3], n2[3];

    for (i = 0; i < 3; i++) {
        id = t1->nodes[i];
        for (k = 0; k < i; k++) {
            if (n1[k] > id) {
                nn = n1[k];
                n1[k] = id;
                id = nn;
            }
        }
        n1[i] = id;
    }
    for (i = 0; i < 3; i++) {
        id = t2->nodes[i];
        for (k = 0; k < i; k++) {
            if (n2[k] > id) {
                nn = n2[k];
                n2[k] = id;
                id = nn;
            }
        }
        n2[i] = id;
    }

    for (i = 0; i < 3; i++) {
        if (n1[i] != n2[i])
            return (int)(n1[i] - n2[i]);
    }
    return 0;
}

/*-------------------------------------------------------------------*/

static size_t hash_tri (void *v)
{
    TRI *t = (TRI *)v;
    int n;
    size_t hash = 0;

    for (n = 0; n < 3; n++)
        hash += (size_t)t->nodes[n];
    return hash;
}

/*-------------------------------------------------------------------*/

static size_t get_tris (void *vt, void *vn)
{
    int *n = (int *)vn;

    Tris[*n] = (TRI *)vt;
    (*n)++;
    return 1;
}

/*--------------------------------------------------------------------*/

static int sort_tris (const void *v1, const void *v2)
{
    TRI **t1 = (TRI **)v1;
    TRI **t2 = (TRI **)v2;

    return (int)((*t1)->id - (*t2)->id);
}

/*--------------------------------------------------------------------*/

static void boundary_elements ()
{
    int ns, nsect, nn, ip, nf;
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn;
    cgsize_t *conn_offset;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33];
    TRI tri, *pt;
    HASH trihash;

    trihash = HashCreate(nTets > 1024 ? (size_t)nTets / 3 : 127,
                         compare_tris, hash_tri);
    if (NULL == trihash)
        err_exit(NULL, "hash table creation failed");

    if (cg_nsections (cgFile, cgBase, cgZone, &nsect))
        err_exit ("cg_nsections", NULL);

    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &nn, &ip))
            err_exit ("cg_section_read", NULL);
        if (elemtype != CGNS_ENUMV(MIXED) &&
            elemtype != CGNS_ENUMV(TETRA_4) &&
            elemtype != CGNS_ENUMV(TETRA_10)) continue;

        if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
            err_exit ("cg_ElementDataSize", NULL);
        conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
        if (conn == NULL)
            err_exit (NULL, "malloc failed for element connectivity");

        conn_offset = NULL;
        if (elemtype == CGNS_ENUMV(MIXED)) {
            conn_offset = (cgsize_t *) malloc ((size_t)(ie-is+2) * sizeof(cgsize_t));
            if (conn_offset == NULL)
                err_exit (NULL, "malloc failed for element connectivity offset");
            if (cg_poly_elements_read (cgFile, cgBase, cgZone, ns, conn, conn_offset, NULL))
                err_exit ("cg_poly_elements_read", NULL);
        }
        else {
            if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                err_exit ("cg_elements_read", NULL);
        }
        ne = ie - is + 1;
        et = elemtype;
        for (i = 0, n = 0; n < ne; n++) {
            if (elemtype == CGNS_ENUMV(MIXED))
                et = (CGNS_ENUMT(ElementType_t))conn[i++];
            if (et == CGNS_ENUMV(TETRA_4) ||
                et == CGNS_ENUMV(TETRA_10)) {
                for (nf = 0; nf < 4; nf++) {
                    for (nn = 0; nn < 3; nn++) {
                        tri.nodes[nn] = conn[tetfaces[nf][nn]];
                    }
                    pt = (TRI *)HashFind(trihash, &tri);
                    if (pt == NULL) {
                        pt = (TRI *)malloc(sizeof(TRI));
                        if (pt == NULL)
                            err_exit(NULL, "malloc failed for a new tri");
                        pt->id = 0;
                        pt->bcnum = 0;
                        for (nn = 0; nn < 3; nn++)
                            pt->nodes[nn] = tri.nodes[nn];
                        HashAdd(trihash, pt);
                    }
                    else {
                        HashDelete(trihash, pt);
                        free(pt);
                    }
                }
            }
            cg_npe(et, &nn);
            i += nn;
        }
        free(conn);
        if (conn_offset) free(conn_offset);
    }


    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &nn, &ip))
            err_exit ("cg_section_read", NULL);
        if (elemtype != CGNS_ENUMV(MIXED) &&
            elemtype != CGNS_ENUMV(TRI_3) &&
            elemtype != CGNS_ENUMV(TRI_6)) continue;

        if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
            err_exit ("cg_ElementDataSize", NULL);
        conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
        if (conn == NULL)
            err_exit (NULL, "malloc failed for element connectivity");
        conn_offset = NULL;
        if (elemtype == CGNS_ENUMV(MIXED)) {
            conn_offset = (cgsize_t *) malloc ((size_t)(ie-is+2) * sizeof(cgsize_t));
            if (conn_offset == NULL)
                err_exit (NULL, "malloc failed for element connectivity offset");
            if (cg_poly_elements_read (cgFile, cgBase, cgZone, ns, conn, conn_offset, NULL))
                err_exit ("cg_poly_elements_read", NULL);
        }
        else {
            if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                err_exit ("cg_elements_read", NULL);
        }

        ne = ie - is + 1;
        et = elemtype;
        for (i = 0, n = 0; n < ne; n++) {
            if (elemtype == CGNS_ENUMV(MIXED))
                et = (CGNS_ENUMT(ElementType_t))conn[i++];
            if (et == CGNS_ENUMV(TRI_3) ||
                et == CGNS_ENUMV(TRI_6)) {
                for (nn = 0; nn < 3; nn++) {
                    tri.nodes[nn] = conn[i+nn];
                }
                pt = (TRI *)HashFind(trihash, &tri);
                if (pt == NULL) {
                    pt = (TRI *)malloc(sizeof(TRI));
                    if (pt == NULL)
                        err_exit(NULL, "malloc failed for a new tri");
                    for (nn = 0; nn < 3; nn++)
                        pt->nodes[nn] = tri.nodes[nn];
                    HashAdd(trihash, pt);
                }
                pt->id = is + n;
                pt->bcnum = -ns;
            }
            cg_npe(et, &nn);
            i += nn;
        }
        free(conn);
        if (conn_offset) free(conn_offset);
    }

    nTris = HashSize(trihash);
    Tris = (TRI **)malloc(nTris * sizeof(TRI *));
    if (Tris == NULL)
        err_exit(NULL, "malloc failed for tri list");
    ne = 0;
    HashList(trihash, get_tris, &ne);
    HashDestroy(trihash, NULL);

    qsort(Tris, nTris, sizeof(TRI *), sort_tris);
}

/*--------------------------------------------------------------------*/

static int sort_points (const void *v1, const void *v2)
{
    return (int)(*((cgsize_t *)v1) - *((cgsize_t *)v2));
}

/*--------------------------------------------------------------------*/

static int find_point (cgsize_t id, cgsize_t np, cgsize_t *pts)
{
    cgsize_t lo = 0, hi = np - 1, mid;

    if (!np || id < pts[0]) return 0;
    if (id == pts[0]) return 1;
    if (!hi || id > pts[hi]) return 0;
    if (id == pts[hi]) return 1;

    while (lo <= hi) {
        mid = (lo + hi) >> 1;
        if (id == pts[mid]) return 1;
        if (id < pts[mid])
            hi = mid - 1;
        else
            lo = mid + 1;
    }
    return 0;
}

/*--------------------------------------------------------------------*/

static void boundary_tris (int nb, CGNS_ENUMT(PointSetType_t) ptype,
    CGNS_ENUMT(GridLocation_t) location, cgsize_t np, cgsize_t *ptset)
{
    cgsize_t nt, is, ie;
    int n;

    if (ptype == CGNS_ENUMV(PointRange) ||
        ptype == CGNS_ENUMV(PointList)) {
        if (location == CGNS_ENUMV(FaceCenter) ||
            location == CGNS_ENUMV(CellCenter)) {
            ptype = (ptype == CGNS_ENUMV(PointRange) ?
                 CGNS_ENUMV(ElementRange) : CGNS_ENUMV(ElementList));
        }
        else if (location != CGNS_ENUMV(Vertex)) {
            return;
        }
    }

    if (ptype == CGNS_ENUMV(PointRange)) {
        if (ptset[0] < ptset[1]) {
            is = ptset[0];
            ie = ptset[1];
        }
        else {
            is = ptset[1];
            ie = ptset[0];
        }
        for (nt = 0; nt < nTris; nt++) {
            for (n = 0; n < 3; n++) {
                if (Tris[nt]->nodes[n] < is ||
                    Tris[nt]->nodes[n] > ie) break;
            }
            if (n == 3) Tris[nt]->bcnum = nb;
        }
        return;
    }

    if (ptype == CGNS_ENUMV(PointList)) {
        qsort(ptset, (size_t)np, sizeof(cgsize_t), sort_points);
        for (nt = 0; nt < nTris; nt++) {
            for (n = 0; n < 3; n++) {
                if (!find_point(Tris[nt]->nodes[n], np, ptset))
                    break;
            }
            if (n == 3) Tris[nt]->bcnum = nb;
        }
        return;
    }

    if (ptype == CGNS_ENUMV(ElementRange)) {
        if (ptset[0] < ptset[1]) {
            is = ptset[0];
            ie = ptset[1];
        }
        else {
            is = ptset[1];
            ie = ptset[0];
        }
        for (nt = 0; nt < nTris; nt++) {
            if (Tris[nt]->id >= is && Tris[nt]->id <= ie)
                Tris[nt]->bcnum = nb;
        }
        return;
    }

    if (ptype == CGNS_ENUMV(ElementList)) {
        qsort(ptset, (size_t)np, sizeof(cgsize_t), sort_points);
        for (nt = 0; nt < nTris; nt++) {
            if (find_point(Tris[nt]->id, np, ptset))
                Tris[nt]->bcnum = nb;
        }
        return;
    }
}

/*--------------------------------------------------------------------*/

static void boundary_conditions ()
{
    int nb, ib, nrmlindex[3];
    cgsize_t is, np, *ptset;
    char name[33];
    CGNS_ENUMT(BCType_t) bctype;
    CGNS_ENUMT(PointSetType_t) ptype;
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(DataType_t) datatype;

    if (cg_nbocos (cgFile, cgBase, cgZone, &nBocos))
        err_exit ("cg_nbocos", NULL);

    if (nBocos) {
        Bocos = (BOCO *)malloc(nBocos * sizeof(BOCO));
        if (Bocos == NULL) err_exit(NULL, "malloc failed for bocos");

        for (nb = 1; nb <= nBocos; nb++) {
            if (cg_boco_info(cgFile, cgBase, cgZone, nb, name,
                    &bctype, &ptype, &np, nrmlindex, &is,
                    &datatype, &ib))
                err_exit("cg_boco_info", NULL);
            Bocos[nb-1].type = bctype;
            strcpy(Bocos[nb-1].name, name);
            if (cg_boco_gridlocation_read(cgFile, cgBase, cgZone,
                    nb, &location))
                err_exit("cg_boco_gridlocation_read", NULL);
            ptset = (cgsize_t *)malloc((size_t)np * sizeof(cgsize_t));
            if (ptset == NULL)
                err_exit(NULL, "malloc failed for boco ptset");
            if (cg_boco_read(cgFile, cgBase, cgZone, nb, ptset, 0))
                err_exit("cg_boco_read", NULL);

            boundary_tris(nb, ptype, location, np, ptset);
            free(ptset);
        }
    }

    for (np = 0; np < nTris; np++) {
        if (Tris[np]->bcnum < 0)
            Tris[np]->bcnum = nBocos - Tris[np]->bcnum;
    }
}

/*--------------------------------------------------------------------*/

static void write_coords (FILE *fp)
{
    int n;
    char name[33];
    cgsize_t imin, imax;

    imin = 1;
    imax = nCoords;

    if (is_single) {
        float *coord;
        coord = (float *)malloc((size_t)nCoords * sizeof(float));
        if (coord == NULL)
            err_exit(NULL, "malloc failed for coordinates");
        for (n = 0; n < 3; n++) {
            sprintf(name, "Coordinate%c", 'X' + n);
            if (cg_coord_read(cgFile, cgBase, cgZone, name,
                CGNS_ENUMV(RealSingle), &imin, &imax, coord))
                err_exit("cg_coord_read", NULL);
            write_floats(fp, (int)nCoords, coord);
        }
        free(coord);
    }
    else {
        double *coord;
        coord = (double *)malloc((size_t)nCoords * sizeof(double));
        if (coord == NULL)
            err_exit(NULL, "malloc failed for coordinates");
        for (n = 0; n < 3; n++) {
            sprintf(name, "Coordinate%c", 'X' + n);
            if (cg_coord_read(cgFile, cgBase, cgZone, name,
                CGNS_ENUMV(RealDouble), &imin, &imax, coord))
                err_exit("cg_coord_read", NULL);
            write_doubles(fp, (int)nCoords, coord);
        }
        free(coord);
    }
}

/*--------------------------------------------------------------------*/

static void write_tris (FILE *fp)
{
    cgsize_t nt;
    int n, tri[3], *bctags;

    bctags = (int *)malloc((size_t)nTris * sizeof(int));
    if (bctags == NULL)
        err_exit(NULL, "malloc failed for element list");

    for (nt = 0; nt < nTris; nt++) {
        for (n = 0; n < 3; n++)
            tri[n] = (int)Tris[nt]->nodes[n];
        write_ints(fp, 3, tri);
        bctags[nt] = Tris[nt]->bcnum;
    }

    write_ints(fp, (int)nTris, bctags);
    free(bctags);
}

/*--------------------------------------------------------------------*/

static void write_tets (FILE *fp)
{
    int ns, nsect, ip, tet[4];
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn, *conn_offset;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33];

    if (cg_nsections (cgFile, cgBase, cgZone, &nsect))
        err_exit ("cg_nsections", NULL);

    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &ip, &ip))
            err_exit ("cg_section_read", NULL);
        ne = ie - is + 1;
        if (elemtype == CGNS_ENUMV(TETRA_4) ||
            elemtype == CGNS_ENUMV(MIXED)) {
            if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
                err_exit ("cg_ElementDataSize", NULL);
            conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
            if (conn == NULL)
                err_exit (NULL, "malloc failed for element connectivity");
            conn_offset = NULL;
            if (elemtype == CGNS_ENUMV(MIXED)) {
                conn_offset = (cgsize_t *) malloc ((size_t)(ne+1) * sizeof(cgsize_t));
                if (conn_offset == NULL)
                    err_exit (NULL, "malloc failed for element connectivity");
                if (cg_poly_elements_read (cgFile, cgBase, cgZone, ns, conn, conn_offset, NULL))
                    err_exit ("cg_poly_elements_read", NULL);
            }
            else {
                if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                    err_exit ("cg_elements_read", NULL);
            }
            if (elemtype == CGNS_ENUMV(MIXED)) {
                for (i = 0, n = 0; n < ne; n++) {
                    et = (CGNS_ENUMT(ElementType_t))conn[i++];
                    if (et == CGNS_ENUMV(TETRA_4)) {
                        for (ip = 0; ip < 4; ip++)
                            tet[ip] = conn[i++];
                        write_ints(fp, 4, tet);
                    }
                    else {
                        if (cg_npe(et, &ip) || ip <= 0)
                            err_exit("cg_npe", NULL);
                        i += ip;
                    }
                }
            }
            else if (elemtype == CGNS_ENUMV(TETRA_4)) {
                for (i = 0, n = 0; n < ne; n++) {
                    for (ip = 0; ip < 4; ip++)
                        tet[ip] = conn[i++];
                    write_ints(fp, 4, tet);
                }
            }
            free (conn);
            if (conn_offset) free(conn_offset);
        }
    }
}

/*--------------------------------------------------------------------*/

static int get_bcnum (CGNS_ENUMT(BCType_t) bctype)
{
    switch (bctype) {
        case CGNS_ENUMV(BCInflowSupersonic):
            return 5025;
        case CGNS_ENUMV(BCOutflow):
        case CGNS_ENUMV(BCOutflowSupersonic):
        case CGNS_ENUMV(BCExtrapolate):
            return 5026;
        case CGNS_ENUMV(BCInflowSubsonic):
        case CGNS_ENUMV(BCTunnelInflow):
            return 7011;
        case CGNS_ENUMV(BCOutflowSubsonic):
            return 7012;
        case CGNS_ENUMV(BCSymmetryPlane):
            if (is_sym)
                return 6660 + is_sym;
            return 1;
        case CGNS_ENUMV(BCFarfield):
        case CGNS_ENUMV(BCInflow):
            return 5000;
        case CGNS_ENUMV(BCWall):
        case CGNS_ENUMV(BCWallViscous):
        case CGNS_ENUMV(BCWallViscousHeatFlux):
        case CGNS_ENUMV(BCWallViscousIsothermal):
            return 4000;
        case CGNS_ENUMV(BCWallInviscid):
            return 3000;
        case CGNS_ENUMV(BCTunnelOutflow):
            return 5051;
/*
        case CGNS_ENUMV(BCAxisymmetricWedge):
        case CGNS_ENUMV(BCDegenerateLine):
        case CGNS_ENUMV(BCDegeneratePoint):
        case CGNS_ENUMV(BCDirichlet):
        case CGNS_ENUMV(BCGeneral):
        case CGNS_ENUMV(BCNeumann):
        case CGNS_ENUMV(BCSymmetryPolar):
*/
        default:
            break;
    }
    return 0;
}

/*--------------------------------------------------------------------*/

int main (int argc, char *argv[])
{
    char name[33], *fastfile, *mapbc, *p;
    int n, nb, nz, idata[3];
    int celldim, phydim;
    CGNS_ENUMT(ZoneType_t) zonetype;
    FILE *fp;

    if (argc < 2)
        print_usage (usgmsg, NULL);

    cgBase = cgZone = 1;
    while ((n = getargs (argc, argv, options)) > 0) {
        switch (n) {
            case '4':
                is_single = 1;
                break;
            case '8':
                is_single = 0;
                break;
            case 'f':
                is_ascii = 1;
                break;
            case 's':
                is_ascii = 0;
                break;
            case 'l':
                is_little = 1;
                break;
            case 'b':
                is_little = 0;
                break;
            case 'x':
            case 'y':
            case 'z':
                is_sym = n - 'x' + 1;
                break;
            case 'B':
                cgBase = atoi(argarg);
                break;
            case 'Z':
                cgZone = atoi(argarg);
                break;
        }
    }

    if (argind > argc - 1)
        print_usage (usgmsg, "CGNSfile not given");
    if (ACCESS (argv[argind], 0))
        err_exit (NULL, "CGNSfile does not exist or is not a file");

    /* open CGNS file */

    printf ("reading CGNS file from \"%s\"\n", argv[argind]);
    fflush (stdout);
    if (cg_open (argv[argind], CG_MODE_READ, &cgFile))
        err_exit ("cg_open", NULL);

    /* get base */

    if (cg_nbases (cgFile, &nb))
        err_exit ("cg_nbases", NULL);
    if (nb == 0) err_exit (NULL, "no bases in the file");
    if (cgBase < 1 || cgBase > nb)
        err_exit (NULL, "specified base number out of range");
    if (cg_base_read (cgFile, cgBase, name, &celldim, &phydim))
        err_exit ("cg_base_read", NULL);
    printf ("using base %d - %s\n", cgBase, name);
    fflush (stdout);
    if (phydim != 3 || (celldim != 1 && celldim != 3))
        err_exit (NULL, "cell and/or physical dimension invalid");

    /* get zone */

    if (cg_nzones (cgFile, cgBase, &nz))
        err_exit ("cg_nzones", NULL);
    if (nz == 0) err_exit (NULL, "no zones under the base");
    if (cgZone < 1 || cgZone > nz)
        err_exit (NULL, "specified zone number out of range");
    if (cg_zone_read (cgFile, cgBase, cgZone, name, sizes))
        err_exit ("cg_zone_read", NULL);
    printf ("using zone %d - %s\n", cgZone, name);
    fflush (stdout);
    if (cg_zone_type (cgFile, cgBase, cgZone, &zonetype))
        err_exit ("cg_zone_type", NULL);
    if (zonetype != CGNS_ENUMV(Unstructured))
        err_exit (NULL, "not an Unstructured zone");

    /* get coord and element counts */

    nCoords = sizes[0];
    count_elements();

    printf("number coordinates = %ld\n", (long)nCoords);
    printf("number triangles   = %ld\n", (long)nTris);
    printf("number tetrahedra  = %ld\n", (long)nTets);

    /* verify dimensions fit in an integer */

    if ((3 * nTris) > CG_MAX_INT32 ||
        (4 * nTets) > CG_MAX_INT32)
        err_exit (NULL, "too many elements to write with integers");

    /* get boundary elements and BCs */

    boundary_elements();
    boundary_conditions();

    /* open output file */

    if (argind < argc - 1) {
        fastfile = argv[++argind];
    }
    else {
        fastfile = (char *)malloc(strlen(argv[argind]) + 12);
        if (fastfile == NULL)
            err_exit(NULL, "malloc failed for FAST filename");
        strcpy(fastfile, argv[argind]);
        p = strrchr(fastfile, '/');
#ifdef _WIN32
        if (p == NULL) p = strrchr(fastfile, '\\');
#endif
        if (p == NULL) p = fastfile;
        if ((p = strrchr(p, '.')) == NULL)
            p = fastfile + strlen(fastfile);
        if (!is_ascii) {
            *p++ = '.';
            if (is_little) *p++ = 'l';
            *p++ = 'b';
            *p++ = is_single ? '4' : '8';
        }
        strcpy(p, ".fgrid");
    }
    printf ("writing %s FAST data to \"%s\"\n",
        is_ascii ? "ASCII" : "binary", fastfile);
    if (NULL == (fp = fopen (fastfile, is_ascii ? "w+" : "w+b")))
        err_exit (NULL, "couldn't open output file for writing");

    /* write file */

    idata[0] = (int)nCoords;
    idata[1] = (int)nTris;
    idata[2] = (int)nTets;
    write_ints (fp, 3, idata);

    write_coords (fp);
    if (nTris) write_tris (fp);
    if (nTets) write_tets (fp);

    fclose (fp);
    cg_close (cgFile);

    if (nBocos) {
        if (argind < argc - 1) {
            mapbc = argv[++argind];
        }
        else {
            mapbc = (char *)malloc(strlen(fastfile) + 8);
            strcpy(mapbc, fastfile);
            p = strrchr(mapbc, '/');
#ifdef _WIN32
            if (p == NULL) p = strrchr(mapbc, '\\');
#endif
            if (p == NULL) p = mapbc;
            if ((p = strrchr(p, '.')) == NULL)
                p = mapbc + strlen(mapbc);
            strcpy(p, ".mapbc");
        }
        printf("writing boundary conditions to \"%s\"\n", mapbc);
        if ((fp = fopen(mapbc, "w+")) == NULL) {
            fprintf(stderr, "couldn't open \"%s\" for writing\n", mapbc);
        }
        else {
            fprintf(fp, "%d\n", nBocos);
            for (nb = 0; nb < nBocos; nb++) {
                fprintf(fp, "%d %d %s\n", nb+1,
                    get_bcnum(Bocos[nb].type), Bocos[nb].name);
            }
            fclose(fp);
        }
    }

    return 0;
}
