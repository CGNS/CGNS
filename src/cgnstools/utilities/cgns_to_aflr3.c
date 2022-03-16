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


#ifndef CGNS_ENUMV
# define CGNS_ENUMV(V) V
# define CGNS_ENUMT(T) T
#endif

#if defined(_WIN32) || defined(__CYGWIN__) || defined(__linux) || \
    defined(__alpha) || defined(__ultrix)
# ifndef LITTLE_ENDIAN
#  define LITTLE_ENDIAN
# endif
#endif

static char options[] = "48fslbxyzB:Z:";

static char *usgmsg[] = {
    "usage: cgns_to_aflr3 [options] CGNSfile [AFLR3file] [MAPBCfile]",
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
    "default is big-endian binary (Fortran stream) AFLR3 file",
    NULL
};

static int is_ascii = 0;
static int is_single = 0;
static int is_little = 0;
static int is_sym = 0;

static int cgFile = 0, cgBase, cgZone;
static cgsize_t sizes[9];

static cgsize_t nCoords = 1;
static cgsize_t nTris = 0;
static cgsize_t nQuads = 0;
static cgsize_t nTets = 0;
static cgsize_t nPyras = 0;
static cgsize_t nPrisms = 0;
static cgsize_t nHexas = 0;

static int is_structured = 0;

typedef struct {
    cgsize_t id;
    int bcnum;
    int nnodes;
    cgsize_t nodes[4];
} FACE;

static int nFaces;
static FACE **Faces;

typedef struct {
    CGNS_ENUMT(BCType_t) type;
    char name[33];
    char family[CG_MAX_GOTO_DEPTH * 33 + 1];
} BOCO;

static int nBocos = 0;
static BOCO *Bocos;

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

#define NODE_INDEX(I,J,K) ((I)+sizes[0]*(((J)-1)+sizes[1]*((K)-1)))

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

/*-------------------------------------------------------------------*/

static int compare_faces (void *v1, void *v2)
{
    FACE *f1 = (FACE *)v1;
    FACE *f2 = (FACE *)v2;
    int i, k;
    cgsize_t id, nn, n1[4], n2[4];

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
            return (int)(n1[i] - n2[i]);
    }
    return 0;
}

/*-------------------------------------------------------------------*/

static size_t hash_face (void *v)
{
    FACE *f = (FACE *)v;
    int n;
    size_t hash = 0;

    for (n = 0; n < f->nnodes; n++)
        hash += (size_t)f->nodes[n];
    return hash;
}

/*-------------------------------------------------------------------*/

static size_t get_faces (void *vf, void *vn)
{
    int *n = (int *)vn;

    Faces[*n] = (FACE *)vf;
    (*n)++;
    return 1;
}

/*-------------------------------------------------------------------*/

static FACE *new_face (cgsize_t faceid)
{
    FACE *face = (FACE *)malloc(sizeof(FACE));
    if (face == NULL)
        err_exit(NULL, "malloc failed for a new face");
    face->id = faceid;
    face->bcnum = 0;
    face->nnodes = 0;
    return face;
}

/*--------------------------------------------------------------------*/

static int sort_faces (const void *v1, const void *v2)
{
    FACE **f1 = (FACE **)v1;
    FACE **f2 = (FACE **)v2;

    return (int)((*f1)->id - (*f2)->id);
}

/*--------------------------------------------------------------------*/

static void count_elements ()
{
    int ns, nsect, nn, ip;
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn, *conn_offset;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33], errmsg[128];

    if (is_structured) {
        nQuads = 2 * (sizes[0] - 1) * (sizes[1] - 1) +
                 2 * (sizes[0] - 1) * (sizes[2] - 1) +
                 2 * (sizes[1] - 1) * (sizes[2] - 1);
        nHexas = (sizes[0] - 1) * (sizes[1] - 1) * (sizes[2] - 1);
        return;
    }

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
            conn_offset = (cgsize_t *) malloc ((size_t)(ne + 1) * sizeof(cgsize_t));
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
                    case CGNS_ENUMV(QUAD_4):
                    case CGNS_ENUMV(QUAD_8):
                    case CGNS_ENUMV(QUAD_9):
                        nQuads++;
                        break;
                    case CGNS_ENUMV(TETRA_4):
                    case CGNS_ENUMV(TETRA_10):
                        nTets++;
                        break;
                    case CGNS_ENUMV(PYRA_5):
                    case CGNS_ENUMV(PYRA_13):
                    case CGNS_ENUMV(PYRA_14):
                        nPyras++;
                        break;
                    case CGNS_ENUMV(PENTA_6):
                    case CGNS_ENUMV(PENTA_15):
                    case CGNS_ENUMV(PENTA_18):
                        nPrisms++;
                        break;
                    case CGNS_ENUMV(HEXA_8):
                    case CGNS_ENUMV(HEXA_20):
                    case CGNS_ENUMV(HEXA_27):
                        nHexas++;
                        break;
                    /* ignore these */
                    case CGNS_ENUMV(NODE):
                    case CGNS_ENUMV(BAR_2):
                    case CGNS_ENUMV(BAR_3):
                        break;
                    /* invalid */
                    default:
                        sprintf(errmsg,
                            "element type %s not allowed for AFLR3",
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
                case CGNS_ENUMV(QUAD_4):
                case CGNS_ENUMV(QUAD_8):
                case CGNS_ENUMV(QUAD_9):
                    nQuads += ne;
                    break;
                case CGNS_ENUMV(TETRA_4):
                case CGNS_ENUMV(TETRA_10):
                    nTets += ne;
                    break;
                case CGNS_ENUMV(PYRA_5):
                case CGNS_ENUMV(PYRA_13):
                case CGNS_ENUMV(PYRA_14):
                    nPyras += ne;
                    break;
                case CGNS_ENUMV(PENTA_6):
                case CGNS_ENUMV(PENTA_15):
                case CGNS_ENUMV(PENTA_18):
                    nPrisms += ne;
                    break;
                case CGNS_ENUMV(HEXA_8):
                case CGNS_ENUMV(HEXA_20):
                case CGNS_ENUMV(HEXA_27):
                    nHexas += ne;
                    break;
                /* ignore these */
                case CGNS_ENUMV(NODE):
                case CGNS_ENUMV(BAR_2):
                case CGNS_ENUMV(BAR_3):
                    break;
                /* invalid */
                default:
                    sprintf(errmsg,
                        "element type %s not allowed for AFLR3",
                        cg_ElementTypeName(elemtype));
                    err_exit(NULL, errmsg);
                    break;
            }
        }
    }
}

/*--------------------------------------------------------------------*/

static void structured_elements ()
{
    int i, j, k, nq = 0;
    int ni = (int)sizes[0];
    int nj = (int)sizes[1];
    int nk = (int)sizes[2];
    FACE *pf;

    nFaces = nQuads;
    Faces = (FACE **)malloc(nFaces * sizeof(FACE));
    if (Faces == NULL)
        err_exit("structured_elements", "malloc failed for quads");

    for (k = 1; k < nk; k++) {
        for (j = 1; j < nj; j++) {
            pf = new_face(0);
            pf->bcnum = -1;
            pf->nnodes = 4;
            pf->nodes[0] = NODE_INDEX(1, j,   k);
            pf->nodes[1] = NODE_INDEX(1, j,   k+1);
            pf->nodes[2] = NODE_INDEX(1, j+1, k+1);
            pf->nodes[3] = NODE_INDEX(1, j+1, k);
            Faces[nq++] = pf;
        }
    }
    for (k = 1; k < nk; k++) {
        for (j = 1; j < nj; j++) {
            pf = new_face(0);
            pf->bcnum = -2;
            pf->nnodes = 4;
            pf->nodes[0] = NODE_INDEX(ni, j,   k);
            pf->nodes[1] = NODE_INDEX(ni, j+1, k);
            pf->nodes[2] = NODE_INDEX(ni, j+1, k+1);
            pf->nodes[3] = NODE_INDEX(ni, j,   k+1);
            Faces[nq++] = pf;
        }
    }
    for (k = 1; k < nk; k++) {
        for (i = 1; i < ni; i++) {
            pf = new_face(0);
            pf->bcnum = -3;
            pf->nnodes = 4;
            pf->nodes[0] = NODE_INDEX(i,   1, k);
            pf->nodes[1] = NODE_INDEX(i+1, 1, k);
            pf->nodes[2] = NODE_INDEX(i+1, 1, k+1);
            pf->nodes[3] = NODE_INDEX(i,   1, k+1);
            Faces[nq++] = pf;
        }
    }
    for (k = 1; k < nk; k++) {
        for (i = 1; i < ni; i++) {
            pf = new_face(0);
            pf->bcnum = -4;
            pf->nnodes = 4;
            pf->nodes[0] = NODE_INDEX(i,   nj, k);
            pf->nodes[1] = NODE_INDEX(i,   nj, k+1);
            pf->nodes[2] = NODE_INDEX(i+1, nj, k+1);
            pf->nodes[3] = NODE_INDEX(i+1, nj, k);
            Faces[nq++] = pf;
        }
    }
    for (j = 1; j < nj; j++) {
        for (i = 1; i < ni; i++) {
            pf = new_face(0);
            pf->bcnum = -5;
            pf->nnodes = 4;
            pf->nodes[0] = NODE_INDEX(i,   j,   1);
            pf->nodes[1] = NODE_INDEX(i,   j+1, 1);
            pf->nodes[2] = NODE_INDEX(i+1, j+1, 1);
            pf->nodes[3] = NODE_INDEX(i+1, j,   1);
            Faces[nq++] = pf;
        }
    }
    for (j = 1; j < nj; j++) {
        for (i = 1; i < ni; i++) {
            pf = new_face(0);
            pf->bcnum = -6;
            pf->nnodes = 4;
            pf->nodes[0] = NODE_INDEX(i,   j,   nk);
            pf->nodes[1] = NODE_INDEX(i+1, j,   nk);
            pf->nodes[2] = NODE_INDEX(i+1, j+1, nk);
            pf->nodes[3] = NODE_INDEX(i,   j+1, nk);
            Faces[nq++] = pf;
        }
    }
}

/*--------------------------------------------------------------------*/

static void unstructured_elements ()
{
    int ns, nsect, nn, ip, nf, j;
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn, *conn_offset;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33];
    FACE face, *pf;
    static HASH facehash;

    ne = nTets + nPyras + nPrisms + nHexas;
    facehash = HashCreate(ne > 1024 ? (size_t)ne / 3 : 127,
                         compare_faces, hash_face);
    if (NULL == facehash)
        err_exit(NULL, "hash table creation failed");

    if (cg_nsections (cgFile, cgBase, cgZone, &nsect))
        err_exit ("cg_nsections", NULL);

    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &nn, &ip))
            err_exit ("cg_section_read", NULL);
        if (elemtype < CGNS_ENUMV(TETRA_4)) continue;
        ne = ie - is + 1;
        if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
            err_exit ("cg_ElementDataSize", NULL);
        conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
        if (conn == NULL)
            err_exit (NULL, "malloc failed for element connectivity");
        conn_offset = NULL;
        if (elemtype == CGNS_ENUMV(MIXED)) {
            conn_offset = (cgsize_t *) malloc ((size_t)(ne + 1)* sizeof(cgsize_t));
            if (conn_offset == NULL)
                err_exit (NULL, "malloc failed for element connectivity offset");
            if (cg_poly_elements_read (cgFile, cgBase, cgZone, ns, conn, conn_offset, NULL))
                err_exit ("cg_poly_elements_read", NULL);
        }
        else {
            if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                err_exit ("cg_elements_read", NULL);
        }
        et = elemtype;
        for (i = 0, n = 0; n < ne; n++) {
            if (elemtype == CGNS_ENUMV(MIXED))
                et = (CGNS_ENUMT(ElementType_t))conn[i++];
            switch (et) {
                case CGNS_ENUMV(TETRA_4):
                case CGNS_ENUMV(TETRA_10):
                    ip = 0;
                    nf = 4;
                    break;
                case CGNS_ENUMV(PYRA_5):
                case CGNS_ENUMV(PYRA_13):
                case CGNS_ENUMV(PYRA_14):
                    ip = 4;
                    nf = 5;
                    break;
                case CGNS_ENUMV(PENTA_6):
                case CGNS_ENUMV(PENTA_15):
                case CGNS_ENUMV(PENTA_18):
                    ip = 9;
                    nf = 5;
                    break;
                case CGNS_ENUMV(HEXA_8):
                case CGNS_ENUMV(HEXA_20):
                case CGNS_ENUMV(HEXA_27):
                    ip = 14;
                    nf = 6;
                    break;
                default:
                    nf = 0;
                    break;
            }
            for (j = 0; j < nf; j++) {
                face.nnodes = facenodes[ip+j][0];
                for (nn = 0; nn < face.nnodes; nn++)
                    face.nodes[nn] = conn[i+facenodes[ip+j][nn+1]];
                pf = (FACE *)HashFind(facehash, &face);
                if (NULL == pf) {
                    pf = new_face(0);
                    pf->nnodes = face.nnodes;
                    for (nn = 0; nn < face.nnodes; nn++)
                        pf->nodes[nn] = face.nodes[nn];
                    HashAdd(facehash, pf);
                }
                else {
                    HashDelete(facehash, pf);
                    free(pf);
                }
            }
            cg_npe (et, &nn);
            i += nn;
        }
        free (conn);
        if (conn_offset) free(conn_offset);
    }

    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &nn, &ip))
            err_exit ("cg_section_read", NULL);
        if (elemtype < CGNS_ENUMV(TRI_3) ||
           (elemtype > CGNS_ENUMV(QUAD_9) &&
            elemtype != CGNS_ENUMV(MIXED))) continue;
        ne = ie - is + 1;
        if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
            err_exit ("cg_ElementDataSize", NULL);
        conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
        if (conn == NULL)
            err_exit (NULL, "malloc failed for element connectivity");
        conn_offset = NULL;
        if (elemtype == CGNS_ENUMV(MIXED)) {
            conn_offset = (cgsize_t *) malloc ((size_t)(ne + 1)* sizeof(cgsize_t));
            if (conn_offset == NULL)
                err_exit (NULL, "malloc failed for element connectivity offset");
            if (cg_poly_elements_read (cgFile, cgBase, cgZone, ns, conn, conn_offset, NULL))
                err_exit ("cg_poly_elements_read", NULL);
        }
        else {
            if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                err_exit ("cg_elements_read", NULL);
        }

        et = elemtype;
        for (i = 0, n = 0; n < ne; n++) {
            if (elemtype == CGNS_ENUMV(MIXED))
                et = (CGNS_ENUMT(ElementType_t))conn[i++];
            switch (et) {
                case CGNS_ENUMV(TRI_3):
                case CGNS_ENUMV(TRI_6):
                    nf = 3;
                    break;
                case CGNS_ENUMV(QUAD_4):
                case CGNS_ENUMV(QUAD_8):
                case CGNS_ENUMV(QUAD_9):
                    nf = 4;
                    break;
                default:
                    nf = 0;
                    break;
            }
            if (nf) {
                face.nnodes = nf;
                for (nn = 0; nn < face.nnodes; nn++)
                    face.nodes[nn] = conn[i+nn];
                pf = (FACE *)HashFind(facehash, &face);
                if (NULL == pf) {
                    pf = new_face(0);
                    pf->nnodes = face.nnodes;
                    for (nn = 0; nn < face.nnodes; nn++)
                        pf->nodes[nn] = face.nodes[nn];
                    HashAdd(facehash, pf);
                }
                pf->id = is + n;
                pf->bcnum = -ns;
            }
            cg_npe (et, &nn);
            i += nn;
        }
        free (conn);
        if (conn_offset) free(conn_offset);
    }

    nFaces = HashSize(facehash);
    Faces = (FACE **)malloc(nFaces * sizeof(FACE *));
    if (Faces == NULL)
        err_exit(NULL, "malloc failed for face list");
    ne = 0;
    HashList(facehash, get_faces, &ne);
    HashDestroy(facehash, NULL);

    qsort(Faces, nFaces, sizeof(FACE *), sort_faces);

    nTris = nQuads = 0;
    for (nf = 0; nf < nFaces; nf++) {
        if (Faces[nf]->nnodes == 4)
            nQuads++;
        else
            nTris++;
    }
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

static void structured_boundary (int nb,
    CGNS_ENUMT(PointSetType_t) ptype,
    CGNS_ENUMT(GridLocation_t) location,
    cgsize_t np, cgsize_t *ptset)
{
    int n, nf;
    cgsize_t *pts;

    if (ptype != CGNS_ENUMV(PointRange) &&
        ptype != CGNS_ENUMV(PointList)) return;
    if (location != CGNS_ENUMV(Vertex)) return;

    if (ptype == CGNS_ENUMV(PointRange)) {
        int i, j, k, rng[2][3];
        np = 1;
        for (n = 0; n < 3; n++) {
            if (ptset[n] > ptset[n+3]) {
                rng[0][n] = ptset[n+3];
                rng[1][n] = ptset[n];
            }
            else {
                rng[0][n] = ptset[n];
                rng[1][n] = ptset[n+3];
            }
            np *= (rng[1][n] - rng[0][n] + 1);
        }
        if (np <= 0) return;
        pts = (cgsize_t *)malloc((size_t)np * sizeof(cgsize_t));
        if (pts == NULL) return;
        n = 0;
        for (k = rng[0][2]; k <= rng[1][2]; k++) {
            for (j = rng[0][1]; j <= rng[1][1]; j++) {
                for (i = rng[0][0]; i <= rng[1][0]; i++) {
                    pts[n++] = NODE_INDEX(i, j, k);
                }
            }
        }
    }
    else {
        cgsize_t nn;
        pts = (cgsize_t *)malloc((size_t)np * sizeof(cgsize_t));
        if (pts == NULL) return;
        for (n = 0, nn = 0; nn < np; nn++) {
            pts[nn] = NODE_INDEX(ptset[n], ptset[n+1], ptset[n+2]);
            n += 3;
        }
    }

    qsort(pts, (size_t)np, sizeof(cgsize_t), sort_points);
    for (nf = 0; nf < nFaces; nf++) {
        for (n = 0; n < 4; n++) {
            if (!find_point(Faces[nf]->nodes[n], np, pts))
                break;
        }
        if (n == 4)
            Faces[nf]->bcnum = nb;
    }

    free(pts);
}

/*--------------------------------------------------------------------*/

static void unstructured_boundary (int nb,
    CGNS_ENUMT(PointSetType_t) ptype,
    CGNS_ENUMT(GridLocation_t) location,
    cgsize_t np, cgsize_t *ptset)
{
    cgsize_t nf, is, ie;
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
        for (nf = 0; nf < nFaces; nf++) {
            for (n = 0; n < Faces[nf]->nnodes; n++) {
                if (Faces[nf]->nodes[n] < is ||
                    Faces[nf]->nodes[n] > ie) break;
            }
            if (n == Faces[nf]->nnodes)
                Faces[nf]->bcnum = nb;
        }
        return;
    }

    if (ptype == CGNS_ENUMV(PointList)) {
        qsort(ptset, (size_t)np, sizeof(cgsize_t), sort_points);
        for (nf = 0; nf < nFaces; nf++) {
            for (n = 0; n < Faces[nf]->nnodes; n++) {
                if (!find_point(Faces[nf]->nodes[n], np, ptset))
                    break;
            }
            if (n == Faces[nf]->nnodes)
                Faces[nf]->bcnum = nb;
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
        for (nf = 0; nf < nFaces; nf++) {
            if (Faces[nf]->id >= is && Faces[nf]->id <= ie)
                Faces[nf]->bcnum = nb;
        }
        return;
    }

    if (ptype == CGNS_ENUMV(ElementList)) {
        qsort(ptset, (size_t)np, sizeof(cgsize_t), sort_points);
        for (nf = 0; nf < nFaces; nf++) {
            if (find_point(Faces[nf]->id, np, ptset))
                Faces[nf]->bcnum = nb;
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
    int dim = is_structured ? 3 : 1;

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
            Bocos[nb - 1].family[0] = '\0';
            strcpy(Bocos[nb-1].name, name);
            if (cg_boco_gridlocation_read(cgFile, cgBase, cgZone,
                    nb, &location))
                err_exit("cg_boco_gridlocation_read", NULL);
            ptset = (cgsize_t *)malloc((size_t)(np * dim) * sizeof(cgsize_t));
            if (ptset == NULL)
                err_exit(NULL, "malloc failed for boco ptset");
            if (cg_boco_read(cgFile, cgBase, cgZone, nb, ptset, 0))
                err_exit("cg_boco_read", NULL);
            if (bctype == CGNS_ENUMV(FamilySpecified)) {
                cg_goto(cgFile, cgBase, "Zone_t", cgZone, "ZoneBC_t", 1 , "BC_t", nb, NULL);
                cg_famname_read(Bocos[nb - 1].family);
            }
            if (is_structured)
                structured_boundary(nb, ptype, location, np, ptset);
            else
                unstructured_boundary(nb, ptype, location, np, ptset);
            free(ptset);
        }

        /* correction for FamilySpecified */
        for (nb = 0; nb < nBocos; nb++) {
            if (Bocos[nb].type != CGNS_ENUMV(FamilySpecified)) {
                continue;
            }
            if (strlen(Bocos[nb].family) > 0 && Bocos[nb].family[0] == '/') {
                if (cg_gopath(cgFile, Bocos[nb].family) != CG_OK) {
                    continue;
                }
            }
            else {
                if (cg_goto(cgFile, cgBase, Bocos[nb].family, 0, NULL) != CG_OK) {
                    continue;
                }
            }
            if (cg_node_fambc_read(1, name, &Bocos[nb].type) != CG_OK) {
                continue;
            }
        }
    }

    for (np = 0; np < nFaces; np++) {
        if (Faces[np]->bcnum < 0)
            Faces[np]->bcnum = nBocos - Faces[np]->bcnum;
    }
}

/*--------------------------------------------------------------------*/

static void write_coords (FILE *fp)
{
    int n, idim;
    cgsize_t i, imin[3], imax[3];

    idim = is_structured ? 3 : 1;
    for (n = 0; n < idim; n++) {
        imin[n] = 1;
        imax[n] = sizes[n];
    }

    if (is_single) {
        float *x, *y, *z, coord[3];
        x = (float *)malloc((size_t)nCoords * sizeof(float));
        y = (float *)malloc((size_t)nCoords * sizeof(float));
        z = (float *)malloc((size_t)nCoords * sizeof(float));
        if (x == NULL || y == NULL || z == NULL)
            err_exit(NULL, "malloc failed for coordinates");
        if (cg_coord_read(cgFile, cgBase, cgZone, "CoordinateX",
                CGNS_ENUMV(RealSingle), imin, imax, x) ||
            cg_coord_read(cgFile, cgBase, cgZone, "CoordinateY",
                CGNS_ENUMV(RealSingle), imin, imax, y) ||
            cg_coord_read(cgFile, cgBase, cgZone, "CoordinateZ",
                CGNS_ENUMV(RealSingle), imin, imax, z))
            err_exit("cg_coord_read", NULL);
        for (i = 0; i < nCoords; i++) {
            coord[0] = x[i];
            coord[1] = y[i];
            coord[2] = z[i];
            write_floats(fp, 3, coord);
        }
        free(x);
        free(y);
        free(z);
    }
    else {
        double *x, *y, *z, coord[3];
        x = (double *)malloc((size_t)nCoords * sizeof(double));
        y = (double *)malloc((size_t)nCoords * sizeof(double));
        z = (double *)malloc((size_t)nCoords * sizeof(double));
        if (x == NULL || y == NULL || z == NULL)
            err_exit(NULL, "malloc failed for coordinates");
        if (cg_coord_read(cgFile, cgBase, cgZone, "CoordinateX",
                CGNS_ENUMV(RealDouble), imin, imax, x) ||
            cg_coord_read(cgFile, cgBase, cgZone, "CoordinateY",
                CGNS_ENUMV(RealDouble), imin, imax, y) ||
            cg_coord_read(cgFile, cgBase, cgZone, "CoordinateZ",
                CGNS_ENUMV(RealDouble), imin, imax, z))
            err_exit("cg_coord_read", NULL);
        for (i = 0; i < nCoords; i++) {
            coord[0] = x[i];
            coord[1] = y[i];
            coord[2] = z[i];
            write_doubles(fp, 3, coord);
        }
        free(x);
        free(y);
        free(z);
    }
}

/*--------------------------------------------------------------------*/

static void write_structured (FILE *fp)
{
    int i, j, k, n;
    int ni = (int)sizes[0];
    int nj = (int)sizes[1];
    int nk = (int)sizes[2];
    int quad[4], hexa[8], *bctags;

    bctags = (int *)malloc((size_t)nQuads * sizeof(int));
    if (bctags == NULL)
        err_exit(NULL, "malloc failed for BC tags");

    for (n = 0; n < nQuads; n++) {
        for (i = 0; i < 4; i++)
            quad[i] = (int)Faces[n]->nodes[i];
        write_ints(fp, 4, quad);
        bctags[n] = Faces[n]->bcnum;
    }
    write_ints(fp, (int)nQuads, bctags);
    free(bctags);

    for (k = 1; k < nk; k++) {
        for (j = 1; j < nj; j++) {
            for (i = 1; i < ni; i++) {
                hexa[0] = NODE_INDEX(i,   j,   k);
                hexa[1] = NODE_INDEX(i+1, j,   k);
                hexa[2] = NODE_INDEX(i+1, j+1, k);
                hexa[3] = NODE_INDEX(i,   j+1, k);
                hexa[4] = NODE_INDEX(i,   j,   k+1);
                hexa[5] = NODE_INDEX(i+1, j,   k+1);
                hexa[6] = NODE_INDEX(i+1, j+1, k+1);
                hexa[7] = NODE_INDEX(i,   j+1, k+1);
                write_ints(fp, 8, hexa);
                n++;
            }
        }
    }
}

/*--------------------------------------------------------------------*/

static void write_boundary (FILE *fp)
{
    int i, j, n;
    int face[4], *bctags;

    bctags = (int *)malloc((size_t)nFaces * sizeof(int));
    if (bctags == NULL)
        err_exit(NULL, "malloc failed for BC tags");

    n = 0;
    if (nTris) {
        for (i = 0; i < nFaces; i++) {
            if (Faces[i]->nnodes == 3) {
                for (j = 0; j < 3; j++)
                    face[j] = (int)Faces[i]->nodes[j];
                write_ints(fp, 3, face);
                bctags[n++] = Faces[i]->bcnum;
            }
        }
    }
    if (nQuads) {
        for (i = 0; i < nFaces; i++) {
            if (Faces[i]->nnodes == 4) {
                for (j = 0; j < 4; j++)
                    face[j] = (int)Faces[i]->nodes[j];
                write_ints(fp, 4, face);
                bctags[n++] = Faces[i]->bcnum;
            }
        }
    }
    write_ints(fp, (int)nFaces, bctags);
    free(bctags);
}

/*--------------------------------------------------------------------*/

static void write_elements (FILE *fp, CGNS_ENUMT(ElementType_t) type)
{
    int ns, nsect, nn, ip;
    int nnodes, elem[8];
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn, *conn_offset;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33];

    if (cg_nsections (cgFile, cgBase, cgZone, &nsect))
        err_exit ("cg_nsections", NULL);
    cg_npe(type, &nnodes);

    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &ip, &ip))
            err_exit ("cg_section_read", NULL);
        if (elemtype != type &&
            elemtype != CGNS_ENUMV(MIXED)) continue;

        ne = ie - is + 1;
        if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
            err_exit ("cg_ElementDataSize", NULL);
        conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
        if (conn == NULL)
            err_exit (NULL, "malloc failed for element connectivity");
        conn_offset = NULL;
        if (elemtype == CGNS_ENUMV(MIXED)) {
            conn_offset = (cgsize_t *) malloc ((size_t)(ne + 1)* sizeof(cgsize_t));
            if (conn_offset == NULL)
                err_exit (NULL, "malloc failed for element connectivity offset");
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
                if (et == type) {
                    if (type == CGNS_ENUMV(PYRA_5)) {
                        elem[0] = (int)conn[i];
                        elem[1] = (int)conn[i+3];
                        elem[2] = (int)conn[i+4];
                        elem[3] = (int)conn[i+1];
                        elem[4] = (int)conn[i+2];
                    }
                    else {
                        for (nn = 0; nn < nnodes; nn++)
                            elem[nn] = (int)conn[i+nn];
                    }
                    write_ints(fp, nnodes, elem);
                }
                if (cg_npe(et, &nn) || nn <= 0)
                    err_exit("cg_npe", NULL);
                i += nn;
            }
        }
        else if (type == CGNS_ENUMV(PYRA_5)) {
            for (i = 0, n = 0; n < ne; n++) {
                elem[0] = (int)conn[i];
                elem[1] = (int)conn[i+3];
                elem[2] = (int)conn[i+4];
                elem[3] = (int)conn[i+1];
                elem[4] = (int)conn[i+2];
                write_ints(fp, 5, elem);
                i += 5;
            }
        }
        else {
            for (i = 0, n = 0; n < ne; n++) {
                for (nn = 0; nn < nnodes; nn++)
                    elem[nn] = (int)conn[i++];
                write_ints(fp, nnodes, elem);
            }
        }
        free (conn);
        if (conn_offset) free(conn_offset);
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
    char name[33], *outfile, *mapbc, *p;
    int n, nb, nz, idata[7];
    int celldim, phydim, idim;
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
    is_structured = (zonetype == CGNS_ENUMV(Structured));

    /* get coord and element counts */

    idim = is_structured ? 3 : 1;
    for (n = 0; n < idim; n++)
        nCoords *= sizes[n];
    count_elements();

    printf("number coords = %ld\n", (long)nCoords);
    printf("number tris   = %ld\n", (long)nTris);
    printf("number quads  = %ld\n", (long)nQuads);
    printf("number tets   = %ld\n", (long)nTets);
    printf("number pyras  = %ld\n", (long)nPyras);
    printf("number pentas = %ld\n", (long)nPrisms);
    printf("number hexas  = %ld\n", (long)nHexas);

    /* verify dimensions fit in an integer */

    if ((3 * nTris + 4 * nQuads) > CG_MAX_INT32 ||
        (4 * nTets) > CG_MAX_INT32 ||
        (5 * nPyras) > CG_MAX_INT32 ||
        (6 * nPrisms) > CG_MAX_INT32 ||
        (4 * nHexas) > CG_MAX_INT32)
        err_exit (NULL, "too many elements to write with integers");

    if (is_structured)
        structured_elements();
    else
        unstructured_elements();
    boundary_conditions();
    
    /* open output file */

    if (argind < argc - 1) {
        outfile = argv[++argind];
    }
    else {
        outfile = (char *)malloc(strlen(argv[argind]) + 12);
        if (outfile == NULL)
            err_exit(NULL, "malloc failed for AFLR3 filename");
        strcpy(outfile, argv[argind]);
        p = strrchr(outfile, '/');
#ifdef _WIN32
        if (p == NULL) p = strrchr(outfile, '\\');
#endif
        if (p == NULL) p = outfile;
        if ((p = strrchr(p, '.')) == NULL)
            p = outfile + strlen(outfile);
        if (!is_ascii) {
            *p++ = '.';
            if (is_little) *p++ = 'l';
            *p++ = 'b';
            *p++ = is_single ? '4' : '8';
        }
        strcpy(p, ".ugrid");
    }
    printf ("writing %s AFLR3 data to \"%s\"\n",
        is_ascii ? "ASCII" : "binary", outfile);
    if (NULL == (fp = fopen (outfile, is_ascii ? "w+" : "w+b")))
        err_exit (NULL, "couldn't open output file for writing");

    /* write file */

    idata[0] = (int)nCoords;
    idata[1] = (int)nTris;
    idata[2] = (int)nQuads;
    idata[3] = (int)nTets;
    idata[4] = (int)nPyras;
    idata[5] = (int)nPrisms;
    idata[6] = (int)nHexas;
    write_ints (fp, 7, idata);

    write_coords (fp);
    if (is_structured) {
        write_structured(fp);
    }
    else {
        if (nFaces)  write_boundary(fp);
        if (nTets)   write_elements(fp, CGNS_ENUMV(TETRA_4));
        if (nPyras)  write_elements(fp, CGNS_ENUMV(PYRA_5));
        if (nPrisms) write_elements(fp, CGNS_ENUMV(PENTA_6));
        if (nHexas)  write_elements(fp, CGNS_ENUMV(HEXA_8));
    }

    fclose (fp);
    cg_close (cgFile);

    if (nBocos) {
        if (argind < argc - 1) {
            mapbc = argv[++argind];
        }
        else {
            mapbc = (char *)malloc(strlen(outfile) + 8);
            strcpy(mapbc, outfile);
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
