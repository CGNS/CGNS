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

#include "getargs.h"
#include "cgnslib.h"

#ifndef CGNS_ENUMV
# define CGNS_ENUMV(V) V
# define CGNS_ENUMT(T) T
#endif

#if defined(_WIN32) || defined(__BORLANDC__) || defined(__GO32__) || \
    defined(__alpha) || defined(ultrix) || defined(linux)
# ifndef LITTLE_ENDIAN
#  define LITTLE_ENDIAN
# endif
#endif

static char options[] = "48fslbB:Z:";

static char *usgmsg[] = {
    "usage: cgns_to_fast [options] CGNSfile FASTfile [MAPBCfile]",
    "  options are:",
    "   -4       = write coordinates as real*4 (default Real*8)",
    "   -8       = write coordinates as real*8 (double - default)",
    "   -f       = formatted (ASCII) file format",
    "   -s       = Fortran stream format (binary - default)",
    "   -l       = little-endian format",
    "   -b       = big-endian format (default)",
    "   -B<base> = use CGNS base number <base> (default 1)",
    "   -Z<zone> = zone number (default 1)",
    "default is big-endian binary (Fortran stream) AFLR3 file",
    NULL
};

typedef struct {
    cgsize_t start, end;
    int ntris, ntets;
} ELEMSET;

int nSets;
ELEMSET *Sets;

static int is_ascii = 0;
static int is_single = 0;
static int is_little = 0;

static int cgFile = 0, cgBase, cgZone;

static cgsize_t nCoords = 0;
static cgsize_t nTris = 0;
static cgsize_t nTets = 0;

int *BCtags;

static cgsize_t sizes[9];

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
    int ns, nsect, nn, ip, ntris, ntets;
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33], errmsg[128];

    if (cg_nsections (cgFile, cgBase, cgZone, &nsect))
        err_exit ("cg_nsections", NULL);
    if (nsect < 1) err_exit (NULL, "no sections defined");

    nSets = nsect;
    Sets = (ELEMSET *)malloc(nSets * sizeof(ELEMSET));
    if (Sets == NULL)
        err_exit (NULL, "malloc failed for elements sets");

    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &nn, &ip))
            err_exit ("cg_section_read", NULL);
        ne = ie - is + 1;
        Sets[ns-1].start = is;
        Sets[ns-1].end = ie;

        ntris = ntets = 0;
        if (elemtype == CGNS_ENUMV(MIXED)) {
            if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
                err_exit ("cg_ElementDataSize", NULL);
            conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
            if (conn == NULL)
                err_exit (NULL, "malloc failed for element connectivity");
           if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                err_exit ("cg_elements_read", NULL);
            for (i = 0, n = 0; n < ne; n++) {
                et = (int)conn[i++];
                switch (et) {
                    case CGNS_ENUMV(TRI_3):
                        ntris++;
                        i += 3;
                        break;
                    case CGNS_ENUMV(TETRA_4):
                        ntets++;
                        i += 4;
                        break;
                    default:
                        sprintf(errmsg, "element type %s not allowed",
                            cg_ElementTypeName(et));
                        err_exit(NULL, errmsg);
                        break;
                }
            }
            free (conn);
        }
        else {
            switch (elemtype) {
                case CGNS_ENUMV(TRI_3):
                    ntris += ne;
                    break;
                case CGNS_ENUMV(TETRA_4):
                    ntets += ne;
                    break;
                default:
                    sprintf(errmsg, "element type %s not allowed",
                        cg_ElementTypeName(elemtype));
                    err_exit(NULL, errmsg);
                    break;
            }
        }
        Sets[ns-1].ntris = ntris;
        Sets[ns-1].ntets = ntets;
        nTris += ntris;
        nTets += ntets;
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

static void get_bcs ()
{
}

/*--------------------------------------------------------------------*/

static void write_tris (FILE *fp)
{
    int ns, nsect, nn, ip, nt, *elems;
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33];

    elems = (int *)malloc((size_t)nTris * 3 * sizeof(int));
    BCtags = (int *)malloc((size_t)nTris * sizeof(int));
    if (elems == NULL || BCtags == NULL)
        err_exit(NULL, "malloc failed for element list");

    if (cg_nsections (cgFile, cgBase, cgZone, &nsect))
        err_exit ("cg_nsections", NULL);

    nn = nt = 0;
    for (ns = 1; ns <= nsect; ns++) {
        if (cg_section_read (cgFile, cgBase, cgZone, ns,
                name, &elemtype, &is, &ie, &ip, &ip))
            err_exit ("cg_section_read", NULL);
        ne = ie - is + 1;
        if (elemtype == CGNS_ENUMV(TRI_3) ||
            elemtype == CGNS_ENUMV(MIXED)) {
            if (cg_ElementDataSize (cgFile, cgBase, cgZone, ns, &size))
                err_exit ("cg_ElementDataSize", NULL);
            conn = (cgsize_t *) malloc ((size_t)size * sizeof(cgsize_t));
            if (conn == NULL)
                err_exit (NULL, "malloc failed for element connectivity");
           if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                err_exit ("cg_elements_read", NULL);
            if (elemtype == CGNS_ENUMV(MIXED)) {
                for (i = 0, n = 0; n < ne; n++) {
                    et = (CGNS_ENUMT(ElementType_t))conn[i++];
                    if (et == CGNS_ENUMV(TRI_3)) {
                        BCtags[nt++] = ns;
                        for (ip = 0; ip < 3; ip++)
                            elems[nn++] = conn[i++];
                    }
                    else {
                        if (cg_npe(et, &ip) || ip <= 0)
                            err_exit("cg_npe", NULL);
                        i += ip;
                    }
                }
            }
            else if (elemtype == CGNS_ENUMV(TRI_3)) {
                for (i = 0, n = 0; n < ne; n++) {
                    BCtags[nt++] = ns;
                    for (ip = 0; ip < 3; ip++)
                        elems[nn++] = conn[i++];
                }
            }
            free (conn);
        }
    }
    write_ints(fp, nn, elems);
    free(elems);

    get_bcs();
    write_ints(fp, (int)nTris, BCtags);
    free(BCtags);
}

/*--------------------------------------------------------------------*/

static void write_tets (FILE *fp)
{
    int ns, nsect, nn, ip, *elems;
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33];

    elems = (int *)malloc((size_t)nTets * 4 * sizeof(int));
    if (elems == NULL)
        err_exit(NULL, "malloc failed for element list");

    if (cg_nsections (cgFile, cgBase, cgZone, &nsect))
        err_exit ("cg_nsections", NULL);

    nn = 0;
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
           if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                err_exit ("cg_elements_read", NULL);
            if (elemtype == CGNS_ENUMV(MIXED)) {
                for (i = 0, n = 0; n < ne; n++) {
                    et = (CGNS_ENUMT(ElementType_t))conn[i++];
                    if (et == CGNS_ENUMV(TETRA_4)) {
                        for (ip = 0; ip < 4; ip++)
                            elems[nn++] = conn[i++];
                    }
                    else {
                        if (cg_npe(et, &ip) || ip <= 0)
                            err_exit("cg_npe", NULL);
                        i += ip;
                    }
                }
            }
            else if (elemtype == CGNS_ENUMV(TETRA_4)) {
                for (n = 0; n < ne * 4; n++)
                    elems[nn++] = conn[n];
            }
            free (conn);
        }
    }

    write_ints(fp, nn, elems);
    free(elems);
}

/*--------------------------------------------------------------------*/

int main (int argc, char *argv[])
{
    char name[33];
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
            case 'B':
                cgBase = atoi(argarg);
                break;
            case 'Z':
                cgZone = atoi(argarg);
                break;
        }
    }

    if (argind > argc - 2)
        print_usage (usgmsg, "CGNSfile and/or FASTfile not given");
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

    /* open output file */

    printf ("writing %s FAST data to <%s>\n",
        is_ascii ? "ASCII" : "binary", argv[++argind]);
    if (NULL == (fp = fopen (argv[argind], is_ascii ? "w+" : "w+b")))
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
    return 0;
}
