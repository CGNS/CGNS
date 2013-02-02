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
# ifndef BIG_ENDIAN
#  define BIG_ENDIAN
# endif
#endif

static char options[] = "48fslbB:Z:";

static char *usgmsg[] = {
    "usage: cgns_to_aflr3 [options] CGNSfile AFLR3file [MAPBCfile]",
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

static int is_ascii = 0;
static int is_single = 0;
static int is_little = 0;

static int cgFile = 0, cgBase, cgZone;

static cgsize_t nCoords = 1;
static cgsize_t nTris = 0;
static cgsize_t nQuads = 0;
static cgsize_t nTets = 0;
static cgsize_t nPyras = 0;
static cgsize_t nPrisms = 0;
static cgsize_t nHexas = 0;

static int is_structured = 0;
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
#ifdef BIG_ENDIAN
    else if (is_little) {
#else
    else if (!is_little) {
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
#ifdef BIG_ENDIAN
    else if (is_little) {
#else
    else if (!is_little) {
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
#ifdef BIG_ENDIAN
    else if (is_little) {
#else
    else if (!is_little) {
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
    cgsize_t size, *conn;
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
           if (cg_elements_read (cgFile, cgBase, cgZone, ns, conn, NULL))
                err_exit ("cg_elements_read", NULL);
            for (i = 0, n = 0; n < ne; n++) {
                et = (int)conn[i++];
                switch (et) {
                    case CGNS_ENUMV(TRI_3):
                        nTris++;
                        i += 3;
                        break;
                    case CGNS_ENUMV(QUAD_4):
                        nQuads++;
                        i += 4;
                        break;
                    case CGNS_ENUMV(TETRA_4):
                        nTets++;
                        i += 4;
                        break;
                    case CGNS_ENUMV(PYRA_5):
                        nPyras++;
                        i += 5;
                        break;
                    case CGNS_ENUMV(PENTA_6):
                        nPrisms++;
                        i += 6;
                        break;
                    case CGNS_ENUMV(HEXA_8):
                        nHexas++;
                        i += 8;
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
                    nTris += ne;
                    break;
                case CGNS_ENUMV(QUAD_4):
                    nQuads += ne;
                    break;
                case CGNS_ENUMV(TETRA_4):
                    nTets += ne;
                    break;
                case CGNS_ENUMV(PYRA_5):
                    nPyras += ne;
                    break;
                case CGNS_ENUMV(PENTA_6):
                    nPrisms += ne;
                    break;
                case CGNS_ENUMV(HEXA_8):
                    nHexas += ne;
                    break;
                default:
                    sprintf(errmsg, "element type %s not allowed",
                        cg_ElementTypeName(elemtype));
                    err_exit(NULL, errmsg);
                    break;
            }
        }
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

static void write_elements (FILE *fp, CGNS_ENUMT(ElementType_t) type,
    int nnodes, cgsize_t nelems)
{
    int ns, nsect, nn, ip, *elems;
    cgsize_t i, n, is, ie, ne;
    cgsize_t size, *conn;
    CGNS_ENUMT(ElementType_t) elemtype, et;
    char name[33];

    elems = (int *)malloc((size_t)nelems * (size_t)nnodes * sizeof(int));
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
        if (elemtype == type || elemtype == CGNS_ENUMV(MIXED)) {
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
                    if (et == type) {
                        if (type == CGNS_ENUMV(PYRA_5)) {
                            elems[nn++] = conn[i];
                            elems[nn++] = conn[i+3];
                            elems[nn++] = conn[i+4];
                            elems[nn++] = conn[i+1];
                            elems[nn++] = conn[i+2];
                            i += 5;
                        }
                        else {
                            for (ip = 0; ip < nnodes; ip++)
                                elems[nn++] = conn[i++];
                        }
                    }
                    else {
                        if (cg_npe(et, &ip) || ip <= 0)
                            err_exit("cg_npe", NULL);
                        i += ip;
                    }
                }
            }
            else if (type == CGNS_ENUMV(PYRA_5)) {
                for (i = 0, n = 0; n < ne; n++) {
                    elems[nn++] = conn[i];
                    elems[nn++] = conn[i+3];
                    elems[nn++] = conn[i+4];
                    elems[nn++] = conn[i+1];
                    elems[nn++] = conn[i+2];
                    i += 5;
                }
            }
            else {
                for (n = 0; n < ne * nnodes; n++)
                    elems[nn++] = conn[n];
            }
            free (conn);
        }
    }
    
    write_ints(fp, nn, elems);
    free(elems);
}

/*--------------------------------------------------------------------*/

static void write_boundary (FILE *fp)
{
    int *tags, nb = (int)(nTris + nQuads);

    if (nTris) write_elements (fp, CGNS_ENUMV(TRI_3), 3, nTris);
    if (nPyras) write_elements (fp, CGNS_ENUMV(QUAD_4), 4, nQuads);

    if (nb == 0) return;
    tags = (int *)calloc((size_t)nb, sizeof(int));
    write_ints(fp, nb, tags);
    free(tags);
}

/*--------------------------------------------------------------------*/

int main (int argc, char *argv[])
{
    char name[33];
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
            case 'B':
                cgBase = atoi(argarg);
                break;
            case 'Z':
                cgZone = atoi(argarg);
                break;
        }
    }

    if (argind > argc - 2)
        print_usage (usgmsg, "CGNSfile and/or AFLR3file not given");
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
    
    /* verify dimensions fit in an integer */

    if ((3 * nTris + 4 * nQuads) > CG_MAX_INT32 ||
        (4 * nTets) > CG_MAX_INT32 ||
        (5 * nPyras) > CG_MAX_INT32 ||
        (6 * nPrisms) > CG_MAX_INT32 ||
        (4 * nHexas) > CG_MAX_INT32)
        err_exit (NULL, "too many elements to write with integers");

    /* open output file */

    printf ("writing %s AFLR3 data to <%s>\n",
        is_ascii ? "ASCII" : "binary", argv[++argind]);
    if (NULL == (fp = fopen (argv[argind], is_ascii ? "w+" : "w+b")))
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
    write_boundary (fp);
    if (nTets) write_elements (fp, CGNS_ENUMV(TETRA_4), 4, nTets);
    if (nPyras) write_elements (fp, CGNS_ENUMV(PYRA_5), 5, nPyras);
    if (nPrisms) write_elements (fp, CGNS_ENUMV(PENTA_6), 6, nPrisms);
    if (nHexas) write_elements (fp, CGNS_ENUMV(HEXA_8), 8, nHexas);

    fclose (fp);
    cg_close (cgFile);
    return 0;
}
