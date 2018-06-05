/*
 * fast_to_cgns.c - convert FAST file to CGNS
 */

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

#include "binaryio.h"
#include "cgnslib.h"
#include "getargs.h"

int nCoords = 0;
void *xCoord, *yCoord, *zCoord;

typedef struct {
    int id, bctype;
    int start, end;
} FACESET;

typedef struct {
    int id;
    int nodes[3];
} FACE;

int nTris = 0;
FACE *Tris;
int nTriSets = 0;
FACESET *TriSets;

int nTets = 0;
cgsize_t *Tets;

static char options[] = "48fuslb";

static char *usgmsg[] = {
    "usage  : fast_to_cgns [options] FASTfile [CGNSfile]",
    "options:",
    "   -4       = coordinates are real*4 (float)",
    "   -8       = coordinates are real*8 (double - default)",
    "   -f       = formatted (ASCII) file format",
    "   -u       = Fortran unformatted file format",
    "   -s       = Fortran stream format (binary - default)",
    "   -l       = little-endian format",
    "   -b       = big-endian format (default)",
    "default is big-endian binary (Fortran stream) FAST file",
    NULL
};

int is_single = 0;

/*----------------------------------------------------------------------*/

static int sort_faces(const void *v1, const void *v2)
{
    const FACE *f1 = (const FACE *)v1;
    const FACE *f2 = (const FACE *)v2;
    return f1->id - f2->id;
}

/*----------------------------------------------------------------------*/

static void build_trisets()
{
    int n, id, ns, nf;

    for (n = 1; n < nTris; n++) {
        if (Tris[n].id < Tris[n-1].id) {
            qsort(Tris, nTris, sizeof(FACE), sort_faces);
            break;
        }
    }

    id = Tris[0].id;
    nTriSets = 1;
    for (n = 1; n < nTris; n++) {
        if (Tris[n].id != id) {
            id = Tris[n].id;
            nTriSets++;
        }
    }

    TriSets = (FACESET *)malloc(nTriSets * sizeof(FACESET));
    if (TriSets == NULL) {
        fprintf(stderr, "malloc failed for tri sets\n");
        exit(1);
    }

    id = Tris[0].id;
    ns = 0;
    nf = 0;
    for (n = 1; n < nTris; n++) {
        if (Tris[n].id != id) {
            TriSets[ns].id = id;
            TriSets[ns].bctype = -1;
            TriSets[ns].start = nf;
            TriSets[ns].end = n - 1;
            ns++;
            id = Tris[n].id;
            nf = n;
        }
    }
    TriSets[ns].id = id;
    TriSets[ns].bctype = -1;
    TriSets[ns].start = nf;
    TriSets[ns].end = nTris - 1;
}

/*----------------------------------------------------------------------*/

static void read_fgrid(char *filename, int flags)
{
    int i, n, nn, counts[3], nodes[4];
    BINARYIO *bf;

    /* open file */

    bf = bf_open(filename, flags);
    if (bf == NULL) {
        fprintf(stderr, "error opening <%s> for reading\n", filename);
        exit(1);
    }

    /* get counts */

    if (3 != bf_getints(bf, 3, counts)) {
        fprintf(stderr, "error reading counts\n");
        exit(1);
    }
    nCoords = counts[0];
    nTris   = counts[1];
    nTets   = counts[2];

    /* read coordinates */

    if (is_single) {
        float *x, *y, *z;
        x = (float *)malloc(nCoords * sizeof(float));
        y = (float *)malloc(nCoords * sizeof(float));
        z = (float *)malloc(nCoords * sizeof(float));
        if (x == NULL || y == NULL || z == NULL) {
            fprintf(stderr, "malloc failed for coordinates\n");
            exit(1);
        }
        if (nCoords != bf_getfloats(bf, nCoords, x) ||
            nCoords != bf_getfloats(bf, nCoords, y) ||
            nCoords != bf_getfloats(bf, nCoords, z)) {
            fprintf(stderr, "error reading coordinates\n");
            exit(1);
        }
        xCoord = (void *)x;
        yCoord = (void *)y;
        zCoord = (void *)z;
    }
    else {
        double *x, *y, *z;
        x = (double *)malloc(nCoords * sizeof(double));
        y = (double *)malloc(nCoords * sizeof(double));
        z = (double *)malloc(nCoords * sizeof(double));
        if (x == NULL || y == NULL || z == NULL) {
            fprintf(stderr, "malloc failed for coordinates\n");
            exit(1);
        }
        if (nCoords != bf_getdoubles(bf, nCoords, x) ||
            nCoords != bf_getdoubles(bf, nCoords, y) ||
            nCoords != bf_getdoubles(bf, nCoords, z)) {
            fprintf(stderr, "error reading coordinates\n");
            exit(1);
        }
        xCoord = (void *)x;
        yCoord = (void *)y;
        zCoord = (void *)z;
    }

    /* read boundary tris */

    if (nTris) {
        Tris = (FACE *)malloc(nTris * sizeof(FACE));
        if (Tris == NULL) {
            fprintf(stderr, "malloc failed for boundary tris\n");
            exit(1);
        }
        for (n = 0; n < nTris; n++) {
            Tris[n].id = 0;
            if (3 != bf_getints(bf, 3, Tris[n].nodes)) {
                fprintf(stderr, "error reading tris\n");
                exit(1);
            }
        }

        for (n = 0; n < nTris; n++) {
            if (1 != bf_getints(bf, 1, &Tris[n].id)) {
                fprintf(stderr, "error reading tri surface IDs\n");
                exit(1);
            }
        }
        build_trisets();
    }

    /* read tets */

    if (nTets) {
        Tets = (cgsize_t *)malloc(nTets * 4 * sizeof(cgsize_t));
        if (Tets == NULL) {
            fprintf(stderr, "malloc failed for tets\n");
            exit(1);
        }
        for (nn = 0, n = 0; n < nTets; n++) {
            if (4 != bf_getints(bf, 4, nodes)) {
                fprintf(stderr, "error reading tets\n");
                exit(1);
            }
            for (i = 0; i < 4; i++)
                Tets[nn++] = nodes[i];
        }
    }

    bf_close(bf);
}

/*----------------------------------------------------------------------*/

static void read_mapbc(char *filename)
{
    FILE *fp;
    int i, n, numBC, id, bc;
    char buf[256];

    if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "error opening <%s> for reading\n", filename);
        return;
    }
    if (NULL == fgets(buf, sizeof(buf), fp) ||
        1 != sscanf(buf, "%d", &numBC)) {
        fprintf(stderr, "error reading mapbc data\n");
        exit(1);
    }
    for (n = 0; n < numBC; n++) {
        if (NULL == fgets(buf, sizeof(buf), fp) ||
            2 != sscanf(buf, "%d %d", &id, &bc)) {
            fprintf(stderr, "error reading bc data\n");
            exit(1);
        }
        for (i = 0; i < nTriSets; i++) {
            if (TriSets[i].id == id)
                TriSets[i].bctype = bc;
        }
    }
    fclose(fp);
}

/*----------------------------------------------------------------------*/

static CGNS_ENUMV(BCType_t) get_bctype (int bctype)
{
    switch (bctype) {
        case 1:
        case 6000:
        case 6001:
        case 6002:
        case 6010:
        case 6011:
        case 6012:
        case 6021:
        case 6022:
        case 6023:
        case 6661:
        case 6662:
        case 6663:
            return CGNS_ENUMV(BCSymmetryPlane);
        case 2:
        case 5026:
            return CGNS_ENUMV(BCExtrapolate);
        case 3:
        case 5000:
        case 5025:
        case 5050:
            return CGNS_ENUMV(BCFarfield);
        case 4:
        case 4000:
            return CGNS_ENUMV(BCWallViscous);
        case 5:
        case 3000:
            return CGNS_ENUMV(BCWallInviscid);
        case 5051:
        case 5052:
            return CGNS_ENUMV(BCTunnelOutflow);
        case 7011:
            return CGNS_ENUMV(BCInflowSubsonic);
        case 7012:
            return CGNS_ENUMV(BCOutflowSubsonic);
        case 7031:
            return CGNS_ENUMV(BCOutflow);
        case 7036:
        case 7100:
            return CGNS_ENUMV(BCInflow);
    }
    return CGNS_ENUMV(BCTypeUserDefined);
}

/*----------------------------------------------------------------------*/

static void write_cgns(char *filename)
{
    char name[33];
    const char *bcname;
    int cgfile, cgbase, cgzone;
    int cgcoord, cgsect, cgbc;
    int n, i, ns, ne, nemax;
    cgsize_t sizes[3], *nodes, start, end, range[2];
    CGNS_ENUMT(DataType_t) dtype;
    CGNS_ENUMT(BCType_t) bctype;

    sizes[0] = nCoords;
    sizes[1] = nTets;
    if (sizes[1] == 0) sizes[1] = nTris;
    sizes[2] = 0;

    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", sizes,
            CGNS_ENUMV(Unstructured), &cgzone))
        cg_error_exit();

    dtype = is_single ? CGNS_ENUMV(RealSingle) : CGNS_ENUMV(RealDouble);
    if (cg_coord_write(cgfile, cgbase, cgzone, dtype,
            "CoordinateX", xCoord, &cgcoord) ||
        cg_coord_write(cgfile, cgbase, cgzone, dtype,
            "CoordinateY", yCoord, &cgcoord) ||
        cg_coord_write(cgfile, cgbase, cgzone, dtype,
            "CoordinateZ", zCoord, &cgcoord))
        cg_error_exit();
    free(xCoord);
    free(yCoord);
    free(zCoord);

    start = 1;
    if (nTets) {
        end = start + nTets - 1;
        if (cg_section_write(cgfile, cgbase, cgzone, "TetElements",
                CGNS_ENUMV(TETRA_4), start, end, 0, Tets, &cgsect))
            cg_error_exit();
        free(Tets);
        start = end + 1;
    }

    if (nTris == 0) {
        cg_close(cgfile);
        return;
    }

    nemax = 0;
    for (ns = 0; ns < nTriSets; ns++) {
        ne = 3 * (TriSets[ns].end - TriSets[ns].start + 1);
        if (nemax < ne) nemax = ne;
    }
    nodes = (cgsize_t *)malloc(nemax * sizeof(cgsize_t));
    if (nodes == NULL) {
        fprintf(stderr, "malloc failed for tri nodes array\n");
        exit(1);
    }

    /* write tris */

    for (ns = 0; ns < nTriSets; ns++) {
        n = 0;
        for (ne = TriSets[ns].start; ne <= TriSets[ns].end; ne++) {
            for (i = 0; i < 3; i++)
                nodes[n++] = Tris[ne].nodes[i];
        }
        end = start + TriSets[ns].end - TriSets[ns].start;
        sprintf(name, "TriElements %d", ns+1);
        if (cg_section_write(cgfile, cgbase, cgzone, name,
                CGNS_ENUMV(TRI_3), start, end, 0, nodes, &cgsect))
            cg_error_exit();
        TriSets[ns].start = (int)start;
        TriSets[ns].end = (int)end;
        start = end + 1;
    }

    free(nodes);

    /* write BCs */

    for (ns = 0; ns < nTriSets; ns++) {
        bctype = get_bctype(TriSets[ns].bctype);
        bcname = cg_BCTypeName(bctype);
        if (0 == strncmp(bcname, "BC", 2)) bcname += 2;
        sprintf(name, "%s %d", bcname, ns+1);
        range[0] = TriSets[ns].start;
        range[1] = TriSets[ns].end;
#if CGNS_VERSION < 3100
        if (cg_boco_write(cgfile, cgbase, cgzone, name,
                bctype, CGNS_ENUMV(ElementRange), 2, range, &cgbc))
#else
        if (cg_boco_write(cgfile, cgbase, cgzone, name,
                bctype, CGNS_ENUMV(PointRange), 2, range, &cgbc) ||
            cg_boco_gridlocation_write(cgfile, cgbase, cgzone,
                cgbc, CGNS_ENUMV(FaceCenter)))
#endif
            cg_error_exit();
    }

    cg_close(cgfile);
}

/*======================================================================*/

int main (int argc, char *argv[])
{
    int n, flags = MACH_IEEE;
    char *p, *tail, *basename;

    if (argc < 2)
        print_usage(usgmsg, NULL);

    /* get options */

    while ((n = getargs (argc, argv, options)) > 0) {
        switch (n) {
            case '4':
                is_single = 1;
                break;
            case '8':
                is_single = 0;
                break;
            case 'f':
                flags &= ~OPEN_FORTRAN;
                flags |= OPEN_ASCII;
                break;
            case 'u':
                flags &= ~OPEN_ASCII;
                flags |= OPEN_FORTRAN;
                break;
            case 's':
                flags &= ~(OPEN_ASCII | OPEN_FORTRAN);
                break;
            case 'l':
                flags &= ~MACH_UNKNOWN;
                flags |= MACH_BSIEEE;
                break;
            case 'b':
                flags &= ~MACH_UNKNOWN;
                flags |= MACH_IEEE;
                break;
        }
    }

    if (argind >= argc)
        print_usage(usgmsg, "FAST filename not specified\n");

    basename = (char *)malloc(strlen(argv[argind]) + 7);
    if (basename == NULL) {
        fprintf(stderr, "malloc failed for filename\n");
        return 1;
    }
    strcpy(basename, argv[argind++]);

    /* read mesh */

    printf("reading grid from \"%s\"\n", basename);
    fflush(stdout);
    read_fgrid(basename, flags);
    printf("number coordinates = %ld\n", (long)nCoords);
    printf("number triangles   = %ld\n", (long)nTris);
    printf("number tetrahedra  = %ld\n", (long)nTets);

    /* check for fastbc/mapbc file */

    tail = strrchr(basename, '/');
#ifdef _WIN32
    if (tail == NULL) tail = strrchr(basename, '\\');
#endif
    if (tail == NULL)
        tail = basename;
    else
        tail++;
    if ((p = strrchr(tail, '.')) == NULL)
        p = basename + strlen(basename);

    strcpy(p, ".fastbc");
    n = ACCESS(basename, 0);
    if (n) {
        strcpy(p, ".mapbc");
        n = ACCESS(basename, 0);
    }
    if (n) {
        *p = 0;
        if ((p = strrchr(tail, '.')) != NULL) {
            strcpy(p, ".fastbc");
            n = ACCESS(basename, 0);
            if (n) {
                strcpy(p, ".mapbc");
                n = ACCESS(basename, 0);
            }
        }
    }
    if (0 == n) {
        printf("reading boundary conditions from \"%s\"\n", basename);
        fflush(stdout);
        read_mapbc(basename);
    }

    /* open CGNS file */

    if (argind < argc) {
        p = argv[argind];
    }
    else {
        strcpy(p, "cgns");
        p = basename;
    }
    printf ("writing CGNS file to \"%s\"\n", p);
    write_cgns(p);

    return 0;
}
