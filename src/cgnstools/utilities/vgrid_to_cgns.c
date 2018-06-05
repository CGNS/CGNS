/*
 * vgrid_to_cgns.c - convert a VGRID file to CGNS
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

#ifndef CGNS_ENUMV
# define CGNS_ENUMV(V) V
# define CGNS_ENUMT(T) T
#endif

typedef struct {
    char name[33];
    int num, bc;
    int start, end;
} PATCH;

int nPatchs = 0;
PATCH *Patchs;

typedef struct {
    int patch;
    int tri[3];
} FACE;

int nFaces = 0;
FACE *Faces;

cgsize_t nTets, nCoords, nBoundary;
cgsize_t *Tets;
double *xCoord, *yCoord, *zCoord;

static char options[] = "c";

static char *usgmsg[] = {
    "usage  : vgrid_to_cgns [options] VGRIDbase [CGNSfile]",
    "options:",
    "   -c = combine patchs with the same name and BC",
    NULL
};

static int combinePatchs = 0;

/*----------------------------------------------------------------------*/

static void read_cogsg(char *filename)
{
    int counts[6], ne, np;
    int i, n, ntet, *nodes;
    double tm;
    BINARYIO *bf;

    /* open file as Fortran unformatted */

    bf = bf_open(filename, ARCH_IEEE | OPEN_READ | OPEN_FORTRAN);
    if (bf == NULL) {
        fprintf(stderr, "error opening <%s> for reading\n", filename);
        exit(1);
    }

    /* get counts */

    if (6 != bf_getints(bf, 6, counts) ||
        1 != bf_getdoubles(bf, 1, &tm)) {
        fprintf(stderr, "error reading counts\n");
        exit(1);
    }

    nTets = counts[1];
    nCoords = counts[2];
    nBoundary = counts[3];
    if (nTets < 1 || nCoords < 4) {
        fprintf(stderr, "invalid number of tets or coordinates\n");
        exit(1);
    }

    /* read tets */

    Tets = (cgsize_t *)malloc(nTets * 4 * sizeof(cgsize_t));
    nodes = (int *)malloc(nTets * sizeof(int));
    if (Tets == NULL || nodes == NULL) {
        fprintf(stderr, "malloc failed for tets\n");
        exit(1);
    }
    for (n = 0; n < 4; n++) {
        if (nTets != bf_getints(bf, nTets, nodes)) {
            fprintf(stderr, "error reading tet nodes\n");
            exit(1);
        }
        for (ne = n, i = 0; i < nTets; i++) {
            Tets[ne] = nodes[i];
            ne += 4;
        }
    }
    free(nodes);

    /* read coordinates */

    xCoord = (double *)malloc(nCoords * sizeof(double));
    yCoord = (double *)malloc(nCoords * sizeof(double));
    zCoord = (double *)malloc(nCoords * sizeof(double));
    if (xCoord == NULL || yCoord == NULL || zCoord == NULL) {
        fprintf(stderr, "malloc failed for coordinates\n");
        exit(1);
    }
    if (nCoords != bf_getdoubles(bf, nCoords, xCoord) ||
        nCoords != bf_getdoubles(bf, nCoords, yCoord) ||
        nCoords != bf_getdoubles(bf, nCoords, zCoord)) {
        fprintf(stderr, "error reading coordinates\n");
        exit(1);
    }

    /* read additional tets and coordinates, if any */

    while (1 == bf_getints(bf, 1, &ntet) && ntet > 0) {
        Tets = (cgsize_t *)realloc(Tets, (nTets + ntet) * 4 * sizeof(cgsize_t));
        nodes = (int *)malloc(ntet * sizeof(int));
        if (Tets == NULL || nodes == NULL) {
            fprintf(stderr, "realloc failed for tets\n");
            exit(1);
        }
        for (n = 0; n < 4; n++) {
            if (ntet != bf_getints(bf, ntet, nodes)) {
                fprintf(stderr, "error reading tet nodes\n");
                exit(1);
            }
            for (ne = 4 * nTets + n, i = 0; i < ntet; i++) {
                Tets[ne] = nodes[i];
                ne += 4;
            }
        }
        nTets += ntet;
        free(nodes);

        bf_getints(bf, 1, &np);
        xCoord = (double *)realloc(xCoord, (nCoords + np) * sizeof(double));
        yCoord = (double *)realloc(yCoord, (nCoords + np) * sizeof(double));
        zCoord = (double *)realloc(zCoord, (nCoords + np) * sizeof(double));
        if (xCoord == NULL || yCoord == NULL || zCoord == NULL) {
            fprintf(stderr, "realloc failed for coordinates\n");
            exit(1);
        }

        if (np != bf_getdoubles(bf, np, &xCoord[nCoords]) ||
            np != bf_getdoubles(bf, np, &yCoord[nCoords]) ||
            np != bf_getdoubles(bf, np, &zCoord[nCoords])) {
            fprintf(stderr, "error reading coordinates\n");
            exit(1);
        }
        nCoords += np;
    }

    bf_close(bf);
}

/*----------------------------------------------------------------------*/

static int sort_faces(const void *v1, const void *v2)
{
    const FACE *f1 = (const FACE *)v1;
    const FACE *f2 = (const FACE *)v2;
    return f1->patch - f2->patch;
}

/*----------------------------------------------------------------------*/

static void read_bc(char *filename)
{
    FILE *fp;
    int n, i, nbc, igrid, nf, np, tri[3];
    char buf[256];

    if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "error opening <%s> for reading\n", filename);
        return;
    }
    if (NULL == fgets(buf, sizeof(buf), fp) ||
        4 != sscanf(buf, "%d %d %d %d", &nFaces, &nbc, &nPatchs, &igrid)) {
        fprintf(stderr, "error reading face and patch counts\n");
        exit(1);
    }

    Faces = (FACE *)malloc(nFaces * sizeof(FACE));
    if (Faces == NULL) {
        fprintf(stderr, "malloc failed for faces\n");
        exit(1);
    }

    /* read faces */

    fgets(buf, sizeof(buf), fp);
    for (n = 0; n < nFaces; n++) {
        if (NULL == fgets(buf, sizeof(buf), fp) ||
            5 != sscanf(buf, "%d %d %d %d %d", &nf, &np,
                        &tri[0], &tri[1], &tri[2])) {
            fprintf(stderr, "error reading face %d\n", n+1);
            exit(1);
        }
        if (np < 1 || np > nPatchs) {
            fprintf(stderr, "invalid patch number %d\n", np);
            exit(1);
        }
        Faces[n].patch = np;
        for (i = 0; i < 3; i++)
            Faces[n].tri[i] = tri[i];
    }

    fclose(fp);

    /* sort faces if needed */

    for (n = 1; n < nFaces; n++) {
        if (Faces[n].patch < Faces[n-1].patch) {
            qsort(Faces, nFaces, sizeof(FACE), sort_faces);
            break;
        }
    }

    /* create patches */

    Patchs = (PATCH *)malloc(nPatchs * sizeof(PATCH));
    if (Patchs == NULL) {
        fprintf(stderr, "malloc failed for patches\n");
        exit(1);
    }
    i = (int)log10((double)nPatchs) + 1;
    for (n = 0; n < nPatchs; n++) {
        sprintf(Patchs[n].name, "Patch%0*d", i, n + 1);
        Patchs[n].num = 0;
        Patchs[n].bc = 0;
        Patchs[n].start = Patchs[n].end = 0;
    }

    nf = 0;
    np = Faces[0].patch;
    for (n = 1; n < nFaces; n++) {
        if (Faces[n].patch != np) {
            Patchs[np-1].num = np;
            Patchs[np-1].start = nf;
            Patchs[np-1].end = n - 1;
            np = Faces[n].patch;
            nf = n;
        }
    }
    Patchs[np-1].num = np;
    Patchs[np-1].start = nf;
    Patchs[np-1].end = nFaces - 1;
}

/*----------------------------------------------------------------------*/

static int sort_patchs(const void *v1, const void *v2)
{
    const PATCH *p1 = (const PATCH *)v1;
    const PATCH *p2 = (const PATCH *)v2;
    int n = strcmp(p1->name, p2->name);
    if (n) return n;
    return p1->bc - p2->bc;
}

/*----------------------------------------------------------------------*/

static void read_mapbc(const char *filename)
{
    FILE *fp;
    int n, np, bc, fam, ns, nsid, nn;
    int len = 30, fmt = 0;
    char buf[256], *p;

    if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "error opening <%s> for reading\n", filename);
        return;
    }
    for (n = 0; n < 4; n++)
        fgets(buf, sizeof(buf), fp);
    if (!combinePatchs) {
        fmt = (int)log10((double)nPatchs) + 1;
        len -= fmt;
    }
    for (n = 0; n < nPatchs; n++) {
        if (NULL == fgets(buf, sizeof(buf), fp) ||
            5 != sscanf(buf, "%d %d %d %d %d%n",
                        &np, &bc, &fam, &ns, &nsid, &nn)) {
            fprintf(stderr, "error reading patch data\n");
            exit(1);
        }
        if (np < 1 || np > nPatchs) {
            fprintf(stderr, "invalid patch number\n");
            exit(1);
        }
        np--;
        Patchs[np].bc = bc;

        p = buf + nn;
        while (*p && isspace(*p)) p++;
        if (*p) {
            nn = 0;
            while (*p && !isspace(*p) && nn < len) {
                Patchs[np].name[nn++] = *p++;
            }
            if (combinePatchs)
                Patchs[np].name[nn] = 0;
            else
                sprintf(&Patchs[np].name[nn], "%0*d", fmt, n+1);
        }
    }
    fclose(fp);
    if (!combinePatchs) return;

    qsort(Patchs, nPatchs, sizeof(PATCH), sort_patchs);

    np = 0;
    while (np < nPatchs) {
        p = Patchs[np].name;
        bc = Patchs[np].bc;
        ns = 0;
        for (nn = np + 1; nn < nPatchs; nn++) {
            if (strcmp(Patchs[nn].name, p)) break;
            if (Patchs[nn].bc != bc) ns++;
        }
        if (ns) {
            ns = 1;
            for (n = np; n < nn; n++) {
                if (Patchs[n].bc != bc) {
                    bc = Patchs[n].bc;
                    ns++;
                }
                len = strlen(Patchs[n].name);
                if (len > 30) len = 30;
                sprintf(&Patchs[n].name[len], "%02d", ns);
            }
        }
        np = nn;
    }
}

/*----------------------------------------------------------------------*/

static void write_cgns(char *filename)
{
    int i, np, cgfile, cgbase, cgzone;
    int cgcoord, cgsect, cgbc;
    int n, ne, nmax, nt, nn;
    cgsize_t sizes[3], *tris, start, end;
    char *name;
    CGNS_ENUMT(BCType_t) bctype;

    sizes[0] = nCoords;
    sizes[1] = nTets;
    sizes[2] = nBoundary;

    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", sizes,
            CGNS_ENUMV(Unstructured), &cgzone))
        cg_error_exit();

    if (cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealDouble),
            "CoordinateX", xCoord, &cgcoord) ||
        cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealDouble),
            "CoordinateY", yCoord, &cgcoord) ||
        cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealDouble),
            "CoordinateZ", zCoord, &cgcoord))
        cg_error_exit();
    free(xCoord);
    free(yCoord);
    free(zCoord);

    if (cg_section_write(cgfile, cgbase, cgzone, "Tetrahedra",
            CGNS_ENUMV(TETRA_4), 1, nTets, 0, Tets, &cgsect))
        cg_error_exit();
    free(Tets);

    if (nFaces == 0) {
        cg_close(cgfile);
        return;
    }

    if (nPatchs == 0) {
        tris = (cgsize_t *)malloc(nFaces * 3 * sizeof(cgsize_t));
        for (n = 0, ne = 0; ne < nFaces; ne++) {
            for (i = 0; i < 3; i++)
                tris[n++] = Faces[ne].tri[i];
        }
        start = nTets + 1;
        end = start + nFaces;
        if (cg_section_write(cgfile, cgbase, cgzone, "Triangles",
                CGNS_ENUMV(TRI_3), start, end, 0, tris, &cgsect))
            cg_error_exit();
        free(tris);
        free(Faces);
        cg_close(cgfile);
        return;
    }

    /* get max section size */

    nmax = 0;
    np = 0;
    while (np < nPatchs) {
        name = Patchs[np].name;
        nt = Patchs[np].end - Patchs[np].start + 1;
        for (nn = np + 1; nn < nPatchs; nn++) {
            if (strcmp(Patchs[nn].name, name)) break;
            nt += (Patchs[nn].end - Patchs[nn].start + 1);
        }
        if (nmax < nt) nmax = nt;
        np = nn;
    }
    tris = (cgsize_t *)malloc(nmax * 3 * sizeof(cgsize_t));
    if (tris == NULL) {
        fprintf(stderr, "malloc failed for tri data\n");
        exit(1);
    }

    /* write the boundary grid sections and BCs */

    start = nTets + 1;
    np = 0;
    while (np < nPatchs) {
        name = Patchs[np].name;
        switch (Patchs[np].bc) {
            case 0:
                bctype = CGNS_ENUMV(BCInflowSupersonic);
                break;
            case 1:
                bctype = CGNS_ENUMV(BCSymmetryPlane);
                break;
            case 2:
                bctype = CGNS_ENUMV(BCExtrapolate);
                break;
            case 3:
                bctype = CGNS_ENUMV(BCFarfield);
                break;
            case 4:
                bctype = CGNS_ENUMV(BCWallViscous);
                break;
            case 5:
                bctype = CGNS_ENUMV(BCWallInviscid);
                break;
            case 1001:
                bctype = CGNS_ENUMV(BCTunnelInflow);
                break;
            case 1002:
                bctype = CGNS_ENUMV(BCTunnelOutflow);
                break;
            default:
                bctype = CGNS_ENUMV(BCTypeUserDefined);
                break;
        }
        n = nt = 0;
        for (nn = np; nn < nPatchs; nn++) {
            if (strcmp(Patchs[nn].name, name)) break;
            for (ne = Patchs[nn].start; ne <= Patchs[nn].end; ne++) {
                nt++;
                for (i = 0; i < 3; i++)
                    tris[n++] = Faces[ne].tri[i];
            }
        }
        np = nn;
        if (nt) {
            end = start + nt - 1;
            sizes[0] = start;
            sizes[1] = end;
            if (cg_section_write(cgfile, cgbase, cgzone, name,
                    CGNS_ENUMV(TRI_3), start, end, 0, tris, &cgsect) ||
#if CGNS_VERSION < 3100
                cg_boco_write(cgfile, cgbase, cgzone, name,
                    bctype, CGNS_ENUMV(ElementRange), 2, sizes, &cgbc))
#else
                cg_boco_write(cgfile, cgbase, cgzone, name,
                    bctype, CGNS_ENUMV(PointRange), 2, sizes, &cgbc) ||
                cg_boco_gridlocation_write(cgfile, cgbase, cgzone,
                    cgbc, CGNS_ENUMV(FaceCenter)))
#endif
                cg_error_exit();
            start = end + 1;
        }
    }
    free(tris);

    cg_close(cgfile);
}

/*======================================================================*/

int main (int argc, char *argv[])
{
    int n;
    char *p, *basename;

    if (argc < 2)
        print_usage(usgmsg, NULL);

    while ((n = getargs(argc, argv, options)) > 0) {
        switch (n) {
            case 'c':
                combinePatchs = 1;
                break;
        }
    }

    if (argind >= argc)
        print_usage(usgmsg, "VGRID filename not specified\n");

    basename = (char *)malloc(strlen(argv[argind]) + 7);
    if (basename == NULL) {
        fprintf(stderr, "malloc failed for filename\n");
        return 1;
    }
    strcpy(basename, argv[argind++]);
    p = strrchr(basename, '/');
#ifdef _WIN32
    if (p == NULL) p = strrchr(basename, '\\');
#endif
    if (p == NULL)
        p = basename;
    else
        p++;
    if ((p = strrchr(p, '.')) == NULL ||
       (strcmp(p, ".cogsg") &&
        strcmp(p, ".bc") &&
        strcmp(p, ".mapbc"))) p = basename + strlen(basename);
    *p++ = '.';

    /* read tet grid */

    strcpy(p, "cogsg");
    printf("reading volume grid from \"%s\"\n", basename);
    fflush(stdout);
    read_cogsg(basename);
    printf("read %ld coordinates and %ld tetrahedra\n",
        (long)nCoords, (long)nTets);

    /* read surface grid */

    strcpy(p, "bc");
    if (0 == ACCESS(basename, 0)) {
        printf("reading surface grid from \"%s\"\n", basename);
        fflush(stdout);
        read_bc(basename);
        printf("read %d boundary faces and %d patches\n", nFaces, nPatchs);
    }

    /* read boundray condition file */

    strcpy(p, "mapbc");
    if (nPatchs && 0 == ACCESS(basename, 0)) {
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
