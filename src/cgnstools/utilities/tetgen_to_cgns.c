#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef _WIN32
# include <io.h>
# define strcasecmp _stricmp
# define access _access
#else
# include <unistd.h>
#endif
#include "cgnslib.h"

#define COORD_TYPE CGNS_ENUMV(RealSingle)
typedef float COORD;

int totCoords;
int nCoords = 0;
COORD *xC, *yC, *zC;

int nEdges = 0;
cgsize_t *Edges;

typedef struct {
    int mark;
    int nnodes;
    int *nodes;
} ELEMENT;

int nTris = 0;
ELEMENT *Tris;

int nTets = 0;
ELEMENT *Tets;

int nFaces = 0;
ELEMENT *Faces;

int offset = 0, lineno = 0;

/*-------------------------------------------------------------------*/

int read_error(char *msg)
{
    fflush(stdout);
    fprintf(stderr, "error on line %d: %s\n", lineno, msg);
    return 0;
}

/*-------------------------------------------------------------------*/

char *next_line (FILE *fp)
{
    char *p;
    static char buff[256];

    while (fgets(buff, sizeof(buff), fp) != NULL) {
        lineno++;
        buff[sizeof(buff)-1] = 0;
        if ((p = strchr(buff, '#')) != NULL)
            *p = 0;
        for (p = buff+strlen(buff)-1; p >= buff && isspace(*p); p--)
            ;
        *++p = 0;
        for (p = buff; *p && isspace(*p); p++)
            ;
        if (*p) return p;
    }
    fprintf(stderr, "premature EOF\n");
    return NULL;
}

/*===================================================================*/

int read_nodes (FILE *fp)
{
    int n, nn, dim;
    double x, y, z;
    char *line;

    if ((line = next_line(fp)) == NULL) return 0;
    n = sscanf (line, "%d %d", &nCoords, &dim);
    if (n != 2 || nCoords < 2 || dim < 2 || dim > 3)
        return read_error("invalid node file");

    xC = (COORD *)malloc(nCoords * sizeof(COORD));
    yC = (COORD *)malloc(nCoords * sizeof(COORD));
    if (xC == NULL || yC == NULL)
        return read_error("malloc failed for coordinates");
    if (dim == 3) {
        zC = (COORD *)malloc(nCoords * sizeof(COORD));
        if (zC == NULL)
            return read_error("malloc failed for coordinates");
    }
    else {
        zC = NULL;
    }

    if ((line = next_line(fp)) == NULL) return 0;
    nn = sscanf(line, "%d %lf %lf %lf", &n, &x, &y, &z);
    if ((dim == 2 && nn < 3) || (dim == 3 && nn != 4))
        return read_error("error reading coordinates");
    xC[0] = (COORD)x;
    yC[0] = (COORD)y;
    if (dim == 3) zC[0] = (COORD)z;
    offset = n;

    if (dim == 2) {
        for (nn = 1; nn < nCoords; nn++) {
            if ((line = next_line(fp)) == NULL) return 0;
            if (3 != sscanf(line, "%d %lf %lf", &n, &x, &y))
                return read_error("error reading coordinates");
            if (n - offset != nn)
                return read_error("coordinates not ordered");
            xC[nn] = (COORD)x;
            yC[nn] = (COORD)y;
        }
    }
    else {
        for (nn = 1; nn < nCoords; nn++) {
            if ((line = next_line(fp)) == NULL) return 0;
            if (4 != sscanf(line, "%d %lf %lf %lf", &n, &x, &y, &z))
                return read_error("error reading coordinates");
            if (n - offset != nn)
                return read_error("coordinates not ordered");
            xC[nn] = (COORD)x;
            yC[nn] = (COORD)y;
            zC[nn] = (COORD)z;
        }
    }

    return dim;
}

/*===================================================================*/

int read_edges (FILE *fp)
{
    int ne, nn, n, id, edge[2];
    char *line;

    if ((line = next_line(fp)) == NULL) return 0;
    nEdges = atoi(line);
    if (nEdges < 1)
        return read_error("invalid edge file");

    Edges = (cgsize_t *)malloc(2 * nEdges * sizeof(cgsize_t));
    if (Edges == NULL)
        return read_error("malloc failed for edges");

    for (nn = 0, ne = 0; ne < nEdges; ne++) {
        if ((line = next_line(fp)) == NULL) return 0;
        if (3 != sscanf(line, "%d %d %d", &n, &edge[0], &edge[1]))
            return read_error("error reading edges");
        for (n = 0; n < 2; n++) {
            id = edge[n] - offset;
            if (id < 0 || id >= nCoords)
                return read_error("edge index is invalid");
            Edges[nn++] = id + 1;
        }
    }

    return nEdges;
}

/*-------------------------------------------------------------------*/

int read_tris (FILE *fp)
{
    int i, nt, n, id, tri[3];
    int hasmark, mark;
    char *line;

    if ((line = next_line(fp)) == NULL) return 0;
    if (2 != sscanf(line, "%d %d", &nTris, &hasmark) || nTris < 1)
        return read_error("invalid face file");

    Tris = (ELEMENT *)malloc(nTris * sizeof(ELEMENT));
    if (Tris == NULL)
        return read_error("malloc failed for tri elements");

    mark = 0;
    for (nt = 0; nt < nTris; nt++) {
        if ((line = next_line(fp)) == NULL) return 0;
        Tris[nt].nnodes = 3;
        Tris[nt].nodes = (int *)malloc(3 * sizeof(int));
        if (Tris[nt].nodes == NULL)
            return read_error("malloc failed for tri nodes");
        if (4 != sscanf(line, "%d %d %d %d%n", &n,
                &tri[0], &tri[1], &tri[2], &i))
            return read_error("error reading tris");
        if (hasmark) {
            if (1 != sscanf(&line[i], "%d", &mark))
                return read_error("error reading boundary marker");
        }
        Tris[nt].mark = mark;
        for (n = 0; n < 3; n++) {
            id = tri[n] - offset;
            if (id < 0 || id >= nCoords)
                return read_error("triangle index is invalid");
            Tris[nt].nodes[n] = id + 1;
        }
    }
    return nTris;
}

/*-------------------------------------------------------------------*/

int read_tets (FILE *fp)
{
    int nt, n, id, np, i, tet[6];
    int hasmark, mark;
    char *line;

    if ((line = next_line(fp)) == NULL) return 0;
    if (3 != sscanf(line, "%d %d %d", &nTets, &np, &hasmark) ||
        nTets < 1 || (np != 4 && np != 10))
        return read_error("invalid tet file");

    Tets = (ELEMENT *)malloc(nTets * sizeof(ELEMENT));
    if (Tets == NULL)
        return read_error("malloc failed for tet elements");

    mark = 0;
    for (nt = 0; nt < nTets; nt++) {
        if ((line = next_line(fp)) == NULL) return 0;
        Tets[nt].nnodes = np;
        Tets[nt].nodes = (int *)malloc(np * sizeof(int));
        if (Tets[nt].nodes == NULL)
            return read_error("malloc failed for tet nodes");
        if (5 != sscanf(line, "%d %d %d %d %d%n", &n, &tet[0],
                &tet[1], &tet[2], &tet[3], &i))
            return read_error("error reading tets");
        for (n = 0; n < 4; n++) {
            id = tet[n] - offset;
            if (id < 0 || id >= nCoords)
                return read_error("tet index is invalid");
            Tets[nt].nodes[n] = id + 1;
        }
        if (np == 10) {
            if (6 != sscanf(&line[i], "%d %d %d %d %d %d%n", &tet[0],
                &tet[1], &tet[2], &tet[3], &tet[4], &tet[5], &i))
                return read_error("error reading tets");
            for (n = 0; n < 6; n++) {
                id = tet[n] - offset;
                if (id < 0 || id >= nCoords)
                    return read_error("tet index is invalid");
                Tets[nt].nodes[n+4] = id + 1;
            }
        }
        if (hasmark) {
            if (1 != sscanf(&line[i], "%d", &mark))
                return read_error("error reading boundary marker");
        }
        Tets[nt].mark = mark;
    }
    return nTets;
}

/*-------------------------------------------------------------------*/

int read_mesh (char *fname)
{
    int dim, ok;
    FILE *fp;
    char *ext = fname + strlen(fname);

    strcpy(ext, ".node");
    printf("reading node file %s\n", fname);
    if ((fp = fopen (fname, "r")) == NULL) {
        fprintf(stderr, "couldn't open <%s>\n", fname);
        return 0;
    }
    lineno = 0;
    dim = read_nodes(fp);
    fclose(fp);
    if (dim == 0) return 0;

    strcpy(ext, ".edge");
    if (0 == access(fname, 0)) {
        printf("reading edge file %s\n", fname);
        if ((fp = fopen (fname, "r")) == NULL) {
            fprintf(stderr, "couldn't open <%s>\n", fname);
            return 0;
        }
        lineno = 0;
        ok = read_edges(fp);
        fclose(fp);
        if (!ok) return 0;
    }

    strcpy(ext, dim == 2 ? ".ele" : ".face");
    printf("reading tri file %s\n", fname);
    if ((fp = fopen (fname, "r")) == NULL) {
        fprintf(stderr, "couldn't open <%s>\n", fname);
        return 0;
    }
    lineno = 0;
    ok = read_tris(fp);
    fclose(fp);
    if (!ok) return 0;

    if (dim == 3) {
        strcpy(ext, ".ele");
        printf("reading tet file %s\n", fname);
        if ((fp = fopen (fname, "r")) == NULL) {
            fprintf(stderr, "couldn't open <%s>\n", fname);
            return 0;
        }
        lineno = 0;
        ok = read_tets(fp);
        fclose(fp);
    }

    if (ok) {
        printf("%d nodes, ", nCoords);
        if (nEdges)
            printf("%d edges, ", nEdges);
        printf("%d triangles and %d tetrahedra\n", nTris, nTets);
    }
    return ok;
}

/*===================================================================*/

int read_faces (FILE *fp, int plsg)
{
    int nf, n, id, np;
    int nfacets, nfmax;
    int i, j, npoly, nhole;
    int hasmark, mark;
    char *line;

    if ((line = next_line(fp)) == NULL) return 0;
    if (2 != sscanf(line, "%d %d", &nfacets, &hasmark) || nfacets < 1)
        return read_error("invalid face file");
    nfmax = nfacets;

    Faces = (ELEMENT *)malloc(nfmax * sizeof(ELEMENT));
    if (Faces == NULL)
        return read_error("malloc failed for face elements");

    nFaces = 0;
    mark = 0;
    for (nf = 0; nf < nfacets; nf++) {
        if ((line = next_line(fp)) == NULL) return 0;
        if (plsg) {
            if (2 != sscanf(line, "%d %d%n", &npoly, &nhole, &i))
                return read_error("error reading num polys/holes");
            if (hasmark) {
                if (1 != sscanf(&line[i], "%d", &mark))
                    return read_error("error reading boundary marker");
            }
            if ((line = next_line(fp)) == NULL) return 0;
        }
        else {
            npoly = 1;
            nhole = 0;
        }
        while (npoly-- > 0) {
            if (1 != sscanf(line, "%d%n", &np, &i))
                return read_error("error reading faces");
            if (np < 1)
                return read_error("invalid number of nodes for face");
            if (nFaces == nfmax) {
                nfmax += 10;
                Faces = (ELEMENT *)realloc(Faces, nfmax * sizeof(ELEMENT));
                if (Faces == NULL)
                    return read_error("realloc failed for face elements");
            }
            Faces[nFaces].nnodes = np;
            Faces[nFaces].nodes = (int *)malloc(np * sizeof(int));
            if (Faces[nFaces].nodes == NULL)
                return read_error("malloc failed for face nodes");
            for (n = 0; n < np; n++) {
                if (1 != sscanf(&line[i], "%d%n", &id, &j))
                    return read_error("error reading faces");
                i += j;
                id -= offset;
                if (id < 0 || id >= nCoords)
                    return read_error("face index is invalid");
                Faces[nFaces].nodes[n] = id + 1;
            }
            if (!plsg && hasmark) {
                if (1 != sscanf(&line[i], "%d", &mark))
                    return read_error("error reading boundary marker");
            }
            Faces[nFaces++].mark = mark;
        }
        while (nhole-- > 0) {
            if (next_line(fp) == NULL) return 0;
        }
    }
    return nFaces;
}

/*-------------------------------------------------------------------*/

int read_poly (char *fname, int plsg)
{
    int ok = 1;
    FILE *fp;

    printf ("reading poly file %s\n", fname);
    if ((fp = fopen (fname, "r")) == NULL) {
        fprintf(stderr, "couldn't open <%s>\n", fname);
        return 0;
    }
    lineno = 0;
    if (!read_nodes(fp) || !read_faces(fp, plsg)) ok = 0;
    fclose(fp);

    if (ok)
        printf("%d nodes and %d polygons\n", nCoords, nFaces);
    return ok;
}

/*===================================================================*/

int compare_marker(const void *v1, const void *v2)
{
    return (((ELEMENT *)v1)->mark - ((ELEMENT *)v2)->mark);
}

/*-------------------------------------------------------------------*/

void write_edges (int cgfile, int cgbase, int cgzone, cgsize_t *start)
{
    int cgsect;
    cgsize_t end = *start + nEdges;

    if (cg_section_write (cgfile, cgbase, cgzone, "Edges",
            CGNS_ENUMV(BAR_2), *start, end-1, 0, Edges, &cgsect))
        cg_error_exit();
    *start = end;
    free (Edges);
}

/*-------------------------------------------------------------------*/

void write_tris (int cgfile, int cgbase, int cgzone, cgsize_t *start)
{
    int n, nn, nt;
    int cgsect, hasmarks = 0;
    cgsize_t end, *tris;

    for (nt = 1; nt < nTris; nt++) {
        if (Tris[nt].mark != Tris[nt-1].mark) hasmarks++;
        if (Tris[nt].mark < Tris[nt-1].mark) {
            qsort(Tris, nTris, sizeof(ELEMENT), compare_marker);
            break;
        }
    }

    tris = (cgsize_t *)malloc(3 * nTris * sizeof(cgsize_t));
    if (tris == NULL) {
        fprintf(stderr, "malloc failed for tri data\n");
        exit(1);
    }

    if (hasmarks) {
        int is, ie, mark;
        char name[33];

        is = 0;
        while (is < nTris) {
            mark = Tris[is].mark;
            for (nn = 0; nn < 3; nn++)
                tris[nn] = Tris[is].nodes[nn];
            for (ie = is + 1; ie < nTris; ie++) {
                if (Tris[ie].mark != mark) break;
                for (n = 0; n < 3; n++)
                    tris[nn++] = Tris[ie].nodes[n];
            }
            sprintf(name, "Tri Group %d", mark);
            end = *start + ie - is;
            if (cg_section_write (cgfile, cgbase, cgzone, name,
                    CGNS_ENUMV(TRI_3), *start, end-1,
                    0, tris, &cgsect))
                cg_error_exit();
            *start = end;
            is = ie;
        }
    }
    else {
        for (nn = 0, nt = 0; nt < nTris; nt++) {
            for (n = 0; n < 3; n++)
                tris[nn++] = Tris[nt].nodes[n];
        }
        end = *start + nTris;
        if (cg_section_write (cgfile, cgbase, cgzone, "Triangles",
                CGNS_ENUMV(TRI_3), *start, end-1, 0, tris, &cgsect))
            cg_error_exit();
        *start = end;
    }

    free(tris);
    for (nt = 0; nt < nTris; nt++)
        free(Tris[nt].nodes);
    free(Tris);
}

/*-------------------------------------------------------------------*/

void write_faces (int cgfile, int cgbase, int cgzone, cgsize_t *start)
{
    int n, nn, nf, np;
    int cgsect, hasmarks = 0;
    cgsize_t end, *faces, *faces_offset;

    np = Faces[0].nnodes;
    for (nn = 0, nf = 1; nf < nFaces; nf++) {
        if (Faces[nf].mark != Faces[nf-1].mark) hasmarks++;
        if (Faces[nf].mark < Faces[nf-1].mark) nn++;
        np += Faces[nf].nnodes;
    }
    if (nn) qsort(Faces, nFaces, sizeof(ELEMENT), compare_marker);

    faces = (cgsize_t *)malloc((np + nFaces) * sizeof(cgsize_t));
    if (faces == NULL) {
        fprintf(stderr, "malloc failed for face data\n");
        exit(1);
    }

    faces_offset = (cgsize_t *)malloc((nFaces+1) * sizeof(cgsize_t));
    if (faces_offset == NULL) {
        fprintf(stderr, "malloc failed for face offset data\n");
        exit(1);
    }
    faces_offset[0] = 0;

    if (hasmarks) {
        int is, ie, mark;
        char name[33];

        is = 0;
        while (is < nFaces) {
            mark = Faces[is].mark;
            nn = 0;
            np = Faces[is].nnodes;
            faces[nn++] = np;
            faces_offset[0] = 0;
            for (n = 0; n < np; n++)
                faces[nn++] = Faces[is].nodes[n];
            faces_offset[1] = faces_offset[0] + np;
            for (ie = is + 1; ie < nFaces; ie++) {
                if (Faces[ie].mark != mark) break;
                np = Faces[ie].nnodes;
                faces[nn++] = np;
                faces_offset[ie-is+1] = faces_offset[ie-is] + np;
                for (n = 0; n < np; n++)
                    faces[nn++] = Faces[ie].nodes[n];
            }
            sprintf(name, "Face Group %d", mark);
            end = *start + ie - is;
            if (cg_poly_section_write (cgfile, cgbase, cgzone, name,
                    CGNS_ENUMV(NGON_n), *start, end-1,
                    0, faces, faces_offset, &cgsect))
                cg_error_exit();
            *start = end;
            is = ie;
        }
    }
    else {
        for (nn = 0, nf = 0; nf < nFaces; nf++) {
            np = Faces[nf].nnodes;
            faces[nn++] = np;
            faces_offset[nf+1] = faces_offset[nf] + np;
            for (n = 0; n < np; n++)
                faces[nn++] = Faces[nf].nodes[n];
        }
        end = *start + nFaces;
        if (cg_poly_section_write (cgfile, cgbase, cgzone, "Polygons",
                CGNS_ENUMV(NGON_n), *start, end-1, 0, faces, NULL, &cgsect))
            cg_error_exit();
        *start = end;
    }

    free(faces);
    free(faces_offset);
    for (nf = 0; nf < nFaces; nf++)
        free(Faces[nf].nodes);
    free(Faces);
}

/*-------------------------------------------------------------------*/

void write_tets (int cgfile, int cgbase, int cgzone, cgsize_t *start)
{
    int nptet, n, nn, nt;
    int cgsect, hasmarks = 0;
    cgsize_t end, *tets;

    for (nt = 1; nt < nTets; nt++) {
        if (Tets[nt].mark != Tets[nt-1].mark) hasmarks++;
        if (Tets[nt].mark < Tets[nt-1].mark) {
            qsort(Tets, nTets, sizeof(ELEMENT), compare_marker);
            break;
        }
    }

    nptet = Tets[0].nnodes;
    tets = (cgsize_t *)malloc(nptet * nTets * sizeof(cgsize_t));
    if (tets == NULL) {
        fprintf(stderr, "malloc failed for tet data\n");
        exit(1);
    }

    if (hasmarks) {
        int is, ie, mark;
        char name[33];

        is = 0;
        while (is < nTets) {
            mark = Tets[is].mark;
            for (nn = 0; nn < nptet; nn++)
                tets[nn] = Tets[is].nodes[nn];
            for (ie = is + 1; ie < nTets; ie++) {
                if (Tets[ie].mark != mark) break;
                for (n = 0; n < nptet; n++)
                    tets[nn++] = Tets[ie].nodes[n];
            }
            sprintf(name, "Tet Group %d", mark);
            end = *start + ie - is;
            if (cg_section_write (cgfile, cgbase, cgzone, name,
                    nptet == 10 ? CGNS_ENUMV(TETRA_10) : CGNS_ENUMV(TETRA_4),
                    *start, end-1, 0, tets, &cgsect))
                cg_error_exit();
            *start = end;
            is = ie;
        }
    }
    else {
        for (nn = 0, nt = 0; nt < nTets; nt++) {
            for (n = 0; n < nptet; n++)
                tets[nn++] = Tets[nt].nodes[n];
        }
        end = *start + nTets;
        if (cg_section_write (cgfile, cgbase, cgzone, "Tetrahedra",
                nptet == 10 ? CGNS_ENUMV(TETRA_10) : CGNS_ENUMV(TETRA_4),
                *start, end-1, 0, tets, &cgsect))
            cg_error_exit();
        *start = end;
    }

    free(tets);
    for (nt = 0; nt < nTets; nt++)
        free(Tets[nt].nodes);
    free(Tets);
}

/*===================================================================*/

int main (int argc, char *argv[])
{
    int type = 0, celldim, phydim;
    int cgfile, cgbase, cgzone, cgcoord;
    cgsize_t sizes[3], start;
    char *ext, *basename, *outfile;

    if (argc < 2) {
        fprintf (stderr, "usage:tet2cgns tetgen_file [cgns_file]\n");
        exit (1);
    }

    basename = (char *)malloc(strlen(argv[1]) + 9);
    if (NULL == basename) {
        fprintf (stderr, "malloc failed for basename\n");
        exit (1);
    }
    strcpy(basename, argv[1]);

    if ((ext = strrchr(basename, '.')) != NULL) {
        if (0 == strcasecmp(ext, ".poly")) {
            type = 1;
        }
        else if (0 == strcasecmp(ext, ".smesh")) {
            type = 2;
        }
        else if (0 == strcasecmp(ext, ".node") ||
                 0 == strcasecmp(ext, ".face") ||
                 0 == strcasecmp(ext, ".ele")) {
            type = 3;
        }
    }

    if (type == 0) {
        ext = basename + strlen(basename);
        strcpy(ext, ".node");
        if (0 == access(basename, 0)) {
            type = 3;
        }
        if (type == 0) {
            strcpy(ext, ".1.node");
            if (0 == access(basename, 0)) {
                type = 3;
                ext += 2;
            }
        }
        if (type == 0) {
            strcpy(ext, ".smesh");
            if (0 == access(basename, 0)) {
                type = 2;
            }
        }
        if (type == 0) {
            strcpy(ext, ".poly");
            if (0 == access(basename, 0)) {
                type = 1;
            }
        }
    }

    if (type == 1) {
        strcpy(ext, ".poly");
        if (!read_poly(basename, 1)) return 1;
    }
    else if (type == 2) {
        strcpy(ext, ".smesh");
        if (!read_poly(basename, 0)) return 1;
    }
    else if (type == 3) {
        *ext = 0;
        if (!read_mesh(basename)) return 1;
    }
    else {
        fprintf(stderr, "couldn't determine what to read\n");
        return 1;
    }

    if (argc > 2)
        outfile = argv[2];
    else {
        strcpy (ext, ".cgns");
        outfile = basename;
    }

    phydim = zC == NULL ? 2 : 3;
    sizes[0] = nCoords;
    sizes[2] = 0;
    if (nTets) {
        celldim = 3;
        sizes[1] = nTets;
    }
    else if (nTris || nFaces) {
        celldim = 2;
        sizes[1] = nTris + nFaces;
    }
    else {
        fprintf(stderr, "no elements to write\n");
        return 1;
    }
    printf("writing CGNS to %s\n", outfile);
    fflush(stdout);

    if (cg_open (outfile, CG_MODE_WRITE, &cgfile) ||
        cg_base_write (cgfile, "Base", celldim, phydim, &cgbase) ||
        cg_zone_write (cgfile, cgbase, "Zone", sizes,
            CGNS_ENUMV(Unstructured), &cgzone) ||
        cg_coord_write (cgfile, cgbase, cgzone, COORD_TYPE,
            "CoordinateX", xC, &cgcoord) ||
        cg_coord_write (cgfile, cgbase, cgzone, COORD_TYPE,
            "CoordinateY", yC, &cgcoord))
        cg_error_exit();
    if (zC != NULL &&
        cg_coord_write (cgfile, cgbase, cgzone, COORD_TYPE,
            "CoordinateZ", zC, &cgcoord))
        cg_error_exit();

    start = 1;
    if (nEdges) write_edges(cgfile, cgbase, cgzone, &start);
    if (nTris) write_tris(cgfile, cgbase, cgzone, &start);
    if (nFaces) write_faces(cgfile, cgbase, cgzone, &start);
    if (nTets) write_tets(cgfile, cgbase, cgzone, &start);

    if (cg_close(cgfile)) cg_error_exit();
    return 0;
}
