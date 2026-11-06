/*
 * Point-set sizing: whatever the library lets an application write, it must
 * read back, sized as SIDS DataSize() defines it.  With a PointList or
 * PointRange present that is ListLength(), whatever the GridLocation, and a
 * PointRange's Begin and End are opposing corners in either order.
 *
 * Also checks that files whose arrays follow the size older libraries gave
 * a reversed range still open, and that data with no definable size is
 * still rejected.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"
#include "cgns_io.h"

#ifndef CGNS_ENUMT
# define CGNS_ENUMT(e) e
# define CGNS_ENUMV(e) e
#endif

static const char *fname = "test_ptset_datasize.cgns";
static char where[200];        /* current case, for failure messages */
static int nwarnings;
static char last_warning[400];

static void fail(int line, const char *what)
{
    fprintf(stderr, "test_ptset_datasize.c:%d [%s] %s failed: %s\n",
            line, where, what, cg_get_error());
    exit(1);
}

#define CHECK(call) do { if ((call) != CG_OK) fail(__LINE__, #call); } while (0)
#define EXPECT(cond) do { if (!(cond)) fail(__LINE__, #cond); } while (0)

static void expect_open_error(const char *message);

static void capture_warnings(int is_error, char *msg)
{
    if (is_error == 0) {
        nwarnings++;
        strncpy(last_warning, msg, sizeof(last_warning) - 1);
    }
}

/* ---------------------------------------------------------------------- */
/* zones */

enum { U2, U3, S3 };    /* unstructured 2-D, unstructured 3-D, structured 3-D */

/* One cell with its edges (and faces) as their own sections, so point sets
   at EdgeCenter and FaceCenter refer to real elements:
     U2: QUAD_4 cell 1, BAR_2 edges 2-5
     U3: HEXA_8 cell 1, QUAD_4 faces 2-7, BAR_2 edges 8-19
     S3: 3x3x3 vertices */
static void write_zone(int fn, int kind, int *B, int *Z)
{
    static const cgsize_t quad[4] = {1, 2, 3, 4};
    static const cgsize_t square_edges[8] = {1,2, 2,3, 3,4, 4,1};
    static const cgsize_t hexa[8] = {1, 2, 3, 4, 5, 6, 7, 8};
    static const cgsize_t hexa_faces[24] = {
        1,4,3,2, 1,2,6,5, 2,3,7,6, 3,4,8,7, 1,5,8,4, 5,6,7,8};
    static const cgsize_t hexa_edges[24] = {
        1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8, 8,5, 1,5, 2,6, 3,7, 4,8};
    cgsize_t size[9];
    int S;

    if (kind == U2) {
        size[0] = 4; size[1] = 1; size[2] = 0;
        CHECK(cg_base_write(fn, "Base", 2, 2, B));
        CHECK(cg_zone_write(fn, *B, "Zone", size, CGNS_ENUMV(Unstructured), Z));
        CHECK(cg_section_write(fn, *B, *Z, "Cell", CGNS_ENUMV(QUAD_4), 1, 1, 0, quad, &S));
        CHECK(cg_section_write(fn, *B, *Z, "Edges", CGNS_ENUMV(BAR_2), 2, 5, 0, square_edges, &S));
    }
    else if (kind == U3) {
        size[0] = 8; size[1] = 1; size[2] = 0;
        CHECK(cg_base_write(fn, "Base", 3, 3, B));
        CHECK(cg_zone_write(fn, *B, "Zone", size, CGNS_ENUMV(Unstructured), Z));
        CHECK(cg_section_write(fn, *B, *Z, "Cell", CGNS_ENUMV(HEXA_8), 1, 1, 0, hexa, &S));
        CHECK(cg_section_write(fn, *B, *Z, "Faces", CGNS_ENUMV(QUAD_4), 2, 7, 0, hexa_faces, &S));
        CHECK(cg_section_write(fn, *B, *Z, "Edges", CGNS_ENUMV(BAR_2), 8, 19, 0, hexa_edges, &S));
    }
    else {
        size[0] = size[1] = size[2] = 3;
        size[3] = size[4] = size[5] = 2;
        size[6] = size[7] = size[8] = 0;
        CHECK(cg_base_write(fn, "Base", 3, 3, B));
        CHECK(cg_zone_write(fn, *B, "Zone", size, CGNS_ENUMV(Structured), Z));
    }
}

/* ---------------------------------------------------------------------- */
/* point-set solutions written through every API path */

typedef struct {
    const char *name;
    int zone;
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(PointSetType_t) type;
    cgsize_t npnts;           /* number of points in pnts */
    cgsize_t pnts[6];
    cgsize_t count;           /* SIDS ListLength() */
} ptset_case;

static const ptset_case cases[] = {
    {"U2 EdgeCenter list",       U2, CGNS_ENUMV(EdgeCenter), CGNS_ENUMV(PointList),  2, {5, 3}, 2},
    {"U2 EdgeCenter range",      U2, CGNS_ENUMV(EdgeCenter), CGNS_ENUMV(PointRange), 2, {2, 4}, 3},
    {"U2 EdgeCenter reversed",   U2, CGNS_ENUMV(EdgeCenter), CGNS_ENUMV(PointRange), 2, {4, 2}, 3},
    {"U3 EdgeCenter list",       U3, CGNS_ENUMV(EdgeCenter), CGNS_ENUMV(PointList),  2, {12, 9}, 2},
    {"U3 EdgeCenter range",      U3, CGNS_ENUMV(EdgeCenter), CGNS_ENUMV(PointRange), 2, {8, 11}, 4},
    {"U3 EdgeCenter reversed",   U3, CGNS_ENUMV(EdgeCenter), CGNS_ENUMV(PointRange), 2, {11, 8}, 4},
    {"U3 FaceCenter list",       U3, CGNS_ENUMV(FaceCenter), CGNS_ENUMV(PointList),  2, {7, 2}, 2},
    {"U3 FaceCenter range",      U3, CGNS_ENUMV(FaceCenter), CGNS_ENUMV(PointRange), 2, {2, 5}, 4},
    {"U3 FaceCenter reversed",   U3, CGNS_ENUMV(FaceCenter), CGNS_ENUMV(PointRange), 2, {5, 2}, 4},
    {"U3 Vertex reversed",       U3, CGNS_ENUMV(Vertex),     CGNS_ENUMV(PointRange), 2, {6, 3}, 4},
    {"S3 EdgeCenter list",       S3, CGNS_ENUMV(EdgeCenter), CGNS_ENUMV(PointList),  2, {1,1,1, 2,2,1}, 2},
    {"S3 EdgeCenter range",      S3, CGNS_ENUMV(EdgeCenter), CGNS_ENUMV(PointRange), 2, {1,1,1, 2,2,1}, 4},
    {"S3 FaceCenter reversed",   S3, CGNS_ENUMV(FaceCenter), CGNS_ENUMV(PointRange), 2, {2,2,1, 1,1,1}, 4},
    {"S3 Vertex reversed",       S3, CGNS_ENUMV(Vertex),     CGNS_ENUMV(PointRange), 2, {3,3,1, 1,1,1}, 9},
    {"S3 Vertex half reversed",  S3, CGNS_ENUMV(Vertex),     CGNS_ENUMV(PointRange), 2, {3,1,1, 1,3,1}, 9},
};

static void check_sol(int fn, int B, int Z, int S, const ptset_case *c,
                      const double *values)
{
    char name[33];
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(PointSetType_t) type;
    cgsize_t npnts, size[3], one = 1, pnts[6];
    double got[9];
    int rank;

    CHECK(cg_sol_info(fn, B, Z, S, name, &location));
    EXPECT(location == c->location);
    CHECK(cg_sol_size(fn, B, Z, S, &rank, size));
    EXPECT(rank == 1 && size[0] == c->count);
    CHECK(cg_sol_ptset_info(fn, B, Z, S, &type, &npnts));
    EXPECT(type == c->type && npnts == c->npnts);
    CHECK(cg_sol_ptset_read(fn, B, Z, S, pnts));
    EXPECT(!memcmp(pnts, c->pnts, (size_t)(c->npnts * (c->zone == S3 ? 3 : 1)) * sizeof(cgsize_t)));
    CHECK(cg_field_read(fn, B, Z, S, "Q", CGNS_ENUMV(RealDouble), &one, size, got));
    EXPECT(!memcmp(got, values, (size_t)c->count * sizeof(double)));
}

static void ptset_round_trip(const ptset_case *c)
{
    static const double values[9] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    cgsize_t count = c->count, size[3];
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(PointSetType_t) type;
    cgsize_t npnts;
    double got[9];
    int fn, B, Z, S, D, G, F, rank;
    char name[33];

    /* write */
    CHECK(cg_open(fname, CG_MODE_WRITE, &fn));
    write_zone(fn, c->zone, &B, &Z);

    CHECK(cg_sol_ptset_write(fn, B, Z, "Sol", c->location, c->type,
                             c->npnts, c->pnts, &S));
    CHECK(cg_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "Q", values, &F));

    CHECK(cg_discrete_ptset_write(fn, B, Z, "Disc", c->location, c->type,
                                  c->npnts, c->pnts, &D));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "DiscreteData_t", D, "end"));
    CHECK(cg_array_write("Q", CGNS_ENUMV(RealDouble), 1, &count, values));

    /* the same solution through cg_goto: location and point set added to
       an existing node */
    CHECK(cg_sol_write(fn, B, Z, "GotoSol", CGNS_ENUMV(Vertex), &G));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "FlowSolution_t", G, "end"));
    if (c->location != CGNS_ENUMV(Vertex))
        CHECK(cg_gridlocation_write(c->location));
    CHECK(cg_ptset_write(c->type, c->npnts, c->pnts));
    CHECK(cg_field_write(fn, B, Z, G, CGNS_ENUMV(RealDouble), "Q", values, &F));
    CHECK(cg_close(fn));

    /* read back */
    CHECK(cg_open(fname, CG_MODE_READ, &fn));
    check_sol(fn, B, Z, S, c, values);
    check_sol(fn, B, Z, G, c, values);

    CHECK(cg_discrete_read(fn, B, Z, D, name));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "DiscreteData_t", D, "end"));
    CHECK(cg_gridlocation_read(&location));
    EXPECT(location == c->location);
    CHECK(cg_discrete_size(fn, B, Z, D, &rank, size));
    EXPECT(rank == 1 && size[0] == c->count);
    CHECK(cg_discrete_ptset_info(fn, B, Z, D, &type, &npnts));
    EXPECT(type == c->type && npnts == c->npnts);
    CHECK(cg_goto(fn, B, "Zone_t", Z, "DiscreteData_t", D, "end"));
    CHECK(cg_array_read(1, got));
    EXPECT(!memcmp(got, values, (size_t)c->count * sizeof(double)));
    CHECK(cg_close(fn));
}

/* ---------------------------------------------------------------------- */
/* other writers that size a PointRange or take a GridLocation */

static void other_writers(void)
{
    /* 5x5x5 vertices; KMin face given End-first: 5*5 points */
    static const cgsize_t zsize[9] = {5,5,5, 4,4,4, 0,0,0};
    static const cgsize_t face[6] = {5,5,1, 1,1,1};
    static const cgsize_t holes[12] = {1,1,1, 2,2,2, 4,4,4, 2,2,2};
    cgsize_t n25 = 25, donor[75], dims[3] = {5, 5, 3}, npnts, nnormal;
    double values[75], normals[75];
    int fn, B, Z, Z2, BC, DS, A, I, H, ndataset, i;
    int normalindex[3] = {0, 0, 1};
    char name[33];
    CGNS_ENUMT(BCType_t) bctype;
    CGNS_ENUMT(PointSetType_t) type;
    CGNS_ENUMT(DataType_t) normaltype;
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(ArbitraryGridMotionType_t) motion;

    for (i = 0; i < 75; i++) {
        values[i] = i;
        normals[i] = i % 3 == 2 ? -1.0 : 0.0;
        donor[i] = 1;
    }

    snprintf(where + strlen(where), sizeof(where) - strlen(where), " other writers");
    CHECK(cg_open(fname, CG_MODE_WRITE, &fn));
    CHECK(cg_base_write(fn, "Base", 3, 3, &B));
    CHECK(cg_zone_write(fn, B, "Zone", zsize, CGNS_ENUMV(Structured), &Z));
    CHECK(cg_zone_write(fn, B, "Zone2", zsize, CGNS_ENUMV(Structured), &Z2));

    /* BC with normals and Dirichlet data, one value per face point */
    CHECK(cg_boco_write(fn, B, Z, "KMin", CGNS_ENUMV(BCWall),
                        CGNS_ENUMV(PointRange), 2, face, &BC));
    CHECK(cg_boco_normal_write(fn, B, Z, BC, normalindex, 1,
                               CGNS_ENUMV(RealDouble), normals));
    CHECK(cg_dataset_write(fn, B, Z, BC, "Data", CGNS_ENUMV(BCWall), &DS));
    CHECK(cg_bcdata_write(fn, B, Z, BC, DS, CGNS_ENUMV(Dirichlet)));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "ZoneBC_t", 1, "BC_t", BC,
                  "BCDataSet_t", DS, "BCData_t", CGNS_ENUMV(Dirichlet), "end"));
    CHECK(cg_array_write("P", CGNS_ENUMV(RealDouble), 1, &n25, values));

    /* abutting interface: the donor count must match the range */
    CHECK(cg_conn_write(fn, B, Z, "Interface", CGNS_ENUMV(Vertex),
                        CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointRange), 2,
                        face, "Zone2", CGNS_ENUMV(Structured),
                        CGNS_ENUMV(PointListDonor), CGNS_ENUMV(Integer), 25,
                        donor, &I));

    /* overset holes with ranges of different sizes */
    CHECK(cg_hole_write(fn, B, Z, "Holes", CGNS_ENUMV(Vertex),
                        CGNS_ENUMV(PointRange), 2, 4, holes, &H));

    /* ArbitraryGridMotion at FaceCenter: accepted on write, so readable */
    CHECK(cg_arbitrary_motion_write(fn, B, Z, "Motion",
                                    CGNS_ENUMV(DeformingGrid), &A));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "ArbitraryGridMotion_t", A, "end"));
    CHECK(cg_gridlocation_write(CGNS_ENUMV(FaceCenter)));
    CHECK(cg_array_write("GridVelocityX", CGNS_ENUMV(RealDouble), 3, dims, values));
    CHECK(cg_close(fn));

    CHECK(cg_open(fname, CG_MODE_READ, &fn));
    CHECK(cg_boco_info(fn, B, Z, BC, name, &bctype, &type, &npnts,
                       normalindex, &nnormal, &normaltype, &ndataset));
    EXPECT(type == CGNS_ENUMV(PointRange) && npnts == 2);
    EXPECT(nnormal == 3 * 25 && ndataset == 1);
    CHECK(cg_arbitrary_motion_read(fn, B, Z, A, name, &motion));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "ArbitraryGridMotion_t", A, "end"));
    CHECK(cg_gridlocation_read(&location));
    EXPECT(location == CGNS_ENUMV(FaceCenter));
    CHECK(cg_close(fn));
}

/* ---------------------------------------------------------------------- */
/* files written with the size older libraries gave a reversed range */

/* Rewrite a node's data in place, as another writer could have stored it */
static void rewrite_node(const char *path, const char *type, int ndims,
                         const cgsize_t *dims, const void *data)
{
    int cgio;
    double root, id;

    if (cgio_open_file(fname, CGIO_MODE_MODIFY, CGIO_FILE_NONE, &cgio) ||
        cgio_get_root_id(cgio, &root) ||
        cgio_get_node_id(cgio, root, path, &id) ||
        cgio_set_dimensions(cgio, id, type, ndims, dims) ||
        cgio_write_all_data(cgio, id, data) ||
        cgio_close_file(cgio)) {
        char msg[CGIO_MAX_ERROR_LENGTH + 1];
        cgio_error_message(msg);
        fprintf(stderr, "[%s] rewriting %s: %s\n", where, path, msg);
        exit(1);
    }
}

static void resize_array(const char *path, cgsize_t n, const double *values)
{
    rewrite_node(path, "R8", 1, &n, values);
}

/* 5x5x5 vertices, with a point-set solution (fields Q and R) and a BC
   (normals and Dirichlet data P), both on the KMin face given End-first in
   I and J.  SIDS size 5*5 = 25; older libraries computed
   (1-5+1)*(1-5+1)*1 = 9. */
static void write_face_data(void)
{
    static const cgsize_t zsize[9] = {5,5,5, 4,4,4, 0,0,0};
    static const cgsize_t face[6] = {5,5,1, 1,1,1};
    int normalindex[3] = {0, 0, 1};
    cgsize_t n25 = 25;
    double values[75];
    int fn, B, Z, S, F, BC, DS, i;

    for (i = 0; i < 75; i++) values[i] = i;
    CHECK(cg_open(fname, CG_MODE_WRITE, &fn));
    CHECK(cg_base_write(fn, "Base", 3, 3, &B));
    CHECK(cg_zone_write(fn, B, "Zone", zsize, CGNS_ENUMV(Structured), &Z));
    CHECK(cg_sol_ptset_write(fn, B, Z, "Sol", CGNS_ENUMV(Vertex),
                             CGNS_ENUMV(PointRange), 2, face, &S));
    CHECK(cg_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "Q", values, &F));
    CHECK(cg_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "R", values, &F));
    CHECK(cg_boco_write(fn, B, Z, "KMin", CGNS_ENUMV(BCWall),
                        CGNS_ENUMV(PointRange), 2, face, &BC));
    CHECK(cg_boco_normal_write(fn, B, Z, BC, normalindex, 1,
                               CGNS_ENUMV(RealDouble), values));
    CHECK(cg_dataset_write(fn, B, Z, BC, "Data", CGNS_ENUMV(BCWall), &DS));
    CHECK(cg_bcdata_write(fn, B, Z, BC, DS, CGNS_ENUMV(Dirichlet)));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "ZoneBC_t", 1, "BC_t", BC,
                  "BCDataSet_t", DS, "BCData_t", CGNS_ENUMV(Dirichlet), "end"));
    CHECK(cg_array_write("P", CGNS_ENUMV(RealDouble), 1, &n25, values));
    CHECK(cg_close(fn));
}

static void legacy_sizes(void)
{
    static const cgsize_t normal_dims[2] = {3, 9};
    cgsize_t size[3], one = 1, npnts, nnormal, dims[12];
    double values[75], got[75];
    int fn, rank, ndataset, nfields, ndims, normalindex[3], F, i;
    char name[33];
    CGNS_ENUMT(BCType_t) bctype;
    CGNS_ENUMT(PointSetType_t) type;
    CGNS_ENUMT(DataType_t) dtype;

    for (i = 0; i < 75; i++) values[i] = i;

    snprintf(where + strlen(where), sizeof(where) - strlen(where), " legacy sizes");
    write_face_data();

    /* what an older library wrote: 9 values per array */
    resize_array("/Base/Zone/Sol/Q", 9, values);
    resize_array("/Base/Zone/Sol/R", 9, values);
    rewrite_node("/Base/Zone/ZoneBC/KMin/InwardNormalList", "R8", 2,
                 normal_dims, values);
    resize_array("/Base/Zone/ZoneBC/KMin/Data/DirichletData/P", 9, values);

    nwarnings = 0;
    CHECK(cg_configure(CG_CONFIG_ERROR, (void *)capture_warnings));
    CHECK(cg_open(fname, CG_MODE_READ, &fn));
    CHECK(cg_configure(CG_CONFIG_ERROR, NULL));
    EXPECT(nwarnings == 2);    /* one per point set */
    EXPECT(strstr(last_warning, "older CGNS libraries") != NULL);

    /* the API reports, and reads, what the file holds */
    CHECK(cg_sol_size(fn, 1, 1, 1, &rank, size));
    EXPECT(rank == 1 && size[0] == 9);
    CHECK(cg_field_read(fn, 1, 1, 1, "R", CGNS_ENUMV(RealDouble), &one, size, got));
    EXPECT(!memcmp(got, values, 9 * sizeof(double)));
    CHECK(cg_boco_info(fn, 1, 1, 1, name, &bctype, &type, &npnts,
                       normalindex, &nnormal, &dtype, &ndataset));
    EXPECT(nnormal == 3 * 9);
    CHECK(cg_boco_read(fn, 1, 1, 1, dims, got));
    EXPECT(!memcmp(got, values, 27 * sizeof(double)));
    CHECK(cg_goto(fn, 1, "Zone_t", 1, "ZoneBC_t", 1, "BC_t", 1,
                  "BCDataSet_t", 1, "BCData_t", CGNS_ENUMV(Dirichlet), "end"));
    CHECK(cg_array_info(1, name, &dtype, &ndims, dims));
    EXPECT(ndims == 1 && dims[0] == 9);
    CHECK(cg_array_read(1, got));
    EXPECT(!memcmp(got, values, 9 * sizeof(double)));
    CHECK(cg_close(fn));

    /* modifying the file keeps its arrays consistent */
    CHECK(cg_configure(CG_CONFIG_ERROR, (void *)capture_warnings));
    CHECK(cg_open(fname, CG_MODE_MODIFY, &fn));
    CHECK(cg_configure(CG_CONFIG_ERROR, NULL));
    CHECK(cg_field_write(fn, 1, 1, 1, CGNS_ENUMV(RealDouble), "T", values, &F));
    CHECK(cg_close(fn));
    CHECK(cg_configure(CG_CONFIG_ERROR, (void *)capture_warnings));
    CHECK(cg_open(fname, CG_MODE_READ, &fn));
    CHECK(cg_configure(CG_CONFIG_ERROR, NULL));
    CHECK(cg_nfields(fn, 1, 1, 1, &nfields));
    CHECK(cg_sol_size(fn, 1, 1, 1, &rank, size));
    EXPECT(nfields == 3 && size[0] == 9);
    CHECK(cg_close(fn));
}

/* arrays sized against one point set must agree with each other */
static void mixed_sizes(void)
{
    static const double values[25] = {0};
    char base[sizeof(where)];

    strcpy(base, where);

    snprintf(where, sizeof(where), "%.100s SIDS size then old size", base);
    write_face_data();
    resize_array("/Base/Zone/Sol/R", 9, values);
    expect_open_error("Invalid field array dimension for ptset solution");

    snprintf(where, sizeof(where), "%.100s old size then SIDS size", base);
    write_face_data();
    resize_array("/Base/Zone/Sol/Q", 9, values);
    expect_open_error("Invalid field array dimension for ptset solution");

    snprintf(where, sizeof(where), "%.100s BC normals then data", base);
    write_face_data();
    resize_array("/Base/Zone/ZoneBC/KMin/Data/DirichletData/P", 9, values);
    expect_open_error("Wrong array size for Dirichlet data");
}

/* a range with more points than a cgsize_t holds still opens, as it did
   with older libraries, but has no usable size */
static void huge_range(void)
{
    static const cgsize_t dims[2] = {3, 2};
    static const int range[6] = {1, 1, 1, 2147483647, 2147483647, 2147483647};
    int fn;

    snprintf(where + strlen(where), sizeof(where) - strlen(where), " huge range");
    write_face_data();
    rewrite_node("/Base/Zone/ZoneBC/KMin/PointRange", "I4", 2, dims, range);
    /* drop the arrays sized against the original range */
    {
        int cgio;
        double root, bc, id;
        if (cgio_open_file(fname, CGIO_MODE_MODIFY, CGIO_FILE_NONE, &cgio) ||
            cgio_get_root_id(cgio, &root) ||
            cgio_get_node_id(cgio, root, "/Base/Zone/ZoneBC/KMin", &bc) ||
            cgio_get_node_id(cgio, bc, "InwardNormalList", &id) ||
            cgio_delete_node(cgio, bc, id) ||
            cgio_get_node_id(cgio, bc, "Data", &id) ||
            cgio_delete_node(cgio, bc, id) ||
            cgio_close_file(cgio))
            fail(__LINE__, "removing BC arrays");
    }

    nwarnings = 0;
    CHECK(cg_configure(CG_CONFIG_ERROR, (void *)capture_warnings));
    CHECK(cg_open(fname, CG_MODE_READ, &fn));
    CHECK(cg_configure(CG_CONFIG_ERROR, NULL));
    EXPECT(nwarnings == 1);
    EXPECT(strstr(last_warning, "exceeds the largest cgsize_t") != NULL);
    CHECK(cg_close(fn));
}

/* ---------------------------------------------------------------------- */
/* data with no definable size is still rejected, with a usable message */

static void expect_open_error(const char *message)
{
    int fn, ier;
    CHECK(cg_configure(CG_CONFIG_ERROR, (void *)capture_warnings));
    ier = cg_open(fname, CG_MODE_READ, &fn);
    CHECK(cg_configure(CG_CONFIG_ERROR, NULL));
    if (ier == CG_OK) {
        cg_close(fn);
        fail(__LINE__, "cg_open rejecting the file");
    }
    if (strstr(cg_get_error(), message) == NULL)
        fail(__LINE__, message);
    /* a failed cg_open must release the file (Windows cannot remove an
       open file) */
    if (remove(fname))
        fail(__LINE__, "removing the file after a failed cg_open");
}

static void rejections(void)
{
    static const cgsize_t list[2] = {9, 12};
    static const double values[3] = {1, 2, 3};
    char base[sizeof(where)];
    int fn, B, Z, S, D, F;

    strcpy(base, where);

    /* EdgeCenter without a point set: SIDS gives it no size */
    snprintf(where, sizeof(where), "%.100s no point set (FlowSolution_t)", base);
    CHECK(cg_open(fname, CG_MODE_WRITE, &fn));
    write_zone(fn, U3, &B, &Z);
    CHECK(cg_sol_write(fn, B, Z, "Sol", CGNS_ENUMV(Vertex), &S));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "FlowSolution_t", S, "end"));
    CHECK(cg_gridlocation_write(CGNS_ENUMV(EdgeCenter)));
    CHECK(cg_close(fn));
    expect_open_error("FlowSolution_t 'Sol': GridLocation EdgeCenter at "
                      "CellDimension 3 needs a PointList or PointRange");

    snprintf(where, sizeof(where), "%.100s no point set (DiscreteData_t)", base);
    CHECK(cg_open(fname, CG_MODE_WRITE, &fn));
    write_zone(fn, U3, &B, &Z);
    CHECK(cg_discrete_write(fn, B, Z, "Disc", &D));
    CHECK(cg_goto(fn, B, "Zone_t", Z, "DiscreteData_t", D, "end"));
    CHECK(cg_gridlocation_write(CGNS_ENUMV(FaceCenter)));
    CHECK(cg_close(fn));
    expect_open_error("DiscreteData_t 'Disc': GridLocation FaceCenter at "
                      "CellDimension 3 needs a PointList or PointRange");

    /* a field that does not match its point set */
    snprintf(where, sizeof(where), "%.100s wrong field length", base);
    CHECK(cg_open(fname, CG_MODE_WRITE, &fn));
    write_zone(fn, U3, &B, &Z);
    CHECK(cg_sol_ptset_write(fn, B, Z, "Sol", CGNS_ENUMV(EdgeCenter),
                             CGNS_ENUMV(PointList), 2, list, &S));
    CHECK(cg_field_write(fn, B, Z, S, CGNS_ENUMV(RealDouble), "Q", values, &F));
    CHECK(cg_close(fn));
    resize_array("/Base/Zone/Sol/Q", 3, values);
    expect_open_error("Invalid field array dimension for ptset solution");
}

/* ---------------------------------------------------------------------- */

static void run(int file_type, const char *backend)
{
    size_t i;

    CHECK(cg_set_file_type(file_type));
    for (i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        snprintf(where, sizeof(where), "%s, %s", backend, cases[i].name);
        ptset_round_trip(&cases[i]);
    }
    snprintf(where, sizeof(where), "%s,", backend);
    other_writers();
    snprintf(where, sizeof(where), "%s,", backend);
    legacy_sizes();
    snprintf(where, sizeof(where), "%s,", backend);
    mixed_sizes();
    snprintf(where, sizeof(where), "%s,", backend);
    huge_range();
    snprintf(where, sizeof(where), "%s,", backend);
    rejections();
}

int main(void)
{
    run(CG_FILE_ADF, "ADF");
#if CG_BUILD_HDF5
    run(CG_FILE_HDF5, "HDF5");
#endif
    remove(fname);
    puts("test_ptset_datasize: all point-set sizing checks passed");
    return 0;
}
