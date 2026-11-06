#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"

#define CHECK(call) do { if ((call) != CG_OK) { \
    fprintf(stderr, "%s: %s\n", #call, cg_get_error()); exit(1); \
} } while (0)

static void round_trip(int dim, CGNS_ENUMT(GridLocation_t) location,
                       CGNS_ENUMT(PointSetType_t) set_type)
{
    const char *file = "test_pointset_locations.cgns";
    const cgsize_t zone[3] = {4, 1, 0};
    const cgsize_t cell[4] = {1, 2, 3, 4};
    const cgsize_t faces[6] = {1, 2, 3, 1, 2, 4};
    const cgsize_t edges[4] = {1, 2, 2, 3};
    const double x[4] = {0, 1, dim == 2, 0};
    const double y[4] = {0, 0, 1, dim == 2};
    const double z[4] = {0, 0, 0, 1};
    const int field[2] = {11, 23}, data[2] = {31, 47};
    const cgsize_t first_edge = dim == 3 ? 4 : 2;
    const cgsize_t first = location == CGNS_ENUMV(EdgeCenter) ? first_edge : 2;
    cgsize_t indices[2] = {first, first + 1}, got_indices[2], count, size;
    cgsize_t lo = 1, hi = 2, vertices = 4;
    double got_x[4];
    int fn, b, zone_id, s, d, id, rank, got_values[2];
    CGNS_ENUMT(PointSetType_t) got_type;

    if (set_type == CGNS_ENUMV(PointList)) {
        indices[0] = first + 1;
        indices[1] = first;
    }
    CHECK(cg_open(file, CG_MODE_WRITE, &fn));
    CHECK(cg_base_write(fn, "Base", dim, dim, &b));
    CHECK(cg_zone_write(fn, b, "Zone", zone, CGNS_ENUMV(Unstructured), &zone_id));
    CHECK(cg_coord_write(fn, b, zone_id, CGNS_ENUMV(RealDouble), "CoordinateX", x, &id));
    CHECK(cg_coord_write(fn, b, zone_id, CGNS_ENUMV(RealDouble), "CoordinateY", y, &id));
    if (dim == 3)
        CHECK(cg_coord_write(fn, b, zone_id, CGNS_ENUMV(RealDouble), "CoordinateZ", z, &id));
    CHECK(cg_section_write(fn, b, zone_id, "Cells",
          dim == 3 ? CGNS_ENUMV(TETRA_4) : CGNS_ENUMV(QUAD_4), 1, 1, 0, cell, &id));
    if (dim == 3)
        CHECK(cg_section_write(fn, b, zone_id, "Faces", CGNS_ENUMV(TRI_3),
              2, 3, 0, faces, &id));
    CHECK(cg_section_write(fn, b, zone_id, "Edges", CGNS_ENUMV(BAR_2),
          first_edge, first_edge + 1, 0, edges, &id));
    CHECK(cg_sol_ptset_write(fn, b, zone_id, "Solution", location, set_type, 2, indices, &s));
    CHECK(cg_field_write(fn, b, zone_id, s, CGNS_ENUMV(Integer), "Field", field, &id));
    CHECK(cg_discrete_ptset_write(fn, b, zone_id, "Discrete", location, set_type, 2, indices, &d));
    CHECK(cg_goto(fn, b, "Zone_t", zone_id, "DiscreteData_t", d, "end"));
    CHECK(cg_array_write("Data", CGNS_ENUMV(Integer), 1, &hi, data));
    CHECK(cg_close(fn));

    CHECK(cg_open(file, CG_MODE_READ, &fn));
    CHECK(cg_coord_read(fn, b, zone_id, "CoordinateX", CGNS_ENUMV(RealDouble), &lo, &vertices, got_x));
    if (memcmp(x, got_x, sizeof(x))) exit(1);
    CHECK(cg_sol_size(fn, b, zone_id, s, &rank, &size));
    if (rank != 1 || size != 2) exit(1);
    CHECK(cg_sol_ptset_info(fn, b, zone_id, s, &got_type, &count));
    if (got_type != set_type || count != 2) exit(1);
    CHECK(cg_sol_ptset_read(fn, b, zone_id, s, got_indices));
    if (memcmp(indices, got_indices, sizeof(indices))) exit(1);
    CHECK(cg_field_read(fn, b, zone_id, s, "Field", CGNS_ENUMV(Integer), &lo, &hi, got_values));
    if (memcmp(field, got_values, sizeof(field))) exit(1);

    CHECK(cg_discrete_size(fn, b, zone_id, d, &rank, &size));
    if (rank != 1 || size != 2) exit(1);
    CHECK(cg_discrete_ptset_info(fn, b, zone_id, d, &got_type, &count));
    if (got_type != set_type || count != 2) exit(1);
    CHECK(cg_discrete_ptset_read(fn, b, zone_id, d, got_indices));
    if (memcmp(indices, got_indices, sizeof(indices))) exit(1);
    CHECK(cg_goto(fn, b, "Zone_t", zone_id, "DiscreteData_t", d, "end"));
    CHECK(cg_array_read(1, got_values));
    if (memcmp(data, got_values, sizeof(data))) exit(1);
    CHECK(cg_close(fn));
    if (remove(file)) exit(1);
}

static void check_backend(int backend)
{
    const CGNS_ENUMT(PointSetType_t) sets[] = {CGNS_ENUMV(PointList), CGNS_ENUMV(PointRange)};
    size_t i;
    CHECK(cg_set_file_type(backend));
    for (i = 0; i < sizeof(sets) / sizeof(sets[0]); ++i) {
        round_trip(2, CGNS_ENUMV(EdgeCenter), sets[i]);
        round_trip(3, CGNS_ENUMV(EdgeCenter), sets[i]);
        round_trip(3, CGNS_ENUMV(FaceCenter), sets[i]);
    }
}

int main(void)
{
    check_backend(CG_FILE_ADF);
#if CG_BUILD_HDF5
    check_backend(CG_FILE_HDF5);
#endif
    return 0;
}

