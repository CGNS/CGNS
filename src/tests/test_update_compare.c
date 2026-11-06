/* Check the public geometry, fields, and elements of the historical test grids. */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"

#define CHECK(call) do { if ((call) != CG_OK) { \
    fprintf(stderr, "%s: %s\n", #call, cg_get_error()); exit(1); } } while (0)
#define SAME(test) do { if (!(test)) { \
    fprintf(stderr, "Comparison differs: %s\n", #test); exit(1); } } while (0)
#define MAX_VALUES 32768

static void values_equal(const double *a, const double *b, cgsize_t n)
{
    cgsize_t i;
    for (i = 0; i < n; ++i) SAME(isfinite(a[i]) && a[i] == b[i]);
}

int main(int argc, char **argv)
{
    int f[2], k, bases[2], cd[2], pd[2], nz[2], nc[2], ns[2], nf[2];
    int actual_type[2], coords, sol, field;
    char name[2][33];
    float version[2];
    cgsize_t size[2][9] = {{0}}, lo = 1, hi, dims[2][3];
    double data[2][MAX_VALUES];
    CGNS_ENUMT(ZoneType_t) zone[2];
    CGNS_ENUMT(DataType_t) type[2];
    CGNS_ENUMT(GridLocation_t) location[2];

    if (argc != 5 || (strcmp(argv[3], "adf") && strcmp(argv[3], "hdf5"))) {
        fprintf(stderr, "usage: test_update_compare old.cgns updated.cgns adf|hdf5 old_version\n");
        return 2;
    }
    for (k = 0; k < 2; ++k) {
        CHECK(cg_open(argv[k + 1], CG_MODE_READ, &f[k]));
        CHECK(cg_get_file_type(f[k], &actual_type[k]));
        SAME(actual_type[k] == (!strcmp(argv[3], "adf") ? CG_FILE_ADF : CG_FILE_HDF5));
        CHECK(cg_version(f[k], &version[k]));
        CHECK(cg_nbases(f[k], &bases[k])); SAME(bases[k] == 1);
        CHECK(cg_base_read(f[k], 1, name[k], &cd[k], &pd[k]));
        CHECK(cg_nzones(f[k], 1, &nz[k])); SAME(nz[k] == 1);
    }
    SAME(fabsf(version[0] - atoi(argv[4]) / 1000.0f) < 0.0001f);
    SAME(version[0] < CGNS_DOTVERS);
    SAME(fabsf(version[1] - CGNS_VERSION / 1000.0f) < 0.0001f);
    SAME(!strcmp(name[0], name[1]) && cd[0] == cd[1] && pd[0] == pd[1]);
    for (k = 0; k < 2; ++k) {
        CHECK(cg_zone_type(f[k], 1, 1, &zone[k]));
        SAME(zone[k] == CGNS_ENUMV(Unstructured));
        CHECK(cg_zone_read(f[k], 1, 1, name[k], size[k]));
        SAME(size[k][0] > 0 && size[k][0] <= MAX_VALUES);
        CHECK(cg_ncoords(f[k], 1, 1, &nc[k])); SAME(nc[k] == 3);
        CHECK(cg_nsols(f[k], 1, 1, &ns[k])); SAME(ns[k] >= 0);
    }
    SAME(!strcmp(name[0], name[1]));
    SAME(!memcmp(size[0], size[1], 3 * sizeof(cgsize_t)));
    SAME(ns[0] == ns[1]); hi = size[0][0];
    for (coords = 1; coords <= nc[0]; ++coords) {
        for (k = 0; k < 2; ++k) {
            CHECK(cg_coord_info(f[k], 1, 1, coords, &type[k], name[k]));
            CHECK(cg_coord_read(f[k], 1, 1, name[k], CGNS_ENUMV(RealDouble), &lo, &hi, data[k]));
        }
        SAME(type[0] == type[1] && !strcmp(name[0], name[1]));
        values_equal(data[0], data[1], hi);
    }
    for (sol = 1; sol <= ns[0]; ++sol) {
        int nd[2]; cgsize_t n = 1;
        for (k = 0; k < 2; ++k) {
            CHECK(cg_sol_info(f[k], 1, 1, sol, name[k], &location[k]));
            CHECK(cg_sol_size(f[k], 1, 1, sol, &nd[k], dims[k]));
            SAME(nd[k] == 1); SAME(dims[k][0] > 0 && dims[k][0] <= MAX_VALUES);
            CHECK(cg_nfields(f[k], 1, 1, sol, &nf[k])); SAME(nf[k] > 0 && nf[k] <= 32);
        }
        SAME(!strcmp(name[0], name[1]) && location[0] == location[1]);
        SAME(dims[0][0] == dims[1][0] && nf[0] == nf[1]); n = dims[0][0];
        for (field = 1; field <= nf[0]; ++field) {
            for (k = 0; k < 2; ++k) {
                CHECK(cg_field_info(f[k], 1, 1, sol, field, &type[k], name[k]));
                CHECK(cg_field_read(f[k], 1, 1, sol, name[k], CGNS_ENUMV(RealDouble), &lo, &n, data[k]));
            }
            SAME(type[0] == type[1] && !strcmp(name[0], name[1]));
            values_equal(data[0], data[1], n);
        }
    }
    /* These historical grids have fixed-size element sections. */
    for (k = 0; k < 2; ++k) CHECK(cg_nsections(f[k], 1, 1, &ns[k]));
    SAME(ns[0] == ns[1] && ns[0] > 0 && ns[0] <= 32);
    for (sol = 1; sol <= ns[0]; ++sol) {
        cgsize_t start[2], end[2]; int boundary[2], parents[2];
        CGNS_ENUMT(ElementType_t) element[2];
        for (k = 0; k < 2; ++k)
            CHECK(cg_section_read(f[k], 1, 1, sol, name[k], &element[k], &start[k], &end[k], &boundary[k], &parents[k]));
        SAME(!strcmp(name[0], name[1]) && element[0] == element[1]);
        SAME(start[0] == start[1] && end[0] == end[1]);
        SAME(boundary[0] == boundary[1] && parents[0] == parents[1]);
        cgsize_t length[2];
        cgsize_t *elements[2], *parent_data[2] = {NULL, NULL};
        size_t parent_count = (size_t)(end[0] - start[0] + 1) * 4;
        for (k = 0; k < 2; ++k)
            CHECK(cg_ElementDataSize(f[k], 1, 1, sol, &length[k]));
        SAME(length[0] == length[1] && length[0] > 0);
        for (k = 0; k < 2; ++k) {
            elements[k] = malloc((size_t)length[0] * sizeof(cgsize_t));
            SAME(elements[k] != NULL);
            if (parents[0]) {
                parent_data[k] = malloc(parent_count * sizeof(cgsize_t));
                SAME(parent_data[k] != NULL);
            }
            CHECK(cg_elements_read(f[k], 1, 1, sol, elements[k], parent_data[k]));
        }
        SAME(!memcmp(elements[0], elements[1], (size_t)length[0] * sizeof(cgsize_t)));
        if (parents[0])
            SAME(!memcmp(parent_data[0], parent_data[1], parent_count * sizeof(cgsize_t)));
        for (k = 0; k < 2; ++k) {
            free(elements[k]);
            free(parent_data[k]);
        }
    }
    CHECK(cg_close(f[0])); CHECK(cg_close(f[1]));
    puts("Old/new public version, backend, geometry, fields and element connectivity match");
    return 0;
}
