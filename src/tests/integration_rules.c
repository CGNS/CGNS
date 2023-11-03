#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) && !defined(__NUTC__)
#include <io.h>
#define unlink _unlink
#else
#include <unistd.h>
#endif

#include "cgnslib.h"
/* Define keywords */
#define ReferenceSpace_s   "ReferenceSpace"
#define ElementSpace_ts    "ElementSpace_t"
#define Barycentric_s      "Barycentric"
#define Parametric_s       "Parametric"
#define CGNSBase_ts        "CGNSBase_t"
#define Zone_ts            "Zone_t"
#define Elements_ts        "Elements_t"
#define Cartesian_s        "Cartesian"
#define ParametricR_s      "ParametricR"
#define ParametricS_s      "ParametricS"
#define ParametricT_s      "ParametricT"
#define IntegrationRules_s "ItgRules"
#define RulesCollection_s   "RulesCollection"
#define RulesCollection_ts  "RulesCollection_t"
#define IdToQualifier_s     "IdToQualifier"
#define MapName_ts          "MapName_t"
#define IntegrationRule_s   "IntegrationRule"
#define IntegrationRule_ts  "IntegrationRule_t"

#ifndef CGNSTYPES_H
#define cgsize_t int
#endif
#ifndef CGNS_ENUMT
#define CGNS_ENUMT(e) e
#define CGNS_ENUMV(e) e
#endif

int num_coord;
float *xcoord, *ycoord, *zcoord;
float *var_fourier_1;
double *var_fourier_2;

/* from cgns_internal: so we can reset expected error messages */
void cgi_error(const char *format, ...);

int CellDim = 3, PhyDim = 3;

int cgfile, cgbase, cgzone;
cgsize_t size[9];

char errmsg[256];

typedef char char_33[33];

typedef struct {
    int sect_num;
    int sect_dim;
    cgsize_t start;
    cgsize_t end;
    char *path;
    int size_ids;
    int *ids;
} section_element_association;

typedef struct {
    int num_associates;
    section_element_association *associates;
} element_association_list;

typedef struct {
    int num_ids;
    int *rule_ids;
    char_33 *rule_names;
    ElementType_t *types;
} cgrule_storage;

typedef struct {
    int num_rules;
    cgrule_storage *rules;
} cgrule_collection;

void init_data();
void write_structured();
void test_complex_solution();
void release_data();

void error_exit(char *where) {
  fprintf(stderr, "ERROR:%s:%s\n", where, cg_get_error());
  exit(1);
}

int main(int argc, char *argv[])
{

    char outfile[32];
    int nbases;
    int cgrule1, cgrule2, cgrule3;
    int itgtetra, itgpyra, itghex, itgpenta, itgtri, itgquad, itgbaryquad, itgbarytri;

    element_association_list rule_mapper;
    rule_mapper.num_associates = 0;
    rule_mapper.associates = NULL;

    cgsize_t *itg_value = NULL;
    int num_valid = 0;
    int *index_sect = NULL;

    cgrule_collection my_rule_collection;
    int S;

    strcpy(outfile, "test_case.cgns");
    if (argc > 1) {
        int type = 0;
        int n = 0;
        if (argv[1][n] == '-') n++;
        if (argv[1][n] == 'a' || argv[1][n] == 'A' || argv[1][n] == '1') {
            if (NULL != strchr(argv[1], '2'))
                type = CG_FILE_ADF2;
            else
                type = CG_FILE_ADF;
        } else if (argv[1][n] == 'h' || argv[1][n] == 'H' || argv[1][n] == '2') {
            type = CG_FILE_HDF5;
        } else if (argv[1][n] == '3') {
            type = CG_FILE_ADF2;
        } else {
            fprintf(stderr, "unknown option\n");
            exit(1);
        }
        if (cg_set_file_type(type)) error_exit("cg_set_file_type");
    }

    if (cg_open(outfile, CG_MODE_MODIFY, &cgfile)) error_exit("cg_open");
    if (cg_nbases(cgfile, &nbases)) error_exit("cg_nbases");

    if (nbases > 0) {
        cgbase = 1;

        if (cg_rcollection_write(cgfile, cgbase, "irc1", &cgrule1))
            error_exit("cg_rules_collection_write");
        if (cg_rcollection_write(cgfile, cgbase, "irc2", &cgrule2))
            error_exit("cg_rules_collection_write");
        if (cg_rcollection_write(cgfile, cgbase, "irc3", &cgrule3))
            error_exit("cg_rules_collection_write");

        double coeff_a = (5.0 - sqrt(5.0)) / 20.;
        double coeff_b = sqrt(5.0) / 5.0;
        double param[3][4];
        param[0][0] = 0.0;
        param[1][0] = 0.0;
        param[2][0] = 0.0;
        param[0][1] = 0.0;
        param[1][1] = 0.0;
        param[2][1] = 1.0;
        param[0][2] = 0.0;
        param[1][2] = 1.0;
        param[2][2] = 0.0;
        param[0][3] = 1.0;
        param[1][3] = 0.0;
        param[2][3] = 0.0;
        for (int ii = 0; ii < 3; ii++) {
            for (int jj = 0; jj < 4; jj++) {
                param[ii][jj] *= coeff_b;
                param[ii][jj] += coeff_a;
            }
        }
        double weights[4] = {1.0 / 24., 1.0 / 24., 1.0 / 24., 1.0 / 24.};
        int npoints;
        cg_npe(TETRA_4, &npoints);
        cg_itg_rule_parametric_write(cgfile, cgbase, cgrule1, "tetra", TETRA_4, npoints,
                                     3, RealDouble, param, weights, &itgtetra);

        double param_pyra[3][1] = {{0.0}, {0.0}, {1. / 5.}};
        double weights_pyra[1] = {2. / 3.};

        cg_itg_rule_parametric_write(cgfile, cgbase, cgrule2, "pyra", PYRA_5, 1, 3,
                                     RealDouble, param_pyra, weights_pyra, &itgpyra);

        // Go to pyra and delete ReferenceSpace for validation
        // cg_goto(...)

        //--------------------------------------------------------------------------
        double coeff = sqrt(3.) / 3.;
        double param_hex[3][8];
        param_hex[0][0] = -1.0;
        param_hex[1][0] = -1.0;
        param_hex[2][0] = -1.0;
        param_hex[0][1] = -1.0;
        param_hex[1][1] = -1.0;
        param_hex[2][1] = 1.0;
        param_hex[0][2] = -1.0;
        param_hex[1][2] = 1.0;
        param_hex[2][2] = -1.0;
        param_hex[0][3] = -1.0;
        param_hex[1][3] = 1.0;
        param_hex[2][3] = 1.0;
        param_hex[0][4] = 1.0;
        param_hex[1][4] = -1.0;
        param_hex[2][4] = -1.0;
        param_hex[0][5] = 1.0;
        param_hex[1][5] = -1.0;
        param_hex[2][5] = 1.0;
        param_hex[0][6] = 1.0;
        param_hex[1][6] = 1.0;
        param_hex[2][6] = -1.0;
        param_hex[0][7] = 1.0;
        param_hex[1][7] = 1.0;
        param_hex[2][7] = 1.0;
        for (int ii = 0; ii < 3; ii++) {
            for (int jj = 0; jj < 8; jj++) {
                param_hex[ii][jj] *= coeff;
                param_hex[ii][jj] =
                    (param_hex[ii][jj] + 1.0) * 0.5; // Transform to [0.0, 1.0] interval
            }
        }
        double weights_hex[8] = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};

        cg_itg_rule_parametric_write(cgfile, cgbase, cgrule2, "hexa", HEXA_8, 8, 3,
                                     RealDouble, param_hex, weights_hex, &itghex);
        //--------------------------------------------------------------------------
        double param_penta[3][2];
        param_penta[0][0] = -1.0 / sqrt(3.0);
        param_penta[1][0] = 1.0 / 3.0;
        param_penta[2][0] = 1.0 / 3.0;
        param_penta[0][1] = 1.0 / sqrt(3.0);
        param_penta[1][1] = 1.0 / 3.0;
        param_penta[2][1] = 1.0 / 3.0;

        double weights_penta[2] = {0.5, 0.5};

        cg_itg_rule_parametric_write(cgfile, cgbase, cgrule1, "penta", PENTA_6, 2, 3,
                                     RealDouble, param_penta, weights_penta, &itgpenta);

        //--------------------------------------------------------------------------
        double param_tri[2][1];
        param_tri[0][0] = 1.0 / 3.0;
        param_tri[1][0] = 1.0 / 3.0;

        double weights_tri[1] = {0.5};
        cg_itg_rule_parametric_write(cgfile, cgbase, cgrule2, "tri", TRI_3, 1, 2,
                                     RealDouble, param_tri, weights_tri, &itgtri);

        //--------------------------------------------------------------------------
        double param_quad[2][4];
        param_quad[0][0] = -1.0 / sqrt(3.0);
        param_quad[1][0] = -1.0 / sqrt(3.0);
        param_quad[0][1] = -1.0 / sqrt(3.0);
        param_quad[1][1] = 1.0 / sqrt(3.0);
        param_quad[0][2] = 1.0 / sqrt(3.0);
        param_quad[1][2] = -1.0 / sqrt(3.0);
        param_quad[0][3] = 1.0 / sqrt(3.0);
        param_quad[1][3] = 1.0 / sqrt(3.0);

        double weights_quad[4] = {1.0, 1.0, 1.0, 1.0};
        //
        for (int ii = 0; ii < 2; ii++) {
            for (int jj = 0; jj < 4; jj++) {
                param_quad[ii][jj] =
                    (param_quad[ii][jj] + 1.0) * 0.5; // Transform to [0.0, 1.0] interval
            }
        }
        //
        cg_itg_rule_parametric_write(cgfile, cgbase, cgrule2, "quad", QUAD_4, 4, 2,
                                     RealDouble, param_penta, weights_quad, &itgquad);

        //==========================================================================
        double pos_itg_tri[3][1];

        pos_itg_tri[0][0] = 1.0 / 3.0;
        pos_itg_tri[1][0] = 1.0 / 3.0;
        pos_itg_tri[2][0] = 1.0 / 3.0;

        double weights_barytri[1] = {0.5};
        cg_itg_rule_barycentric_write(cgfile, cgbase, cgrule3, "barytri", TRI_3, 1, 3,
                                      RealDouble, pos_itg_tri, weights_barytri,
                                      &itgbarytri);
        //--------------------------------------------------------------------------
        double pos_itg_quad[4][4];
        pos_itg_quad[0][0] = (1 + sqrt(3)) / 4;
        pos_itg_quad[0][1] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[0][2] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[0][3] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[1][0] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[1][1] = (1 + sqrt(3)) / 4;
        pos_itg_quad[1][2] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[1][3] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[2][0] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[2][1] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[2][2] = (1 + sqrt(3)) / 4;
        pos_itg_quad[2][3] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[3][0] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[3][1] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[3][2] = (1 - 1 / sqrt(3)) / 4;
        pos_itg_quad[3][3] = (1 + sqrt(3)) / 4;
        double weights_baryquad[4] = {1.0, 1.0, 1.0, 1.0};

        cg_itg_rule_barycentric_write(cgfile, cgbase, cgrule3, "baryquad", QUAD_4, 4, 4,
                                      RealDouble, pos_itg_quad, weights_baryquad,
                                      &itgbaryquad);

        my_rule_collection.num_rules = 3;
        my_rule_collection.rules = (cgrule_storage *)malloc(3 * sizeof(cgrule_storage));

        my_rule_collection.rules[0].num_ids = 2;
        my_rule_collection.rules[0].rule_ids = (int *)malloc(2 * sizeof(int));
        my_rule_collection.rules[0].rule_names = (char_33 *)malloc(2 * sizeof(char_33));
        my_rule_collection.rules[0].types =
            (ElementType_t *)malloc(2 * sizeof(ElementType_t));

        my_rule_collection.rules[0].rule_ids[0] = 1;
        my_rule_collection.rules[0].rule_ids[1] = 2;

        strcpy(my_rule_collection.rules[0].rule_names[0], "tetra");
        strcpy(my_rule_collection.rules[0].rule_names[1], "penta");

        my_rule_collection.rules[0].types[0] = TETRA_4;
        my_rule_collection.rules[0].types[1] = PENTA_6;

        cg_rcollection_idtoqualifier_write(
            cgfile, cgbase, cgrule1, my_rule_collection.rules[0].num_ids,
            my_rule_collection.rules[0].rule_ids,
            (char *)(my_rule_collection.rules[0].rule_names));

        my_rule_collection.rules[1].num_ids = 4;
        my_rule_collection.rules[1].rule_ids = (int *)malloc(4 * sizeof(int));
        my_rule_collection.rules[1].rule_names = (char_33 *)malloc(4 * sizeof(char_33));
        my_rule_collection.rules[1].types =
            (ElementType_t *)malloc(4 * sizeof(ElementType_t));

        my_rule_collection.rules[1].rule_ids[0] = 187;
        my_rule_collection.rules[1].rule_ids[1] = 186;
        my_rule_collection.rules[1].rule_ids[2] = 123;
        my_rule_collection.rules[1].rule_ids[3] = 181;

        strcpy(my_rule_collection.rules[1].rule_names[0], "pyra");
        strcpy(my_rule_collection.rules[1].rule_names[1], "hexa");
        strcpy(my_rule_collection.rules[1].rule_names[2], "tri");
        strcpy(my_rule_collection.rules[1].rule_names[3], "quad");
        my_rule_collection.rules[1].types[0] = PYRA_5;
        my_rule_collection.rules[1].types[1] = HEXA_8;
        my_rule_collection.rules[1].types[2] = TRI_3;
        my_rule_collection.rules[1].types[3] = QUAD_4;

        cg_rcollection_idtoqualifier_write(
            cgfile, cgbase, cgrule2, my_rule_collection.rules[1].num_ids,
            my_rule_collection.rules[1].rule_ids,
            (char *)(my_rule_collection.rules[1].rule_names));

        my_rule_collection.rules[2].num_ids = 2;
        my_rule_collection.rules[2].rule_ids = (int *)malloc(2 * sizeof(int));
        my_rule_collection.rules[2].rule_names = (char_33 *)malloc(2 * sizeof(char_33));
        my_rule_collection.rules[2].types =
            (ElementType_t *)malloc(2 * sizeof(ElementType_t));

        my_rule_collection.rules[2].rule_ids[0] = 4;
        my_rule_collection.rules[2].rule_ids[1] = 3;

        strcpy(my_rule_collection.rules[2].rule_names[0], "bary_tri");
        strcpy(my_rule_collection.rules[2].rule_names[1], "bary_quad");

        my_rule_collection.rules[2].types[0] = TRI_3;
        my_rule_collection.rules[2].types[1] = QUAD_4;

        cg_rcollection_idtoqualifier_write(
            cgfile, cgbase, cgrule3, my_rule_collection.rules[2].num_ids,
            my_rule_collection.rules[2].rule_ids,
            (char *)(my_rule_collection.rules[2].rule_names));

        // TODO: and check: cg_rcollection_idtoqualifier_read(cgfile, cgbase,
        // cgrule1);
        // TODO: cg_rcollection_idtoqualifier_info(cgfile, cgbase, cgrule1,
        // num_ids)
        int nsections;
        char_33 section_name;
        CGNS_ENUMT(ElementType_t) eltype;
        cgsize_t start, end;
        int nbndry, parent_flag;
        cgbase = 1;
        cgzone = 1;

        cg_nsections(cgfile, cgbase, cgzone, &nsections);
        rule_mapper.num_associates = nsections;
        rule_mapper.associates = (section_element_association *)malloc(
            sizeof(section_element_association) * nsections);
        for (int cgsection = 0; cgsection < nsections; cgsection++) {
            rule_mapper.associates[cgsection].sect_num = -1;
            rule_mapper.associates[cgsection].sect_dim = -1;
        }
        // Create Element Association
        //
        for (int cgsection = 1; cgsection <= nsections; cgsection++) {
            int found = 0;
            cg_section_read(cgfile, cgbase, cgzone, cgsection, section_name, &eltype,
                            &start, &end, &nbndry, &parent_flag);
            if (eltype != CGNS_ENUMV(MIXED)) {
                char *path = NULL;
                int ids;
                int size_ids = 1;
                int elem_dim = 3;
                for (int collection_idx = 0;
                     collection_idx < my_rule_collection.num_rules; collection_idx++) {
                    for (int rule_idx = 0;
                         rule_idx < my_rule_collection.rules[collection_idx].num_ids;
                         rule_idx++) {
                        if (my_rule_collection.rules[collection_idx].types[rule_idx] ==
                            eltype) {
                            ids = my_rule_collection.rules[collection_idx]
                                      .rule_ids[rule_idx];
                            if (collection_idx == 0) {
                                path = "/Base/irc1"; // or simple "irc1" when store in
                                                     // same base ?
                            } else if (collection_idx == 1) {
                                path = "/Base/irc2";
                            } else {
                                path = "/Base/irc3";
                            }
                            found = 1;
                            break;
                        }
                    }
                    if (found) {
                        break;
                    }
                }
                if (found != 1) {
                    // Not found association
                    continue;
                }
                elem_dim = 3;
                if (eltype == TRI_3 || eltype == QUAD_4) {
                    elem_dim = 2;
                }
                //
                cg_goto(cgfile, cgbase, Zone_ts, cgzone, Elements_ts, cgsection, NULL);
                cg_association_write(IntegrationRule_s, path, size_ids, &ids);
                //
                rule_mapper.associates[cgsection - 1].sect_num = cgsection;
                rule_mapper.associates[cgsection - 1].sect_dim = elem_dim;
                rule_mapper.associates[cgsection - 1].start = start;
                rule_mapper.associates[cgsection - 1].end = end;
                rule_mapper.associates[cgsection - 1].path = path;
                rule_mapper.associates[cgsection - 1].size_ids = 1;
                rule_mapper.associates[cgsection - 1].ids = (int *)malloc(sizeof(int));
                rule_mapper.associates[cgsection - 1].ids[0] = ids;
                //
                // get_elem_itg_rule_ids: scan elt_type in collection
                // only one association
                //
            } else if (eltype == CGNS_ENUMV(MIXED)) {
                // path = Only one path !
                // read elements
                cgsize_t *elements = NULL;
                cgsize_t *offsets = NULL;
                cgsize_t nelem = end - start + 1;
                cgsize_t datasize;
                ElementType_t curtype;
                int *ids = NULL;
                char *path = NULL;
                int elem_dim = 3;

                cg_ElementDataSize(cgfile, cgbase, cgzone, cgsection, &datasize);
                elements = (cgsize_t *)malloc(datasize * sizeof(cgsize_t));
                offsets = (cgsize_t *)malloc((nelem + 1) * sizeof(cgsize_t));

                // Allocate Element Association
                rule_mapper.associates[cgsection - 1].start = start;
                rule_mapper.associates[cgsection - 1].end = end;
                rule_mapper.associates[cgsection - 1].size_ids = nelem;
                rule_mapper.associates[cgsection - 1].ids =
                    (int *)malloc((nelem) * sizeof(int));
                //
                ids = rule_mapper.associates[cgsection - 1].ids;
                cg_poly_elements_read(cgfile, cgbase, cgzone, cgsection, elements,
                                      offsets, NULL);
                // Init with -1
                for (cgsize_t idx = 0; idx < nelem; idx++) {
                    ids[idx] = -1;
                }
                elem_dim = 3;
                if (elements[0] == TRI_3 || elements[0] == QUAD_4) {
                    elem_dim = 2;
                }
                rule_mapper.associates[cgsection - 1].sect_num = cgsection;
                rule_mapper.associates[cgsection - 1].sect_dim = elem_dim;
                // all elements should be in same collection
                int collection_idx = 0;
                for (cgsize_t idx = 0; idx < nelem; idx++) {
                    curtype = (ElementType_t)elements[offsets[idx]];
                    for (int rule_idx = 0;
                         rule_idx < my_rule_collection.rules[collection_idx].num_ids;
                         rule_idx++) {
                        if (my_rule_collection.rules[collection_idx].types[rule_idx] ==
                            curtype) {
                            ids[idx] = my_rule_collection.rules[collection_idx]
                                           .rule_ids[rule_idx];
                            found = 1;
                            break;
                        }
                    }
                }
                if (collection_idx == 0) {
                    path = "/Base/irc1"; // or simple "irc1" ?
                } else if (collection_idx == 1) {
                    path = "/Base/irc2";
                } else {
                    path = "/Base/irc3";
                }
                int prop = 0;
                cg_element_association_write(cgfile, cgbase, cgzone, cgsection,
                                             IntegrationRule_s, path, nelem, ids, &prop);
                rule_mapper.associates[cgsection - 1].path = path;
                //
                free(offsets);
                free(elements);
                path = NULL;
            }
        }

        //============================================
        // Create the IntegrationPointOffset
        char_33 zonename;
        cgsize_t size[9];
        cg_zone_read(cgfile, cgbase, cgzone, zonename, size);
        cgsize_t ncells = size[1];
        // get order of section
        int num_valid = 0;
        int *index_sect = (int *)malloc(sizeof(int) * rule_mapper.num_associates);
        for (int k = 0; k < rule_mapper.num_associates; k++) {
            index_sect[k] = -1;
        }
        for (int k = 0; k < rule_mapper.num_associates; k++) {
            if (rule_mapper.associates[k].sect_num < 0) {
                continue;
            }
            if (rule_mapper.associates[k].sect_dim != 3) {
                continue;
            }
            index_sect[num_valid] = k;
            num_valid += 1;
        }
        // Sorting section by range
        for (int i = 0; i < num_valid; i++) {
            for (int j = i + 1; j < num_valid; j++) {
                if (rule_mapper.associates[index_sect[j]].start <
                    rule_mapper.associates[index_sect[i]].start) {
                    int temp = index_sect[j];
                    index_sect[j] = index_sect[i];
                    index_sect[i] = temp;
                }
            }
        }

        itg_value = (cgsize_t *)malloc((size_t)(sizeof(cgsize_t) * (ncells + 1)));
        for (int k = 0; k < ncells + 1; k++) {
            itg_value[k] = 0;
        }

        // Go through valid 3d section to determine num of points at each cell.
        start = 1;
        for (int sect_pos = 0; sect_pos < num_valid; sect_pos++) {
            int idx = index_sect[sect_pos];
            int size =
                rule_mapper.associates[idx].end - rule_mapper.associates[idx].start + 1;
            int collection_id = 0;
            if (strcmp(rule_mapper.associates[idx].path, "/Base/irc1") == 0) {
                collection_id = 0;
            } else if (strcmp(rule_mapper.associates[idx].path, "/Base/irc2") == 0) {
                collection_id = 1;
            } else {
                collection_id = 2;
            }

            if (rule_mapper.associates[idx].size_ids != size &&
                rule_mapper.associates[idx].size_ids == 1) {
                int id = rule_mapper.associates[idx].ids[0];
                int found = -1;
                int num_itg_pts;
                for (int k = 0; k < my_rule_collection.rules[collection_id].num_ids;
                     k++) {
                    if (my_rule_collection.rules[collection_id].rule_ids[k] == id) {
                        found = k;
                        break;
                    }
                }
                if (found == -1) {
                    num_itg_pts = 0;
                } else if (strcmp(
                               my_rule_collection.rules[collection_id].rule_names[found],
                               "tetra") == 0) {
                    num_itg_pts = 4;
                } else if (strcmp(
                               my_rule_collection.rules[collection_id].rule_names[found],
                               "pyra") == 0) {
                    num_itg_pts = 1;
                } else if (strcmp(
                               my_rule_collection.rules[collection_id].rule_names[found],
                               "hexa") == 0) {
                    num_itg_pts = 8;
                } else if (strcmp(
                               my_rule_collection.rules[collection_id].rule_names[found],
                               "penta") == 0) {
                    num_itg_pts = 2;
                } else if (strcmp(
                               my_rule_collection.rules[collection_id].rule_names[found],
                               "tri") == 0) {
                    num_itg_pts = 1;
                } else if (strcmp(
                               my_rule_collection.rules[collection_id].rule_names[found],
                               "quad") == 0) {
                    num_itg_pts = 4;
                } else if (strcmp(
                               my_rule_collection.rules[collection_id].rule_names[found],
                               "barytri") == 0) {
                    num_itg_pts = 1;
                } else if (strcmp(
                               my_rule_collection.rules[collection_id].rule_names[found],
                               "baryquad") == 0) {
                    num_itg_pts = 4;
                }

                for (int k = 0; k < size; k++) {
                    itg_value[start + k] = (cgsize_t)num_itg_pts;
                }
                start += size;
            } else {
                for (int idx_elem = 0; idx_elem < rule_mapper.associates[idx].size_ids;
                     idx_elem++) {
                    int id = rule_mapper.associates[idx].ids[idx_elem];
                    int found = -1;
                    int num_itg_pts;
                    for (int k = 0; k < my_rule_collection.rules[collection_id].num_ids;
                         k++) {
                        if (my_rule_collection.rules[collection_id].rule_ids[k] == id) {
                            found = k;
                            break;
                        }
                    }
                    if (found == -1) {
                        num_itg_pts = 0;
                    } else if (strcmp(my_rule_collection.rules[collection_id]
                                          .rule_names[found],
                                      "tetra") == 0) {
                        num_itg_pts = 4;
                    } else if (strcmp(my_rule_collection.rules[collection_id]
                                          .rule_names[found],
                                      "pyra") == 0) {
                        num_itg_pts = 1;
                    } else if (strcmp(my_rule_collection.rules[collection_id]
                                          .rule_names[found],
                                      "hexa") == 0) {
                        num_itg_pts = 8;
                    } else if (strcmp(my_rule_collection.rules[collection_id]
                                          .rule_names[found],
                                      "penta") == 0) {
                        num_itg_pts = 2;
                    } else if (strcmp(my_rule_collection.rules[collection_id]
                                          .rule_names[found],
                                      "tri") == 0) {
                        num_itg_pts = 1;
                    } else if (strcmp(my_rule_collection.rules[collection_id]
                                          .rule_names[found],
                                      "quad") == 0) {
                        num_itg_pts = 4;
                    } else if (strcmp(my_rule_collection.rules[collection_id]
                                          .rule_names[found],
                                      "barytri") == 0) {
                        num_itg_pts = 1;
                    } else if (strcmp(my_rule_collection.rules[collection_id]
                                          .rule_names[found],
                                      "baryquad") == 0) {
                        num_itg_pts = 4;
                    }

                    itg_value[start + idx_elem] = (cgsize_t)num_itg_pts;
                }
                start += rule_mapper.associates[idx].size_ids;
            }
        }

        for (int k = 0; k < ncells; k++) {
            itg_value[k + 1] += itg_value[k];
        }
        // offset created

        // New Solution
        cg_sol_write(cgfile, cgbase, cgzone, "solutionIP", CGNS_ENUMV(IntegrationPoint),
                     &S);
        // Create the optional helper offset
#if CG_SIZEOF_SIZE == 64
        CGNS_ENUMT(DataType_t) id_type = CGNS_ENUMV(LongInteger);
#else
        CGNS_ENUMT(DataType_t) id_type = CGNS_ENUMV(Integer);
#endif
        cg_sol_itg_offset_write(cgfile, cgbase, cgzone, S, id_type, ncells + 1,
                                itg_value);

        // ---
        // Create a fake itg_rule for testing since the offset is enough and
        // the association is already defined at the element section level.
        char *path = "Base/fake_integration_node";
        int fakeids = 1;
        cg_sol_itg_rule_write(cgfile, cgbase, cgzone, S, path, 1, &fakeids);
        // Then remove it
        cg_goto(cgfile, cgbase, "Zone_t", cgzone, "FlowSolution_t", S, "end");
        cg_delete_node(IntegrationRule_s);

        // TODO other definition::
        // cg_goto(cgfile, cgbase, "Zone_t", cgzone, "FlowSolution_t", S,"end");
        // cg_write_offset(IntegrationStartOffset_s, ncells+1, id_type, itg_value);
        free(itg_value);
    }
    // Close and reopen to be sure that file is flushed.
    if (cg_close(cgfile)) error_exit("cg_close");

    if (cg_open(outfile, CG_MODE_MODIFY, &cgfile)) error_exit("cg_open");
    if (cg_nbases(cgfile, &nbases)) error_exit("cg_nbases");

    if (nbases > 0) {
        cgbase = 1;
        char_33 zonename;
        cgsize_t size[9];
        cg_zone_read(cgfile, cgbase, cgzone, zonename, size);
        cgsize_t ncells = size[1];
        itg_value = (cgsize_t *)malloc((size_t)(sizeof(cgsize_t) * (ncells + 1)));
        //
        // Create data at solution integration point for writing
        //
        //
        double *data_cx = (double *)malloc(sizeof(double) * itg_value[ncells]);
        double *data_cy = (double *)malloc(sizeof(double) * itg_value[ncells]);
        double *data_cz = (double *)malloc(sizeof(double) * itg_value[ncells]);
        int idx_ip = 0;
        //

        for (int sect = 0; sect < num_valid; sect++) {
            int idx = index_sect[sect];
            int size =
                rule_mapper.associates[idx].end - rule_mapper.associates[idx].start + 1;
            int collection_id = 0;
            if (strcmp(rule_mapper.associates[idx].path, "/Base/irc1") == 0) {
                collection_id = 0;
                cg_gopath(cgfile, "/Base/irc1");
            } else if (strcmp(rule_mapper.associates[idx].path, "/Base/irc2") == 0) {
                collection_id = 1;
                cg_gopath(cgfile, "/Base/irc2");
            } else {
                collection_id = 2;
                cg_gopath(cgfile, "/Base/irc3");
            }

            if (rule_mapper.associates[idx].size_ids != size &&
                rule_mapper.associates[idx].size_ids == 1) {
                int id = rule_mapper.associates[idx].ids[0];
                int found = -1;
                for (int k = 0; k < my_rule_collection.rules[collection_id].num_ids;
                     k++) {
                    if (my_rule_collection.rules[collection_id].rule_ids[k] == id) {
                        found = k;
                        break;
                    }
                }
                if (found < 0) continue;

                cg_gorel(cgfile,
                         my_rule_collection.rules[collection_id].rule_names[found]);
                CGNS_ENUMT(ElementSpace_t) refspace;
                double *cur_points_param, *cur_weights;
                int num_of_points, param_dim;
                DataType_t datatype;
                ElementType_t elem_type;
                char *integration_name_definition = NULL;
                IntegrationName_t *integration_name_struct =
                    (IntegrationName_t *)malloc(sizeof(IntegrationName_t));
                // Init structure
                integration_name_struct->CombineStatus = CGNS_ENUMV(CombineNo);
                integration_name_struct->IntegrationNameParam1 = NULL;
                integration_name_struct->IntegrationNameParam2 = NULL;
                integration_name_struct->IntegrationNameParam3 = NULL;
                integration_name_struct->ParametricDimension = 0;
                //
                char_33 rulename;
                cg_itg_rule_info(cgfile, cgbase, 1, collection_id + 1, rulename,
                                 &elem_type, &refspace, &num_of_points, &param_dim,
                                 &datatype);

                if (refspace == Parametric) {
                    cur_points_param =
                        (double *)malloc((num_of_points * param_dim) * sizeof(double));
                    cur_weights = (double *)malloc((num_of_points) * sizeof(double));
                    cg_integration_name_read(&integration_name_definition);
                    cg_itg_rule_read_data(cgfile, cgbase, 1, collection_id + 1,
                                          RealDouble, cur_weights, cur_points_param);
                    for (int elem = 0; elem < size; elem++) {
                        for (int small_ip = 0; small_ip < num_of_points; small_ip++) {
                            data_cx[idx_ip] = cur_points_param[small_ip + 0];
                            data_cy[idx_ip] =
                                cur_points_param[small_ip + 1 * num_of_points];
                            data_cz[idx_ip] =
                                cur_points_param[small_ip + 2 * num_of_points];
                            idx_ip += 1;
                        }
                    }
                    free(cur_weights);
                    free(cur_points_param);
                }
                free(integration_name_struct);
                cg_gorel(cgfile, "..");
            } else {
                for (int ii = 0; ii < rule_mapper.associates[idx].size_ids; ii++) {
                    int id = rule_mapper.associates[idx].ids[ii];
                    int found = -1;
                    if (strcmp(rule_mapper.associates[idx].path, "/Base/irc1") == 0) {
                        cg_gopath(cgfile, "/Base/irc1");
                    } else if (strcmp(rule_mapper.associates[idx].path, "/Base/irc2") ==
                               0) {
                        cg_gopath(cgfile, "/Base/irc2");
                    } else {
                        cg_gopath(cgfile, "/Base/irc3");
                    }

                    for (int k = 0; k < my_rule_collection.rules[collection_id].num_ids;
                         k++) {
                        if (my_rule_collection.rules[collection_id].rule_ids[k] == id) {
                            found = k;
                            break;
                        }
                    }
                    if (found < 0) continue;
                    cg_gorel(cgfile,
                             my_rule_collection.rules[collection_id].rule_names[found]);
                    CGNS_ENUMT(ElementSpace_t) refspace;
                    cg_reference_space_read(&refspace);
                    int num_of_points, param_dim;
                    ElementType_t elem_type;
                    DataType_t datatype;
                    double *cur_points_param, *cur_weights;
                    char *integration_name_definition = NULL;
                    IntegrationName_t *integration_name_struct =
                        (IntegrationName_t *)malloc(sizeof(IntegrationName_t));
                    // Init structure
                    integration_name_struct->CombineStatus = CGNS_ENUMV(CombineNo);
                    integration_name_struct->IntegrationNameParam1 = NULL;
                    integration_name_struct->IntegrationNameParam2 = NULL;
                    integration_name_struct->IntegrationNameParam3 = NULL;
                    integration_name_struct->ParametricDimension = 0;
                    //
                    if (refspace == Parametric) {
                        char_33 rulename;
                        cg_integration_info(rulename, &elem_type, &refspace,
                                            &num_of_points, &param_dim, &datatype);
                        cur_points_param = (double *)malloc((num_of_points * param_dim) *
                                                            sizeof(double));
                        cur_weights = (double *)malloc((num_of_points) * sizeof(double));
                        integration_name_struct->ParametricDimension = param_dim;
                        cg_integration_name_struct_read(integration_name_struct);
                        cg_integration_name_read(&integration_name_definition);
                        cg_itg_rule_read_data(cgfile, cgbase, 1, collection_id + 1,
                                              RealDouble, cur_weights, cur_points_param);
                        for (int elem = 0; elem < size; elem++) {
                            for (int small_ip = 0; small_ip < num_of_points; small_ip++) {
                                data_cx[idx_ip] = cur_points_param[small_ip + 0];
                                data_cy[idx_ip] =
                                    cur_points_param[small_ip + 1 * num_of_points];
                                data_cz[idx_ip] =
                                    cur_points_param[small_ip + 2 * num_of_points];
                                idx_ip += 1;
                            }
                        }
                        free(cur_weights);
                        free(cur_points_param);
                    }
                    free(integration_name_struct);
                    cg_gorel(cgfile, "..");
                }
            }
            int fld;
            cg_field_write(cgfile, cgbase, cgzone, S, CGNS_ENUMV(RealDouble),
                           ParametricR_s, data_cx, &fld);
            cg_field_write(cgfile, cgbase, cgzone, S, CGNS_ENUMV(RealDouble),
                           ParametricS_s, data_cy, &fld);
            cg_field_write(cgfile, cgbase, cgzone, S, CGNS_ENUMV(RealDouble),
                           ParametricT_s, data_cz, &fld);
        }
        //
        free(data_cx);
        free(data_cy);
        free(data_cz);
        free(itg_value);
        if (cg_close(cgfile)) error_exit("cg_close");

        return 0;
    }
}
