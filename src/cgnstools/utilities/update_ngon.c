/*
 * update_ngon.c - convert old NGON_n, NFACE_n structure to recent CPEX structure
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <math.h>
#ifdef _WIN32
#include <io.h>
#define ACCESS _access
#define unlink _unlink
#else
#include <unistd.h>
#define ACCESS access
#endif

#include "cgns_io.h"
#include "getargs.h"
#include "cgnslib.h"
#include "cgnsutil.h"

#ifndef CG_MODE_READ
# define CG_MODE_READ   MODE_READ
# define CG_MODE_WRITE  MODE_WRITE
# define CG_MODE_MODIFY MODE_MODIFY
#endif

static char *usgmsg[] = {
    "usage  : update_ngon CGNSfile [newCGNSfile]",
    NULL
};


int convert_elements(int cgio_num, double elem_id)
{
    /* This function convert elements of a section to new format
     * Hanfle NGON_n, NFACE_n  and MIXED
     *
     */
    cgsize_t size = 1;
    cgsize_t dim_vals[12];
    int ndim;
    int n;
    int *data = NULL;

    if (cgio_get_dimensions(cgio_num, elem_id, &ndim, dim_vals) != CG_OK) {
        cgio_error_exit("cgio_get_dimensions");
        return 1;
    }

    /* allocate data */
    for (n = 0; n < ndim; n++) {
        size *= dim_vals[n];
    }
    if (size <= 0) {
        return 1;
    }
    data = malloc((size_t) size*sizeof(int));
    /* read elem type */
    if (cgio_read_all_data_type(cgio_num, elem_id, "I4", data) != CG_OK) {
        free(data);
        return 1;
    }
    int elem_type = data[0];
    free(data);
    /* Handle NGON_n and NFAce_n in the same way but MIXED is a bit different */
    if (elem_type == CGNS_ENUMV(NGON_n) ||
        elem_type == CGNS_ENUMV(NFACE_n)) {
        double connect_id;
        double offset_id;
        const char* connectivity_path = "ElementConnectivity";
        const char* offset_name = "ElementStartOffset";
        const char* offset_label = "DataArray_t";
        char datatype[3];
        cgsize_t elem_size;

        cgio_get_node_id(cgio_num, elem_id, connectivity_path, &connect_id);
        cgio_get_data_type(cgio_num, connect_id, datatype);

        if (cgio_get_dimensions(cgio_num, connect_id, &ndim, dim_vals) != CG_OK) {
            cgio_error_exit("cgio_get_dimensions");
            return 1;
        }
        if (ndim != 1) {
            return 1;
        }
        elem_size = dim_vals[0];
        /* I4 vs I8 */
        if (strcmp(datatype, "I4") == 0) {
            int * connectivity = NULL;
            int *new_connectivity = NULL;
            int * new_offset = NULL;
            cgsize_t num_elem = 0;
            cgsize_t idx = 0;
            cgsize_t cur_idx = 0;
            cgsize_t cur_elem = 0;
            cgsize_t local_idx;

            connectivity = malloc((size_t)sizeof(int)*elem_size);
            cgio_read_all_data_type(cgio_num, connect_id, datatype, (void *)connectivity);
            /* first count number of elements */
            idx = 0;
            while (idx < elem_size) {
                num_elem += 1;
                idx += connectivity[idx] + 1;
            }

            new_connectivity = malloc((size_t)(elem_size - num_elem)*sizeof(int));
            new_offset = malloc((size_t)(num_elem + 1)*sizeof(int));

            idx = 0;
            cur_elem = 0;
            cur_idx = 0;
            new_offset[0] = 0;

            for (idx = 0; idx < num_elem; ++idx) {
                int nparts_in_elem = connectivity[cur_elem];
                new_offset[idx + 1] = new_offset[idx] + nparts_in_elem;
                for (local_idx = 0; local_idx < nparts_in_elem; local_idx++) {
                    new_connectivity[cur_idx] = connectivity[cur_elem + local_idx + 1];
                    cur_idx++;
                }
                cur_elem += nparts_in_elem + 1;
            }
            free(connectivity);
            /* Now update CGNS nodes with new connectivity and new offset */
            dim_vals[0] = new_offset[num_elem];
            cgio_set_dimensions(cgio_num, connect_id, "I4", 1, dim_vals);
            cgio_write_all_data(cgio_num, connect_id, new_connectivity);

            cgio_create_node(cgio_num, elem_id, offset_name, &offset_id);
            cgio_set_label(cgio_num, offset_id, offset_label);
            dim_vals[0] = num_elem + 1;
            cgio_set_dimensions(cgio_num, offset_id, "I4", 1, dim_vals);
            cgio_write_all_data(cgio_num, offset_id, new_offset);

            free(new_offset);
            free(new_connectivity);
        }
        else if (strcmp(datatype, "I8") == 0) {
            cglong_t * connectivity = NULL;
            cglong_t * new_connectivity = NULL;
            cglong_t * new_offset = NULL;
            cgsize_t num_elem = 0;
            cgsize_t idx = 0;
            cgsize_t cur_idx = 0;
            cgsize_t cur_elem = 0;
            cgsize_t local_idx;
            connectivity = malloc((size_t)sizeof(cglong_t)*elem_size);
            cgio_read_all_data_type(cgio_num, connect_id, datatype, (void *)connectivity);
            /* first count number of elements */
            while (idx < elem_size) {
                idx += connectivity[idx] +1;
                num_elem += 1;
            }

            new_connectivity = malloc((size_t)(elem_size - num_elem) * sizeof(cglong_t));
            new_offset = malloc((size_t)(num_elem + 1) * sizeof(cglong_t));

            idx = 0;
            cur_elem = 0;
            cur_idx = 0;
            new_offset[0] = 0;

            for (idx = 0; idx < num_elem; ++idx) {
                int nparts_in_elem = connectivity[cur_elem];

                new_offset[idx + 1] = new_offset[idx] + nparts_in_elem;

                for (local_idx = 0; local_idx < nparts_in_elem; local_idx++)
                {
                    new_connectivity[cur_idx] = connectivity[cur_elem + local_idx + 1];
                    cur_idx++;
                }
                cur_elem += nparts_in_elem + 1;
            }
            free(connectivity);
            /* Now update CGNS nodes with new connectivity and new offset */
            dim_vals[0] = new_offset[num_elem];
            cgio_set_dimensions(cgio_num, connect_id, "I8", 1, dim_vals);
            cgio_write_all_data(cgio_num, connect_id, new_connectivity);

            cgio_create_node(cgio_num, elem_id, offset_name, &offset_id);
            cgio_set_label(cgio_num, offset_id, offset_label);
            dim_vals[0] = num_elem + 1;
            cgio_set_dimensions(cgio_num, offset_id, "I8", 1, dim_vals);
            cgio_write_all_data(cgio_num, offset_id, new_offset);

            free(new_offset);
            free(new_connectivity);
        }
        else {
            return 1;
        }
    }
    /* Handle MIXED connectivity */
    if (elem_type == CGNS_ENUMV(MIXED)) {
        /* Cannot do the same thing as in NGON_n NFACE_n
           since we would need one more array to store celltypes. Do we want it ?
           If we want to mix NGON_n NFACE_n and other elements inside MIXED celltypes array will be necessary
           For now keep ElementConnectivity as it is and add offset
           to allow for parallel reading. So no mix off ngon with elements. */
        const char* connectivity_path = "ElementConnectivity";
        const char* offset_name = "ElementStartOffset";
        const char* offset_label = "DataArray_t";
        double connect_id;
        double offset_id;
        char datatype[3];
        cgsize_t elem_size;

        cgio_get_node_id(cgio_num, elem_id, connectivity_path, &connect_id);
        cgio_get_data_type(cgio_num, connect_id, datatype);

        if (cgio_get_dimensions(cgio_num, connect_id, &ndim, dim_vals) != CG_OK) {
            cgio_error_exit("cgio_get_dimensions");
            return 1;
        }
        if (ndim != 1) {
            return 1;
        }
        elem_size = dim_vals[0];
        /* I4 vs I8 */
        if (strcmp(datatype, "I4") == 0) {
            int * connectivity = NULL;
            int * new_offset = NULL;
            int nparts_in_elem;
            cgsize_t num_elem = 0;
            cgsize_t idx = 0;
            cgsize_t cur_idx = 0;
            cgsize_t cur_elem = 0;

            connectivity = malloc((size_t)sizeof(int)*elem_size);
            cgio_read_all_data_type(cgio_num, connect_id, datatype, (void *)connectivity);
            /* first count number of elements */
            while (idx < elem_size) {
                int celltype = connectivity[idx];
                int npe;
                cg_npe(celltype, &npe);
                idx += npe + 1;
                num_elem += 1;
            }

            new_offset = malloc((size_t)(num_elem + 1) * sizeof(int));

            idx = 0;
            cur_elem = 0;
            cur_idx = 0;
            new_offset[0] = 0;

            for (idx = 0; idx < num_elem; ++idx) {
                cg_npe(connectivity[cur_elem], &nparts_in_elem);
                new_offset[idx + 1] = new_offset[idx] + nparts_in_elem + 1;
                cur_elem += nparts_in_elem + 1;
            }
            /* Now update CGNS node with new offset array */
            cgio_create_node(cgio_num, elem_id, offset_name, &offset_id);
            cgio_set_label(cgio_num, offset_id, offset_label);
            dim_vals[0] = num_elem + 1;
            cgio_set_dimensions(cgio_num, offset_id, "I4", 1, dim_vals);
            cgio_write_all_data(cgio_num, offset_id, new_offset);

            free(new_offset);
            free(connectivity);
        }
        else if (strcmp(datatype, "I8") == 0) {
            cglong_t * connectivity = NULL;
            cglong_t * new_offset = NULL;
            cgsize_t num_elem = 0;
            cgsize_t idx = 0;
            cgsize_t cur_idx = 0;
            cgsize_t cur_elem = 0;
            int nparts_in_elem;

            connectivity = malloc((size_t)sizeof(cglong_t)*elem_size);
            cgio_read_all_data_type(cgio_num, connect_id, datatype, (void *)connectivity);
            /* first count number of elements */
            while (idx < elem_size) {
                cglong_t celltype = connectivity[idx];
                int npe;
                cg_npe(celltype, &npe);
                idx += npe + 1;
                num_elem += 1;
            }

            new_offset = malloc((size_t)(num_elem + 1) * sizeof(cglong_t));

            idx = 0;
            cur_elem = 0;
            cur_idx = 0;
            new_offset[0] = 0;

            for (idx = 0; idx < num_elem; ++idx) {
                cg_npe(connectivity[cur_elem], &nparts_in_elem);
                new_offset[idx + 1] = new_offset[idx] + nparts_in_elem + 1;
                cur_elem += nparts_in_elem + 1;
            }
            /* Now update CGNS node with new offset array */
            cgio_create_node(cgio_num, elem_id, offset_name, &offset_id);
            cgio_set_label(cgio_num, offset_id, offset_label);
            dim_vals[0] = num_elem + 1;
            cgio_set_dimensions(cgio_num, offset_id, "I8", 1, dim_vals);
            cgio_write_all_data(cgio_num, offset_id, new_offset);

            free(new_offset);
            free(connectivity);
        }
        else {
            return 1;
        }
    }
    return 0;
}


int convert_zone(int cgio_num, double zone_id)
{
    /* Test unstructured */
    CGNS_ENUMT(ZoneType_t) zt = CGNS_ENUMV(ZoneTypeNull);

    zt = CGNS_ENUMV(Structured);
    int len;
    int nchildren = 0;
    int child;
    char nodelabel[CGIO_MAX_NAME_LENGTH + 1];
    double * node_ids = NULL;

    cgio_number_children(cgio_num, zone_id, &nchildren);
    node_ids = malloc(nchildren * sizeof(double));
    cgio_children_ids(cgio_num, zone_id, 1, nchildren, &len, node_ids);

    if (len != nchildren) {
        free(node_ids);
        char msg[512];
        sprintf(msg, "Mismatch in number of children and child IDs read\n");
        FATAL(NULL, msg);
        return 1;
    }

    for (child = 0; child < nchildren; child++) {
        double childid = node_ids[child];
        if (cgio_get_label(cgio_num, childid, nodelabel) != CG_OK) {
            free(node_ids);
            return 1;
        }
        if (strcmp(nodelabel, "ZoneType_t") == 0) {
            cgsize_t size = 1;
            cgsize_t dim_vals[12];
            int ndim;
            int n;
            char *data;

            if (cgio_get_dimensions(cgio_num, childid, &ndim, dim_vals) != CG_OK) {
                free(node_ids);
                cgio_error_exit("cgio_get_dimensions");
                return 1;
            }
            /* allocate data */
            for (n = 0; n < ndim; n++) {
                size *= dim_vals[n];
            }
            if (size <= 0) {
                free(node_ids);
                return 1;
            }
            data = malloc((size_t)sizeof(char)*(size + 1));
            /* read data zonetype */
            if (cgio_read_all_data_type(cgio_num, childid, "C1", (void*)data) != CG_OK) {
                data[0] = '\0';
            }
            else {
                data[size] = '\0';
            }
            /* define CGNS zone type */
            if (strcmp(data, "Structured") == 0) {
                zt = CGNS_ENUMV(Structured);
            }
            else if (strcmp(data, "Unstructured") == 0) {
                zt = CGNS_ENUMV(Unstructured);
            }
            else if (strcmp(data, "Null") == 0) {
                zt = CGNS_ENUMV(ZoneTypeNull);
            }
            else if (strcmp(data, "UserDefined") == 0) {
                zt = CGNS_ENUMV(ZoneTypeUserDefined);
            }
            free(data);
            break;
        }
    }
    /* handle Unstructured zone only ! */
    if (zt == CGNS_ENUMV(Unstructured)) {
        /* process each section */
        for (child = 0; child < nchildren; child++) {
            double childid = node_ids[child];
            if (cgio_get_label(cgio_num, childid, nodelabel) != CG_OK) {
                free(node_ids);
                return 1;
            }
            /* Convert section elements */
            if (strcmp(nodelabel, "Elements_t") == 0) {
                convert_elements(cgio_num, childid);
            }
        }
    } /* Unstructured zone handling done */
    /* Release all nodes */
    for (child = 0; child < nchildren; child++) {
        double childid = node_ids[child];
        cgio_release_id(cgio_num, childid);
    }
    return 0;
}

int convert_base (int cgio_num, double baseid)
{
    double * node_ids = NULL;
    int len = 0;
    int nchildren = 0;
    int child;
    char label[CGIO_MAX_NAME_LENGTH + 1];

    cgio_number_children(cgio_num, baseid, &nchildren);
    node_ids = malloc(nchildren * sizeof(double));
    cgio_children_ids(cgio_num, baseid, 1, nchildren, &len, node_ids);
    if (len != nchildren) {
        free(node_ids);
        char msg[512];
        sprintf(msg, "Mismatch in number of children and child IDs read\n");
        FATAL(NULL, msg);
        return 1;
    }
    for (child = 0; child < nchildren; child++) {
        double child_id = node_ids[child];
        if (cgio_get_label(cgio_num, child_id, label) != CG_OK) {
            free(node_ids);
            return 1;
        }
        if (strcmp(label, "Zone_t") == 0) {
            /* Try to convert inside zone */
            convert_zone(cgio_num, child_id);
        }
        else {
            cgio_release_id(cgio_num, child_id);
        }
    }
    return 0;

}

int is_file_old_version(char *inpfile)
{
    double root_id;
    double node_id;
    float cgns_version;
    int inpcg;
    int inptype;
    char version[CGIO_MAX_NAME_LENGTH+1];
    char created[CGIO_MAX_NAME_LENGTH+1];
    char modified[CGIO_MAX_NAME_LENGTH+1];
    int status = 1;

    if (cgio_open_file(inpfile, CGIO_MODE_READ, CGIO_FILE_NONE, &inpcg)){
        cgio_error_exit("cgio_open_file");
    }

    if (cgio_get_root_id (inpcg, &root_id)) {
        cgio_error_exit ("cgio_get_root_id");
    }
    if (cgio_get_file_type(inpcg, &inptype)) {
        cgio_error_exit("cgio_get_file_type");
    }

    if (cgio_file_version (inpcg, version, created, modified)) {
            cgio_error_exit ("cgio_file_version");
    }

    if (0 == cgio_get_node_id (inpcg, root_id,
              "CGNSLibraryVersion",&node_id) &&
        0 == cgio_read_all_data_type(inpcg, node_id, "R4", &cgns_version)) {
        printf ("CGNS version  : %4.2f\n", cgns_version);
        if (cgns_version < 4.0) {
            status = 1;
        }
        else {
            status = 0;
        }
    }
    else {
       printf ("CGNS version  : not defined\n");
       status = 1;
    }

    if (cgio_close_file (inpcg)) {
         cgio_error_exit ("cgio_close_file");
    }

    return status;
}


void update_cgns_version(int cgio_num, double root_id)
{
    double node_id;
    float cgns_version = 4.00;
    cgsize_t dim_vals[12];

    if (0 == cgio_get_node_id (cgio_num, root_id, "CGNSLibraryVersion", &node_id)) {
        cgio_write_all_data(cgio_num, node_id, &cgns_version);
    }
    else {
        cgio_create_node(cgio_num, root_id, "CGNSLibraryVersion", &node_id);
        cgio_set_label(cgio_num, node_id, "CGNSLibraryVersion_t");
        dim_vals[0] = 1;
        cgio_set_dimensions(cgio_num, node_id, "R4", 1, dim_vals);
        cgio_write_all_data(cgio_num, node_id, &cgns_version);
    }
}


int main (int argc, char *argv[])
{
    double root_id;
    double *child_ids = NULL;

    int nchildren, len, child;
    int inpcg = 0;
    int inptype;
    int argind = 1;

    char *inpfile, *outfile;
    char tempfile[1024];
    char label[CGIO_MAX_NAME_LENGTH + 1];

    if (argc < 2) {
        print_usage(usgmsg, NULL);
    }

    inpfile = argv[argind];
    if (argind + 1 < argc) argind++;
    outfile = argv[argind];

    sprintf(tempfile, "%s.temp", outfile);
    unlink(tempfile);

    if (!file_exists(inpfile)) {
        FATAL(NULL, "CGNSfile does not exist or is not a file");
    }

    /* Test file version */
    if (is_file_old_version(inpfile) != 1)
    {
        printf("convert aborted\n");
        return 0;
    }

    /* create a working copy */
    printf("creating a working copy of %s\n", inpfile);
    copy_file(inpfile, tempfile);

    /* read CGNS file */
    printf("reading CGNS file from %s\n", tempfile);
    if (cgio_open_file(tempfile, CGIO_MODE_MODIFY, CGIO_FILE_NONE, &inpcg) != CG_OK){
        cgio_error_exit("cgio_open_file");
    }
    if (cgio_get_file_type(inpcg, &inptype) != CG_OK){
        cgio_error_exit("cgio_get_file_type");
    }

    /* Start doing stuff */
    cgio_get_root_id(inpcg, &root_id);
    cgio_number_children(inpcg, root_id, &nchildren);

    child_ids = malloc(nchildren * sizeof(double));

    if (cgio_children_ids(inpcg, root_id, 1, nchildren, &len, child_ids) != CG_OK) {
        exit(1);
    }
    if (len != nchildren) {
        free(child_ids);
        char msg[512];
        sprintf(msg, "Mismatch in number of children and child IDs read\n");
        FATAL(NULL, msg);
        exit(1);
    }

    for (child = 0; child < nchildren; child++) {
        double child_id = child_ids[child];
        cgio_get_label(inpcg, child_id, label);
        if (strcmp(label, "CGNSBase_t") == 0) {
            /* Try to convert inside a base */
            convert_base(inpcg, child_id);
        }
        else {
            cgio_release_id(inpcg, child_id);
        }
    }
    /* Update file version */
    update_cgns_version(inpcg, root_id);

    /* We are done */
    if (cgio_close_file(inpcg) != CG_OK){
        cgio_error_exit("cgio_close_file");
    }

    /* Transfer temporary file */
    printf("renaming %s to %s\n", tempfile, outfile);
    unlink(outfile);

    if (rename(tempfile, outfile)) {
        char msg[512];
        sprintf(msg, "rename %s -> %s failed", tempfile, outfile);
        FATAL(NULL, msg);
        exit(1);
    }

    return 0;
}
