/*-------------------------------------------------------------------------
This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from
the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not
   be misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.
-------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "fortran_macros.h"
#include "cgns_io.h"
#ifdef MEM_DEBUG
#include "cg_malloc.h"
#endif

#if defined(_WIN32) && defined(BUILD_DLL)
# define CGIODLL _declspec(dllexport)
#else
# define CGIODLL
#endif

/*=========================================================
 * local string conversions
 *=========================================================*/

static int to_c_string(char *f_str, int f_len, char *c_str, int c_len)
{
    int i, iend;

    for (iend = f_len-1; iend >= 0; iend--) {
        if (f_str[iend] != ' ') break;
    }
    if (iend >= c_len) iend = c_len - 1;

    for (i = 0; i <= iend; i++)
        c_str[i] = f_str[i];
    c_str[i] = 0;
    return i;
}

/*---------------------------------------------------------*/

static int to_f_string(char *c_str, char *f_str, int f_len)
{
    int i, c_len;

    c_len = strlen(c_str);
    if (c_len > f_len) c_len = f_len;

    for (i = 0; i < c_len; i++)
        f_str[i] = c_str[i];
    while (i < f_len)
        f_str[i++] = ' ';
    return f_len;
}

/*---------------------------------------------------------*/

static char *new_c_string (char *str, int len, int *ier)
{
    char *c_str;

    if (len < 1 || str == NULL) {
        *ier = CGIO_ERR_NULL_STRING;
        return NULL;
    }
    c_str = (char *) malloc (len + 1);
    if (c_str == NULL) {
        *ier = CGIO_ERR_MALLOC;
        return NULL;
    }
    to_c_string (str, len, c_str, len);
    if (strlen(c_str) < 1) {
        free (c_str);
        *ier = CGIO_ERR_NULL_STRING;
        return NULL;
    }
    *ier = 0;
    return c_str;
}

/*=========================================================
 * paths for searching for linked-to files
 *=========================================================*/

CGIODLL void FMNAME(cgio_path_add_f, CGIO_PATH_ADD_F) (
    int *ier, STR_PSTR(path) STR_PLEN(path))
{
    char *c_path = new_c_string(STR_PTR(path), STR_LEN(path), ier);

    if (*ier == 0) {
        *ier = cgio_path_add(c_path);
        free(c_path);
    }
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_path_delete_f, CGIO_PATH_DELETE_F) (
    int *ier, STR_PSTR(path) STR_PLEN(path))
{
    char *c_path = new_c_string(STR_PTR(path), STR_LEN(path), ier);

    if (*ier == CGIO_ERR_MALLOC) return;
    *ier = cgio_path_delete(c_path);
    if (c_path) free(c_path);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_find_file_f, CGIO_FIND_FILE_F) (
    int *ier, STR_PSTR(filename), int *file_type, STR_PSTR(pathname)
    STR_PLEN(filename) STR_PLEN(pathname))
{
    char *c_name, *c_path;

    c_name = new_c_string(STR_PTR(filename), STR_LEN(filename), ier);
    if (*ier) return;
    c_path = new_c_string(STR_PTR(pathname), STR_LEN(pathname), ier);
    if (*ier) {
        free(c_name);
        return;
    }
    *ier = cgio_find_file(c_name, *file_type, STR_LEN(pathname), c_path);
    if (*ier == 0)
        to_f_string(c_path, STR_PTR(pathname), STR_LEN(pathname));
    free(c_name);
    free(c_path);
}

/*=========================================================
 * utility routines independent of open files
 *=========================================================*/

CGIODLL void FMNAME(cgio_is_supported_f, CGIO_IS_SUPPORTED_F) (
    int *ier, int *file_type)
{
    *ier = cgio_is_supported(*file_type);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_cleanup_f, CGIO_CLEANUP_F) (int *ier)
{
    cgio_cleanup();
    *ier = 0;
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_check_file_f, CGIO_CHECK_FILE_F) (
    int *ier, STR_PSTR(filename), int *file_type STR_PLEN(filename))
{
    char *c_name = new_c_string(STR_PTR(filename), STR_LEN(filename), ier);

    if (*ier == 0) {
        *ier = cgio_check_file(c_name, file_type);
        free(c_name);
    }
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_compute_data_size_f, CGIO_COMPUTE_DATA_SIZE_F) (
    int *ier, STR_PSTR(data_type), int *ndims, int *dims, int *count,
    int *size STR_PLEN(data_type))
{
    unsigned long num;
    char c_type[CGIO_MAX_DATATYPE_LENGTH+1];

    to_c_string (STR_PTR(data_type), STR_LEN(data_type),
        c_type, CGIO_MAX_DATATYPE_LENGTH);
    *size = cgio_compute_data_size(c_type, *ndims, dims, &num);
    *count = (int)num;
    *ier = 0;
}

/*=========================================================
 * file operations
 *=========================================================*/

CGIODLL void FMNAME(cgio_open_file_f, CGIO_OPEN_FILE_F) (
    int *ier, STR_PSTR(filename), int *file_mode, int *file_type,
    int *cgio_num STR_PLEN(filename))
{
    char *c_name = new_c_string(STR_PTR(filename), STR_LEN(filename), ier);

    if (*ier == 0) {
        *ier = cgio_open_file(c_name, *file_mode, *file_type, cgio_num);
        free(c_name);
    }
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_close_file_f, CGIO_CLOSE_FILE_F) (
    int *ier, int *cgio_num)
{
    *ier = cgio_close_file(*cgio_num);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_flush_to_disk_f, CGIO_FLUSH_TO_DISK_F) (
    int *ier, int *cgio_num)
{
    *ier = cgio_flush_to_disk(*cgio_num);
}

/*=========================================================
 * file information
 *=========================================================*/

CGIODLL void FMNAME(cgio_library_version_f, CGIO_LIBRARY_VERSION_F) (
    int *ier, int *cgio_num, STR_PSTR(version) STR_PLEN(version))
{
    char c_version[CGIO_MAX_VERSION_LENGTH+1];

    *ier = cgio_library_version(*cgio_num, c_version);
    if (*ier == 0)
        to_f_string(c_version, STR_PTR(version), STR_LEN(version));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_file_version_f, CGIO_FILE_VERSION_F) (
    int *ier, int *cgio_num, STR_PSTR(file_version),
    STR_PSTR(creation_date), STR_PSTR(modified_date)
    STR_PLEN(file_version) STR_PLEN(creation_date)
    STR_PLEN(modified_date))
{
    char c_version[CGIO_MAX_VERSION_LENGTH+1];
    char c_cdate[CGIO_MAX_VERSION_LENGTH+1];
    char c_mdate[CGIO_MAX_VERSION_LENGTH+1];

    *ier = cgio_file_version(*cgio_num, c_version, c_cdate, c_mdate);
    if (*ier == 0) {
        to_f_string(c_version, STR_PTR(file_version), STR_LEN(file_version));
        to_f_string(c_cdate, STR_PTR(creation_date), STR_LEN(creation_date));
        to_f_string(c_mdate, STR_PTR(modified_date), STR_LEN(modified_date));
    }
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_root_id_f, CGIO_GET_ROOT_ID_F) (
    int *ier, int *cgio_num, double *rootid)
{
    *ier = cgio_get_root_id(*cgio_num, rootid);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_file_type_f, CGIO_GET_FILE_TYPE_F) (
    int *ier, int *cgio_num, int *file_type)
{
    *ier = cgio_get_file_type(*cgio_num, file_type);
}

/*=========================================================
 * error handling
 *=========================================================*/

CGIODLL void FMNAME(cgio_error_code_f, CGIO_ERROR_CODE_F) (
    int *errcode, int *file_type)
{
    cgio_error_code(errcode, file_type);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_error_message_f, CGIO_ERROR_MESSAGE_F) (
    int *ier, STR_PSTR(errmsg) STR_PLEN(errmsg))
{
    char c_error[CGIO_MAX_ERROR_LENGTH+1];

    *ier = cgio_error_message(CGIO_MAX_ERROR_LENGTH, c_error);
    if (*ier == 0)
        to_f_string(c_error, STR_PTR(errmsg), STR_LEN(errmsg));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_error_exit_f, CGIO_ERROR_EXIT_F) (
    STR_PSTR(errmsg) STR_PLEN(errmsg))
{
    int ier;
    char *c_error = new_c_string(STR_PTR(errmsg), STR_LEN(errmsg), &ier);

    cgio_error_exit(c_error);
}

/*=========================================================
 * basic node operations
 *=========================================================*/

CGIODLL void FMNAME(cgio_create_node_f, CGIO_CREATE_NODE_F) (
    int *ier, int *cgio_num, double *pid, STR_PSTR(name),
    double *id STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    to_c_string(STR_PTR(name), STR_LEN(name), c_name, CGIO_MAX_NAME_LENGTH);
    *ier = cgio_create_node(*cgio_num, *pid, c_name, id);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_delete_node_f, CGIO_DELETE_NODE_F) (
    int *ier, int *cgio_num, double *pid, double *id)
{
    *ier = cgio_delete_node(*cgio_num, *pid, *id);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_move_node_f, CGIO_MOVE_NODE_F) (
    int *ier, int *cgio_num, double *pid, double *id, double *npid)
{
    *ier = cgio_move_node(*cgio_num, *pid, *id, *npid);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_release_id_f, CGIO_RELEASE_ID_F) (
    int *ier, int *cgio_num, double *id)
{
    *ier = cgio_release_id(*cgio_num, *id);
}

/*=========================================================
 * links
 *=========================================================*/

CGIODLL void FMNAME(cgio_is_link_f, CGIO_IS_LINK_F) (
    int *ier, int *cgio_num, double *id, int *link_len)
{
    *ier = cgio_is_link(*cgio_num, *id, link_len);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_link_size_f, CGIO_LINK_SIZE_F) (
    int *ier, int *cgio_num, double *id, int *file_len, int *name_len)
{
    *ier = cgio_link_size(*cgio_num, *id, file_len, name_len);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_create_link_f, CGIO_CREATE_LINK_F) (
    int *ier, int *cgio_num, double *pid, STR_PSTR(name),
    STR_PSTR(filename), STR_PSTR(name_in_file), double *id
    STR_PLEN(name) STR_PLEN(filename) STR_PLEN(name_in_file))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    char *c_file, *c_link;

    c_file = new_c_string(STR_PTR(filename), STR_LEN(filename), ier);
    if (*ier) {
        if (*ier != CGIO_ERR_NULL_STRING) return;
        c_file = "";
    }
    c_link = new_c_string(STR_PTR(name_in_file), STR_LEN(name_in_file), ier);
    if (*ier) {
        if (*c_file) free(c_file);
        return;
    }
    to_c_string(STR_PTR(name), STR_LEN(name), c_name, CGIO_MAX_NAME_LENGTH);
    *ier = cgio_create_link(*cgio_num, *pid, c_name, c_file, c_link, id);
    if (*c_file) free(c_file);
    free(c_link);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_link_f, CGIO_GET_LINK_F) (
    int *ier, int *cgio_num, double *id, STR_PSTR(filename),
    STR_PSTR(name_in_file) STR_PLEN(filename) STR_PLEN(name_in_file))
{
    char c_file[CGIO_MAX_FILE_LENGTH+1];
    char c_link[CGIO_MAX_LINK_LENGTH+1];

    *ier = cgio_get_link(*cgio_num, *id, c_file, c_link);
    if (*ier == 0) {
        to_f_string(c_file, STR_PTR(filename), STR_LEN(filename));
        to_f_string(c_link, STR_PTR(name_in_file), STR_LEN(name_in_file));
    }
}

/*=========================================================
 * node children
 *=========================================================*/

CGIODLL void FMNAME(cgio_number_children_f, CGIO_NUMBER_CHILDREN_F) (
    int *ier, int *cgio_num, double *pid, int *num_children)
{
    *ier = cgio_number_children(*cgio_num, *pid, num_children);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_children_ids_f, CGIO_CHILDREN_IDS_F) (
    int *ier, int *cgio_num, double *pid, int *start,
    int *max_ret, int *num_ret, double *ids)
{
    *ier = cgio_children_ids(*cgio_num, *pid, *start, *max_ret,
               num_ret, ids);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_children_names_f, CGIO_CHILDREN_NAMES_F) (
    int *ier, int *cgio_num, double *pid, int *start, int *max_ret,
    int *name_len, int *num_ret, STR_PSTR(names) STR_PLEN(names))
{
    char *c_names;

    if ((*max_ret * *name_len) > STR_LEN(names)) {
        *ier = CGIO_ERR_TOO_SMALL;
        return;
    }
    c_names = (char *) malloc (*max_ret * (CGIO_MAX_NAME_LENGTH + 1));
    if (c_names == NULL) {
        *ier = CGIO_ERR_MALLOC;
        return;
    }
    *ier = cgio_children_names(*cgio_num, *pid, *start, *max_ret,
               CGIO_MAX_NAME_LENGTH, num_ret, c_names);
    if (*ier != 0) {
        int n;
        char *pc = c_names;
        char *pf = STR_PTR(names);
        to_f_string("", pf, STR_LEN(names));
        for (n = 0; n < *num_ret; n++) {
            to_f_string(pc, pf, *name_len);
            pc += (CGIO_MAX_NAME_LENGTH + 1);
            pf += *name_len;
        }
    }
    free(c_names);
}

/*=========================================================
 * read nodes
 *=========================================================*/

CGIODLL void FMNAME(cgio_get_node_id_f, CGIO_GET_NODE_ID_F) (
    int *ier, int *cgio_num, double *pid, STR_PSTR(name), double *id
    STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    to_c_string(STR_PTR(name), STR_LEN(name), c_name, CGIO_MAX_NAME_LENGTH);
    *ier = cgio_get_node_id(*cgio_num, *pid, c_name, id);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_name_f, CGIO_GET_NAME_F) (
    int *ier, int *cgio_num, double *id, STR_PSTR(name) STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cgio_get_name(*cgio_num, *id, c_name);
    if (*ier == 0)
        to_f_string(c_name, STR_PTR(name), STR_LEN(name));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_label_f, CGIO_GET_LABEL_F) (
    int *ier, int *cgio_num, double *id, STR_PSTR(label) STR_PLEN(label))
{
    char c_label[CGIO_MAX_LABEL_LENGTH+1];

    *ier = cgio_get_label(*cgio_num, *id, c_label);
    if (*ier == 0)
        to_f_string(c_label, STR_PTR(label), STR_LEN(label));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_data_type_f, CGIO_GET_DATA_TYPE_F) (
    int *ier, int *cgio_num, double *id, STR_PSTR(data_type)
    STR_PLEN(data_type))
{
    char c_type[CGIO_MAX_DATATYPE_LENGTH+1];

    *ier = cgio_get_data_type(*cgio_num, *id, c_type);
    if (*ier == 0)
        to_f_string(c_type, STR_PTR(data_type), STR_LEN(data_type));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_data_size_f, CGIO_GET_DATA_SIZE_F) (
    int *ier, int *cgio_num, double *id, int *size)
{
    unsigned long data_size;

    *ier = cgio_get_data_size(*cgio_num, *id, &data_size);
    if (*ier == 0)
        *size = (int)data_size;
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_dimensions_f, CGIO_GET_DIMENSIONS_F) (
    int *ier, int *cgio_num, double *id, int *ndims, int *dims)
{
    *ier = cgio_get_dimensions(*cgio_num, *id, ndims, dims);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_read_all_data_f, CGIO_READ_ALL_DATA_F) (
    int *ier, int *cgio_num, double *id, void *data)
{
    *ier = cgio_read_all_data(*cgio_num, *id, data);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_read_data_f, CGIO_READ_DATA_F) (
    int *ier, int *cgio_num, double *id, int *s_start, int *s_end,
    int *s_stride, int *m_ndims, int *m_dims, int *m_start,
    int *m_end, int *m_stride, void *data)
{
    *ier = cgio_read_data(*cgio_num, *id, s_start, s_end, s_stride,
               *m_ndims, m_dims, m_start, m_end, m_stride, data);
}

/*=========================================================
 * write nodes
 *=========================================================*/

CGIODLL void FMNAME(cgio_set_name_f, CGIO_SET_NAME_F) (
    int *ier, int *cgio_num, double *pid, double *id,
    STR_PSTR(name) STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    to_c_string(STR_PTR(name), STR_LEN(name), c_name, CGIO_MAX_NAME_LENGTH);
    *ier = cgio_set_name(*cgio_num, *pid, *id, c_name);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_set_label_f, CGIO_SET_LABEL_F) (
    int *ier, int *cgio_num, double *id, STR_PSTR(label) STR_PLEN(label))
{
    char c_label[CGIO_MAX_LABEL_LENGTH+1];

    to_c_string(STR_PTR(label), STR_LEN(label), c_label, CGIO_MAX_LABEL_LENGTH);
    *ier = cgio_set_label(*cgio_num, *id, c_label);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_set_dimensions_f, CGIO_SET_DIMENSIONS_F) (
    int *ier, int *cgio_num, double *id, STR_PSTR(data_type),
    int *ndims, int *dims STR_PLEN(data_type))
{
    char c_type[CGIO_MAX_DATATYPE_LENGTH+1];

    to_c_string(STR_PTR(data_type), STR_LEN(data_type),
        c_type, CGIO_MAX_DATATYPE_LENGTH);
    *ier = cgio_set_dimensions(*cgio_num, *id, c_type, *ndims, dims);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_write_all_data_f, CGIO_WRITE_ALL_DATA_F) (
    int *ier, int *cgio_num, double *id, void *data)
{
    *ier = cgio_write_all_data(*cgio_num, *id, data);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_write_data_f, CGIO_WRITE_DATA_F) (
    int *ier, int *cgio_num, double *id, int *s_start, int *s_end,
    int *s_stride, int *m_ndims, int *m_dims, int *m_start,
    int *m_end, int *m_stride, void *data)
{
    *ier = cgio_write_data(*cgio_num, *id, s_start, s_end, s_stride,
               *m_ndims, m_dims, m_start, m_end, m_stride, data);
}

#undef CGIODLL

