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
# define CGIODLL __declspec(dllexport)
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

    c_len = (int)strlen(c_str);
    if (c_len > f_len) c_len = f_len;

    for (i = 0; i < c_len; i++)
        f_str[i] = c_str[i];
    while (i < f_len)
        f_str[i++] = ' ';
    return f_len;
}

/*---------------------------------------------------------*/

static char *new_c_string (char *str, int len, cgint_f *ier)
{
    char *c_str;

    if (len < 1 || str == NULL) {
      *ier = (cgint_f)CGIO_ERR_NULL_STRING;
        return NULL;
    } else if ( len == 2 && strncmp(str, "\0", 2) == 0 ) {
      *ier = (cgint_f)CGIO_ERR_NULL_STRING;
      return NULL;
    }

    c_str = (char *) malloc (len + 1);
    if (c_str == NULL) {
        *ier = (cgint_f)CGIO_ERR_MALLOC;
        return NULL;
    }
    to_c_string (str, len, c_str, len);
    if (strlen(c_str) < 1) {
        free (c_str);
        *ier = (cgint_f)CGIO_ERR_NULL_STRING;
        return NULL;
    }
    *ier = 0;
    return c_str;
}

/*=========================================================
 * paths for searching for linked-to files
 *=========================================================*/

CGIODLL void FMNAME(cgio_path_add_f, CGIO_PATH_ADD_F) (
    STR_PSTR(path), cgint_f *ier STR_PLEN(path))
{
  char *c_path = new_c_string(STR_PTR(path), STR_LEN(path), ier);

  if (*ier == 0) {
    *ier = (cgint_f)cgio_path_add(c_path);
    free(c_path);
  }
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_path_delete_f, CGIO_PATH_DELETE_F) (
    STR_PSTR(path), cgint_f *ier STR_PLEN(path))
{
    char *c_path = new_c_string(STR_PTR(path), STR_LEN(path), ier);

    if (*ier == CGIO_ERR_MALLOC) return;
    *ier = (cgint_f)cgio_path_delete(c_path);
    if (c_path) free(c_path);
}

/*=========================================================
 * utility routines independent of open files
 *=========================================================*/

CGIODLL void cgio_is_supported_f(
    cgsize_t *file_type, cgint_f *ier)
{
    *ier = (cgint_f)cgio_is_supported((int)*file_type);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_check_file_f, CGIO_CHECK_FILE_F) (
    STR_PSTR(filename), cgint_f *file_type, cgint_f *ier STR_PLEN(filename))
{
    int i_file_type;
    char *c_name = new_c_string(STR_PTR(filename), STR_LEN(filename), ier);

    if (*ier == 0) {
        *ier = (cgint_f)cgio_check_file(c_name, &i_file_type);
        *file_type = (cgint_f)i_file_type;
        free(c_name);
    }
}

/*=========================================================
 * file operations
 *=========================================================*/

CGIODLL void FMNAME(cgio_open_file_f, CGIO_OPEN_FILE_F) (
    STR_PSTR(filename), cgint_f *file_mode, cgint_f *file_type,
    cgint_f *cgio_num, cgint_f *ier STR_PLEN(filename))
{
    int i_cgio_num;
    char *c_name = new_c_string(STR_PTR(filename), STR_LEN(filename), ier);

    if (*ier == 0) {
        *ier = (cgint_f)cgio_open_file(c_name, (int)*file_mode, (int)*file_type, &i_cgio_num);
        *cgio_num = (cgint_f)i_cgio_num;
        free(c_name);
    }
}

/*---------------------------------------------------------*/

CGIODLL void cgio_close_file_f(
    cgint_f *cgio_num, cgint_f *ier)
{
    *ier = (cgint_f)cgio_close_file((int)*cgio_num);
}

/*---------------------------------------------------------*/

CGIODLL void cgio_flush_to_disk_f(
    cgint_f *cgio_num, cgint_f *ier)
{
    *ier = (cgint_f)cgio_flush_to_disk((int)*cgio_num);
}

/*=========================================================
 * file information
 *=========================================================*/

CGIODLL void FMNAME(cgio_library_version_f, CGIO_LIBRARY_VERSION_F) (
    cgint_f *cgio_num, STR_PSTR(version), cgint_f *ier STR_PLEN(version))
{
    char c_version[CGIO_MAX_VERSION_LENGTH+1];

    *ier = (cgint_f)cgio_library_version((int)*cgio_num, c_version);
    if (*ier == 0)
        to_f_string(c_version, STR_PTR(version), STR_LEN(version));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_file_version_f, CGIO_FILE_VERSION_F) (
    cgint_f *cgio_num, STR_PSTR(file_version), STR_PSTR(creation_date),
    STR_PSTR(modified_date), cgint_f *ier STR_PLEN(file_version)
    STR_PLEN(creation_date) STR_PLEN(modified_date))
{
    char c_version[CGIO_MAX_VERSION_LENGTH+1];
    char c_cdate[CGIO_MAX_VERSION_LENGTH+1];
    char c_mdate[CGIO_MAX_VERSION_LENGTH+1];

    *ier = (cgint_f)cgio_file_version((int)*cgio_num, c_version, c_cdate, c_mdate);
    if (*ier == 0) {
        to_f_string(c_version, STR_PTR(file_version), STR_LEN(file_version));
        to_f_string(c_cdate, STR_PTR(creation_date), STR_LEN(creation_date));
        to_f_string(c_mdate, STR_PTR(modified_date), STR_LEN(modified_date));
    }
}

/*---------------------------------------------------------*/

CGIODLL void cgio_get_root_id_f(
    cgint_f *cgio_num, double *rootid, cgint_f *ier)
{
    *ier = (cgint_f)cgio_get_root_id((int)*cgio_num, rootid);
}

/*---------------------------------------------------------*/

CGIODLL void cgio_get_file_type_f(
    cgint_f *cgio_num, cgint_f *file_type, cgint_f *ier)
{
    int i_file_type;

    *ier = (cgint_f)cgio_get_file_type((int)*cgio_num, &i_file_type);
    *file_type = (cgint_f)i_file_type;
}

/*=========================================================
 * error handling
 *=========================================================*/

CGIODLL void cgio_error_code_f(
    cgint_f *errcode, cgint_f *file_type)
{
    int i_errcode, i_file_type;

    cgio_error_code(&i_errcode, &i_file_type);
    *errcode = (cgint_f)i_errcode;
    *file_type = (cgint_f)i_file_type;
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_error_message_f,CGIO_ERROR_MESSAGE_F) (
    STR_PSTR(errmsg), cgint_f *ier STR_PLEN(errmsg))
{
    char c_error[CGIO_MAX_ERROR_LENGTH+1];

    *ier = (cgint_f)cgio_error_message(c_error);
    if (*ier == 0)
        to_f_string(c_error, STR_PTR(errmsg), STR_LEN(errmsg));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_error_exit_f, CGIO_ERROR_EXIT_F)(
    STR_PSTR(errmsg) STR_PLEN(errmsg))
{
    cgint_f ier;
    char *c_error = new_c_string(STR_PTR(errmsg), STR_LEN(errmsg), &ier);

    cgio_error_exit(c_error);
}

/*---------------------------------------------------------*/

CGIODLL void cgio_error_abort_f(
    cgint_f *abort_flag)
{
    cgio_error_abort((int)*abort_flag);
}

/*=========================================================
 * basic node operations
 *=========================================================*/

CGIODLL void FMNAME(cgio_create_node_f, CGIO_CREATE_NODE_F) (
    cgint_f *cgio_num, double *pid, STR_PSTR(name),
    double *id, cgint_f *ier STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    to_c_string(STR_PTR(name), STR_LEN(name), c_name, CGIO_MAX_NAME_LENGTH);
    *ier = (cgint_f)cgio_create_node((int)*cgio_num, *pid, c_name, id);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_new_node_f, CGIO_NEW_NODE_F) (
    cgint_f *cgio_num, double *pid, STR_PSTR(name), STR_PSTR(label),
    STR_PSTR(data_type), cgint_f *ndims, cgsize_t *dims, void *data,
    double *id, cgint_f *ier STR_PLEN(name) STR_PLEN(label) STR_PLEN(data_type))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    char c_label[CGIO_MAX_LABEL_LENGTH+1];
    char c_dtype[CGIO_MAX_DATATYPE_LENGTH+1];

    to_c_string(STR_PTR(name), STR_LEN(name), c_name, CGIO_MAX_NAME_LENGTH);
    to_c_string(STR_PTR(label), STR_LEN(label), c_label, CGIO_MAX_LABEL_LENGTH);
    to_c_string(STR_PTR(data_type), STR_LEN(data_type),
                c_dtype, CGIO_MAX_DATATYPE_LENGTH);
    *ier = (cgint_f)cgio_new_node((int)*cgio_num, *pid, c_name, c_label, c_dtype,
               (int)*ndims, dims, data, id);
}

/*---------------------------------------------------------*/

CGIODLL void cgio_delete_node_f(
    cgint_f *cgio_num, double *pid, double *id, cgint_f *ier)
{
    *ier = (cgint_f)cgio_delete_node((int)*cgio_num, *pid, *id);
}

/*---------------------------------------------------------*/

CGIODLL void cgio_move_node_f(
    cgint_f *cgio_num, double *pid, double *id, double *npid, cgint_f *ier)
{
    *ier = (cgint_f)cgio_move_node((int)*cgio_num, *pid, *id, *npid);
}

/*---------------------------------------------------------*/

CGIODLL void cgio_release_id_f(
    cgint_f *cgio_num, double *id, cgint_f *ier)
{
    *ier = (cgint_f)cgio_release_id((int)*cgio_num, *id);
}

/*=========================================================
 * links
 *=========================================================*/

CGIODLL void cgio_is_link_f(
    cgint_f *cgio_num, double *id, cgint_f *link_len, cgint_f *ier)
{
    int i_link_len;

    *ier = (cgint_f)cgio_is_link((int)*cgio_num, *id, &i_link_len);
    *link_len = (cgint_f)i_link_len;
}

/*---------------------------------------------------------*/

CGIODLL void cgio_link_size_f(
    cgint_f *cgio_num, double *id, cgint_f *file_len,
    cgint_f *name_len, cgint_f *ier)
{
    int i_file_len, i_name_len;

    *ier = (cgint_f)cgio_link_size((int)*cgio_num, *id, &i_file_len, &i_name_len);
    *file_len = (cgint_f)i_file_len;
    *name_len = (cgint_f)i_name_len;
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_create_link_f, CGIO_CREATE_LINK_F) (
    cgint_f *cgio_num, double *pid, STR_PSTR(name), STR_PSTR(filename),
    STR_PSTR(name_in_file), double *id, cgint_f *ier
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
    *ier = (cgint_f)cgio_create_link((int)*cgio_num, *pid, c_name, c_file, c_link, id);
    if (*c_file) free(c_file);
    free(c_link);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_link_f, CGIO_GET_LINK_F) (
    cgint_f *cgio_num, double *id, STR_PSTR(filename), STR_PSTR(name_in_file),
    cgint_f *ier STR_PLEN(filename) STR_PLEN(name_in_file))
{
    char c_file[CGIO_MAX_FILE_LENGTH+1];
    char c_link[CGIO_MAX_LINK_LENGTH+1];

    *ier = (cgint_f)cgio_get_link((int)*cgio_num, *id, c_file, c_link);
    if (*ier == 0) {
        to_f_string(c_file, STR_PTR(filename), STR_LEN(filename));
        to_f_string(c_link, STR_PTR(name_in_file), STR_LEN(name_in_file));
    }
}

/*=========================================================
 * node children
 *=========================================================*/

CGIODLL void cgio_number_children_f(
    cgint_f *cgio_num, double *pid, cgint_f *num_children, cgint_f *ier)
{
    int i_num_children;

    *ier = cgio_number_children(*cgio_num, *pid, &i_num_children);
    *num_children = (cgint_f)i_num_children;
}

/*---------------------------------------------------------*/

CGIODLL void cgio_children_ids_f(
    cgint_f *cgio_num, double *pid, cgint_f *start, cgint_f *max_ret,
    cgint_f *num_ret, double *ids, cgint_f *ier)
{
    int i_num_ret;

    *ier = cgio_children_ids((int)*cgio_num, *pid, (int)*start, (int)*max_ret,
               &i_num_ret, ids);
    *num_ret = (cgint_f)i_num_ret;
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_children_names_f, CGIO_CHILDREN_NAMES_F) (
    cgint_f *cgio_num, double *pid, cgint_f *start, cgint_f *max_ret,
    cgint_f *name_len, cgint_f *num_ret, STR_PSTR(names),
    cgint_f *ier STR_PLEN(names))
{
    int i_num_ret, i_name_len = (int)*name_len;
    char *c_names;

    c_names = (char *) malloc ((size_t)*max_ret * (CGIO_MAX_NAME_LENGTH + 1));
    if (c_names == NULL) {
      *ier = (cgint_f)CGIO_ERR_MALLOC;
        return;
    }
    *ier = (cgint_f)cgio_children_names((int)*cgio_num, *pid, (int)*start, (int)*max_ret,
               CGIO_MAX_NAME_LENGTH + 1, &i_num_ret, c_names);
    if (*ier == 0) {
        int n;
        char *pc = c_names;
        char *pf = STR_PTR(names);
        for (n = 0; n < i_num_ret; n++) {
            to_f_string(pc, pf, i_name_len);
            pc += (CGIO_MAX_NAME_LENGTH + 1);
            pf += i_name_len;
        }
        *num_ret = (cgint_f)i_num_ret;
    }
    free(c_names);
}

/*=========================================================
 * read nodes
 *=========================================================*/

CGIODLL void FMNAME(cgio_get_node_id_f, CGIO_GET_NODE_ID_F) (
    cgint_f *cgio_num, double *pid, STR_PSTR(name), double *id,
    cgint_f *ier STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    to_c_string(STR_PTR(name), STR_LEN(name), c_name, CGIO_MAX_NAME_LENGTH);
    *ier = (cgint_f)cgio_get_node_id((int)*cgio_num, *pid, c_name, id);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_name_f, CGIO_GET_NAME_F) (
    cgint_f *cgio_num, double *id, STR_PSTR(name),
    cgint_f *ier STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cgio_get_name((int)*cgio_num, *id, c_name);
    if (*ier == 0)
        to_f_string(c_name, STR_PTR(name), STR_LEN(name));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_label_f, CGIO_GET_LABEL_F) (
    cgint_f *cgio_num, double *id, STR_PSTR(label),
    cgint_f *ier STR_PLEN(label))
{
    char c_label[CGIO_MAX_LABEL_LENGTH+1];

    *ier = (cgint_f)cgio_get_label((int)*cgio_num, *id, c_label);
    if (*ier == 0)
        to_f_string(c_label, STR_PTR(label), STR_LEN(label));
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_get_data_type_f, CGIO_GET_DATA_TYPE_F) (
    cgint_f *cgio_num, double *id, STR_PSTR(data_type),
    cgint_f *ier STR_PLEN(data_type))
{
    char c_type[CGIO_MAX_DATATYPE_LENGTH+1];

    *ier = (cgint_f)cgio_get_data_type((int)*cgio_num, *id, c_type);
    if (*ier == 0)
        to_f_string(c_type, STR_PTR(data_type), STR_LEN(data_type));
}

/*---------------------------------------------------------*/

CGIODLL void cgio_get_data_size_f(
    cgint_f *cgio_num, double *id, cgsize_t *size, cgint_f *ier)
{
    cglong_t data_size;

    *ier = (cgint_f)cgio_get_data_size((int)*cgio_num, *id, &data_size);
    *size = (cgsize_t)data_size;
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_read_all_data_type_f, CGIO_READ_ALL_DATA_TYPE_F) (
    cgint_f *cgio_num, double *id, STR_PSTR(m_data_type), void *data, cgint_f *ier STR_PLEN(m_data_type))
{
    char c_dtype[CGIO_MAX_DATATYPE_LENGTH+1];
    to_c_string(STR_PTR(m_data_type), STR_LEN(m_data_type), c_dtype, CGIO_MAX_DATATYPE_LENGTH);
    *ier = (cgint_f)cgio_read_all_data_type((int)*cgio_num, *id, c_dtype, data);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_read_block_data_type_f, CGIO_READ_BLOCK_DATA_TYPE_F) (
    cgint_f *cgio_num, double *id, cgsize_t *b_start, cgsize_t *b_end,
    STR_PSTR(m_data_type), void *data, cgint_f *ier STR_PLEN(m_data_type))
{
    char c_dtype[CGIO_MAX_DATATYPE_LENGTH+1];
    to_c_string(STR_PTR(m_data_type), STR_LEN(m_data_type), c_dtype, CGIO_MAX_DATATYPE_LENGTH);
    *ier = (cgint_f)cgio_read_block_data_type((int)*cgio_num, *id, *b_start, *b_end, c_dtype, data);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_read_data_type_f, CGIO_READ_DATA_TYPE_F) (
    cgint_f *cgio_num, double *id, cgsize_t *s_start, cgsize_t *s_end,
    cgsize_t *s_stride,  STR_PSTR(m_data_type), cgint_f *m_ndims, cgsize_t *m_dims, cgsize_t *m_start,
    cgsize_t *m_end, cgsize_t *m_stride, void *data, cgint_f *ier STR_PLEN(m_data_type))
{
    char c_dtype[CGIO_MAX_DATATYPE_LENGTH+1];
    to_c_string(STR_PTR(m_data_type), STR_LEN(m_data_type), c_dtype, CGIO_MAX_DATATYPE_LENGTH);
    *ier = (cgint_f)cgio_read_data_type((int)*cgio_num, *id, s_start, s_end, s_stride, c_dtype,
           (int)*m_ndims, m_dims, m_start, m_end, m_stride, data);
}

/*=========================================================
 * write nodes
 *=========================================================*/

CGIODLL void FMNAME(cgio_set_name_f, CGIO_SET_NAME_F) (
    cgint_f *cgio_num, double *pid, double *id, STR_PSTR(name),
    cgint_f *ier STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    to_c_string(STR_PTR(name), STR_LEN(name), c_name, CGIO_MAX_NAME_LENGTH);
    *ier = (cgint_f)cgio_set_name((int)*cgio_num, *pid, *id, c_name);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_set_label_f, CGIO_SET_LABEL_F) (
    cgint_f *cgio_num, double *id, STR_PSTR(label),
    cgint_f *ier STR_PLEN(label))
{
    char c_label[CGIO_MAX_LABEL_LENGTH+1];

    to_c_string(STR_PTR(label), STR_LEN(label), c_label, CGIO_MAX_LABEL_LENGTH);
    *ier = (cgint_f)cgio_set_label((int)*cgio_num, *id, c_label);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_write_all_data_f, CGIO_WRITE_ALL_DATA_F) (
    cgint_f *cgio_num, double *id, void *data, cgint_f *ier)
{
    *ier = (cgint_f)cgio_write_all_data((int)*cgio_num, *id, data);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_write_block_data_f, CGIO_WRITE_BLOCK_DATA_F) (
    cgint_f *cgio_num, double *id, cgsize_t *b_start, cgsize_t *b_end,
    void *data, cgint_f *ier)
{
    *ier = (cgint_f)cgio_write_block_data((int)*cgio_num, *id, *b_start, *b_end, data);
}

/*---------------------------------------------------------*/

CGIODLL void FMNAME(cgio_write_data_f, CGIO_WRITE_DATA_F) (
    cgint_f *cgio_num, double *id, cgsize_t *s_start, cgsize_t *s_end,
    cgsize_t *s_stride, cgsize_t *m_ndims, cgsize_t *m_dims, cgsize_t *m_start,
    cgsize_t *m_end, cgsize_t *m_stride, void *data, cgint_f *ier)
{
    *ier = (cgint_f)cgio_write_data((int)*cgio_num, *id, s_start, s_end, s_stride,
               (int)*m_ndims, m_dims, m_start, m_end, m_stride, data);
}

#undef CGIODLL

