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
#include "fortran_macros.h"
#include "cgnslib.h"
#include "cgns_header.h"
#include <string.h>
#include "cgns_io.h"
#ifdef MEM_DEBUG
#include "cg_malloc.h"
#endif
#if CG_BUILD_PARALLEL
#include "pcgnslib.h"
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Convert between Fortran and C strings                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void string_2_C_string(char *string, int string_length,
	char *c_string, int max_len, cgint_f *ierr)
{
    int i, iend;

    if (string == NULL || c_string == NULL) {
        cgi_error ("NULL string pointer");
        *ierr = (cgint_f)CG_ERROR;
        return;
    }

    /** Skip and trailing blanks **/
    for (iend = string_length-1; iend >= 0; iend--) {
        if (string[iend] != ' ') break;
    }
    if (iend >= max_len) iend = max_len - 1;

    /** Copy the non-trailing blank portion of the string **/
    for (i = 0; i <= iend; i++)
        c_string[i] = string[i];

    /** NULL terminate the C string **/
    c_string[i] = '\0';
    *ierr = (cgint_f)CG_OK;
}

/*-----------------------------------------------------------------------*/

static void string_2_F_string(char *c_string, char *string,
	int string_length, cgint_f *ierr)
{
    int i, len;

    if (c_string == NULL || string == NULL) {
        cgi_error ("NULL string pointer");
        *ierr = (cgint_f)CG_ERROR;
        return;
    }
    len = (int)strlen(c_string);
    if (len > string_length) len = string_length;

    for (i = 0; i < len; i++)
        string[i] = c_string[i];
    while (i < string_length)
        string[i++] = ' ';
    *ierr = (cgint_f)CG_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      LIBRARY FUNCTIONS                                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_is_cgns_f, CG_IS_CGNS_F) (STR_PSTR(filename),
	cgint_f *file_type, cgint_f *ier STR_PLEN(filename))
{
    int length, i_file_type;
    char *c_name;

    length = (int) STR_LEN(filename);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(filename), STR_LEN(filename), c_name, length, ier);
    if (*ier == 0) {
        *ier = (cgint_f)cg_is_cgns(c_name, &i_file_type);
        *file_type = (cgint_f)i_file_type;
    }
    CGNS_FREE(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_open_f, CG_OPEN_F) (STR_PSTR(filename), cgint_f *mode,
	cgint_f *fn, cgint_f *ier STR_PLEN(filename))
{
    int length, i_fn;
    char *c_name;

    length = (int) STR_LEN(filename);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(filename), STR_LEN(filename), c_name, length, ier);
    if (*ier == 0) {
#if DEBUG_FTOC
        printf("filename='%s'\n",c_name);
#endif
        *ier = (cgint_f)cg_open(c_name, (int)*mode, &i_fn);
        *fn  = (cgint_f)i_fn;
    }
    CGNS_FREE(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_version_f(cgint_f *fn, float *FileVersion, cgint_f *ier)
{
    *ier = (cgint_f)cg_version((int)*fn, FileVersion);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_precision_f(cgint_f *fn, cgint_f *precision, cgint_f *ier)
{
    int i_precision;

    *ier = (cgint_f)cg_precision((int)*fn, &i_precision);
    *precision = (cgint_f)i_precision;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_close_f(cgint_f *fn, cgint_f *ier)
{
  *ier = (cgint_f)cg_close((int)*fn);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_save_as_f, CG_SAVE_AS_F) (cgint_f *fn,
	STR_PSTR(filename), cgint_f *file_type, cgint_f *follow_links,
	cgint_f *ier STR_PLEN(filename))
{
    int length;
    char *c_name;

    length = (int) STR_LEN(filename);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(filename), STR_LEN(filename), c_name, length, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_save_as((int)*fn, c_name, (int)*file_type, (int)*follow_links);
    CGNS_FREE(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_set_file_type_f(cgint_f *ft, cgint_f *ier)
{
    *ier = (cgint_f)cg_set_file_type((int)*ft);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_configure_c_ptr(cgint_f *what, void *value, cgint_f *ier)
{
  /* CHARACTERS */
  if( (int)*what == CG_CONFIG_SET_PATH ||
      (int)*what == CG_CONFIG_ADD_PATH) {
    *ier = (cgint_f)cg_configure((int)*what, value);

  /* MPI COMMUNICATOR */
#if CG_BUILD_PARALLEL
  } else if( (int)*what == CG_CONFIG_HDF5_MPI_COMM ) {
    MPI_Fint F_comm = *(MPI_Fint *)value;
    MPI_Comm C_comm = MPI_Comm_f2c(F_comm);
    *ier = (cgint_f)cg_configure((int)*what, &C_comm);
#endif

  /* RIND */    
  } else if( (int)*what == CG_CONFIG_RIND_INDEX) {
    if(*(int*)value == 0) {
      *ier = (cgint_f)cg_configure((int)*what, CG_CONFIG_RIND_ZERO);
    } else if(*(int*)value == 1) {
      *ier = (cgint_f)cg_configure((int)*what, CG_CONFIG_RIND_CORE);
    } else {
      *ier = (cgint_f)CG_ERROR;
      return;
    }

  /* EVERYTHING ELSE */
  } else {
    *ier = (cgint_f)cg_configure((int)*what, (void *)(*(size_t *)value));
  }
}
/*-----------------------------------------------------------------------*/

CGNSDLL void cg_configure_c_funptr(cgint_f *what, void *value, cgint_f *ier)
{
  /* FUNCTION POINTER */
  if( (int)*what == CG_CONFIG_ERROR ) {
    *ier = (cgint_f)cg_configure((int)*what, value);
  } else {
    *ier = (cgint_f)CG_ERROR;
  }
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_get_file_type_f(cgint_f *fn, cgint_f *ft, cgint_f *ier)
{
    int i_ft;

    *ier = (cgint_f)cg_get_file_type((int)*fn, &i_ft);
    *ft = (cgint_f)i_ft;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_set_compress_f(cgint_f *cmpr, cgint_f *ier)
{
    *ier = (cgint_f)cg_set_compress((int)*cmpr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_get_compress_f(cgint_f *cmpr, cgint_f *ier)
{
    int i_cmpr;

    *ier = (cgint_f)cg_get_compress(&i_cmpr);
    *cmpr = (cgint_f)i_cmpr;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_set_path_f, CG_SET_PATH_F) (STR_PSTR(pathname),
	cgint_f *ier STR_PLEN(pathname))
{
    int length;
    char *c_name;

    length = (int) STR_LEN(pathname);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(pathname), STR_LEN(pathname), c_name, length, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_set_path(c_name);
    CGNS_FREE(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_add_path_f, CG_ADD_PATH_F) (STR_PSTR(pathname),
	cgint_f *ier STR_PLEN(pathname))
{
    int length;
    char *c_name;

    length = (int) STR_LEN(pathname);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(pathname), STR_LEN(pathname), c_name, length, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_add_path(c_name);
    CGNS_FREE(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_set_rind_zero_f(cgint_f *ier)
{
    *ier = (cgint_f)cg_configure(CG_CONFIG_RIND_INDEX, (void *)CG_CONFIG_RIND_ZERO);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_set_rind_core_f(cgint_f *ier)
{
    *ier = (cgint_f)cg_configure(CG_CONFIG_RIND_INDEX, (void *)CG_CONFIG_RIND_CORE);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_get_cgio_f(cgint_f *fn, cgint_f *cgio_num, cgint_f *ier)
{
    int i_cgio_num;

    *ier = (cgint_f)cg_get_cgio((int)*fn, &i_cgio_num);
    *cgio_num = (cgint_f)i_cgio_num;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_root_id_f(cgint_f *fn, double *rootid, cgint_f *ier)
{
    *ier = (cgint_f)cg_root_id((int)*fn, rootid);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write CGNSBase_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nbases_f(cgint_f *fn, cgint_f *nbases, cgint_f *ier)
{
    int i_nbases;

    *ier = (cgint_f)cg_nbases((int)*fn, &i_nbases);
    *nbases = (cgint_f)i_nbases;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_base_read_f, CG_BASE_READ_F) (cgint_f *fn, cgint_f *B,
	STR_PSTR(basename), cgint_f *cell_dim, cgint_f *phys_dim,
	cgint_f *ier STR_PLEN(basename))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_cell_dim, i_phys_dim;

    *ier = (cgint_f)cg_base_read((int)*fn, (int)*B, c_name, &i_cell_dim, &i_phys_dim);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(basename), STR_LEN(basename), ier);
    *cell_dim = (cgint_f)i_cell_dim;
    *phys_dim = (cgint_f)i_phys_dim;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_base_id_f(cgint_f *fn, cgint_f *B, double *base_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_base_id((int)*fn, (int)*B, base_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_base_write_f, CG_BASE_WRITE_F) (cgint_f *fn,
	STR_PSTR(basename), cgint_f *cell_dim, cgint_f *phys_dim,
	cgint_f *B, cgint_f *ier STR_PLEN(basename))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_B;

    string_2_C_string(STR_PTR(basename), STR_LEN(basename),
		      c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("\nbasename='%s'\n", c_name);
    printf("cell_dim=%d\n",*cell_dim);
    printf("phys_dim=%d\n",*phys_dim);
#endif
    *ier = (cgint_f)cg_base_write((int)*fn, c_name, (int)*cell_dim, (int)*phys_dim, &i_B);
    *B = (cgint_f)i_B;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_cell_dim_f(cgint_f *fn, cgint_f *B, cgint_f *dim, cgint_f *ier)
{
    int i_dim;

    *ier = (cgint_f)cg_cell_dim((int)*fn, (int)*B, &i_dim);
    *dim = (cgint_f)i_dim;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Zone_t Nodes                                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nzones_f(cgint_f *fn, cgint_f *B, cgint_f *nzones, cgint_f *ier)
{
    int i_nzones;

    *ier = (cgint_f)cg_nzones((int)*fn, (int)*B, &i_nzones);
    *nzones = (cgint_f)i_nzones;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_zone_type_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, CGNS_ENUMT(ZoneType_t)*type, cgint_f *ier)
{
    CGNS_ENUMT(ZoneType_t) i_type;

    *ier = (cgint_f)cg_zone_type((int)*fn, (int)*B, (int)*Z, &i_type);
    *type = i_type;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_zone_read_f, CG_ZONE_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, STR_PSTR(zonename), cgsize_t *size,
	cgint_f *ier STR_PLEN(zonename))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_zone_read((int)*fn, (int)*B, (int)*Z, c_name, size);
    if (*ier == 0)
      string_2_F_string(c_name, STR_PTR(zonename), STR_LEN(zonename), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_zone_id_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, double *zone_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_zone_id((int)*fn, (int)*B, (int)*Z, zone_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_zone_write_f, CG_ZONE_WRITE_F) (cgint_f *fn, cgint_f *B,
	STR_PSTR(zonename), cgsize_t *size, CGNS_ENUMT(ZoneType_t) *type,
	cgint_f *Z, cgint_f *ier STR_PLEN(zonename))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_Z;

    string_2_C_string(STR_PTR(zonename), STR_LEN(zonename),
		      c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("\n  zonename='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_zone_write((int)*fn, (int)*B, c_name, size,
               (CGNS_ENUMT(ZoneType_t))*type, &i_Z);
    *Z = (cgint_f)i_Z;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_index_dim_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *dim, cgint_f *ier)
{
    int i_dim;

    *ier = (cgint_f)cg_index_dim((int)*fn, (int)*B, (int)*Z, &i_dim);
    *dim = (cgint_f)i_dim;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Family_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nfamilies_f(cgint_f *fn, cgint_f *B,
	cgint_f *nfamilies, cgint_f *ier)
{
    int i_nfamilies;

    *ier = (cgint_f)cg_nfamilies((int)*fn, (int)*B, &i_nfamilies);
    *nfamilies = (cgint_f)i_nfamilies;
}

CGNSDLL void cg_node_nfamilies_f(cgint_f *nfamilies, cgint_f *ier)
{
    int i_nfamilies;

    *ier = (cgint_f)cg_node_nfamilies(&i_nfamilies);
    *nfamilies = (cgint_f)i_nfamilies;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_family_read_f, CG_FAMILY_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *F, STR_PSTR(family_name), cgint_f *nboco, cgint_f *ngeos,
	cgint_f *ier STR_PLEN(family_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_nboco, i_ngeos;

    *ier = (cgint_f)cg_family_read((int)*fn, (int)*B, (int)*F, c_name, &i_nboco, &i_ngeos);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(family_name), STR_LEN(family_name), ier);

    *nboco = (cgint_f)i_nboco;
    *ngeos = (cgint_f)i_ngeos;
}

CGNSDLL void FMNAME(cg_node_family_read_f, CG_NODE_FAMILY_READ_F) (
    cgint_f *F, STR_PSTR(family_name), cgint_f *nboco, cgint_f *ngeos,
    cgint_f *ier STR_PLEN(family_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_nboco, i_ngeos;

    *ier = (cgint_f)cg_node_family_read((int)*F, c_name, &i_nboco, &i_ngeos);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(family_name), STR_LEN(family_name), ier);

    *nboco = (cgint_f)i_nboco;
    *ngeos = (cgint_f)i_ngeos;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_family_write_f, CG_FAMILY_WRITE_F) (cgint_f *fn, cgint_f *B,
	STR_PSTR(family_name), cgint_f *F, cgint_f *ier STR_PLEN(family_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH*CG_MAX_GOTO_DEPTH+1];
    int i_F;

    string_2_C_string(STR_PTR(family_name), STR_LEN(family_name),
		      c_name, CGIO_MAX_NAME_LENGTH*CG_MAX_GOTO_DEPTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_family_write((int)*fn, (int)*B, c_name, &i_F);
    *F = (cgint_f)i_F;
}

CGNSDLL void FMNAME(cg_node_family_write_f, CG_NODE_FAMILY_WRITE_F) (
    STR_PSTR(family_name), cgint_f *F, cgint_f *ier STR_PLEN(family_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_F;

    string_2_C_string(STR_PTR(family_name), STR_LEN(family_name),
              c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_node_family_write(c_name, &i_F);
    *F = (cgint_f)i_F;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_nfamily_names_f(cgint_f *fn,
	cgint_f *B, cgint_f *F, cgint_f *nnames, cgint_f *ier)
{
    int i_nnames;

    *ier = (cgint_f)cg_nfamily_names((int)*fn, (int)*B, (int)*F, &i_nnames);
    *nnames = (cgint_f)i_nnames;
}

CGNSDLL void cg_node_nfamily_names_f(cgint_f *nnames, cgint_f *ier)
{
    int i_nnames;

    *ier = (cgint_f)cg_node_nfamily_names(&i_nnames);
    *nnames = (cgint_f)i_nnames;
}
/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_family_name_read_f, CG_FAMILY_NAME_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *F, cgint_f *N, STR_PSTR(name), STR_PSTR(family),
	cgint_f *ier STR_PLEN(name) STR_PLEN(family))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    char c_family[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_family_name_read((int)*fn, (int)*B, (int)*F, (int)*N, c_name, c_family);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(name), STR_LEN(name), ier);
    if (*ier) return;
    string_2_F_string(c_family, STR_PTR(family), STR_LEN(family), ier);
}

CGNSDLL void FMNAME(cg_node_family_name_read_f, CG_NODE_FAMILY_NAME_READ_F) (
    cgint_f *N, STR_PSTR(name), STR_PSTR(family),
    cgint_f *ier STR_PLEN(name) STR_PLEN(family))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    char c_family[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_node_family_name_read((int)*N, c_name, c_family);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(name), STR_LEN(name), ier);
    if (*ier) return;
    string_2_F_string(c_family, STR_PTR(family), STR_LEN(family), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_family_name_write_f, CG_FAMILY_NAME_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *F, STR_PSTR(name), STR_PSTR(family),
	cgint_f *ier STR_PLEN(name) STR_PLEN(family))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    char c_family[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(name), STR_LEN(name),
		      c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(family), STR_LEN(family),
		      c_family, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_family_name_write((int)*fn, (int)*B, (int)*F, c_name, c_family);
}

CGNSDLL void FMNAME(cg_node_family_name_write_f, CG_NODE_FAMILY_NAME_WRITE_F) (
    STR_PSTR(name), STR_PSTR(family),
    cgint_f *ier STR_PLEN(name) STR_PLEN(family))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    char c_family[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(name), STR_LEN(name),
              c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(family), STR_LEN(family),
              c_family, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_node_family_name_write(c_name, c_family);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FamBC_t Nodes                                     *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_fambc_read_f, CG_FAMBC_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *F, cgint_f *BC, STR_PSTR(fambc_name), CGNS_ENUMT(BCType_t) *bocotype,
	cgint_f *ier STR_PLEN(fambc_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(BCType_t) i_bocotype;

    *ier = (cgint_f)cg_fambc_read((int)*fn, (int)*B, (int)*F, (int)*BC,
               c_name, &i_bocotype);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(fambc_name), STR_LEN(fambc_name), ier);

    *bocotype = (CGNS_ENUMT(BCType_t))i_bocotype;
}

CGNSDLL void FMNAME(cg_node_fambc_read_f, CG_NODE_FAMBC_READ_F) (
    cgint_f *BC, STR_PSTR(fambc_name), CGNS_ENUMT(BCType_t) *bocotype,
    cgint_f *ier STR_PLEN(fambc_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(BCType_t) i_bocotype;

    *ier = (cgint_f)cg_node_fambc_read((int)*BC, c_name, &i_bocotype);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(fambc_name), STR_LEN(fambc_name), ier);

    *bocotype = (CGNS_ENUMT(BCType_t))i_bocotype;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_fambc_write_f, CG_FAMBC_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *F, STR_PSTR(fambc_name), CGNS_ENUMT(BCType_t) *bocotype,
	cgint_f *BC, cgint_f *ier STR_PLEN(fambc_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_BC;

    string_2_C_string(STR_PTR(fambc_name), STR_LEN(fambc_name),
		      c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_fambc_write((int)*fn, (int)*B, (int)*F, c_name,
               (CGNS_ENUMT(BCType_t))*bocotype, &i_BC);
    *BC = (cgint_f)i_BC;
}

CGNSDLL void FMNAME(cg_node_fambc_write_f, CG_NODE_FAMBC_WRITE_F) (
    STR_PSTR(fambc_name), CGNS_ENUMT(BCType_t) *bocotype,
    cgint_f *BC, cgint_f *ier STR_PLEN(fambc_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_BC;

    string_2_C_string(STR_PTR(fambc_name), STR_LEN(fambc_name),
              c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_node_fambc_write(c_name,
               (CGNS_ENUMT(BCType_t))*bocotype, &i_BC);
    *BC = (cgint_f)i_BC;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryReference_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_geo_read_f, CG_GEO_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *F, cgint_f *G, STR_PSTR(geo_name), STR_PSTR(geo_file),
	STR_PSTR(CAD_name), cgint_f *npart, cgint_f *ier STR_PLEN(geo_name)
	STR_PLEN(geo_file) STR_PLEN(CAD_name))
{
    char c_geo_name[CGIO_MAX_NAME_LENGTH+1];
    char c_CAD_name[CGIO_MAX_NAME_LENGTH+1];
    char *c_geo_file;
    int i_npart;

    *ier = (cgint_f)cg_geo_read((int)*fn, (int)*B, (int)*F, (int)*G, c_geo_name,
               &c_geo_file, c_CAD_name, &i_npart);
    if (*ier) return;
    *npart = (cgint_f)i_npart;
    string_2_F_string(c_geo_file, STR_PTR(geo_file), STR_LEN(geo_file), ier);
    CGNS_FREE(c_geo_file);
    if (*ier) return;
    string_2_F_string(c_geo_name, STR_PTR(geo_name), STR_LEN(geo_name), ier);
    if (*ier) return;
    string_2_F_string(c_CAD_name, STR_PTR(CAD_name), STR_LEN(CAD_name), ier);
}

CGNSDLL void FMNAME(cg_node_geo_read_f, CG_NODE_GEO_READ_F) (
    cgint_f *G, STR_PSTR(geo_name), STR_PSTR(geo_file),
    STR_PSTR(CAD_name), cgint_f *npart, cgint_f *ier STR_PLEN(geo_name)
    STR_PLEN(geo_file) STR_PLEN(CAD_name))
{
    char c_geo_name[CGIO_MAX_NAME_LENGTH+1];
    char c_CAD_name[CGIO_MAX_NAME_LENGTH+1];
    char *c_geo_file;
    int i_npart;

    *ier = (cgint_f)cg_node_geo_read((int)*G, c_geo_name,
               &c_geo_file, c_CAD_name, &i_npart);
    if (*ier) return;
    *npart = (cgint_f)i_npart;
    string_2_F_string(c_geo_file, STR_PTR(geo_file), STR_LEN(geo_file), ier);
    CGNS_FREE(c_geo_file);
    if (*ier) return;
    string_2_F_string(c_geo_name, STR_PTR(geo_name), STR_LEN(geo_name), ier);
    if (*ier) return;
    string_2_F_string(c_CAD_name, STR_PTR(CAD_name), STR_LEN(CAD_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_geo_write_f, CG_GEO_WRITE_F) (cgint_f *fn, cgint_f *B,
	cgint_f *F, STR_PSTR(geo_name), STR_PSTR(geo_file), STR_PSTR(CAD_name),
	cgint_f *G, cgint_f *ier STR_PLEN(geo_name) STR_PLEN(geo_file)
	STR_PLEN(CAD_name))
{
    char c_geo_name[CGIO_MAX_NAME_LENGTH+1];
    char c_CAD_name[CGIO_MAX_NAME_LENGTH+1];
    char *c_geo_file;
    int length, i_G;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(geo_name), STR_LEN(geo_name),
        c_geo_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(CAD_name), STR_LEN(CAD_name),
        c_CAD_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    length = STR_LEN(geo_file);
    c_geo_file = CGNS_NEW(char, length+1);
    string_2_C_string(STR_PTR(geo_file), STR_LEN(geo_file),
        c_geo_file, length, ier);
    if (*ier == 0) {
        *ier = (cgint_f)cg_geo_write((int)*fn, (int)*B, (int)*F, c_geo_name,
                   c_geo_file, c_CAD_name, &i_G);
        *G = (cgint_f)i_G;
    }
    CGNS_FREE(c_geo_file);
}

CGNSDLL void FMNAME(cg_node_geo_write_f, CG_NODE_GEO_WRITE_F) (
    STR_PSTR(geo_name), STR_PSTR(geo_file), STR_PSTR(CAD_name),
    cgint_f *G, cgint_f *ier STR_PLEN(geo_name) STR_PLEN(geo_file)
    STR_PLEN(CAD_name))
{
    char c_geo_name[CGIO_MAX_NAME_LENGTH+1];
    char c_CAD_name[CGIO_MAX_NAME_LENGTH+1];
    char *c_geo_file;
    int length, i_G;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(geo_name), STR_LEN(geo_name),
        c_geo_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(CAD_name), STR_LEN(CAD_name),
        c_CAD_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    length = STR_LEN(geo_file);
    c_geo_file = CGNS_NEW(char, length+1);
    string_2_C_string(STR_PTR(geo_file), STR_LEN(geo_file),
        c_geo_file, length, ier);
    if (*ier == 0) {
        *ier = (cgint_f)cg_node_geo_write(c_geo_name,
                   c_geo_file, c_CAD_name, &i_G);
        *G = (cgint_f)i_G;
    }
    CGNS_FREE(c_geo_file);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryEntity_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_part_read_f, CG_PART_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *F,cgint_f *G, cgint_f *P, STR_PSTR(part_name),
	cgint_f *ier STR_PLEN(part_name))
{
    char c_part_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_part_read((int)*fn, (int)*B, (int)*F, (int)*G, (int)*P, c_part_name);
    if (*ier == 0)
      string_2_F_string(c_part_name, STR_PTR(part_name), STR_LEN(part_name), ier);
}

CGNSDLL void FMNAME(cg_node_part_read_f, CG_NODE_PART_READ_F) (
    cgint_f *G, cgint_f *P, STR_PSTR(part_name),
    cgint_f *ier STR_PLEN(part_name))
{
    char c_part_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_node_part_read((int)*G, (int)*P, c_part_name);
    if (*ier == 0)
      string_2_F_string(c_part_name, STR_PTR(part_name), STR_LEN(part_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_part_write_f, CG_PART_WRITE_F) (cgint_f *fn, cgint_f *B,
	cgint_f *F, cgint_f *G, STR_PSTR(part_name), cgint_f *P,
	cgint_f *ier STR_PLEN(part_name))
{
    char c_part_name[CGIO_MAX_NAME_LENGTH+1];
    int i_P;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(part_name), STR_LEN(part_name),
        c_part_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_part_write((int)*fn, (int)*B, (int)*F, (int)*G, c_part_name, &i_P);
    *P = (cgint_f)i_P;
}

CGNSDLL void FMNAME(cg_node_part_write_f, CG_NODE_PART_WRITE_F) (
    cgint_f *G, STR_PSTR(part_name), cgint_f *P,
    cgint_f *ier STR_PLEN(part_name))
{
    char c_part_name[CGIO_MAX_NAME_LENGTH+1];
    int i_P;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(part_name), STR_LEN(part_name),
        c_part_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_node_part_write((int)*G, c_part_name, &i_P);
    *P = (cgint_f)i_P;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DiscreteData_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_ndiscrete_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *ndiscrete, cgint_f *ier)
{
    int i_ndiscrete;

    *ier = (cgint_f)cg_ndiscrete((int)*fn, (int)*B, (int)*Z, &i_ndiscrete);
    *ndiscrete = (cgint_f)i_ndiscrete;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_discrete_read_f, CG_DISCRETE_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *D, STR_PSTR(discrete_name),
	cgint_f *ier STR_PLEN(discrete_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_discrete_read((int)*fn, (int)*B, (int)*Z, (int)*D, c_name);
    if (*ier == 0)
      string_2_F_string(c_name, STR_PTR(discrete_name), STR_LEN(discrete_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_discrete_write_f, CG_DISCRETE_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, STR_PSTR(discrete_name), cgint_f *D,
	cgint_f *ier STR_PLEN(discrete_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_D;

    string_2_C_string(STR_PTR(discrete_name), STR_LEN(discrete_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("    discrete_name='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_discrete_write((int)*fn, (int)*B, (int)*Z, c_name, &i_D);
    *D = (cgint_f)i_D;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_discrete_size_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *D, cgint_f *ndim,
	cgsize_t *dims, cgint_f *ier)
{
    int i_ndim;

    *ier = (cgint_f)cg_discrete_size((int)*fn, (int)*B, (int)*Z, (int)*D,
                &i_ndim, dims);
    *ndim = (cgint_f)i_ndim;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_discrete_ptset_info_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	CGNS_ENUMT(PointSetType_t) *ptype, cgsize_t *npnts, cgint_f *ier)
{
    CGNS_ENUMT(PointSetType_t) i_ptype;

    *ier = (cgint_f)cg_discrete_ptset_info((int)*fn, (int)*B, (int)*Z,
               (int)*S, &i_ptype, npnts);
    *ptype = (CGNS_ENUMT(PointSetType_t))i_ptype;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_discrete_ptset_read_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	cgsize_t *pnts, cgint_f *ier)
{
    *ier = (cgint_f)cg_discrete_ptset_read((int)*fn, (int)*B, (int)*Z,
               (int)*S, pnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_discrete_ptset_write_f, CG_DISCRETE_PTSET_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(name),
	CGNS_ENUMT(GridLocation_t) *location, CGNS_ENUMT(PointSetType_t) *ptype, cgsize_t *npnts,
	cgsize_t *pnts, cgint_f *D, cgint_f *ier STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_D;

    string_2_C_string(STR_PTR(name), STR_LEN(name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cg_discrete_ptset_write((int)*fn, (int)*B, (int)*Z, c_name,
               (CGNS_ENUMT(GridLocation_t))*location,
               (CGNS_ENUMT(PointSetType_t))*ptype, *npnts, pnts, &i_D);

    *D = (cgint_f)i_D;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t/DataArray_t Nodes               *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_ncoords_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *ncoords, cgint_f *ier)
{
    int i_ncoords;

    *ier = (cgint_f)cg_ncoords((int)*fn, (int)*B, (int)*Z, &i_ncoords);
    *ncoords = (cgint_f)i_ncoords;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_info_f, CG_COORD_INFO_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *C, CGNS_ENUMT(DataType_t) *type, STR_PSTR(coordname),
	cgint_f *ier STR_PLEN(coordname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(DataType_t) i_type;

    *ier = (cgint_f)cg_coord_info((int)*fn, (int)*B, (int)*Z, (int)*C, &i_type, c_name);
    if (*ier) return;
    *type = (CGNS_ENUMT(DataType_t))i_type;
    string_2_F_string(c_name, STR_PTR(coordname), STR_LEN(coordname), ier);

}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_read_f, CG_COORD_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, STR_PSTR(coordname), CGNS_ENUMT(DataType_t) *type, cgsize_t *rmin,
	cgsize_t *rmax, void *coord, cgint_f *ier STR_PLEN(coordname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("coordname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_coord_read((int)*fn, (int)*B, (int)*Z, c_name,
               *type, rmin, rmax, coord);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_general_read_f, CG_COORD_GENERAL_READ_F) (
        cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(coordname),
        cgsize_t *s_rmin, cgsize_t *s_rmax, CGNS_ENUMT(DataType_t) *m_type,
        cgint_f *m_numdim, cgsize_t *m_dimvals,
        cgsize_t *m_rmin, cgsize_t *m_rmax, void *coord,
        cgint_f *ier STR_PLEN(coordname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("coordname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_coord_general_read(
        (int)*fn, (int)*B, (int)*Z, c_name,
        s_rmin, s_rmax,
        *m_type, (int)*m_numdim, m_dimvals, m_rmin, m_rmax, coord);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_coord_id_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *C, double *coord_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_coord_id((int)*fn, (int)*B, (int)*Z, (int)*C, coord_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_write_f, CG_COORD_WRITE_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, CGNS_ENUMT(DataType_t) *type, STR_PSTR(coordname), void *coord, cgint_f *C,
	cgint_f *ier STR_PLEN(coordname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_C;

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("    coordname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_coord_write((int)*fn, (int)*B, (int)*Z,
               (CGNS_ENUMT(DataType_t))*type, c_name, coord, &i_C);
    *C = (cgint_f)i_C;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_partial_write_f, CG_COORD_PARTIAL_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, CGNS_ENUMT(DataType_t) *type, STR_PSTR(coordname),
	cgsize_t *rmin, cgsize_t *rmax, void *coord, cgint_f *C,
	cgint_f *ier STR_PLEN(coordname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_C;

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("    coordname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_coord_partial_write((int)*fn, (int)*B, (int)*Z,
               (CGNS_ENUMT(DataType_t))*type, c_name, rmin, rmax,
               coord, &i_C);
    *C = (cgint_f)i_C;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_general_write_f, CG_COORD_GENERAL_WRITE_F) (
        cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(coordname),
        CGNS_ENUMT(DataType_t) *s_type, cgsize_t *s_rmin, cgsize_t *s_rmax,
        CGNS_ENUMT(DataType_t) *m_type, cgint_f *m_numdim, cgsize_t *m_dims,
        cgsize_t *m_rmin, cgsize_t *m_rmax, void *coord, cgint_f *C,
        cgint_f *ier STR_PLEN(coordname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_C;

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("    coordname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_coord_general_write((int)*fn, (int)*B, (int)*Z,
                                           c_name, *s_type, s_rmin, s_rmax,
                                           *m_type, (int)*m_numdim, m_dims,
                                           m_rmin, m_rmax, coord, &i_C);
    *C = (cgint_f)i_C;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Elements_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nsections_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *nsections, cgint_f *ier)
{
    int i_nsections;

    *ier = (cgint_f)cg_nsections((int)*fn, (int)*B, (int)*Z, &i_nsections);
    *nsections = (cgint_f)i_nsections;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_section_read_f, CG_SECTION_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *E, STR_PSTR(section_name),
	CGNS_ENUMT(ElementType_t) *type, cgsize_t *start, cgsize_t *end, cgint_f *nbndry,
	cgint_f *parent_flag, cgint_f *ier STR_PLEN(section_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(ElementType_t) i_type;
    int i_nbndry, i_parent_flag;

    *ier = (cgint_f)cg_section_read((int)*fn, (int)*B, (int)*Z, (int)*E, c_name,
               &i_type, start, end, &i_nbndry, &i_parent_flag);
    if (*ier) return;
    *type = (CGNS_ENUMT(ElementType_t))i_type;
    *nbndry = (cgint_f)i_nbndry;
    *parent_flag = (cgint_f)i_parent_flag;
    string_2_F_string(c_name, STR_PTR(section_name), STR_LEN(section_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elements_read_f, CG_ELEMENTS_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *E, cgsize_t *elements,
	cgsize_t *parent_data, cgint_f *ier)
{
    *ier = (cgint_f)cg_elements_read((int)*fn, (int)*B, (int)*Z, (int)*E,
               elements, parent_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_poly_elements_read_f, CG_POLY_ELEMENTS_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *E, cgsize_t *elements,
	cgsize_t *connect_offset, cgsize_t *parent_data, cgint_f *ier)
{
    *ier = (cgint_f)cg_poly_elements_read((int)*fn, (int)*B, (int)*Z, (int)*E,
               elements, connect_offset, parent_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_elementdatasize_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *E, cgsize_t *ElementDataSize,
	cgint_f *ier)
{
    *ier = (cgint_f)cg_ElementDataSize((int)*fn, (int)*B, (int)*Z, (int)*E,
               ElementDataSize);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_elementpartialsize_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *E, cgsize_t *start, cgsize_t *end,
	cgsize_t *ElementDataSize, cgint_f *ier)
{
    *ier = (cgint_f)cg_ElementPartialSize((int)*fn, (int)*B, (int)*Z, (int)*E,
               *start, *end, ElementDataSize);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_section_write_f, CG_SECTION_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, STR_PSTR(section_name), CGNS_ENUMT(ElementType_t)*type,
	cgsize_t *start, cgsize_t *end, cgint_f *nbndry, cgsize_t *elements,
	cgint_f *S, cgint_f *ier STR_PLEN(section_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_S;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(section_name), STR_LEN(section_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cg_section_write((int)*fn, (int)*B, (int)*Z, c_name,
				     *type, *start, *end,
				     (int)*nbndry, elements, &i_S);
    *S = (cgint_f)i_S;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_poly_section_write_f, CG_POLY_SECTION_WRITE_F)
	(cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(section_name),
	CGNS_ENUMT(ElementType_t)*type,	cgsize_t *start, cgsize_t *end,
	cgint_f *nbndry, cgsize_t *elements, cgsize_t *connect_offset,
	cgint_f *S, cgint_f *ier STR_PLEN(section_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_S;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(section_name), STR_LEN(section_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cg_poly_section_write((int)*fn, (int)*B, (int)*Z, c_name,
		     *type, *start, *end,
		     (int)*nbndry, elements, connect_offset, &i_S);
    *S = (cgint_f)i_S;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_section_general_write_f, CG_SECTION_GENERAL_WRITE_F)
	(cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(section_name),
	CGNS_ENUMT(ElementType_t)*type,	CGNS_ENUMT(DataType_t)*  elementDataType,
	cgsize_t *start, cgsize_t *end, cgsize_t *elementDataSize,
	cgint_f *nbndry, cgint_f *S, cgint_f *ier STR_PLEN(section_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_S;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(section_name), STR_LEN(section_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cg_section_general_write((int)*fn, (int)*B, (int)*Z, c_name,
		     *type, *elementDataType, *start, *end, *elementDataSize,
		     (int)*nbndry, &i_S);
    *S = (cgint_f)i_S;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_section_initialize_f, CG_SECTION_INITIALIZE_F)
	(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgint_f *ier)
{

    *ier = (cgint_f)cg_section_initialize((int)*fn, (int)*B, (int)*Z, (int) *S);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void  FMNAME(cg_parent_data_write_f, CG_PARENT_DATA_WRITE_F )(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *parent_data, cgint_f *ier)
{
    *ier = (cgint_f)cg_parent_data_write((int)*fn, (int)*B, (int)*Z, (int)*S, parent_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_section_partial_write_f, CG_SECTION_PARTIAL_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(section_name),
	CGNS_ENUMT(ElementType_t)*type, cgsize_t *start, cgsize_t *end, cgint_f *nbndry,
	cgint_f *S, cgint_f *ier STR_PLEN(section_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_S;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(section_name), STR_LEN(section_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_section_partial_write((int)*fn, (int)*B, (int)*Z, c_name,
               *type, *start,
               *end, (int)*nbndry, &i_S);
    *S = (cgint_f)i_S;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elements_partial_write_f, CG_ELEMENTS_PARTIAL_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, cgsize_t *elements, cgint_f *ier)
{
    *ier = (cgint_f)cg_elements_partial_write((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, elements);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elements_general_write_f, CG_ELEMENTS_GENERAL_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, CGNS_ENUMT(DataType_t) *mtype, void *elements, cgint_f *ier)
{

    *ier = (cgint_f)cg_elements_general_write((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, *mtype, elements);
}


/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_poly_elements_partial_write_f, CG_POLY_ELEMENTS_PARTIAL_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, cgsize_t *elements, cgsize_t *connect_offset, cgint_f *ier)
{
    *ier = (cgint_f)cg_poly_elements_partial_write((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, elements, connect_offset);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_poly_elements_general_write_f, CG_POLY_ELEMENTS_GENERAL_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, CGNS_ENUMT(DataType_t) *mtype,
	void *elements, void *connect_offset, cgint_f *ier)
{
    *ier = (cgint_f)cg_poly_elements_general_write((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, *mtype, elements, connect_offset);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_parent_data_partial_write_f, CG_PARENT_DATA_PARTIAL_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, cgsize_t *parent_data, cgint_f *ier)
{
    *ier = (cgint_f)cg_parent_data_partial_write((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, parent_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elements_partial_read_f, CG_ELEMENTS_PARTIAL_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, cgsize_t *elements, cgsize_t *parent, cgint_f *ier)
{
    *ier = (cgint_f)cg_elements_partial_read((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, elements, parent);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_poly_elements_partial_read_f, CG_POLY_ELEMENTS_PARTIAL_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, cgsize_t *elements, cgsize_t *connect_offset, cgsize_t *parent, cgint_f *ier)
{
    *ier = (cgint_f)cg_poly_elements_partial_read((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, elements, connect_offset, parent);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elements_general_read_f, CG_ELEMENTS_GENERAL_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, CGNS_ENUMT(DataType_t) *mtype, void *elements, cgint_f *ier)
{
    *ier = (cgint_f)cg_elements_general_read((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, *mtype, elements);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_poly_elements_general_read_f, CG_POLY_ELEMENTS_GENERAL_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, CGNS_ENUMT(DataType_t) *mtype, void *elements,
	void *connect_offset, cgint_f *ier)
{
    *ier = (cgint_f)cg_poly_elements_general_read((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, *mtype, elements, connect_offset);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_parent_elements_general_read_f, CG_PARENT_ELEMENTS_GENERAL_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, CGNS_ENUMT(DataType_t) *mtype, void *parelem, cgint_f *ier)
{
    *ier = (cgint_f)cg_parent_elements_general_read((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, *mtype, parelem);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_parent_elements_position_general_read_f, CG_PARENT_ELEMENTS_POSITION_GENERAL_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
	cgsize_t *rmax, CGNS_ENUMT(DataType_t) *mtype, void *parface, cgint_f *ier)
{
    *ier = (cgint_f)cg_parent_elements_position_general_read((int)*fn, (int)*B, (int)*Z, (int)*S,
               *rmin, *rmax, *mtype, parface);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FlowSolution_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nsols_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *nsols, cgint_f *ier)
{
    int i_nsols;

    *ier = (cgint_f)cg_nsols((int)*fn, (int)*B, (int)*Z, &i_nsols);
    *nsols = (cgint_f)i_nsols;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_sol_info_f, CG_SOL_INFO_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *S, STR_PSTR(solname), CGNS_ENUMT(GridLocation_t) *location,
	cgint_f *ier STR_PLEN(solname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(GridLocation_t) i_location;

    *ier = (cgint_f)cg_sol_info((int)*fn, (int)*B, (int)*Z, (int)*S, c_name, &i_location);
    if (*ier) return;
    *location = (cgint_f)i_location;
    string_2_F_string(c_name, STR_PTR(solname), STR_LEN(solname), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_sol_id_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *S, double *sol_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_sol_id((int)*fn, (int)*B, (int)*Z, (int)*S, sol_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_sol_write_f, CG_SOL_WRITE_F)(cgint_f *fn, cgint_f *B,
	cgint_f *Z, STR_PSTR(solname), CGNS_ENUMT(GridLocation_t)*location, cgint_f *S,
	cgint_f *ier STR_PLEN(solname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_S;

    string_2_C_string(STR_PTR(solname), STR_LEN(solname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("\n    solname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_sol_write((int)*fn, (int)*B, (int)*Z, c_name,
               *location, &i_S);
    *S = (cgint_f)i_S;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_sol_size_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *S, cgint_f *ndim,
	cgsize_t *dims, cgint_f *ier)
{
    int i_ndim;

    *ier = (cgint_f)cg_sol_size((int)*fn, (int)*B, (int)*Z, (int)*S,
                &i_ndim, dims);
    *ndim = (cgint_f)i_ndim;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_sol_ptset_info_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	CGNS_ENUMT(PointSetType_t) *ptype, cgsize_t *npnts, cgint_f *ier)
{
    CGNS_ENUMT(PointSetType_t) i_ptype;

    *ier = (cgint_f)cg_sol_ptset_info((int)*fn, (int)*B, (int)*Z,
               (int)*S, &i_ptype, npnts);
    *ptype = i_ptype;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_sol_ptset_read_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	cgsize_t *pnts, cgint_f *ier)
{
    *ier = (cgint_f)cg_sol_ptset_read((int)*fn, (int)*B, (int)*Z,
               (int)*S, pnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_sol_ptset_write_f, CG_SOL_PTSET_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(name),
	CGNS_ENUMT(GridLocation_t) *location, CGNS_ENUMT(PointSetType_t) *ptype, cgsize_t *npnts,
	cgsize_t *pnts, cgint_f *S, cgint_f *ier STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_S;

    string_2_C_string(STR_PTR(name), STR_LEN(name),
		      c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cg_sol_ptset_write((int)*fn, (int)*B, (int)*Z, c_name,
               *location,
               *ptype, *npnts, pnts, &i_S);
    *S = (cgint_f)i_S;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write solution DataArray_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nfields_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *S, cgint_f *nfields, cgint_f *ier)
{
    int i_nfields;

    *ier = (cgint_f)cg_nfields((int)*fn, (int)*B, (int)*Z, (int)*S, &i_nfields);
    *nfields = (cgint_f)i_nfields;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_info_f, CG_FIELD_INFO_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *S, cgint_f *F, CGNS_ENUMT(DataType_t) *type, STR_PSTR(fieldname),
	cgint_f *ier STR_PLEN(fieldname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(DataType_t) i_type;

    *ier = (cgint_f)cg_field_info((int)*fn, (int)*B, (int)*Z, (int)*S, (int)*F,
               &i_type, c_name);
    if (*ier) return;
    *type = i_type;
    string_2_F_string(c_name, STR_PTR(fieldname), STR_LEN(fieldname), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_read_f, CG_FIELD_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *S, STR_PSTR(fieldname), CGNS_ENUMT(DataType_t) *type, cgsize_t *rmin,
	cgsize_t *rmax, void *field_ptr, cgint_f *ier STR_PLEN(fieldname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
		      c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("fieldname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_field_read((int)*fn, (int)*B, (int)*Z, (int)*S, c_name,
				  *type, rmin, rmax, field_ptr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_general_read_f, CG_FIELD_GENERAL_READ_F) (
        cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, STR_PSTR(fieldname),
        cgsize_t *s_rmin, cgsize_t *s_rmax, CGNS_ENUMT(DataType_t) *m_type,
        cgint_f *m_numdim, cgsize_t *m_dimvals,
        cgsize_t *m_rmin, cgsize_t *m_rmax, void *field_ptr,
        cgint_f *ier STR_PLEN(fieldname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
		      c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("fieldname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_field_general_read(
        (int)*fn, (int)*B, (int)*Z, (int)*S, c_name,
        s_rmin, s_rmax,
        *m_type, (int)*m_numdim, m_dimvals, m_rmin, m_rmax, field_ptr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_field_id_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *S, cgint_f *F, double *field_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_field_id((int)*fn, (int)*B, (int)*Z, (int)*S, (int)*F, field_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_write_f, CG_FIELD_WRITE_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *S, CGNS_ENUMT(DataType_t) *type, STR_PSTR(fieldname), void *field_ptr,
	cgint_f *F, cgint_f *ier STR_PLEN(fieldname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_F;

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
		      c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("      fieldname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_field_write((int)*fn, (int)*B, (int)*Z, (int)*S,
               *type, c_name, field_ptr, &i_F);
    *F = (cgint_f)i_F;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_partial_write_f, CG_FIELD_PARTIAL_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *S, CGNS_ENUMT(DataType_t) *type, STR_PSTR(fieldname),
	cgsize_t *rmin, cgsize_t *rmax, void *field_ptr, cgint_f *F,
	cgint_f *ier STR_PLEN(fieldname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_F;

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("      fieldname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_field_partial_write((int)*fn, (int)*B, (int)*Z, (int)*S,
					   *type, c_name,
					   rmin, rmax, field_ptr, &i_F);
    *F = (cgint_f)i_F;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_general_write_f, CG_FIELD_GENERAL_WRITE_F) (
        cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, STR_PSTR(fieldname),
        CGNS_ENUMT(DataType_t) *s_type, cgsize_t *s_rmin, cgsize_t *s_rmax,
        CGNS_ENUMT(DataType_t) *m_type, cgint_f *m_numdim, cgsize_t *m_dims,
	cgsize_t *m_rmin, cgsize_t *m_rmax, void *field_ptr, cgint_f *F,
	cgint_f *ier STR_PLEN(fieldname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_F;

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("      fieldname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_field_general_write((int)*fn, (int)*B, (int)*Z, (int)*S,
                                           c_name, *s_type, s_rmin, s_rmax,
                                           *m_type, (int)*m_numdim, m_dims,
                                           m_rmin, m_rmax, field_ptr, &i_F);
    *F = (cgint_f)i_F;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ZoneSubRegion_t Nodes  			         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nsubregs_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *nsubreg, cgint_f *ier)
{
    int i_nsub;

    *ier = (cgint_f)cg_nsubregs((int)*fn, (int)*B, (int)*Z, &i_nsub);
    *nsubreg = (cgint_f)i_nsub;
}

CGNSDLL void FMNAME(cg_subreg_info_f, CG_SUBREG_INFO_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *S, STR_PSTR(regname),
	cgint_f *dimension, CGNS_ENUMT(GridLocation_t) *location,
	CGNS_ENUMT(PointSetType_t) *ptset_type,
	cgsize_t *npnts, cgint_f *bcname_len, cgint_f *gcname_len,
	cgint_f *ier STR_PLEN(regname))
{
    char c_regname[CGIO_MAX_NAME_LENGTH+1];
    int i_dimension, i_bcname_len, i_gcname_len;
    CGNS_ENUMT(GridLocation_t) i_location;
    CGNS_ENUMT(PointSetType_t) i_ptset_type;

    *ier = (cgint_f)cg_subreg_info((int)*fn, (int)*B, (int)*Z, (int)*S, c_regname,
               &i_dimension, &i_location, &i_ptset_type, npnts,
               &i_bcname_len, &i_gcname_len);
    if (*ier) return;
    string_2_F_string(c_regname, STR_PTR(regname), STR_LEN(regname), ier);
    *dimension = (cgint_f)i_dimension;
    *location = i_location;
    *ptset_type = i_ptset_type;
    *bcname_len = (cgint_f)i_bcname_len;
    *gcname_len = (cgint_f)i_gcname_len;
}

CGNSDLL void cg_subreg_ptset_read_f(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	cgsize_t *pnts, cgint_f *ier)
{
    *ier = (cgint_f)cg_subreg_ptset_read((int)*fn, (int)*B, (int)*Z, (int)*S, pnts);
}

CGNSDLL void FMNAME(cg_subreg_bcname_read_f, CG_SUBREG_BCNAME_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	STR_PSTR(bcname), cgint_f *ier STR_PLEN(bcname))
{
    char *name = 0;
    char regname[CGIO_MAX_NAME_LENGTH+1];
    int dimension, bclen, gclen;
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(PointSetType_t) ptset_type;
    cgsize_t npnts;

    *ier = (cgint_f)cg_subreg_info((int)*fn, (int)*B, (int)*Z, (int)*S, regname,
               &dimension, &location, &ptset_type, &npnts, &bclen, &gclen);
    if (*ier) return;
    if (bclen) name = CGNS_NEW(char, bclen+1);
    *ier = (cgint_f)cg_subreg_bcname_read((int)*fn, (int)*B, (int)*Z, (int)*S, name);
    if (!*ier && name)
        string_2_F_string(name, STR_PTR(bcname), STR_LEN(bcname), ier);
    CGNS_FREE(name);
}

CGNSDLL void FMNAME(cg_subreg_gcname_read_f, CG_SUBREG_GCNAME_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	STR_PSTR(gcname), cgint_f *ier STR_PLEN(gcname))
{
    char *name = 0;
    char regname[CGIO_MAX_NAME_LENGTH+1];
    int dimension, bclen, gclen;
    CGNS_ENUMT(GridLocation_t) location;
    CGNS_ENUMT(PointSetType_t) ptset_type;
    cgsize_t npnts;

    *ier = (cgint_f)cg_subreg_info((int)*fn, (int)*B, (int)*Z, (int)*S, regname,
               &dimension, &location, &ptset_type, &npnts, &bclen, &gclen);
    if (*ier) return;
    if (gclen) name = CGNS_NEW(char, gclen+1);
    *ier = (cgint_f)cg_subreg_gcname_read((int)*fn, (int)*B, (int)*Z, (int)*S, name);
    if (!*ier && name)
        string_2_F_string(name, STR_PTR(gcname), STR_LEN(gcname), ier);
    CGNS_FREE(name);
}

CGNSDLL void FMNAME(cg_subreg_ptset_write_f, CG_SUBREG_PTSET_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(regname),
	cgint_f *dimension, CGNS_ENUMT(GridLocation_t) *location, CGNS_ENUMT(PointSetType_t) *ptset_type,
	cgsize_t *npnts, cgsize_t *pnts, cgint_f *S,
	cgint_f *ier STR_PLEN(regname))
{
    char c_regname[CGIO_MAX_NAME_LENGTH+1];
    int i_S;

    string_2_C_string(STR_PTR(regname), STR_LEN(regname),
        c_regname, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cg_subreg_ptset_write((int)*fn, (int)*B, (int)*Z, c_regname,
	       (int)*dimension, *location,
	       *ptset_type, *npnts,
	       pnts, &i_S);
    *S = (cgint_f)i_S;
}

CGNSDLL void FMNAME(cg_subreg_bcname_write_f, CG_SUBREG_BCNAME_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(regname),
	cgint_f *dimension, STR_PSTR(bcname), cgint_f *S,
	cgint_f *ier STR_PLEN(regname) STR_PLEN(bcname))
{
    char c_regname[CGIO_MAX_NAME_LENGTH+1];
    char *name;
    int length, i_S;

    string_2_C_string(STR_PTR(regname), STR_LEN(regname),
        c_regname, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    length = (int)STR_LEN(bcname);
    name = CGNS_NEW(char, length+1);
    string_2_C_string(STR_PTR(bcname), STR_LEN(bcname), name, length, ier);
    if (!*ier) {
        *ier = (cgint_f)cg_subreg_bcname_write((int)*fn, (int)*B, (int)*Z, c_regname,
	           (int)*dimension, name, &i_S);
	*S = (cgint_f)i_S;
    }
    CGNS_FREE(name);
}

CGNSDLL void FMNAME(cg_subreg_gcname_write_f, CG_SUBREG_GCNAME_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(regname),
	cgint_f *dimension, STR_PSTR(gcname), cgint_f *S,
	cgint_f *ier STR_PLEN(regname) STR_PLEN(gcname))
{
    char c_regname[CGIO_MAX_NAME_LENGTH+1];
    char *name;
    int length, i_S;

    string_2_C_string(STR_PTR(regname), STR_LEN(regname),
        c_regname, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    length = (int)STR_LEN(gcname);
    name = CGNS_NEW(char, length+1);
    string_2_C_string(STR_PTR(gcname), STR_LEN(gcname), name, length, ier);
    if (!*ier) {
        *ier = (cgint_f)cg_subreg_gcname_write((int)*fn, (int)*B, (int)*Z, c_regname,
	           (int)*dimension, name, &i_S);
	*S = (cgint_f)i_S;
    }
    CGNS_FREE(name);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ZoneGridConnectivity_t Nodes  			 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nzconns_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *nzconns, cgint_f *ier)
{
    int i_nzconns;

    *ier = (cgint_f)cg_nzconns((int)*fn, (int)*B, (int)*Z, &i_nzconns);
    *nzconns = (cgint_f)i_nzconns;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_zconn_read_f, CG_ZCONN_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *C, STR_PSTR(name),
	cgint_f *ier STR_PLEN(name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_zconn_read((int)*fn, (int)*B, (int)*Z, (int)*C, c_name);
    if (!*ier)
      string_2_F_string(c_name, STR_PTR(name), STR_LEN(name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_zconn_write_f, CG_ZCONN_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, STR_PSTR(name), cgint_f *C,
	cgint_f *ier STR_PLEN(name))
{
    int i_C;
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(name), STR_LEN(name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_zconn_write((int)*fn, (int)*B, (int)*Z, c_name, &i_C);
    *C = (cgint_f)i_C;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_zconn_get_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *C, cgint_f *ier)
{
    int i_C;

    *ier = (cgint_f)cg_zconn_get((int)*fn, (int)*B, (int)*Z, &i_C);
    *C = (cgint_f)i_C;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_zconn_set_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *C, cgint_f *ier)
{
    *ier = (cgint_f)cg_zconn_set((int)*fn, (int)*B, (int)*Z, (int)*C);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write OversetHoles_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nholes_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *nholes, cgint_f *ier)
{
    int i_nholes;

    *ier = (cgint_f)cg_nholes((int)*fn, (int)*B, (int)*Z, &i_nholes);
    *nholes = (cgint_f)i_nholes;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_hole_info_f, CG_HOLE_INFO_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *I, STR_PSTR(holename), CGNS_ENUMT(GridLocation_t) *location,
	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *nptsets, cgsize_t *npnts,
	cgint_f *ier STR_PLEN(holename))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(GridLocation_t) i_location;
    CGNS_ENUMT(PointSetType_t) i_ptset_type;
    int i_nptsets;

    *ier = (cgint_f)cg_hole_info((int)*fn, (int)*B, (int)*Z, (int)*I, c_name,
               &i_location, &i_ptset_type, &i_nptsets, npnts);
    if (*ier) return;
    *location = i_location;
    *ptset_type = i_ptset_type;
    *nptsets = i_nptsets;
    string_2_F_string(c_name, STR_PTR(holename), STR_LEN(holename), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_hole_read_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *I, cgsize_t *pnts, cgint_f *ier)
{
    *ier = (cgint_f)cg_hole_read((int)*fn, (int)*B, (int)*Z, (int)*I, pnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_hole_id_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *I, double *hole_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_hole_id((int)*fn, (int)*B, (int)*Z, (int)*I, hole_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_hole_write_f, CG_HOLE_WRITE_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, STR_PSTR(holename), CGNS_ENUMT(GridLocation_t) *location,
	CGNS_ENUMT(PointSetType_t) *ptset_type, cgint_f *nptsets, cgsize_t *npnts,
	cgsize_t *pnts, cgint_f *I, cgint_f *ier STR_PLEN(holename))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_I;

    string_2_C_string(STR_PTR(holename), STR_LEN(holename),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("holename='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_hole_write((int)*fn, (int)*B, (int)*Z, c_name,
               *location,
               *ptset_type,
               (int)*nptsets, *npnts, pnts, &i_I);
    *I = (cgint_f)i_I;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity_t Nodes                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nconns_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *nconns, cgint_f *ier)
{
    int i_nconns;

    *ier = (cgint_f)cg_nconns((int)*fn, (int)*B, (int)*Z, &i_nconns);
    *nconns = (cgint_f)i_nconns;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_info_f, CG_CONN_INFO_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *I, STR_PSTR(connectname), CGNS_ENUMT(GridLocation_t) *location,
	CGNS_ENUMT(GridConnectivityType_t) *type, CGNS_ENUMT(PointSetType_t) *ptset_type,
	cgsize_t *npnts, STR_PSTR(donorname),
	CGNS_ENUMT(ZoneType_t) *donor_zonetype,
	CGNS_ENUMT(PointSetType_t) *donor_ptset_type,
	CGNS_ENUMT(DataType_t) *donor_datatype,
	cgsize_t *ndata_donor, cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
{
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(GridLocation_t) i_location;
    CGNS_ENUMT(GridConnectivityType_t) i_type;
    CGNS_ENUMT(PointSetType_t) i_ptset_type;
    CGNS_ENUMT(ZoneType_t) i_donor_zonetype;
    CGNS_ENUMT(PointSetType_t) i_donor_ptset_type;
    CGNS_ENUMT(DataType_t) i_donor_datatype;

    *ier = (cgint_f)cg_conn_info((int)*fn, (int)*B, (int)*Z, (int)*I, cc_name, &i_location,
               &i_type, &i_ptset_type, npnts, dc_name,
               &i_donor_zonetype, &i_donor_ptset_type,
               &i_donor_datatype, ndata_donor);
    if (*ier) return;
    string_2_F_string(cc_name, STR_PTR(connectname), STR_LEN(connectname), ier);
    if (*ier) return;
    string_2_F_string(dc_name, STR_PTR(donorname), STR_LEN(donorname), ier);
    if (*ier) return;
    *location = i_location;
    *type = i_type;
    *ptset_type = i_ptset_type;
    *donor_zonetype = i_donor_zonetype;
    *donor_ptset_type = i_donor_ptset_type;
    *donor_datatype = i_donor_datatype;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_conn_read_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *I, cgsize_t *pnts, CGNS_ENUMT(DataType_t) *donor_datatype,
	cgsize_t *donor_data, cgint_f *ier)
{
    *ier = (cgint_f)cg_conn_read((int)*fn, (int)*B, (int)*Z, (int)*I, pnts,
               *donor_datatype, donor_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_conn_read_short_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *I, cgsize_t *pnts, cgint_f *ier)
{
    *ier = (cgint_f)cg_conn_read_short((int)*fn, (int)*B, (int)*Z, (int)*I, pnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_conn_id_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *I, double *conn_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_conn_id((int)*fn, (int)*B, (int)*Z, (int)*I, conn_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_write_f, CG_CONN_WRITE_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, STR_PSTR(connectname),
        CGNS_ENUMT(GridLocation_t) *location,
        CGNS_ENUMT(GridConnectivityType_t) *type,
	CGNS_ENUMT(PointSetType_t) *ptset_type,
	cgsize_t *npnts, cgsize_t *pnts,
	STR_PSTR(donorname),
        CGNS_ENUMT(ZoneType_t)*donor_zonetype,
        CGNS_ENUMT(PointSetType_t)*donor_ptset_type,
	CGNS_ENUMT(DataType_t)*donor_datatype, cgsize_t *ndata_donor, cgsize_t *donor_data,
	cgint_f *I, cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
{
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];
    int i_I;

    string_2_C_string(STR_PTR(connectname), STR_LEN(connectname),
        cc_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(donorname), STR_LEN(donorname),
        dc_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("connectname='%s'\n", cc_name);
    printf("donorname='%s'\n", dc_name);
#endif
    *ier = (cgint_f)cg_conn_write((int)*fn, (int)*B, (int)*Z, cc_name,
               *location,
               *type,
               *ptset_type,
               *npnts, pnts, dc_name,
               *donor_zonetype,
               *donor_ptset_type,
               *donor_datatype,
               *ndata_donor, donor_data, &i_I);
    *I = (cgint_f)i_I;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_write_short_f, CG_CONN_WRITE_SHORT_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, STR_PSTR(connectname), CGNS_ENUMT(GridLocation_t) *location,
	CGNS_ENUMT(GridConnectivityType_t) *type, CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts,
	cgsize_t *pnts, STR_PSTR(donorname), cgint_f *I,
	cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
{
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];
    int i_I;

    string_2_C_string(STR_PTR(connectname), STR_LEN(connectname),
        cc_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(donorname), STR_LEN(donorname),
        dc_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("connectname='%s'\n", cc_name);
    printf("donorname='%s'\n", dc_name);
#endif
    *ier = (cgint_f)cg_conn_write_short((int)*fn, (int)*B, (int)*Z, cc_name,
					*location,
					*type,
					*ptset_type,
					*npnts, pnts, dc_name, &i_I);
    *I = (cgint_f)i_I;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity1to1_t Nodes in a zone            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_n1to1_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f*n1to1, cgint_f *ier)
{
    int i_n1to1;

    *ier = (cgint_f)cg_n1to1((int)*fn, (int)*B, (int)*Z, &i_n1to1);
    *n1to1 = (cgint_f)i_n1to1;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_read_f, CG_1TO1_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *I, STR_PSTR(connectname), STR_PSTR(donorname),
	cgsize_t *range, cgsize_t *donor_range, cgint_f *transform,
	cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
{
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];
    int n, index_dim, i_transform[3];

    *ier = (cgint_f)cg_index_dim((int)*fn, (int)*B, (int)*Z, &index_dim);
    if (*ier) return;
    *ier = (cgint_f)cg_1to1_read((int)*fn, (int)*B, (int)*Z, (int)*I, cc_name, dc_name,
               range, donor_range, i_transform);
    if (*ier) return;
    string_2_F_string(cc_name, STR_PTR(connectname), STR_LEN(connectname), ier);
    if (*ier) return;
    string_2_F_string(dc_name, STR_PTR(donorname), STR_LEN(donorname), ier);
    if (*ier) return;
    for (n = 0; n < index_dim; n++)
        transform[n] = (cgint_f)i_transform[n];
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_1to1_id_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *I, double *one21_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_1to1_id((int)*fn, (int)*B, (int)*Z, (int)*I, one21_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_write_f, CG_1TO1_WRITE_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, STR_PSTR(connectname), STR_PSTR(donorname), cgsize_t *range,
	cgsize_t *donor_range, cgint_f *transform, cgint_f *I,
	cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
{
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];
    int n, index_dim, i_I, i_transform[3];

    *ier = (cgint_f)cg_index_dim((int)*fn, (int)*B, (int)*Z, &index_dim);
    if (*ier) return;
    string_2_C_string(STR_PTR(connectname), STR_LEN(connectname),
        cc_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(donorname), STR_LEN(donorname),
        dc_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("connectname='%s'\n", cc_name);
    printf("donorname='%s'\n", dc_name);
#endif
    for (n = 0; n < index_dim; n++)
        i_transform[n] = (int)transform[n];
    *ier = (cgint_f)cg_1to1_write((int)*fn, (int)*B, (int)*Z, cc_name, dc_name, range,
               donor_range, i_transform, &i_I);
    *I = (cgint_f)i_I;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read all GridConnectivity1to1_t Nodes of a base                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_n1to1_global_f(cgint_f *fn,
	cgint_f *B, cgint_f *n1to1_global, cgint_f *ier)
{
    int i_n1to1_global;

    *ier = (cgint_f)cg_n1to1_global((int)*fn, (int)*B, &i_n1to1_global);
    *n1to1_global = (cgint_f)i_n1to1_global;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_read_global_f, CG_1TO1_READ_GLOBAL_F) (cgint_f *fn,
	cgint_f *B, STR_PSTR(connectname), STR_PSTR(zonename), STR_PSTR(donorname),
	cgsize_t *range, cgsize_t *donor_range, cgint_f *transform,
	cgint_f *ier STR_PLEN(connectname) STR_PLEN(zonename) STR_PLEN(donorname))
{
    int n, i, step, len;
    int cell_dim, phys_dim;     /* number of dimension for model    */
    int Ndim;           /* indexDimension           */
    int Nglobal;            /* number of 1to1 interface in base     */
    char **c_connectname, **c_zonename, **c_donorname;
    char basename[CGIO_MAX_NAME_LENGTH+1];
    cgsize_t **c_range, **c_donor_range;
    int **c_transform;

     /* get number of dimension for model: Ndim */
    *ier = (cgint_f)cg_base_read((int)*fn, (int)*B, basename, &cell_dim, &phys_dim);
    if (*ier) return;

     /* For structured grid: */
    Ndim = cell_dim;

     /* get number of 1to1 interface in base:  Nglobal */
    *ier = (cgint_f)cg_n1to1_global((int)*fn, (int)*B, &Nglobal);
    if (*ier) return;
    if (Nglobal < 1) {
        cgi_error("Number of interface must equal 1 or more");
        *ier = 1;
        return;
    }
     /* allocate memory for C-arrays (ptr-to-ptr) */
    if ((c_connectname = (char **)malloc(Nglobal*sizeof(char *)))==NULL ||
        (c_zonename    = (char **)malloc(Nglobal*sizeof(char *)))==NULL ||
        (c_donorname   = (char **)malloc(Nglobal*sizeof(char *)))==NULL ||
        (c_range       = (cgsize_t **)malloc(Nglobal*sizeof(cgsize_t *)))==NULL ||
        (c_donor_range = (cgsize_t **)malloc(Nglobal*sizeof(cgsize_t *)))==NULL ||
        (c_transform   = (int **)malloc(Nglobal*sizeof(int *)))==NULL) {
        cgi_error("Error allocating memory...");
        *ier = 1;
	goto cleanup;
    }
    len = CGIO_MAX_NAME_LENGTH+1;
    for (n = 0; n < Nglobal; n++) {
        if ((c_connectname[n] = (char *)malloc(len*sizeof(char)))==NULL ||
            (c_zonename[n]    = (char *)malloc(len*sizeof(char)))==NULL ||
            (c_donorname[n]   = (char *)malloc(len*sizeof(char)))==NULL ||
            (c_range[n]       = (cgsize_t *)malloc(6*sizeof(cgsize_t)))==NULL ||
            (c_donor_range[n] = (cgsize_t *)malloc(6*sizeof(cgsize_t)))==NULL ||
            (c_transform[n]   = (int *)malloc(3*sizeof(int)))==NULL) {
            cgi_error("Error allocating memory...");
            *ier = 1;
	    goto cleanup;
        }
    }
     /* get all 1to1 interfaces */
    *ier = (cgint_f)cg_1to1_read_global((int)*fn, (int)*B, c_connectname, c_zonename,
               c_donorname, c_range, c_donor_range, c_transform);
     /* copy C-arrays in Fortran arrays */
    if (*ier == 0) {
        for (n = 0; n < Nglobal; n++) {
            step = n*CGIO_MAX_NAME_LENGTH;
            string_2_F_string(c_connectname[n], STR_PTR(connectname)+step,
                              CGIO_MAX_NAME_LENGTH, ier);
            if (*ier) break;
            string_2_F_string(c_zonename[n],    STR_PTR(zonename)   +step,
                              CGIO_MAX_NAME_LENGTH, ier);
            if (*ier) break;
            string_2_F_string(c_donorname[n],   STR_PTR(donorname)  +step,
                              CGIO_MAX_NAME_LENGTH, ier);
            if (*ier) break;

            for (i = 0; i < Ndim; i++) {
                step = Ndim*2*n;
                range[step+i] = c_range[n][i];
                range[step+i+Ndim] = c_range[n][i+Ndim];
                donor_range[step+i] = c_donor_range[n][i];
                donor_range[step+i+Ndim] = c_donor_range[n][i+Ndim];
                transform[Ndim*n+i] = (cgint_f)c_transform[n][i];
            }
#if DEBUG_FTOC
            printf("c_connectname[n]='%s'\n",c_connectname[n]);
            printf("c_zonename   [n]='%s'\n",c_zonename[n]);
            printf("c_donorname  [n]='%s'\n",c_donorname[n]);
            printf("..................12345678901234567890123456789012\n");
#endif
        }
#if DEBUG_FTOC
        printf("connectname='%s'\n",connectname);
        printf("zonename   ='%s'\n",zonename);
        printf("donorname  ='%s'\n",donorname);
        printf(".............+2345678901234567890123456789012+2345678901234567890123456789012\n");
#endif
    }

 cleanup:
    for (n = 0; n < Nglobal; n++) {
      CGNS_FREE(c_connectname[n]);
      CGNS_FREE(c_zonename[n]);
      CGNS_FREE(c_donorname[n]);
      CGNS_FREE(c_range[n]);
      CGNS_FREE(c_donor_range[n]);
      CGNS_FREE(c_transform[n]);
    }
    CGNS_FREE(c_connectname);
    CGNS_FREE(c_zonename);
    CGNS_FREE(c_donorname);
    CGNS_FREE(c_range);
    CGNS_FREE(c_donor_range);
    CGNS_FREE(c_transform);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BC_t Nodes                                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_nbocos_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *nbocos, cgint_f *ier)
{
    int i_nbocos;

    *ier = (cgint_f)cg_nbocos((int)*fn, (int)*B, (int)*Z, &i_nbocos);
    *nbocos = (cgint_f)i_nbocos;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_boco_info_f, CG_BOCO_INFO_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *BC, STR_PSTR(boconame), CGNS_ENUMT(BCType_t) *bocotype,
	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts, cgint_f *NormalIndex,
	cgsize_t *NormalListSize, CGNS_ENUMT(DataType_t) *NormalDataType, cgint_f *ndataset,
	cgint_f *ier STR_PLEN(boconame))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(BCType_t) i_bocotype;
    CGNS_ENUMT(PointSetType_t) i_ptset_type;
    CGNS_ENUMT(DataType_t) i_NormalDataType;
    int n, i_ndataset, index_dim, i_NormalIndex[3];

    *ier = (cgint_f)cg_index_dim((int)*fn, (int)*B, (int)*Z, &index_dim);
    if (*ier) return;
    *ier = (cgint_f)cg_boco_info((int)*fn, (int)*B, (int)*Z, (int)*BC, c_name,
               &i_bocotype, &i_ptset_type, npnts, i_NormalIndex,
               NormalListSize, &i_NormalDataType, &i_ndataset);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(boconame), STR_LEN(boconame), ier);

    *bocotype = i_bocotype;
    *ptset_type = i_ptset_type;
    *NormalDataType = i_NormalDataType;
    *ndataset = (cgint_f)i_ndataset;
    for (n = 0; n < index_dim; n++)
        NormalIndex[n] = (cgint_f)i_NormalIndex[n];
}

/*-----------------------------------------------------------------------*/

CGNSDLL void  FMNAME(cg_boco_read_f,CG_BOCO_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *BC, cgsize_t *pnts, void *NormalList, cgint_f *ier)
{
    *ier = (cgint_f)cg_boco_read((int)*fn, (int)*B, (int)*Z, (int)*BC, pnts, NormalList);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_boco_id_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *BC, double *boco_id, cgint_f *ier)
{
    *ier = (cgint_f)cg_boco_id((int)*fn, (int)*B, (int)*Z, (int)*BC, boco_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_boco_write_f, CG_BOCO_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, STR_PSTR(boconame), CGNS_ENUMT(BCType_t) *bocotype,
	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts, cgsize_t *pnts,
	cgint_f *BC, cgint_f *ier STR_PLEN(boconame))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_BC;

    string_2_C_string(STR_PTR(boconame), STR_LEN(boconame),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("boconame='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_boco_write((int)*fn, (int)*B, (int)*Z, c_name,
               *bocotype,
               *ptset_type,
               *npnts, pnts, &i_BC);
    *BC = (cgint_f)i_BC;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_boco_normal_write_f, CG_BOCO_NORMAL_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
	cgsize_t *NormalIndex, cgint_f *NormalListFlag,
	CGNS_ENUMT(DataType_t) *NormalDataType, void *NormalList, cgint_f *ier)
{
    int n, index_dim, i_NormalIndex[3];

    *ier = (cgint_f)cg_index_dim((int)*fn, (int)*B, (int)*Z, &index_dim);
    if (*ier) return;
    for (n = 0; n < index_dim; n++)
        i_NormalIndex[n] = (int)NormalIndex[n];
    *ier = (cgint_f)cg_boco_normal_write((int)*fn, (int)*B, (int)*Z, (int)*BC,
               i_NormalIndex, (int)*NormalListFlag,
               *NormalDataType, NormalList);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_boco_gridlocation_read_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
	CGNS_ENUMT(GridLocation_t) *location, cgint_f *ier)
{
    CGNS_ENUMT(GridLocation_t) i_location;

    *ier = (cgint_f)cg_boco_gridlocation_read((int)*fn, (int)*B, (int)*Z,
               (int)*BC, &i_location);
    *location = i_location;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_boco_gridlocation_write_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
	CGNS_ENUMT(GridLocation_t) *location, cgint_f *ier)
{
    *ier = (cgint_f)cg_boco_gridlocation_write((int)*fn, (int)*B, (int)*Z,
               (int)*BC, *location);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/WallFunction_t Nodes                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_bc_wallfunction_read_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
	CGNS_ENUMT(WallFunctionType_t) *WallFunctionType, cgint_f *ier)
{
    CGNS_ENUMT(WallFunctionType_t) i_WallFunctionType;

    *ier = (cgint_f)cg_bc_wallfunction_read((int)*fn, (int)*B, (int)*Z, (int)*BC,
               &i_WallFunctionType);
    *WallFunctionType = i_WallFunctionType;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_bc_wallfunction_write_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
	CGNS_ENUMT(WallFunctionType_t) *WallFunctionType, cgint_f *ier)
{
    *ier = (cgint_f)cg_bc_wallfunction_write((int)*fn, (int)*B, (int)*Z, (int)*BC,
					     *WallFunctionType);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/Area_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_bc_area_read_f, CG_BC_AREA_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *BC, CGNS_ENUMT(AreaType_t) *AreaType,
	float *SurfaceArea, STR_PSTR(RegionName),
	cgint_f *ier STR_PLEN(RegionName))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(AreaType_t) i_AreaType;

    *ier = (cgint_f)cg_bc_area_read((int)*fn, (int)*B, (int)*Z, (int)*BC, &i_AreaType,
               SurfaceArea, c_name);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(RegionName), STR_LEN(RegionName), ier);
    *AreaType = i_AreaType;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_bc_area_write_f, CG_BC_AREA_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *BC, CGNS_ENUMT(AreaType_t) *AreaType,
	float *SurfaceArea, STR_PSTR(RegionName),
	cgint_f *ier STR_PLEN(RegionName))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
#if DEBUG_FTOC
    int n;
#endif

    string_2_C_string(STR_PTR(RegionName), STR_LEN(RegionName),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("RegionName='");
    for (n=0; n<32; n++) printf("%c",*((STR_PTR(RegionName))+n));
        printf("', c_name='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_bc_area_write((int)*fn, (int)*B, (int)*Z, (int)*BC,
				     *AreaType,
				     *SurfaceArea, c_name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivityProperty_t/Periodic_t Nodes       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_conn_periodic_read_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
	float *RotationCenter, float *RotationAngle, float *Translation,
	cgint_f *ier)
{
    *ier = (cgint_f)cg_conn_periodic_read((int)*fn, (int)*B, (int)*Z, (int)*I,
               RotationCenter, RotationAngle, Translation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_conn_periodic_write_f (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
	float *RotationCenter, float *RotationAngle, float *Translation,
	cgint_f *ier)
{
    *ier = (cgint_f)cg_conn_periodic_write((int)*fn, (int)*B, (int)*Z, (int)*I,
               RotationCenter, RotationAngle, Translation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_1to1_periodic_read_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
	float *RotationCenter, float *RotationAngle, float *Translation,
	cgint_f *ier)
{
    *ier = (cgint_f)cg_1to1_periodic_read((int)*fn, (int)*B, (int)*Z, (int)*I,
               RotationCenter, RotationAngle, Translation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_1to1_periodic_write_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
	float *RotationCenter, float *RotationAngle, float *Translation,
	cgint_f *ier)
{
    *ier = (cgint_f)cg_1to1_periodic_write((int)*fn, (int)*B, (int)*Z, (int)*I,
               RotationCenter, RotationAngle, Translation);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *   Read and write GridConnectivityProperty_t/AverageInterface_t Nodes  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_conn_average_read_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
	CGNS_ENUMT(AverageInterfaceType_t) *AverageInterfaceType, cgint_f *ier)
{
    CGNS_ENUMT(AverageInterfaceType_t) i_AverageInterfaceType;

    *ier = (cgint_f)cg_conn_average_read((int)*fn, (int)*B, (int)*Z, (int)*I,
               &i_AverageInterfaceType);
    *AverageInterfaceType = i_AverageInterfaceType;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_conn_average_write_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
	CGNS_ENUMT(AverageInterfaceType_t) *AverageInterfaceType, cgint_f *ier)
{
    *ier = (cgint_f)cg_conn_average_write((int)*fn, (int)*B, (int)*Z, (int)*I,
					  *AverageInterfaceType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_1to1_average_read_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
	CGNS_ENUMT(AverageInterfaceType_t) *AverageInterfaceType, cgint_f *ier)
{
    CGNS_ENUMT(AverageInterfaceType_t) i_AverageInterfaceType;

    *ier = (cgint_f)cg_1to1_average_read((int)*fn, (int)*B, (int)*Z, (int)*I,
					 &i_AverageInterfaceType);
    *AverageInterfaceType = i_AverageInterfaceType;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_1to1_average_write_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
	CGNS_ENUMT(AverageInterfaceType_t) *AverageInterfaceType, cgint_f *ier)
{
    *ier = (cgint_f)cg_1to1_average_write((int)*fn, (int)*B, (int)*Z, (int)*I,
					  *AverageInterfaceType);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCDataSet_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_dataset_read_f, CG_DATASET_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *BC, cgint_f *DSet,
	STR_PSTR(Dataset_name), CGNS_ENUMT(BCType_t) *BCType, cgint_f *DirichletFlag,
	cgint_f *NeumannFlag, cgint_f *ier STR_PLEN(Dataset_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(BCType_t) i_BCType;
    int i_DirichletFlag, i_NeumannFlag;

    *ier = (cgint_f)cg_dataset_read((int)*fn, (int)*B, (int)*Z, (int)*BC, (int)*DSet,
               c_name, &i_BCType, &i_DirichletFlag, &i_NeumannFlag);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(Dataset_name), STR_LEN(Dataset_name), ier);
    *BCType = i_BCType;
    *DirichletFlag = (cgint_f)i_DirichletFlag;
    *NeumannFlag = (cgint_f)i_NeumannFlag;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_dataset_write_f, CG_DATASET_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *BC, STR_PSTR(Dataset_name),
	CGNS_ENUMT(BCType_t) *BCType, cgint_f *Dset, cgint_f *ier STR_PLEN(Dataset_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_Dset;

    string_2_C_string(STR_PTR(Dataset_name), STR_LEN(Dataset_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("Dataset_name='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_dataset_write((int)*fn, (int)*B, (int)*Z, (int)*BC, c_name,
               *BCType, &i_Dset);
    *Dset = (cgint_f)i_Dset;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_bcdataset_write_f, CG_BCDATASET_WRITE_F) (
	STR_PSTR(Dataset_name), CGNS_ENUMT(BCType_t) *BCType,
	CGNS_ENUMT(BCDataType_t) *BCDataType,
	cgint_f *ier STR_PLEN(Dataset_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(Dataset_name), STR_LEN(Dataset_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("Dataset_name='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_bcdataset_write(c_name, *BCType,
               *BCDataType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_bcdataset_info_f(
	cgint_f *ndataset, cgint_f *ier STR_PLEN(Dataset_name))
{
    int i_ndataset;

    *ier = (cgint_f)cg_bcdataset_info(&i_ndataset);
    *ndataset = (cgint_f)i_ndataset;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_bcdataset_read_f, CG_BCDATASET_READ_F) (
	cgint_f *index, STR_PSTR(Dataset_name), CGNS_ENUMT(BCType_t) *BCType,
	cgint_f *DirichletFlag, cgint_f *NeumannFlag,
	cgint_f *ier STR_PLEN(Dataset_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(BCType_t) i_BCType;
    int i_DirichletFlag, i_NeumannFlag;

    *ier = (cgint_f)cg_bcdataset_read((int)*index, c_name, &i_BCType,
               &i_DirichletFlag, &i_NeumannFlag);
    if (*ier) return;
    *BCType = i_BCType;
    *DirichletFlag = (cgint_f)i_DirichletFlag;
    *NeumannFlag = (cgint_f)i_NeumannFlag;
    string_2_F_string(c_name, STR_PTR(Dataset_name), STR_LEN(Dataset_name), ier);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCData_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_bcdata_write_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *BC, cgint_f *Dset,
	CGNS_ENUMT(BCDataType_t) *BCDataType, cgint_f *ier)
{
    *ier = (cgint_f)cg_bcdata_write((int)*fn, (int)*B, (int)*Z, (int)*BC,
               (int)*Dset, *BCDataType);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RigidGridMotion_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_n_rigid_motions_f(cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *n_rigid_motions, cgint_f *ier)
{
    int i_n_rigid_motions;

    *ier = (cgint_f)cg_n_rigid_motions((int)*fn, (int)*B, (int)*Z, &i_n_rigid_motions);
    *n_rigid_motions = (cgint_f)i_n_rigid_motions;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_rigid_motion_read_f, CG_RIGID_MOTION_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *R, STR_PSTR(rmotion_name),
	CGNS_ENUMT(RigidGridMotionType_t) *type, cgint_f *ier STR_PLEN(rmotion_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(RigidGridMotionType_t) i_type;

    *ier = (cgint_f)cg_rigid_motion_read((int)*fn, (int)*B, (int)*Z, (int)*R,
               c_name, &i_type);
    if (*ier) return;
    *type = i_type;
    string_2_F_string(c_name, STR_PTR(rmotion_name), STR_LEN(rmotion_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_rigid_motion_write_f, CG_RIGID_MOTION_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, STR_PSTR(rmotion_name),
	CGNS_ENUMT(RigidGridMotionType_t) *type, cgint_f *R, cgint_f *ier STR_PLEN(rmotion_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_R;

    string_2_C_string(STR_PTR(rmotion_name), STR_LEN(rmotion_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_rigid_motion_write((int)*fn, (int)*B, (int)*Z, c_name,
               *type, &i_R);
    *R = (cgint_f)i_R;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ArbitraryGridMotion_t Nodes                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_n_arbitrary_motions_f(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *n_arbitrary_motions,
	cgint_f *ier)
{
    int i_n_arbitrary_motions;

    *ier = (cgint_f)cg_n_arbitrary_motions((int)*fn, (int)*B, (int)*Z,
               &i_n_arbitrary_motions);
    *n_arbitrary_motions = (cgint_f)i_n_arbitrary_motions;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_arbitrary_motion_read_f, CG_ARBITRARY_MOTION_READ_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *A,
	STR_PSTR(amotion_name), CGNS_ENUMT(ArbitraryGridMotionType_t) *type,
	cgint_f *ier STR_PLEN(amotion_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(ArbitraryGridMotionType_t) i_type;

    *ier = (cgint_f)cg_arbitrary_motion_read((int)*fn, (int)*B, (int)*Z, (int)*A,
               c_name, &i_type);
    if (*ier) return;
    *type = i_type;
    string_2_F_string(c_name, STR_PTR(amotion_name), STR_LEN(amotion_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_arbitrary_motion_write_f, CG_ARBITRARY_MOTION_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(amotion_name),
	CGNS_ENUMT(ArbitraryGridMotionType_t) *type, cgint_f *A, cgint_f *ier STR_PLEN(amotion_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_A;

    string_2_C_string(STR_PTR(amotion_name), STR_LEN(amotion_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_arbitrary_motion_write((int)*fn, (int)*B, (int)*Z, c_name,
               *type, &i_A);
    *A = (cgint_f)i_A;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_ngrids_f(cgint_f *fn, cgint_f *B,
	cgint_f *Z, cgint_f *ngrids, cgint_f *ier)
{
    int i_ngrids;

    *ier = (cgint_f)cg_ngrids((int)*fn, (int)*B, (int)*Z, &i_ngrids);
    *ngrids = (cgint_f)i_ngrids;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_grid_read_f, CG_GRID_READ_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *G, STR_PSTR(gridname),
	cgint_f *ier STR_PLEN(gridname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_grid_read((int)*fn, (int)*B, (int)*Z, (int)*G, c_name);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(gridname), STR_LEN(gridname), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_grid_write_f, CG_GRID_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, STR_PSTR(gridname), cgint_f *G,
	cgint_f *ier STR_PLEN(gridname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_G;

    string_2_C_string(STR_PTR(gridname), STR_LEN(gridname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_grid_write((int)*fn, (int)*B, (int)*Z, c_name, &i_G);
    *G = (cgint_f)i_G;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_grid_bounding_box_write_f, CG_GRID_BOUNDING_BOX_WRITE_F)  (
        cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *G,
        CGNS_ENUMT(DataType_t) *datatype, void *bbox_array,
        cgint_f *ier)
{
    *ier = (cgint_f)cg_grid_bounding_box_write(
        (int)*fn, (int)*B, (int)*Z, (int)*G,
        *datatype, bbox_array);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_grid_bounding_box_read_f, CG_GRID_BOUNDING_BOX_READ_F) (
        cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *G,
        CGNS_ENUMT(DataType_t) *datatype, void *bbox_array,
        cgint_f *ier)
{
    *ier = (cgint_f)cg_grid_bounding_box_read(
        (int)*fn, (int)*B, (int)*Z, (int)*G,
        *datatype, bbox_array);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write SimulationType_t Node                             *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_simulation_type_read_f (
	cgint_f *fn, cgint_f *B, CGNS_ENUMT(SimulationType_t) *type, cgint_f *ier)
{
    CGNS_ENUMT(SimulationType_t) i_type;

    *ier = (cgint_f)cg_simulation_type_read((int)*fn, (int)*B, &i_type);
    *type = i_type;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_simulation_type_write_f(
	cgint_f *fn, cgint_f *B, CGNS_ENUMT(SimulationType_t) *type, cgint_f *ier)
{
    *ier = (cgint_f)cg_simulation_type_write((int)*fn, (int)*B,
               *type);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BaseIterativeData_t Node                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_biter_read_f, CG_BITER_READ_F) (cgint_f *fn,
	cgint_f *B, STR_PSTR(bitername), cgint_f *nsteps,
	cgint_f *ier STR_PLEN(bitername))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_nsteps;

    *ier = (cgint_f)cg_biter_read((int)*fn, (int)*B, c_name, &i_nsteps);
    if (*ier) return;
    *nsteps = (cgint_f)i_nsteps;
    string_2_F_string(c_name, STR_PTR(bitername), STR_LEN(bitername), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_biter_write_f, CG_BITER_WRITE_F) (cgint_f *fn,
	cgint_f *B, STR_PSTR(bitername), cgint_f *nsteps,
	cgint_f *ier STR_PLEN(bitername))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(bitername), STR_LEN(bitername),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_biter_write((int)*fn, (int)*B, c_name, (int)*nsteps);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ZoneIterativeData_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_ziter_read_f, CG_ZITER_READ_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, STR_PSTR(zitername), cgint_f *ier STR_PLEN(zitername))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_ziter_read((int)*fn, (int)*B, (int)*Z, c_name);
    if (*ier == 0) {
      string_2_F_string(c_name, STR_PTR(zitername), STR_LEN(zitername), ier);
    }
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ziter_write_f, CG_ZITER_WRITE_F) (cgint_f *fn, cgint_f *B,
	cgint_f *Z, STR_PSTR(zitername), cgint_f *ier STR_PLEN(zitername))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(zitername), STR_LEN(zitername),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_ziter_write((int)*fn, (int)*B, (int)*Z, c_name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Gravity_t Node                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_gravity_read_f(cgint_f *fn,
	cgint_f *B, float *gravity_vector, cgint_f *ier)
{
    *ier = (cgint_f)cg_gravity_read((int)*fn, (int)*B, gravity_vector);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_gravity_write_f(cgint_f *fn,
	cgint_f *B, float *gravity_vector, cgint_f *ier)
{
   *ier = (cgint_f)cg_gravity_write((int)*fn, (int)*B, gravity_vector);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Axisymmetry_t Node                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_axisym_read_f(cgint_f *fn,
	cgint_f *B, float *ref_point, float *axis, cgint_f *ier)
{
    *ier = (cgint_f)cg_axisym_read((int)*fn, (int)*B, ref_point, axis);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_axisym_write_f(cgint_f *fn,
	cgint_f *B, float *ref_point, float *axis, cgint_f *ier)
{
    *ier = (cgint_f)cg_axisym_write((int)*fn, (int)*B, ref_point, axis);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RotatingCoordinates_t Node                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_rotating_read_f(
	float *rot_rate, float *rot_center, cgint_f *ier)
{
    *ier = (cgint_f)cg_rotating_read(rot_rate, rot_center);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_rotating_write_f(
	float *rot_rate, float *rot_center, cgint_f *ier)
{
        *ier = (cgint_f)cg_rotating_write(rot_rate, rot_center);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write  IndexArray/Range_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void cg_ptset_info_f(
	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts, cgint_f *ier)
{
    CGNS_ENUMT(PointSetType_t) i_ptset_type;

    *ier = (cgint_f)cg_ptset_info(&i_ptset_type, npnts);
    *ptset_type = i_ptset_type;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ptset_read_f, CG_PTSET_READ_F) (cgsize_t *pnts, cgint_f *ier)
{
    *ier = (cgint_f)cg_ptset_read(pnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ptset_write_f, CG_PTSET_WRITE_F) (
	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts, cgsize_t *pnts, cgint_f *ier)
{
    *ier = (cgint_f)cg_ptset_write(*ptset_type, *npnts, pnts);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Go - To Function                                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_goto_f(cgint_f *fn, cgint_f *B, cgint_f *ier, ...)
#else
CGNSDLL void FMNAME(cg_goto_f, CG_GOTO_F)(cgint_f *fn, cgint_f *B, cgint_f *ier, ...)
#endif
{
#ifndef _fcd
#define _fcd char*
#endif
    char *f_label[CG_MAX_GOTO_DEPTH], *label[CG_MAX_GOTO_DEPTH];
    int index[CG_MAX_GOTO_DEPTH], n, i, len[CG_MAX_GOTO_DEPTH];
    va_list ap;

     /* initialize ap to the last parameter before the variable argument list */
     /* Note:  On HP, print statements btw va_start and va_end create major problems */

    va_start(ap, ier);

     /* read arguments */
    for (n = 0; n < CG_MAX_GOTO_DEPTH; n++)  {
        f_label[n] = STR_PTR(va_arg(ap, _fcd));
#ifdef _CRAY
        len[n] = _fcdlen(cray_string);
#endif
# ifdef WIN32_FORTRAN
     /* In Windows, the arguments appear in order: char*, len, index,...*/
        len[n] = (int)va_arg(ap, int);
# endif
        if (f_label[n][0] == ' ' || 0 == strncmp(f_label[n],"end",3) ||
            0 == strncmp(f_label[n],"END",3)) break;

        index[n] = (int)*(va_arg(ap, cgint_f *));
        if (index[n] < 0) {
            cgi_error("Incorrect input to function cg_goto_f");
            *ier = 1;
            return;
        }
    }
#if !defined(_CRAY) && !defined(WIN32_FORTRAN)
    for (i=0; i<n; i++) {
      len[i] = va_arg(ap, int);
    }
#endif
    va_end(ap);

     /* convert strings to C-strings */
    for (i=0; i < n; i++) {
        label[i] = CGNS_NEW(char,len[i]+1);
        string_2_C_string(f_label[i], len[i], label[i], len[i], ier);
    }

#if DEBUG_GOTO
    printf("\nIn cg_ftoc.c: narguments=%d\n",n);
    for (i=0; i<n; i++) printf("\targ %d: '%s' #%d\n",i,label[i], index[i]);
#endif

    *ier = (cgint_f)cgi_set_posit((int)*fn, (int)*B, n, index, label);

    for (i=0; i<n; i++) CGNS_FREE(label[i]);
    return;
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_gorel_f(cgint_f *fn, cgint_f *ier, ...)
#else
CGNSDLL void FMNAME(cg_gorel_f, CG_GOREL_F)(cgint_f *fn, cgint_f *ier, ...)
#endif
{
#ifdef _CRAY
    _fcd cray_string;
#endif
    char *f_label[CG_MAX_GOTO_DEPTH], *label[CG_MAX_GOTO_DEPTH];
    int index[CG_MAX_GOTO_DEPTH], n, i, len[CG_MAX_GOTO_DEPTH];

    va_list ap;

    if (posit == 0) {
        cgi_error ("position not set with cg_goto");
        *ier = (cgint_f)CG_ERROR;
        return;
    }
    if ((int)*fn != posit_file) {
        cgi_error("current position is in the wrong file");
        *ier = (cgint_f)CG_ERROR;
        return;
    }

     /* initialize ap to the last parameter before the variable argument list */
     /* Note:  On HP, print statements btw va_start and va_end create major problems */

    va_start(ap, ier);

     /* read arguments */
    for (n = 0; n < CG_MAX_GOTO_DEPTH; n++)  {
#ifdef _CRAY
        cray_string = va_arg(ap, _fcd);
        f_label[n] = _fcdtocp(cray_string);
        len[n] = _fcdlen(cray_string);
#else
        f_label[n] = va_arg(ap, char *);
# ifdef WIN32_FORTRAN
     /* In Windows, the arguments appear in a different order: char*, len, index,...*/
        len[n] = va_arg(ap, int);
# endif
#endif
        if (f_label[n][0] == ' ' || 0 == strncmp(f_label[n],"end",3) ||
            0 == strncmp(f_label[n],"END",3)) break;

        index[n] = (int)*(va_arg(ap, cgint_f *));
        if (index[n] < 0) {
            cgi_error("Incorrect input to function cg_goto_f");
            *ier = 1;
            return;
        }
    }
#if !defined(_CRAY) && !defined(WIN32_FORTRAN)
    for (i=0; i<n; i++) {
        len[i] = va_arg(ap, int);
    }
#endif
    va_end(ap);

     /* convert strings to C-strings */
    for (i=0; i < n; i++) {
        label[i] = CGNS_NEW(char,len[i]+1);
        string_2_C_string(f_label[i], len[i], label[i], len[i], ier);
    }

#if DEBUG_GOTO
    printf("\nIn cg_ftoc.c: narguments=%d\n",n);
    for (i=0; i<n; i++) printf("\targ %d: '%s' #%d\n",i,label[i], index[i]);
#endif

    *ier = (cgint_f)cgi_update_posit(n, index, label);

    for (i=0; i<n; i++) CGNS_FREE(label[i]);
    return;
}

CGNSDLL void FMNAME(cg_goto_f1, CG_GOTO_F1)(cgint_f *fn, cgint_f *B, cgint_f *ier, STR_PSTR(name), cgint_f *index STR_PLEN(name))
{
    int length;
    char *c_label[2];
    int c_index[2], n;

    if (*index < 0) {
        cgi_error("Incorrect input to function cg_goto_f");
        *ier = 1;
        return;
    }

    length = (int) STR_LEN(name);
    c_label[0] = CGNS_NEW(char, length+1);
    c_label[1] = "end";
    c_index[0] = (int)*index;
    c_index[1] = 0;

    string_2_C_string(STR_PTR(name), STR_LEN(name), c_label[0], length, ier);

    if (*ier == 0) {
      if (c_label[0][0] == ' ' || 0 == strncmp(c_label[0],"end",3) ||
          0 == strncmp(c_label[0],"END",3)) {
        n=0;
      } else {
        n=1;
      }
      *ier = (cgint_f)cgi_set_posit((int)*fn, (int)*B, n, c_index, c_label);
    }

    CGNS_FREE(c_label[0]);
}

CGNSDLL void FMNAME(cg_gorel_f1, CG_GOREL_F1)(cgint_f *fn, cgint_f *ier, STR_PSTR(name), cgint_f *index STR_PLEN(name))
{
    int length;
    char *c_label[2];
    int c_index[2], n;

    if (posit == 0) {
        cgi_error ("position not set with cg_goto");
        *ier = (cgint_f)CG_ERROR;
        return;
    }
    if ((int)*fn != posit_file) {
        cgi_error("current position is in the wrong file");
        *ier = (cgint_f)CG_ERROR;
        return;
    }
    if (*index < 0) {
        cgi_error("Incorrect input to function cg_gorel_f1");
        *ier = 1;
        return;
    }

    length = (int) STR_LEN(name);
    c_label[0] = CGNS_NEW(char, length+1);
    c_label[1] = "end";
    c_index[0] = (int)*index;
    c_index[1] = 0;

    string_2_C_string(STR_PTR(name), STR_LEN(name), c_label[0], length, ier);

    if (*ier == 0) {
      if (c_label[0][0] == ' ' || 0 == strncmp(c_label[0],"end",3) ||
          0 == strncmp(c_label[0],"END",3)) {
        n=0;
      } else {
        n=1;
      }
      *ier = (cgint_f)cgi_update_posit(n, c_index, c_label);
    }

    CGNS_FREE(c_label[0]);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_gopath_f, CG_GOPATH_F) (cgint_f *fn,
	STR_PSTR(path), cgint_f *ier STR_PLEN(path))
{
    int length;
    char *c_path;

    length = (int) STR_LEN(path);
    c_path = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(path), STR_LEN(path), c_path, length, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_gopath((int)*fn, c_path);
    CGNS_FREE(c_path);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *              Read Multiple path nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_famname_read_f, CG_FAMNAME_READ_F) (
	STR_PSTR(famname), cgint_f *ier STR_PLEN(famname))
{
    char c_name[(CGIO_MAX_NAME_LENGTH+1)*CG_MAX_GOTO_DEPTH+1];

    *ier = (cgint_f)cg_famname_read(c_name);
    if (*ier == 0)
      string_2_F_string(c_name, STR_PTR(famname), STR_LEN(famname), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_nmultifam_f(cgint_f *nfam, cgint_f *ier)
{
    int i_nfam;

    *ier = (cgint_f)cg_nmultifam(&i_nfam);
    *nfam = (cgint_f)i_nfam;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_multifam_read_f, CG_MULTIFAM_READ_F) (cgint_f *N,
	STR_PSTR(name), STR_PSTR(family),
	cgint_f *ier STR_PLEN(name) STR_PLEN(family))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    char c_family[(CGIO_MAX_NAME_LENGTH+1)*CG_MAX_GOTO_DEPTH+1];

    *ier = (cgint_f)cg_multifam_read((int)*N, c_name, c_family);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(name), STR_LEN(name), ier);
    if (*ier) return;
    string_2_F_string(c_family, STR_PTR(family), STR_LEN(family), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_convergence_read_f, CG_CONVERGENCE_READ_F) (
	cgint_f *iterations, STR_PSTR(NormDefinitions),
	cgint_f *ier STR_PLEN(NormDefinitions))
{
    char *c_descr_text;
    int i_iterations;

    *ier = (cgint_f)cg_convergence_read(&i_iterations, &c_descr_text);
    if (*ier) return;
    string_2_F_string(c_descr_text, STR_PTR(NormDefinitions),
        STR_LEN(NormDefinitions), ier);
    *iterations = (cgint_f)i_iterations;
    CGNS_FREE(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_state_size_f(
	cgint_f *size, cgint_f *ier)
{
    char *c_descr_text;

    *ier = (cgint_f)cg_state_read(&c_descr_text);
    if (*ier) return;
    *size = (cgint_f)strlen(c_descr_text);
    CGNS_FREE(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_state_read_f, CG_STATE_READ_F) (
	STR_PSTR(StateDescription), cgint_f *ier STR_PLEN(StateDescription))
{
    char *c_descr_text;

    *ier = (cgint_f)cg_state_read(&c_descr_text);
    if (*ier) return;
    string_2_F_string(c_descr_text, STR_PTR(StateDescription),
        STR_LEN(StateDescription), ier);
    CGNS_FREE(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_equationset_read_f(
	cgint_f *EquationDimension, cgint_f *GoverningEquationsFlag,
	cgint_f *GasModelFlag, cgint_f *ViscosityModelFlag,
	cgint_f *ThermalConductivityModelFlag,
	cgint_f *TurbulenceClosureFlag, cgint_f *TurbulenceModelFlag,
	cgint_f *ier)
{
    int i_EquationDimension, i_GoverningEquationsFlag, i_GasModelFlag;
    int i_ViscosityModelFlag, i_ThermalConductivityModelFlag;
    int i_TurbulenceClosureFlag, i_TurbulenceModelFlag;

    *ier = (cgint_f)cg_equationset_read(&i_EquationDimension,
               &i_GoverningEquationsFlag,
               &i_GasModelFlag, &i_ViscosityModelFlag,
               &i_ThermalConductivityModelFlag,
               &i_TurbulenceClosureFlag, &i_TurbulenceModelFlag);
#if DEBUG_FTOC
    printf("in cg_ftoc, EquationDimension=%d\n",*EquationDimension);
#endif
    *EquationDimension = (cgint_f)i_EquationDimension;
    *GoverningEquationsFlag = (cgint_f)i_GoverningEquationsFlag;
    *GasModelFlag = (cgint_f)i_GasModelFlag;
    *ViscosityModelFlag = (cgint_f)i_ViscosityModelFlag;
    *ThermalConductivityModelFlag = (cgint_f)i_ThermalConductivityModelFlag;
    *TurbulenceClosureFlag = (cgint_f)i_TurbulenceClosureFlag;
    *TurbulenceModelFlag = (cgint_f)i_TurbulenceModelFlag;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_equationset_chemistry_read_f(
	cgint_f *ThermalRelaxationFlag, cgint_f *ChemicalKineticsFlag, cgint_f *ier)
{
    int i_ThermalRelaxationFlag, i_ChemicalKineticsFlag;

    *ier = (cgint_f)cg_equationset_chemistry_read(&i_ThermalRelaxationFlag,
               &i_ChemicalKineticsFlag);
    *ThermalRelaxationFlag = (cgint_f)i_ThermalRelaxationFlag;
    *ChemicalKineticsFlag = (cgint_f)i_ChemicalKineticsFlag;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_equationset_elecmagn_read_f(
	cgint_f *ElecFldModelFlag, cgint_f *MagnFldModelFlag,
	cgint_f *ConductivityModelFlag, cgint_f *ier)
{
    int i_ElecFldModelFlag, i_MagnFldModelFlag, i_ConductivityModelFlag;

    *ier = (cgint_f)cg_equationset_elecmagn_read(&i_ElecFldModelFlag,
               &i_MagnFldModelFlag, &i_ConductivityModelFlag);
    *ElecFldModelFlag = (cgint_f)i_ElecFldModelFlag;
    *MagnFldModelFlag = (cgint_f)i_MagnFldModelFlag;
    *ConductivityModelFlag = (cgint_f)i_ConductivityModelFlag;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_governing_read_f(
	CGNS_ENUMT(GoverningEquationsType_t) *EquationsType, cgint_f *ier)
{
    CGNS_ENUMT(GoverningEquationsType_t) i_EquationsType;

    *ier = (cgint_f)cg_governing_read(&i_EquationsType);
    *EquationsType = i_EquationsType;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_diffusion_read_f(cgint_f *diffusion_model, cgint_f *ier)
{
    int n, index_dim, ndata, i_diffusion_model[6];

    index_dim = cgi_posit_index_dim();
    if (index_dim == 1) ndata = 1;
    else if (index_dim == 2) ndata = 3;
    else if (index_dim == 3) ndata = 6;
    else {
        cgi_error("invalid value for IndexDimension");
        *ier = (cgint_f)CG_ERROR;
        return;
    }
    *ier = (cgint_f)cg_diffusion_read(i_diffusion_model);
    if (*ier) return;
    for (n = 0; n < ndata; n++)
        diffusion_model[n] = (cgint_f)i_diffusion_model[n];
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_model_read_f, CG_MODEL_READ_F) (STR_PSTR(ModelLabel),
	CGNS_ENUMT(ModelType_t) *ModelType, cgint_f *ier STR_PLEN(ModelLabel))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(ModelType_t) i_ModelType;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(ModelLabel), STR_LEN(ModelLabel),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_model_read(c_name, &i_ModelType);
    *ModelType = i_ModelType;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_narrays_f(cgint_f *narrays, cgint_f *ier)
{
    int i_narrays;

    *ier = (cgint_f)cg_narrays(&i_narrays);
    *narrays = (cgint_f)i_narrays;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_array_info_f, CG_ARRAY_INFO_F) (cgint_f *A,
	STR_PSTR(ArrayName), CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension,
	cgsize_t *DimensionVector, cgint_f *ier STR_PLEN(ArrayName))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_DataDimension;
    CGNS_ENUMT(DataType_t) i_DataType;

    *ier = (cgint_f)cg_array_info((int)*A, c_name, &i_DataType, &i_DataDimension,
                         DimensionVector);
    if (*ier) return;
    string_2_F_string(c_name, STR_PTR(ArrayName), STR_LEN(ArrayName), ier);
    *DataType = i_DataType;
    *DataDimension = (cgint_f)i_DataDimension;
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_array_read_f(cgint_f *A, void *Data, ...)
{
    va_list ap;
    cgint_f *ier;
    int DataDimension;
    cgsize_t DimensionVector[CGIO_MAX_DIMENSIONS];
    char ArrayName[CGIO_MAX_NAME_LENGTH+1];
    CGNS_ENUMT(DataType_t) DataType;

    cg_array_info((int)*A, ArrayName, &DataType, &DataDimension, DimensionVector);

    va_start(ap, Data);
    if (DataType == CGNS_ENUMV(Character)) (void) va_arg(ap, int);
    ier = va_arg(ap, cgsize_t *);
    va_end(ap);
#else
CGNSDLL void FMNAME(cg_array_read_f, CG_ARRAY_READ_F) (cgint_f *A,
	void *Data, cgint_f *ier)
{
#endif
    *ier = (cgint_f)cg_array_read((int)*A, Data);
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_array_read_as_f(cgint_f *A, CGNS_ENUMT(DataType_t) *type,
	void *Data, ...)
{
    va_list ap;
    cgint_f *ier;
    va_start(ap, Data);
    if (*type == CGNS_ENUMV(Character))
        (void) va_arg(ap, int);
    ier = va_arg(ap, cgsize_t *);
    va_end(ap);
#else
    CGNSDLL void FMNAME(cg_array_read_as_f, CG_ARRAY_READ_AS_F) (cgint_f *A,
	CGNS_ENUMT(DataType_t) *type, void *Data, cgint_f *ier)
{
#endif
    *ier = (cgint_f)cg_array_read_as((int)*A, *type, Data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_array_general_read_f, CG_ARRAY_GENERAL_READ_F) (
        cgint_f *A,
        cgsize_t *s_rmin, cgsize_t *s_rmax, CGNS_ENUMT(DataType_t) *m_type, 
        cgint_f *m_numdim, cgsize_t *m_dimvals,
        cgsize_t *m_rmin, cgsize_t *m_rmax, void *data,
        cgint_f *ier)
{
    *ier = (cgint_f)cg_array_general_read(
        (int)*A,
        s_rmin, s_rmax,
        *m_type, (int)*m_numdim, m_dimvals, m_rmin, m_rmax, data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_nintegrals_f(
	cgint_f *nintegrals, cgint_f *ier)
{
    int i_nintegrals;

    *ier = (cgint_f)cg_nintegrals(&i_nintegrals);
    *nintegrals = (cgint_f)i_nintegrals;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_integral_read_f, CG_INTEGRAL_READ_F) (
	cgint_f *IntegralDataIndex, STR_PSTR(IntegralDataName),
	cgint_f *ier STR_PLEN(IntegralDataName))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_integral_read((int)*IntegralDataIndex, c_name);
    if (!*ier)
      string_2_F_string(c_name, STR_PTR(IntegralDataName),
			STR_LEN(IntegralDataName), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_rind_read_f(
	cgint_f *RindData, cgint_f *ier)
{
    int n, index_dim, i_RindData[6];

    index_dim = cgi_posit_index_dim();
    *ier = (cgint_f)cg_rind_read(i_RindData);
    if (*ier) return;
    for (n = 0; n < 2*index_dim; n++)
        RindData[n] = (cgint_f)i_RindData[n];
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_ndescriptors_f(cgint_f *ndescriptors, cgint_f *ier)
{
    int i_ndescriptors;

    *ier = (cgint_f)cg_ndescriptors(&i_ndescriptors);
    *ndescriptors = (cgint_f)i_ndescriptors;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_descriptor_size_f(
	cgint_f *descr_no, cgint_f *descr_size, cgint_f *ier)
{
    char *c_descr_text;
    char descr_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_descriptor_read((int)*descr_no, descr_name, &c_descr_text);
    if (!*ier) {
        *descr_size = (cgint_f)strlen(c_descr_text);
        CGNS_FREE(c_descr_text);
    }
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_descriptor_read_f, CG_DESCRIPTOR_READ_F) (
	cgint_f *descr_no, STR_PSTR(descr_name), STR_PSTR(descr_text),
	cgint_f *ier STR_PLEN(descr_name)  STR_PLEN(descr_text))
{
    char *c_descr_text;
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_descriptor_read((int)*descr_no, c_name, &c_descr_text);
    if (*ier) return;
#if DEBUG_FTOC
    printf("In cg_descriptor_read_f, descr_no=%d, descr_name='%s', c_descr_text='%s'\n",
        *descr_no, c_name, c_descr_text);
#endif
    string_2_F_string(c_name, STR_PTR(descr_name), STR_LEN(descr_name), ier);
    if (!*ier)
      string_2_F_string(c_descr_text, STR_PTR(descr_text),
			STR_LEN(descr_text), ier);
    CGNS_FREE(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_nunits_f(cgint_f *nunits, cgint_f *ier)
{
    int i_nunits;

    *ier = (cgint_f)cg_nunits(&i_nunits);
    *nunits = (cgint_f)i_nunits;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_units_read_f(
	CGNS_ENUMT(MassUnits_t) *mass, CGNS_ENUMT(LengthUnits_t) *length,  CGNS_ENUMT(TimeUnits_t) *time,
	CGNS_ENUMT(TemperatureUnits_t) *temperature, CGNS_ENUMT(AngleUnits_t) *angle, cgint_f *ier)
{
    CGNS_ENUMT(MassUnits_t) i_mass;
    CGNS_ENUMT(LengthUnits_t) i_length;
    CGNS_ENUMT(TimeUnits_t) i_time;
    CGNS_ENUMT(TemperatureUnits_t) i_temperature;
    CGNS_ENUMT(AngleUnits_t) i_angle;

    *ier = (cgint_f)cg_units_read(&i_mass, &i_length, &i_time, &i_temperature, &i_angle);
    *mass = i_mass;
    *length = i_length;
    *time = i_time;
    *temperature = i_temperature;
    *angle = i_angle;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_unitsfull_read_f(
	CGNS_ENUMT(MassUnits_t) *mass, CGNS_ENUMT(LengthUnits_t) *length, CGNS_ENUMT(TimeUnits_t) *time,
	CGNS_ENUMT(TemperatureUnits_t) *temperature, CGNS_ENUMT(AngleUnits_t) *angle, CGNS_ENUMT(ElectricCurrentUnits_t) *current,
	CGNS_ENUMT(SubstanceAmountUnits_t) *amount, CGNS_ENUMT(LuminousIntensityUnits_t) *intensity, cgint_f *ier)
{
    CGNS_ENUMT(MassUnits_t) i_mass;
    CGNS_ENUMT(LengthUnits_t) i_length;
    CGNS_ENUMT(TimeUnits_t) i_time;
    CGNS_ENUMT(TemperatureUnits_t) i_temperature;
    CGNS_ENUMT(AngleUnits_t) i_angle;
    CGNS_ENUMT(ElectricCurrentUnits_t) i_current;
    CGNS_ENUMT(SubstanceAmountUnits_t) i_amount;
    CGNS_ENUMT(LuminousIntensityUnits_t) i_intensity;

    *ier = (cgint_f)cg_unitsfull_read(&i_mass, &i_length, &i_time, &i_temperature,
                             &i_angle, &i_current, &i_amount, &i_intensity);
    *mass = i_mass;
    *length = i_length;
    *time = i_time;
    *temperature = i_temperature;
    *angle = i_angle;
    *current = i_current;
    *amount = i_amount;
    *intensity = i_intensity;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_exponents_info_f(CGNS_ENUMT(DataType_t) *DataType, cgint_f *ier)
{
    CGNS_ENUMT(DataType_t) i_DataType;

    *ier = (cgint_f)cg_exponents_info(&i_DataType);
    *DataType = i_DataType;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_nexponents_f(cgint_f*nexps, cgint_f *ier)
{
    int i_nexps;

    *ier = (cgint_f)cg_nexponents(&i_nexps);
    *nexps = (cgint_f)i_nexps;
}

/*-----------------------------------------------------------------------*/

 CGNSDLL void FMNAME(cg_exponents_read_f, CG_EXPONENTS_READ_F) (void *exponents, cgint_f *ier)
{
    *ier = (cgint_f)cg_exponents_read(exponents);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_expfull_read_f, CG_EXPFULL_READ_F) (void *exponents, cgint_f *ier)
{
    *ier = (cgint_f)cg_expfull_read(exponents);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_conversion_info_f(CGNS_ENUMT(DataType_t) *DataType, cgint_f *ier)
{
    CGNS_ENUMT(DataType_t) i_DataType;

    *ier = (cgint_f)cg_conversion_info(&i_DataType);
    *DataType = i_DataType;
}

/*-----------------------------------------------------------------------*/

 CGNSDLL void FMNAME(cg_conversion_read_f,CG_CONVERSION_READ_F) (void *ConversionFactors, cgint_f *ier)
{
    *ier = (cgint_f)cg_conversion_read(ConversionFactors);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_dataclass_read_f(CGNS_ENUMT(DataClass_t) *dataclass, cgint_f *ier)
{
    CGNS_ENUMT(DataClass_t) i_dataclass;

    *ier = (cgint_f)cg_dataclass_read(&i_dataclass);
    *dataclass = i_dataclass;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_gridlocation_read_f(
	CGNS_ENUMT(GridLocation_t) *GridLocation, cgint_f *ier)
{
    CGNS_ENUMT(GridLocation_t) i_GridLocation;

    *ier = (cgint_f)cg_gridlocation_read(&i_GridLocation);
    *GridLocation = i_GridLocation;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_ordinal_read_f(cgint_f *Ordinal, cgint_f *ier)
{
    int i_Ordinal;

    *ier = (cgint_f)cg_ordinal_read(&i_Ordinal);
    *Ordinal = (cgint_f)i_Ordinal;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_npe_f(CGNS_ENUMT(ElementType_t) *type,
	cgint_f *npe, cgint_f *ier)
{
    int i_npe;

    *ier = (cgint_f)cg_npe(*type, &i_npe);
    *npe = (cgint_f)i_npe;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_is_link_f(cgint_f *path_length, cgint_f *ier)
{
    int i_path_length;

    *ier = (cgint_f)cg_is_link(&i_path_length);
    *path_length = (cgint_f)i_path_length;
}

/*-----------------------------------------------------------------------*/

 CGNSDLL void FMNAME(cg_link_read_f, CG_LINK_READ_F) (
	STR_PSTR(filename), STR_PSTR(link_path), cgint_f *ier
	STR_PLEN(filename)  STR_PLEN(link_path))
{
    char *f_name, *l_name;

    *ier = (cgint_f)cg_link_read(&f_name, &l_name);
    if (*ier) return;
    string_2_F_string(f_name, STR_PTR(filename), STR_LEN(filename), ier);
    if (*ier == 0)
      string_2_F_string(l_name, STR_PTR(link_path), STR_LEN(link_path), ier);
    CGNS_FREE(f_name);
    CGNS_FREE(l_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_nuser_data_f(
	cgint_f *nuser_data, cgint_f *ier)
{
    int i_nuser_data;

    *ier = (cgint_f)cg_nuser_data(&i_nuser_data);
    *nuser_data = (cgint_f)i_nuser_data;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_user_data_read_f, CG_USER_DATA_READ_F) (cgint_f *index,
	STR_PSTR(dataname), cgint_f *ier STR_PLEN(dataname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = (cgint_f)cg_user_data_read((int)*index, c_name);
    if (*ier == 0)
      string_2_F_string(c_name, STR_PTR(dataname), STR_LEN(dataname), ier);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *                   Write Multiple path nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_famname_write_f, CG_FAMNAME_WRITE_F) (
	STR_PSTR(family_name), cgint_f *ier STR_PLEN(family_name))
{
    char c_name[(CGIO_MAX_NAME_LENGTH+1)*CG_MAX_GOTO_DEPTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(family_name), STR_LEN(family_name),
        c_name, (CGIO_MAX_NAME_LENGTH+1)*CG_MAX_GOTO_DEPTH, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_famname_write(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_multifam_write_f, CG_MULTIFAM_WRITE_F) (
	STR_PSTR(name), STR_PSTR(family),
	cgint_f *ier STR_PLEN(name) STR_PLEN(family))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    char c_family[(CGIO_MAX_NAME_LENGTH+1)*CG_MAX_GOTO_DEPTH+1];

    string_2_C_string(STR_PTR(name), STR_LEN(name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(family), STR_LEN(family),
        c_family, (CGIO_MAX_NAME_LENGTH+1)*CG_MAX_GOTO_DEPTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_multifam_write(c_name, c_family);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_convergence_write_f, CG_CONVERGENCE_WRITE_F) (
	cgint_f *iterations, STR_PSTR(NormDefinitions),
	cgint_f *ier STR_PLEN(NormDefinitions))
{
    char *c_string;
    int len;

    len = STR_LEN(NormDefinitions);
     /* convert Fortran-text-string to a C-string */
    c_string = CGNS_NEW(char, len+1);
    string_2_C_string(STR_PTR(NormDefinitions), len, c_string, len, ier);
    if (*ier == 0) {
#if DEBUG_FTOC
        printf("In cg_ftoc: c_NormDefinitions = '%s'",c_string);
#endif
        *ier = (cgint_f)cg_convergence_write((int)*iterations, c_string);
    }
    CGNS_FREE(c_string);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_state_write_f, CG_STATE_WRITE_F) (STR_PSTR(StateDescription),
	cgint_f *ier STR_PLEN(StateDescription))
{
    char *c_string;
    int len;

    len = STR_LEN(StateDescription);
     /* convert Fortran-text-string to a C-string */
    c_string = CGNS_NEW(char, len+1);
    string_2_C_string(STR_PTR(StateDescription), len, c_string, len, ier);
    if (*ier == 0) {
#if DEBUG_FTOC
        printf("In cg_ftoc: C_StateDescription = '%s'",c_string);
#endif
        *ier = (cgint_f)cg_state_write(c_string);
    }
    CGNS_FREE(c_string);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_equationset_write_f(cgint_f *EquationDimension, cgint_f *ier)
{
#if DEBUG_FTOC
    printf("In cg_ftoc: EquationDimension=%d\n",*EquationDimension);
#endif
    *ier = (cgint_f)cg_equationset_write((int)*EquationDimension);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_governing_write_f(
    CGNS_ENUMT(GoverningEquationsType_t) *Equationstype, cgint_f *ier)
{
    *ier = (cgint_f)cg_governing_write(*Equationstype);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_diffusion_write_f(
	cgint_f *diffusion_model, cgint_f *ier)
{
    int n, index_dim, ndata, i_diffusion_model[6];

    index_dim = cgi_posit_index_dim();
    if (index_dim == 1) ndata = 1;
    else if (index_dim == 2) ndata = 3;
    else if (index_dim == 3) ndata = 6;
    else {
        cgi_error("invalid value for IndexDimension");
        *ier = (cgint_f)CG_ERROR;
        return;
    }
    for (n = 0; n < ndata; n++)
        i_diffusion_model[n] = (int)diffusion_model[n];
    *ier = (cgint_f)cg_diffusion_write(i_diffusion_model);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_model_write_f, CG_MODEL_WRITE_F) (STR_PSTR(ModelLabel),
	CGNS_ENUMT(ModelType_t) *ModelType, cgint_f *ier STR_PLEN(ModelLabel))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(ModelLabel), STR_LEN(ModelLabel),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_model_write(c_name, *ModelType);
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_array_write_f(STR_PSTR(ArrayName),
	CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension,
	cgsize_t *DimensionVector, void *Data, ...)
{
    va_list ap;
    cgint_f *ier;
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    va_start(ap, Data);
    if ((CGNS_ENUMT(DataType_t))*DataType == CGNS_ENUMV(Character))
        (void) va_arg(ap, int);
    ier = va_arg(ap, cgsize_t *);
    va_end(ap);
#else
CGNSDLL void FMNAME(cg_array_write_f, CG_ARRAY_WRITE_F) (STR_PSTR(ArrayName),
	CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension, cgsize_t *DimensionVector,
	void *Data, cgint_f *ier STR_PLEN(ArrayName))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
#endif

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(ArrayName), STR_LEN(ArrayName),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_array_write(c_name, *DataType,
                              (int)*DataDimension, DimensionVector, Data);
}

/* CGNSDLL void cg_array_write_f03 (ArrayName, */
/* 	CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension, cgsize_t *DimensionVector, */
/* 	void *Data, cgint_f *ier STR_PLEN(ArrayName)) */
/* { */
/*     char c_name[CGIO_MAX_NAME_LENGTH+1]; */

/*      /\* convert Fortran-text-string to a C-string *\/ */
/*     string_2_C_string(STR_PTR(ArrayName), STR_LEN(ArrayName), */
/*         c_name, CGIO_MAX_NAME_LENGTH, ier); */
/*     if (*ier == 0) */
/*         *ier = (cgint_f)cg_array_write(c_name, *DataType, */
/*                               (int)*DataDimension, DimensionVector, Data); */
/* } */

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_array_general_write_f, CG_ARRAY_GENERAL_WRITE_F) (
        STR_PSTR(arrayname), CGNS_ENUMT(DataType_t) *s_type, cgint_f *s_numdim, cgsize_t *s_dimvals,
        cgsize_t *s_rmin, cgsize_t *s_rmax,
        CGNS_ENUMT(DataType_t) *m_type, cgint_f *m_numdim, cgsize_t *m_dimvals,
        cgsize_t *m_rmin, cgsize_t *m_rmax,
        void *data, cgint_f *ier STR_PLEN(arrayname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(arrayname), STR_LEN(arrayname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("      arrayname='%s'\n", c_name);
#endif
    *ier = (cgint_f)cg_array_general_write(c_name, *s_type, (int)*s_numdim,
                                           s_dimvals, s_rmin, s_rmax,
                                           *m_type, (int)*m_numdim,
                                           m_dimvals, m_rmin, m_rmax, data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_integral_write_f, CG_INTEGRAL_WRITE_F) (
	STR_PSTR(IntegralDataName), cgint_f *ier STR_PLEN(IntegralDataName))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(IntegralDataName), STR_LEN(IntegralDataName),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_integral_write(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_rind_write_f(cgint_f *RindData, cgint_f *ier)
{
    int n, index_dim, i_RindData[6];

    index_dim = cgi_posit_index_dim();
    for (n = 0; n < 2*index_dim; n++)
        i_RindData[n] = (int)RindData[n];
    *ier = (cgint_f)cg_rind_write(i_RindData);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_descriptor_write_f, CG_DESCRIPTOR_WRITE_F) (
	STR_PSTR(descr_name), STR_PSTR(descr_text),
	cgint_f *ier STR_PLEN(descr_name) STR_PLEN(descr_text))
{
    char *c_descr_text, c_descr_name[CGIO_MAX_NAME_LENGTH+1];
    int len;

/*  On Linux, the string found in STR_PTR(descr_text) is not terminated.
    Therefore it can be much longer and can't be used as is.  The value returned
    by STR_LEN(descr_text) is correct.  So the string can be terminatted properly:
    char *terminated_descr_text;
    terminated_descr_text=(char*)malloc(strlen(STR_PTR(descr_text))+1);
    strcpy(terminated_descr_text,STR_PTR(descr_text));
    terminated_descr_text[STR_LEN(descr_text)]='\0';
    It's not necessary to do this here since we're always calling get_adf_c_name()
    which truncates the string to STR_LEN(descr_name) or shorter.
*/

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(descr_name), STR_LEN(descr_name),
        c_descr_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    len = STR_LEN(descr_text);
    c_descr_text = CGNS_NEW(char, len+1);
    string_2_C_string(STR_PTR(descr_text), len, c_descr_text, len, ier);
    if (*ier == 0) {
#if DEBUG_FTOC
        printf("c_descr_name='%s', c_descr_text='%s'\n",c_descr_name, c_descr_text);
#endif

         /* Call C-routine */
        *ier = (cgint_f)cg_descriptor_write(c_descr_name, c_descr_text);
    }
    CGNS_FREE(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_units_write_f(
	CGNS_ENUMT(MassUnits_t) *mass, CGNS_ENUMT(LengthUnits_t) *length, CGNS_ENUMT(TimeUnits_t) *time,
	CGNS_ENUMT(TemperatureUnits_t) *temperature, CGNS_ENUMT(AngleUnits_t) *angle, cgint_f *ier)
{
    *ier = (cgint_f)cg_units_write(*mass,
               *length,
               *time,
               *temperature,
               *angle);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_unitsfull_write_f(
	CGNS_ENUMT(MassUnits_t) *mass, CGNS_ENUMT(LengthUnits_t) *length, CGNS_ENUMT(TimeUnits_t) *time,
	CGNS_ENUMT(TemperatureUnits_t) *temperature, CGNS_ENUMT(AngleUnits_t) *angle, CGNS_ENUMT(ElectricCurrentUnits_t) *current,
	CGNS_ENUMT(SubstanceAmountUnits_t) *amount, CGNS_ENUMT(LuminousIntensityUnits_t) *intensity, cgint_f *ier)
{
    *ier = (cgint_f)cg_unitsfull_write(*mass,
               *length,
               *time,
               *temperature,
               *angle,
               *current,
               *amount,
               *intensity);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_exponents_write_f,CG_EXPONENTS_WRITE_F) (
	CGNS_ENUMT(DataType_t)*DataType, void *exponents, cgint_f *ier)
{
    *ier = (cgint_f)cg_exponents_write(*DataType, exponents);
}

/*-----------------------------------------------------------------------*/

 CGNSDLL void FMNAME(cg_expfull_write_f, CG_EXPFULL_WRITE_F) (
	CGNS_ENUMT(DataType_t) *DataType, void *exponents, cgint_f *ier)
{
    *ier = (cgint_f)cg_expfull_write(*DataType, exponents);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conversion_write_f, CG_CONVERSION_WRITE_F) (
	CGNS_ENUMT(DataType_t) *DataType, void *ConversionFactors, cgint_f *ier)
{
    *ier = (cgint_f)cg_conversion_write(*DataType,
               ConversionFactors);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_dataclass_write_f(
	CGNS_ENUMT(DataClass_t) *dataclass, cgint_f *ier)
{
    *ier = (cgint_f)cg_dataclass_write(*dataclass);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_gridlocation_write_f(
	CGNS_ENUMT(GridLocation_t) *GridLocation, cgint_f *ier)
{
    *ier = (cgint_f)cg_gridlocation_write(*GridLocation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_ordinal_write_f(cgint_f *Ordinal, cgint_f *ier)
{
    *ier = (cgint_f)cg_ordinal_write((int)*Ordinal);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_link_write_f, CG_LINK_WRITE_F) (
	STR_PSTR(nodename), STR_PSTR(filename), STR_PSTR(name_in_file), cgint_f *ier
	STR_PLEN(nodename)  STR_PLEN(filename)  STR_PLEN(name_in_file))
{
    char n_name[CGIO_MAX_NAME_LENGTH+1];
    char f_name[CGIO_MAX_FILE_LENGTH+1];
    char i_name[CGIO_MAX_LINK_LENGTH+1];

    string_2_C_string(STR_PTR(nodename), STR_LEN(nodename),
        n_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(filename), STR_LEN(filename),
        f_name, CGIO_MAX_FILE_LENGTH, ier);
    if (*ier) return;
    string_2_C_string(STR_PTR(name_in_file), STR_LEN(name_in_file),
        i_name, CGIO_MAX_LINK_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cg_link_write(n_name, f_name, i_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_user_data_write_f, CG_USER_DATA_WRITE_F) (
	STR_PSTR(dataname), cgint_f *ier STR_PLEN(dataname))
{
    char d_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(dataname), STR_LEN(dataname),
        d_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier == 0)
        *ier = (cgint_f)cg_user_data_write(d_name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      General Delete Function                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_delete_node_f, CG_DELETE_NODE_F) (STR_PSTR(node_name),
	cgint_f *ier STR_PLEN(node_name))
{
/* ici */
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(node_name), STR_LEN(node_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cg_delete_node(c_name);
#if DEBUG_FTOC
    printf("\n  Deleting node ='%s'\n", c_name);
#endif
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Error Handling Functions                                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_get_error_f, CG_GET_ERROR_F) (
	STR_PSTR(errmsg) STR_PLEN(errmsg))
{
    cgint_f ierr;

    string_2_F_string ((char *)cg_get_error(), STR_PTR(errmsg),
        STR_LEN(errmsg), &ierr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_error_exit_f()
{
    cg_error_exit();
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cg_error_print_f()
{
    cg_error_print();
}

/*-----------------------------------------------------------------------*/

static void exit_on_error(int is_fatal, char *errmsg)
{
    if (is_fatal) {
        fprintf(stderr, "FATAL ERROR:%s\n", errmsg);
        exit(1);
    }
}

CGNSDLL void cg_exit_on_error_f(cgint_f *flag)
{
  cg_error_handler((int)*flag ? exit_on_error : NULL);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgio_set_dimensions_f_0, CGIO_SET_DIMENSIONS_F_0) (
    cgint_f *cgio_num, double *id, STR_PSTR(data_type), cgint_f *ndims,
    cgsize_t *dims, cgint_f *ier STR_PLEN(data_type) )
{
    char c_type[CGIO_MAX_DATATYPE_LENGTH+1];

    string_2_C_string(STR_PTR(data_type), STR_LEN(data_type),
        c_type, CGIO_MAX_DATATYPE_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cgio_set_dimensions((int)*cgio_num, *id, c_type, (int)*ndims, dims);
}

CGNSDLL void FMNAME(cgio_set_dimensions_f_1, CGIO_SET_DIMENSIONS_F_1) (
    cgint_f *cgio_num, double *id, STR_PSTR(data_type), cgint_f *ndims,
    cgsize_t *dims, cgint_f *ier STR_PLEN(data_type) )
{
    char c_type[CGIO_MAX_DATATYPE_LENGTH+1];

    string_2_C_string(STR_PTR(data_type), STR_LEN(data_type),
        c_type, CGIO_MAX_DATATYPE_LENGTH, ier);
    if (*ier) return;

    *ier = (cgint_f)cgio_set_dimensions((int)*cgio_num, *id, c_type, (int)*ndims, dims);
}

CGNSDLL void cgio_get_dimensions_f_0(
    cgint_f *cgio_num, double *id, cgint_f *ndims, cgsize_t *dims,
    cgint_f *ier)
{
    int i_ndims;

    *ier = (cgint_f)cgio_get_dimensions((int)*cgio_num, *id, &i_ndims, dims);
    *ndims = (cgint_f)i_ndims;
}

CGNSDLL void cgio_get_dimensions_f_1(
    cgint_f *cgio_num, double *id, cgint_f *ndims, cgsize_t *dims,
    cgint_f *ier)
{
    int i_ndims;

    *ier = (cgint_f)cgio_get_dimensions((int)*cgio_num, *id, &i_ndims, dims);
    *ndims = (cgint_f)i_ndims;
}

#if CG_BUILD_PARALLEL

/*======================================================================
 * parallel IO interface
 *======================================================================*/

CGNSDLL void cgp_mpi_comm_f(cgint_f *mpi_comm_f, cgint_f *ier)
{
   MPI_Comm mpi_comm_c;
   mpi_comm_c = MPI_Comm_f2c((int)*mpi_comm_f);
   *ier = (cgint_f)cgp_mpi_comm(mpi_comm_c);
}

CGNSDLL void cgp_mpi_info_f(int *pcg_mpi_info_f, cgint_f *ier)
{
  MPI_Info pcg_mpi_info_c;
  pcg_mpi_info_c = MPI_Info_f2c((int)*pcg_mpi_info_f);
  *ier = (cgint_f)cgp_mpi_info(pcg_mpi_info_c);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cgp_pio_mode_f(CGNS_ENUMT(PIOmode_t) *mode, cgint_f *ier)
{
  *ier = (cgint_f)cgp_pio_mode(*mode);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_open_f, CGP_OPEN_F) (STR_PSTR(filename), int *mode,
	cgint_f *fn, cgint_f *ier STR_PLEN(filename))

{
  int length, i_fn;
  char *c_name;

    length = (int) STR_LEN(filename);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(filename), STR_LEN(filename), c_name, length, ier);
    if (*ier == 0) {
#if DEBUG_FTOC
        printf("filename='%s'\n",c_name);
#endif
        *ier = (cgint_f)cgp_open(c_name, *mode, &i_fn);
        *fn  = (cgint_f)i_fn;
    }
    CGNS_FREE(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cgp_close_f(cgint_f *fn, cgint_f *ier)
{
  *ier = (cgint_f)cgp_close((int)*fn);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_coord_write_f, CGP_COORD_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, CGNS_ENUMT(DataType_t) *type, STR_PSTR(coordname),
	cgint_f *C, cgint_f *ier STR_PLEN(coordname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_C;

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cgp_coord_write((int)*fn, (int)*B, (int)*Z,
               *type, c_name, &i_C);
    *C = (cgint_f)i_C;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_coord_write_data_f,CGP_COORD_WRITE_DATA_F)(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C,
	cgsize_t *rmin, cgsize_t *rmax, void *data, cgint_f *ier)
{
  *ier = (cgint_f)cgp_coord_write_data((int)*fn, (int)*B, (int)*Z, (int)*C,
               rmin, rmax, data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_coord_read_data_f,CGP_COORD_READ_DATA_F)(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C,
	cgsize_t *rmin, cgsize_t *rmax, void *data, cgint_f *ier)
{
  *ier = (cgint_f)cgp_coord_read_data((int)*fn, (int)*B, (int)*Z, (int)*C,
               rmin, rmax, data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_section_write_f, CGP_SECTION_WRITE_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(section_name),
	CGNS_ENUMT(ElementType_t)*type, cgsize_t *start, cgsize_t *end, cgint_f *nbndry,
	cgint_f *S, cgint_f *ier STR_PLEN(section_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_S;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(section_name), STR_LEN(section_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cgp_section_write((int)*fn, (int)*B, (int)*Z, c_name,
               *type, *start, *end, (int)*nbndry, &i_S);
    *S = (cgint_f)i_S;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_elements_write_data_f,CGP_ELEMENTS_WRITE_DATA_F)(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *start,
	cgsize_t *end, cgsize_t *elements, cgint_f *ier)
{
    *ier = (cgint_f)cgp_elements_write_data((int)*fn, (int)*B, (int)*Z, (int)*S,
               *start, *end, elements);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_elements_read_data_f,CGP_ELEMENTS_READ_DATA_F)(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *start,
	cgsize_t *end, cgsize_t *elements, cgint_f *ier)
{
    *ier = (cgint_f)cgp_elements_read_data((int)*fn, (int)*B, (int)*Z, (int)*S,
               *start, *end, elements);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_field_write_f, CGP_FIELD_WRITE_F) (cgint_f *fn,
	cgint_f *B, cgint_f *Z, cgint_f *S, CGNS_ENUMT(DataType_t) *type,
	STR_PSTR(fieldname), cgint_f *F, cgint_f *ier STR_PLEN(fieldname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_F;

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cgp_field_write((int)*fn, (int)*B, (int)*Z, (int)*S,
               *type, c_name, &i_F);
    *F = (cgint_f)i_F;
}

/*-----------------------------------------------------------------------*/

 CGNSDLL void FMNAME(cgp_field_write_data_f,CGP_FIELD_WRITE_DATA_F)(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	cgint_f *F, cgsize_t *rmin, cgsize_t *rmax, void *field_ptr,
	cgint_f *ier)
{
    *ier = (cgint_f)cgp_field_write_data((int)*fn, (int)*B, (int)*Z, (int)*S,
               (int)*F, rmin, rmax, field_ptr);
}

/*-----------------------------------------------------------------------*/

 CGNSDLL void FMNAME(cgp_field_read_data_f,CGP_FIELD_READ_DATA_F)(
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	cgint_f *F, cgsize_t *rmin, cgsize_t *rmax, void *field_ptr,
	cgint_f *ier)
{
    *ier = (cgint_f)cgp_field_read_data((int)*fn, (int)*B, (int)*Z, (int)*S,
               (int)*F, rmin, rmax, field_ptr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cgp_array_write_f, CGP_ARRAY_WRITE_F) (STR_PSTR(ArrayName),
	CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension, cgsize_t *DimensionVector,
	cgint_f *A, cgint_f *ier STR_PLEN(ArrayName))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int i_A;

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(ArrayName), STR_LEN(ArrayName),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
    *ier = (cgint_f)cgp_array_write(c_name, *DataType,
               (int)*DataDimension, DimensionVector, &i_A);
    *A = (cgint_f)i_A;
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cgp_array_write_data_f(cgint_f *A,
	cgsize_t *rmin, cgsize_t *rmax, void *data, ...)
{
    va_list ap;
    int ierr;
    cgint_f *ier;
    cgns_array *array;

    int have_dup = 0;
    array = cgi_array_address(CG_MODE_READ, 0, (int)*A, "dummy", &have_dup,
                              &ierr);
    *ier = (cgint_f)ierr;
    if (array == NULL || (*ier) == (cgint_f)CG_ERROR ) return;
    va_start(ap, data);
    if (0 == strcmp(array->data_type, "C1"))
        (void) va_arg(ap, int);
    ier = va_arg(ap, cgsize_t *);
    va_end(ap);
#else
CGNSDLL void FMNAME(cgp_array_write_data_f, CGP_ARRAY_WRITE_DATA_F) (
	cgint_f *A, cgsize_t *rmin, cgsize_t *rmax, void *data,
	cgint_f *ier)
{
#endif
    *ier = (cgint_f)cgp_array_write_data((int)*A, rmin, rmax, data);
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cgp_array_read_data_f(cgint_f *A,
	cgsize_t *rmin, cgsize_t *rmax, void *data, ...)
{
    va_list ap;
    int ierr;
    cgint_f *ier;
    cgns_array *array;

    int have_dup = 0;
    array = cgi_array_address(CG_MODE_READ, 0, (int)*A, "dummy", &have_dup,
                              &ierr);
    if (array == NULL) return;
    va_start(ap, data);
    if (0 == strcmp(array->data_type, "C1"))
        (void) va_arg(ap, int);
    ier = va_arg(ap, cgsize_t *);
    va_end(ap);
#else
CGNSDLL void FMNAME(cgp_array_read_data_f, CGP_ARRAY_READ_DATA_F) (
	cgint_f *A, cgsize_t *rmin, cgsize_t *rmax, void *data,
	cgint_f *ier)
{
#endif
    *ier = (cgint_f)cgp_array_read_data((int)*A, rmin, rmax, data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void cgp_error_exit_f()
{
    cgp_error_exit();
}

#if HDF5_HAVE_MULTI_DATASETS

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *       cgp_coord_multi_read_data Function                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32_FORTRAN
 CGNSDLL void __stdcall cgp_coord_multi_read_data_f(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C,
	cgsize_t *rmin, cgsize_t *rmax,
	void *coordsX,  void *coordsY, void *coordsZ, cgint_f *ier)
#else
CGNSDLL void cgp_coord_multi_read_data_f(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C,
	cgsize_t *rmin, cgsize_t *rmax,
	void *coordsX, void *coordsY, void *coordsZ, cgint_f *ier)
#endif
{
  *ier = (cgint_f)cgp_coord_multi_read_data((int)*fn, (int)*B, (int)*Z, (int*)C, rmin, rmax, coordsX, coordsY, coordsZ);

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *       cgp_coord_multi_write_data Function                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32_FORTRAN
 CGNSDLL void __stdcall cgp_coord_multi_write_data_f(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C,
						    cgsize_t *rmin, cgsize_t *rmax,
						    void *coordsX,  void *coordsY, void *coordsZ, cgint_f *ier)
#else
CGNSDLL void cgp_coord_multi_write_data_f(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C,
						    cgsize_t *rmin, cgsize_t *rmax,
						    void *coordsX, void *coordsY, void *coordsZ, cgint_f *ier)
#endif
{
  *ier = (cgint_f)cgp_coord_multi_write_data((int)*fn, (int)*B, (int)*Z, (int*)C, rmin, rmax, coordsX, coordsY, coordsZ);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *       cgp_field_multi_write_data Function                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cgp_field_multi_write_data_f, CGP_FIELD_MULTI_WRITE_DATA_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	cgint_f *F, cgsize_t *rmin, cgsize_t *rmax, cgint_f *ier, cgsize_t *nsets, ...)
{
  va_list ap; /* argument list passed from the API call */
  int *F_c;
  int n;

  va_start(ap, nsets);

  if(sizeof(cgsize_t)!=sizeof(int)) {
    /* type cast F from cgsize_t to an int */
    if ((F_c = (int *)malloc(*nsets*sizeof(int)))==NULL) {
      cgi_error("Error allocating memory...");
      *ier = 1;
      return;
    }
    for (n = 0; n < *nsets; n++) {
      F_c[n] = (int)F[n];
    }
    *ier = vcgp_field_multi_write_data((int)*fn, (int)*B, (int)*Z, (int)*S,
				       F_c, rmin, rmax, (int)*nsets, ap);
    CGNS_FREE(F_c);
  } else {
    *ier = vcgp_field_multi_write_data((int)*fn, (int)*B, (int)*Z, (int)*S,
				       F, rmin, rmax, (int)*nsets, ap);
  }


}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *       cgp_field_multi_read_data Function                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cgp_field_multi_read_data_f, CGP_FIELD_MULTI_READ_DATA_F) (
	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
	cgint_f *F, cgsize_t *rmin, cgsize_t *rmax, cgint_f *ier, cgint_f *nsets, ...)
{
  va_list ap; /* argument list passed from the API call */
  int *F_c;
  int n;

  va_start(ap, nsets);

  if(sizeof(cgsize_t)!=sizeof(int)) {
    /* type cast F from cgsize_t to an int */
    if ((F_c = (int *)malloc(*nsets*sizeof(int)))==NULL) {
      cgi_error("Error allocating memory...");
      *ier = 1;
      return;
    }
    for (n = 0; n < *nsets; n++) {
      F_c[n] = (int)F[n];
    }
    *ier = (cgint_f)vcgp_field_multi_read_data((int)*fn, (int)*B, (int)*Z, (int)*S,
				    F_c, rmin, rmax, (int)*nsets, ap);
  } else {
    *ier = (cgint_f)vcgp_field_multi_read_data((int)*fn, (int)*B, (int)*Z, (int)*S,
				    F, rmin, rmax, (int)*nsets, ap);
  }

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *        cgp_array_multi_write_data Function                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cgp_array_multi_write_data_f, CGP_ARRAY_MULTI_WRITE_DATA_F) (
	cgint_f *fn, cgint_f *A, cgsize_t *rmin, cgsize_t *rmax,
	cgint_f *ier, cgint_f *nsets, ...)
{

  va_list ap; /* argument list passed from the API call */
  int *A_c;
  int n;

  va_start(ap, nsets);

  if(sizeof(cgsize_t)!=sizeof(int)) {
    /* type cast F from cgsize_t to an int */
    if ((A_c = (int *)malloc(*nsets*sizeof(int)))==NULL) {
      cgi_error("Error allocating memory...");
      *ier = 1;
      return;
    }
    for (n = 0; n < *nsets; n++) {
      A_c[n] = (int)A[n];
    }
    *ier = (cgint_f)vcgp_array_multi_write_data((int)*fn, A_c, rmin, rmax, (int)*nsets, ap);
  }else {
    *ier = (cgint_f)vcgp_array_multi_write_data((int)*fn, A, rmin, rmax, (int)*nsets, ap);
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *        cgp_array_multi_read_data Function                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cgp_array_multi_read_data_f, CGP_ARRAY_MULTI_READ_DATA_F) (
	cgint_f *fn, cgint_f *A, cgsize_t *rmin, cgsize_t *rmax,
	cgint_f *ier, cgint_f *nsets, ...)
{

  va_list ap; /* argument list passed from the API call */
  int *A_c;
  int n;


  va_start(ap, nsets);
  if(sizeof(cgsize_t)!=sizeof(int)) {
    /* type cast F from cgsize_t to an int */
    if ((A_c = (int *)malloc(*nsets*sizeof(int)))==NULL) {
      cgi_error("Error allocating memory...");
      *ier = 1;
      return;
    }
    for (n = 0; n < *nsets; n++) {
      A_c[n] = (int)A[n];
    }
    *ier = (cgint_f)vcgp_array_multi_read_data((int)*fn, A_c, rmin, rmax, (int)*nsets, ap);
  }else {
    *ier = (cgint_f)vcgp_array_multi_read_data((int)*fn, A, rmin, rmax, (int)*nsets, ap);
  }

}
#endif /*HDF5_HAVE_MULTI_DATASETS*/
#endif /*CG_BUILD_PARALLEL*/

