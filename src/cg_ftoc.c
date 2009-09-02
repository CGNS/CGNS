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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Convert between Fortran and C strings                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void string_2_C_string(char *string, int string_length,
    char *c_string, int max_len, int *ierr) {
    int i, iend;

    if (string == NULL || c_string == NULL) {
        cgi_error ("NULL string pointer");
        *ierr = CG_ERROR;
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
    *ierr = CG_OK;
}

/*-----------------------------------------------------------------------*/

static void string_2_F_string(char *c_string, char *string,
    int string_length, int *ierr) {
    int i, len;

    if (c_string == NULL || string == NULL) {
        cgi_error ("NULL string pointer");
        *ierr = CG_ERROR;
        return;
    }
    len = strlen(c_string);
    if (len > string_length) len = string_length;

    for (i = 0; i < len; i++)
        string[i] = c_string[i];
    while (i < string_length)
        string[i++] = ' ';
    *ierr = CG_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      LIBRARY FUNCTIONS                                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_is_cgns_f, CG_IS_CGNS_F) (STR_PSTR(filename),
    int *file_type, int *ier STR_PLEN(filename)) {
    int length;
    char *c_name;

    length = (int) STR_LEN(filename);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(filename), STR_LEN(filename), c_name, length, ier);
    if (*ier == 0)
        *ier = cg_is_cgns(c_name, file_type);
    CGNS_FREE(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_open_f, CG_OPEN_F) (STR_PSTR(filename), int *mode,
    int *fn, int *ier STR_PLEN(filename)) {
    int length;
    char *c_name;

    length = (int) STR_LEN(filename);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(filename), STR_LEN(filename), c_name, length, ier);
    if (*ier) {
        free (c_name);
        return;
    }
#if DEBUG_FTOC
    printf("filename='%s'\n",c_name);
#endif
    *ier = cg_open(c_name, *mode, fn);
    free(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_version_f, CG_VERSION_F) (int *fn, float *FileVersion, int *ier) {

    *ier = cg_version(*fn, FileVersion);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_close_f, CG_CLOSE_F) (int *fn, int *ier) {

    *ier = cg_close(*fn);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_save_as_f, CG_SAVE_AS_F) (int *fn, STR_PSTR(filename),
    int *file_type, int *follow_links, int *ier STR_PLEN(filename)) {
    int length;
    char *c_name;

    length = (int) STR_LEN(filename);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(filename), STR_LEN(filename), c_name, length, ier);
    if (*ier) {
        free (c_name);
        return;
    }
    *ier = cg_save_as(*fn, c_name, *file_type, *follow_links);
    free(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_set_file_type_f, CG_SET_FILE_TYPE_F) (int *ft, int *ier) {

    *ier = cg_set_file_type(*ft);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_get_file_type_f, CG_GET_FILE_TYPE_F) (int *fn, int *ft, int *ier) {

    *ier = cg_get_file_type(*fn, ft);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_set_compress_f, CG_SET_COMPRESS_F) (int *cmpr, int *ier) {

    *ier = cg_set_compress(*cmpr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_get_compress_f, CG_GET_COMPRESS_F) (int *cmpr, int *ier) {

    *ier = cg_get_compress(cmpr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_set_path_f, CG_SET_PATH_F) (STR_PSTR(pathname),
    int *ier STR_PLEN(pathname)) {
    int length;
    char *c_name;

    length = (int) STR_LEN(pathname);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(pathname), STR_LEN(pathname), c_name, length, ier);
    if (*ier) {
        free (c_name);
        return;
    }
    *ier = cg_set_path(c_name);
    free(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_add_path_f, CG_ADD_PATH_F) (STR_PSTR(pathname),
    int *ier STR_PLEN(pathname)) {
    int length;
    char *c_name;

    length = (int) STR_LEN(pathname);
    c_name = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(pathname), STR_LEN(pathname), c_name, length, ier);
    if (*ier) {
        free (c_name);
        return;
    }
    *ier = cg_add_path(c_name);
    free(c_name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write CGNSBase_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nbases_f, CG_NBASES_F) (int *fn, int *nbases, int *ier) {

    *ier = cg_nbases(*fn, nbases);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_base_read_f, CG_BASE_READ_F) (int *fn, int *B, STR_PSTR(basename),
    int *cell_dim, int *phys_dim, int *ier STR_PLEN(basename)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_base_read(*fn, *B, c_name, cell_dim, phys_dim);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(basename), STR_LEN(basename), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_base_id_f, CG_BASE_ID_F) (int *fn, int *B, double *base_id,
    int *ier) {

    *ier = cg_base_id(*fn, *B, base_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_base_write_f, CG_BASE_WRITE_F) (int *fn, STR_PSTR(basename),
       int *cell_dim, int *phys_dim, int *B, int *ier STR_PLEN(basename)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(basename), STR_LEN(basename),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("\nbasename='%s'\n", c_name);
    printf("cell_dim=%d\n",*cell_dim);
    printf("phys_dim=%d\n",*phys_dim);
#endif

    *ier = cg_base_write(*fn, c_name, *cell_dim, *phys_dim, B);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Zone_t Nodes                                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nzones_f, CG_NZONES_F) (int *fn, int *B, int *nzones, int *ier) {

    *ier = cg_nzones(*fn, *B, nzones);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_zone_type_f, CG_ZONE_TYPE_F) (int *fn, int *B, int *Z,
    ZoneType_t *type, int *ier) {

    *ier = cg_zone_type(*fn, *B, *Z, type);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_zone_read_f, CG_ZONE_READ_F) (int *fn, int *B, int *Z,
    STR_PSTR(zonename), int *size, int *ier STR_PLEN(zonename)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_zone_read(*fn, *B, *Z, c_name, size);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(zonename), STR_LEN(zonename), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_zone_id_f, CG_ZONE_ID_F) (int *fn, int *B, int *Z, double *zone_id,
    int *ier) {

    *ier = cg_zone_id(*fn, *B, *Z, zone_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_zone_write_f, CG_ZONE_WRITE_F) (int *fn, int *B, STR_PSTR(zonename),
    int *size, ZoneType_t *type, int *Z, int *ier STR_PLEN(zonename)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(zonename), STR_LEN(zonename),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

#if DEBUG_FTOC
    printf("\n  zonename='%s'\n", c_name);
#endif

    *ier = cg_zone_write(*fn, *B, c_name, size, *type, Z);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Family_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nfamilies_f, CG_NFAMILIES_F) (int *fn, int *B,
    int *nfamilies, int *ier) {

    *ier = cg_nfamilies(*fn, *B, nfamilies);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_family_read_f, CG_FAMILY_READ_F) (int *fn, int *B, int *F,
    STR_PSTR(family_name), int *nboco, int *ngeos, int *ier
    STR_PLEN(family_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_family_read(*fn, *B, *F, c_name, nboco, ngeos);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(family_name), STR_LEN(family_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_family_write_f, CG_FAMILY_WRITE_F) (int *fn, int *B,
    STR_PSTR(family_name), int *F, int *ier STR_PLEN(family_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(family_name), STR_LEN(family_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_family_write(*fn, *B, c_name, F);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FamBC_t Nodes                                     *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_fambc_read_f, CG_FAMBC_READ_F) (int *fn, int *B, int *F,
    int *BC, STR_PSTR(fambc_name), BCType_t *bocotype, int *ier
    STR_PLEN(fambc_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_fambc_read(*fn, *B, *F, *BC, c_name, bocotype);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(fambc_name), STR_LEN(fambc_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_fambc_write_f, CG_FAMBC_WRITE_F) (int *fn, int *B, int *F,
    STR_PSTR(fambc_name), BCType_t *bocotype, int *BC, int *ier
    STR_PLEN(fambc_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(fambc_name), STR_LEN(fambc_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_fambc_write(*fn, *B, *F, c_name, *bocotype, BC);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryReference_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_geo_read_f, CG_GEO_READ_F) (int *fn, int *B, int *F, int *G,
    STR_PSTR(geo_name), STR_PSTR(geo_file), STR_PSTR(CAD_name),
    int *npart, int *ier STR_PLEN(geo_name) STR_PLEN(geo_file)
    STR_PLEN(CAD_name)) {
    char c_geo_name[CGIO_MAX_NAME_LENGTH+1];
    char c_CAD_name[CGIO_MAX_NAME_LENGTH+1];
    char *c_geo_file;

    *ier = cg_geo_read(*fn, *B, *F, *G, c_geo_name, &c_geo_file,
        c_CAD_name, npart);
    if (*ier) return;

    string_2_F_string(c_geo_name, STR_PTR(geo_name), STR_LEN(geo_name), ier);
    if (*ier) return;
    string_2_F_string(c_CAD_name, STR_PTR(CAD_name), STR_LEN(CAD_name), ier);
    if (*ier) return;
    string_2_F_string(c_geo_file, STR_PTR(geo_file), STR_LEN(geo_file), ier);
    free(c_geo_file);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_geo_write_f, CG_GEO_WRITE_F) (int *fn, int *B, int *F,
    STR_PSTR(geo_name), STR_PSTR(geo_file), STR_PSTR(CAD_name),
    int *G, int *ier STR_PLEN(geo_name) STR_PLEN(geo_file)
    STR_PLEN(CAD_name)) {
    char c_geo_name[CGIO_MAX_NAME_LENGTH+1];
    char c_CAD_name[CGIO_MAX_NAME_LENGTH+1];
    char *c_geo_file;
    int length;

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
    if (!*ier)
        *ier = cg_geo_write(*fn, *B, *F, c_geo_name, c_geo_file, c_CAD_name, G);
    free(c_geo_file);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryEntity_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_part_read_f, CG_PART_READ_F) (int *fn, int *B, int *F, int *G,
    int *P, STR_PSTR(part_name), int *ier STR_PLEN(part_name)) {
    char c_part_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_part_read(*fn, *B, *F, *G, *P, c_part_name);
    if (!*ier)
        string_2_F_string(c_part_name, STR_PTR(part_name), STR_LEN(part_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_part_write_f, CG_PART_WRITE_F) (int *fn, int *B, int *F,
    int *G, STR_PSTR(part_name), int *P, int *ier STR_PLEN(part_name)) {
    char c_part_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(part_name), STR_LEN(part_name),
        c_part_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_part_write(*fn, *B, *F, *G, c_part_name, P);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DiscreteData_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_ndiscrete_f, CG_NDISCRETE_F) (int *fn, int *B, int *Z,
    int *ndiscrete, int *ier) {

    *ier = cg_ndiscrete(*fn, *B, *Z, ndiscrete);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_discrete_read_f, CG_DISCRETE_READ_F) (int *fn, int *B, int *Z,
    int *D, STR_PSTR(discrete_name), int *ier STR_PLEN(discrete_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_discrete_read(*fn, *B, *Z, *D, c_name);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(discrete_name), STR_LEN(discrete_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_discrete_write_f, CG_DISCRETE_WRITE_F) (int *fn, int *B, int *Z,
    STR_PSTR(discrete_name), int *D, int *ier STR_PLEN(discrete_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(discrete_name), STR_LEN(discrete_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("    discrete_name='%s'\n", c_name);
#endif

    *ier = cg_discrete_write(*fn, *B, *Z, c_name, D);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t/DataArray_t Nodes               *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_ncoords_f, CG_NCOORDS_F) (int *fn, int *B, int *Z, int *ncoords,
    int *ier) {

    *ier = cg_ncoords(*fn, *B, *Z, ncoords);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_info_f, CG_COORD_INFO_F) (int *fn, int *B, int *Z, int *C,
    DataType_t *type, STR_PSTR(coordname), int *ier STR_PLEN(coordname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_coord_info(*fn, *B, *Z, *C, type, c_name);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(coordname), STR_LEN(coordname), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_read_f, CG_COORD_READ_F) (int *fn, int *B, int *Z,
    STR_PSTR(coordname), DataType_t *type, int *rmin, int *rmax,
    void *coord, int *ier STR_PLEN(coordname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

#if DEBUG_FTOC
    printf("coordname='%s'\n", c_name);
#endif

    *ier = cg_coord_read(*fn, *B, *Z, c_name, *type, rmin, rmax, coord);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_id_f, CG_COORD_ID_F) (int *fn, int *B, int *Z, int *C,
    double *coord_id, int *ier) {

    *ier = cg_coord_id(*fn, *B, *Z, *C, coord_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_write_f, CG_COORD_WRITE_F) (int *fn, int *B, int *Z,
    DataType_t *type, STR_PSTR(coordname), void *coord, int *C, int *ier
    STR_PLEN(coordname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("    coordname='%s'\n", c_name);
#endif

    *ier = cg_coord_write(*fn, *B, *Z, *type, c_name, coord, C);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_coord_partial_write_f, CG_COORD_PARTIAL_WRITE_F) (int *fn,
    int *B, int *Z, DataType_t *type, STR_PSTR(coordname), int *rmin,
    int *rmax, void *coord, int *C, int *ier STR_PLEN(coordname))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(coordname), STR_LEN(coordname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("    coordname='%s'\n", c_name);
#endif

    *ier = cg_coord_partial_write(*fn, *B, *Z, *type, c_name, rmin, rmax,
				  coord, C);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Elements_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nsections_f, CG_NSECTIONS_F) (int *fn, int *B, int *Z,
    int *nsections, int *ier) {

    *ier = cg_nsections(*fn, *B, *Z, nsections);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_section_read_f, CG_SECTION_READ_F) (int *fn, int *B, int *Z,
    int *E, STR_PSTR(section_name), ElementType_t *type, int *start, int *end,
    int *nbndry, int *parent_flag, int *ier STR_PLEN(section_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_section_read(*fn, *B, *Z, *E, c_name, type, start,
            end, nbndry, parent_flag);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(section_name), STR_LEN(section_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elements_read_f, CG_ELEMENTS_READ_F) (int *fn, int *B, int *Z,
    int *E, int *elements, int *parent_data, int *ier) {

    *ier = cg_elements_read(*fn, *B, *Z, *E, elements, parent_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elementdatasize_f, CG_ELEMENTDATASIZE_F) (int *fn, int *B, int *Z,
    int *E, int *ElementDataSize, int *ier) {

    *ier = cg_ElementDataSize(*fn, *B, *Z, *E, ElementDataSize);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elementpartialsize_f, CG_ELEMENTPARTIALSIZE_F) (int *fn, int *B, int *Z,
    int *E, int *start, int *end, int *ElementDataSize, int *ier) {

    *ier = cg_ElementPartialSize(*fn, *B, *Z, *E, *start, *end, ElementDataSize);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_section_write_f, CG_SECTION_WRITE_F) (int *fn, int *B, int *Z,
    STR_PSTR(section_name), ElementType_t *type, int *start, int *end, int *nbndry,
    int *elements, int *S, int *ier STR_PLEN(section_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(section_name), STR_LEN(section_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_section_write(*fn, *B, *Z, c_name, *type, *start, *end,
            *nbndry, elements, S);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_parent_data_write_f, CG_PARENT_DATA_WRITE_F) (int *fn, int *B,
    int *Z, int *S, int *parent_data, int *ier) {

    *ier = cg_parent_data_write(*fn, *B, *Z, *S, parent_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_section_partial_write_f, CG_SECTION_PARTIAL_WRITE_F) (int *fn,
    int *B, int *Z, STR_PSTR(section_name), ElementType_t *type, int *start,
    int *end, int *nbndry, int *S, int *ier STR_PLEN(section_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(section_name), STR_LEN(section_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_section_partial_write(*fn, *B, *Z, c_name, *type, *start,
					*end, *nbndry, S);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_elements_partial_write_f, CG_ELEMENTS_PARTIAL_WRITE_F)
    (int *fn, int *B, int *Z, int *S, int *rmin, int *rmax, int *elements,
    int *ier)
{
    *ier = cg_elements_partial_write(*fn, *B, *Z, *S, *rmin, *rmax,
					elements);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_parent_data_partial_write_f, CG_PARENT_DATA_PARTIAL_WRITE_F)
    (int *fn, int *B, int *Z, int *S, int *rmin, int *rmax, int *parent_data,
    int *ier)
{
    *ier = cg_parent_data_partial_write(*fn, *B, *Z, *S, *rmin, *rmax,
					parent_data);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FlowSolution_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nsols_f, CG_NSOLS_F) (int *fn, int *B, int *Z, int *nsols, int *ier) {

    *ier = cg_nsols(*fn, *B, *Z, nsols);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_sol_info_f, CG_SOL_INFO_F) (int *fn, int *B, int *Z, int *S,
    STR_PSTR(solname), GridLocation_t *location, int *ier STR_PLEN(solname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_sol_info(*fn, *B, *Z, *S, c_name, location);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(solname), STR_LEN(solname), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_sol_id_f, CG_SOL_ID_F) (int *fn, int *B, int *Z, int *S,
    double *sol_id, int *ier) {

    *ier = cg_sol_id(*fn, *B, *Z, *S, sol_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_sol_write_f, CG_SOL_WRITE_F)(int *fn, int *B, int *Z, STR_PSTR(solname),
    GridLocation_t *location, int *S, int *ier STR_PLEN(solname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(solname), STR_LEN(solname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("\n    solname='%s'\n", c_name);
#endif

    *ier = cg_sol_write(*fn, *B, *Z, c_name, *location, S);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write solution DataArray_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nfields_f, CG_NFIELDS_F) (int *fn, int *B, int *Z, int *S,
    int *nfields, int *ier) {

    *ier = cg_nfields(*fn, *B, *Z, *S, nfields);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_info_f, CG_FIELD_INFO_F) (int *fn, int *B, int *Z, int *S,
    int *F, DataType_t *type, STR_PSTR(fieldname), int *ier STR_PLEN(fieldname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_field_info(*fn, *B, *Z, *S, *F, type, c_name);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(fieldname), STR_LEN(fieldname), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_read_f, CG_FIELD_READ_F) (int *fn, int *B, int *Z, int *S,
    STR_PSTR(fieldname), DataType_t *type, int *rmin, int *rmax,
    void *field_ptr, int *ier STR_PLEN(fieldname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("fieldname='%s'\n", c_name);
#endif

    *ier = cg_field_read(*fn, *B, *Z, *S, c_name, *type, rmin, rmax, field_ptr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_id_f, CG_FIELD_ID_F) (int *fn, int *B, int *Z, int *S, int *F,
    double *field_id, int *ier) {

    *ier = cg_field_id(*fn, *B, *Z, *S, *F, field_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_write_f, CG_FIELD_WRITE_F) (int *fn, int *B, int *Z, int *S,
    DataType_t *type, STR_PSTR(fieldname), void *field_ptr, int *F,
    int *ier STR_PLEN(fieldname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("      fieldname='%s'\n", c_name);
#endif

    *ier = cg_field_write(*fn, *B, *Z, *S, *type, c_name, field_ptr, F);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_field_partial_write_f, CG_FIELD_PARTIAL_WRITE_F) (int *fn,
    int *B, int *Z, int *S, DataType_t *type, STR_PSTR(fieldname),
    int *rmin, int *rmax, void *field_ptr, int *F,
    int *ier STR_PLEN(fieldname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(fieldname), STR_LEN(fieldname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("      fieldname='%s'\n", c_name);
#endif

    *ier = cg_field_partial_write(*fn, *B, *Z, *S, *type, c_name, rmin,
				  rmax, field_ptr, F);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write OversetHoles_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nholes_f, CG_NHOLES_F) (int *fn, int *B, int *Z, int *nholes,
    int *ier) {

    *ier = cg_nholes(*fn, *B, *Z, nholes);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_hole_info_f, CG_HOLE_INFO_F) (int *fn, int *B, int *Z, int *I,
    STR_PSTR(holename), GridLocation_t *location, PointSetType_t *ptset_type,
    int *nptsets, int *npnts, int *ier STR_PLEN(holename)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_hole_info(*fn, *B, *Z, *I, c_name, location,
        ptset_type, nptsets, npnts);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(holename), STR_LEN(holename), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_hole_read_f, CG_HOLE_READ_F) (int *fn, int *B, int *Z, int *I,
    int *pnts, int *ier) {

    *ier = cg_hole_read(*fn, *B, *Z, *I, pnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_hole_id_f, CG_HOLE_ID_F) (int *fn, int *B, int *Z, int *I,
    double *hole_id, int *ier) {

    *ier = cg_hole_id(*fn, *B, *Z, *I, hole_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_hole_write_f, CG_HOLE_WRITE_F) (int *fn, int *B, int *Z,
    STR_PSTR(holename), GridLocation_t *location, PointSetType_t *ptset_type,
    int *nptsets, int *npnts, int *pnts, int *I, int *ier STR_PLEN(holename)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(holename), STR_LEN(holename),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("holename='%s'\n", c_name);
#endif

    *ier = cg_hole_write(*fn, *B, *Z, c_name, *location, *ptset_type,
                     *nptsets, *npnts, pnts, I);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity_t Nodes                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nconns_f, CG_NCONNS_F) (int *fn, int *B, int *Z, int *nconns,
    int *ier) {

    *ier = cg_nconns(*fn, *B, *Z, nconns);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_info_f, CG_CONN_INFO_F) (int *fn, int *B, int *Z, int *I,
    STR_PSTR(connectname), GridLocation_t *location, GridConnectivityType_t *type,
    PointSetType_t *ptset_type, int *npnts, STR_PSTR(donorname),
    ZoneType_t *donor_zonetype, PointSetType_t *donor_ptset_type,
    DataType_t *donor_datatype, int *ndata_donor, int *ier
    STR_PLEN(connectname) STR_PLEN(donorname)) {
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_conn_info(*fn, *B, *Z, *I, cc_name, location,
                type, ptset_type, npnts, dc_name,
                donor_zonetype, donor_ptset_type, donor_datatype,
                ndata_donor);
    if (!*ier) {
        string_2_F_string(cc_name, STR_PTR(connectname), STR_LEN(connectname), ier);
        if (*ier) return;
        string_2_F_string(dc_name, STR_PTR(donorname), STR_LEN(donorname), ier);
    }
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_read_f, CG_CONN_READ_F) (int *fn, int *B, int *Z, int *I,
    int *pnts, DataType_t *donor_datatype, void *donor_data, int *ier) {

    *ier = cg_conn_read(*fn, *B, *Z, *I, pnts, *donor_datatype, donor_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_read_short_f, CG_CONN_READ_SHORT_F) (int *fn, int *B,
    int *Z, int *I, int *pnts, int *ier) {

    *ier = cg_conn_read_short(*fn, *B, *Z, *I, pnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_id_f, CG_CONN_ID_F) (int *fn, int *B, int *Z, int *I,
    double *conn_id, int *ier) {

    *ier = cg_conn_id(*fn, *B, *Z, *I, conn_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_write_f, CG_CONN_WRITE_F) (int *fn, int *B, int *Z,
    STR_PSTR(connectname), GridLocation_t *location, GridConnectivityType_t *type,
    PointSetType_t *ptset_type, int *npnts, int *pnts, STR_PSTR(donorname),
    ZoneType_t *donor_zonetype, PointSetType_t *donor_ptset_type,
    DataType_t *donor_datatype, int *ndata_donor, void *donor_data, int *I,
    int *ier STR_PLEN(connectname) STR_PLEN(donorname)) {
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];

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

    *ier = cg_conn_write(*fn, *B, *Z, cc_name, *location, *type, *ptset_type,
                 *npnts, pnts, dc_name, *donor_zonetype, *donor_ptset_type,
                 *donor_datatype, *ndata_donor, donor_data, I);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_write_short_f, CG_CONN_WRITE_SHORT_F) (int *fn, int *B,
    int *Z, STR_PSTR(connectname), GridLocation_t *location,
    GridConnectivityType_t *type, PointSetType_t *ptset_type, int *npnts,
    int *pnts, STR_PSTR(donorname), int *I,
    int *ier STR_PLEN(connectname) STR_PLEN(donorname)) {
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];

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

    *ier = cg_conn_write_short(*fn, *B, *Z, cc_name, *location, *type,
                 *ptset_type, *npnts, pnts, dc_name, I);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity1to1_t Nodes in a zone            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_n1to1_f, CG_N1TO1_F) (int *fn, int *B, int *Z, int *n1to1,
    int *ier) {

    *ier = cg_n1to1(*fn, *B, *Z, n1to1);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_read_f, CG_1TO1_READ_F) (int *fn, int *B, int *Z, int *I,
    STR_PSTR(connectname), STR_PSTR(donorname), int *range, int *donor_range,
    int *transform, int *ier STR_PLEN(connectname) STR_PLEN(donorname)) {
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_1to1_read(*fn, *B, *Z, *I, cc_name, dc_name,
                range, donor_range, transform);
    if (!*ier) {
        string_2_F_string(cc_name, STR_PTR(connectname), STR_LEN(connectname), ier);
        if (*ier) return;
        string_2_F_string(dc_name, STR_PTR(donorname), STR_LEN(donorname), ier);
    }
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_id_f, CG_1TO1_ID_F) (int *fn, int *B, int *Z, int *I,
    double *one21_id, int *ier) {

    *ier = cg_1to1_id(*fn, *B, *Z, *I, one21_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_write_f, CG_1TO1_WRITE_F) (int *fn, int *B, int *Z,
    STR_PSTR(connectname), STR_PSTR(donorname), int *range,
    int *donor_range, int *transform, int *I, int *ier
    STR_PLEN(connectname) STR_PLEN(donorname)) {
    char cc_name[CGIO_MAX_NAME_LENGTH+1], dc_name[CGIO_MAX_NAME_LENGTH+1];

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

    *ier = cg_1to1_write(*fn, *B, *Z, cc_name, dc_name, range,
                 donor_range, transform, I);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read all GridConnectivity1to1_t Nodes of a base                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_n1to1_global_f, CG_N1TO1_GLOBAL_F) (int *fn, int *B,
    int *n1to1_global, int *ier) {

    *ier = cg_n1to1_global(*fn, *B, n1to1_global);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_read_global_f, CG_1TO1_READ_GLOBAL_F) (int *fn, int *B,
    STR_PSTR(connectname), STR_PSTR(zonename), STR_PSTR(donorname),
    int *range, int *donor_range, int *transform, int *ier
    STR_PLEN(connectname) STR_PLEN(zonename) STR_PLEN(donorname)) {
    int n, i, step, len, ierr;
    int cell_dim, phys_dim;     /* number of dimension for model    */
    int Ndim;           /* indexDimension           */
    int Nglobal;            /* number of 1to1 interface in base     */
    char **c_connectname, **c_zonename, **c_donorname;
    char basename[CGIO_MAX_NAME_LENGTH+1];
    int **c_range, **c_donor_range;
    int **c_transform;

     /* initialize error code */
    *ier=0;

     /* get number of dimension for model: Ndim */
    if (cg_base_read(*fn, *B, basename, &cell_dim, &phys_dim)) {
        *ier=1;
        return;
    }
     /* For structured grid: */
    Ndim = cell_dim;

     /* get number of 1to1 interface in base:  Nglobal */
    if (cg_n1to1_global(*fn, *B, &Nglobal)) {
        *ier=1;
        return;
    }
    if (Nglobal<1) {
        cgi_error("Number of interface must equal 1 or more");
        *ier=1;
        return;
    }
     /* allocate memory for C-arrays (ptr-to-ptr) */
    if ((c_connectname = (char **)malloc(Nglobal*sizeof(char *)))==NULL ||
        (c_zonename    = (char **)malloc(Nglobal*sizeof(char *)))==NULL ||
        (c_donorname   = (char **)malloc(Nglobal*sizeof(char *)))==NULL ||
        (c_range       = (int **)  malloc(Nglobal*sizeof(int *)))==NULL ||
        (c_donor_range = (int **)  malloc(Nglobal*sizeof(int *)))==NULL ||
        (c_transform   = (int **)  malloc(Nglobal*sizeof(int *)))==NULL) {
        cgi_error("Error allocating memory...");
        *ier = 1;
        return;
    }
    len = CGIO_MAX_NAME_LENGTH+1;
    for (n=0; n<Nglobal; n++) {
        if ((c_connectname[n] = (char *)malloc(len*sizeof(char)))==NULL ||
            (c_zonename[n]    = (char *)malloc(len*sizeof(char)))==NULL ||
            (c_donorname[n]   = (char *)malloc(len*sizeof(char)))==NULL ||
            (c_range[n]       = (int *)  malloc(6*sizeof(int)))==NULL ||
            (c_donor_range[n] = (int *)  malloc(6*sizeof(int)))==NULL ||
            (c_transform[n]   = (int *)  malloc(3*sizeof(int)))==NULL) {
            cgi_error("Error allocating memory...");
            *ier = 1;
            return;
        }
    }
     /* get all 1to1 interfaces */
    if (cg_1to1_read_global(*fn, *B, (char **)c_connectname, (char **)c_zonename,
        (char **)c_donorname, (int **)c_range, (int **)c_donor_range, (int **)c_transform)){
        *ier=1;
        return;
    }
     /* copy C-arrays in Fortran arrays */
    for (n=0; n<Nglobal; n++) {
        step = n*CGIO_MAX_NAME_LENGTH;
        string_2_F_string(c_connectname[n], STR_PTR(connectname)+step,
            CGIO_MAX_NAME_LENGTH, &ierr);
        string_2_F_string(c_zonename[n],    STR_PTR(zonename)   +step,
            CGIO_MAX_NAME_LENGTH, &ierr);
        string_2_F_string(c_donorname[n],   STR_PTR(donorname)  +step,
            CGIO_MAX_NAME_LENGTH, &ierr);

        for (i=0; i<Ndim; i++) {
            step = Ndim*2*n;
            range[step+i] = c_range[n][i];
            range[step+i+Ndim] = c_range[n][i+Ndim];
            donor_range[step+i] = c_donor_range[n][i];
            donor_range[step+i+Ndim] = c_donor_range[n][i+Ndim];
            transform[Ndim*n+i] = c_transform[n][i];
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
    for (n=0; n<Nglobal; n++) {
        free(c_connectname[n]);
        free(c_zonename[n]);
        free(c_donorname[n]);
        free(c_range[n]);
        free(c_donor_range[n]);
        free(c_transform[n]);
    }
    free(c_connectname);
    free(c_zonename);
    free(c_donorname);
    free(c_range);
    free(c_donor_range);
    free(c_transform);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BC_t Nodes                                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_nbocos_f, CG_NBOCOS_F) (int *fn, int *B, int *Z, int *nbocos,
    int *ier) {

    *ier = cg_nbocos(*fn, *B, *Z, nbocos);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_boco_info_f, CG_BOCO_INFO_F) (int *fn, int *B, int *Z, int *BC,
    STR_PSTR(boconame), BCType_t *bocotype, PointSetType_t *ptset_type,
    int *npnts, int *NormalIndex, int *NormalListFlag, DataType_t *NormalDataType,
    int *ndataset, int *ier STR_PLEN(boconame)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_boco_info(*fn, *B, *Z, *BC, c_name, bocotype,
                ptset_type, npnts, NormalIndex, NormalListFlag,
                NormalDataType, ndataset);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(boconame), STR_LEN(boconame), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_boco_read_f, CG_BOCO_READ_F) (int *fn, int *B, int *Z, int *BC,
    int *pnts, void *NormalList, int *ier) {

    *ier = cg_boco_read(*fn, *B, *Z, *BC, pnts, NormalList);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_boco_id_f, CG_BOCO_ID_F) (int *fn, int *B, int *Z, int *BC,
    double *boco_id, int *ier) {

    *ier = cg_boco_id(*fn, *B, *Z, *BC, boco_id);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_boco_write_f, CG_BOCO_WRITE_F) (int *fn, int *B, int *Z,
    STR_PSTR(boconame), BCType_t *bocotype, PointSetType_t *ptset_type,
    int *npnts, int *pnts, int *BC, int *ier STR_PLEN(boconame)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(boconame), STR_LEN(boconame),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("boconame='%s'\n", c_name);
#endif

    *ier = cg_boco_write(*fn, *B, *Z, c_name, *bocotype, *ptset_type,
             *npnts, pnts, BC);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_boco_normal_write_f, CG_BOCO_NORMAL_WRITE_F) (int *fn, int *B,
    int *Z, int *BC, int *NormalIndex, int *NormalListFlag,
    DataType_t *NormalDataType, void *NormalList, int *ier) {

    *ier = cg_boco_normal_write(*fn, *B, *Z, *BC, NormalIndex,
         *NormalListFlag, *NormalDataType, NormalList);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/WallFunction_t Nodes                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_bc_wallfunction_read_f, CG_BC_WALLFUNCTION_READ_F) (int *fn,
    int *B, int *Z, int *BC, WallFunctionType_t *WallFunctionType, int *ier) {

    *ier = cg_bc_wallfunction_read(*fn, *B, *Z, *BC, WallFunctionType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_bc_wallfunction_write_f, CG_BC_WALLFUNCTION_WRITE_F) (int *fn,
    int *B, int *Z, int *BC, WallFunctionType_t *WallFunctionType, int *ier) {

    *ier = cg_bc_wallfunction_write(*fn, *B, *Z, *BC, *WallFunctionType);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/Area_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_bc_area_read_f, CG_BC_AREA_READ_F) (int *fn, int *B,
    int *Z, int *BC, AreaType_t *AreaType, float *SurfaceArea,
    STR_PSTR(RegionName), int *ier STR_PLEN(RegionName)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_bc_area_read(*fn, *B, *Z, *BC, AreaType, SurfaceArea, c_name);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(RegionName), STR_LEN(RegionName), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_bc_area_write_f, CG_BC_AREA_WRITE_F) (int *fn, int *B,
    int *Z, int *BC, AreaType_t *AreaType, float *SurfaceArea,
    STR_PSTR(RegionName), int *ier STR_PLEN(RegionName)) {
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
    *ier = cg_bc_area_write(*fn, *B, *Z, *BC, *AreaType, *SurfaceArea, c_name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivityProperty_t/Periodic_t Nodes       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_conn_periodic_read_f, CG_CONN_PERIODIC_READ_F) (int *fn,
    int *B, int *Z, int *I, float *RotationCenter, float *RotationAngle,
    float *Translation, int *ier) {

    *ier = cg_conn_periodic_read(*fn, *B, *Z, *I, RotationCenter,
        RotationAngle, Translation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_periodic_write_f, CG_CONN_PERIODIC_WRITE_F) (int *fn,
    int *B, int *Z, int *I, float *RotationCenter, float *RotationAngle,
        float *Translation, int *ier) {

    *ier = cg_conn_periodic_write(*fn, *B, *Z, *I, RotationCenter,
                RotationAngle, Translation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_periodic_read_f, CG_1TO1_PERIODIC_READ_F) (int *fn,
    int *B, int *Z, int *I, float *RotationCenter, float *RotationAngle,
    float *Translation, int *ier) {

    *ier = cg_1to1_periodic_read(*fn, *B, *Z, *I, RotationCenter,
        RotationAngle, Translation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_periodic_write_f, CG_1TO1_PERIODIC_WRITE_F) (int *fn,
    int *B, int *Z, int *I, float *RotationCenter, float *RotationAngle,
        float *Translation, int *ier) {

    *ier = cg_1to1_periodic_write(*fn, *B, *Z, *I, RotationCenter,
                RotationAngle, Translation);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *   Read and write GridConnectivityProperty_t/AverageInterface_t Nodes  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_conn_average_read_f, CG_CONN_AVERAGE_READ_F) (int *fn, int *B,
    int *Z, int *I, AverageInterfaceType_t *AverageInterfaceType, int *ier) {

    *ier = cg_conn_average_read(*fn, *B, *Z, *I, AverageInterfaceType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conn_average_write_f, CG_CONN_AVERAGE_WRITE_F) (int *fn, int *B,
    int *Z, int *I, AverageInterfaceType_t *AverageInterfaceType, int *ier) {

    *ier = cg_conn_average_write(*fn, *B, *Z, *I, *AverageInterfaceType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_average_read_f, CG_1TO1_AVERAGE_READ_F) (int *fn, int *B,
    int *Z, int *I, AverageInterfaceType_t *AverageInterfaceType, int *ier) {

    *ier = cg_1to1_average_read(*fn, *B, *Z, *I, AverageInterfaceType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_1to1_average_write_f, CG_1TO1_AVERAGE_WRITE_F) (int *fn, int *B,
    int *Z, int *I, AverageInterfaceType_t *AverageInterfaceType, int *ier) {

    *ier = cg_1to1_average_write(*fn, *B, *Z, *I, *AverageInterfaceType);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCDataSet_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_dataset_read_f, CG_DATASET_READ_F) (int *fn, int *B, int *Z,
    int *BC, int *DSet, STR_PSTR(Dataset_name), BCType_t *BCType,
    int *DirichletFlag, int *NeumannFlag, int *ier STR_PLEN(Dataset_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_dataset_read(*fn, *B, *Z, *BC, *DSet, c_name,
        BCType, DirichletFlag, NeumannFlag);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(Dataset_name), STR_LEN(Dataset_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_dataset_write_f, CG_DATASET_WRITE_F) (int *fn, int *B, int *Z,
    int *BC, STR_PSTR(Dataset_name), BCType_t *BCType, int *Dset,
    int *ier STR_PLEN(Dataset_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(Dataset_name), STR_LEN(Dataset_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("Dataset_name='%s'\n", c_name);
#endif

    *ier = cg_dataset_write(*fn, *B, *Z, *BC, c_name, *BCType, Dset);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_bcdataset_write_f, CG_BCDATASET_WRITE_F) (
    STR_PSTR(Dataset_name), BCType_t *BCType, BCDataType_t *BCDataType,
		 int *ier STR_PLEN(Dataset_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(Dataset_name), STR_LEN(Dataset_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;
#if DEBUG_FTOC
    printf("Dataset_name='%s'\n", c_name);
#endif

    *ier = cg_bcdataset_write(c_name, *BCType, *BCDataType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_bcdataset_info_f, CG_BCDATASET_INFO_F) (int *ndataset,
     int *ier STR_PLEN(Dataset_name))
{
    *ier = cg_bcdataset_info(ndataset);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_bcdataset_read_f, CG_BCDATASET_READ_F) (int *index,
    STR_PSTR(Dataset_name), BCType_t *BCType,
    int *DirichletFlag, int *NeumannFlag, int *ier STR_PLEN(Dataset_name))
{
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_bcdataset_read(*index, c_name, BCType, DirichletFlag, NeumannFlag);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(Dataset_name), STR_LEN(Dataset_name), ier);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCData_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_bcdata_write_f, CG_BCDATA_WRITE_F) (int *fn, int *B, int *Z,
    int *BC, int *Dset, BCDataType_t *BCDataType, int *ier) {

    *ier = cg_bcdata_write(*fn, *B, *Z, *BC, *Dset, *BCDataType);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RigidGridMotion_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_n_rigid_motions_f, CG_N_RIGID_MOTIONS_F) (int *fn, int *B,
    int *Z, int *n_rigid_motions, int *ier) {

    *ier = cg_n_rigid_motions(*fn, *B, *Z, n_rigid_motions);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_rigid_motion_read_f, CG_RIGID_MOTION_READ_F) (int *fn, int *B,
    int *Z, int *R, STR_PSTR(rmotion_name), RigidGridMotionType_t *type,
    int *ier STR_PLEN(rmotion_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_rigid_motion_read(*fn, *B, *Z, *R, c_name, type);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(rmotion_name), STR_LEN(rmotion_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_rigid_motion_write_f, CG_RIGID_MOTION_WRITE_F) (int *fn,
    int *B, int *Z, STR_PSTR(rmotion_name), RigidGridMotionType_t *type,
    int *R, int *ier STR_PLEN(rmotion_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(rmotion_name), STR_LEN(rmotion_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_rigid_motion_write(*fn, *B, *Z, c_name, *type, R);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ArbitraryGridMotion_t Nodes                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_n_arbitrary_motions_f, CG_N_ARBITRARY_MOTIONS_F) (int *fn,
    int *B, int *Z, int *n_arbitrary_motions, int *ier) {

    *ier = cg_n_arbitrary_motions(*fn, *B, *Z, n_arbitrary_motions);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_arbitrary_motion_read_f, CG_ARBITRARY_MOTION_READ_F)
    (int *fn, int *B, int *Z, int *A, STR_PSTR(amotion_name),
    ArbitraryGridMotionType_t *type, int *ier STR_PLEN(amotion_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_arbitrary_motion_read(*fn, *B, *Z, *A, c_name, type);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(amotion_name), STR_LEN(amotion_name), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_arbitrary_motion_write_f, CG_ARBITRARY_MOTION_WRITE_F) (int *fn,
    int *B, int *Z, STR_PSTR(amotion_name), ArbitraryGridMotionType_t *type,
    int *A, int *ier STR_PLEN(amotion_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(amotion_name), STR_LEN(amotion_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_arbitrary_motion_write(*fn, *B, *Z, c_name, *type, A);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_ngrids_f, CG_NGRIDS_F) (int *fn, int *B, int *Z, int *ngrids,
    int *ier){

    *ier = cg_ngrids(*fn, *B, *Z, ngrids);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_grid_read_f, CG_GRID_READ_F) (int *fn, int *B, int *Z,
    int *G, STR_PSTR(gridname), int *ier STR_PLEN(gridname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_grid_read(*fn, *B, *Z, *G, c_name);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(gridname), STR_LEN(gridname), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_grid_write_f, CG_GRID_WRITE_F) (int *fn, int *B, int *Z,
    STR_PSTR(gridname), int *G, int *ier STR_PLEN(gridname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(gridname), STR_LEN(gridname),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_grid_write(*fn, *B, *Z, c_name, G);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write SimulationType_t Node                             *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_simulation_type_read_f, CG_SIMULATION_TYPE_READ_F) (int *fn,
    int *B, SimulationType_t *type, int *ier) {

    *ier = cg_simulation_type_read(*fn, *B, type);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_simulation_type_write_f, CG_SIMULATION_TYPE_WRITE_F) (int *fn,
    int *B, SimulationType_t *type, int *ier) {

    *ier = cg_simulation_type_write(*fn, *B, *type);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BaseIterativeData_t Node                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_biter_read_f, CG_BITER_READ_F) (int *fn, int *B,
    STR_PSTR(bitername), int *nsteps, int *ier STR_PLEN(bitername)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_biter_read(*fn, *B, c_name, nsteps);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(bitername), STR_LEN(bitername), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_biter_write_f, CG_BITER_WRITE_F) (int *fn, int *B,
    STR_PSTR(bitername), int *nsteps, int *ier STR_PLEN(bitername)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(bitername), STR_LEN(bitername),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_biter_write(*fn, *B, c_name, *nsteps);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ZoneIterativeData_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_ziter_read_f, CG_ZITER_READ_F) (int *fn, int *B, int *Z,
    STR_PSTR(zitername), int *ier STR_PLEN(zitername)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_ziter_read(*fn, *B, *Z, c_name);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(zitername), STR_LEN(zitername), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ziter_write_f, CG_ZITER_WRITE_F) (int *fn, int *B, int *Z,
    STR_PSTR(zitername), int *ier STR_PLEN(zitername)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(zitername), STR_LEN(zitername),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_ziter_write(*fn, *B, *Z, c_name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Gravity_t Node                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_gravity_read_f, CG_GRAVITY_READ_F) (int *fn, int *B,
    float *gravity_vector, int *ier) {

    *ier = cg_gravity_read(*fn, *B, gravity_vector);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_gravity_write_f, CG_GRAVITY_WRITE_F) (int *fn, int *B,
    float *gravity_vector, int *ier) {

   *ier = cg_gravity_write(*fn, *B, gravity_vector);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Axisymmetry_t Node                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_axisym_read_f, CG_AXISYM_READ_F) (int *fn, int *B,
    float *ref_point, float *axis, int *ier) {

    *ier = cg_axisym_read(*fn, *B, ref_point, axis);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_axisym_write_f, CG_AXISYM_WRITE_F) (int *fn, int *B,
    float *ref_point, float *axis, int *ier) {

    *ier = cg_axisym_write(*fn, *B, ref_point, axis);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RotatingCoordinates_t Node                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_rotating_read_f, CG_ROTATING_READ_F) (float *rot_rate,
    float *rot_center, int *ier) {

    *ier = cg_rotating_read(rot_rate, rot_center);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_rotating_write_f, CG_ROTATING_WRITE_F) (float *rot_rate,
        float *rot_center, int *ier) {

        *ier = cg_rotating_write(rot_rate, rot_center);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write  IndexArray/Range_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_ptset_info_f, CG_PTSET_INFO_F) (PointSetType_t *ptset_type, int *npnts, int *ier) {

    *ier = cg_ptset_info(ptset_type, npnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ptset_read_f, CG_PTSET_READ_F) (int *pnts, int *ier) {

    *ier = cg_ptset_read(pnts);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ptset_write_f, CG_PTSET_WRITE_F) (PointSetType_t *ptset_type, int *npnts, int *pnts, int *ier) {

    *ier = cg_ptset_write(*ptset_type, *npnts, pnts);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Go - To Function                                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_goto_f(int *fn, int *B, int *ier, ...) {
#else
CGNSDLL void FMNAME(cg_goto_f, CG_GOTO_F)(int *fn, int *B, int *ier, ...) {
#endif

#ifdef _CRAY
    _fcd cray_string;
#endif
    char *f_label[CG_MAX_GOTO_DEPTH], *label[CG_MAX_GOTO_DEPTH];
    int index[CG_MAX_GOTO_DEPTH], n, i, len[CG_MAX_GOTO_DEPTH];
    va_list ap;

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

        index[n] = *(va_arg(ap, int *));
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

    *ier = cgi_set_posit(*fn, *B, n, index, label);

    for (i=0; i<n; i++) CGNS_FREE(label[i]);
    return;
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_gorel_f(int *fn, int *ier, ...) {
#else
CGNSDLL void FMNAME(cg_gorel_f, CG_GOREL_F)(int *fn, int *ier, ...) {
#endif

#ifdef _CRAY
    _fcd cray_string;
#endif
    char *f_label[CG_MAX_GOTO_DEPTH], *label[CG_MAX_GOTO_DEPTH];
    int index[CG_MAX_GOTO_DEPTH], n, i, len[CG_MAX_GOTO_DEPTH];
    va_list ap;

    if (posit == 0) {
        cgi_error ("position not set with cg_goto");
        *ier = CG_ERROR;
        return;
    }
    if (*fn != posit_file) {
        cgi_error("current position is in the wrong file");
        *ier = CG_ERROR;
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

        index[n] = *(va_arg(ap, int *));
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

    *ier = cgi_update_posit(n, index, label);

    for (i=0; i<n; i++) CGNS_FREE(label[i]);
    return;
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_gopath_f, CG_GOPATH_F) (int *fn,
    STR_PSTR(path), int *ier STR_PLEN(path)) {
    int length;
    char *c_path;

    length = (int) STR_LEN(path);
    c_path = CGNS_NEW(char, length+1);

    string_2_C_string(STR_PTR(path), STR_LEN(path), c_path, length, ier);
    if (*ier == 0)
        *ier = cg_gopath(*fn, c_path);
    CGNS_FREE(c_path);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *              Read Multiple path nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_famname_read_f, CG_FAMNAME_READ_F) (STR_PSTR(famname), int *ier
    STR_PLEN(famname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int ierr;

    *ier = cg_famname_read(c_name);
    if (*ier != CG_ERROR) {
        string_2_F_string(c_name, STR_PTR(famname), STR_LEN(famname), &ierr);
        if (ierr) *ier = ierr;
    }
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_convergence_read_f, CG_CONVERGENCE_READ_F) (int *iterations,
    STR_PSTR(NormDefinitions), int *ier STR_PLEN(NormDefinitions)) {
    char *c_descr_text;

    *ier = cg_convergence_read(iterations, &c_descr_text);
    if (*ier) return;
    string_2_F_string(c_descr_text, STR_PTR(NormDefinitions),
        STR_LEN(NormDefinitions), ier);
    free(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_state_size_f, CG_STATE_SIZE_F) (int *size, int *ier) {
    char *c_descr_text;

    *ier = cg_state_read(&c_descr_text);
    if (!*ier) {
        *size = strlen(c_descr_text);
        free(c_descr_text);
    }
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_state_read_f, CG_STATE_READ_F) (STR_PSTR(StateDescription), int *ier
    STR_PLEN(StateDescription)) {
    char *c_descr_text;

    *ier = cg_state_read(&c_descr_text);
    if (*ier) return;
    string_2_F_string(c_descr_text, STR_PTR(StateDescription),
        STR_LEN(StateDescription), ier);
    free(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_equationset_read_f, CG_EQUATIONSET_READ_F) (int *EquationDimension,
    int *GoverningEquationsFlag, int *GasModelFlag, int *ViscosityModelFlag,
    int *ThermalConductivityModelFlag, int *TurbulenceClosureFlag,
    int *TurbulenceModelFlag, int *ier) {

    *ier = cg_equationset_read(EquationDimension, GoverningEquationsFlag,
        GasModelFlag, ViscosityModelFlag, ThermalConductivityModelFlag,
        TurbulenceClosureFlag, TurbulenceModelFlag);
#if DEBUG_FTOC
    printf("in cg_ftoc, EquationDimension=%d\n",*EquationDimension);
#endif
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_equationset_chemistry_read_f, CG_EQUATIONSET_CHEMISTRY_READ_F) (
    int *ThermalRelaxationFlag, int *ChemicalKineticsFlag, int *ier) {

    *ier = cg_equationset_chemistry_read(ThermalRelaxationFlag, ChemicalKineticsFlag);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_equationset_elecmagn_read_f, CG_EQUATIONSET_ELECMAGN_READ_F) (
    int *ElecFldModelFlag, int *MagnFldModelFlag, int *ConductivityModelFlag,
    int *ier) {

    *ier = cg_equationset_elecmagn_read(ElecFldModelFlag, MagnFldModelFlag,
					ConductivityModelFlag);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_governing_read_f, CG_GOVERNING_READ_F) (GoverningEquationsType_t *EquationsType,
    int *ier) {

    *ier = cg_governing_read(EquationsType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_diffusion_read_f, CG_DIFFUSION_READ_F) (int *diffusion_model, int *ier) {

    *ier = cg_diffusion_read(diffusion_model);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_model_read_f, CG_MODEL_READ_F) (STR_PSTR(ModelLabel), ModelType_t *ModelType,
    int *ier STR_PLEN(ModelLabel)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(ModelLabel), STR_LEN(ModelLabel),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_model_read(c_name, ModelType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_narrays_f, CG_NARRAYS_F) (int *narrays, int *ier) {

    *ier = cg_narrays(narrays);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_array_info_f, CG_ARRAY_INFO_F) (int *A, STR_PSTR(ArrayName), DataType_t *DataType,
    int *DataDimension, int *DimensionVector, int *ier STR_PLEN(ArrayName)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_array_info(*A, c_name, DataType, DataDimension, DimensionVector);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(ArrayName), STR_LEN(ArrayName), ier);
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_array_read_f(int *A, void *Data, ...) {
    va_list ap;
    int *ier;
    int DataDimension, *DimensionVector;
    char *ArrayName;
    DataType_t DataType;

    ArrayName = CGNS_NEW(char,CGIO_MAX_NAME_LENGTH+1);
    DimensionVector = CGNS_NEW(int, 3);
    cg_array_info(*A, ArrayName, &DataType, &DataDimension, DimensionVector);

    va_start(ap, Data);
    if (DataType == Character) (void) va_arg(ap, int);
    ier = va_arg(ap, int *);
    va_end(ap);
#else
CGNSDLL void FMNAME(cg_array_read_f, CG_ARRAY_READ_F) (int *A, void *Data, int *ier) {
#endif

    *ier = cg_array_read(*A, Data);
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_array_read_as_f(int *A, DataType_t *type, void *Data, ...) {
    va_list ap;
    int *ier;
    va_start(ap, Data);
    if (*type == Character) (void) va_arg(ap, int);
    ier = va_arg(ap, int *);
    va_end(ap);
#else
CGNSDLL void FMNAME(cg_array_read_as_f, CG_ARRAY_READ_AS_F) (int *A, DataType_t *type,
        void *Data, int *ier) {
#endif

    *ier = cg_array_read_as(*A, *type, Data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_nintegrals_f, CG_NINTEGRALS_F) (int *nintegrals, int *ier) {

    *ier = cg_nintegrals(nintegrals);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_integral_read_f, CG_INTEGRAL_READ_F) (int *IntegralDataIndex,
    STR_PSTR(IntegralDataName), int *ier STR_PLEN(IntegralDataName)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_integral_read(*IntegralDataIndex, c_name);
    if (!*ier)
        string_2_F_string(c_name, STR_PTR(IntegralDataName),
            STR_LEN(IntegralDataName), ier);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_rind_read_f, CG_RIND_READ_F) (int *RindData, int *ier) {

    *ier = cg_rind_read(RindData);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ndescriptors_f, CG_NDESCRIPTORS_F) (int *ndescriptors, int *ier) {

    *ier = cg_ndescriptors(ndescriptors);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_descriptor_size_f, CG_DESCRIPTOR_SIZE_F) (int *descr_no,
    int *descr_size, int *ier) {
    char *c_descr_text;
    char descr_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_descriptor_read(*descr_no, descr_name, &c_descr_text);
    if (!*ier) {
        *descr_size = strlen(c_descr_text);
        free(c_descr_text);
    }
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_descriptor_read_f, CG_DESCRIPTOR_READ_F) (int *descr_no,
    STR_PSTR(descr_name), STR_PSTR(descr_text), int *ier
    STR_PLEN(descr_name)  STR_PLEN(descr_text)) {
    char *c_descr_text;
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    *ier = cg_descriptor_read(*descr_no, c_name, &c_descr_text);
    if (*ier) return;
#if DEBUG_FTOC
    printf("In cg_descriptor_read_f, descr_no=%d, descr_name='%s', c_descr_text='%s'\n",
        *descr_no, c_name, c_descr_text);
#endif
    string_2_F_string(c_name, STR_PTR(descr_name), STR_LEN(descr_name), ier);
    if (!*ier)
        string_2_F_string(c_descr_text, STR_PTR(descr_text),
            STR_LEN(descr_text), ier);
    free(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_nunits_f, CG_NUNITS_F) (int *nunits, int *ier) {

    *ier = cg_nunits(nunits);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_units_read_f, CG_UNITS_READ_F) (MassUnits_t *mass, LengthUnits_t *length,
    TimeUnits_t *time, TemperatureUnits_t *temperature, AngleUnits_t *angle, int *ier) {

    *ier = cg_units_read(mass, length, time, temperature, angle);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_unitsfull_read_f, CG_UNITSFULL_READ_F) (MassUnits_t *mass,
    LengthUnits_t *length, TimeUnits_t *time, TemperatureUnits_t *temperature,
    AngleUnits_t *angle, ElectricCurrentUnits_t *current,
    SubstanceAmountUnits_t *amount, LuminousIntensityUnits_t *intensity, int *ier) {

    *ier = cg_unitsfull_read(mass, length, time, temperature, angle,
        current, amount, intensity);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_exponents_info_f, CG_EXPONENTS_INFO_F) (DataType_t *DataType, int *ier) {

    *ier = cg_exponents_info(DataType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_nexponents_f, CG_NEXPONENTS_F) (int *nexps, int *ier) {
    *ier = cg_nexponents(nexps);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_exponents_read_f, CG_EXPONENTS_READ_F) (void *exponents, int *ier) {

    *ier = cg_exponents_read(exponents);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_expfull_read_f, CG_EXPFULL_READ_F) (void *exponents, int *ier) {

    *ier = cg_expfull_read(exponents);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conversion_info_f, CG_CONVERSION_INFO_F) (DataType_t *DataType, int *ier) {

    *ier = cg_conversion_info(DataType);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conversion_read_f, CG_CONVERSION_READ_F) (void *ConversionFactors,
    int *ier) {

    *ier = cg_conversion_read(ConversionFactors);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_dataclass_read_f, CG_DATACLASS_READ_F) (DataClass_t *dataclass, int *ier) {

    *ier = cg_dataclass_read(dataclass);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_gridlocation_read_f, CG_GRIDLOCATION_READ_F) (GridLocation_t *GridLocation,
    int *ier) {

    *ier = cg_gridlocation_read(GridLocation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ordinal_read_f, CG_ORDINAL_READ_F) (int *Ordinal, int *ier) {

    *ier = cg_ordinal_read(Ordinal);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_npe_f, CG_NPE_F) (ElementType_t *type, int *npe, int *ier) {

    *ier = cg_npe(*type, npe);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_is_link_f, CG_IS_LINK_F) (int *path_length, int *ier) {

    *ier = cg_is_link(path_length);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_link_read_f, CG_LINK_READ_F) (
    STR_PSTR(filename), STR_PSTR(link_path), int *ier
    STR_PLEN(filename)  STR_PLEN(link_path)) {
    char *f_name, *l_name;

    *ier = cg_link_read(&f_name, &l_name);
    if (*ier) return;
    string_2_F_string(f_name, STR_PTR(filename), STR_LEN(filename), ier);
    if (!*ier)
        string_2_F_string(l_name, STR_PTR(link_path), STR_LEN(link_path), ier);
    free(f_name);
    free(l_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_nuser_data_f, CG_NUSER_DATA_F) (int *nuser_data, int *ier) {

    *ier = cg_nuser_data(nuser_data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_user_data_read_f, CG_USER_DATA_READ_F) (int *index, STR_PSTR(dataname),
    int *ier STR_PLEN(dataname)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];
    int ierr;

    *ier = cg_user_data_read(*index, c_name);
    if (*ier != CG_ERROR) {
        string_2_F_string(c_name, STR_PTR(dataname), STR_LEN(dataname), &ierr);
        if (ierr) *ier = ierr;
    }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *                   Write Multiple path nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_famname_write_f, CG_FAMNAME_WRITE_F) (STR_PSTR(family_name),
    int *ier STR_PLEN(family_name)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(family_name), STR_LEN(family_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_famname_write(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_convergence_write_f, CG_CONVERGENCE_WRITE_F) (int *iterations,
    STR_PSTR(NormDefinitions), int *ier STR_PLEN(NormDefinitions)) {
    char *c_string;
    int len;

    len = STR_LEN(NormDefinitions);
     /* convert Fortran-text-string to a C-string */
    c_string = CGNS_NEW(char, len+1);
    string_2_C_string(STR_PTR(NormDefinitions), len, c_string, len, ier);
    if (!*ier) {
#if DEBUG_FTOC
        printf("In cg_ftoc: c_NormDefinitions = '%s'",c_string);
#endif
        *ier = cg_convergence_write(*iterations, c_string);
    }
    free(c_string);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_state_write_f, CG_STATE_WRITE_F) (STR_PSTR(StateDescription), int *ier
    STR_PLEN(StateDescription)) {
    char *c_string;
    int len;

    len = STR_LEN(StateDescription);
     /* convert Fortran-text-string to a C-string */
    c_string = CGNS_NEW(char, len+1);
    string_2_C_string(STR_PTR(StateDescription), len, c_string, len, ier);
    if (!*ier) {
#if DEBUG_FTOC
        printf("In cg_ftoc: C_StateDescription = '%s'",c_string);
#endif
        *ier = cg_state_write(c_string);
    }
    free(c_string);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_equationset_write_f, CG_EQUATIONSET_WRITE_F) (int *EquationDimension, int *ier) {

#if DEBUG_FTOC
    printf("In cg_ftoc: EquationDimension=%d\n",*EquationDimension);
#endif

    *ier = cg_equationset_write(*EquationDimension);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_governing_write_f, CG_GOVERNING_WRITE_F) (GoverningEquationsType_t *Equationstype,
    int *ier) {

    *ier = cg_governing_write(*Equationstype);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_diffusion_write_f, CG_DIFFUSION_WRITE_F) (int *diffusion_model, int *ier) {

    *ier = cg_diffusion_write(diffusion_model);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_model_write_f, CG_MODEL_WRITE_F) (STR_PSTR(ModelLabel), ModelType_t *ModelType,
    int *ier STR_PLEN(ModelLabel)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(ModelLabel), STR_LEN(ModelLabel),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_model_write(c_name, *ModelType);
}

/*-----------------------------------------------------------------------*/

#ifdef WIN32_FORTRAN
CGNSDLL void __stdcall cg_array_write_f(STR_PSTR(ArrayName), DataType_t *DataType,
    int *DataDimension, int *DimensionVector, void *Data, ...) {
    va_list ap;
    int *ier;
    char c_name[CGIO_MAX_NAME_LENGTH+1];

    va_start(ap, Data);
    if (*DataType == Character) (void) va_arg(ap, int);
    ier = va_arg(ap, int *);
    va_end(ap);
#else
CGNSDLL void FMNAME(cg_array_write_f, CG_ARRAY_WRITE_F) (STR_PSTR(ArrayName), DataType_t *DataType,
    int *DataDimension, int *DimensionVector, void *Data, int *ier STR_PLEN(ArrayName)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];
#endif

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(ArrayName), STR_LEN(ArrayName),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_array_write(c_name, *DataType, *DataDimension,
            DimensionVector, Data);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_integral_write_f, CG_INTEGRAL_WRITE_F) (STR_PSTR(IntegralDataName), int *ier
    STR_PLEN(IntegralDataName)) {
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(IntegralDataName), STR_LEN(IntegralDataName),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_integral_write(c_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_rind_write_f, CG_RIND_WRITE_F) (int *RindData, int *ier) {

    *ier = cg_rind_write(RindData);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_descriptor_write_f, CG_DESCRIPTOR_WRITE_F) (STR_PSTR(descr_name),
    STR_PSTR(descr_text), int *ier  STR_PLEN(descr_name) STR_PLEN(descr_text)) {
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
    if (!*ier) {
#if DEBUG_FTOC
        printf("c_descr_name='%s', c_descr_text='%s'\n",c_descr_name, c_descr_text);
#endif

         /* Call C-routine */
        *ier = cg_descriptor_write(c_descr_name, c_descr_text);
    }
    free(c_descr_text);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_units_write_f, CG_UNITS_WRITE_F) (MassUnits_t *mass, LengthUnits_t *length,
    TimeUnits_t *time, TemperatureUnits_t *temperature, AngleUnits_t *angle, int *ier) {

    *ier = cg_units_write(*mass, *length, *time, *temperature, *angle);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_unitsfull_write_f, CG_UNITSFULL_WRITE_F) (MassUnits_t *mass,
    LengthUnits_t *length, TimeUnits_t *time, TemperatureUnits_t *temperature,
    AngleUnits_t *angle, ElectricCurrentUnits_t *current,
    SubstanceAmountUnits_t *amount, LuminousIntensityUnits_t *intensity, int *ier) {

    *ier = cg_unitsfull_write(*mass, *length, *time, *temperature, *angle,
        *current, *amount, *intensity);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_exponents_write_f, CG_EXPONENTS_WRITE_F) (DataType_t *DataType, void *exponents,
    int *ier) {

    *ier = cg_exponents_write(*DataType, exponents);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_expfull_write_f, CG_EXPFULL_WRITE_F) (DataType_t *DataType, void *exponents, int *ier)
{

    *ier = cg_expfull_write(*DataType, exponents);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_conversion_write_f, CG_CONVERSION_WRITE_F) (DataType_t *DataType,
    void *ConversionFactors, int *ier) {

    *ier = cg_conversion_write(*DataType, ConversionFactors);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_dataclass_write_f, CG_DATACLASS_WRITE_F) (DataClass_t *dataclass, int *ier) {

    *ier = cg_dataclass_write(*dataclass);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_gridlocation_write_f, CG_GRIDLOCATION_WRITE_F) (GridLocation_t *GridLocation,
    int *ier) {

    *ier = cg_gridlocation_write(*GridLocation);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_ordinal_write_f, CG_ORDINAL_WRITE_F) (int *Ordinal, int *ier) {

    *ier = cg_ordinal_write(*Ordinal);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_link_write_f, CG_LINK_WRITE_F) (
    STR_PSTR(nodename), STR_PSTR(filename), STR_PSTR(name_in_file), int *ier
    STR_PLEN(nodename)  STR_PLEN(filename)  STR_PLEN(name_in_file)) {
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

    *ier = cg_link_write(n_name, f_name, i_name);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_user_data_write_f, CG_USER_DATA_WRITE_F) (
    STR_PSTR(dataname), int *ier
    STR_PLEN(dataname) ) {
    char d_name[CGIO_MAX_NAME_LENGTH+1];

    string_2_C_string(STR_PTR(dataname), STR_LEN(dataname),
        d_name, CGIO_MAX_NAME_LENGTH, ier);
    if (!*ier)
        *ier = cg_user_data_write(d_name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      General Delete Function                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_delete_node_f, CG_DELETE_NODE_F) (STR_PSTR(node_name), int *ier
    STR_PLEN(node_name)) {
/* ici */
    char c_name[CGIO_MAX_NAME_LENGTH+1];

     /* convert Fortran-text-string to a C-string */
    string_2_C_string(STR_PTR(node_name), STR_LEN(node_name),
        c_name, CGIO_MAX_NAME_LENGTH, ier);
    if (*ier) return;

    *ier = cg_delete_node(c_name);

#if DEBUG_FTOC
    printf("\n  Deleting node ='%s'\n", c_name);
#endif
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Error Handling Functions                                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL void FMNAME(cg_get_error_f, CG_GET_ERROR_F) (STR_PSTR(errmsg) STR_PLEN(errmsg)) {
    int ierr;

    string_2_F_string ((char *)cg_get_error(), STR_PTR(errmsg),
        STR_LEN(errmsg), &ierr);
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_error_exit_f, CG_ERROR_EXIT_F) () {
    cg_error_exit();
}

/*-----------------------------------------------------------------------*/

CGNSDLL void FMNAME(cg_error_print_f, CG_ERROR_PRINT_F) () {
    cg_error_print();
}

