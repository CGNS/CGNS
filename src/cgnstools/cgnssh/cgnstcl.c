#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#ifdef _WIN32
#define NOCASECMP _stricmp
#else
#define NOCASECMP strcasecmp
#endif

#include "tcl.h"
#include "cgnslib.h"
#include "cgns_header.h"

#ifndef CONST
# define CONST
#endif

#if !defined(CGNS_VERSION) || CGNS_VERSION < 2500
# error must use CGNS Version 2.5 or greater
#endif

static char buff[1024];

/*-----------------------------------------------------------------------*/

static int get_cg_error (Tcl_Interp *interp, char *funcname)
{
    Tcl_AppendResult (interp, cg_get_error(), NULL);
    return TCL_ERROR;
}

/*-----------------------------------------------------------------------*/

static int sizeof_datatype (DataType_t dtype)
{
    if (dtype == Character)  return sizeof(char);
    if (dtype == Integer)    return sizeof(int);
    if (dtype == RealSingle) return sizeof(float);
    if (dtype == RealDouble) return sizeof(double);
    return 0;
}

/*-----------------------------------------------------------------------*/

static int get_data_size (Tcl_Interp *interp, DataType_t type,
    int ndim, int *dims, int *count, int *bytes)
{
    int n;

    *count = *bytes = 0;
    if (ndim) {
        *count = 1;
        for (n = 0; n < ndim; n++)
            *count *= dims[n];
    }
    if (*count < 1) {
        Tcl_AppendResult (interp, "dimension is not 1 or greater", NULL);
        return 1;
    }
    *bytes = sizeof_datatype (type);
    if (*bytes == 0) {
        Tcl_AppendResult (interp, "invalid data type", NULL);
        return 1;
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int extract_data (Tcl_Interp *interp, char *list, DataType_t type,
    int *count, void **data)
{
    int n, cnt, bytes;
    CONST char **args;

    bytes = sizeof_datatype (type);
    if (bytes == 0) {
        Tcl_AppendResult (interp, "invalid data type", NULL);
        return 1;
    }
    if (TCL_OK != Tcl_SplitList (interp, list, &cnt, &args))
        return 1;
    if (cnt < 1) {
        Tcl_AppendResult (interp, "data length less than 1", NULL);
        return 1;
    }
    if ((*data = malloc (cnt * bytes)) == NULL) {
        Tcl_AppendResult (interp, "malloc failed for data array", NULL);
        return 1;
    }
    if (type == Character) {
        char *c = (char *)*data;
        for (n = 0; n < cnt; n++)
            c[n] = (char) atoi (args[n]);
    }
    else if (type == Integer) {
        int *i = (int *)*data;
        for (n = 0; n < cnt; n++)
            i[n] = (int) atoi (args[n]);
    }
    else if (type == RealSingle) {
        float *f = (float *)*data;
        for (n = 0; n < cnt; n++)
            f[n] = (float) atof (args[n]);
    }
    else {
        double *d = (double *)*data;
        for (n = 0; n < cnt; n++)
            d[n] = (double) atof (args[n]);
    }
    Tcl_Free ((char *)args);
    *count = cnt;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int construct_data (Tcl_Interp *interp, DataType_t type,
    int count, void *data)
{
    int n;

    if (type == Character) {
        char *c = (char *)data;
        for (n = 0; n < count; n++) {
            sprintf (buff, "%c", c[n]);
            Tcl_AppendElement (interp, buff);
        }
    }
    else if (type == Integer) {
        int *i = (int *)data;
        for (n = 0; n < count; n++) {
            sprintf (buff, "%d", i[n]);
            Tcl_AppendElement (interp, buff);
        }
    }
    else if (type == RealSingle) {
        float *f = (float *)data;
        for (n = 0; n < count; n++) {
            sprintf (buff, "%g", f[n]);
            Tcl_AppendElement (interp, buff);
        }
    }
    else if (type == RealDouble) {
        double *d = (double *)data;
        for (n = 0; n < count; n++) {
            sprintf (buff, "%g", d[n]);
            Tcl_AppendElement (interp, buff);
        }
    }
    else {
        Tcl_AppendResult (interp, "invalid data type", NULL);
        return 1;
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int check_name (Tcl_Interp *interp, char *desc, char *name)
{
    int len = strlen (name);

    if (len < 1 || len > 32) {
        Tcl_AppendResult (interp, "invalid length for ", desc, "name", NULL);
        return 1;
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int get_type (Tcl_Interp *interp, char *desc, char *name, int cnt,
    const char **list, int *type)
{
    int n;

    for (n = 0; n < cnt; n++) {
        if (0 == NOCASECMP (name, list[n])) {
            *type = n;
            return 0;
        }
    }
    Tcl_AppendResult (interp, desc, " type \"", name, "\" is invalid", NULL);
    return 1;
}

/*-----------------------------------------------------------------------*/

static int get_data_type (Tcl_Interp *interp, char *name, DataType_t *datatype)
{
    int type;

    if (get_type (interp, "data", name,
            NofValidDataTypes, DataTypeName, &type)) return 1;
    *datatype = (DataType_t)type;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int count_elements (Tcl_Interp *interp, ElementType_t type,
    int cnt, int *elements, int nelems)
{
    int npe, ne, n;

    if (type <= ElementTypeUserDefined || type >= NofValidElementTypes) {
        Tcl_AppendResult (interp, "can't handle element type ",
            cg_ElementTypeName(type), NULL);
        return 1;
    }
    if (type < MIXED) {
        cg_npe (type, &npe);
        if (nelems * npe > cnt) {
            sprintf (buff, "needs %d values", nelems * npe);
            Tcl_AppendResult (interp,
                "insufficient element data - ", buff, NULL);
            return 1;
        }
        return 0;
    }
    if (type == MIXED) {
        for (n = 0, ne = 0; ne < nelems; ne++) {
            type = (ElementType_t)elements[n++];
            if (type > NGON_n) {
                npe = type - NGON_n;
            }
            else if (cg_npe(type, &npe) || npe <= 0) {
                Tcl_AppendResult (interp, "can't handle element type ",
                    cg_ElementTypeName(type), " as MIXED element", NULL);
                return 1;
            }
        }
        n += npe;
        if (n > cnt) {
            Tcl_AppendResult (interp,
                "insufficient mixed element data", NULL);
            return 1;
        }
        return 0;
    }
#if CGNS_VERSION < 3000
    Tcl_AppendResult (interp, "can't handle element type ",
        cg_ElementTypeName(type), NULL);
    return 1;
#else
    for (n = 0, ne = 0; ne < nelems; ne++) {
        npe = elements[n++];
        n += npe;
        if (n > cnt) {
            Tcl_AppendResult (interp, "insufficient ",
                cg_ElementTypeName(type), " element data", NULL);
            return 1;
        }
    }
#endif
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_physical_dim (int cgfile, int cgbase, int *phys_dim)
{
    cgns_file *file;
    cgns_base *base;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    base = cgi_get_base (file, cgbase);
    if (base == NULL) return 1;
    *phys_dim = base->phys_dim;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_cell_dim (int cgfile, int cgbase, int *cell_dim)
{
    cgns_file *file;
    cgns_base *base;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    base = cgi_get_base (file, cgbase);
    if (base == NULL) return 1;
    *cell_dim = base->cell_dim;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_index_dim (int cgfile, int cgbase, int cgzone, int *index_dim)
{
    cgns_file *file;
    cgns_zone *zone;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    zone = cgi_get_zone (file, cgbase, cgzone);
    if (zone == NULL) return 1;
    *index_dim = zone->index_dim;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_coord_dim (int cgfile, int cgbase, int cgzone,
    int *index_dim, int *dims)
{
    int n;
    cgns_file *file;
    cgns_zone *zone;
    cgns_zcoor *zcoor;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    zone = cgi_get_zone (file, cgbase, cgzone);
    if (zone == NULL) return 1;
    zcoor = cgi_get_zcoorGC (file, cgbase, cgzone);
    if (zcoor == NULL) return 1;

    *index_dim = zone->index_dim;
    return cgi_datasize(zone->index_dim, zone->nijk, Vertex,
               zcoor->rind_planes, dims);
}

/*-----------------------------------------------------------------------*/

static int cg_section_dim (int cgfile, int cgbase, int cgzone,
    int cgsect, ElementType_t *type, int *start, int *end)
{
    int n;
    cgns_file *file;
    cgns_zone *zone;
    cgns_section *section;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    zone = cgi_get_zone (file, cgbase, cgzone);
    if (zone == NULL) return 1;
    section = cgi_get_section(file, cgbase, cgzone, cgsect);
    if (section == NULL) return 1;

    *type  = section->el_type;
    *start = section->range[0];
    *end   = section->range[1];
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_solution_dim (int cgfile, int cgbase, int cgzone, int cgsoln,
    int *index_dim, int *dims)
{
    int n;
    cgns_file *file;
    cgns_zone *zone;
    cgns_sol *sol;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    zone = cgi_get_zone (file, cgbase, cgzone);
    if (zone == NULL) return 1;
    sol = cgi_get_sol (file, cgbase, cgzone, cgsoln);
    if (sol == NULL) return 1;

    *index_dim = zone->index_dim;
    return cgi_datasize(zone->index_dim, zone->nijk, sol->location,
               sol->rind_planes, dims);
}

/*-----------------------------------------------------------------------*/

static int cg_hole_size (int cgfile, int cgbase, int cgzone,
    int cghole, int *size)
{
    int n;
    cgns_file *file;
    cgns_hole *hole;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    hole = cgi_get_hole (file, cgbase, cgzone, cghole);
    if (hole == NULL) return 1;

    *size = 0;
    for (n = 0; n < hole->nptsets; n++)
        *size += hole->ptset[n].npts;
    *size *= file->base[cgbase-1].zone[cgzone-1].index_dim;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_conn_dim (int cgfile, int cgbase, int cgzone, int cgconn,
    int *cnt, int *size, int *dsize, DataType_t *dtype)
{
    cgns_file *file;
    cgns_conn *conn;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    conn = cgi_get_conn (file, cgbase, cgzone, cgconn);
    if (conn == NULL) return 1;

    *cnt = conn->ptset.npts * file->base[cgbase-1].zone[cgzone-1].index_dim;
    *size = conn->ptset.size_of_patch;
    if (conn->dptset.npts == 0) {
        *dsize = 0;
        *dtype = DataTypeNull;
    }
    else {
        int n, idim = 0;
        for (n = 0; n < file->base[cgbase-1].nzones; n++) {
            if (strcmp (file->base[cgbase-1].zone[n].name, conn->donor) == 0) {
                idim = file->base[cgbase-1].zone[n].type == Structured ?
                       file->base[cgbase-1].cell_dim : 1;
                break;
            }
        }
        if (idim == 0) {
            cgi_error ("cg_conn_read:donor zone %s does not exist",
                conn->donor);
            return 1;
        }
        *dtype = cgi_datatype(conn->dptset.data_type);
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_boco_dim (int cgfile, int cgbase, int cgzone, int cgboco,
    int *pcnt, int *psize, int *ncnt, DataType_t *ntype)
{
    cgns_file *file;
    cgns_boco *boco;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    boco = cgi_get_boco (file, cgbase, cgzone, cgboco);
    if (boco == NULL) return 1;

    *pcnt = boco->ptset->npts * file->base[cgbase-1].zone[cgzone-1].index_dim;
    *psize = boco->ptset->size_of_patch;
    if (boco->normal == NULL) {
        *ncnt = 0;
        *ntype = DataTypeNull;
    }
    else {
        *ncnt = boco->ptset->size_of_patch * file->base[cgbase-1].phys_dim;
        *ntype = cgi_datatype(boco->normal->data_type);
    }
    return 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      LIBRARY FUNCTIONS						 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_library_version (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    sprintf (buff, "%d", CGNSLibVersion);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_get_names (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, cnt;
    const char **names;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " type_t", NULL);
        return TCL_ERROR;
    }
    if (0 == NOCASECMP (argv[1], "MassUnits_t")) {
        cnt = NofValidMassUnits;
        names = MassUnitsName;
    }
    else if (0 == NOCASECMP (argv[1], "LengthUnits_t")) {
        cnt = NofValidLengthUnits;
        names = LengthUnitsName;
    }
    else if (0 == NOCASECMP (argv[1], "TimeUnits_t")) {
        cnt = NofValidTimeUnits;
        names = TimeUnitsName;
    }
    else if (0 == NOCASECMP (argv[1], "TemperatureUnits_t")) {
        cnt = NofValidTemperatureUnits;
        names = TemperatureUnitsName;
    }
    else if (0 == NOCASECMP (argv[1], "AngleUnits_t")) {
        cnt = NofValidAngleUnits;
        names = AngleUnitsName;
    }
    else if (0 == NOCASECMP (argv[1], "ElectricCurrentUnits_t")) {
        cnt = NofValidElectricCurrentUnits;
        names = ElectricCurrentUnitsName;
    }
    else if (0 == NOCASECMP (argv[1], "SubstanceAmountUnits_t")) {
        cnt = NofValidSubstanceAmountUnits;
        names = SubstanceAmountUnitsName;
    }
    else if (0 == NOCASECMP (argv[1], "LuminousIntensityUnits_t")) {
        cnt = NofValidLuminousIntensityUnits;
        names = LuminousIntensityUnitsName;
    }
    else if (0 == NOCASECMP (argv[1], "DataClass_t")) {
        cnt = NofValidDataClass;
        names = DataClassName;
    }
    else if (0 == NOCASECMP (argv[1], "GridLocation_t")) {
        cnt = NofValidGridLocation;
        names = GridLocationName;
    }
    else if (0 == NOCASECMP (argv[1], "BCDataType_t")) {
        cnt = NofValidBCDataTypes;
        names = BCDataTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "GridConnectivityType_t")) {
        cnt = NofValidGridConnectivityTypes;
        names = GridConnectivityTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "PointSetType_t")) {
        cnt = NofValidPointSetTypes;
        names = PointSetTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "GoverningEquationsType_t")) {
        cnt = NofValidGoverningEquationsTypes;
        names = GoverningEquationsTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "ModelType_t")) {
        cnt = NofValidModelTypes;
        names = ModelTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "BCType_t")) {
        cnt = NofValidBCTypes;
        names = BCTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "DataType_t")) {
        cnt = NofValidDataTypes;
        names = DataTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "ElementType_t")) {
        cnt = NofValidElementTypes;
        names = ElementTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "ZoneType_t")) {
        cnt = NofValidZoneTypes;
        names = ZoneTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "RigidGridMotionType_t")) {
        cnt = NofValidRigidGridMotionTypes;
        names = RigidGridMotionTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "ArbitraryGridMotionType_t")) {
        cnt = NofValidArbitraryGridMotionTypes;
        names = ArbitraryGridMotionTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "SimulationType_t")) {
        cnt = NofValidSimulationTypes;
        names = SimulationTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "WallFunctionType_t")) {
        cnt = NofValidWallFunctionTypes;
        names = WallFunctionTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "AreaType_t")) {
        cnt = NofValidAreaTypes;
        names = AreaTypeName;
    }
    else if (0 == NOCASECMP (argv[1], "AverageInterfaceType_t")) {
        cnt = NofValidAverageInterfaceTypes;
        names = AverageInterfaceTypeName;
    }
    else {
        Tcl_AppendResult (interp, "type \"", argv[1], "\" not known", NULL);
        return TCL_ERROR;
    }

    for (n = 0; n < cnt; n++)
        Tcl_AppendElement (interp, names[n]);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_is_cgns (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
#if CGNS_VERSION >= 3000
    int file_type;
#endif

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filename", NULL);
        return TCL_ERROR;
    }
#if CGNS_VERSION >= 3000
    ierr = cg_is_cgns(argv[1], &file_type);
    if (file_type == CG_FILE_ADF)
        sprintf (buff, "%d ADF", ierr ? 0 : 1);
    else if (file_type == CG_FILE_HDF5)
        sprintf (buff, "%d HDF5", ierr ? 0 : 1);
    else if (file_type == CG_FILE_XML)
        sprintf (buff, "%d XML", ierr ? 0 : 1);
    else
        sprintf (buff, "%d unknown", ierr ? 0 : 1);
#else
    ierr = cg_is_cgns(argv[1]);
    sprintf (buff, "%d", ierr ? 0 : 1);
#endif
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_open (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int mode, cgfile;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filename mode", NULL);
        return TCL_ERROR;
    }
    if (argv[2][0] == 'r' || argv[2][0] == 'R')
        mode = CG_MODE_READ;
    else if (argv[2][0] == 'w' || argv[2][0] == 'W')
        mode = CG_MODE_WRITE;
    else if (argv[2][0] == 'm' || argv[2][0] == 'M')
        mode = CG_MODE_MODIFY;
    else {
        Tcl_AppendResult (interp,
            "invalid mode: should be r[ead], w[rite] or m[odify]", NULL);
        return TCL_ERROR;
    }

    if (cg_open (argv[1], mode, &cgfile))
        return get_cg_error (interp, "cg_open");

    sprintf (buff, "%d", cgfile);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_version (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    float version;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filenum", NULL);
        return TCL_ERROR;
    }

    if (cg_version (atoi(argv[1]), &version))
        return get_cg_error (interp, "cg_version");

    sprintf (buff, "%d", (int)(1000.0 * version + 0.5));
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_close (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filenum", NULL);
        return TCL_ERROR;
    }

    if (cg_close (atoi(argv[1])))
        return get_cg_error (interp, "cg_close");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write CGNSBase_t Nodes					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nbases (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nbases;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nbases (atoi(argv[1]), &nbases))
        return get_cg_error (interp, "cg_nbases");

    sprintf (buff, "%d", nbases);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_base_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    int cell, phy;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_base_read (atoi(argv[1]), atoi(argv[2]), name, &cell, &phy))
        return get_cg_error (interp, "cg_base_read");

    Tcl_AppendElement (interp, name);
    sprintf (buff, "%d", cell);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", phy);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_base_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_base_id (atoi(argv[1]), atoi(argv[2]), &id))
        return get_cg_error (interp, "cg_base_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_base_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgbase;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basename celldim phydim", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "base", argv[2])) return TCL_ERROR;

    if (cg_base_write (atoi(argv[1]), argv[2],
            atoi(argv[3]), atoi(argv[4]), &cgbase))
        return get_cg_error (interp, "cg_base_write");

    sprintf (buff, "%d", cgbase);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Zone_t Nodes    					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nzones (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nzones;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nzones (atoi(argv[1]), atoi(argv[2]), &nzones))
        return get_cg_error (interp, "cg_nzones");

    sprintf (buff, "%d", nzones);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_zone_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char *p, name[33];
    int n, idim, sizes[9];
    int cgfile, cgbase, cgzone;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim) ||
        cg_zone_read (cgfile, cgbase, cgzone, name, sizes))
        return get_cg_error (interp, "cg_zone_read");

    Tcl_AppendElement (interp, name);
    sprintf (buff, "%d", sizes[0]);
    for (p = buff, n = 1; n < 3 * idim; n++) {
        p += strlen (p);
        sprintf (p, " %d", sizes[n]);
    }
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_zone_type (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    ZoneType_t type;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_zone_type (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &type))
        return get_cg_error (interp, "cg_zone_type");

    Tcl_AppendResult (interp, cg_ZoneTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_zone_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_zone_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &id))
        return get_cg_error (interp, "cg_zone_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_zone_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, cgfile, cgbase, cgzone;
    int cdim, len, *sizes;
    ZoneType_t type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonename sizes zonetype", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi (argv[1]);
    cgbase = atoi (argv[2]);
    if (check_name (interp, "zone", argv[3])) return TCL_ERROR;
    if (0 == NOCASECMP (argv[5], "Structured"))
        type = Structured;
    else if (0 == NOCASECMP (argv[5], "Unstructured"))
        type = Unstructured;
    else {
        Tcl_AppendResult (interp, "invalid zone type", NULL);
        return TCL_ERROR;
    }

    if (cg_cell_dim (cgfile, cgbase, &cdim))
        return get_cg_error (interp, "cg_zone_write");
    if (extract_data (interp, argv[4], Integer, &len, (void **)&sizes))
        return TCL_ERROR;
    if ((type == Structured && len != 3 * cdim) ||
        (type == Unstructured && len != 3)) {
        free (sizes);
        sprintf (buff, "must be %d", type == Structured ? 3 * cdim : 3);
        Tcl_AppendResult (interp,
            "size array length invalid - ", buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_zone_write (cgfile, cgbase, argv[3], sizes, type, &cgzone);
    free (sizes);
    if (ierr) return get_cg_error (interp, "cg_zone_write");

    sprintf (buff, "%d", cgzone);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Family_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nfamilies (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nfam;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nfamilies (atoi(argv[1]), atoi(argv[2]), &nfam))
        return get_cg_error (interp, "cg_nfamilies");

    sprintf (buff, "%d", nfam);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_family_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    int nboco, ngeo;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum", NULL);
        return TCL_ERROR;
    }

    if (cg_family_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            name, &nboco, &ngeo))
        return get_cg_error (interp, "cg_family_read");

    Tcl_AppendElement (interp, name);
    sprintf (buff, "%d", nboco);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", ngeo);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_family_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfam;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familyname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "family", argv[3])) return TCL_ERROR;

    if (cg_family_write (atoi(argv[1]), atoi(argv[2]), argv[3], &cgfam))
        return get_cg_error (interp, "cg_family_write");

    sprintf (buff, "%d", cgfam);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FamilyName_t Nodes                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_famname_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_famname_read (name);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_famname_read");
    }

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_famname_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " familyname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "family", argv[1])) return TCL_ERROR;

    if (cg_famname_write (argv[1]))
        return get_cg_error (interp, "cg_famname_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FamilyBC_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_fambc_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    BCType_t bctype;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum bcnum", NULL);
        return TCL_ERROR;
    }

    if (cg_fambc_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &bctype))
        return get_cg_error (interp, "cg_fambc_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_BCTypeName(bctype));
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_fambc_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type, cgbc;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum bcname bctype", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "fambc", argv[4]) ||
        get_type (interp, "BC", argv[5], NofValidBCTypes, BCTypeName, &type))
        return TCL_ERROR;

    if (cg_fambc_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            argv[4], (BCType_t)type, &cgbc))
        return get_cg_error (interp, "cg_fambc_write");

    sprintf (buff, "%d", cgbc);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryReference_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_geo_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int npart;
    char name[33], *filename, cadname[33];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum geonum", NULL);
        return TCL_ERROR;
    }

    if (cg_geo_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &filename, cadname, &npart))
        return get_cg_error (interp, "cg_geo_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, filename);
    Tcl_AppendElement (interp, cadname);
    sprintf (buff, "%d", npart);
    Tcl_AppendElement (interp, buff);
    free (filename);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_geo_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cggeo;

    Tcl_ResetResult (interp);
    if (argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum geoname filename cadsystem", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "geo", argv[4]) ||
        check_name (interp, "cadsystem", argv[6])) return TCL_ERROR;
    if (strlen (argv[5]) < 1) {
        Tcl_AppendResult (interp, "filename not given", NULL);
        return TCL_ERROR;
    }

    if (cg_geo_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            argv[4], argv[5], argv[6], &cggeo))
        return get_cg_error (interp, "cg_geo_write");

    sprintf (buff, "%d", cggeo);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryEntity_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_part_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum geonum partnum", NULL);
        return TCL_ERROR;
    }

    if (cg_part_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), name))
        return get_cg_error (interp, "cg_part_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_part_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgpart;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum geonum partname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "part", argv[5])) return TCL_ERROR;

    if (cg_part_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), argv[5], &cgpart))
        return get_cg_error (interp, "cg_part_write");

    sprintf (buff, "%d", cgpart);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ngrids (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ngrids;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_ngrids (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &ngrids))
        return get_cg_error (interp, "cg_ngrids");

    sprintf (buff, "%d", ngrids);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_grid_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum gridnum", NULL);
        return TCL_ERROR;
    }

    if (cg_grid_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name))
        return get_cg_error (interp, "cg_grid_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_grid_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cggrid;
    char gridname[33];

    Tcl_ResetResult (interp);
    if (argc < 4 || argc > 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum [gridname]", NULL);
        return TCL_ERROR;
    }
    if (argc == 5) {
        if (check_name (interp, "grid", argv[4])) return TCL_ERROR;
        strcpy (gridname, argv[4]);
    }
    else
        strcpy (gridname, "GridCoordinates");

    if (cg_grid_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            gridname, &cggrid))
        return get_cg_error (interp, "cg_grid_write");

    sprintf (buff, "%d", cggrid);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t/DataArray_t Nodes               *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ncoords (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ncoor;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_ncoords (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &ncoor))
        return get_cg_error (interp, "cg_ncoords");

    sprintf (buff, "%d", ncoor);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_coord_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum coordnum", NULL);
        return TCL_ERROR;
    }

    if (cg_coord_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &type, name))
        return get_cg_error (interp, "cg_coord_info");

    Tcl_AppendElement (interp, cg_DataTypeName(type));
    Tcl_AppendElement (interp, name);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_coord_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone;
    int n, index_dim, dims[3];
    int cnt, rmin[3], rmax[3];
    DataType_t datatype;
    void *coords;

    Tcl_ResetResult (interp);
    if (argc != 6 && argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum coordname datatype [rmin rmax]", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "coord", argv[4]) ||
        get_data_type (interp, argv[5], &datatype)) return TCL_ERROR;
    if (datatype != RealSingle && datatype != RealDouble) {
        Tcl_AppendResult (interp,
            "datatype not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_coord_dim (cgfile, cgbase, cgzone, &index_dim, dims))
        return get_cg_error (interp, "cg_coord_read");

    cnt = 1;
    if (argc == 8) {
        if (index_dim != sscanf (argv[6], "%d%d%d",
                &rmin[0], &rmin[1], &rmin[2]) ||
            index_dim != sscanf (argv[7], "%d%d%d",
                &rmax[0], &rmax[1], &rmax[2])) {
            Tcl_AppendResult (interp,
                "invalid number rmin and/or rmax values", NULL);
            return TCL_ERROR;
        }
        for (n = 0; n < index_dim; n++) {
            if (rmin[n] < 1 || rmax[n] > dims[n] || rmin[n] > rmax[n]) {
                Tcl_AppendResult (interp,
                    "rmin and/or rmax value are invalid", NULL);
                return TCL_ERROR;
            }
            cnt *= (rmax[n] - rmin[n] + 1);
        }
    }
    else {
        for (n = 0; n < index_dim; n++) {
            rmin[n] = 1;
            rmax[n] = dims[n];
            cnt *= rmax[n];
        }
    }

    if ((coords = malloc (cnt * sizeof_datatype(datatype))) == NULL) {
        Tcl_AppendResult (interp, "malloc failed for coordinates", NULL);
        return TCL_ERROR;
    }

    if (cg_coord_read (cgfile, cgbase, cgzone, argv[4],
            datatype, rmin, rmax, coords)) {
        free (coords);
        return get_cg_error (interp, "cg_coord_read");
    }

    n = construct_data (interp, datatype, cnt, coords);
    free (coords);
    return n ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_coord_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum coordnum", NULL);
        return TCL_ERROR;
    }

    if (cg_coord_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_coord_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_coord_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, cgfile, cgbase, cgzone, cgcoord;
    int ierr, cnt, index_dim, dims[3];
    int rmin[3], rmax[3];
    DataType_t datatype;
    void *coords;

    Tcl_ResetResult (interp);
    if (argc != 7 && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum datatype coordname [rmin rmax] coords",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "coord", argv[5]) ||
        get_data_type (interp, argv[4], &datatype)) return TCL_ERROR;
    if (datatype != RealSingle && datatype != RealDouble) {
        Tcl_AppendResult (interp,
            "datatype not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_coord_dim (cgfile, cgbase, cgzone, &index_dim, dims))
        return get_cg_error (interp, "cg_coord_read");

    if (argc == 9) {
        if (index_dim != sscanf (argv[7], "%d%d%d",
                &rmin[0], &rmin[1], &rmin[2]) ||
            index_dim != sscanf (argv[8], "%d%d%d",
                &rmax[0], &rmax[1], &rmax[2])) {
            Tcl_AppendResult (interp,
                "invalid number rmin and/or rmax values", NULL);
            return TCL_ERROR;
        }
        for (n = 0; n < index_dim; n++) {
            if (rmin[n] < 1 || rmax[n] > dims[n] || rmin[n] > rmax[n]) {
                Tcl_AppendResult (interp,
                    "rmin and/or rmax value are invalid", NULL);
                return TCL_ERROR;
            }
            dims[n] = (rmax[n] - rmin[n] + 1);
        }
    }
    for (cnt = 1, n = 0; n < index_dim; n++)
        cnt *= dims[n];

    if (extract_data (interp, argv[argc-1], datatype, &n, &coords))
        return TCL_ERROR;
    if (n < cnt) {
        free (coords);
        sprintf (buff, "needs %d", cnt);
        Tcl_AppendResult (interp,
            "insufficient number of coordinate values - ", buff, NULL);
        return TCL_ERROR;
    }

    if (argc == 9)
        ierr = cg_coord_partial_write (cgfile, cgbase, cgzone, datatype,
                   argv[5], rmin, rmax, coords, &cgcoord);
    else
        ierr = cg_coord_write (cgfile, cgbase, cgzone, datatype,
                   argv[5], coords, &cgcoord);
    free (coords);
    if (ierr) return get_cg_error (interp, "cg_coord_write");

    sprintf (buff, "%d", cgcoord);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Elements_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nsections (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nsect;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nsections (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nsect))
        return get_cg_error (interp, "cg_nsections");

    sprintf (buff, "%d", nsect);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_section_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    ElementType_t type;
    int start, end, nbndry, parent;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum", NULL);
        return TCL_ERROR;
    }

    if (cg_section_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &type, &start, &end, &nbndry, &parent))
        return get_cg_error (interp, "cg_section_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_ElementTypeName(type));
    sprintf (buff, "%d", start);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", end);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nbndry);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", parent);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_elements_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    ElementType_t type;
    int ierr, ns, ne;
    int cgfile, cgbase, cgzone, cgsect;
    int start, end, nbndry, haspar, size;
    int *elements;

    Tcl_ResetResult (interp);
    if (argc != 5 && argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum [start end]", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsect = atoi(argv[4]);

    if (cg_section_read (cgfile, cgbase, cgzone, cgsect,
            name, &type, &start, &end, &nbndry, &haspar))
        return get_cg_error (interp, "cg_elements_read");

    ns = start;
    ne = end;
    if (argc == 7) {
        ns = atoi(argv[5]);
        ne = atoi(argv[6]);
        if (ns > ne || ns < start || ne > end) {
            Tcl_AppendResult (interp, "invalid element range", NULL);
            return TCL_ERROR;
        }
        if (cg_ElementPartialSize (cgfile, cgbase, cgzone, cgsect,
                ns, ne, &size))
            return get_cg_error (interp, "cg_elements_read");
    }
    else {
        if (cg_ElementDataSize (cgfile, cgbase, cgzone, cgsect, &size))
            return get_cg_error (interp, "cg_elements_read");
    }

    elements = (int *) malloc (size * sizeof(int));
    if (elements == NULL) {
        Tcl_AppendResult (interp, "malloc failed for elements", NULL);
        return TCL_ERROR;
    }

    if (ns == start && ne == end) {
        ierr = cg_elements_read (cgfile, cgbase, cgzone, cgsect, elements, 0);
    }
    else {
        ierr = cg_elements_partial_read (cgfile, cgbase, cgzone, cgsect,
                                         ns, ne, elements, 0);
    }
    if (ierr) {
        free (elements);
        return get_cg_error (interp, "cg_elements_read");
    }
    ierr = construct_data (interp, Integer, size, elements);
    free (elements);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_parent_data_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    ElementType_t type;
    int ierr, nelem, ns, ne;
    int cgfile, cgbase, cgzone, cgsect;
    int start, end, nbndry, haspar, size;
    int *elements, *parent;

    Tcl_ResetResult (interp);
    if (argc != 5 && argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum [start end]", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsect = atoi(argv[4]);

    if (cg_section_read (cgfile, cgbase, cgzone, cgsect,
            name, &type, &start, &end, &nbndry, &haspar))
        return get_cg_error (interp, "cg_parent_data_read");

    if (!haspar) return TCL_OK;

    ns = start;
    ne = end;
    if (argc == 7) {
        ns = atoi(argv[5]);
        ne = atoi(argv[6]);
        if (ns > ne || ns < start || ne > end) {
            Tcl_AppendResult (interp, "invalid element range", NULL);
            return TCL_ERROR;
        }
        if (cg_ElementPartialSize (cgfile, cgbase, cgzone, cgsect,
                ns, ne, &size))
            return get_cg_error (interp, "cg_parent_data_read");
    }
    else {
        if (cg_ElementDataSize (cgfile, cgbase, cgzone, cgsect, &size))
            return get_cg_error (interp, "cg_parent_data_read");
    }

    nelem = ne - ns + 1;
    elements = (int *) malloc (size * sizeof(int));
    if (elements == NULL) {
        Tcl_AppendResult (interp, "malloc failed for elements", NULL);
        return TCL_ERROR;
    }
    parent = (int *) malloc (4 * nelem * sizeof(int));
    if (parent == NULL) {
        free (elements);
        Tcl_AppendResult (interp, "malloc failed for parent data", NULL);
        return TCL_ERROR;
    }

    if (ns == start && ne == end) {
        ierr = cg_elements_read (cgfile, cgbase, cgzone, cgsect, elements, parent);
    }
    else {
        ierr = cg_elements_partial_read (cgfile, cgbase, cgzone, cgsect,
                                         ns, ne, elements, parent);
    }
    free (elements);
    if (ierr) {
        free (parent);
        return get_cg_error (interp, "cg_parent_data_read");
    }
    ierr = construct_data (interp, Integer, 4 * nelem, parent);
    free (parent);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_section_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, type, cnt, start, end, nbndry, cgsect;
    void *elements;

    Tcl_ResetResult (interp);
#if CGNS_VERSION < 3000
    if (argc != 10) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectname elementtype",
            " start end nbndry elements", NULL);
        return TCL_ERROR;
    }
#else
    if (argc < 9 || argc > 10) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectname elementtype",
            " start end nbndry [elements]", NULL);
        return TCL_ERROR;
    }
#endif
    if (check_name (interp, "section", argv[4]) ||
        get_type (interp, "element", argv[5], NofValidElementTypes,
            ElementTypeName, &type)) return TCL_ERROR;
    start = atoi(argv[6]);
    end = atoi(argv[7]);
    nbndry = atoi(argv[8]);

#if CGNS_VERSION >= 3000
    if (argc == 9) {
        if (cg_section_partial_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
                argv[4], (ElementType_t)type, start, end, nbndry, &cgsect))
            return get_cg_error (interp, "cg_section_write");
        sprintf (buff, "%d", cgsect);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_OK;
    }
#endif
    if (extract_data (interp, argv[9], Integer, &cnt, &elements) ||
        count_elements (interp, (ElementType_t)type, cnt,
            (int *)elements, end - start + 1))
        return TCL_ERROR;

    ierr = cg_section_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
               argv[4], (ElementType_t)type, start, end, nbndry,
               elements, &cgsect);
    free (elements);
    if (ierr) return get_cg_error (interp, "cg_section_write");

    sprintf (buff, "%d", cgsect);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_elements_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    ElementType_t type;
    int ierr, cgfile, cgbase, cgzone, cgsect;
    int start, end, size, ns, ne;
    void *elements;

    Tcl_ResetResult (interp);
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum start end elements", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsect = atoi(argv[4]);

    if (cg_section_dim (cgfile, cgbase, cgzone, cgsect, &type, &start, &end))
        return get_cg_error (interp, "cg_elements_write");

    ns = atoi(argv[5]);
    ne = atoi(argv[6]);
    if (ns > ne || ns < start || ne > end) {
        Tcl_AppendResult (interp, "invalid element range", NULL);
        return TCL_ERROR;
    }

    if (extract_data (interp, argv[7], Integer, &size, &elements) ||
        count_elements (interp, type, size, (int *)elements, ne - ns + 1))
        return TCL_ERROR;

#if CGNS_VERSION < 3000
    ierr = cg_section_partial_write (cgfile, cgbase, cgzone, name, type,
               ns, ne, nbndry, elements, &cgsect);
#else
    ierr = cg_elements_partial_write (cgfile, cgbase, cgzone, cgsect,
               ns, ne, elements);
#endif
    free (elements);
    if (ierr) return get_cg_error (interp, "cg_elements_write");
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_parent_data_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    ElementType_t type;
    int ierr, cgfile, cgbase, cgzone, cgsect;
    int start, end, size, ns, ne;
    void *parent;

    Tcl_ResetResult (interp);
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum start end parentdata", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsect = atoi(argv[4]);

    if (cg_section_dim (cgfile, cgbase, cgzone, cgsect, &type, &start, &end))
        return get_cg_error (interp, "cg_parent_data_write");

    ns = atoi(argv[5]);
    ne = atoi(argv[6]);
    if (ns > ne || ns < start || ne > end) {
        Tcl_AppendResult (interp, "invalid element range", NULL);
        return TCL_ERROR;
    }

    if (extract_data (interp, argv[7], Integer, &size, &parent))
        return TCL_ERROR;
    if (size < 4 * (ne - ns + 1)) {
        free (parent);
        sprintf (buff, "needs %d values - got %d", 4 * (ne - ns + 1), size);
        Tcl_AppendResult (interp,
            "insufficient parent data - ", buff, NULL);
        return TCL_ERROR;
    }

    if (ns == start && ne == end)
        ierr = cg_parent_data_write (cgfile, cgbase, cgzone, cgsect, parent);
    else
        ierr = cg_parent_data_partial_write (cgfile, cgbase, cgzone,
                   cgsect, ns, ne, parent);
    free (parent);
    if (ierr) return get_cg_error (interp, "cg_parent_data_write");
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_npe (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type, npe;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " elementtype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "element", argv[1], NofValidElementTypes,
        ElementTypeName, &type)) return TCL_ERROR;

    if (cg_npe ((ElementType_t)type, &npe))
        return get_cg_error (interp, "cg_npe");

    sprintf (buff, "%d", npe);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ElementDataSize (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int size, ierr;

    Tcl_ResetResult (interp);
    if (argc != 5 && argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum [start end]", NULL);
        return TCL_ERROR;
    }

    if (argc == 7) {
        ierr = cg_ElementPartialSize(atoi(argv[1]), atoi(argv[2]),
                   atoi(argv[3]), atoi(argv[4]),
                   atoi(argv[5]), atoi(argv[5]), &size);
    }
    else {
        ierr = cg_ElementDataSize(atoi(argv[1]), atoi(argv[2]),
                   atoi(argv[3]), atoi(argv[4]), &size);
    }
    if (ierr)
        return get_cg_error (interp, "cg_ElementDataSize");

    sprintf (buff, "%d", size);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FlowSolution_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nsols (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nsol;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nsols (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nsol))
        return get_cg_error (interp, "cg_nsols");

    sprintf (buff, "%d", nsol);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_sol_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    GridLocation_t location;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum", NULL);
        return TCL_ERROR;
    }

    if (cg_sol_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &location))
        return get_cg_error (interp, "cg_sol_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_GridLocationName(location));
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_sol_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum", NULL);
        return TCL_ERROR;
    }

    if (cg_sol_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_coord_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_sol_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgsoln, location;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solname location", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "solution", argv[4]) ||
        get_type (interp, "location", argv[5], NofValidGridLocation,
            GridLocationName, &location)) return TCL_ERROR;

    if (cg_sol_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            argv[4], (GridLocation_t)location, &cgsoln))
        return get_cg_error (interp, "cg_sol_write");

    sprintf (buff, "%d", cgsoln);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write solution DataArray_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nfields (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nfield;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum", NULL);
        return TCL_ERROR;
    }

    if (cg_nfields (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &nfield))
        return get_cg_error (interp, "cg_nfields");

    sprintf (buff, "%d", nfield);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_field_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum fieldnum", NULL);
        return TCL_ERROR;
    }

    if (cg_field_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), &type, name))
        return get_cg_error (interp, "cg_field_info");

    Tcl_AppendElement (interp, cg_DataTypeName(type));
    Tcl_AppendElement (interp, name);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_field_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone, cgsoln;
    int n, index_dim, dims[3];
    int cnt, rmin[3], rmax[3];
    DataType_t datatype;
    void *field;

    Tcl_ResetResult (interp);
    if (argc != 7 && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum fieldname datatype [rmin rmax]",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "field", argv[5]) ||
        get_data_type (interp, argv[6], &datatype)) return TCL_ERROR;
    if (datatype != RealSingle && datatype != RealDouble) {
        Tcl_AppendResult (interp,
            "datatype not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsoln = atoi(argv[4]);

    if (cg_solution_dim (cgfile, cgbase, cgzone, cgsoln, &index_dim, dims))
        return get_cg_error (interp, "cg_field_read");

    cnt = 1;
    if (argc == 9) {
        if (index_dim != sscanf (argv[6], "%d%d%d",
                &rmin[0], &rmin[1], &rmin[2]) ||
            index_dim != sscanf (argv[7], "%d%d%d",
                &rmax[0], &rmax[1], &rmax[2])) {
            Tcl_AppendResult (interp,
                "invalid number rmin and/or rmax values", NULL);
            return TCL_ERROR;
        }
        for (n = 0; n < index_dim; n++) {
            if (rmin[n] < 1 || rmax[n] > dims[n] || rmin[n] > rmax[n]) {
                Tcl_AppendResult (interp,
                    "rmin and/or rmax value are invalid", NULL);
                return TCL_ERROR;
            }
            cnt *= (rmax[n] - rmin[n] + 1);
        }
    }
    else {
        for (n = 0; n < index_dim; n++) {
            rmin[n] = 1;
            rmax[n] = dims[n];
            cnt *= rmax[n];
        }
    }

    if ((field = malloc (cnt * sizeof_datatype(datatype))) == NULL) {
        Tcl_AppendResult (interp, "malloc failed for field data", NULL);
        return TCL_ERROR;
    }

    if (cg_field_read (cgfile, cgbase, cgzone, cgsoln, argv[5],
            datatype, rmin, rmax, field)) {
        free (field);
        return get_cg_error (interp, "cg_field_read");
    }

    n = construct_data (interp, datatype, cnt, field);
    free (field);
    return n ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_field_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum fieldnum", NULL);
        return TCL_ERROR;
    }

    if (cg_field_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), &id))
        return get_cg_error (interp, "cg_field_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_field_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, cgfile, cgbase, cgzone, cgsoln, cgfield;
    int ierr, cnt, index_dim, dims[3];
    int rmin[3], rmax[3];
    DataType_t datatype;
    void *field;

    Tcl_ResetResult (interp);
    if (argc != 8 && argc != 10) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum datatype",
            " fieldname [rmin rmax] field_data",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "field", argv[6]) ||
        get_data_type (interp, argv[5], &datatype)) return TCL_ERROR;
    if (datatype != Integer && datatype != RealSingle &&
        datatype != RealDouble) {
        Tcl_AppendResult (interp,
            "datatype not Integer, RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsoln = atoi(argv[4]);

    if (cg_solution_dim (cgfile, cgbase, cgzone, cgsoln, &index_dim, dims))
        return get_cg_error (interp, "cg_field_read");

    if (argc == 10) {
        if (index_dim != sscanf (argv[8], "%d%d%d",
                &rmin[0], &rmin[1], &rmin[2]) ||
            index_dim != sscanf (argv[9], "%d%d%d",
                &rmax[0], &rmax[1], &rmax[2])) {
            Tcl_AppendResult (interp,
                "invalid number rmin and/or rmax values", NULL);
            return TCL_ERROR;
        }
        for (n = 0; n < index_dim; n++) {
            if (rmin[n] < 1 || rmax[n] > dims[n] || rmin[n] > rmax[n]) {
                Tcl_AppendResult (interp,
                    "rmin and/or rmax value are invalid", NULL);
                return TCL_ERROR;
            }
            dims[n] = (rmax[n] - rmin[n] + 1);
        }
    }
    for (cnt = 1, n = 0; n < index_dim; n++)
        cnt *= dims[n];

    if (extract_data (interp, argv[argc-1], datatype, &n, &field))
        return TCL_ERROR;
    if (n < cnt) {
        free (field);
        sprintf (buff, "needs %d", cnt);
        Tcl_AppendResult (interp,
            "insufficient number of field values - ", buff, NULL);
        return TCL_ERROR;
    }

    if (argc == 10)
        ierr = cg_field_partial_write (cgfile, cgbase, cgzone, cgsoln,
                   datatype, argv[6], rmin, rmax, field, &cgfield);
    else
        ierr = cg_field_write (cgfile, cgbase, cgzone, cgsoln,
                   datatype, argv[6], field, &cgfield);
    free (field);
    if (ierr) return get_cg_error (interp, "cg_field_write");

    sprintf (buff, "%d", cgfield);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write OversetHoles_t Nodes  				 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nholes (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nhole;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nholes (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nhole))
        return get_cg_error (interp, "cg_nholes");

    sprintf (buff, "%d", nhole);
    Tcl_AppendResult (interp, buff, NULL);

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_hole_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    GridLocation_t location;
    PointSetType_t ptsettype;
    int nptsets, npnts;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum holenum", NULL);
        return TCL_ERROR;
    }

    if (cg_hole_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &location, &ptsettype, &nptsets, &npnts))
        return get_cg_error (interp, "cg_hole_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_GridLocationName(location));
    Tcl_AppendElement (interp, cg_PointSetTypeName(ptsettype));
    sprintf (buff, "%d", nptsets);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", npnts);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_hole_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone, cghole;
    int ierr, size, *pts;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum holenum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cghole = atoi(argv[4]);

    if (cg_hole_size (cgfile, cgbase, cgzone, cghole, &size))
        return get_cg_error (interp, "cg_hole_read");
    if ((pts = malloc (size * sizeof(int))) == NULL) {
        Tcl_AppendResult (interp, "malloc failed for points", NULL);
        return TCL_ERROR;
    }

    if (cg_hole_read (cgfile, cgbase, cgzone, cghole, pts)) {
        free (pts);
        return get_cg_error (interp, "cg_hole_read");
    }

    ierr = construct_data (interp, Integer, size, pts);
    free (pts);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_hole_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum holenum", NULL);
        return TCL_ERROR;
    }
    if (cg_hole_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_hole_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_hole_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone, cghole;
    int ierr, location, ptype, nsets, npnts, cnt, idim;
    void *pnts;

    Tcl_ResetResult (interp);
    if (argc != 10) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum holename location",
            " ptsettype nptsets npts pntdata", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "hole", argv[4]) ||
        get_type (interp, "location", argv[5], NofValidGridLocation,
            GridLocationName, &location) ||
        get_type (interp, "ptset", argv[6], NofValidPointSetTypes,
            PointSetTypeName, &ptype)) return TCL_ERROR;
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim))
        return get_cg_error (interp, "cg_hole_write");

    if (extract_data (interp, argv[9], Integer, &cnt, &pnts))
        return TCL_ERROR;
    nsets = atoi(argv[7]);
    npnts = atoi(argv[8]);
    if (cnt < nsets * npnts * idim) {
        free (pnts);
        sprintf (buff, "needs %d values", nsets * npnts * idim);
        Tcl_AppendResult (interp,
            "insufficient point data - ", buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_hole_write (cgfile, cgbase, cgzone, argv[4],
              (GridLocation_t)location, (PointSetType_t)ptype,
               nsets, npnts, pnts, &cghole);
    free (pnts);
    if (ierr) return get_cg_error (interp, "cg_hole_write");

    sprintf (buff, "%d", cghole);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity_t Nodes                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nconns (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nconn;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nconns (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nconn))
        return get_cg_error (interp, "cg_nconns");

    sprintf (buff, "%d", nconn);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33], dname[33];
    GridLocation_t location;
    GridConnectivityType_t conntype;
    PointSetType_t ptsettype, dptsettype;
    ZoneType_t dztype;
    DataType_t datatype;
    int npts, dnpts;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum connnum", NULL);
        return TCL_ERROR;
    }

    if (cg_conn_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &location, &conntype, &ptsettype, &npts,
            dname, &dztype, &dptsettype, &datatype, &dnpts))
        return get_cg_error (interp, "cg_conn_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_GridLocationName(location));
    Tcl_AppendElement (interp, cg_GridConnectivityTypeName(conntype));
    Tcl_AppendElement (interp, cg_PointSetTypeName(ptsettype));
    sprintf (buff, "%d", npts);
    Tcl_AppendElement (interp, buff);
    Tcl_AppendElement (interp, dname);
    Tcl_AppendElement (interp, cg_ZoneTypeName(dztype));
    Tcl_AppendElement (interp, cg_PointSetTypeName(dptsettype));
    Tcl_AppendElement (interp, cg_DataTypeName(datatype));
    sprintf (buff, "%d", dnpts);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone, cgconn;
    int get_donor = (int)((size_t)data);
    int ierr, cnt, size, dsize;
    DataType_t dtype;
    void *pnts, *donor = NULL;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum connnum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgconn = atoi(argv[4]);

    if (cg_conn_dim (cgfile, cgbase, cgzone, cgconn,
            &cnt, &size, &dsize, &dtype))
        return get_cg_error (interp, "cg_conn_read");

    if (cnt == 0) return TCL_OK;
    if (get_donor && dsize == 0) return TCL_OK;

    pnts = malloc (cnt * sizeof(int));
    if (pnts == NULL) {
        Tcl_AppendResult (interp, "malloc failed for points", NULL);
        return TCL_ERROR;
    }
    if (dsize) {
        donor = malloc (dsize * sizeof_datatype(dtype));
        if (donor == NULL) {
            free (pnts);
            Tcl_AppendResult (interp, "malloc failed for donor data", NULL);
            return TCL_ERROR;
        }
    }

    if (cg_conn_read (cgfile, cgbase, cgzone, cgconn, pnts, dtype, donor)) {
        free (pnts);
        if (donor != NULL) free (donor);
        return get_cg_error (interp, "cg_conn_read");
    }

    if (get_donor)
        ierr = construct_data (interp, dtype, dsize, donor);
    else
        ierr = construct_data (interp, Integer, cnt, pnts);
    free (pnts);
    if (donor != NULL) free (donor);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum connnum", NULL);
        return TCL_ERROR;
    }
    if (cg_conn_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_conn_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone, cgconn;
    int ierr, idim, cdim, cnt;
    int location, ctype, ptype, npnts;
    int dztype, dptype, ddtype, ndonor;
    void *pnts, *donor;

    Tcl_ResetResult (interp);
    if (argc != 11 && argc != 16) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum connname location conntype",
            " ptsettype npnts ptdata donorname [donorzonetype",
            " donorptsettype donordatatype ndonor donordata]",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "conn", argv[4]) ||
        get_type (interp, "location", argv[5], NofValidGridLocation,
            GridLocationName, &location) ||
        get_type (interp, "conntype", argv[6], NofValidGridConnectivityTypes,
            GridConnectivityTypeName, &ctype) ||
        get_type (interp, "ptset", argv[7], NofValidPointSetTypes,
            PointSetTypeName, &ptype) ||
        check_name (interp, "donor", argv[10])) return TCL_ERROR;
    
    if (argc == 16) {
        if (get_type (interp, "donorzone", argv[11], NofValidZoneTypes,
                ZoneTypeName, &dztype) ||
            get_type (interp, "donorptset", argv[12], NofValidPointSetTypes,
                PointSetTypeName, &dptype) ||
            get_type (interp, "donordata", argv[13], NofValidDataTypes,
                DataTypeName, &ddtype)) return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    npnts  = atoi(argv[8]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim))
        return get_cg_error (interp, "cg_conn_write");
    if ((ZoneType_t)dztype == Unstructured)
        cdim = 1;
    else {
        if (cg_cell_dim (cgfile, cgbase, &cdim))
            return get_cg_error (interp, "cg_conn_write");
    }

    if (extract_data (interp, argv[9], Integer, &cnt, &pnts))
        return TCL_ERROR;
    if (cnt < npnts * idim) {
        free (pnts);
        sprintf (buff, "needs %d values", npnts * idim);
        Tcl_AppendResult (interp,
            "insufficient point data - ", buff, NULL);
        return TCL_ERROR;
    }

    if (argc == 11) {
        ierr = cg_conn_write_short (cgfile, cgbase, cgzone, argv[4],
                   (GridLocation_t)location, (GridConnectivityType_t)ctype,
                  (PointSetType_t)ptype, npnts, pnts, argv[10], &cgconn);
        free (pnts);
        if (ierr) return get_cg_error(interp, "cg_conn_write_short");
    }
    else {
        ndonor = atoi(argv[14]);
        if (extract_data (interp, argv[15], (DataType_t)ddtype, &cnt, &donor)) {
            free(pnts);
            return TCL_ERROR;
        }
        if (cnt < ndonor * cdim) {
            free (pnts);
            free (donor);
            sprintf (buff, "needs %d values", ndonor * cdim);
            Tcl_AppendResult (interp,
                "insufficient point data - ", buff, NULL);
            return TCL_ERROR;
        }
        ierr = cg_conn_write (cgfile, cgbase, cgzone, argv[4],
                  (GridLocation_t)location, (GridConnectivityType_t)ctype,
                  (PointSetType_t)ptype, npnts, pnts, argv[10],
                  (ZoneType_t)dztype, (PointSetType_t)dptype,
                  (DataType_t)ddtype, ndonor, donor, &cgconn);
        free (pnts);
        free (donor);
        if (ierr) return get_cg_error (interp, "cg_conn_write");
    }

    sprintf (buff, "%d", cgconn);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity1to1_t Nodes in a zone            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_n1to1 (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n1to1;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_n1to1 (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &n1to1))
        return get_cg_error (interp, "cg_n1to1");

    sprintf (buff, "%d", n1to1);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, idim, cgfile, cgbase, cgzone;
    char *p, name[33], dname[33];
    int range[6], drange[6], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum one21num", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim) ||
        cg_1to1_read (cgfile, cgbase, cgzone, atoi(argv[4]),
            name, dname, range, drange, trans))
        return get_cg_error (interp, "cg_1to1_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, dname);
    sprintf (buff, "%d", range[0]);
    for (p = buff, n = 1; n < 2 * idim; n++) {
        p += strlen(p);
        sprintf (p, " %d", range[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", drange[0]);
    for (p = buff, n = 1; n < 2 * idim; n++) {
        p += strlen(p);
        sprintf (p, " %d", drange[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", trans[0]);
    for (p = buff, n = 1; n < idim; n++) {
        p += strlen(p);
        sprintf (p, " %d", trans[n]);
    }
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum one21num", NULL);
        return TCL_ERROR;
    }
    if (cg_1to1_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_1to1_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone, cgconn;
    int n, *r, nr, idim;
    char *p, name[33], dname[33];
    int range[6], drange[6], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum one21name donorname",
            " range donorrange transform", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "one21", argv[4]) ||
        check_name (interp, "donor", argv[5])) return TCL_ERROR;
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim))
        return get_cg_error (interp, "cg_1to1_write");

    if (extract_data (interp, argv[6], Integer, &nr, (void **)&r))
        return TCL_ERROR;
    if (nr < 2 * idim) {
        free (r);
        sprintf (buff, "needs %d values", 2 * idim);
        Tcl_AppendResult (interp,
            "insufficient range data - ", buff, NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < nr; n++)
        range[n] = r[n];
    free (r);

    if (extract_data (interp, argv[7], Integer, &nr, (void **)&r))
        return TCL_ERROR;
    if (nr < 2 * idim) {
        free (r);
        sprintf (buff, "needs %d values", 2 * idim);
        Tcl_AppendResult (interp,
            "insufficient donor range data - ", buff, NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < nr; n++)
        drange[n] = r[n];
    free (r);

    if (extract_data (interp, argv[8], Integer, &nr, (void **)&r))
        return TCL_ERROR;
    if (nr < idim) {
        free (r);
        sprintf (buff, "needs %d values", idim);
        Tcl_AppendResult (interp,
            "insufficient transform data - ", buff, NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < nr; n++)
        trans[n] = r[n];
    free (r);

    if (cg_1to1_write (cgfile, cgbase, cgzone, argv[4], argv[5],
            range, drange, trans, &cgconn))
        return get_cg_error (interp, "cg_1to1_write");

    sprintf (buff, "%d", cgconn);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read all GridConnectivity1to1_t Nodes of a base                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_n1to1_global (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n1to1;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_n1to1_global (atoi(argv[1]), atoi(argv[2]), &n1to1))
        return get_cg_error (interp, "cg_n1to1_global");

    sprintf (buff, "%d", n1to1);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_read_global (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, n, n1to1;
    int file, base;
    char *cdata;
    char **cname, **zname, **dname;
    int *idata;
    int **range, **drang, **trans;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }
    file = atoi(argv[1]);
    base = atoi(argv[2]);

    if (cg_n1to1_global (file, base, &n1to1))
        return get_cg_error (interp, "cg_n1to1_global");
    if (n1to1 < 1) return TCL_OK;

    cdata = (char *) malloc (n1to1 * 3 * 33);
    cname = (char **) malloc (n1to1 * sizeof(char *));
    zname = (char **) malloc (n1to1 * sizeof(char *));
    dname = (char **) malloc (n1to1 * sizeof(char *));
    idata = (int *) malloc (n1to1 * 15 * sizeof(int));
    range = (int **) malloc (n1to1 * sizeof(int *));
    drang = (int **) malloc (n1to1 * sizeof(int *));
    trans = (int **) malloc (n1to1 * sizeof(int *));
    if (cdata == NULL || cname == NULL || zname == NULL || dname == NULL ||
        idata == NULL || range == NULL || drang == NULL || trans == NULL) {
        if (cdata != NULL) free (cdata);
        if (cname != NULL) free (cname);
        if (zname != NULL) free (zname);
        if (dname != NULL) free (dname);
        if (idata != NULL) free (idata);
        if (range != NULL) free (range);
        if (drang != NULL) free (drang);
        if (trans != NULL) free (trans);
        Tcl_AppendResult(interp, "malloc failed for 1to1 connectivity data", NULL);
        return TCL_ERROR;
    }

    for (n = 0; n < n1to1; n++) {
        cname[n] = &cdata[n * 33];
        zname[n] = &cdata[(n + n1to1) * 33];
        dname[n] = &cdata[(n + 2 * n1to1) * 33];
        range[n] = &idata[n * 6];
        drang[n] = &idata[(n + n1to1) * 6];
        trans[n] = &idata[(2 * n1to1) * 6 + n * 3];
    }

    ierr = cg_1to1_read_global(file, base, cname, zname, dname,
               range, drang, trans);
    if (ierr) {
        get_cg_error(interp, "cg_1to1_read_global");
    }
    else {
        for (n = 0; n < n1to1; n++) {
            sprintf(buff, "{%s} {%s} {%s} {%d %d %d %d %d %d} "
                "{%d %d %d %d %d %d} {%d %d %d}", cname[n], zname[n], dname[n],
                range[n][0], range[n][1], range[n][2],
                range[n][3], range[n][4], range[n][5],
                drang[n][0], drang[n][1], drang[n][2],
                drang[n][3], drang[n][4], drang[n][5],
                trans[n][0], trans[n][1], trans[n][2]);
            Tcl_AppendElement(interp, buff);
        }
    }
    free (cdata);
    free (cname);
    free (zname);
    free (dname);
    free (idata);
    free (range);
    free (drang);
    free (trans);

    return ierr ? TCL_ERROR : TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BC_t Nodes                                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nbocos (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nboco;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nbocos (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nboco))
        return get_cg_error (interp, "cg_nbocos");

    sprintf (buff, "%d", nboco);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char *p, name[33];
    int n, idim, cgfile, cgbase, cgzone, cgboco;
    int npnts, ndataset, nindex[3], nflag;
    BCType_t bctype;
    PointSetType_t ptype;
    DataType_t dtype;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgboco = atoi(argv[4]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim) ||
        cg_boco_info (cgfile, cgbase, cgzone, cgboco, name,
            &bctype, &ptype, &npnts, nindex, &nflag, &dtype, &ndataset))
        return get_cg_error (interp, "cg_boco_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_BCTypeName(bctype));
    Tcl_AppendElement (interp, cg_PointSetTypeName(ptype));
    sprintf (buff, "%d", npnts);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nindex[0]);
    for (p = buff, n = 1; n < idim; n++) {
        p += strlen(p);
        sprintf (p, " %d", nindex[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nflag);
    Tcl_AppendElement (interp, buff);
    Tcl_AppendElement (interp, cg_DataTypeName(dtype));
    sprintf (buff, "%d", ndataset);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone, cgboco;
    int ierr, pcnt, psize, ncnt;
    int get_nrmls = (int)((size_t)data);
    DataType_t ntype;
    void *pnts, *nrml = NULL;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgboco = atoi(argv[4]);

    if (cg_boco_dim (cgfile, cgbase, cgzone, cgboco,
            &pcnt, &psize, &ncnt, &ntype))
        return get_cg_error (interp, "cg_boco_read");

    if (pcnt == 0) return TCL_OK;
    if (get_nrmls && ncnt == 0) return TCL_OK;

    pnts = malloc (pcnt * sizeof(int));
    if (pnts == NULL) {
        Tcl_AppendResult (interp, "malloc failed for points", NULL);
        return TCL_ERROR;
    }
    if (get_nrmls) {
        nrml = malloc (ncnt * sizeof_datatype(ntype));
        if (nrml == NULL) {
            free (pnts);
            Tcl_AppendResult (interp, "malloc failed for normals", NULL);
            return TCL_ERROR;
        }
    }

    if (cg_boco_read (cgfile, cgbase, cgzone, cgboco, pnts, nrml)) {
        free (pnts);
        if (nrml != NULL) free (nrml);
        return get_cg_error (interp, "cg_boco_read");
    }

    if (get_nrmls)
        ierr = construct_data (interp, ntype, ncnt, nrml);
    else
        ierr = construct_data (interp, Integer, pcnt, pnts);
    free (pnts);
    if (nrml != NULL) free (nrml);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_id (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }

    if (cg_boco_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_boco_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgfile, cgbase, cgzone, cgboco;
    int ierr, bctype, ptype, npnts, cnt, idim;
    void *pnts;

    Tcl_ResetResult (interp);
    if (argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcname bctype ptsettype npts pnts",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "bc", argv[4]) ||
        get_type (interp, "bc", argv[5], NofValidBCTypes,
            BCTypeName, &bctype) ||
        get_type (interp, "ptset", argv[6], NofValidPointSetTypes,
            PointSetTypeName, &ptype)) return TCL_ERROR;
    if (ptype != PointRange && ptype != ElementRange &&
        ptype != PointList  && ptype != ElementList) {
        Tcl_AppendResult (interp, "invalid ptset type", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    npnts  = atoi(argv[7]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim))
        return get_cg_error (interp, "cg_boco_write");

    if (extract_data (interp, argv[8], Integer, &cnt, &pnts))
        return TCL_ERROR;
    if (cnt < npnts * idim) {
        free (pnts);
        sprintf (buff, "needs %d values", npnts * idim);
        Tcl_AppendResult (interp,
            "insufficient point data - ", buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_boco_write (cgfile, cgbase, cgzone, argv[4],
              (BCType_t)bctype, (PointSetType_t)ptype,
               npnts, pnts, &cgboco);
    free (pnts);
    if (ierr) return get_cg_error (interp, "cg_boco_write");

    sprintf (buff, "%d", cgboco);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_normal_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, cgfile, cgbase, cgzone, cgboco;
    int nflag, type, dim, psize, cnt, index[3];
    DataType_t ntype;
    void *normals;

    Tcl_ResetResult (interp);
    if (argc != 6 && argc != 8 && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum normalindex",
            " [[normallistflag] normaldatatype normals]", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgboco = atoi(argv[4]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &dim))
        return get_cg_error (interp, "cg_boco_normal_write");

    if (dim > sscanf(argv[5], "%d%d%d", &index[0], &index[1], &index[2])) {
        sprintf (buff, "needs %d", dim);
        Tcl_AppendResult (interp,
            "insufficient number of normalindex values - ", buff, NULL);
        return TCL_ERROR;
    }

    nflag = type = 0;
    normals = NULL;
    if (argc >= 8)
        nflag = argc == 9 ? atoi(argv[6]) : 1;

    if (nflag) {
        if (get_type (interp, "normal data", argv[argc-2],
            NofValidDataTypes, DataTypeName, &type)) return TCL_ERROR;
        if (type != RealSingle && type != RealDouble) {
            Tcl_AppendResult (interp,
                "normal data type not RealSingle or RealDouble", NULL);
            return TCL_ERROR;
        }
        if (cg_physical_dim (cgfile, cgbase, &dim) ||
            cg_boco_dim (cgfile, cgbase, cgzone, cgboco,
                &cnt, &psize, &cnt, &ntype))
            return get_cg_error (interp, "cg_boco_normal_write");
        if (extract_data (interp, argv[argc-1], type, &cnt, &normals))
            return TCL_ERROR;
        if (cnt < psize * dim) {
            free (normals);
            sprintf (buff, "needs %d", psize * dim);
            Tcl_AppendResult (interp,
                "insufficient normal values - ", buff, NULL);
            return TCL_ERROR;
        }
    }

    ierr = cg_boco_normal_write (cgfile, cgbase, cgzone, cgboco,
           index, nflag, (DataType_t)type, normals);
    if (nflag) free (normals);
    if (ierr) return get_cg_error (interp, "cg_boco_normal_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCDataSet_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_dataset_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    BCType_t bctype;
    int dflag, nflag;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum dsnum", NULL);
        return TCL_ERROR;
    }

    if (cg_dataset_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), name, &bctype, &dflag, &nflag))
        return get_cg_error (interp, "cg_dataset_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_BCTypeName(bctype));
    sprintf (buff, "%d", dflag);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nflag);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_dataset_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int bctype, cgds;

    Tcl_ResetResult (interp);
    if (argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum datasetname bctype", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "dataset", argv[5]) ||
        get_type (interp, "bc", argv[6], NofValidBCTypes,
            BCTypeName, &bctype)) return TCL_ERROR;

    if (cg_dataset_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), argv[5], (BCType_t)bctype, &cgds))
        return get_cg_error (interp, "cg_dataset_write");

    sprintf (buff, "%d", cgds);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bcdataset_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ndset;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_bcdataset_info (&ndset))
        return get_cg_error (interp, "cg_bcdataset_info");

    sprintf (buff, "%d", ndset);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bcdataset_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int bctype, bcdatatype;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " bcdatasetname bctype bcdatatype", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "bcdataset", argv[1]) ||
        get_type (interp, "bc", argv[2], NofValidBCTypes,
            BCTypeName, &bctype) ||
        get_type (interp, "bcdata", argv[3], NofValidBCDataTypes,
            BCDataTypeName, &bcdatatype)) return TCL_ERROR;

    if (cg_bcdataset_write (argv[1], (BCType_t)bctype,
            (BCDataType_t)bcdatatype))
        return get_cg_error (interp, "cg_bcdataset_write");

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bcdataset_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    BCType_t bctype;
    int dflag, nflag;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " index", NULL);
        return TCL_ERROR;
    }

    if (cg_bcdataset_read (atoi(argv[1]), name, &bctype, &dflag, &nflag))
        return get_cg_error (interp, "cg_bcdataset_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_BCTypeName(bctype));
    sprintf (buff, "%d", dflag);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nflag);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCData_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_bcdata_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum dsnum bcdatatype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "bc", argv[6], NofValidBCDataTypes,
            BCDataTypeName, &type)) return TCL_ERROR;

    if (cg_bcdata_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), (BCDataType_t)type))
        return get_cg_error (interp, "cg_bcdata_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DiscreteData_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ndiscrete (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ndisc;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_ndiscrete (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &ndisc))
        return get_cg_error (interp, "cg_ndiscrete");

    sprintf (buff, "%d", ndisc);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_discrete_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum discretenum", NULL);
        return TCL_ERROR;
    }

    if (cg_discrete_read (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), name))
        return get_cg_error (interp, "cg_discrete_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_discrete_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgdisc;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum discretename", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "discrete", argv[4])) return TCL_ERROR;

    if (cg_discrete_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), argv[4], &cgdisc))
        return get_cg_error (interp, "cg_discrete_write");

    sprintf (buff, "%d", cgdisc);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RigidGridMotion_t Nodes				 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_n_rigid_motions (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nrm;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_n_rigid_motions (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), &nrm))
        return get_cg_error (interp, "cg_n_rigid_motions");

    sprintf (buff, "%d", nrm);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_rigid_motion_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    RigidGridMotionType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum rigidmotionnum", NULL);
        return TCL_ERROR;
    }

    if (cg_rigid_motion_read (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), name, &type))
        return get_cg_error (interp, "cg_rigid_motion_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_RigidGridMotionTypeName(type));
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_rigid_motion_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgrm, type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum rigidmotionname rigidmotiontype", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "rigidmotion", argv[4]) ||
        get_type (interp, "rigidmotion", argv[5], NofValidRigidGridMotionTypes,
            RigidGridMotionTypeName, &type)) return TCL_ERROR;

    if (cg_rigid_motion_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), argv[4], (RigidGridMotionType_t)type, &cgrm))
        return get_cg_error (interp, "cg_rigid_motion_write");
    
    sprintf (buff, "%d", cgrm);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ArbitraryGridMotion_t Nodes                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_n_arbitrary_motions (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nam;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_n_arbitrary_motions (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), &nam))
        return get_cg_error (interp, "cg_n_arbitrary_motions");

    sprintf (buff, "%d", nam);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_arbitrary_motion_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    ArbitraryGridMotionType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum arbitarymotionnum", NULL);
        return TCL_ERROR;
    }

    if (cg_arbitrary_motion_read (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), name, &type))
        return get_cg_error (interp, "cg_arbitrary_motion_read");
    
    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_ArbitraryGridMotionTypeName(type));
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_arbitrary_motion_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int cgam, type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum arbitrarymotionname arbitrarymotiontype",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "arbitrarymotion", argv[4]) ||
        get_type (interp, "arbitrarymotion", argv[5],
            NofValidArbitraryGridMotionTypes,
            ArbitraryGridMotionTypeName, &type)) return TCL_ERROR;

    if (cg_arbitrary_motion_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), argv[4], (ArbitraryGridMotionType_t)type, &cgam))
        return get_cg_error (interp, "cg_arbitrary_motion_write");
    
    sprintf (buff, "%d", cgam);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write SimulationType_t Node                             *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_simulation_type_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    SimulationType_t type;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_simulation_type_read (atoi(argv[1]), atoi(argv[2]), &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_simulation_type_read");
    }
    
    Tcl_AppendResult (interp, cg_SimulationTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_simulation_type_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum simulationtype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "simulation", argv[3], NofValidSimulationTypes,
            SimulationTypeName, &type)) return TCL_ERROR;

    if (cg_simulation_type_write (atoi(argv[1]), atoi(argv[2]),
            (SimulationType_t)type))
        return get_cg_error (interp, "cg_simulation_type_write");
    
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BaseIterativeData_t Node                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_biter_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, nsteps;
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_biter_read (atoi(argv[1]), atoi(argv[2]), name, &nsteps);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_biter_read");
    }

    Tcl_AppendElement (interp, name);
    sprintf (buff, "%d", nsteps);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_biter_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum bitername nsteps", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "biter", argv[3])) return TCL_ERROR;

    if (cg_biter_write (atoi(argv[1]), atoi(argv[2]), argv[3], atoi(argv[4])))
        return get_cg_error (interp, "cg_biter_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ZoneIterativeData_t Node                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ziter_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_ziter_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), name);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_ziter_read");
    }

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ziter_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum zitername", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "ziter", argv[4])) return TCL_ERROR;

    if (cg_ziter_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), argv[4]))
        return get_cg_error (interp, "cg_ziter_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Gravity_t Nodes                                   *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_gravity_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, cgfile, cgbase, ierr, phys_dim;
    float vector[3];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);

    ierr = cg_gravity_read (cgfile, cgbase, vector);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_gravity_read");
    }

    if (cg_physical_dim (cgfile, cgbase, &phys_dim))
        return get_cg_error (interp, "cg_gravity_read");
    for (n = 0; n < phys_dim; n++) {
        sprintf (buff, "%g", vector[n]);
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_gravity_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int len, cgfile, cgbase, phys_dim;
    float vector[3];

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum vector", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);

    if (cg_physical_dim (cgfile, cgbase, &phys_dim))
        return get_cg_error (interp, "cg_gravity_write");

    len = sscanf (argv[3], "%f%f%f", &vector[0], &vector[1], &vector[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for gravity vector - ", buff, NULL);
        return TCL_ERROR;
    }

    if (cg_gravity_write (cgfile, cgbase, vector))
        return get_cg_error (interp, "cg_gravity_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Axisymmetry_t Nodes                               *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_axisym_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char *p;
    int ierr, n, cgfile, cgbase, phys_dim;
    float ref[3], axis[3];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);

    ierr = cg_axisym_read (cgfile, cgbase, ref, axis);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_axisym_read");
    }

    if (cg_physical_dim (cgfile, cgbase, &phys_dim))
        return get_cg_error (interp, "cg_gravity_read");
    sprintf (buff, "%g", ref[0]);
    for (p = buff, n = 1; n < phys_dim; n++) {
        p += strlen(p);
        sprintf (p, " %g", ref[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%g", axis[0]);
    for (p = buff, n = 1; n < phys_dim; n++) {
        p += strlen(p);
        sprintf (p, " %g", axis[n]);
    }
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_axisym_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int len, cgfile, cgbase, phys_dim;
    float ref[3], axis[3];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum refpoint axis", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);

    if (cg_physical_dim (cgfile, cgbase, &phys_dim))
        return get_cg_error (interp, "cg_gravity_read");

    len = sscanf (argv[3], "%f%f%f", &ref[0], &ref[1], &ref[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for reference point - ", buff, NULL);
        return TCL_ERROR;
    }
    len = sscanf (argv[4], "%f%f%f", &axis[0], &axis[1], &axis[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for axis vector - ", buff, NULL);
        return TCL_ERROR;
    }

    if (cg_axisym_write (cgfile, cgbase, ref, axis))
        return get_cg_error (interp, "cg_axisym_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RotatingCoordinates_t Nodes                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_rotating_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char *p;
    int file, base, depth;
    int ierr, n, phys_dim;
    float rate[3], center[3];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    if (cg_where(&file, &base, &depth, 0, 0))
        return get_cg_error (interp, "cg_where");

    ierr = cg_rotating_read (rate, center);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_rotating_read");
    }

    if (cg_physical_dim (file, base, &phys_dim))
        return get_cg_error (interp, "cg_rotating_read");

    sprintf (buff, "%g", rate[0]);
    for (p = buff, n = 1; n < phys_dim; n++) {
        p += strlen(p);
        sprintf (p, " %g", rate[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%g", center[0]);
    for (p = buff, n = 1; n < phys_dim; n++) {
        p += strlen(p);
        sprintf (p, " %g", center[n]);
    }
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_rotating_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int len, phys_dim;
    int file, base, depth;
    float rate[3], center[3];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " rate center", NULL);
        return TCL_ERROR;
    }
    if (cg_where(&file, &base, &depth, 0, 0))
        return get_cg_error (interp, "cg_where");
    if (cg_physical_dim (file, base, &phys_dim))
        return get_cg_error (interp, "cg_rotating_write");

    len = sscanf (argv[1], "%f%f%f", &rate[0], &rate[1], &rate[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for rate - ", buff, NULL);
        return TCL_ERROR;
    }
    len = sscanf (argv[2], "%f%f%f", &center[0], &center[1], &center[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for center - ", buff, NULL);
        return TCL_ERROR;
    }

    if (cg_rotating_write (rate, center))
        return get_cg_error (interp, "cg_rotating_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/WallFunction_t Nodes   	         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_bc_wallfunction_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    WallFunctionType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_bc_wallfunction_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_bc_wallfunction_read");
    }

    Tcl_AppendResult (interp, cg_WallFunctionTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bc_wallfunction_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum wallfuctiontype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "wall function", argv[5],
        NofValidWallFunctionTypes, WallFunctionTypeName, &type))
        return TCL_ERROR;

    if (cg_bc_wallfunction_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), (WallFunctionType_t)type))
        return get_cg_error (interp, "cg_bc_wallfunction_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/Area_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_bc_area_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    char *p, name[33];
    AreaType_t type;
    float area;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_bc_area_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), &type, &area, name);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_bc_area_read");
    }

    Tcl_AppendElement (interp, cg_AreaTypeName(type));
    sprintf (buff, "%g", area);
    Tcl_AppendElement (interp, buff);
    for (p = name+strlen(name)-1; p >= name && isspace(*p); p--)
        ;
    *++p = 0;
    Tcl_AppendElement (interp, name);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bc_area_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type;
    float area;

    Tcl_ResetResult (interp);
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum areatype area regionname", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "area", argv[5],
        NofValidAreaTypes, AreaTypeName, &type))
        return TCL_ERROR;
    area = (float) atof(argv[6]);

    if (cg_bc_area_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), (AreaType_t)type, area, argv[7]))
        return get_cg_error (interp, "cg_bc_area_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivityProperty_t/Periodic_t Nodes       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_conn_periodic_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char *p;
    int ierr, i, idim;
    int file, base;
    float angle[3], center[3], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum", NULL);
        return TCL_ERROR;
    }
    file = atoi(argv[1]);
    base = atoi(argv[2]);
    if (cg_physical_dim(file, base, &idim))
        return get_cg_error (interp, "cg_conn_periodic_read");

    ierr = cg_conn_periodic_read (file, base, atoi(argv[3]),
               atoi(argv[4]), center, angle, trans);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_conn_periodic_read");
    }

    sprintf (buff, "%g", center[0]);
    for (i = 1; i < idim; i++) {
        p = buff + strlen(buff);
        sprintf(p, " %g", center[i]);
    }
    Tcl_AppendElement (interp, buff);

    sprintf (buff, "%g", angle[0]);
    for (i = 1; i < idim; i++) {
        p = buff + strlen(buff);
        sprintf(p, " %g", angle[i]);
    }
    Tcl_AppendElement (interp, buff);

    sprintf (buff, "%g", trans[0]);
    for (i = 1; i < idim; i++) {
        p = buff + strlen(buff);
        sprintf(p, " %g", trans[i]);
    }
    Tcl_AppendElement (interp, buff);

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_periodic_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int idim;
    int file, base;
    float angle[3], center[3], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum",
            " center angle translation", NULL);
        return TCL_ERROR;
    }
    file = atoi(argv[1]);
    base = atoi(argv[2]);
    if (cg_physical_dim(file, base, &idim))
        return get_cg_error (interp, "cg_conn_periodic_write");

    if (idim != sscanf (argv[5], "%f%f%f", &center[0], &center[1], &center[2])) {
        sprintf(buff, "center needs %d values", idim);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }
    if (idim != sscanf (argv[6], "%f%f%f", &angle[0], &angle[1], &angle[2])) {
        sprintf(buff, "angle needs %d values", idim);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }
    if (idim != sscanf (argv[7], "%f%f%f", &trans[0], &trans[1], &trans[2])) {
        sprintf(buff, "translation needs %d values", idim);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }

    if (cg_conn_periodic_write (file, base, atoi(argv[3]), atoi(argv[4]),
            center, angle, trans))
        return get_cg_error (interp, "cg_conn_periodic_write");
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_periodic_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char *p;
    int ierr, i, idim;
    int file, base;
    float angle[3], center[3], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum", NULL);
        return TCL_ERROR;
    }
    file = atoi(argv[1]);
    base = atoi(argv[2]);
    if (cg_physical_dim(file, base, &idim))
        return get_cg_error (interp, "cg_1to1_periodic_read");

    ierr = cg_1to1_periodic_read (file, base, atoi(argv[3]),
               atoi(argv[4]), center, angle, trans);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_1to1_periodic_read");
    }

    sprintf (buff, "%g", center[0]);
    for (i = 1; i < idim; i++) {
        p = buff + strlen(buff);
        sprintf(p, " %g", center[i]);
    }
    Tcl_AppendElement (interp, buff);

    sprintf (buff, "%g", angle[0]);
    for (i = 1; i < idim; i++) {
        p = buff + strlen(buff);
        sprintf(p, " %g", angle[i]);
    }
    Tcl_AppendElement (interp, buff);

    sprintf (buff, "%g", trans[0]);
    for (i = 1; i < idim; i++) {
        p = buff + strlen(buff);
        sprintf(p, " %g", trans[i]);
    }
    Tcl_AppendElement (interp, buff);

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_periodic_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int idim;
    int file, base;
    float angle[3], center[3], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum",
            " center angle translation", NULL);
        return TCL_ERROR;
    }
    file = atoi(argv[1]);
    base = atoi(argv[2]);
    if (cg_physical_dim(file, base, &idim))
        return get_cg_error (interp, "cg_1to1_periodic_write");

    if (idim != sscanf (argv[5], "%f%f%f", &center[0], &center[1], &center[2])) {
        sprintf(buff, "center needs %d values", idim);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }
    if (idim != sscanf (argv[6], "%f%f%f", &angle[0], &angle[1], &angle[2])) {
        sprintf(buff, "angle needs %d values", idim);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }
    if (idim != sscanf (argv[7], "%f%f%f", &trans[0], &trans[1], &trans[2])) {
        sprintf(buff, "translation needs %d values", idim);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }

    if (cg_1to1_periodic_write (file, base, atoi(argv[3]), atoi(argv[4]),
            center, angle, trans))
        return get_cg_error (interp, "cg_1to1_periodic_write");
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *   Read and write GridConnectivityProperty_t/AverageInterface_t Nodes  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_conn_average_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    AverageInterfaceType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_conn_average_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_conn_average_read");
    }

    Tcl_AppendResult (interp, cg_AverageInterfaceTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_average_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n;
    AverageInterfaceType_t type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum averagetype", NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < NofValidAverageInterfaceTypes; n++) {
        if (0 == strcmp (argv[5], AverageInterfaceTypeName[n])) {
            type = (AverageInterfaceType_t)n;
            break;
        }
    }
    if (n == NofValidAverageInterfaceTypes) {
        Tcl_AppendResult (interp, "invalid average interface type", NULL);
        return TCL_ERROR;
    }

    if (cg_conn_average_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), type))
        return get_cg_error (interp, "cg_conn_average_write");

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_average_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    AverageInterfaceType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_1to1_average_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_1to1_average_read");
    }

    Tcl_AppendResult (interp, cg_AverageInterfaceTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_average_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n;
    AverageInterfaceType_t type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum averagetype", NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < NofValidAverageInterfaceTypes; n++) {
        if (0 == strcmp (argv[5], AverageInterfaceTypeName[n])) {
            type = (AverageInterfaceType_t)n;
            break;
        }
    }
    if (n == NofValidAverageInterfaceTypes) {
        Tcl_AppendResult (interp, "invalid average interface type", NULL);
        return TCL_ERROR;
    }

    if (cg_1to1_average_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), type))
        return get_cg_error (interp, "cg_1to1_average_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Variable Argument List Functions                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_goto (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, ierr, file, base, depth;
    int index[CG_MAX_GOTO_DEPTH];
    char lab[CG_MAX_GOTO_DEPTH*33];
    char *label[CG_MAX_GOTO_DEPTH];

    Tcl_ResetResult (interp);
    if (argc < 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum [label1 index1 [label2 index2 [...]]]", NULL);
        return TCL_ERROR;
    }
    if (argc > 2 * (CG_MAX_GOTO_DEPTH + 1)) {
        Tcl_AppendResult (interp, "max goto depth exceeded", NULL);
        return TCL_ERROR;
    }
    file = atoi(argv[1]);
    base = atoi(argv[2]);
    depth = 0;
    for (n = 3; n < argc; n++) {
        if (0 == strcmp (argv[n], "end")) break;
        label[depth] = &lab[depth*33];
        strncpy (label[depth], argv[n], 32);
        label[depth][32] = 0;
        if (++n >= argc) {
            Tcl_AppendResult (interp, "missing final index value", NULL);
            return TCL_ERROR;
        }
        index[depth] = atoi(argv[n]);
        depth++;
    }

    if (cg_golist(file, base, depth, label, index))
        return get_cg_error(interp, "cg_golist");
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_gorel (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int fn, n, index;
    char *label;

    Tcl_ResetResult (interp);
    if (argc < 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum label1 index1 [label2 index2 [...]]", NULL);
        return TCL_ERROR;
    }
    fn = atoi(argv[1]);

    for (n = 2; n < argc; n++) {
        label = argv[n];
        if (0 == strcmp (label, "end")) break;
        if (++n >= argc) {
            Tcl_AppendResult (interp, "missing final index value", NULL);
            return TCL_ERROR;
        }
        index = atoi(argv[n]);
        if (cg_gorel(fn, label, index, 0))
            return get_cg_error(interp, "cg_gorel");
    }

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_gopath (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int file;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filenum path", NULL);
        return TCL_ERROR;
    }
    file = atoi(argv[1]);
    if (cg_gopath(file, argv[2]))    
        return get_cg_error (interp, "cg_gopath");
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_where (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, ierr, depth, file, base;
    int index[CG_MAX_GOTO_DEPTH];
    char lab[CG_MAX_GOTO_DEPTH*33];
    char *label[CG_MAX_GOTO_DEPTH];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < CG_MAX_GOTO_DEPTH; n++) {
        label[n] = &lab[n*33];
    }
    if (cg_where(&file, &base, &depth, label, index))
        return get_cg_error(interp, "cg_where");
    sprintf (buff, "%d %d", file, base);
    Tcl_AppendResult (interp, buff, NULL);
    for (n = 0; n < depth; n++) {
        Tcl_AppendElement (interp, label[n]);
        sprintf (buff, "%d", index[n]);
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ConvergenceHistory_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_convergence_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, niter;
    char *normdefs;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_convergence_read (&niter, &normdefs);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_convergence_read");
    }

    sprintf (buff, "%d", niter);
    Tcl_AppendElement (interp, buff);
    if (normdefs != NULL) {
        Tcl_AppendElement (interp, normdefs);
        free (normdefs);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_convergence_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " niters [normdefinition]", NULL);
        return TCL_ERROR;
    }

    if (cg_convergence_write (atoi(argv[1]),
        argc == 3 ? argv[2] : ""))
        return get_cg_error (interp, "cg_convergence_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ReferenceState_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_state_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, niter;
    char *desc;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_state_read (&desc);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_state_read");
    }

    if (desc != NULL) {
        Tcl_AppendResult (interp, desc, NULL);
        free (desc);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_state_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc > 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " [statedescription]", NULL);
        return TCL_ERROR;
    }

    if (cg_state_write (argc == 2 ? argv[1] : ""))
        return get_cg_error (interp, "cg_state_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FlowEquationSet_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_equationset_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, ed, gef, gmf, vmf, tcmf, tcf, tmf;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_equationset_read (&ed, &gef, &gmf, &vmf, &tcmf, &tcf, &tmf);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_equationset_read");
    }

    sprintf (buff, "%d %d %d %d %d %d %d", ed, gef, gmf, vmf, tcmf, tcf, tmf);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_equationset_chemistry_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, trf, ckf;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_equationset_chemistry_read (&trf, &ckf);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_equationset_chemistry_read");
    }

    sprintf (buff, "%d %d", trf, ckf);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_equationset_elecmagn_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, emf, mmf, cmf;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_equationset_elecmagn_read (&emf, &mmf, &cmf);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_equationset_elecmagn_read");
    }

    sprintf (buff, "%d %d %d", emf, mmf, cmf);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_equationset_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " eqndimension", NULL);
        return TCL_ERROR;
    }

    if (cg_equationset_write (atoi(argv[1])))
        return get_cg_error (interp, "cg_equationset_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GoverningEquations_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_governing_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    GoverningEquationsType_t type;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_governing_read (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_governing_read");
    }

    Tcl_AppendResult (interp, cg_GoverningEquationsTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_governing_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " equationntype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "equation", argv[1],
        NofValidGoverningEquationsTypes,
        GoverningEquationsTypeName, &type)) return TCL_ERROR;

    if (cg_governing_write ((GoverningEquationsType_t)type))
        return get_cg_error (interp, "cg_governing_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Diffusion Model Nodes                             *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_diffusion_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, idim, diff[6];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (posit == 0) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }
    if (posit_zone)
        ierr = cg_index_dim (posit_file, posit_base, posit_zone, &idim);
    else
        ierr = cg_cell_dim (posit_file, posit_base, &idim);
    if (ierr) return get_cg_error (interp, "cg_diffusion_read");

    ierr = cg_diffusion_read (diff);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_diffusion_read");
    }

    if (idim == 1)
        sprintf (buff, "%d", diff[0]);
    else if (idim == 2)
        sprintf (buff, "%d %d %d", diff[0], diff[1], diff[2]);
    else
        sprintf (buff, "%d %d %d %d %d %d", diff[0], diff[1], diff[2],
            diff[3], diff[4], diff[5]);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_diffusion_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int idim, cnt, ierr, diff[6];

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " diffusionmodel", NULL);
        return TCL_ERROR;
    }

    if (posit == 0) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }
    if (posit_zone)
        ierr = cg_index_dim (posit_file, posit_base, posit_zone, &idim);
    else
        ierr = cg_cell_dim (posit_file, posit_base, &idim);
    if (ierr) return get_cg_error (interp, "cg_diffusion_read");

    if (idim == 1)
        cnt = 1;
    else if (idim == 2)
        cnt = 3;
    else if (idim == 3)
        cnt = 6;
    else {
        Tcl_AppendResult (interp, "couldn't determine index dimension", NULL);
        return TCL_ERROR;
    }
    if (cnt > sscanf (argv[1], "%d%d%d%d%d%d", &diff[0], &diff[1],
        &diff[2], &diff[3], &diff[4], &diff[5])) {
        sprintf (buff, "needs %d", cnt);
        Tcl_AppendResult (interp,
            "insufficient diffusion model values - ", buff, NULL);
        return TCL_ERROR;
    }

    if (cg_diffusion_write (diff))
        return get_cg_error (interp, "cg_diffusion_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GasModel_t, ViscosityModel_t,                     *
 *      ThermalConductivityModel_t, TurbulenceClosure_t,                 *
 *      TurbulenceModel_t, ThermalRelaxationModel_t,                     *
 *      ChemicalKineticsModel_t, EMElectricFieldModel_t,                 *
 *      EMMagneticFieldModel_t Nodes                                     *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_model_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    ModelType_t type;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " modellabel", NULL);
        return TCL_ERROR;
    }

    ierr = cg_model_read (argv[1], &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_model_read");
    }

    Tcl_AppendResult (interp, cg_ModelTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_model_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " modellabel modeltype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "model", argv[2], NofValidModelTypes,
        ModelTypeName, &type)) return TCL_ERROR;

    if (cg_model_write (argv[1], (ModelType_t)type))
        return get_cg_error (interp, "cg_model_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataArray_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_narrays (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int narray;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_narrays (&narray))
        return get_cg_error (interp, "cg_narrays");

    sprintf (buff, "%d", narray);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_array_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    int ndim, dims[12];
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " arraynum", NULL);
        return TCL_ERROR;
    }

    if (cg_array_info (atoi(argv[1]), name, &type, &ndim, dims))
        return get_cg_error (interp, "cg_array_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_DataTypeName(type));
    sprintf (buff, "%d", ndim);
    Tcl_AppendElement (interp, buff);
    if (ndim == 0)
        Tcl_AppendElement (interp, "");
    else {
        int n;
        char *p = buff;
        sprintf (p, "%d", dims[0]);
        for (n = 1; n < ndim; n++) {
            p += strlen(p);
            sprintf (p, " %d", dims[n]);
        }
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_array_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    int na, ndim, dims[12];
    int count, bytes;
    DataType_t type;
    void *array;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " arraynum", NULL);
        return TCL_ERROR;
    }
    na = atoi(argv[1]);

    if (cg_array_info (na, name, &type, &ndim, dims))
        return get_cg_error (interp, "cg_array_info");
    if (get_data_size (interp, type, ndim, dims, &count, &bytes))
        return TCL_ERROR;
    array = malloc (count * bytes);
    if (array == NULL) {
        Tcl_AppendResult (interp, "malloc failed for array data", NULL);
        return TCL_ERROR;
    }

    if (cg_array_read (na, array)) {
        free (array);
        return get_cg_error (interp, "cg_array_read");
    }

    na = construct_data (interp, type, count, array);
    free (array);
    return na ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_array_read_as (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];
    int na, ndim, dims[12];
    int count, bytes;
    DataType_t type;
    void *array;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " arraynum datatype", NULL);
        return TCL_ERROR;
    }
    na = atoi(argv[1]);

    if (cg_array_info (na, name, &type, &ndim, dims))
        return get_cg_error (interp, "cg_array_info");
    if (get_data_type (interp, argv[2], &type)) return TCL_ERROR;
    if (get_data_size (interp, type, ndim, dims, &count, &bytes))
        return TCL_ERROR;
    array = malloc (count * bytes);
    if (array == NULL) {
        Tcl_AppendResult (interp, "malloc failed for array data", NULL);
        return TCL_ERROR;
    }

    if (cg_array_read_as (na, type, array)) {
        free (array);
        return get_cg_error (interp, "cg_array_read_as");
    }

    na = construct_data (interp, type, count, array);
    free (array);
    return na ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_array_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, len, ndim, *dims, count;
    DataType_t type;
    void *array;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " arrayname datatype ndim dims data", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "array", argv[1]) ||
        get_data_type (interp, argv[2], &type) ||
        extract_data (interp, argv[4], Integer, &ndim, (void **)&dims))
        return TCL_ERROR;
    if (ndim != atoi(argv[3])) {
        free (dims);
        Tcl_AppendResult (interp,
            "mismatch in ndim and dimension count", NULL);
        return TCL_ERROR;
    }
    if (get_data_size (interp, type, ndim, dims, &len, &count)) {
        free (dims);
        return TCL_ERROR;
    }
    if (extract_data (interp, argv[5], type, &count, &array)) {
        free (dims);
        return TCL_ERROR;
    }
    if (count != len) {
        free (dims);
        free (array);
        Tcl_AppendResult (interp,
            "mismatch in dimension count and data array length", NULL);
        return TCL_ERROR;
    }

    ierr = cg_array_write (argv[1], type, ndim, dims, array);
    free (dims);
    free (array);
    if (ierr)
        return get_cg_error (interp, "cg_array_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write UserDefinedData_t Nodes - new in version 2.1      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nuser_data (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, nuser;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_nuser_data (&nuser);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_nuser_data");
    }

    sprintf (buff, "%d", nuser);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_user_data_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " userdatanum", NULL);
        return TCL_ERROR;
    }

    if (cg_user_data_read (atoi(argv[1]), name))
        return get_cg_error (interp, "cg_user_data_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_user_data_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " userdataname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "userdata", argv[1])) return TCL_ERROR;

    if (cg_user_data_write (argv[1]))
        return get_cg_error (interp, "cg_user_data_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write IntegralData_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nintegrals (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nint;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_nintegrals (&nint))
        return get_cg_error (interp, "cg_nintegrals");

    sprintf (buff, "%d", nint);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_integral_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " integralum", NULL);
        return TCL_ERROR;
    }

    if (cg_integral_read (atoi(argv[1]), name))
        return get_cg_error (interp, "cg_integral_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_integral_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " integralname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "integral", argv[1])) return TCL_ERROR;

    if (cg_integral_write (argv[1]))
        return get_cg_error (interp, "cg_integral_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Rind_t Nodes                                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_rind_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, n, idim, rind[6];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (posit == 0) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }
    if (posit_zone)
        ierr = cg_index_dim (posit_file, posit_base, posit_zone, &idim);
    else
        ierr = cg_cell_dim (posit_file, posit_base, &idim);
    if (ierr) return get_cg_error (interp, "cg_rind_read");

    ierr = cg_rind_read (rind);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_rind_read");
    }

    for (n = 0; n < 2*idim; n++) {
        sprintf (buff, "%d", rind[n]);
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_rind_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, idim, nr, *rind;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " rind", NULL);
        return TCL_ERROR;
    }

    if (posit == 0) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }
    if (posit_zone)
        ierr = cg_index_dim (posit_file, posit_base, posit_zone, &idim);
    else
        ierr = cg_cell_dim (posit_file, posit_base, &idim);
    if (ierr) return get_cg_error (interp, "cg_rind_write");

    if (extract_data (interp, argv[1], Integer, &nr, (void **)&rind))
        return TCL_ERROR;
    if (nr < 2*idim) {
        free (rind);
        sprintf (buff, "insufficient rind values - need %d", 2*idim);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_rind_write (rind);
    free (rind);
    if (ierr) return get_cg_error (interp, "cg_rind_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Descriptor_t Nodes                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ndescriptors (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ndesc;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_ndescriptors (&ndesc))
        return get_cg_error (interp, "cg_ndescriptors");

    sprintf (buff, "%d", ndesc);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_descriptor_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char name[33], *text;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " descnum", NULL);
        return TCL_ERROR;
    }

    if (cg_descriptor_read (atoi(argv[1]), name, &text))
        return get_cg_error (interp, "cg_descriptor_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, text);
    free (text);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_descriptor_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " descname desctext", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "descriptor", argv[1])) return TCL_ERROR;

    if (cg_descriptor_write (argv[1], argv[2]))
        return get_cg_error (interp, "cg_descriptor_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DimensionalUnits_t Nodes                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_units_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    MassUnits_t mass;
    LengthUnits_t length;
    TimeUnits_t time;
    TemperatureUnits_t temp;
    AngleUnits_t angle;
    int nunits, full = (int)((size_t)data);
    ElectricCurrentUnits_t current;
    SubstanceAmountUnits_t amount;
    LuminousIntensityUnits_t intensity;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_unitsfull_read (&mass, &length, &time, &temp, &angle,
        &current, &amount, &intensity);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_unitsfull_read");
    }
    if (cg_nunits (&nunits))
        return get_cg_error (interp, "cg_nunits");

    Tcl_AppendElement (interp, cg_MassUnitsName(mass));
    Tcl_AppendElement (interp, cg_LengthUnitsName(length));
    Tcl_AppendElement (interp, cg_TimeUnitsName(time));
    Tcl_AppendElement (interp, cg_TemperatureUnitsName(temp));
    Tcl_AppendElement (interp, cg_AngleUnitsName(angle));
    if (full || nunits > 5) {
        Tcl_AppendElement (interp, cg_ElectricCurrentUnitsName(current));
        Tcl_AppendElement (interp, cg_SubstanceAmountUnitsName(amount));
        Tcl_AppendElement (interp, cg_LuminousIntensityUnitsName(intensity));
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_units_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int type;
    MassUnits_t mass;
    LengthUnits_t length;
    TimeUnits_t time;
    TemperatureUnits_t temp;
    AngleUnits_t angle;
    int full = (int)((size_t)data);
    ElectricCurrentUnits_t current;
    SubstanceAmountUnits_t amount;
    LuminousIntensityUnits_t intensity;

    Tcl_ResetResult (interp);
    if (full && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " massunits lengthunits timeunits temperatureunits angleunits",
            " currentunits amountunits intensityunits", NULL);
        return TCL_ERROR;
    }
    if (argc != 6 && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " massunits lengthunits timeunits temperatureunits angleunits",
            " [currentunits amountunits intensityunits]",
            NULL);
        return TCL_ERROR;
    }

    if (get_type (interp, "mass unit", argv[1],
        NofValidMassUnits, MassUnitsName, &type)) return TCL_ERROR;
    mass = (MassUnits_t)type;

    if (get_type (interp, "length unit", argv[2],
        NofValidLengthUnits, LengthUnitsName, &type)) return TCL_ERROR;
    length = (LengthUnits_t)type;

    if (get_type (interp, "time unit", argv[3],
        NofValidTimeUnits, TimeUnitsName, &type)) return TCL_ERROR;
    time = (TimeUnits_t)type;

    if (get_type (interp, "temperature unit", argv[4],
        NofValidTemperatureUnits, TemperatureUnitsName, &type))
        return TCL_ERROR;
    temp = (TemperatureUnits_t)type;

    if (get_type (interp, "angle unit", argv[5],
        NofValidAngleUnits, AngleUnitsName, &type)) return TCL_ERROR;
    angle = (AngleUnits_t)type;

    if (full || argc == 9) {
        if (get_type (interp, "electric current unit", argv[6],
            NofValidElectricCurrentUnits, ElectricCurrentUnitsName, &type))
            return TCL_ERROR;
        current = (ElectricCurrentUnits_t)type;

        if (get_type (interp, "substance amount unit", argv[7],
            NofValidSubstanceAmountUnits, SubstanceAmountUnitsName, &type))
            return TCL_ERROR;
        amount = (SubstanceAmountUnits_t)type;

        if (get_type (interp, "luminous intensity unit", argv[8],
            NofValidLuminousIntensityUnits, LuminousIntensityUnitsName, &type))
            return TCL_ERROR;
        intensity = (LuminousIntensityUnits_t)type;

        if (cg_unitsfull_write (mass, length, time, temp, angle,
                current, amount, intensity))
            return get_cg_error (interp, "cg_unitsfull_write");

        return TCL_OK;
    }

    if (cg_units_write (mass, length, time, temp, angle))
        return get_cg_error (interp, "cg_units_write");

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_nunits (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nunits;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_nunits (&nunits))
        return get_cg_error (interp, "cg_nunits");

    sprintf (buff, "%d", nunits);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DimensionalExponents_t Nodes                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_exponents_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_exponents_info (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_exponents_info");
    }

    Tcl_AppendResult (interp, cg_DataTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_exponents_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr, nexp;
    DataType_t type;
    double exps[8];
    int full = (int)((size_t)data);

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_exponents_info (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_exponents_info");
    }
    if (type != RealSingle && type != RealDouble) {
        Tcl_AppendResult (interp, "invalid exponents data type", NULL);
        return TCL_ERROR;
    }
    if (full)
        nexp = 8;
    else {
        if (cg_nexponents (&nexp))
            return get_cg_error (interp, "cg_nexponents");
    }
    if (cg_expfull_read ((void *)exps))
        return get_cg_error (interp, "cg_expfull_read");

    if (construct_data (interp, type, nexp, (void *)exps))
        return TCL_ERROR;
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_exponents_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nexp;
    DataType_t type;
    void *exps;
    int full = (int)((size_t)data);

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " datatype exponents", NULL);
        return TCL_ERROR;
    }
    if (get_data_type (interp, argv[1], &type)) return TCL_ERROR;
    if (type != RealSingle && type != RealDouble) {
        Tcl_AppendResult (interp,
            "data type not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    if (extract_data (interp, argv[2], type, &nexp, &exps))
        return TCL_ERROR;
    if (nexp == 5) {
        if (full) {
            free (exps);
            Tcl_AppendResult (interp, "expecting 8 exponents", NULL);
            return TCL_ERROR;
        }
        if (cg_exponents_write (type, exps)) {
            free (exps);
            return get_cg_error (interp, "cg_exponents_write");
        }
    }
    else if (nexp == 8) {
        if (cg_expfull_write (type, exps)) {
            free (exps);
            return get_cg_error (interp, "cg_expfull_write");
        }
    }
    else {
        free (exps);
        if (full)
            sprintf (buff, "expecting 8 exponents");
        else
            sprintf (buff, "expecting 5 or 8 exponents");
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }

    free (exps);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_nexponents (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int nexps;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_nexponents (&nexps))
        return get_cg_error (interp, "cg_nexponents");

    sprintf (buff, "%d", nexps);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataConversion_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_conversion_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_conversion_info (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_conversion_info");
    }

    Tcl_AppendResult (interp, cg_DataTypeName(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conversion_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int n, ierr;
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_conversion_info (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_conversion_info");
    }
    if (type == RealSingle) {
        float vals[2];
        if (cg_conversion_read (vals))
            return get_cg_error (interp, "cg_conversion_read");
        sprintf (buff, "%g %g", vals[0], vals[1]);
    }
    else if (type == RealDouble) {
        double vals[2];
        if (cg_conversion_read (vals))
            return get_cg_error (interp, "cg_conversion_read");
        sprintf (buff, "%g %g", vals[0], vals[1]);
    }
    else {
        Tcl_AppendResult (interp, "invalid conversion data type", NULL);
        return TCL_ERROR;
    }

    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conversion_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    DataType_t type;
    double conv[2];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " datatype conversion", NULL);
        return TCL_ERROR;
    }
    if (get_data_type (interp, argv[1], &type)) return TCL_ERROR;
    if (type != RealSingle && type != RealDouble) {
        Tcl_AppendResult (interp,
            "data type not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    if (2 != sscanf (argv[2], "%lf%lf", &conv[0], &conv[1])) {
        Tcl_AppendResult (interp,
            "conversion scale and offset not given", NULL);
        return TCL_ERROR;
    }

    if (type == RealSingle) {
        float vals[2];
        vals[0] = (float)conv[0];
        vals[1] = (float)conv[1];
        ierr = cg_conversion_write (RealSingle, vals);
    }
    else
        ierr = cg_conversion_write (RealDouble, conv);
    if (ierr)
        return get_cg_error (interp, "cg_conversion_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataClass_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_dataclass_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    DataClass_t dataclass;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_dataclass_read (&dataclass);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_dataclass_read");
    }

    Tcl_AppendResult (interp, cg_DataClassName(dataclass), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_dataclass_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int dataclass;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " dataclass", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "dataclass", argv[1], NofValidDataClass,
            DataClassName, &dataclass)) return TCL_ERROR;

    if (cg_dataclass_write ((DataClass_t)dataclass))
        return get_cg_error (interp, "cg_dataclass_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridLocation_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_gridlocation_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ierr;
    GridLocation_t location;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_gridlocation_read (&location);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_gridlocation_read");
    }

    Tcl_AppendResult (interp, cg_GridLocationName(location), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_gridlocation_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int location;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " location", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "grid location", argv[1], NofValidGridLocation,
            GridLocationName, &location)) return TCL_ERROR;

    if (cg_gridlocation_write ((GridLocation_t)location))
        return get_cg_error (interp, "cg_gridlocation_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Ordinal_t Nodes                                   *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ordinal_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int ordinal, ierr;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_ordinal_read (&ordinal);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_ordinal_read");
    }

    sprintf (buff, "%d", ordinal);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ordinal_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " ordinal", NULL);
        return TCL_ERROR;
    }

    if (cg_ordinal_write (atoi(argv[1])))
        return get_cg_error (interp, "cg_ordinal_write");

    return TCL_OK;
}
























/*-----------------------------------------------------------------------*/

static int tcl_cg_ptset_info (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_SetResult (interp, "not implemented", TCL_STATIC);
    return TCL_ERROR;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ptset_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_SetResult (interp, "not implemented", TCL_STATIC);
    return TCL_ERROR;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ptset_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_SetResult (interp, "not implemented", TCL_STATIC);
    return TCL_ERROR;
}















/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Link Handling Functions - new in version 2.1                     *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_is_link (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    int len;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    if (cg_is_link (&len))
        return get_cg_error (interp, "cg_is_link");

    sprintf (buff, "%d", len);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_link_read (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    char *filename, *pathname;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_link_read (&filename, &pathname))
        return get_cg_error (interp, "cg_link_read");

    Tcl_AppendElement (interp, filename);
    free (filename);
    Tcl_AppendElement (interp, pathname);
    free (pathname);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_link_write (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " nodename filename nameinfile", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "node", argv[1])) return TCL_ERROR;
    if (strlen (argv[3]) < 1) {
        Tcl_AppendResult (interp, "nameinfile not given", NULL);
        return TCL_ERROR;
    }

    if (cg_link_write (argv[1], argv[2], argv[3]))
        return get_cg_error (interp, "cg_link_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      General Delete Function						 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_delete_node (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " nodename", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "node", argv[1])) return TCL_ERROR;

    if (cg_delete_node (argv[1]))
        return get_cg_error (interp, "cg_delete_node");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Error Handling Functions                                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_get_error (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    Tcl_AppendResult (interp, cg_get_error(), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_error_exit (ClientData data, Tcl_Interp *interp,
    int argc, char **argv)
{
    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    cg_error_exit();
    return TCL_OK;
}

/*---------- CGNStcl_Init ---------------------------------------
 * Initialize and create the commands
 *--------------------------------------------------------------*/

#if defined(_WIN32) && defined(BUILD_DLL)
__declspec(dllexport)
#endif
int Cgnstcl_Init(Tcl_Interp *interp)
{
    Tcl_CreateCommand (interp, "cg_library_version",
        (Tcl_CmdProc *)tcl_cg_library_version, 0, 0);
    Tcl_CreateCommand (interp, "cg_get_names",
        (Tcl_CmdProc *)tcl_cg_get_names, 0, 0);

    Tcl_CreateCommand (interp, "cg_is_cgns",
        (Tcl_CmdProc *)tcl_cg_is_cgns, 0, 0);
    Tcl_CreateCommand (interp, "cg_open",
        (Tcl_CmdProc *)tcl_cg_open, 0, 0);
    Tcl_CreateCommand (interp, "cg_version",
        (Tcl_CmdProc *)tcl_cg_version, 0, 0);
    Tcl_CreateCommand (interp, "cg_close",
        (Tcl_CmdProc *)tcl_cg_close, 0, 0);

    Tcl_CreateCommand (interp, "cg_nbases",
        (Tcl_CmdProc *)tcl_cg_nbases, 0, 0);
    Tcl_CreateCommand (interp, "cg_base_read",
        (Tcl_CmdProc *)tcl_cg_base_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_base_id",
        (Tcl_CmdProc *)tcl_cg_base_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_base_write",
        (Tcl_CmdProc *)tcl_cg_base_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nzones",
        (Tcl_CmdProc *)tcl_cg_nzones, 0, 0);
    Tcl_CreateCommand (interp, "cg_zone_read",
        (Tcl_CmdProc *)tcl_cg_zone_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_zone_type",
        (Tcl_CmdProc *)tcl_cg_zone_type, 0, 0);
    Tcl_CreateCommand (interp, "cg_zone_id",
        (Tcl_CmdProc *)tcl_cg_zone_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_zone_write",
        (Tcl_CmdProc *)tcl_cg_zone_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nfamilies",
        (Tcl_CmdProc *)tcl_cg_nfamilies, 0, 0);
    Tcl_CreateCommand (interp, "cg_family_read",
        (Tcl_CmdProc *)tcl_cg_family_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_family_write",
        (Tcl_CmdProc *)tcl_cg_family_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_famname_read",
        (Tcl_CmdProc *)tcl_cg_famname_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_famname_write",
        (Tcl_CmdProc *)tcl_cg_famname_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_fambc_read",
        (Tcl_CmdProc *)tcl_cg_fambc_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_fambc_write",
        (Tcl_CmdProc *)tcl_cg_fambc_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_geo_read",
        (Tcl_CmdProc *)tcl_cg_geo_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_geo_write",
        (Tcl_CmdProc *)tcl_cg_geo_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_part_read",
        (Tcl_CmdProc *)tcl_cg_part_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_part_write",
        (Tcl_CmdProc *)tcl_cg_part_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ngrids",
        (Tcl_CmdProc *)tcl_cg_ngrids, 0, 0);
    Tcl_CreateCommand (interp, "cg_grid_read",
        (Tcl_CmdProc *)tcl_cg_grid_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_grid_write",
        (Tcl_CmdProc *)tcl_cg_grid_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ncoords",
        (Tcl_CmdProc *)tcl_cg_ncoords, 0, 0);
    Tcl_CreateCommand (interp, "cg_coord_info",
        (Tcl_CmdProc *)tcl_cg_coord_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_coord_read",
        (Tcl_CmdProc *)tcl_cg_coord_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_coord_id",
        (Tcl_CmdProc *)tcl_cg_coord_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_coord_write",
        (Tcl_CmdProc *)tcl_cg_coord_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nsections",
        (Tcl_CmdProc *)tcl_cg_nsections, 0, 0);
    Tcl_CreateCommand (interp, "cg_section_read",
        (Tcl_CmdProc *)tcl_cg_section_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_elements_read",
        (Tcl_CmdProc *)tcl_cg_elements_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_parent_data_read",
        (Tcl_CmdProc *)tcl_cg_parent_data_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_section_write",
        (Tcl_CmdProc *)tcl_cg_section_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_elements_write",
        (Tcl_CmdProc *)tcl_cg_elements_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_parent_data_write",
        (Tcl_CmdProc *)tcl_cg_parent_data_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_npe",
        (Tcl_CmdProc *)tcl_cg_npe, 0, 0);
    Tcl_CreateCommand (interp, "cg_ElementDataSize",
        (Tcl_CmdProc *)tcl_cg_ElementDataSize, 0, 0);

    Tcl_CreateCommand (interp, "cg_nsols",
        (Tcl_CmdProc *)tcl_cg_nsols, 0, 0);
    Tcl_CreateCommand (interp, "cg_sol_info",
        (Tcl_CmdProc *)tcl_cg_sol_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_sol_id",
        (Tcl_CmdProc *)tcl_cg_sol_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_sol_write",
        (Tcl_CmdProc *)tcl_cg_sol_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nfields",
        (Tcl_CmdProc *)tcl_cg_nfields, 0, 0);
    Tcl_CreateCommand (interp, "cg_field_info",
        (Tcl_CmdProc *)tcl_cg_field_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_field_read",
        (Tcl_CmdProc *)tcl_cg_field_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_field_id",
        (Tcl_CmdProc *)tcl_cg_field_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_field_write",
        (Tcl_CmdProc *)tcl_cg_field_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nholes",
        (Tcl_CmdProc *)tcl_cg_nholes, 0, 0);
    Tcl_CreateCommand (interp, "cg_hole_info",
        (Tcl_CmdProc *)tcl_cg_hole_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_hole_read",
        (Tcl_CmdProc *)tcl_cg_hole_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_hole_id",
        (Tcl_CmdProc *)tcl_cg_hole_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_hole_write",
        (Tcl_CmdProc *)tcl_cg_hole_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nconns",
        (Tcl_CmdProc *)tcl_cg_nconns, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_info",
        (Tcl_CmdProc *)tcl_cg_conn_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_read",
        (Tcl_CmdProc *)tcl_cg_conn_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_donor_read",
        (Tcl_CmdProc *)tcl_cg_conn_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_conn_id",
        (Tcl_CmdProc *)tcl_cg_conn_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_write",
        (Tcl_CmdProc *)tcl_cg_conn_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_n1to1",
        (Tcl_CmdProc *)tcl_cg_n1to1, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_read",
        (Tcl_CmdProc *)tcl_cg_1to1_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_id",
        (Tcl_CmdProc *)tcl_cg_1to1_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_write",
        (Tcl_CmdProc *)tcl_cg_1to1_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_n1to1_global",
        (Tcl_CmdProc *)tcl_cg_n1to1_global, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_read_global",
        (Tcl_CmdProc *)tcl_cg_1to1_read_global, 0, 0);

    Tcl_CreateCommand (interp, "cg_nbocos",
        (Tcl_CmdProc *)tcl_cg_nbocos, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_info",
        (Tcl_CmdProc *)tcl_cg_boco_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_read",
        (Tcl_CmdProc *)tcl_cg_boco_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_normal_read",
        (Tcl_CmdProc *)tcl_cg_boco_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_boco_id",
        (Tcl_CmdProc *)tcl_cg_boco_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_write",
        (Tcl_CmdProc *)tcl_cg_boco_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_normal_write",
        (Tcl_CmdProc *)tcl_cg_boco_normal_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_dataset_read",
        (Tcl_CmdProc *)tcl_cg_dataset_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_dataset_write",
        (Tcl_CmdProc *)tcl_cg_dataset_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_bcdataset_info",
        (Tcl_CmdProc *)tcl_cg_bcdataset_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_bcdataset_write",
        (Tcl_CmdProc *)tcl_cg_bcdataset_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_bcdataset_read",
        (Tcl_CmdProc *)tcl_cg_bcdataset_read, 0, 0);

    Tcl_CreateCommand (interp, "cg_bcdata_write",
        (Tcl_CmdProc *)tcl_cg_bcdata_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ndiscrete",
        (Tcl_CmdProc *)tcl_cg_ndiscrete, 0, 0);
    Tcl_CreateCommand (interp, "cg_discrete_read",
        (Tcl_CmdProc *)tcl_cg_discrete_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_discrete_write",
        (Tcl_CmdProc *)tcl_cg_discrete_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_n_rigid_motions",
        (Tcl_CmdProc *)tcl_cg_n_rigid_motions, 0, 0);
    Tcl_CreateCommand (interp, "cg_rigid_motion_read",
        (Tcl_CmdProc *)tcl_cg_rigid_motion_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_rigid_motion_write",
        (Tcl_CmdProc *)tcl_cg_rigid_motion_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_n_arbitrary_motions",
        (Tcl_CmdProc *)tcl_cg_n_arbitrary_motions, 0, 0);
    Tcl_CreateCommand (interp, "cg_arbitrary_motion_read",
        (Tcl_CmdProc *)tcl_cg_arbitrary_motion_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_arbitrary_motion_write",
        (Tcl_CmdProc *)tcl_cg_arbitrary_motion_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_simulation_type_read",
        (Tcl_CmdProc *)tcl_cg_simulation_type_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_simulation_type_write",
        (Tcl_CmdProc *)tcl_cg_simulation_type_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_biter_read",
        (Tcl_CmdProc *)tcl_cg_biter_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_biter_write",
        (Tcl_CmdProc *)tcl_cg_biter_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ziter_read",
        (Tcl_CmdProc *)tcl_cg_ziter_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_ziter_write",
        (Tcl_CmdProc *)tcl_cg_ziter_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_gravity_read",
        (Tcl_CmdProc *)tcl_cg_gravity_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_gravity_write",
        (Tcl_CmdProc *)tcl_cg_gravity_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_axisym_read",
        (Tcl_CmdProc *)tcl_cg_axisym_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_axisym_write",
        (Tcl_CmdProc *)tcl_cg_axisym_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_rotating_read",
        (Tcl_CmdProc *)tcl_cg_rotating_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_rotating_write",
        (Tcl_CmdProc *)tcl_cg_rotating_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_bc_wallfunction_read",
        (Tcl_CmdProc *)tcl_cg_bc_wallfunction_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_bc_wallfunction_write",
        (Tcl_CmdProc *)tcl_cg_bc_wallfunction_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_bc_area_read",
        (Tcl_CmdProc *)tcl_cg_bc_area_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_bc_area_write",
        (Tcl_CmdProc *)tcl_cg_bc_area_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_conn_periodic_read",
        (Tcl_CmdProc *)tcl_cg_conn_periodic_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_periodic_write",
        (Tcl_CmdProc *)tcl_cg_conn_periodic_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_conn_average_read",
        (Tcl_CmdProc *)tcl_cg_conn_average_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_average_write",
        (Tcl_CmdProc *)tcl_cg_conn_average_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_1to1_periodic_read",
        (Tcl_CmdProc *)tcl_cg_1to1_periodic_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_periodic_write",
        (Tcl_CmdProc *)tcl_cg_1to1_periodic_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_average_read",
        (Tcl_CmdProc *)tcl_cg_1to1_average_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_average_write",
        (Tcl_CmdProc *)tcl_cg_1to1_average_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_goto",
        (Tcl_CmdProc *)tcl_cg_goto, 0, 0);
    Tcl_CreateCommand (interp, "cg_gorel",
        (Tcl_CmdProc *)tcl_cg_gorel, 0, 0);
    Tcl_CreateCommand (interp, "cg_gopath",
        (Tcl_CmdProc *)tcl_cg_gopath, 0, 0);
    Tcl_CreateCommand (interp, "cg_where",
        (Tcl_CmdProc *)tcl_cg_where, 0, 0);

    Tcl_CreateCommand (interp, "cg_convergence_read",
        (Tcl_CmdProc *)tcl_cg_convergence_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_convergence_write",
        (Tcl_CmdProc *)tcl_cg_convergence_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_state_read",
        (Tcl_CmdProc *)tcl_cg_state_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_state_write",
        (Tcl_CmdProc *)tcl_cg_state_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_equationset_read",
        (Tcl_CmdProc *)tcl_cg_equationset_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_equationset_chemistry_read",
        (Tcl_CmdProc *)tcl_cg_equationset_chemistry_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_equationset_elecmagn_read",
        (Tcl_CmdProc *)tcl_cg_equationset_elecmagn_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_equationset_write",
        (Tcl_CmdProc *)tcl_cg_equationset_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_governing_read",
        (Tcl_CmdProc *)tcl_cg_governing_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_governing_write",
        (Tcl_CmdProc *)tcl_cg_governing_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_diffusion_read",
        (Tcl_CmdProc *)tcl_cg_diffusion_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_diffusion_write",
        (Tcl_CmdProc *)tcl_cg_diffusion_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_model_read",
        (Tcl_CmdProc *)tcl_cg_model_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_model_write",
        (Tcl_CmdProc *)tcl_cg_model_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_narrays",
        (Tcl_CmdProc *)tcl_cg_narrays, 0, 0);
    Tcl_CreateCommand (interp, "cg_array_info",
        (Tcl_CmdProc *)tcl_cg_array_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_array_read",
        (Tcl_CmdProc *)tcl_cg_array_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_array_read_as",
        (Tcl_CmdProc *)tcl_cg_array_read_as, 0, 0);
    Tcl_CreateCommand (interp, "cg_array_write",
        (Tcl_CmdProc *)tcl_cg_array_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nuser_data",
        (Tcl_CmdProc *)tcl_cg_nuser_data, 0, 0);
    Tcl_CreateCommand (interp, "cg_user_data_read",
        (Tcl_CmdProc *)tcl_cg_user_data_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_user_data_write",
        (Tcl_CmdProc *)tcl_cg_user_data_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nintegrals",
        (Tcl_CmdProc *)tcl_cg_nintegrals, 0, 0);
    Tcl_CreateCommand (interp, "cg_integral_read",
        (Tcl_CmdProc *)tcl_cg_integral_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_integral_write",
        (Tcl_CmdProc *)tcl_cg_integral_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_rind_read",
        (Tcl_CmdProc *)tcl_cg_rind_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_rind_write",
        (Tcl_CmdProc *)tcl_cg_rind_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ndescriptors",
        (Tcl_CmdProc *)tcl_cg_ndescriptors, 0, 0);
    Tcl_CreateCommand (interp, "cg_descriptor_read",
        (Tcl_CmdProc *)tcl_cg_descriptor_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_descriptor_write",
        (Tcl_CmdProc *)tcl_cg_descriptor_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_units_read",
        (Tcl_CmdProc *)tcl_cg_units_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_units_write",
        (Tcl_CmdProc *)tcl_cg_units_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_nunits",
        (Tcl_CmdProc *)tcl_cg_nunits, 0, 0);
    Tcl_CreateCommand (interp, "cg_unitsfull_read",
        (Tcl_CmdProc *)tcl_cg_units_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_unitsfull_write",
        (Tcl_CmdProc *)tcl_cg_units_write, (ClientData)1, 0);

    Tcl_CreateCommand (interp, "cg_exponents_info",
        (Tcl_CmdProc *)tcl_cg_exponents_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_exponents_read",
        (Tcl_CmdProc *)tcl_cg_exponents_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_exponents_write",
        (Tcl_CmdProc *)tcl_cg_exponents_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_nexponents",
        (Tcl_CmdProc *)tcl_cg_nexponents, 0, 0);
    Tcl_CreateCommand (interp, "cg_expfull_read",
        (Tcl_CmdProc *)tcl_cg_exponents_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_expfull_write",
        (Tcl_CmdProc *)tcl_cg_exponents_write, (ClientData)1, 0);

    Tcl_CreateCommand (interp, "cg_conversion_info",
        (Tcl_CmdProc *)tcl_cg_conversion_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_conversion_read",
        (Tcl_CmdProc *)tcl_cg_conversion_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_conversion_write",
        (Tcl_CmdProc *)tcl_cg_conversion_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_dataclass_read",
        (Tcl_CmdProc *)tcl_cg_dataclass_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_dataclass_write",
        (Tcl_CmdProc *)tcl_cg_dataclass_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_gridlocation_read",
        (Tcl_CmdProc *)tcl_cg_gridlocation_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_gridlocation_write",
        (Tcl_CmdProc *)tcl_cg_gridlocation_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ordinal_read",
        (Tcl_CmdProc *)tcl_cg_ordinal_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_ordinal_write",
        (Tcl_CmdProc *)tcl_cg_ordinal_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ptset_info",
        (Tcl_CmdProc *)tcl_cg_ptset_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_ptset_read",
        (Tcl_CmdProc *)tcl_cg_ptset_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_ptset_write",
        (Tcl_CmdProc *)tcl_cg_ptset_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_is_link",
        (Tcl_CmdProc *)tcl_cg_is_link, 0, 0);
    Tcl_CreateCommand (interp, "cg_link_read",
        (Tcl_CmdProc *)tcl_cg_link_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_link_write",
        (Tcl_CmdProc *)tcl_cg_link_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_delete_node",
        (Tcl_CmdProc *)tcl_cg_delete_node, 0, 0);

    Tcl_CreateCommand (interp, "cg_get_error",
        (Tcl_CmdProc *)tcl_cg_get_error, 0, 0);
    Tcl_CreateCommand (interp, "cg_error_exit",
        (Tcl_CmdProc *)tcl_cg_error_exit, 0, 0);

    return TCL_OK;
}

