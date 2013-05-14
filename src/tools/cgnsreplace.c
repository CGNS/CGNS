#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef _WIN32
# include <io.h>
# define unlink _unlink
# define access _access
# define isatty _isatty
#else
# include <unistd.h>
#endif

#include "cgns_io.h"

/*#define CHECK_ORDER*/

static const char *fixed_labels[] = {
    "AdditionalExponents_t",
    "AdditionalUnits_t",
    "AreaType_t",
    "Area_t",
    "AverageInterface_t",
    "Axisymmetry_t",
    "BCData_t",
    "BCProperty_t",
    "CGNSLibraryVersion_t",
    "ChemicalKineticsModel_t",
    "ConvergenceHistory_t",
    "DataClass_t",
    "DataConversion_t",
    "DimensionalExponents_t",
    "DimensionalUnits_t",
    "EMConductivityModel_t",
    "EMElectricFieldModel_t",
    "EMMagneticFieldModel_t",
    "FamilyBC_t",
    "FamilyName_t",
    "FlowEquationSet_t",
    "GasModel_t",
    "GeometryFile_t",
    "GeometryFormat_t",
    "GoverningEquations_t",
    "Gravity_t",
    "GridConnectivityProperty_t",
    "GridConnectivityType_t",
    "GridCoordinates_t",
    "GridLocation_t",
    "IndexArray_t",
    "IndexRange_t",
    "Ordinal_t",
    "Periodic_t",
    "ReferenceState_t",
    "Rind_t",
    "RotatingCoordinates_t",
    "SimulationType_t",
    "ThermalConductivityModel_t",
    "ThermalRelaxationModel_t",
    "TurbulenceClosure_t",
    "TurbulenceModel_t",
    "ViscosityModel_t",
    "WallFunctionType_t",
    "WallFunction_t",
    "ZoneBC_t",
    "ZoneType_t"
};

#define NUM_LABELS (sizeof(fixed_labels)/sizeof(char *))

static const char *fixed_names[] = {
    "ArbitraryGridMotionPointers",
    "AxisymmetryAngle",
    "AxisymmetryAxisVector",
    "AxisymmetryReferencePoint",
    "CoordinateNames",
    "ElementConnectivity",
    "FamilyPointers",
    "FlowSolutionPointers",
    "GravityVector",
    "GridCoordinatesPointers",
    "InterpolantsDonor",
    "IterationValues",
    "NumberOfFamilies",
    "NumberOfZones",
    "OriginLocation",
    "ParentData",
    "ParentElements",
    "ParentElementsPosition"
    "RegionName",
    "RigidGridMotionPointers",
    "RigidRotationAngle",
    "RigidRotationRate",
    "RigidVelocity",
    "RotationCenter",
    "RotationRateVector",
    "SurfaceArea",
    "TimeValues",
    "ZoneGridConnectivityPointers",
    "ZonePointers",
    "ZoneSubRegionPointers"
};

#define NUM_NAMES (sizeof(fixed_names)/sizeof(char *))

static char *tempfile = NULL;
static int cgio_num = -1;

/*--------------------------------------------------------------------*/

static void error_exit (const char *funcname)
{
    char errmsg[CGIO_MAX_ERROR_LENGTH+1];

    cgio_error_message (errmsg);
    if (cgio_num >= 0) cgio_close_file (cgio_num);
    if (tempfile != NULL) unlink (tempfile);
    fprintf (stderr, "ERROR in %s:%s\n", funcname, errmsg);
    exit (1);
}

/*--------------------------------------------------------------------*/

static char *temporary_file (char *basename)
{
    char *p, *temp;
    int n = strlen(basename);

    temp = (char *) malloc (n + 10);
    if (temp == NULL) {
        fprintf (stderr, "malloc failed for temp filename\n");
        exit (1);
    }
    sprintf (temp, "%s.tmp", basename);
    p = temp + strlen(temp);
    for (n = 0; n < 1000; n++) {
        sprintf (p, "%3.3d", n);
        if (access (temp, 0)) return temp;
    }
    fprintf (stderr, "failed to create temporary filename\n");
    exit (1);
    return 0; /* quit compiler */
}

/*--------------------------------------------------------------------*/

static void create_copy (char *filename)
{
    int c;
    FILE *oldfp, *newfp;

    tempfile = temporary_file (filename);
    if (NULL == (oldfp = fopen (filename, "rb"))) {
        fprintf (stderr, "error opening %s for reading\n", filename);
        exit (1);
    }
    if (NULL == (newfp = fopen (tempfile, "w+b"))) {
        fclose (oldfp);
        fprintf (stderr, "error opening %s for writing\n", tempfile);
        unlink (tempfile);
        exit (1);
    }
    while (EOF != (c = getc (oldfp)))
        putc (c, newfp);
    fclose (oldfp);
    fclose (newfp);
}

/*--------------------------------------------------------------------*/

static int find_string (const char *name, int nlist, const char *list[])
{
    int cmp, mid, lo = 0, hi = nlist - 1;

    if (0 == strcmp (list[lo], name)) return 0;
    if (0 == strcmp (list[hi], name)) return 0;
    while (lo <= hi) {
        mid = (lo + hi) >> 1;
        cmp = strcmp (list[mid], name);
        if (0 == cmp) return 0;
        if (cmp > 0)
            hi = mid - 1;
        else
            lo = mid + 1;
    }

    return 1;
}

/*--------------------------------------------------------------------*/

static int can_change (const char *name, const char *label)
{
    if (0 == strcmp (label, "\"int\"") ||
        0 == strcmp (label, "\"int[IndexDimension]\"")) {
        return 0;
    }
    if (0 == strcmp (label, "Descriptor_t")) {
        if (0 == strcmp (name, "NormDefinitions") ||
            0 == strcmp (name, "ReferenceStateDescription") ||
            0 == strcmp (name, "BCRegionName") ||
            0 == strcmp (name, "GridConnectivityRegionName"))
            return 0;
        return 1;
    }
    if (0 == strcmp (label, "DataArray_t"))
        return find_string (name, NUM_NAMES, fixed_names);
    return find_string (label, NUM_LABELS, fixed_labels);
}

/*--------------------------------------------------------------------*/

static int recurse_children (double parent_id,
    const char *oldname, const char *newname)
{
    int nc, nchildren, len_ret, ndim, cnt = 0;
    char name[CGIO_MAX_NAME_LENGTH+1];
    char label[CGIO_MAX_LABEL_LENGTH+1];
    char type[CGIO_MAX_DATATYPE_LENGTH+1];
    char data[CGIO_MAX_NAME_LENGTH+1];
    cgsize_t dims[CGIO_MAX_DIMENSIONS];
    double child_id;

    if (cgio_number_children (cgio_num, parent_id, &nchildren))
        error_exit ("cgio_number_children");
    if (!nchildren) return 0;

    for (nc = 1; nc <= nchildren; nc++) {
        if (cgio_children_ids (cgio_num, parent_id, nc, 1, &len_ret,
               &child_id))
            error_exit ("cgio_children_ids");
        if (cgio_get_name (cgio_num, child_id, name))
            error_exit ("cgio_get_name");
        if (cgio_get_label (cgio_num, child_id, label))
            error_exit ("cgio_get_label");
        if (0 == strcmp (name, oldname) && can_change (name, label)) {
            if (cgio_set_name (cgio_num, parent_id, child_id, newname))
                error_exit ("cgio_set_name");
            cnt++;
        }
        if (cgio_is_link (cgio_num, child_id, &len_ret))
            error_exit ("cgio_is_link");
        if (len_ret > 0) continue;
        if (cgio_get_data_type (cgio_num, child_id, type))
            error_exit ("cgio_get_data_type");
        if (cgio_get_dimensions (cgio_num, child_id, &ndim, dims))
            error_exit ("cgio_get_data_type");
/* what about:
BaseIterativeData/FamilyPointers
BaseIterativeData/ZonePointers

ZoneIterativeData/ArbitraryGridMotionPointers
ZoneIterativeData/FlowSolutionsPointers
ZoneIterativeData/GridCoordinatesPointers
ZoneIterativeData/RigidGridMotionPointers
ZoneIterativeData/ZoneGridConnectivityPointers
ZoneIterativeData/ZoneSubRegionPointers
*/
        if (0 == strcmp (type, "C1") &&
            ndim == 1 && dims[0] <= CGIO_MAX_NAME_LENGTH) {
            if (cgio_read_all_data (cgio_num, child_id, data))
                error_exit ("cgio_read_all_data");
            if (0 == strcmp (data, oldname)) {
                dims[0] = strlen (newname);
                if (cgio_set_dimensions (cgio_num, child_id, type, 1, dims))
                    error_exit ("cgio_set_dimensions");
                if (cgio_write_all_data (cgio_num, child_id, newname))
                    error_exit ("cgio_write_all_data");
                cnt++;
            }
        }
        cnt += recurse_children (child_id, oldname, newname);
    }
    return cnt;
}

/*--------------------------------------------------------------------*/

static int get_names (char *buf, char **old, char **new)
{
    char *delim;

    delim = strchr (buf, '/');
    if (delim == NULL) {
        fprintf (stderr, "/ delimiter missing from \"%s\"\n", buf);
        return 0;
    }
    *delim++ = 0;
    if (!*buf || !*delim) {
        fprintf (stderr, "oldname and/or newname not given\n");
        return 0;
    }
    if (strlen(buf) > CGIO_MAX_NAME_LENGTH ||
        strlen(delim) > CGIO_MAX_NAME_LENGTH) {
        fprintf (stderr, "oldname and/or newname too long\n");
        return 0;
    }
    *old = buf;
    *new = delim;
    return 1;
}

/*--------------------------------------------------------------------*/

int main (int argc, char *argv[])
{
    double root_id;
    int n, cgio, cnt;
    char *oldname, *newname;
    char *p, buffer[128];

#ifdef CHECK_ORDER
    for (n = 1; n < NUM_LABELS; n++) {
        if (strcmp (fixed_labels[n-1], fixed_labels[n]) >= 0) {
            fprintf(stderr, "%s >= %s\n",
                fixed_labels[n-1], fixed_labels[n]);
            return 1;
        }
    }
    for (n = 1; n < NUM_NAMES; n++) {
        if (strcmp (fixed_names[n-1], fixed_names[n]) >= 0) {
            fprintf(stderr, "%s >= %s\n",
                fixed_names[n-1], fixed_names[n]);
            return 1;
        }
    }
#endif

    if (argc < 2) {
        fprintf(stderr, "usage:cgnsreplace CGNSfile oldname1/newname1 "
                        "[oldname2/newname2 [...]]\n");
        return 1;
    }
    if (access (argv[1], 0)) {
        fprintf (stderr, "%s does not exist\n", argv[1]);
        return 1;
    }
    create_copy (argv[1]);

    if (cgio_open_file (tempfile, CGIO_MODE_MODIFY,
            CGIO_FILE_NONE, &cgio))
        error_exit ("cgio_open_file");
    cgio_num = cgio;
    if (cgio_get_root_id (cgio_num, &root_id))
        error_exit ("cgio_get_root_id");

    if (argc > 2) {
        for (n = 2; n < argc; n++) {
            strcpy (buffer, argv[n]);
            if (get_names (buffer, &oldname, &newname)) {
                printf ("replacing \"%s\" with \"%s\"\n", oldname, newname);
                cnt = recurse_children (root_id, oldname, newname);
                printf (" replaced %d occurences\n", cnt);
            }
        }
    }
    else {
        while (1) {
            if (isatty (0)) putchar ('>');
            if (NULL == fgets (buffer, sizeof(buffer), stdin)) break;
            p = buffer + strlen(buffer);
            while (--p >= buffer && isspace(*p))
                ;
            *++p = 0;
            for (p = buffer; *p && isspace(*p); p++)
                ;
            if (!*p) break;
            if (get_names (p, &oldname, &newname)) {
                printf ("replacing \"%s\" with \"%s\"\n", oldname, newname);
                cnt = recurse_children (root_id, oldname, newname);
                printf (" replaced %d occurences\n", cnt);
            }
        }
    }

    if (cgio_close_file (cgio_num))
        error_exit ("cgio_close_file");

    unlink (argv[1]);
    if (rename (tempfile, argv[1])) {
        fprintf (stderr, "rename of %s -> %s failed\n", tempfile, argv[1]);
        return 1;
    }

    return 0;
}

