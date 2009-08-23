/*
 * convert_dataclass.c - convert between dimensional/nondimensional variables
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "getargs.h"
#include "cgnslib.h"
#include "cgnsutil.h"
#include "calc.h"

/* command line options */

static char options[] = "b:z:s:f:dnuhv";

static char *usgmsg[] = {
    "usage  : convert_dataclass [options] CGNSfile [newCGNSfile]",
    "options:",
    "   -b<base> = use CGNS base number <base> (default 1)",
    "   -z<zone> = read zone number <zone> (default all)",
    "   -s<sol>  = read solution number <sol> (default all)",
    "   -f<file> = read conversion expressions from <file>",
    "   -d       = convert to Dimensional (default)",
    "   -n       = convert to NormalizedByDimensional",
    "   -u       = convert to NormalizedByUnknownDimensional",
    "   -v       = verbose (show expression parsing errors)",
    NULL
};

static int verbose = 0;
static DataClass_t dataclass = DataClassNull;
static char buff[1024];
static char cgnstemp[17] = "";

static int unconverted = 0;
static int expressions = 0;
static int conversions = 0;

/*-------------------------------------------------------------------*/

static void get_error (
#ifdef PROTOTYPE
    int errnum, char *errmsg, int pos, char *str)
#else
    errnum, errmsg, pos, str)
int errnum, pos;
char *errmsg, *str;
#endif
{
    /* fatal error */

    if (errnum < 0) {
        if (cgnsFile)
            cg_close (cgnsFile);
        if (*cgnstemp)
            unlink (cgnstemp);
        fprintf (stderr, "%s\n", errmsg);
        exit (-1);
    }

    /* parsing error */

    if (verbose) {
        printf ("ERROR:");
        if (NULL != str) {
            printf ("%s\n      ", str);
            while (pos-- > 0)
                putchar ('-');
            putchar ('^');
        }
        printf ("%s\n", errmsg);
    }
}

/*-------------------------------------------------------------------*/

static void write_units (
#ifdef PROTOTYPE
    Units *units)
#else
    units)
Units *units;
#endif
{
    int n;

    for (n = 0; n < 5; n++) {
        if (units->units[n]) {
            if (cg_units_write (
                    (MassUnits_t)units->units[0],
                    (LengthUnits_t)units->units[1],
                    (TimeUnits_t)units->units[2],
                    (TemperatureUnits_t)units->units[3],
                    (AngleUnits_t)units->units[4]))
                cgnsCalcFatal ((char *)cg_get_error());
            break;
        }
    }

#ifdef NofValidWallFunctionTypes
    if (n == 5) cg_delete_node ("DimensionalUnits");
#endif
    for (n = 0; n < 5; n++) {
        if (units->exps[n]) {
            if (cg_exponents_write (RealSingle, units->exps))
                cgnsCalcFatal ((char *)cg_get_error());
            break;
        }
    }
#ifdef NofValidWallFunctionTypes
    if (n == 5) cg_delete_node ("DimensionalExponents");
#endif
}

/*-------------------------------------------------------------------*/

static void update_zone (
#ifdef PROTOTYPE
    void
#endif
){
    int n, i, len;
    Variable *var = coordinates;
    VECSYM *sym;
    VECFLOAT *vf;
    float *f = NULL;
    Units *units;

    printf (" coordinates...\n");
    for (n = 0; n < NumCoordinates; n++, var++) {
        len = var->len;
        printf ("  %s -> ", var->name);
        if (var->dataclass == dataclass) {
            puts ("OK");
            continue;
        }
        if ((sym = find_symbol (var->name, 0)) != NULL) {
            expressions++;
            printf ("expression\n");
            fflush (stdout);
            if (vecsym_type(sym) != VECSYM_VECTOR ||
                vecsym_veclen(sym) != len)
                cgnsCalcFatal ("result is not a vector of the right length");
            vf = vecsym_vector(sym);
            units = vecsym_user(sym);
        }
        else if (var->hasconv) {
            conversions++;
            printf ("conversion\n");
            fflush (stdout);
            sym = cgnsCalcCommand (var->name);
            vf = vecsym_vector(sym);
            if (dataclass == Dimensional) {
                for (i = 0; i < len; i++)
                    vf[i] = vf[i] * var->dataconv[0] + var->dataconv[1];
            }
            else {
                for (i = 0; i < len; i++)
                    vf[i] = (vf[i] - var->dataconv[1]) / var->dataconv[0];
            }
            units = NULL;
        }
        else {
            unconverted++;
            puts ("???");
            continue;
        }
        if (var->datatype == RealSingle) {
            if (f == NULL) {
                f = (float *) malloc (len * sizeof(float));
                if (f == NULL)
                    cgnsCalcFatal ("malloc failed for coordinate data");
            }
            for (i = 0; i < len; i++)
                f[i] = (float)vf[i];
            if (cg_coord_write (cgnsFile, cgnsBase, cgnsZone,
                RealSingle, var->name, f, &i))
                cgnsCalcFatal ((char *)cg_get_error());
        }
        else {
            if (cg_coord_write (cgnsFile, cgnsBase, cgnsZone,
                RealDouble, var->name, vf, &i))
                cgnsCalcFatal ((char *)cg_get_error());
        }
        if (cg_goto (cgnsFile, cgnsBase, "Zone_t", cgnsZone,
            "GridCoordinates_t", 1, "DataArray_t", n+1, "end") ||
            cg_dataclass_write (dataclass))
            cgnsCalcFatal ((char *)cg_get_error());
        if (units != NULL) write_units (units);
    }
    if (f != NULL)
        free (f);
}

/*-------------------------------------------------------------------*/

static void update_solution (
#ifdef PROTOTYPE
    void
#endif
){
    int n, i, len;
    Variable *var = variables;
    VECSYM *sym;
    VECFLOAT *vf;
    float *f = NULL;
    Units *units;

    printf (" solution %d - %s...\n", cgnsSoln, SolnName);
    for (n = 0; n < NumVariables; n++, var++) {
        len = var->len;
        printf ("  %s -> ", var->name);
        if (var->dataclass == dataclass) {
            puts ("OK");
            continue;
        }
        if ((sym = find_symbol (var->name, 0)) != NULL) {
            expressions++;
            printf ("expression\n");
            fflush (stdout);
            if (vecsym_type(sym) != VECSYM_VECTOR ||
                vecsym_veclen(sym) != len)
                cgnsCalcFatal ("result is not a vector of the right length");
            vf = vecsym_vector(sym);
            units = vecsym_user(sym);
        }
        else if (var->hasconv) {
            conversions++;
            printf ("conversion\n");
            fflush (stdout);
            sym = cgnsCalcCommand (var->name);
            vf = vecsym_vector(sym);
            if (dataclass == Dimensional) {
                for (i = 0; i < len; i++)
                    vf[i] = vf[i] * var->dataconv[0] + var->dataconv[1];
            }
            else {
                for (i = 0; i < len; i++)
                    vf[i] = (vf[i] - var->dataconv[1]) / var->dataconv[0];
            }
            units = NULL;
        }
        else {
            unconverted++;
            puts ("???");
            continue;
        }
        if (var->datatype == RealSingle) {
            if (f == NULL) {
                f = (float *) malloc (len * sizeof(float));
                if (f == NULL)
                    cgnsCalcFatal ("malloc failed for coordinate data");
            }
            for (i = 0; i < len; i++)
                f[i] = (float)vf[i];
            if (cg_field_write (cgnsFile, cgnsBase, cgnsZone, cgnsSoln,
                RealSingle, var->name, f, &i))
                cgnsCalcFatal ((char *)cg_get_error());
        }
        else {
            if (cg_field_write (cgnsFile, cgnsBase, cgnsZone, cgnsSoln,
                RealDouble, var->name, vf, &i))
                cgnsCalcFatal ((char *)cg_get_error());
        }
        if (cg_goto (cgnsFile, cgnsBase, "Zone_t", cgnsZone,
            "FlowSolution_t", cgnsSoln, "DataArray_t", n+1, "end") ||
            cg_dataclass_write (dataclass))
            cgnsCalcFatal ((char *)cg_get_error());
        if (units != NULL) write_units (units);
    }
    if (f != NULL)
        free (f);
}

/*-------------------------------------------------------------------*/

char *next_line (
#ifdef PROTOTYPE
    FILE *fp)
#else
    fp)
FILE *fp;
#endif
{
    int n = 0, len;
    char *p, line[257];

    while (fgets (line, sizeof(line), fp) != NULL) {
        line[sizeof(line)-1] = 0;
        p = line + strlen(line);
        while (--p >= line && isspace(*p))
            ;
        *++p = 0;
        for (p = line; *p && isspace(*p); p++)
            ;
        if (!*p) continue;
        strcpy (buff, p);
        n = strlen (buff);
        while (buff[n-1] == '\\') {
            for (n -= 2; n >= 0 && isspace(buff[n]); n--)
                ;
            buff[++n] = 0;
            if (fgets (line, sizeof(line), fp) == NULL) break;
            line[sizeof(line)-1] = 0;
            p = line + strlen(line);
            while (--p >= line && isspace(*p))
                ;
            *++p = 0;
            for (p = line; *p && isspace(*p); p++)
                ;
            if (!*p) break;
            len = strlen (p);
            if (n + len >= sizeof(buff))
                cgnsCalcFatal ("internal command buffer length exceeded");
            strcpy (&buff[n], p);
            n += len;
        }
        if ((p = strchr (buff, '#')) != NULL)
            *p = 0;
        for (p = buff+strlen(buff)-1; p >= buff && isspace(*p); p--)
            ;
        *++p = 0;
        for (p = buff; *p && isspace(*p); p++)
            ;
        if (*p) return (p);
    }
    return (NULL);
}

/*-------------------------------------------------------------------*/

static void parse_convfile (
#ifdef PROTOTYPE
    char *cnvfile)
#else
    cnvfile)
char *cnvfile;
#endif
{
    char *cmd;
    FILE *fp = fopen (cnvfile, "r");

    printf (" parsing conversion file...\n");
    if (fp == NULL) {
        sprintf (buff, "couldn't open <%s> for reading", cnvfile);
        cgnsCalcFatal (buff);
    }
    cgnsCalcReset ();
    while ((cmd = next_line (fp)) != NULL)
        cgnsCalcCommand (cmd);
    fclose (fp);
}

/*-------------------------------------------------------------------*/

int main (argc, argv)
int argc;
char *argv[];
{
    int n, nz, ns, scnt;
    int ibase = 1, izone = 0, isol = 0;
    char *convfile = NULL, *tmpfile;

    if (argc < 2)
        print_usage (usgmsg, NULL);

    while ((n = getargs (argc, argv, options)) > 0) {
        switch (n) {
            case 'b':
                ibase = atoi (argarg);
                break;
            case 'z':
                izone = atoi (argarg);
                break;
            case 's':
                isol = atoi (argarg);
                break;
            case 'f':
                convfile = argarg;
                break;
            case 'd':
                dataclass = Dimensional;
                break;
            case 'n':
                dataclass = NormalizedByDimensional;
                break;
            case 'u':
                dataclass = NormalizedByUnknownDimensional;
                break;
            case 'v':
                verbose = 1;
                break;
        }
    }

    if (dataclass == DataClassNull)
        print_usage (usgmsg, "need to select one of -d, -n or -u");

    if (argind == argc)
        print_usage (usgmsg, "CGNSfile not given");
    if (access (argv[argind], 0))
        cgnsCalcFatal ("CGNSfile does not exist");

    /* create a working copy */

    printf ("creating a working copy of %s\n", argv[argind]);
    tmpfile = temporary_file (argv[argind]);
    copy_file (argv[argind], tmpfile);

    /* read CGNS file */

    printf ("reading CGNS file from %s\n", tmpfile);
    cgnsCalcInit (tmpfile, 1, get_error);
    if (ibase != 1) cgnsCalcBase (ibase);
    printf ("reading base %d - %s\n", cgnsBase, BaseName);

    for (nz = 1; nz <= NumZones; nz++) {
        if (!izone || nz == izone) {
            scnt = 0;
            cgnsCalcZone (nz);
            printf ("\nupdating zone %d - %s\n", cgnsZone, ZoneName);
            for (ns = 1; ns <= NumSolns; ns++) {
                if (!isol || ns == isol) {
                    cgnsCalcSoln (ns);
                    if (convfile != NULL)
                        parse_convfile (convfile);
                    if (!scnt++)
                        update_zone ();
                    update_solution ();
                }
            }
            if (!scnt) {
                if (convfile != NULL)
                    parse_convfile (convfile);
                update_zone ();
            }
        }
    }

    cgnsCalcDone ();
    if (expressions + conversions == 0)
        cgnsCalcFatal ("no variables were converted");

    if (argind + 1 < argc) argind++;
    printf ("\nrenaming %s to %s\n", tmpfile, argv[argind]);
    unlink (argv[argind]);
    if (rename (tmpfile, argv[argind])) {
        char msg[512];
        sprintf (msg, "rename %s -> %s failed", tmpfile, argv[argind]);
        cgnsCalcFatal (msg);
        exit (1);
    }
    free (tmpfile);
    printf ("%d variables converted using expressions\n", expressions);
    printf ("%d variables converted using conversion factors\n", conversions);
    printf ("%d variables were unchanged\n", unconverted);

    return 0;
}
