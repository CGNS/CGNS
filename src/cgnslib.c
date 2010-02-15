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

/***********************************************************************
 * Revisions:
 *
 ***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#if defined(_WIN32) && !defined(__NUTC__)
#include <io.h>     /* suggested by MTI */
#else
#include <unistd.h>
#endif
#include "cgnslib.h"
#include "cgns_header.h"
#include "cgns_io.h"
#if defined(_WIN32) && !defined(__NUTC__)
#include <ctype.h>
#ifndef F_OK
#define R_OK    004 /* Test for Read permission */
#define W_OK    002 /* Test for Write permission */
#define X_OK    001 /* Test for eXecute permission */
#define F_OK    000 /* Test for existence of File */
#endif
/* fix for unresolved reference to __ftol2 when using VC7 with VC6 libs */
/* see http://www.manusoft.com/Resources/ARXTips/Main.stm */
#ifdef NEED_FTOL2
#ifdef __cplusplus
extern "C" {
#endif
long _ftol(double);
long _ftol2(double dValue) {return _ftol(dValue);}
#ifdef __cplusplus
}
#endif
#endif
#endif
#ifdef MEM_DEBUG
#include "cg_malloc.h"
#endif

/***********************************************************************
 * external variable declarations
 ***********************************************************************/
cgns_file *cgns_files = 0;
cgns_file *cg;
int n_cgns_files = 0;
cgns_posit *posit = 0;
int posit_file, posit_base, posit_zone;
int CGNSLibVersion=CGNS_VERSION;/* Version of the CGNSLibrary*1000  */
int cgns_compress = -1;
int cgns_filetype = CG_FILE_NONE;

extern void (*cgns_error_handler)(int, char *);

/***********************************************************************
 * Name strings
 ***********************************************************************/
char const * MassUnitsName[NofValidMassUnits] =
    {"Null", "UserDefined",
     "Kilogram", "Gram", "Slug", "PoundMass"
    };
char const * LengthUnitsName[NofValidLengthUnits] =
    {"Null", "UserDefined",
     "Meter", "Centimeter", "Millimeter", "Foot", "Inch"
    };
char const * TimeUnitsName[NofValidTimeUnits] =
    {"Null", "UserDefined",
     "Second"
    };
char const * TemperatureUnitsName[NofValidTemperatureUnits] =
    {"Null", "UserDefined",
     "Kelvin", "Celsius", "Rankine", "Fahrenheit"
     };
char const * AngleUnitsName[NofValidAngleUnits] =
    {"Null", "UserDefined",
     "Degree", "Radian"
    };
char const * ElectricCurrentUnitsName[NofValidElectricCurrentUnits] =
    {"Null", "UserDefined",
     "Ampere", "Abampere", "Statampere", "Edison", "a.u."
    };
char const * SubstanceAmountUnitsName[NofValidSubstanceAmountUnits] =
    {"Null", "UserDefined",
     "Mole", "Entities", "StandardCubicFoot", "StandardCubicMeter"
    };
char const * LuminousIntensityUnitsName[NofValidLuminousIntensityUnits] =
    {"Null", "UserDefined",
     "Candela", "Candle", "Carcel", "Hefner", "Violle"
    };
char const * DataClassName[NofValidDataClass] =
    {"Null", "UserDefined",
     "Dimensional", "NormalizedByDimensional",
     "NormalizedByUnknownDimensional", "NondimensionalParameter",
     "DimensionlessConstant"
    };
char const * GridLocationName[NofValidGridLocation] =
    {"Null", "UserDefined",
     "Vertex", "CellCenter", "FaceCenter", "IFaceCenter",
     "JFaceCenter", "KFaceCenter", "EdgeCenter"
    };
char const * BCDataTypeName[NofValidBCDataTypes] =
    {"Null", "UserDefined",
     "Dirichlet", "Neumann"
    };
char const * GridConnectivityTypeName[NofValidGridConnectivityTypes] =
    {"Null", "UserDefined",
     "Overset", "Abutting", "Abutting1to1"
    };
char const * PointSetTypeName[NofValidPointSetTypes] =
    {"Null", "UserDefined",
     "PointList",  "PointListDonor",
     "PointRange", "PointRangeDonor",
     "ElementRange", "ElementList", "CellListDonor"
    };
char const * GoverningEquationsTypeName[NofValidGoverningEquationsTypes]=
    {"Null", "UserDefined",
     "FullPotential", "Euler", "NSLaminar",
     "NSTurbulent", "NSLaminarIncompressible",
     "NSTurbulentIncompressible"
    };
char const * ModelTypeName[NofValidModelTypes]=
    {"Null", "UserDefined",
     "Ideal", "VanderWaals", "Constant", "PowerLaw", "SutherlandLaw",
     "ConstantPrandtl", "EddyViscosity", "ReynoldsStress", "ReynoldsStressAlgebraic",
     "Algebraic_BaldwinLomax", "Algebraic_CebeciSmith",
     "HalfEquation_JohnsonKing", "OneEquation_BaldwinBarth",
     "OneEquation_SpalartAllmaras", "TwoEquation_JonesLaunder",
     "TwoEquation_MenterSST", "TwoEquation_Wilcox",
     "CaloricallyPerfect", "ThermallyPerfect",
     "ConstantDensity", "RedlichKwong",
     "Frozen", "ThermalEquilib", "ThermalNonequilib",
     "ChemicalEquilibCurveFit", "ChemicalEquilibMinimization",
     "ChemicalNonequilib",
     "EMElectricField", "EMMagneticField", "EMConductivity",
     "Voltage", "Interpolated", "Equilibrium_LinRessler", "Chemistry_LinRessler"
    };
char const * BCTypeName[NofValidBCTypes] =
    {"Null", "UserDefined",
     "BCAxisymmetricWedge", "BCDegenerateLine", "BCDegeneratePoint",
     "BCDirichlet", "BCExtrapolate", "BCFarfield", "BCGeneral",
     "BCInflow", "BCInflowSubsonic", "BCInflowSupersonic", "BCNeumann",
     "BCOutflow", "BCOutflowSubsonic", "BCOutflowSupersonic",
     "BCSymmetryPlane", "BCSymmetryPolar", "BCTunnelInflow",
     "BCTunnelOutflow", "BCWall", "BCWallInviscid", "BCWallViscous",
     "BCWallViscousHeatFlux", "BCWallViscousIsothermal", "FamilySpecified"
     };
char const * DataTypeName[NofValidDataTypes] =
    {"Null", "UserDefined",
     "Integer", "RealSingle", "RealDouble", "Character"
    };
char const * ElementTypeName[NofValidElementTypes] =
    {"Null", "UserDefined",
     "NODE", "BAR_2", "BAR_3",
     "TRI_3", "TRI_6",
     "QUAD_4", "QUAD_8", "QUAD_9",
     "TETRA_4", "TETRA_10",
     "PYRA_5", "PYRA_14",
     "PENTA_6", "PENTA_15", "PENTA_18",
     "HEXA_8", "HEXA_20", "HEXA_27",
     "MIXED", "NGON_n", "NFACE_n","PYRA_13"
    };
char const * ZoneTypeName[NofValidZoneTypes] =
    {"Null", "UserDefined",
     "Structured", "Unstructured"
    };
char const * RigidGridMotionTypeName[NofValidRigidGridMotionTypes] =
    {"Null", "UserDefined",
     "ConstantRate", "VariableRate"
    };
char const * ArbitraryGridMotionTypeName[NofValidArbitraryGridMotionTypes] =
    {"Null", "UserDefined",
     "NonDeformingGrid", "DeformingGrid"
    };
char const * SimulationTypeName[NofValidSimulationTypes] =
    {"Null", "UserDefined",
     "TimeAccurate", "NonTimeAccurate"
    };
char const * WallFunctionTypeName[NofValidWallFunctionTypes] =
    {"Null", "UserDefined", "Generic"
    };
char const * AreaTypeName[NofValidAreaTypes] =
    {"Null", "UserDefined",
     "BleedArea", "CaptureArea"
    };
char const * AverageInterfaceTypeName[NofValidAverageInterfaceTypes] =
    {"Null", "UserDefined",
     "AverageAll", "AverageCircumferential", "AverageRadial",
     "AverageI", "AverageJ", "AverageK"
    };

/***********************************************************************
 * global variable definitions
 ***********************************************************************/
int n_open = 0;
int cgns_file_size = 0;
int file_number_offset = 0;
int VersionList[] = {3000,
                     2540, 2530, 2520, 2510, 2500,
                     2460, 2420, 2400,
                     2300, 2200, 2100,2000, 1270, 1200, 1100, 1050};
#define nVersions (sizeof(VersionList)/sizeof(int))

/***********************************************************************
 * library functions
 ***********************************************************************/

/* check for a valid CGNS file */

int cg_is_cgns(const char *filename, int *file_type) {
    int cgio, ierr;
    double rootid, childid;

    *file_type = CG_FILE_NONE;
    if (cgio_open_file(filename, CG_MODE_READ, CG_FILE_NONE, &cgio))
        return CG_ERROR;
    cgio_get_root_id(cgio, &rootid);
    cgio_get_file_type(cgio, file_type);
    ierr = cgio_get_node_id(cgio, rootid, "CGNSLibraryVersion", &childid);
    cgio_close_file(cgio);
    return ierr ? CG_ERROR : CG_OK;
}

/***********************************************************************
 * cg_open(char *filename, int mode, int *file_number)
 *
 ***********************************************************************/

int cg_open(char const * filename, int mode, int *file_number) {
    int not_found, dim_vals, cgio;
    double dummy_id;
    float FileVersion;

#ifdef __CG_MALLOC_H__
    fprintf(stderr, "before open:files %d/%d: memory %d/%d\n", n_open,
        cgns_file_size, cgmemnow(), cgmemmax());
#endif

    /* determine accessibility of a file :
       If the requested access is permitted, a value of 0 is returned. */

    not_found = access(filename, F_OK) ;

    /* check file mode */
    switch(mode) {
        case CG_MODE_READ:
            if (not_found) {
                cgi_error("Error opening file: '%s' not found!", filename);
                return CG_ERROR;
            }
            break;
        case CG_MODE_WRITE:
            if (!not_found) {
                unlink(filename);
                /*cgi_error("Error opening file: '%s' already exists!", filename);
                        return CG_ERROR;
                */
            }
            break;
        case CG_MODE_MODIFY:
            if (not_found) {
                cgi_error("Error opening file: '%s' not found!", filename);
                return CG_ERROR;
            }
            break;
        default:
            cgi_error("Unknown opening file mode: %d ??",mode);
            return CG_ERROR;
    }

    /* set default file type if not done */
    if (cgns_filetype == CG_FILE_NONE)
        cg_set_file_type(CG_FILE_NONE);

    /* Open CGNS file */
    if (cgio_open_file(filename, mode, cgns_filetype, &cgio)) {
        cg_io_error("cgio_open_file");
        return CG_ERROR;
    }
    n_open++;

    /* make sure there is enough space in the cgns_files array */
    if (cgns_file_size == 0) {
        cgns_file_size = 1;
        cgns_files = CGNS_NEW(cgns_file,cgns_file_size);
    } else if (n_cgns_files == cgns_file_size) {
        cgns_file_size *= 2;
        cgns_files = CGNS_RENEW(cgns_file,cgns_file_size, cgns_files);
    }
    cg = &(cgns_files[n_cgns_files]);
    n_cgns_files++;
    (*file_number) = n_cgns_files + file_number_offset;

    /* Keep in-memory copy of cgns file 'header' information */
    cg->mode = mode;
    cg->filename = CGNS_NEW(char,strlen(filename) + 1);
    strcpy(cg->filename, filename);
    cg->cgio = cgio;
    cgio_get_root_id(cgio, &cg->rootid);
    cg->file_number = (*file_number);
    cg->version = 0;
    cg->deleted = 0;
    cg->added = 0;

     /* CGNS-Library Version */
    if (mode == CG_MODE_WRITE) {
        dim_vals = 1;
        FileVersion = (float) CGNS_DOTVERS;
        if (cgi_new_node(cg->rootid, "CGNSLibraryVersion",
            "CGNSLibraryVersion_t", &dummy_id, "R4", 1, &dim_vals,
            (void *)&FileVersion)) return CG_ERROR;
        cg->version = CGNSLibVersion;
    }
    else {

     /* read file version from file and set cg->version = FileVersion*1000 */
        if (cg_version(cg->file_number, &FileVersion)) return CG_ERROR;

     /* Check that the library version is at least as recent as the one used
        to create the file being read */

        if (cg->version > CGNSLibVersion) {

        /* This code allows reading version newer than the lib,
               as long as the 1st digit of the versions are equal */
            if ((cg->version / 1000) > (CGNSLibVersion / 1000)) {
                cgi_error("The file %s was written with a more recent version of the CGNS library.  You must update your CGNS library before trying to read this file.",filename);
                return CG_ERROR;
            }
            /* warn only if different in second digit */
            if ((cg->version / 100) > (CGNSLibVersion / 100)) {
                cgi_warning("The file being read is more recent that the CGNS library used");
            }
        }
    }

    /* Get ADF Database version & dates, and ADF Library version */

    if (cgio_file_version(cg->cgio, cg->dtb_version, cg->creation_date,
            cg->modify_date)) {
        cg_io_error("cgio_file_version");
        return CG_ERROR;
    }
    if (cgio_library_version(cg->cgio, cg->adf_lib_version)) {
        cg_io_error("cgio_library_version");
        return CG_ERROR;
    }

    /* read CGNS file */
    if (mode == CG_MODE_READ || mode == CG_MODE_MODIFY) {
        if (cgi_read()) return CG_ERROR;

        /* update version number in modify mode */
        if (cg->version < CGNSLibVersion && mode == CG_MODE_MODIFY) {
            int nnod;
            double *id;
            FileVersion = (float)CGNS_DOTVERS;
            if (cgi_get_nodes(cg->rootid, "CGNSLibraryVersion_t",
                    &nnod, &id))
                return CG_ERROR;
            if (nnod) {
                if (cgio_write_all_data(cg->cgio, id[0], &FileVersion)) {
                    cg_io_error("cgio_write_all_data");
                    return CG_ERROR;
                }
                free(id);
            }
            else {
                dim_vals = 1;
                if (cgi_new_node(cg->rootid, "CGNSLibraryVersion",
                    "CGNSLibraryVersion_t", &dummy_id, "R4", 1, &dim_vals,
                    (void *)&FileVersion)) return CG_ERROR;
            }
            cg->version = CGNSLibVersion;
        }
    } else {
        cg->nbases=0;
        cg->base = 0;
    }

#ifdef __CG_MALLOC_H__
    fprintf(stderr, "after  open:files %d/%d: memory %d/%d\n", n_open,
        cgns_file_size, cgmemnow(), cgmemmax());
#endif

    return CG_OK;
}

int cg_version(int file_number, float *FileVersion) {
    int nnod;
    double *id;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

/* if open in CG_MODE_WRITE */
    if (cg->version) {
        (*FileVersion)=(float)(cg->version)/1000;
        return CG_OK;
    }

/* if open in MODE_READ or MODE_MODIFY */
     /* get number of CGNSLibraryVersion_t nodes and their ID */
    if (cgi_get_nodes(cg->rootid, "CGNSLibraryVersion_t", &nnod, &id))
        return CG_ERROR;
    if (nnod==0) {
        cg->version=1050;
        *FileVersion= (float) 1.05;
    } else if (nnod!=1) {
        cgi_error("More then one CGNSLibraryVersion_t node found under ROOT.");
        return CG_ERROR;
    } else {
        int vers, ndim, dim_vals[12], temp_version;
        char_33 node_name;
        char_33 data_type;
        void *data;

        if (cgi_read_node(id[0], node_name, data_type, &ndim, dim_vals,
                &data, 1)) {
            cgi_error("Error reading CGNS-Library-Version");
            return CG_ERROR;
        }
     /* check data type */
        if (strcmp(data_type,"R4")!=0) {
            cgi_error("Unexpected data type for CGNS-Library-Version='%s'",data_type);
            return CG_ERROR;
        }
     /* check data dim */
        if (ndim != 1 || (dim_vals[0]!=1)) {
            cgi_error("Wrong data dimension for CGNS-Library-Version");
            return CG_ERROR;
        }
     /* save data */
        *FileVersion = *((float *)data);
        free(data);
        cg->version = (int)(1000.0*(*FileVersion)+0.5);

     /* To prevent round off error in version number for file of older or current version */
        temp_version = cg->version;
     /* cg->version = 0;  Commented for fwd compatibility */
        for (vers=0; vers<nVersions; vers++) {
            if (temp_version > (VersionList[vers]-2) &&
                temp_version < (VersionList[vers]+2)) {
                cg->version = VersionList[vers];
                break;
            }
        }
        if (cg->version == 0) {
            cgi_error("Error:  Unable to determine the version number");
            return CG_ERROR;
        }

        free(id);
    }
#if DEBUG_VERSION
    printf("FileVersion=%f\n",*FileVersion);
    printf("cg->version=%d\n",cg->version);
#endif

    return CG_OK;
}

int cg_close(int file_number) {

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

#ifdef __CG_MALLOC_H__
    fprintf(stderr, "before close:files %d/%d: memory %d/%d\n", n_open,
        cgns_file_size, cgmemnow(), cgmemmax());
#endif

    if (cgns_compress && cg->mode == CG_MODE_MODIFY &&
       (cg->deleted || cgns_compress == 1)) {
        if (cgio_compress_file (cg->cgio, cg->filename)) {
            cg_io_error("cgio_compress_file");
            return CG_ERROR;
        }
    }
    else {
        if (cgio_close_file(cg->cgio)) {
            cg_io_error("cgio_close_file");
            return CG_ERROR;
        }
    }
    n_open--;

     /* Free the in-memory copy of the CGNS file */
    cgi_free_file(cg);
    cg->mode = CG_MODE_CLOSED;

    /* if all files are closed, free up memory */

    if (n_open == 0) {
      file_number_offset = n_cgns_files;
      free (cgns_files);
      cgns_files = 0;
      cgns_file_size = 0;
      n_cgns_files = 0;
    }

#ifdef __CG_MALLOC_H__
    fprintf(stderr, "after  close:files %d/%d: memory %d/%d\n",
        n_open, cgns_file_size, cgmemnow(), cgmemmax());
#endif

    return CG_OK;
}

int cg_save_as(int file_number, const char *filename, int file_type,
    int follow_links)
{
    int output;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (file_type == CG_FILE_NONE)
        file_type = cgns_filetype;
    if (cgio_is_supported(file_type)) {
        cgi_error("file type unknown or not supported");
        return CG_ERROR;
    }
    if (cgio_open_file(filename, CG_MODE_WRITE, file_type, &output)) {
        cg_io_error ("cgio_open_file");
        return CG_ERROR;
    }
    if (cgio_copy_file(cg->cgio, output, follow_links)) {
        cg_io_error("cgio_copy_file");
        return CG_ERROR;
    }
    if (cgio_close_file(output)) {
        cg_io_error("cgio_close_file");
        return CG_ERROR;
    }
    return CG_OK;
}

int cg_set_file_type(int file_type) {
    if (file_type == CG_FILE_NONE) {
        char *type = getenv("CGNS_FILETYPE");
        if (type == NULL || !*type)
            cgns_filetype = CG_FILE_ADF;
#ifdef BUILD_HDF5
        else if (*type == '2' || *type == 'h' || *type == 'H')
            cgns_filetype = CG_FILE_HDF5;
#endif        
#ifdef BUILD_XML
        else if (*type == '3' || *type == 'x' || *type == 'X')
            cgns_filetype = CG_FILE_XML;
#endif
        else
            cgns_filetype = CG_FILE_ADF;
    }
    else {
        if (cgio_is_supported(file_type)) {
            cgi_error("file type unknown or not supported");
            return CG_ERROR;
        }
        cgns_filetype = file_type;
    }
    return CG_OK;
}

int cg_get_file_type(int file_number, int *file_type) {
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;
    if (cgio_get_file_type(cg->cgio, file_type)) {
        cg_io_error("cgio_get_file_type");
        return CG_ERROR;
    }
    return CG_OK;
}

int cg_root_id(int file_number, double *root_id) {
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;
    if (cgio_get_root_id(cg->cgio, root_id)) {
        cg_io_error("cgio_get_root_id");
        return CG_ERROR;
    }
    return CG_OK;
}

/* configure stuff */

int cg_configure(int what, void *value) {
    /* cgio options */
    if (what > 100) {
        if (cgio_configure(what, value)) {
            cg_io_error("cgio_configure");
            return CG_ERROR;
        }
    }
    /* error message handler */
    else if (what == CG_CONFIG_ERROR) {
        cgns_error_handler = (void (*)(int, char *))value;
    }
    /* file compression */
    else if (what == CG_CONFIG_COMPRESS) {
        cgns_compress = (int)((size_t)value);
    }
    /* initialize link search path */
    else if (what == CG_CONFIG_SET_PATH) {
        return cg_set_path((const char *)value);
    }
    /* add to link search path */
    else if (what == CG_CONFIG_ADD_PATH) {
        return cg_set_path((const char *)value);
    }
    /* default file type */
    else if (what == CG_CONFIG_FILE_TYPE) {
        return cg_set_file_type((int)((size_t)value));
    }
    else {
        cgi_error("unknown config setting");
        return CG_ERROR;
    }
    return CG_OK;
}

int cg_error_handler(void (*func)(int, char *)) {
    cgns_error_handler = func;
    return CG_OK;
}

int cg_set_compress(int compress) {
    cgns_compress = compress;
    return CG_OK;
}

int cg_get_compress(int *compress) {
    *compress = cgns_compress;
    return CG_OK;
}

int cg_set_path(const char *path) {
    cgio_path_delete(NULL);
    if (path && *path) {
        if (cgio_path_add(path)) {
            cg_io_error("cgio_path_add");
            return CG_ERROR;
        }
    }
    return CG_OK;
}

int cg_add_path(const char *path) {
    if (cgio_path_add(path)) {
        cg_io_error("cgio_path_add");
        return CG_ERROR;
    }
    return CG_OK;
}

/*****************************************************************************\
 *          utility functions
\*****************************************************************************/

/* get type name with bounds checking */

const char *cg_get_name(int nnames, const char **names, int type) {
    if (type < 0 || type >= nnames) return "<invalid>";
    return names[type];
}

const char *cg_MassUnitsName(MassUnits_t type) {
    return cg_get_name(NofValidMassUnits,MassUnitsName,(int)type);
}
const char *cg_LengthUnitsName(LengthUnits_t type) {
    return cg_get_name(NofValidLengthUnits,LengthUnitsName,(int)type);
}
const char *cg_TimeUnitsName(TimeUnits_t type) {
    return cg_get_name(NofValidTimeUnits,TimeUnitsName,(int)type);
}
const char *cg_TemperatureUnitsName(TemperatureUnits_t type) {
    return cg_get_name(NofValidTemperatureUnits,TemperatureUnitsName,(int)type);
}
const char *cg_AngleUnitsName(AngleUnits_t type) {
    return cg_get_name(NofValidAngleUnits,AngleUnitsName,(int)type);
}
const char *cg_ElectricCurrentUnitsName(ElectricCurrentUnits_t type) {
    return cg_get_name(NofValidElectricCurrentUnits,ElectricCurrentUnitsName,(int)type);
}
const char *cg_SubstanceAmountUnitsName(SubstanceAmountUnits_t type) {
    return cg_get_name(NofValidSubstanceAmountUnits,SubstanceAmountUnitsName,(int)type);
}
const char *cg_LuminousIntensityUnitsName(LuminousIntensityUnits_t type) {
    return cg_get_name(NofValidLuminousIntensityUnits,LuminousIntensityUnitsName,(int)type);
}
const char *cg_DataClassName(DataClass_t type) {
    return cg_get_name(NofValidDataClass,DataClassName,(int)type);
}
const char *cg_GridLocationName(GridLocation_t type) {
    return cg_get_name(NofValidGridLocation,GridLocationName,(int)type);
}
const char *cg_BCDataTypeName(BCDataType_t type) {
    return cg_get_name(NofValidBCDataTypes,BCDataTypeName,(int)type);
}
const char *cg_GridConnectivityTypeName(GridConnectivityType_t type) {
    return cg_get_name(NofValidGridConnectivityTypes,GridConnectivityTypeName,(int)type);
}
const char *cg_PointSetTypeName(PointSetType_t type) {
    return cg_get_name(NofValidPointSetTypes,PointSetTypeName,(int)type);
}
const char *cg_GoverningEquationsTypeName(GoverningEquationsType_t type) {
    return cg_get_name(NofValidGoverningEquationsTypes,GoverningEquationsTypeName,(int)type);
}
const char *cg_ModelTypeName(ModelType_t type) {
    return cg_get_name(NofValidModelTypes,ModelTypeName,(int)type);
}
const char *cg_BCTypeName(BCType_t type) {
    return cg_get_name(NofValidBCTypes,BCTypeName,(int)type);
}
const char *cg_DataTypeName(DataType_t type) {
    return cg_get_name(NofValidDataTypes,DataTypeName,(int)type);
}
const char *cg_ElementTypeName(ElementType_t type) {
    return cg_get_name(NofValidElementTypes,ElementTypeName,(int)type);
}
const char *cg_ZoneTypeName(ZoneType_t type) {
    return cg_get_name(NofValidZoneTypes,ZoneTypeName,(int)type);
}
const char *cg_RigidGridMotionTypeName(RigidGridMotionType_t type) {
    return cg_get_name(NofValidRigidGridMotionTypes,RigidGridMotionTypeName,(int)type);
}
const char *cg_ArbitraryGridMotionTypeName(ArbitraryGridMotionType_t type) {
    return cg_get_name(NofValidArbitraryGridMotionTypes,ArbitraryGridMotionTypeName,(int)type);
}
const char *cg_SimulationTypeName(SimulationType_t type) {
    return cg_get_name(NofValidSimulationTypes,SimulationTypeName,(int)type);
}
const char *cg_WallFunctionTypeName(WallFunctionType_t type) {
    return cg_get_name(NofValidWallFunctionTypes,WallFunctionTypeName,(int)type);
}
const char *cg_AreaTypeName(AreaType_t type) {
    return cg_get_name(NofValidAreaTypes,AreaTypeName,(int)type);
}
const char *cg_AverageInterfaceTypeName(AverageInterfaceType_t type) {
    return cg_get_name(NofValidAverageInterfaceTypes,AverageInterfaceTypeName,(int)type);
}

/*****************************************************************************\
 *          Inquiry Functions for CGNSBase_t Nodes
\*****************************************************************************/

int cg_nbases(int file_number, int *nbases) {

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    *nbases = cg->nbases;
    return CG_OK;
}

int cg_base_read(int file_number, int B, char *basename, int *cell_dim,
    int *phys_dim) {
    cgns_base *base;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

    *cell_dim = base->cell_dim;
    *phys_dim = base->phys_dim;
    strcpy(basename, base->name);

    return CG_OK;
}

int cg_base_id(int file_number, int B, double *base_id) {
    cgns_base *base;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

    *base_id = base->id;
    return CG_OK;
}
/*****************************************************************************\
 *            Inquiry Functions for Zone_t Nodes
\*****************************************************************************/

int cg_nzones(int file_number, int B, int *nzones) {
    cgns_base *base;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

    *nzones = base->nzones;
    return CG_OK;
}

int cg_zone_type(int file_number, int B, int Z, ZoneType_t *type) {
    cgns_zone *zone;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    *type = zone->type;
    return CG_OK;
}

int cg_zone_read(int file_number, int B, int Z, char *zonename,
    int *nijk) {
    cgns_zone *zone;
    int i;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    strcpy(zonename, zone->name);

    for (i=0; i<3*(zone->index_dim); i++) nijk[i] = zone->nijk[i];

    return CG_OK;
}

int cg_zone_id(int file_number, int B, int Z, double *zone_id) {
    cgns_zone *zone;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    *zone_id = zone->id;
    return CG_OK;
}

/*****************************************************************************\
 *    Inquiry Functions for Family_t Nodes
\*****************************************************************************/
int cg_nfamilies(int file_number, int B, int *nfamilies) {
    cgns_base *base;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

    *nfamilies = base->nfamilies;
    return CG_OK;
}

int cg_family_read(int file_number, int B, int F, char *family_name,
    int *nboco, int *ngeos) {

    cgns_family *family;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    family = cgi_get_family(cg, B, F);
    if (family==0) return CG_ERROR;

    strcpy(family_name, family->name);
    *nboco = family->nfambc;
    *ngeos  = family->ngeos;

    return CG_OK;
}

int cg_fambc_read(int file_number, int B, int F, int BC,
    char *fambc_name, BCType_t *bocotype) {
    cgns_family *family;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    family = cgi_get_family(cg, B, F);
    if (family==0) return CG_ERROR;

    if (BC<=0 || BC>family->nfambc) {
        cgi_error("Invalid family b.c. number");
        return CG_ERROR;
    }
    strcpy(fambc_name,family->fambc[BC-1].name);
    *bocotype = family->fambc[BC-1].type;

    return CG_OK;
}

int cg_geo_read(int file_number, int B, int F, int G, char *geo_name,
    char **geo_file, char *CAD_name, int *npart) {
    cgns_family *family;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    family = cgi_get_family(cg, B, F);
    if (family==0) return CG_ERROR;

    if (G<=0 || G>family->ngeos) {
        cgi_error("Invalid geometry reference number");
        return CG_ERROR;
    }
    strcpy(geo_name,family->geo[G-1].name);
    strcpy(CAD_name,family->geo[G-1].format);

     /* This string is not limited to 32 characters and can't be pre-allocated
    in the application */
    geo_file[0]=CGNS_NEW(char,strlen(family->geo[G-1].file)+1);
    strcpy(geo_file[0],family->geo[G-1].file);

    *npart=family->geo[G-1].npart;

    return CG_OK;
}

int cg_part_read(int file_number, int B, int F, int G, int P, char *part_name) {
    cgns_family *family;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    family = cgi_get_family(cg, B, F);
    if (family==0) return CG_ERROR;

    if (P<=0 || P>family->geo[G-1].npart) {
        cgi_error("Invalid part number");
        return CG_ERROR;
    }
    strcpy(part_name,family->geo[G-1].part[P-1].name);
    return CG_OK;
}

/*****************************************************************************\
 *    Inquiry Functions for DiscreteData_t Nodes
\*****************************************************************************/
int cg_ndiscrete(int file_number, int B, int Z, int *ndiscrete) {
    cgns_zone *zone;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    (*ndiscrete) = zone->ndiscrete;
    return CG_OK;
}

int cg_discrete_read(int file_number, int B, int Z, int D, char *discrete_name) {

    cgns_discrete *discrete;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    discrete = cgi_get_discrete(cg, B, Z, D);
    if (discrete==0) return CG_ERROR;

    strcpy(discrete_name, discrete->name);

    return CG_OK;
}
/*****************************************************************************\
 *    Inquiry Functions for GridCoordinates_t Nodes
\*****************************************************************************/
int cg_ngrids(int file_number, int B, int Z, int *ngrids) {
    cgns_zone *zone;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* Get memory address for zone */
    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    (*ngrids) = zone->nzcoor;
    return CG_OK;
}

int cg_grid_read(int file_number, int B, int Z, int G, char *gridname) {
    cgns_zcoor *zcoor;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* Get memory address for GridCoordinates_t node */
    zcoor = cgi_get_zcoor(cg, B, Z, G);
    if (zcoor==0) return CG_ERROR;

     /* Return ADF name for the GridCoordinates_t node */
    strcpy(gridname,zcoor->name);
    return CG_OK;
}

/*****************************************************************************\
 *    Inquiry Functions for GridCoordinates_t/DataArray_t Nodes
\*****************************************************************************/
int cg_ncoords(int file_number, int B, int Z, int *ncoords) {
    cgns_zcoor *zcoor;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* Get memory address for node "GridCoordinates" */
    zcoor = cgi_get_zcoorGC(cg, B, Z);
    if (zcoor==0) *ncoords = 0;     /* if ZoneGridCoordinates_t is undefined */
    else          *ncoords = zcoor->ncoords;
    return CG_OK;
}

int cg_coord_info(int file_number, int B, int Z, int C, DataType_t *type, char *coordname) {
    cgns_zcoor *zcoor;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* Get memory address for node "GridCoordinates" */
    zcoor = cgi_get_zcoorGC(cg, B, Z);
    if (zcoor==0) return CG_ERROR;

    if (C>zcoor->ncoords || C<=0) {
        cgi_error("coord number %d invalid",C);
        return CG_ERROR;
    }
    *type = cgi_datatype(zcoor->coord[C-1].data_type);
    strcpy(coordname, zcoor->coord[C-1].name);

    return CG_OK;
}

int cg_coord_read(int file_number, int B, int Z, char const * coordname, DataType_t type,
          int const * rmin, int const * rmax, void *coord_ptr) {
    cgns_zcoor *zcoor;
    cgns_array *coord;
    int n, num=1, pos, i, j, k, c;
    void *xyz;
    int imin[3], imax[3];
    int read_full_range=1;
    int npt=0;
    int index_dim;

/* define: rmin[0]<=i<=rmax[0]  where the max range is 1<=i<=imax
       rmin[1]<=j<=rmax[1]                 1<=j<=jmax
 only 3D   rmin[2]<=k<=rmax[2]                 1<=k<=kmax
*/

     /* verify input */
    if (type!=RealSingle && type!=RealDouble) {
        cgi_error("Invalid data type for coord. array: %d",type);
        return CG_ERROR;
    }
     /* find address */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* Get memory address for node "GridCoordinates" */
    zcoor = cgi_get_zcoorGC(cg, B, Z);
    if (zcoor==0) return CG_ERROR;

     /* find the coord address in the database */
    coord = 0;
    for (c=0; c<zcoor->ncoords; c++) {
        if (strcmp(zcoor->coord[c].name, coordname)==0) {
            coord = &zcoor->coord[c];
            break;
        }
    }
    if (coord==0) {
        cgi_error("Coordinate %s not found.",coordname);
        return CG_NODE_NOT_FOUND;
    }

     /* number of points in data array: */
    index_dim=cg->base[B-1].zone[Z-1].index_dim;
    num = 1;
    for (n=0; n<index_dim; n++) num *= coord->dim_vals[n];

     /* verify that range requested does not exceed range stored */
    for (n=0; n<index_dim; n++) {
        if (rmax[n] > coord->dim_vals[n] || rmin[n]<1) {
            cgi_error("Invalid range of data requested");
            return CG_ERROR;
        }
    }

     /* check if requested to return full range */
    for (n=0; n<index_dim; n++) {
        if (rmin[n]!=1 || rmax[n] != coord->dim_vals[n]) {
            read_full_range=0;
            break;
        }
    }

     /* Read entire data array into 'xyz': */
    /* Decide not to use Read_Data due to the C/f77 ordering mismatch */
    if ((xyz=(void *)malloc(num*size_of(coord->data_type)))==NULL) {
        cgi_error("Error allocating xyz");
        return CG_ERROR;
    }
    if (cgio_read_all_data(cg->cgio, coord->id, xyz)) {
        cg_io_error("cgio_read_all_data");
        return CG_ERROR;
    }
    /* qick transfer of data if same data types and if read whole array */
    if (read_full_range && type==cgi_datatype(coord->data_type)) {
        memcpy(coord_ptr, xyz, num*size_of(coord->data_type));

    } else {
     /* Bring to 3D even if 2D or 1D */
        for (n=0; n<index_dim; n++) {
            imin[n]=rmin[n];
            imax[n]=rmax[n];
        }
        for (n=index_dim; n<3; n++) imin[n]=imax[n]=1;

     /* return array only for the range prescribed */
        npt=0;

     /* R4 - R4 */
        if (type==RealSingle && cgi_datatype(coord->data_type)==RealSingle) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*coord->dim_vals[0] + k*coord->dim_vals[0]*coord->dim_vals[1];
                *((float *) coord_ptr+npt) = *((float *) xyz+pos);
                npt++;
            }
     /* R4 - R8 */
        } else if (type==RealSingle && cgi_datatype(coord->data_type)==RealDouble) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*coord->dim_vals[0] + k*coord->dim_vals[0]*coord->dim_vals[1];
                *((float *) coord_ptr+npt) = (float)(*((double *) xyz+pos));
                npt++;
            }
     /* R8 - R4 */
        } else if (type==RealDouble && cgi_datatype(coord->data_type)==RealSingle) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*coord->dim_vals[0] + k*coord->dim_vals[0]*coord->dim_vals[1];
                *((double *) coord_ptr+npt) = (double)(*((float *) xyz+pos));
                npt++;
            }
     /* R8 - R8 */
        } else if (type==RealDouble && cgi_datatype(coord->data_type)==RealDouble) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*coord->dim_vals[0] + k*coord->dim_vals[0]*coord->dim_vals[1];
                *((double *) coord_ptr+npt) = *((double *) xyz+pos);
                npt++;
            }
        }
    }
    free(xyz);
    return CG_OK;
}

int cg_coord_id(int file_number, int B, int Z, int C, double *coord_id) {
    cgns_zcoor *zcoor;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* Get memory address for node "GridCoordinates" */
    zcoor = cgi_get_zcoorGC(cg, B, Z);
    if (zcoor==0) return CG_ERROR;

    if (C>zcoor->ncoords || C<=0) {
        cgi_error("coord number %d invalid",C);
        return CG_ERROR;
    }

    *coord_id = zcoor->coord[C-1].id;
    return CG_OK;
}

/*****************************************************************************\
 *    Inquiry Functions for Elements_t Nodes
\*****************************************************************************/

int cg_nsections(int file_number, int B, int Z, int *nsections) {
    cgns_zone *zone;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    (*nsections) = zone->nsections;
    return CG_OK;
}

int cg_section_read(int file_number, int B, int Z, int S, char *SectionName,
    ElementType_t *type, int *start, int *end, int *nbndry, int *parent_flag) {
    cgns_section *section;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    section = cgi_get_section(cg, B, Z, S);
    if (section == 0) return CG_ERROR;

    strcpy(SectionName, section->name);
    *type = section->el_type;
    *start = section->range[0];
    *end   = section->range[1];
    *nbndry = section->el_bound;
    if (section->parent) *parent_flag=1;
    else *parent_flag=0;
    return CG_OK;
}

/* This function was created for revision 1.2 to return the size of the
   connectivity vector, which can't be known without it when type=MIXED */

int cg_ElementDataSize(int file_number, int B, int Z, int S, int *ElementDataSize) {

    cgns_section *section;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    section = cgi_get_section(cg, B, Z, S);
    if (section == 0) return CG_ERROR;

    *ElementDataSize = section->connect->dim_vals[0];
    return CG_OK;
}

int cg_elements_read(int file_number, int B, int Z, int S, int *elements,
    int *parent_data) {
    cgns_section *section;
    int count, num, ElementDataSize=0;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    section = cgi_get_section(cg, B, Z, S);
    if (section == 0) return CG_ERROR;

     /* cgns_internals takes care of adjusting for version */
    ElementDataSize = section->connect->dim_vals[0];

     /* Double check ElementDataSize (not necessary) */
    num = section->range[1] - section->range[0] +1;
    count = cgi_element_data_size(section->el_type, num, section->connect->data);
    if (count < 0) return CG_ERROR;
    if (count && count != ElementDataSize) {
        cgi_error("Error in recorded element connectivity array...");
        return CG_ERROR;
    }

    if (section->connect->data) {
        memcpy(elements, section->connect->data, ElementDataSize*sizeof(int));
    }
    else {
        if (cgio_read_all_data(cg->cgio, section->connect->id, elements)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
    }

    if (section->parent && parent_data) {
        if (section->parent->data) {
            memcpy(parent_data, section->parent->data, num*4*sizeof(int));
        }
        else {
            if (cgio_read_all_data(cg->cgio, section->parent->id, parent_data)) {
                cg_io_error("cgio_read_all_data");
                return CG_ERROR;
            }
        }
    }

    return CG_OK;
}

/*****************************************************************************\
 *    Inquiry Functions for FlowSolution_t Nodes
\*****************************************************************************/
int cg_nsols(int file_number, int B, int Z, int *nsols) {
    cgns_zone *zone;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    *nsols = zone->nsols;
    return CG_OK;
}

int cg_sol_info(int file_number, int B, int Z, int S, char *solname,
        GridLocation_t *location) {
    cgns_sol *sol;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    sol = cgi_get_sol(cg, B, Z, S);
    if (sol==0) return CG_ERROR;

    strcpy(solname, sol->name);
    *location = sol->location;
    return CG_OK;
}

int cg_sol_id(int file_number, int B, int Z, int S, double *sol_id) {
    cgns_sol *sol;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    sol = cgi_get_sol(cg, B, Z, S);
    if (sol==0) return CG_ERROR;

    *sol_id = sol->id;
    return CG_OK;
}

/*****************************************************************************\
 *    Inquiry Functions for flow field  DataArray_t Nodes
\*****************************************************************************/
int cg_nfields(int file_number, int B, int Z, int S, int *nfields) {
    cgns_sol *sol;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    sol = cgi_get_sol(cg, B, Z, S);
    if (sol==0) return CG_ERROR;

    *nfields = sol->nfields;
    return CG_OK;
}

int cg_field_info(int file_number, int B, int Z, int S, int F, DataType_t *type,
          char *fieldname) {
    cgns_array *field;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    field = cgi_get_field(cg, B, Z, S, F);
    if (field==0) return CG_ERROR;

    strcpy(fieldname, field->name);
    *type = cgi_datatype(field->data_type);

    return CG_OK;
}

int cg_field_read(int file_number, int B, int Z, int S, char *fieldname, DataType_t type,
          int *rmin, int *rmax, void *field_ptr) {
    cgns_sol *sol;
    cgns_array *field;
    int n, num=1, pos, i, j, k, f;
    void *values;
    int imin[3], imax[3];
    int read_full_range=1;
    int index_dim;

/* define: rmin[0]<=i<=rmax[0]  where the max range is 1<=i<=imax
       rmin[1]<=j<=rmax[1]                 1<=j<=jmax
 only 3D   rmin[2]<=k<=rmax[2]                 1<=k<=kmax
*/
     /* verify input */
    if (type<0 || type>=NofValidDataTypes) {
        cgi_error("Invalid data type requested for flow solution: %d",type);
        return CG_ERROR;
    }
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    sol = cgi_get_sol(cg, B, Z, S);
    if (sol==0) return CG_ERROR;
    field = 0;
    for (f=0; f<sol->nfields; f++) {
        if (strcmp(sol->field[f].name, fieldname)==0) {
            field = cgi_get_field(cg, B, Z, S, f+1);
            if (field==0) return CG_ERROR;
            break;
        }
    }
    if (field==0) {
        cgi_error("Flow solution array %s not found",fieldname);
        return CG_NODE_NOT_FOUND;
    }

     /* number of points in data array: */
    index_dim=cg->base[B-1].zone[Z-1].index_dim;
    num =1;
    for (n=0; n<index_dim; n++) num *= field->dim_vals[n];

     /* verify that range requested does not exceed range stored */
    for (n=0; n<index_dim; n++) {
        if (rmax[n] > field->dim_vals[n] || rmin[n]<1) {
            cgi_error("Invalid range of data requested");
            return CG_ERROR;
        }
    }

     /* check if requested to return full range */
    for (n=0; n<index_dim; n++) {
        if (rmin[n]!=1 || rmax[n] != field->dim_vals[n]) {
            read_full_range=0;
            break;
        }
    }

     /* Read entire data array into 'values': */
    /* Decide not to use Read_Data due to the C/f77 ordering mismatch */
    if ((values=(void *)malloc(num*size_of(field->data_type)))==NULL) {
        cgi_error("Error allocating values");
        return CG_ERROR;
    }
    if (cgio_read_all_data(cg->cgio, field->id, values)) {
        cg_io_error("cgio_read_all_data");
        return CG_ERROR;
    }
     /* quick transfer of data if same data types and if read whole array */
    if (read_full_range && type == cgi_datatype(field->data_type)) {
        memcpy(field_ptr, values, num*size_of(field->data_type));

    } else {
     /* Bring to 3D even if 2D or 1D */
        for (n=0; n<index_dim; n++) {
            imin[n]=rmin[n];
            imax[n]=rmax[n];
        }
        for (n=index_dim; n<3; n++) imin[n]=imax[n]=1;

     /* return data array members in the range prescribed */
        n=0;

     /* I4 - I4 */
        if (type==Integer && cgi_datatype(field->data_type)==Integer) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((int *) field_ptr+n) = *((int *) values+pos);
                n++;
            }
     /* I4 - R4 */
        } else if (type==Integer && cgi_datatype(field->data_type)==RealSingle) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((int *) field_ptr+n) = (int) (*((float *) values+pos));
                n++;
            }
     /* I4 - R8 */
        } else if (type==Integer && cgi_datatype(field->data_type)==RealDouble) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((int *) field_ptr+n) = (int) (*((double *) values+pos));
                n++;
            }
     /* R4 - I4 */
        } else if (type==RealSingle && cgi_datatype(field->data_type)==Integer) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((float *) field_ptr+n) = (float) (*((int *) values+pos));
                n++;
            }
     /* R4 - R4 */
        } else if (type==RealSingle && cgi_datatype(field->data_type)==RealSingle) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((float *) field_ptr+n) = *((float *) values+pos);
                n++;
            }
     /* R4 - R8 */
        } else if (type==RealSingle && cgi_datatype(field->data_type)==RealDouble) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((float *) field_ptr+n) = (float)(*((double *) values+pos));
                n++;
            }
     /* R8 - I4 */
        } else if (type==RealDouble && cgi_datatype(field->data_type)==Integer) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((double *) field_ptr+n) = (double)(*((int *) values+pos));
                n++;
            }
     /* R8 - R4 */
        } else if (type==RealDouble && cgi_datatype(field->data_type)==RealSingle) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((double *) field_ptr+n) = (double)(*((float *) values+pos));
                n++;
            }
     /* R8 - R8 */
        } else if (type==RealDouble && cgi_datatype(field->data_type)==RealDouble) {
            for (k=imin[2]-1; k<imax[2]; k++)
            for (j=imin[1]-1; j<imax[1]; j++)
            for (i=imin[0]-1; i<imax[0]; i++) {
                pos = i + j*field->dim_vals[0] + k*field->dim_vals[0]*field->dim_vals[1];
                *((double *) field_ptr+n) = *((double *) values+pos);
                n++;
            }
        }
    }
    free(values);

    return CG_OK;
}

int cg_field_id(int file_number, int B, int Z, int S, int F, double *field_id) {
    cgns_array *field;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    field = cgi_get_field(cg, B, Z, S, F);
    if (field==0) return CG_ERROR;

    *field_id = field->id;
    return CG_OK;
}

/*****************************************************************************\
 *         Inquiry Functions for OversetHoles_t Nodes
\*****************************************************************************/

int cg_nholes(int file_number, int B, int Z, int *nholes) {
    cgns_zconn *zconn;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zconn = cgi_get_zconn(cg, B, Z);
    if (zconn==0) *nholes = 0;  /* if ZoneGridConnectivity_t is undefined */
    else          *nholes = zconn->nholes;
    return CG_OK;
}

int cg_hole_info(int file_number, int B, int Z, int I, char *holename,
    GridLocation_t *location, PointSetType_t *ptset_type, int *nptsets,
    int *npnts) {

    cgns_hole *hole;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    hole = cgi_get_hole(cg, B, Z, I);
    if (hole==0) return CG_ERROR;

    strcpy(holename, hole->name);
    *location = hole->location;
    *ptset_type = hole->nptsets ? hole->ptset[0].type : PointSetTypeNull;
    *nptsets = hole->nptsets;
     /* if multiple pointsets are defined, only PointRange is allowed */
    if (hole->nptsets==1) *npnts = hole->ptset[0].npts;
    else                  *npnts = 2*hole->nptsets;
    return CG_OK;
}

int cg_hole_read(int file_number, int B, int Z, int I, int *pnts) {
    cgns_hole *hole;
    int set, index_dim;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    hole = cgi_get_hole(cg, B, Z, I);
    if (hole==0) return CG_ERROR;

    index_dim = cg->base[B-1].zone[Z-1].index_dim;

     /* read point-set directly from ADF file */
    for (set=0; set<hole->nptsets; set++) {
        if (hole->ptset[set].npts>0) {
            if (cgio_read_all_data(cg->cgio, hole->ptset[set].id,
                (void *)((int *)pnts+2*index_dim*set))) {
                cg_io_error("cgio_read_all_data");
                return CG_ERROR;
            }
        } else {
            cgi_warning("Overset hole #%d set %d, of zone #%d, base #%d, contains no points",
                I, set, Z, B);
        }
    }

    return CG_OK;
}

int cg_hole_id(int file_number, int B, int Z, int I, double *hole_id) {
    cgns_hole *hole;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    hole = cgi_get_hole(cg, B, Z, I);
    if (hole==0) return CG_ERROR;

    *hole_id = hole->id;
    return CG_OK;
}

/*****************************************************************************\
 *         Inquiry Functions for GridConnectivity_t Nodes
\*****************************************************************************/

int cg_nconns(int file_number, int B, int Z, int *nconns) {
    cgns_zconn *zconn;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zconn = cgi_get_zconn(cg, B, Z);
    if (zconn==0) *nconns = 0;  /* if ZoneGridConnectivity_t is undefined */
    else          *nconns = zconn->nconns;
    return CG_OK;
}

/* in cg_conn_info, donor_datatype is useless starting with version 1.27, because
   it's always I4.  Howver this arg. is left for backward compatibility of API
   and to be able to read old files */
int cg_conn_info(int file_number, int B, int Z, int I, char *connectname,
         GridLocation_t *location, GridConnectivityType_t *type,
         PointSetType_t *ptset_type, int *npnts, char *donorname,
         ZoneType_t *donor_zonetype, PointSetType_t *donor_ptset_type,
         DataType_t *donor_datatype, int *ndata_donor) {

    int dZ;
    cgns_conn *conn;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    conn = cgi_get_conn(cg, B, Z, I);
    if (conn==0) return CG_ERROR;

    strcpy(connectname, conn->name);
    *type = conn->type;
    *location = conn->location;
    *ptset_type = conn->ptset.type;
    *npnts = conn->ptset.npts;

     /* information relative to donor */
    strcpy(donorname, conn->donor);
    *donor_datatype = cgi_datatype(conn->dptset.data_type);
    *ndata_donor = conn->dptset.npts;
    *donor_ptset_type = conn->dptset.type;

     /* Find ZoneType_t of DonorZone given its name */
    *donor_zonetype = ZoneTypeNull;

    for (dZ=0; dZ<cg->base[B-1].nzones; dZ++) {
        if (strcmp(cg->base[B-1].zone[dZ].name,donorname)==0) {
            *donor_zonetype = cg->base[B-1].zone[dZ].type;
            break;
        }
    }
    if (*donor_zonetype == 0) {
        cgi_error("cg_conn_info:donor zone %s does not exist",donorname);
        return CG_ERROR;
    }
    return CG_OK;
}

/* in cg_conn_read, donor_datatype is useless starting with version 1.27, because
   it's always I4.  However this arg. is left for backward compatibility of API
   and to be able to read old files */
int cg_conn_read(int file_number, int B, int Z, int I, int *pnts,
         DataType_t donor_datatype, void *donor_data) {
    char_33 data_type;
    cgns_conn *conn;
    void *dpnts;
    int n, size, cell_dim;

     /* verify input */
    if (donor_data != NULL && donor_datatype!=Integer &&
        donor_datatype!=RealSingle && donor_datatype!=RealDouble) {
        cgi_error("Invalid datatype requested for a point set: %d", donor_datatype);
        return CG_ERROR;
    }

     /* Find address */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    conn = cgi_get_conn(cg, B, Z, I);
    if (conn==0) return CG_ERROR;

    cell_dim = cg->base[B-1].cell_dim;

     /* read point-set of current zone from ADF file - always I4 integer */
    if (conn->ptset.npts>0) {
        if (cgio_read_all_data(cg->cgio, conn->ptset.id, (void *)pnts)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
    } else {
        cgi_warning("Interface receiver patch #%d of zone #%d, base #%d, contains no points",
            I, Z, B);
    }

    if (donor_data == NULL) return CG_OK;

     /* read donor points from ADF file - data_type may be I4, R4 or R8 */
    if (conn->dptset.npts>0) {
        cgns_ptset dptset = conn->dptset;
        int index_dim = 0;
        for (n=0; n<cg->base[B-1].nzones; n++) {
            if (strcmp(cg->base[B-1].zone[n].name,conn->donor)==0) {
                index_dim = cg->base[B-1].zone[n].type == Structured ? cell_dim : 1;
                break;
            }
        }
        if (index_dim == 0) {
            cgi_error("cg_conn_read:donor zone %s does not exist",conn->donor);
            return CG_ERROR;
        }
        size = index_dim*dptset.npts;
        if ((dpnts=(void *)malloc(size*size_of(dptset.data_type)))==NULL) {
            cgi_error("Error allocating dpnts...");
            return CG_ERROR;
        }
        if (cgio_read_all_data(cg->cgio, dptset.id, dpnts)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
        strcpy(data_type, dptset.data_type);
    } else {
        cgi_warning("Interface donor patch #%d of zone #%d, base #%d, contains no points",
            I, Z, B);
        return CG_OK;
    }

     /* Return value in precision requested */
    if (donor_datatype==Integer && cgi_datatype(data_type)==Integer) {
        for (n=0; n<size; n++)
            *((int *)donor_data+n) = *((int *)dpnts+n);

    } else if (donor_datatype==Integer && cgi_datatype(data_type)==RealSingle) {
        for (n=0; n<size; n++)
            *((int *)donor_data+n) = (int)(*((float *)dpnts+n));

    } else if (donor_datatype==Integer && cgi_datatype(data_type)==RealDouble) {
        for (n=0; n<size; n++)
            *((int *)donor_data+n) = (int)(*((double *)dpnts+n));

    } else if (donor_datatype==RealSingle && cgi_datatype(data_type)==Integer) {
        for (n=0; n<size; n++)
            *((float *)donor_data+n) = (float)(*((int *)dpnts+n));

    } else if (donor_datatype==RealSingle && cgi_datatype(data_type)==RealSingle) {
        for (n=0; n<size; n++)
            *((float *)donor_data+n) = *((float *)dpnts+n);

    } else if (donor_datatype==RealSingle && cgi_datatype(data_type)==RealDouble) {
        for (n=0; n<size; n++)
            *((float *)donor_data+n) = (float)(*((double *)dpnts+n));

    } else if (donor_datatype==RealDouble && cgi_datatype(data_type)==Integer) {
        for (n=0; n<size; n++)
            *((double *)donor_data+n) = (double)(*((int *)dpnts+n));

    } else if (donor_datatype==RealDouble && cgi_datatype(data_type)==RealSingle) {
        for (n=0; n<size; n++)
            *((double *)donor_data+n) = (double)(*((float *)dpnts+n));

    } else if (donor_datatype==RealDouble && cgi_datatype(data_type)==RealDouble) {
        for (n=0; n<size; n++)
            *((double *)donor_data+n) = *((double *)dpnts+n);
    }
    free(dpnts);

    return CG_OK;
}

int cg_conn_read_short(int file_number, int B, int Z, int I, int *pnts) {
    return cg_conn_read(file_number, B, Z, I, pnts, DataTypeNull, NULL);
}

int cg_conn_id(int file_number, int B, int Z, int I, double *conn_id) {
    cgns_conn *conn;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    conn = cgi_get_conn(cg, B, Z, I);
    if (conn==0) return CG_ERROR;

    *conn_id = conn->id;
    return CG_OK;
}

/*****************************************************************************\
 *         Inquiry Functions for GridConnectivity1to1_t Nodes
\*****************************************************************************/

int cg_n1to1(int file_number, int B, int Z, int *n1to1) {
    cgns_zconn *zconn;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zconn = cgi_get_zconn(cg, B, Z);
    if (zconn==0) *n1to1 = 0;   /* if ZoneGridConnectivity_t is undefined */
    else          *n1to1 = zconn->n1to1;
    return CG_OK;
}

int cg_n1to1_global(int file_number, int B, int *n1to1_global) {
    cgns_base *base;
    cgns_zone *zone;
    cgns_zconn *zconn;
    int Z, I, D;
    int_3 transform;
    int_6 donor_range, range;
    char_33 connectname, donorname;
     /* added for zones interfacing themselves (C-topology) */
    int ndouble=0;
    char_33 *Dzonename = 0;
    int_6 *Drange = 0, *Ddonor_range = 0;
    int index_dim;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

    *n1to1_global = 0;
    for (Z=1; Z<=base->nzones; Z++) {
        zone = cgi_get_zone(cg, B, Z);
        if (zone==0) return CG_ERROR;
        index_dim = zone->index_dim;
        zconn = cgi_get_zconn(cg, B, Z);
        if (zconn==0) continue; /* if ZoneGridConnectivity_t is undefined */
        if (zconn->n1to1 ==0) continue;
        for (I=1; I<=zconn->n1to1; I++) {
            if (cg_1to1_read(file_number, B, Z, I, connectname, donorname,
                         range, donor_range, transform)) return CG_ERROR;
            if (cgi_zone_no(base, donorname, &D)) return CG_ERROR;

             /* count each interface only once */
            if (Z<D) (*n1to1_global)++;

             /* Special treatment for zone interfacing itself */
            if (Z==D) {
             /* if this interface is not yet recorded, add to list */
                if (cgi_add_czone(zone->name, range, donor_range, index_dim,
                    &ndouble, &Dzonename, &Drange, &Ddonor_range)) {
                    (*n1to1_global)++;
                }
            }
        }           /* loop through interfaces of a zone */
    }           /* loop through zones */
    if (Dzonename) free(Dzonename);
    if (Drange) free(Drange);
    if (Ddonor_range) free(Ddonor_range);

    return CG_OK;
}


int cg_1to1_read(int file_number, int B, int Z, int I, char *connectname,
    char *donorname, int *range, int *donor_range, int *transform) {
    cgns_1to1 *one21;
    int i, index_dim;

/* in 2D, range[0], range[1] = imin, jmin
      range[2], range[3] = imax, jmax
   in 3D, range[0], range[1], range[2] = imin, jmin, kmin
      range[3], range[4], range[5] = imax, jmax, kmax
 */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    one21 = cgi_get_1to1(cg, B, Z, I);
    if (one21==0) return CG_ERROR;

     /* read pointset from ADF file */
    if (one21->ptset.npts > 0) {
        if (cgio_read_all_data(cg->cgio, one21->ptset.id, (void *)range)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
    } else {
        cgi_warning("1to1 interface %d (receiver side) for zone %d base % is undefined",
            I,Z,B);
    }

     /* read donor pointset from ADF file */
    if (one21->dptset.npts > 0) {
        if (cgio_read_all_data(cg->cgio, one21->dptset.id,
            (void *)donor_range)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
    } else {
        cgi_warning("1to1 interface %d (donor side) for zone %d base % is undefined",
            I,Z,B);
    }

     /* read transform from internal database */
    index_dim = cg->base[B-1].zone[Z-1].index_dim;
    for (i=0; i<index_dim; i++) transform[i] = one21->transform[i];

    strcpy(connectname, one21->name);
    strcpy(donorname, one21->donor);
    return CG_OK;
}

int cg_1to1_read_global(int file_number, int B, char **connectname, char **zonename,
    char **donorname, int **range, int **donor_range, int **transform){

    cgns_base *base;
    cgns_zone *zone;
    cgns_zconn *zconn;
    int Z, I, D, n=0, j, index_dim;
    char connect[33], donor[33];
    int rang[6], drang[6], trans[6];
     /* added for zones interfacing themselves (C-topology) */
    int ndouble=0;
    char_33 *Dzonename = 0;
    int_6 *Drange = 0, *Ddonor_range = 0;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

    for (Z=1; Z<=base->nzones; Z++) {
        zone = cgi_get_zone(cg, B, Z);
        if (zone->type==Unstructured) {
            cgi_error("GridConnectivity1to1 is only applicable to structured zones.");
            return CG_ERROR;
        }
        index_dim = zone->index_dim;
        zconn = cgi_get_zconn(cg, B, Z);
        if (zconn==0) continue; /* if ZoneGridConnectivity_t is undefined */
        if (zconn->n1to1 ==0) continue;
        for (I=1; I<=zconn->n1to1; I++) {
            if (cg_1to1_read(file_number, B, Z, I, connect, donor, rang,
                drang, trans)) return CG_ERROR;
            if (cgi_zone_no(base, donor, &D)) return CG_ERROR;
             /* count each interface only once */
            if (Z<D || (Z==D && cgi_add_czone(zone->name, rang, drang, index_dim,
                &ndouble, &Dzonename, &Drange, &Ddonor_range))) {
                strcpy(connectname[n], connect);
                strcpy(zonename[n],zone->name);
                strcpy(donorname[n], donor);
                for (j=0; j<index_dim; j++) {
                    range[n][j]= rang[j];
                    range[n][j+index_dim]= rang[j+index_dim];
                    donor_range[n][j]= drang[j];
                    donor_range[n][j+index_dim]= drang[j+index_dim];
                    transform[n][j] = trans[j];
                }
                n++;
            }
        }
    }
    if (Dzonename) free(Dzonename);
    if (Drange) free(Drange);
    if (Ddonor_range) free(Ddonor_range);
    return CG_OK;
}

int cg_1to1_id(int file_number, int B, int Z, int I, double *one21_id) {
    cgns_1to1 *one21;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    one21 = cgi_get_1to1(cg, B, Z, I);
    if (one21==0) return CG_ERROR;

    *one21_id = one21->id;
    return CG_OK;
}

/*****************************************************************************\
 *          Inquiry Functions for BC_t Nodes
\*****************************************************************************/

int cg_nbocos(int file_number, int B, int Z, int *nbocos) {
    cgns_zboco *zboco;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zboco = cgi_get_zboco(cg, B, Z);
    if (zboco==0) *nbocos = 0;  /* if ZoneBC_t is undefined */
    else          *nbocos = zboco->nbocos;
    return CG_OK;
}

int cg_boco_info(int file_number, int B, int Z, int BC, char *boconame,
    BCType_t *bocotype, PointSetType_t *ptset_type, int *npnts,
    int *NormalIndex, int *NormalListFlag, DataType_t *NormalDataType,
    int *ndataset) {

    cgns_boco *boco;
    int n, index_dim;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    boco = cgi_get_boco(cg, B, Z, BC);
    if (boco==0) return CG_ERROR;

    strcpy(boconame,boco->name);
    *bocotype = boco->type;
    if (boco->ptset) {
        *ptset_type = boco->ptset->type;
        *npnts = boco->ptset->npts;
    }
    else {
        *ptset_type = PointSetTypeNull;
        *npnts = 0;
    }

    index_dim = cg->base[B-1].zone[Z-1].index_dim;
    if (NormalIndex) {
        if (boco->Nindex) {
            for (n=0; n<index_dim; n++)
                NormalIndex[n]=boco->Nindex[n];
        } else {
            for (n=0; n<index_dim; n++)
                NormalIndex[n]=0;
        }
    }
    if (boco->normal && boco->ptset) {
        *NormalListFlag=boco->ptset->size_of_patch*cg->base[B-1].phys_dim;
        *NormalDataType = cgi_datatype(boco->normal->data_type);
    } else {
        *NormalListFlag=0;
        *NormalDataType=DataTypeNull;
    }
    *ndataset = boco->ndataset;

    return CG_OK;
}

int cg_boco_read(int file_number, int B, int Z, int BC, int *pnts, void *NormalList) {
    cgns_boco *boco;
    int phys_dim;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    boco = cgi_get_boco(cg, B, Z, BC);
    if (boco==0) return CG_ERROR;

     /* Read point-set directly from ADF-file */
    if (boco->ptset && boco->ptset->npts>0) {
        if (cgio_read_all_data(cg->cgio, boco->ptset->id, (void *)pnts)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
    } else {
        cgi_warning("B.C. patch %d of zone %d base %d is undefined",
            BC, Z, B);
    }

     /* if it exists, read NormalList */
    phys_dim=cg->base[B-1].phys_dim;
    if (NormalList && boco->normal && boco->ptset && boco->ptset->npts>0) {
        memcpy(NormalList, boco->normal->data,
        boco->ptset->size_of_patch*phys_dim*size_of(boco->normal->data_type));
    }

    return CG_OK;
}

int cg_boco_id(int file_number, int B, int Z, int BC, double *boco_id) {
    cgns_boco *boco;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    boco = cgi_get_boco(cg, B, Z, BC);
    if (boco==0) return CG_ERROR;

    *boco_id = boco->id;
    return CG_OK;
}

/*****************************************************************************\
 *          Inquiry Functions for BCDataSet_t Nodes
\*****************************************************************************/

int cg_dataset_read(int file_number, int B, int Z, int BC, int DSet, char *name,
     BCType_t *BCType, int *DirichletFlag, int *NeumannFlag) {

    cgns_dataset *dataset;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    dataset = cgi_get_dataset(cg, B, Z, BC, DSet);
    if (dataset==0) return CG_ERROR;

    strcpy(name, dataset->name);
    *BCType = dataset->type;
    if (dataset->dirichlet) *DirichletFlag=1;
    else                    *DirichletFlag=0;
    if (dataset->neumann) *NeumannFlag=1;
    else                  *NeumannFlag=0;

    return CG_OK;
}

/*****************************************************************************\
 *         Inquiry Functions for RigidGridMotion_t Nodes
\*****************************************************************************/

int cg_n_rigid_motions(int file_number, int B, int Z, int *n_rigid_motions) {
    cgns_zone *zone;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    *n_rigid_motions = zone->nrmotions;

    return CG_OK;
}

int cg_rigid_motion_read(int file_number, int B, int Z, int R, char *name,
    RigidGridMotionType_t *type) {

    cgns_rmotion *rmotion;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    rmotion = cgi_get_rmotion(cg, B, Z, R);
    if (rmotion==0) return CG_ERROR;

    strcpy(name, rmotion->name);
    *type = rmotion->type;

    return CG_OK;
}

/*****************************************************************************\
 *      Inquiry Functions for ArbitraryGridMotion_t Nodes
\*****************************************************************************/

int cg_n_arbitrary_motions(int file_number, int B, int Z, int *n_arbitrary_motions) {
    cgns_zone *zone;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    *n_arbitrary_motions = zone->namotions;

    return CG_OK;
}

int cg_arbitrary_motion_read(int file_number, int B, int Z, int A, char *name,
    ArbitraryGridMotionType_t *type) {

    cgns_amotion *amotion;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    amotion = cgi_get_amotion(cg, B, Z, A);
    if (amotion==0) return CG_ERROR;

    strcpy(name, amotion->name);
    *type = amotion->type;

    return CG_OK;
}

/*****************************************************************************\
 *      Inquiry Functions for SimulationType_t Node
\*****************************************************************************/

int cg_simulation_type_read(int file_number, int B, SimulationType_t *type) {
    cgns_base *base;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

    *type = base->type;

    return CG_OK;
}

/*****************************************************************************\
 *      Inquiry Functions for BaseIterativeData_t Node
\*****************************************************************************/

int cg_biter_read(int file_number, int B, char *bitername, int *nsteps) {
    cgns_biter *biter;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    biter = cgi_get_biter(cg, B);
    if (biter==0) return CG_NODE_NOT_FOUND;

    *nsteps = biter->nsteps;
    strcpy(bitername,biter->name);

    return CG_OK;
}

/*****************************************************************************\
 *      Inquiry Functions for ZoneIterativeData_t Node
\*****************************************************************************/

int cg_ziter_read(int file_number, int B, int Z, char *zitername) {
    cgns_ziter *ziter;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    ziter = cgi_get_ziter(cg, B, Z);
    if (ziter==0) return CG_NODE_NOT_FOUND;

    strcpy(zitername, ziter->name);

    return CG_OK;
}

/*****************************************************************************\
 *      Inquiry Function for Gravity_t Node
\*****************************************************************************/

int cg_gravity_read(int file_number, int B, float *gravity_vector) {
    cgns_base *base;
    cgns_gravity *gravity;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* get memory address for base */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* get memory address for gravity */
    gravity = cgi_get_gravity(cg, B);
    if (gravity==0) return CG_NODE_NOT_FOUND;

    memcpy(gravity_vector, gravity->vector->data, base->phys_dim*sizeof(float));
    return CG_OK;
}

/*****************************************************************************\
 *      Inquiry Function for Axisymmetry_t Node
\*****************************************************************************/

int cg_axisym_read(int file_number, int B, float *ref_point, float *axis) {
    int n;
    cgns_base *base;
    cgns_axisym *axisym;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* get memory address for base (for base->phys_dim) */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* get memory address for axisym */
    axisym = cgi_get_axisym(cg, B);
    if (axisym==0) return CG_NODE_NOT_FOUND;

    for (n=0; n<axisym->narrays; n++) {
        if (strcmp(axisym->array[n].name,"AxisymmetryReferencePoint")==0)
            memcpy(ref_point, axisym->array[n].data, base->phys_dim*sizeof(float));
        else if (strcmp(axisym->array[n].name,"AxisymmetryAxisVector")==0)
            memcpy(axis, axisym->array[n].data, base->phys_dim*sizeof(float));
    }
    return CG_OK;
}

/*****************************************************************************\
 *      Inquiry Function for BCProperty_t Node
\*****************************************************************************/

int cg_bc_wallfunction_read(int file_number, int B, int Z, int BC,
        WallFunctionType_t *WallFunctionType) {
    cgns_bprop *bprop;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* get memory address for bprop */
    bprop = cgi_get_bprop(cg, B, Z, BC);
    if (bprop==0) return CG_NODE_NOT_FOUND;

    if (bprop->bcwall==0) {
        cgi_error("BCProperty_t/WallFunction_t node doesn't exist under BC_t %d",BC);
        return CG_NODE_NOT_FOUND;
    }
    *WallFunctionType = bprop->bcwall->type;

    return CG_OK;
}

int cg_bc_area_read(int file_number, int B, int Z, int BC,
        AreaType_t *AreaType, float *SurfaceArea, char *RegionName) {
    int n;
    cgns_bprop *bprop;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* get memory address for bprop */
    bprop = cgi_get_bprop(cg, B, Z, BC);
    if (bprop==0) return CG_NODE_NOT_FOUND;

    if (bprop->bcarea==0) {
        cgi_error("BCProperty_t/Area_t node doesn't exist under BC_t %d",BC);
        return CG_NODE_NOT_FOUND;
    }
    *AreaType = bprop->bcarea->type;
    for (n=0; n<bprop->bcarea->narrays; n++) {
        if (strcmp("SurfaceArea",bprop->bcarea->array[n].name)==0)
            memcpy(SurfaceArea, bprop->bcarea->array[n].data, sizeof(float));
        else if (strcmp("RegionName",bprop->bcarea->array[n].name)==0) {
            memcpy(RegionName, bprop->bcarea->array[n].data, 32*sizeof(char));
            RegionName[32]='\0';
        }
    }

    return CG_OK;
}

/*****************************************************************************\
 *      Inquiry Function for GridConnectivityProperty_t Node
\*****************************************************************************/

int cg_conn_periodic_read(int file_number, int B, int Z, int I,
        float *RotationCenter, float *RotationAngle, float *Translation) {

    int n;
    cgns_base *base;
    cgns_cprop *cprop;
    cgns_cperio *cperio;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* get memory address for base (for base->phys_dim) */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* get memory address for cprop */
    cprop = cgi_get_cprop(cg, B, Z, I);
    if (cprop==0) return CG_NODE_NOT_FOUND;

    if (cprop->cperio == 0) {
        cgi_error("GridConnectivityProperty_t/Periodic_t node doesn't exist under GridConnectivity_t %d",I);
        return CG_NODE_NOT_FOUND;
    }
    cperio = cprop->cperio;

     /* Copy data to be returned */
    for (n=0; n<cperio->narrays; n++) {
        if (strcmp(cperio->array[n].name,"RotationCenter")==0)
            memcpy(RotationCenter, cperio->array[n].data, base->phys_dim*sizeof(float));
        else if (strcmp(cperio->array[n].name,"RotationAngle")==0)
            memcpy(RotationAngle, cperio->array[n].data, base->phys_dim*sizeof(float));
        else if (strcmp(cperio->array[n].name,"Translation")==0)
            memcpy(Translation, cperio->array[n].data, base->phys_dim*sizeof(float));
    }

    return CG_OK;
}

int cg_conn_average_read(int file_number, int B, int Z, int I,
        AverageInterfaceType_t *AverageInterfaceType) {

    cgns_cprop *cprop;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* get memory address for cprop */
    cprop = cgi_get_cprop(cg, B, Z, I);
    if (cprop==0) return CG_NODE_NOT_FOUND;

    if (cprop->caverage == 0) {
        cgi_error("GridConnectivityProperty_t/AverageInterface_t node doesn't exist under GridConnectivity_t %d",I);
        return CG_NODE_NOT_FOUND;
    }
    *AverageInterfaceType = cprop->caverage->type;

    return CG_OK;
}

int cg_1to1_periodic_read(int file_number, int B, int Z, int I,
        float *RotationCenter, float *RotationAngle, float *Translation)
{
    int n;
    cgns_base *base;
    cgns_cprop *cprop;
    cgns_cperio *cperio;
    cgns_1to1 *one21;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* get memory address for base (for base->phys_dim) */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* get memory address for cprop from one21->cprop */
    one21 = cgi_get_1to1(cg, B, Z, I);
    if (one21==0) return CG_ERROR;

    cprop = one21->cprop;

    if (cprop == 0 || cprop->cperio == 0) {
        cgi_error("GridConnectivityProperty_t/Periodic_t node doesn't exist under GridConnectivity1to1_t %d",I);
        return CG_NODE_NOT_FOUND;
    }
    cperio = cprop->cperio;

     /* Copy data to be returned */
    for (n=0; n<cperio->narrays; n++) {
        if (strcmp(cperio->array[n].name,"RotationCenter")==0)
            memcpy(RotationCenter, cperio->array[n].data, base->phys_dim*sizeof(float));
        else if (strcmp(cperio->array[n].name,"RotationAngle")==0)
            memcpy(RotationAngle, cperio->array[n].data, base->phys_dim*sizeof(float));
        else if (strcmp(cperio->array[n].name,"Translation")==0)
            memcpy(Translation, cperio->array[n].data, base->phys_dim*sizeof(float));
    }

    return CG_OK;
}

int cg_1to1_average_read(int file_number, int B, int Z, int I,
        AverageInterfaceType_t *AverageInterfaceType)
{
    cgns_cprop *cprop;
    cgns_1to1 *one21;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* get memory address for cprop from one21->cprop */
    one21 = cgi_get_1to1(cg, B, Z, I);
    if (one21==0) return CG_ERROR;

    cprop = one21->cprop;

    if (cprop == 0 || cprop->caverage == 0) {
        cgi_error("GridConnectivityProperty_t/AverageInterface_t node doesn't exist under GridConnectivity1to1_t %d",I);
        return CG_NODE_NOT_FOUND;
    }
    *AverageInterfaceType = cprop->caverage->type;

    return CG_OK;
}

/*****************************************************************************\
 *           Go - To Function
\*****************************************************************************/

int cg_goto(int file_number, int B, ...)
{
    int n;
    va_list ap;
    int index[CG_MAX_GOTO_DEPTH];
    char *label[CG_MAX_GOTO_DEPTH];

     /* initialize */
    posit = 0;

     /* set global variable cg */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    va_start(ap, B);

     /* read variable argument list */
    for (n = 0; n < CG_MAX_GOTO_DEPTH; n++) {
        label[n] = va_arg(ap,char *);
        if (label[n] == NULL || label[n][0] == 0) break;
        if (strcmp("end",label[n])==0 || strcmp("END",label[n])==0) break;
        index[n] = va_arg(ap, int);
    }
    va_end(ap);

    return cgi_set_posit(file_number, B, n, index, label);
}

/*-----------------------------------------------------------------------*/

int cg_gorel(int file_number, ...)
{
    int n = 0;
    int index[CG_MAX_GOTO_DEPTH];
    char *label[CG_MAX_GOTO_DEPTH];
    va_list ap;

    if (posit == 0) {
        cgi_error ("position not set with cg_goto");
        return CG_ERROR;
    }
    if (file_number != posit_file) {
        cgi_error("current position is in the wrong file");
        return CG_ERROR;
    }

    va_start (ap, file_number);
    for (n = 0; n < CG_MAX_GOTO_DEPTH; n++) {
        label[n] = va_arg(ap, char *);
        if (label[n] == NULL || label[n][0] == 0) break;
        if (strcmp("end",label[n])==0 || strcmp("END",label[n])==0) break;
        index[n] = va_arg(ap, int);
    }
    va_end(ap);

    return cgi_update_posit(n, index, label);
}

/*-----------------------------------------------------------------------*/

int cg_gopath(int file_number, const char *path)
{
    int n, len;
    const char *p = path, *s;
    int index[CG_MAX_GOTO_DEPTH];
    char label[CG_MAX_GOTO_DEPTH][33];
    char *lab[CG_MAX_GOTO_DEPTH];

    if (p == 0 || !*p) {
        cgi_error("path not given");
        return CG_ERROR;
    }

    /* absolute path */

    if (*p == '/') {
        int ierr, B = 0;

        posit = 0;
        while (*++p && *p == '/')
            ;
        if (!*p) {
            cgi_error("base name not given");
            return CG_ERROR;
        }
        s = strchr(p, '/');
        if (s == 0)
            len = strlen(p);
        else
            len = (int)(s - p);
        if (len > 32) {
            cgi_error("base name in path is too long");
            return CG_ERROR;
        }
        strncpy(label[0], p, len);
        label[0][len] = 0;

        cg = cgi_get_file(file_number);
        if (cg == 0) return CG_ERROR;

        for (n = 0; n < cg->nbases; n++) {
            if (0 == strcmp(label[0], cg->base[n].name)) {
                B = n + 1;
                break;
            }
        }
        if (B == 0) {
            cgi_error("base '%s' not found", label[0]);
            return CG_ERROR;
        }
        ierr = cgi_set_posit(file_number, B, 0, index, lab);
        if (ierr != CG_OK) return ierr;
        if (s == 0) return CG_OK;
        p = s;
    }

    /* relative path */

    else {
        if (posit == 0) {
            cgi_error("position not set with cg_goto");
            return CG_ERROR;
        }
        if (file_number != posit_file) {
            cgi_error("current position is in the wrong file");
            return CG_ERROR;
        }
    }

    n = 0;
    while (p && *p) {
        while (*p && *p == '/') p++;
        if (!*p) break;
        s = strchr(p, '/');
        if (s == 0)
            len = strlen(p);
        else
            len = (int)(s - p);
        if (len > 32) {
            posit = 0;
            cgi_error("node name in path is too long");
            return CG_ERROR;
        }
        if (n == CG_MAX_GOTO_DEPTH) {
            posit = 0;
            cgi_error("path is too deep");
            return CG_ERROR;
        }
        strncpy(label[n], p, len);
        label[n][len] = 0;
        lab[n] = label[n];
        index[n++] = 0;
        p = s;
    }

    return cgi_update_posit(n, index, lab);
}

/*-----------------------------------------------------------------------*/

int cg_golist(int file_number, int B, int depth, char **label, int *index)
{
    if (depth >= CG_MAX_GOTO_DEPTH) {
        cgi_error("path is too deep");
        return CG_ERROR;
    }
    return cgi_set_posit(file_number, B, depth, index, label);
}

/*-----------------------------------------------------------------------*/

int cg_where(int *file_number, int *B, int *depth, char **label, int *num)
{
    int n;

    if (posit == 0) {
        cgi_error ("position not set with cg_goto");
        return CG_ERROR;
    }
    *file_number = posit_file;
    *B = posit_base;
    /* first entry is base */
    *depth = posit_depth > 1 ? posit_depth - 1 : 0;
    if (NULL != label) {
        for (n = 1; n < posit_depth; n++)
            strcpy(label[n-1], posit_stack[n].label);
    }
    if (NULL != num) {
        for (n = 1; n < posit_depth; n++)
            num[n-1] = posit_stack[n].index;
    }
    return CG_OK;
}

/*****************************************************************************\
 *           Read Multiple path nodes
\*****************************************************************************/

int cg_famname_read(char *family_name) {
    char *famname;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    famname = cgi_famname_address(CG_MODE_READ, &ier);
    if (famname==0) return ier;

    strcpy(family_name,famname);
    if (!strlen(famname)) return CG_NODE_NOT_FOUND;
    return CG_OK;
}

int cg_convergence_read(int *iterations, char **NormDefinitions) {
    cgns_converg *converg;
    cgns_descr *descr;
    int ier=0;

     /* verify input (cg set in cg_goto) */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    converg = cgi_converg_address(CG_MODE_READ, &ier);
    if (converg==0) return ier;

    (*iterations) = converg->iterations;
    if (converg->NormDefinitions==0) {
        NormDefinitions[0] = CGNS_NEW(char, 1);
        NormDefinitions[0][0]='\0';
    } else {
        descr = converg->NormDefinitions;
        NormDefinitions[0] = CGNS_NEW(char, strlen(descr->text)+1);
        strcpy(NormDefinitions[0], descr->text);
    }
    return CG_OK;
}

int cg_state_read(char **StateDescription) {
    cgns_state *state;
    cgns_descr *descr;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    state = cgi_state_address(CG_MODE_READ, &ier);
    if (state==0) return ier;

    if (state->StateDescription == 0) {
        StateDescription[0]=CGNS_NEW(char, 1);
        StateDescription[0][0]='\0';
    } else {
        descr = state->StateDescription;
        StateDescription[0]=CGNS_NEW(char, strlen(descr->text)+1);
        strcpy(StateDescription[0], descr->text);
    }
    return CG_OK;
}

int cg_equationset_read(int *EquationDimension,
    int *GoverningEquationsFlag, int *GasModelFlag,
    int *ViscosityModelFlag,     int *ThermalConductivityModelFlag,
    int *TurbulenceClosureFlag,  int *TurbulenceModelFlag) {
    cgns_equations *eq;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    eq = cgi_equations_address(CG_MODE_READ, &ier);
    if (eq==0) return ier;

    (*EquationDimension) = eq->equation_dim;
    if (eq->governing) (*GoverningEquationsFlag)=1;
    else           (*GoverningEquationsFlag)=0;

    if (eq->gas) (*GasModelFlag)=1;
    else         (*GasModelFlag)=0;

    if (eq->visc) (*ViscosityModelFlag)=1;
    else          (*ViscosityModelFlag)=0;

    if (eq->conduct) (*ThermalConductivityModelFlag)=1;
    else         (*ThermalConductivityModelFlag)=0;

    if (eq->closure) (*TurbulenceClosureFlag)=1;
    else         (*TurbulenceClosureFlag)=0;

    if (eq->turbulence) (*TurbulenceModelFlag)=1;
    else            (*TurbulenceModelFlag)=0;

    /* Version 2.1 chemistry extensions get their own read routine
    ** for backward compatability.
    */
    return CG_OK;
}

int cg_equationset_chemistry_read(int *ThermalRelaxationFlag,
    int *ChemicalKineticsFlag) {
    cgns_equations *eq;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    eq = cgi_equations_address(CG_MODE_READ, &ier);
    if (eq==0) return ier;

    if (eq->relaxation) (*ThermalRelaxationFlag)=1;
    else            (*ThermalRelaxationFlag)=0;

    if (eq->chemkin) (*ChemicalKineticsFlag)=1;
    else         (*ChemicalKineticsFlag)=0;

    return CG_OK;
}

int cg_equationset_elecmagn_read(int *ElecFldModelFlag, int *MagnFldModelFlag,
				 int *ConductivityModelFlag)
{
    cgns_equations *eq;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    eq = cgi_equations_address(CG_MODE_READ, &ier);
    if (eq==0) return ier;

    if (eq->elecfield) (*ElecFldModelFlag)=1;
    else            (*ElecFldModelFlag)=0;

    if (eq->magnfield) (*MagnFldModelFlag)=1;
    else         (*MagnFldModelFlag)=0;

    if (eq->emconduct) (*ConductivityModelFlag)=1;
    else         (*ConductivityModelFlag)=0;

    return CG_OK;
}

int cg_governing_read(GoverningEquationsType_t *EquationsType) {
    cgns_governing *governing;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    governing = cgi_governing_address(CG_MODE_READ, &ier);
    if (governing==0) return ier;

    (*EquationsType) = governing->type;
    return CG_OK;
}

int cg_diffusion_read(int *diffusion_model) {
    int n, ndata, ier=0;
    int *diffusion, index_dim;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    diffusion = cgi_diffusion_address(CG_MODE_READ, &ier);
    if (diffusion==0) return ier;

    if (posit_base && posit_zone) {
        index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;

     /* If defined under CGNSBase_t node */
    } else if (posit_base) {
        index_dim = cg->base[posit_base-1].cell_dim;

    } else {
        cgi_error("Can't find IndexDimension in cg_diffusion_read.");
        return CG_NO_INDEX_DIM;
    }
    if (index_dim==1) ndata=1;
    else if (index_dim==2) ndata=3;
    else if (index_dim==3) ndata=6;
    else {
        cgi_error("invalid value for IndexDimension");
        return CG_ERROR;
    }

    for (n=0; n<ndata; n++) diffusion_model[n] = diffusion[n];

    return CG_OK;
}

int cg_model_read(char const *ModelLabel, ModelType_t *ModelType) {
    cgns_model *model;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    model = cgi_model_address(CG_MODE_READ, ModelLabel, &ier);
    if (model==0) return ier;

    (*ModelType) = model->type;

    return CG_OK;
}

int cg_narrays(int *narrays) {

/* Possible parents:
 *  GridCoordinates_t, FlowSolution_t, DiscreteData_t, BC_t,
 *  BCData_t, GasModel_t, ViscosityModel_t, ThermalConductivityModel_t, TurbulenceClosure_t,
 *  TurbulenceModel_t, ThermalRelaxationModel_t, ChemicalKineticsModel_t,
 *  EMElectricFieldModel_t, EMMagneticFieldModel_t, EMConductivityModel_t,
 *  ConvergenceHistory_t, IntegralData_t, ReferenceState_t,
 *  RigidGridMotion_t, ArbitraryGridMotion_t, BaseIterativeData_t, ZoneIterativeData_t
 *  GridConnectivity_t, UserDefinedData_t, Gravity_t, Axisymmetry_t, RotatingCoordinates_t,
 *  Area_t, Periodic_t
 */

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* check for valid posit */
    if (posit == 0) {
        cgi_error("No current position set by cg_goto\n");
        (*narrays) = 0;
        return CG_ERROR;
    }
    if (strcmp(posit->label,"GridCoordinates_t")==0) {
        cgns_zcoor *zcoor= (cgns_zcoor *)posit->posit;
        (*narrays) = zcoor->ncoords;

    } else if (strcmp(posit->label,"FlowSolution_t")==0) {
        cgns_sol *sol = (cgns_sol *)posit->posit;
        (*narrays) = sol->nfields;

    } else if (strcmp(posit->label,"DiscreteData_t")==0) {
        cgns_discrete *discrete = (cgns_discrete *)posit->posit;
        (*narrays) = discrete->narrays;

    } else if (strcmp(posit->label,"GridConnectivity_t")==0) {
        cgns_conn *conn = (cgns_conn *)posit->posit;
        (*narrays) = conn->narrays;

    } else if (strcmp(posit->label,"BC_t")==0) {
        /*cgns_boco *boco = (cgns_boco *)posit->posit;*/
        (*narrays) = 1;  /* Always supports exactly 1. */

    } else if (strcmp(posit->label,"BCData_t")==0) {
        cgns_bcdata *bcdata = (cgns_bcdata *)posit->posit;
        (*narrays) = bcdata->narrays;

    } else if (strcmp(posit->label,"GasModel_t")==0 ||
        strcmp(posit->label,"ViscosityModel_t")==0 ||
        strcmp(posit->label,"ThermalConductivityModel_t")==0 ||
        strcmp(posit->label,"TurbulenceModel_t")==0 ||
        strcmp(posit->label,"TurbulenceClosure_t")==0 ||
        strcmp(posit->label,"ThermalRelaxationModel_t")==0 ||
        strcmp(posit->label,"ChemicalKineticsModel_t")==0 ||
	strcmp(posit->label,"EMElectricFieldModel_t")==0 ||
	strcmp(posit->label,"EMMagneticFieldModel_t")==0 ||
	strcmp(posit->label,"EMConductivityModel_t")==0) {
        cgns_model *model = (cgns_model *)posit->posit;
        (*narrays) = model->narrays;

    }  else if (strcmp(posit->label,"ConvergenceHistory_t")==0) {
        cgns_converg *converg = (cgns_converg *)posit->posit;
        (*narrays) = converg->narrays;

    } else if (strcmp(posit->label,"IntegralData_t")==0) {
        cgns_integral *integral = (cgns_integral *)posit->posit;
        (*narrays) = integral->narrays;

    } else if (strcmp(posit->label,"ReferenceState_t")==0) {
        cgns_state *state = (cgns_state *)posit->posit;
        (*narrays) = state->narrays;

    } else if (strcmp(posit->label,"RigidGridMotion_t")==0) {
        cgns_rmotion *rmotion = (cgns_rmotion *)posit->posit;
        (*narrays) = rmotion->narrays;

    } else if (strcmp(posit->label,"ArbitraryGridMotion_t")==0) {
        cgns_amotion *amotion = (cgns_amotion *)posit->posit;
        (*narrays) = amotion->narrays;

    } else if (strcmp(posit->label,"BaseIterativeData_t")==0) {
        cgns_biter *biter = (cgns_biter *)posit->posit;
        (*narrays) = biter->narrays;

    } else if (strcmp(posit->label,"ZoneIterativeData_t")==0) {
        cgns_ziter *ziter = (cgns_ziter *)posit->posit;
        (*narrays) = ziter->narrays;

    } else if (strcmp(posit->label,"UserDefinedData_t")==0) {
        cgns_user_data *user_data = (cgns_user_data *)posit->posit;
        (*narrays) = user_data->narrays;

    } else if (strcmp(posit->label,"Gravity_t")==0) {
        cgns_gravity *gravity = (cgns_gravity *)posit->posit;
        (*narrays) = gravity->narrays;

    } else if (strcmp(posit->label,"Axisymmetry_t")==0) {
        cgns_axisym *axisym = (cgns_axisym *)posit->posit;
        (*narrays) = axisym->narrays;

    } else if (strcmp(posit->label,"RotatingCoordinates_t")==0) {
        cgns_rotating *rotating = (cgns_rotating *)posit->posit;
        (*narrays) = rotating->narrays;

    } else if (strcmp(posit->label,"Area_t")==0) {
        cgns_bcarea *bcarea = (cgns_bcarea *)posit->posit;
        (*narrays) = bcarea->narrays;

    } else if (strcmp(posit->label,"Periodic_t")==0) {
        cgns_cperio *cperio = (cgns_cperio *)posit->posit;
        (*narrays) = cperio->narrays;

    } else {
        cgi_error("User defined DataArray_t node not supported under '%s' type node",posit->label);
        (*narrays) = 0;
        return CG_INCORRECT_PATH;
    }
    return CG_OK;
}

int cg_array_info(int A, char *ArrayName, DataType_t *DataType,
    int *DataDimension, int *DimensionVector) {

    cgns_array *array;
    int n, ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    array = cgi_array_address(CG_MODE_READ, A, "dummy", &ier);
    if (array==0) return ier;

    strcpy(ArrayName, array->name);
    (*DataType) = cgi_datatype(array->data_type);
    (*DataDimension) = array->data_dim;
    for (n=0; n<array->data_dim; n++) DimensionVector[n] = array->dim_vals[n];

    return CG_OK;
}

int cg_array_read(int A, void *Data) {
    cgns_array *array;
    int n, num =1, ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    array = cgi_array_address(CG_MODE_READ, A, "dummy", &ier);
    if (array==0) return ier;

    for (n=0; n<array->data_dim; n++) num *= array->dim_vals[n];

    if (array->data)
        memcpy(Data, array->data, num*size_of(array->data_type));
    else {
        if (cgio_read_all_data(cg->cgio, array->id, Data)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
    }

    return CG_OK;
}

int cg_array_read_as(int A, DataType_t type, void *Data) {
    cgns_array *array;
    int n, num =1, ier=0;
    void *array_data;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    array = cgi_array_address(CG_MODE_READ, A, "dummy", &ier);
    if (array==0) return ier;

    for (n=0; n<array->data_dim; n++) num *= array->dim_vals[n];

     /* Special for Character arrays */
    if ((type==Character && cgi_datatype(array->data_type)!=Character) ||
        (type!=Character && cgi_datatype(array->data_type)==Character)) {
        cgi_error("Error exit:  Character array can only be read as character");
        return CG_ERROR;
    }
    if (type==Character) {
        if (array->data)
            memcpy(Data, array->data, num*size_of(array->data_type));
        else {
            if (cgio_read_all_data(cg->cgio, array->id, Data)) {
                cg_io_error("cgio_read_all_data");
                return CG_ERROR;
            }
        }
        return CG_OK;
    }

     /* All numerical data types: */
    if (array->data)
        array_data = array->data;
    else {
        array_data = (void *)malloc(num*size_of(array->data_type));
        if (array_data == NULL) {
            cgi_error("Error allocating array_data");
            return CG_ERROR;
        }
        if (cgio_read_all_data(cg->cgio, array->id, array_data)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
    }

     /* Write "Data" using the data-type requested */
     /* I4 - I4 */
    if (type==Integer && cgi_datatype(array->data_type)==Integer) {
        for (n=0; n<num; n++) *((int *) Data+n) = *((int *) array_data+n);

     /* I4 - R4 */
    } else if (type==Integer && cgi_datatype(array->data_type)==RealSingle) {
        for (n=0; n<num; n++) *((int *) Data+n) = (int)(*((float *) array_data+n));

     /* I4 - R8 */
    } else if (type==Integer && cgi_datatype(array->data_type)==RealDouble) {
        for (n=0; n<num; n++) *((int *) Data+n) = (int)(*((double *) array_data+n));

     /* R4 - I4 */
    } else if (type==RealSingle && cgi_datatype(array->data_type)==Integer) {
        for (n=0; n<num; n++) *((float *) Data+n) = (float)(*((int *) array_data+n));

     /* R4 - R4 */
    } else if (type==RealSingle && cgi_datatype(array->data_type)==RealSingle) {
        for (n=0; n<num; n++) *((float *) Data+n) = *((float *) array_data+n);

     /* R4 - R8 */
    } else if (type==RealSingle && cgi_datatype(array->data_type)==RealDouble) {
        for (n=0; n<num; n++) *((float *) Data+n) = (float)(*((double *) array_data+n));

     /* R8 - I4 */
    } else if (type==RealDouble && cgi_datatype(array->data_type)==Integer) {
        for (n=0; n<num; n++) *((double *) Data+n) = (double)(*((int *) array_data+n));

     /* R8 - R4 */
    } else if (type==RealDouble && cgi_datatype(array->data_type)==RealSingle) {
        for (n=0; n<num; n++) *((double *) Data+n) = (double)(*((float *) array_data+n));

     /* R8 - R8 */
    } else if (type==RealDouble && cgi_datatype(array->data_type)==RealDouble) {
        for (n=0; n<num; n++) *((double *) Data+n) = *((double *) array_data+n);
    }

    if (array_data != array->data) free(array_data);

    return CG_OK;
}

int cg_nintegrals(int *nintegrals) {

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* check for valid posit */
    if (posit == 0) {
        cgi_error("No current position set by cg_goto\n");
        (*nintegrals) = 0;
        return CG_ERROR;
    }

    if (strcmp(posit->label,"CGNSBase_t")==0) {
        cgns_base *base= (cgns_base *)posit->posit;
        (*nintegrals) = base->nintegrals;

    } else if (strcmp(posit->label,"Zone_t")==0) {
        cgns_zone *zone = (cgns_zone *)posit->posit;
        (*nintegrals) = zone->nintegrals;
    } else {
        cgi_error("IntegralData_t node not supported under '%s' type node",posit->label);
        (*nintegrals) = 0;
        return CG_INCORRECT_PATH;
    }
    return CG_OK;
}

int cg_integral_read(int IntegralDataIndex, char *IntegralDataName) {
    int ier=0;
    cgns_integral *integral;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    integral = cgi_integral_address(CG_MODE_READ, IntegralDataIndex,
           "dummy", &ier);
    if (integral==0) return ier;

    strcpy(IntegralDataName, integral->name);
    return CG_OK;
}

int cg_rind_read(int *RindData) {
    int n, ier=0;
    int *rind, index_dim;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    rind = cgi_rind_address(CG_MODE_READ, &ier);
    if (rind==0) return ier;

    if (posit_base && posit_zone) {
        index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;
    } else {
        cgi_error("Can't find IndexDimension in cg_rind_read.");
        return CG_NO_INDEX_DIM;
    }

    for (n=0; n<2*index_dim; n++) RindData[n] = rind[n];
    return CG_OK;
}

int cg_ndescriptors(int *ndescriptors) {

/* Possible parents of Descriptor_t node:
 *  CGNSBase_t, Zone_t, GridCoordinates_t, Elements_t, FlowSolution_t,
 *  DiscreteData_t, ZoneGridConnectivity_t, GridConnectivity1to1_t,
 *  GridConnectivity_t, OversetHoles_t, ZoneBC_t, BC_t, BCDataSet_t,
 *  BCData_t, FlowEquationSet_t, GoverningEquations_t, GasModel_t,
 *  ViscosityModel_t, ThermalConductivityModel_t, TurbulenceClosure_t,
 *  TurbulenceModel_t, ThermalRelaxationModel_t, ChemicalKineticsModel_t,
 *  EMElectricFieldModel_t, EMMagneticFieldModel_t, EMConductivityModel_t,
 *  ConvergenceHistory_t, IntegralData_t, ReferenceState_t,
 *  DataArray_t, Family_t, GeometryReference_t, RigidGridMotion_t,
 *  ArbitraryGridMotion_t, BaseIterativeData_t, ZoneIterativeData_t,
 *  UserDefinedData_t, Gravity_t, Axisymmetry_t, RotatingCoordinates_t,
 *  BCProperty_t, WallFunction_t, Area_t,
 *  GridConnectivityProperty_t, Periodic_t, AverageInterface_t
 */

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* check for valid posit */
    if (posit == 0) {
        cgi_error("No current position set by cg_goto\n");
        (*ndescriptors)=0;
        return CG_ERROR;
    }

    if (strcmp(posit->label,"CGNSBase_t")==0)
        NDESCRIPTOR(cgns_base)
    else if (strcmp(posit->label,"Zone_t")==0)
        NDESCRIPTOR(cgns_zone)
    else if (strcmp(posit->label,"GridCoordinates_t")==0)
        NDESCRIPTOR(cgns_zcoor)
    else if (strcmp(posit->label,"Elements_t")==0)
        NDESCRIPTOR(cgns_section)
    else if (strcmp(posit->label,"FlowSolution_t")==0)
        NDESCRIPTOR(cgns_sol)
    else if (strcmp(posit->label,"DiscreteData_t")==0)
        NDESCRIPTOR(cgns_discrete)
    else if (strcmp(posit->label,"ZoneGridConnectivity_t")==0)
        NDESCRIPTOR(cgns_zconn)
    else if (strcmp(posit->label,"GridConnectivity1to1_t")==0)
        NDESCRIPTOR(cgns_1to1)
    else if (strcmp(posit->label,"GridConnectivity_t")==0)
        NDESCRIPTOR(cgns_conn)
    else if (strcmp(posit->label,"OversetHoles_t")==0)
        NDESCRIPTOR(cgns_hole)
    else if (strcmp(posit->label,"ZoneBC_t")==0)
        NDESCRIPTOR(cgns_zboco)
    else if (strcmp(posit->label,"BC_t")==0)
        NDESCRIPTOR(cgns_boco)
    else if (strcmp(posit->label,"BCDataSet_t")==0)
        NDESCRIPTOR(cgns_dataset)
    else if (strcmp(posit->label,"BCData_t")==0)
        NDESCRIPTOR(cgns_bcdata)
    else if (strcmp(posit->label,"FlowEquationSet_t")==0)
        NDESCRIPTOR(cgns_equations)
    else if (strcmp(posit->label,"GoverningEquations_t")==0)
        NDESCRIPTOR(cgns_governing)
    else if (strcmp(posit->label,"GasModel_t")==0 ||
         strcmp(posit->label,"ViscosityModel_t")==0 ||
         strcmp(posit->label,"ThermalConductivityModel_t")==0 ||
         strcmp(posit->label,"TurbulenceModel_t")==0 ||
         strcmp(posit->label,"TurbulenceClosure_t")==0 ||
         strcmp(posit->label,"ThermalRelaxationModel_t")==0 ||
         strcmp(posit->label,"ChemicalKineticsModel_t")==0 ||
	 strcmp(posit->label,"EMElectricFieldModel_t")==0 ||
	 strcmp(posit->label,"EMMagneticFieldModel_t")==0 ||
	 strcmp(posit->label,"EMConductivityModel_t")==0)
        NDESCRIPTOR(cgns_model)
    else if (strcmp(posit->label,"ConvergenceHistory_t")==0)
        NDESCRIPTOR(cgns_converg)
    else if (strcmp(posit->label,"IntegralData_t")==0)
        NDESCRIPTOR(cgns_integral)
    else if (strcmp(posit->label,"ReferenceState_t")==0)
        NDESCRIPTOR(cgns_state)
    else if (strcmp(posit->label,"DataArray_t")==0)
        NDESCRIPTOR(cgns_array)
    else if (strcmp(posit->label,"Family_t")==0)
        NDESCRIPTOR(cgns_family)
    else if (strcmp(posit->label,"GeometryReference_t")==0)
        NDESCRIPTOR(cgns_geo)
    else if (strcmp(posit->label,"RigidGridMotion_t")==0)
        NDESCRIPTOR(cgns_rmotion)
    else if (strcmp(posit->label,"ArbitraryGridMotion_t")==0)
        NDESCRIPTOR(cgns_amotion)
    else if (strcmp(posit->label,"BaseIterativeData_t")==0)
        NDESCRIPTOR(cgns_biter)
    else if (strcmp(posit->label,"ZoneIterativeData_t")==0)
        NDESCRIPTOR(cgns_ziter)
    else if (strcmp(posit->label,"UserDefinedData_t")==0)
        NDESCRIPTOR(cgns_user_data)
    else if (strcmp(posit->label,"Gravity_t")==0)
        NDESCRIPTOR(cgns_gravity)
    else if (strcmp(posit->label,"Axisymmetry_t")==0)
        NDESCRIPTOR(cgns_axisym)
    else if (strcmp(posit->label,"RotatingCoordinates_t")==0)
        NDESCRIPTOR(cgns_rotating)
    else if (strcmp(posit->label,"BCProperty_t")==0)
        NDESCRIPTOR(cgns_bprop)
    else if (strcmp(posit->label,"WallFunction_t")==0)
        NDESCRIPTOR(cgns_bcwall)
    else if (strcmp(posit->label,"Area_t")==0)
        NDESCRIPTOR(cgns_bcarea)
    else if (strcmp(posit->label,"GridConnectivityProperty_t")==0)
        NDESCRIPTOR(cgns_cprop)
    else if (strcmp(posit->label,"Periodic_t")==0)
        NDESCRIPTOR(cgns_cperio)
    else if (strcmp(posit->label,"AverageInterface_t")==0)
        NDESCRIPTOR(cgns_caverage)
    else {
        cgi_error("Descriptor_t node not supported under '%s' type node",posit->label);
        (*ndescriptors)=0;
        return CG_INCORRECT_PATH;
    }
    return CG_OK;
}

int cg_descriptor_read(int descr_no, char *descr_name, char **descr_text) {
    cgns_descr *descr;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

     /* common part */
    descr = cgi_descr_address(CG_MODE_READ, descr_no, "dummy", &ier);
    if (descr==0) return ier;

     /* return Descriptor text and name */
    descr_text[0]=CGNS_NEW(char, strlen(descr->text)+1);
    strcpy(descr_text[0], descr->text);
    strcpy(descr_name, descr->name);

    return CG_OK;
}

int cg_nunits(int *nunits) {
    cgns_units *units;
    int ier=0;

    *nunits = 0;
     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    units = cgi_units_address(CG_MODE_READ, &ier);
    if (units==0) return ier;
    *nunits = units->nunits;
    return CG_OK;
}

int cg_units_read(MassUnits_t *mass, LengthUnits_t *length, TimeUnits_t *time,
    TemperatureUnits_t *temperature, AngleUnits_t *angle) {

    cgns_units *units;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    units = cgi_units_address(CG_MODE_READ, &ier);
    if (units==0) return ier;

    (*mass) = units->mass;
    (*length) = units->length;
    (*time) = units->time;
    (*temperature) = units->temperature;
    (*angle) = units->angle;
    return CG_OK;
}

int cg_unitsfull_read(MassUnits_t *mass, LengthUnits_t *length,
    TimeUnits_t *time, TemperatureUnits_t *temperature, AngleUnits_t *angle,
    ElectricCurrentUnits_t *current, SubstanceAmountUnits_t *amount,
    LuminousIntensityUnits_t *intensity) {

    cgns_units *units;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    units = cgi_units_address(CG_MODE_READ, &ier);
    if (units==0) return ier;

    (*mass) = units->mass;
    (*length) = units->length;
    (*time) = units->time;
    (*temperature) = units->temperature;
    (*angle) = units->angle;
    (*current) = units->current;
    (*amount) = units->amount;
    (*intensity) = units->intensity;
    return CG_OK;
}

int cg_exponents_info(DataType_t *DataType) {
    cgns_exponent *exponent;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    exponent = cgi_exponent_address(CG_MODE_READ, &ier);
    if (exponent==0) return ier;

    (*DataType) = cgi_datatype(exponent->data_type);
    return CG_OK;
}

int cg_nexponents(int *numexp) {
    cgns_exponent *exponent;
    int ier=0;

    *numexp = 0;
     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    exponent = cgi_exponent_address(CG_MODE_READ, &ier);
    if (exponent==0) return ier;
    *numexp = exponent->nexps;
    return CG_OK;
}

int cg_exponents_read(void *exponents) {
    cgns_exponent *exponent;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    exponent = cgi_exponent_address(CG_MODE_READ, &ier);
    if (exponent==0) return ier;

    if (cgi_datatype(exponent->data_type)==RealSingle) {
        (*((float *)exponents+0)) = (*((float *) exponent->data+0));
        (*((float *)exponents+1)) = (*((float *) exponent->data+1));
        (*((float *)exponents+2)) = (*((float *) exponent->data+2));
        (*((float *)exponents+3)) = (*((float *) exponent->data+3));
        (*((float *)exponents+4)) = (*((float *) exponent->data+4));

    } else if (cgi_datatype(exponent->data_type)==RealDouble) {
        (*((double *)exponents+0)) = (*((double *) exponent->data+0));
        (*((double *)exponents+1)) = (*((double *) exponent->data+1));
        (*((double *)exponents+2)) = (*((double *) exponent->data+2));
        (*((double *)exponents+3)) = (*((double *) exponent->data+3));
        (*((double *)exponents+4)) = (*((double *) exponent->data+4));
    }
    return CG_OK;
}

int cg_expfull_read(void *exponents)
{
    cgns_exponent *exponent;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    exponent = cgi_exponent_address(CG_MODE_READ, &ier);
    if (exponent==0) return ier;

    /* don't fail - just return 0's for last 3
    if(exponent->nexps != 8)
    {
	cgi_error("Full set of exponents not written, use cg_exponents_read.");
        return 1;
    }*/

    if (cgi_datatype(exponent->data_type)==RealSingle) {
        (*((float *)exponents+0)) = (*((float *) exponent->data+0));
        (*((float *)exponents+1)) = (*((float *) exponent->data+1));
        (*((float *)exponents+2)) = (*((float *) exponent->data+2));
        (*((float *)exponents+3)) = (*((float *) exponent->data+3));
        (*((float *)exponents+4)) = (*((float *) exponent->data+4));
        if (exponent->nexps == 8) {
            (*((float *)exponents+5)) = (*((float *) exponent->data+5));
            (*((float *)exponents+6)) = (*((float *) exponent->data+6));
            (*((float *)exponents+7)) = (*((float *) exponent->data+7));
        }
        else {
            (*((float *)exponents+5)) = (float)0.0;
            (*((float *)exponents+6)) = (float)0.0;;
            (*((float *)exponents+7)) = (float)0.0;;
        }

    } else if (cgi_datatype(exponent->data_type)==RealDouble) {
        (*((double *)exponents+0)) = (*((double *) exponent->data+0));
        (*((double *)exponents+1)) = (*((double *) exponent->data+1));
        (*((double *)exponents+2)) = (*((double *) exponent->data+2));
        (*((double *)exponents+3)) = (*((double *) exponent->data+3));
        (*((double *)exponents+4)) = (*((double *) exponent->data+4));
        if (exponent->nexps == 8) {
            (*((double *)exponents+5)) = (*((double *) exponent->data+5));
            (*((double *)exponents+6)) = (*((double *) exponent->data+6));
            (*((double *)exponents+7)) = (*((double *) exponent->data+7));
        }
        else {
            (*((double *)exponents+5)) = (double)0.0;
            (*((double *)exponents+6)) = (double)0.0;;
            (*((double *)exponents+7)) = (double)0.0;;
        }
    }
    return CG_OK;
}

int cg_conversion_info(DataType_t *DataType) {
    cgns_conversion *conversion;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    conversion = cgi_conversion_address(CG_MODE_READ, &ier);
    if (conversion==0) return ier;

    (*DataType) = cgi_datatype(conversion->data_type);
    return CG_OK;
}

int cg_conversion_read(void *ConversionFactors) {
    cgns_conversion *conversion;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    conversion = cgi_conversion_address(CG_MODE_READ, &ier);
    if (conversion==0) return ier;

    if (cgi_datatype(conversion->data_type)==RealSingle) {
        *((float *)ConversionFactors+0) = *((float *) conversion->data+0);
        *((float *)ConversionFactors+1) = *((float *) conversion->data+1);

    } else if (cgi_datatype(conversion->data_type)==RealDouble) {
        *((double *)ConversionFactors+0) = *((double *) conversion->data+0);
        *((double *)ConversionFactors+1) = *((double *) conversion->data+1);
    }
    return CG_OK;
}

int cg_dataclass_read(DataClass_t *dataclass) {
    DataClass_t *DataClass;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    DataClass = cgi_dataclass_address(CG_MODE_READ, &ier);
    if (DataClass==0) return ier;

    if (*DataClass==DataClassNull) return CG_NODE_NOT_FOUND;
    (*dataclass) = (*DataClass);
    return CG_OK;
}

int cg_gridlocation_read(GridLocation_t *GridLocation) {
    GridLocation_t *location;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    location = cgi_location_address(CG_MODE_READ, &ier);
    if (location==0) return ier;

    (*GridLocation) = (*location);
    return CG_OK;
}

int cg_ordinal_read(int *Ordinal) {
    int *ordinal;
    int ier=0;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    ordinal = cgi_ordinal_address(CG_MODE_READ, &ier);
    if (ordinal==0) return ier;

    (*Ordinal) = (*ordinal);
    return CG_OK;
}

int cg_is_link(int *path_length) {
    double posit_id;

    *path_length = 0;

    /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ))
        return CG_ERROR;

    if (cgi_posit_id(&posit_id)) return CG_ERROR;

    if (cgio_is_link(cg->cgio, posit_id, path_length)) {
        cg_io_error("cgio_is_link");
        return CG_ERROR;
    }

    return CG_OK;
}

int cg_link_read(char **filename, char **link_path) {
    int name_len, file_len;
    double posit_id;

    /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    if (cgi_posit_id(&posit_id)) return CG_ERROR;

    if (cgio_link_size(cg->cgio, posit_id, &file_len, &name_len)) {
        cg_io_error("cgio_link_size");
        return CG_ERROR;
    }
    *filename = CGNS_NEW(char, file_len + 1);
    *link_path = CGNS_NEW(char, name_len + 1);

    if (cgio_get_link(cg->cgio, posit_id, *link_path, *filename)) {
        CGNS_FREE(*filename);
        CGNS_FREE(*link_path);
        *filename = *link_path = 0;
        cg_io_error("cgio_get_link");
        return CG_ERROR;
    }
    return CG_OK;
}

int cg_nuser_data(int *nuser_data) {

/* Possible parents of UserDefinedData_t node:
 *  IntegralData_t, DiscreteData_t, ConvergenceHistory_t, ReferenceState_t,
 *  xxxModel_t, GoverningEquations_t, FlowEquationSet_t, BCData_t, BCDataSet_t,
 *  Elements_t, BC_t, ZoneBC_t, OversetHoles_t, GridConnectivity_t,
 *  GridConnectivity1to1_t, ZoneGridConnectivity_t, FlowSolution_t,
 *  GridCoordinates_t, RigidGridMotion_t, ArbitraryGridMotion_t,
 *  ZoneIterativeData_t, BaseIterativeData_t, Zone_t, GeometryReference_t,
 *  Family_t, CGNSBase_t, Gravity_t, Axisymmetry_t, RotatingCoordinates_t,
 *  BCProperty_t, WallFunction_t, Area_t, UserDefinedData_t,
 *  GridConnectivityProperty_t, Periodic_t, AverageInterface_t
 */

     /* This is valid and used during write as well as read mode. */

     /* check for valid posit */
    if (posit == 0) {
        cgi_error("No current position set by cg_goto\n");
        (*nuser_data) = 0;
        return CG_ERROR;
    }

    if (strcmp(posit->label,"IntegralData_t")==0)
        (*nuser_data) = ((cgns_integral *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"DiscreteData_t")==0)
        (*nuser_data) = ((cgns_discrete *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"ConvergenceHistory_t")==0)
        (*nuser_data) = ((cgns_converg *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"ReferenceState_t")==0)
        (*nuser_data) = ((cgns_state *)posit->posit)->nuser_data;
    else if ( (strcmp(posit->label,"GasModel_t")==0 ||
        strcmp(posit->label,"ViscosityModel_t")==0 ||
        strcmp(posit->label,"ThermalConductivityModel_t")==0 ||
        strcmp(posit->label,"TurbulenceModel_t")==0 ||
        strcmp(posit->label,"TurbulenceClosure_t")==0 ||
        strcmp(posit->label,"ThermalRelaxationModel_t")==0 ||
        strcmp(posit->label,"ChemicalKineticsModel_t")==0 ||
	 strcmp(posit->label,"EMElectricFieldModel_t")==0 ||
	 strcmp(posit->label,"EMMagneticFieldModel_t")==0 ||
	 strcmp(posit->label,"EMConductivityModel_t")==0) )
        (*nuser_data) = ((cgns_model *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"GoverningEquations_t")==0)
        (*nuser_data) = ((cgns_governing *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"FlowEquationSet_t")==0)
        (*nuser_data) = ((cgns_equations *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"BCData_t")==0)
        (*nuser_data) = ((cgns_bcdata *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"BCDataSet_t")==0)
        (*nuser_data) = ((cgns_dataset *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"Elements_t")==0)
        (*nuser_data) = ((cgns_section *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"BC_t")==0)
        (*nuser_data) = ((cgns_boco *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"ZoneBC_t")==0)
        (*nuser_data) = ((cgns_zboco *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"OversetHoles_t")==0)
        (*nuser_data) = ((cgns_hole *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"GridConnectivity_t")==0)
        (*nuser_data) = ((cgns_conn *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"GridConnectivity1to1_t")==0)
        (*nuser_data) = ((cgns_1to1 *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"ZoneGridConnectivity_t")==0)
        (*nuser_data) = ((cgns_zconn *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"FlowSolution_t")==0)
        (*nuser_data) = ((cgns_sol *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"GridCoordinates_t")==0)
        (*nuser_data) = ((cgns_zcoor *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"RigidGridMotion_t")==0)
        (*nuser_data) = ((cgns_rmotion *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"ArbitraryGridMotion_t")==0)
        (*nuser_data) = ((cgns_amotion *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"ZoneIterativeData_t")==0)
        (*nuser_data) = ((cgns_ziter *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"BaseIterativeData_t")==0)
        (*nuser_data) = ((cgns_biter *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"Zone_t")==0)
        (*nuser_data) = ((cgns_zone *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"GeometryReference_t")==0)
        (*nuser_data) = ((cgns_geo *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"Family_t")==0)
        (*nuser_data) = ((cgns_family *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"CGNSBase_t")==0)
        (*nuser_data) = ((cgns_base *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"Gravity_t")==0)
        (*nuser_data) = ((cgns_gravity *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"Axisymmetry_t")==0)
        (*nuser_data) = ((cgns_axisym *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"RotatingCoordinates_t")==0)
        (*nuser_data) = ((cgns_rotating *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"BCProperty_t")==0)
         (*nuser_data) = ((cgns_bprop *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"WallFunction_t")==0)
         (*nuser_data) = ((cgns_bcwall *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"Area_t")==0)
         (*nuser_data) = ((cgns_bcarea *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"UserDefinedData_t")==0)
         (*nuser_data) = ((cgns_user_data *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"GridConnectivityProperty_t")==0)
         (*nuser_data) = ((cgns_cprop *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"Periodic_t")==0)
         (*nuser_data) = ((cgns_cperio *)posit->posit)->nuser_data;
    else if (strcmp(posit->label,"AverageInterface_t")==0)
         (*nuser_data) = ((cgns_caverage *)posit->posit)->nuser_data;

    else {
        cgi_error("UserDefinedData_t node not supported under '%s' type node",posit->label);
        (*nuser_data) = 0;
        return CG_INCORRECT_PATH;
    }
    return CG_OK;
}

int cg_user_data_read(int Index, char *UserDataName) {
    int ier=0;
    cgns_user_data *user_data;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    user_data = cgi_user_data_address(CG_MODE_READ, Index,
           "dummy", &ier);
    if (user_data==0) return ier;

    strcpy(UserDataName, user_data->name);
    return CG_OK;
}

int cg_rotating_read(float *rot_rate, float *rot_center) {
    cgns_rotating *rotating;
    cgns_base *base;
    int ier=0, n;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    rotating = cgi_rotating_address(CG_MODE_READ, &ier);
    if (rotating==0) return ier;

    if (posit_base) {
        base = &cg->base[posit_base-1];
    } else {
        cgi_error("Can't find the base");
        return CG_ERROR;
    }

    for (n=0; n<rotating->narrays; n++) {
        if (strcmp(rotating->array[n].name,"RotationCenter")==0)
            memcpy(rot_center, rotating->array[n].data, base->phys_dim*sizeof(float));
        else if (strcmp(rotating->array[n].name,"RotationRateVector")==0)
            memcpy(rot_rate, rotating->array[n].data, base->phys_dim*sizeof(float));
    }
    return CG_OK;
}

int cg_ptset_info(PointSetType_t *ptset_type, int *npnts)
{
    cgns_ptset *ptset;
    int ier=0;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ))
	return CG_ERROR;

    ptset = cgi_ptset_address(CG_MODE_READ, &ier);
    if (ptset == 0)
	return ier;

    *ptset_type = ptset->type;
    *npnts = ptset->npts;

    return CG_OK;
}

int cg_ptset_read(int *pnts)
{
    cgns_ptset *ptset;
    int ier=0;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    ptset = cgi_ptset_address(CG_MODE_READ, &ier);
    if (ptset == 0)
	return ier;

    /* Read point-set directly from ADF-file */
    if (ptset->npts > 0) {
        if (cgio_read_all_data(cg->cgio, ptset->id, (void *)pnts)) {
            cg_io_error("cgio_read_all_data");
            return CG_ERROR;
        }
/*
    } else
    {
        printf("Warning: No points for current point set.");
*/
    }

    return CG_OK;
}

/*****************************************************************************\
 *           Write Functions
\*****************************************************************************/
int cg_base_write(int file_number, char const * basename, int cell_dim, int phys_dim, int *B) {
    cgns_base *base = NULL;
    int index;
    int dim_vals;
    int data[2];

     /* verify input */
    if (cgi_check_strlen(basename)) return CG_ERROR;
    if (cell_dim<1 || cell_dim>3 || phys_dim<1 || phys_dim>3) {
        cgi_error("Invalid input:  cell_dim=%d, phys_dim=%d",cell_dim,phys_dim);
        return CG_ERROR;
    }
     /* get memory address for base */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* Overwrite a CGNSBase_t Node: */
    for (index=0; index<cg->nbases; index++) {
        if (strcmp(basename, cg->base[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",basename);
                return CG_ERROR;
            }

             /* overwrite an existing base */
             /* delete the existing base from file */
            if (cgi_delete_node(cg->rootid, cg->base[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            base = &(cg->base[index]);
             /* free memory */
            cgi_free_base(base);
            break;
        }
    }
     /* ... or add a CGNSBase_t Node: */
    if (index==cg->nbases) {
        if (cg->nbases == 0) {
            cg->base = CGNS_NEW(cgns_base, cg->nbases+1);
        } else {
            cg->base = CGNS_RENEW(cgns_base, cg->nbases+1, cg->base);
        }
        base = &(cg->base[cg->nbases]);
        cg->nbases ++;
    }
    (*B) = index+1;

     /* save data in memory and initialize base data structure */
    strcpy(base->name, basename);
    base->cell_dim = cell_dim;
    base->phys_dim = phys_dim;
    base->id=0;
    base->nzones=0;
    base->ndescr=0;
    base->nfamilies=0;
    base->state=0;
    base->data_class=DataClassNull;
    base->units=0;
    base->equations=0;
    base->converg=0;
    base->nintegrals=0;
    base->biter=0;
    base->type=SimulationTypeNull;
    base->type_id=0;
    base->nuser_data=0;
    base->gravity=0;
    base->axisym=0;
    base->rotating=0;

     /* save data in file */
    data[0] = cell_dim;
    data[1] = phys_dim;
    dim_vals=2;
    if (cgi_new_node(cg->rootid, base->name, "CGNSBase_t", &base->id,
        "I4", 1, &dim_vals, (void *)data)) return CG_ERROR;

    return CG_OK;
}

int cg_zone_write(int file_number, int B, char const *zonename, int const * nijk,
    ZoneType_t type, int *Z) {
    cgns_base *base;
    cgns_zone *zone = NULL;
    int index, i, index_dim;
    int dim_vals[2];
    double dummy_id;

     /* verify input */
    if (cgi_check_strlen(zonename)) return CG_ERROR;

     /* get memory address file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for base */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* Set index dimension */
    if (type == Structured)
        index_dim = base->cell_dim;
    else if (type == Unstructured)
        index_dim = 1;
    else {
        cgi_error("Invalid zone type - not Structured or Unstructured");
        return CG_ERROR;
    }

    for (i=0; i<index_dim; i++) {
        if (nijk[i]<=0) {
            cgi_error("Invalid input:  nijk[%d]=%d", i, nijk[i]);
            return CG_ERROR;
        }
        if (type == Structured && nijk[i]!=nijk[i+index_dim]+1) {
            cgi_error("Invalid input:  VertexSize[%d]=%d and CellSize[%d]=%d",
                   i, nijk[i], i, nijk[i+index_dim]);
            return CG_ERROR;
        }
    }

     /* Overwrite a Zone_t Node: */
    for (index=0; index<base->nzones; index++) {
        if (strcmp(zonename, base->zone[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",zonename);
                return CG_ERROR;
            }

             /* overwrite an existing zone */
             /* delete the existing zone from file */
            if (cgi_delete_node(base->id, base->zone[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            zone = &(base->zone[index]);
             /* free memory */
            cgi_free_zone(zone);
            break;
        }
    }
     /* ... or add a Zone_t Node: */
    if (index==base->nzones) {
        if (base->nzones == 0) {
            base->zone = CGNS_NEW(cgns_zone, base->nzones+1);
        } else {
            base->zone = CGNS_RENEW(cgns_zone, base->nzones+1, base->zone);
        }
        zone = &(base->zone[base->nzones]);
        base->nzones++;
    }
    (*Z) = index+1;

     /* save data to zone */
    strcpy(zone->name,zonename);
    if ((zone->nijk = (int *)malloc(index_dim*3*sizeof(int)))==NULL) {
        cgi_error("Error allocating zone->nijk");
        return CG_ERROR;
    }
    for (i=0; i<3*index_dim; i++) zone->nijk[i] = nijk[i];
    zone->index_dim = index_dim;
    zone->type = type;

     /* initialize */

    zone->id = 0;
    zone->link = 0;
    zone->ndescr = 0;
    zone->nzcoor = 0;
    zone->nsections = 0;
    zone->family_name[0]='\0';
    zone->nsols  = 0;
    zone->ndiscrete = 0;
    zone->nintegrals = 0;
    zone->zconn = 0;
    zone->zboco = 0;
    zone->state = 0;
    zone->data_class = DataClassNull;
    zone->units = 0;
    zone->equations = 0;
    zone->converg = 0;
    zone->ordinal = 0;
    zone->nrmotions = 0;
    zone->namotions = 0;
    zone->ziter = 0;
    zone->nuser_data= 0;
    zone->rotating = 0;

     /* save data in file */
    dim_vals[0]=zone->index_dim;
    dim_vals[1]=3;
    if (cgi_new_node(base->id, zone->name, "Zone_t", &zone->id,
        "I4", 2, dim_vals, (void *)zone->nijk)) return CG_ERROR;

    dim_vals[0] = strlen(ZoneTypeName[type]);
    if (cgi_new_node(zone->id, "ZoneType", "ZoneType_t", &dummy_id,
        "C1", 1, dim_vals, ZoneTypeName[type])) return CG_ERROR;

    return CG_OK;
}

int cg_family_write(int file_number, int B, char const * family_name, int *F) {
    int index;
    cgns_base *base;
    cgns_family *family = NULL;

     /* verify input */
    if (cgi_check_strlen(family_name)) return CG_ERROR;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for base */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* Overwrite a Family_t Node: */
    for (index=0; index<base->nfamilies; index++) {
        if (strcmp(family_name, base->family[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",family_name);
                return CG_ERROR;
            }

             /* overwrite an existing zone */
             /* delete the existing zone from file */
            if (cgi_delete_node(base->id, base->family[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            family = &(base->family[index]);
             /* free memory */
            cgi_free_family(family);
            break;
        }
    }
     /* ... or add a Family_t Node: */
    if (index==base->nfamilies) {
        if (base->nfamilies == 0) {
            base->family = CGNS_NEW(cgns_family, base->nfamilies+1);
        } else {
            base->family = CGNS_RENEW(cgns_family, base->nfamilies+1, base->family);
        }
        family = &(base->family[base->nfamilies]);
        base->nfamilies++;
    }
    (*F) = index+1;

    strcpy(family->name, family_name);
    family->id=0;
    family->link = 0;
    family->ndescr = 0;
    family->nfambc = 0;
    family->ngeos = 0;
    family->ordinal = 0;
    family->nuser_data= 0;
    family->rotating = 0;

     /* save data in file */
    if (cgi_new_node(base->id, family->name, "Family_t", &family->id,
        "MT", 0, 0, 0)) return CG_ERROR;

    return CG_OK;
}

int cg_fambc_write(int file_number, int B, int F, char const * fambc_name,
    BCType_t bocotype, int *BC) {
    int index, length;
    cgns_family *family;
    cgns_fambc *fambc = NULL;

     /* verify input */
    if (cgi_check_strlen(fambc_name)) return CG_ERROR;
    if (bocotype<0 || bocotype>=NofValidBCTypes) {
        cgi_error("Invalid BCType:  %d",bocotype);
        return CG_ERROR;
    }

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for family */
    family = cgi_get_family(cg, B, F);
    if (family==0) return CG_ERROR;

     /* Overwrite a FamilyBC_t Node: */
    for (index=0; index<family->nfambc; index++) {
        if (strcmp(fambc_name, family->fambc[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",fambc_name);
                return CG_ERROR;
            }

             /* overwrite an existing zone */
             /* delete the existing fambc from file */
            if (cgi_delete_node(family->id, family->fambc[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            fambc = &(family->fambc[index]);
             /* free memory */
            cgi_free_fambc(fambc);
            break;
        }
    }
     /* ... or add a FamilyBC_t Node: */
    if (index==family->nfambc) {
        if (family->nfambc == 0) {
            family->fambc = CGNS_NEW(cgns_fambc, family->nfambc+1);
        } else {
            family->fambc = CGNS_RENEW(cgns_fambc, family->nfambc+1, family->fambc);
        }
        fambc = &(family->fambc[family->nfambc]);
        family->nfambc++;
    }
    (*BC) = index+1;

    strcpy(fambc->name, fambc_name);
    fambc->id=0;
    fambc->link = 0;
    fambc->type = bocotype;
    fambc->ndataset = 0;
    fambc->dataset = 0;

     /* save data in file */
    length = strlen(BCTypeName[bocotype]);
    if (cgi_new_node(family->id, fambc->name, "FamilyBC_t", &fambc->id,
        "C1", 1, &length, BCTypeName[bocotype])) return CG_ERROR;
    return CG_OK;
}

int cg_geo_write(int file_number, int B, int F, char const * geo_name,
    char const * filename, char const * CADname, int *G) {
    int index, length;
    cgns_family *family;
    cgns_geo *geo = NULL;
    double dummy_id;

     /* verify input */
    if (cgi_check_strlen(geo_name)) return CG_ERROR;
    if (cgi_check_strlen(CADname)) return CG_ERROR;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for family */
    family = cgi_get_family(cg, B, F);
    if (family==0) return CG_ERROR;

     /* Overwrite a GeometryReference_t Node: */
    for (index=0; index<family->ngeos; index++) {
        if (strcmp(geo_name, family->geo[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",geo_name);
                return CG_ERROR;
            }

             /* overwrite an existing zone */
             /* delete the existing geo from file */
            if (cgi_delete_node(family->id, family->geo[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            geo = &(family->geo[index]);
             /* free memory */
            cgi_free_geo(geo);
            break;
        }
    }
     /* ... or add a GeometryReference_t Node: */
    if (index==family->ngeos) {
        if (family->ngeos == 0) {
            family->geo = CGNS_NEW(cgns_geo, family->ngeos+1);
        } else {
            family->geo = CGNS_RENEW(cgns_geo, family->ngeos+1, family->geo);
        }
        geo = &(family->geo[family->ngeos]);
        family->ngeos++;
    }
    (*G) = index+1;


    strcpy(geo->name, geo_name);
    strcpy(geo->format, CADname);
    geo->id=0;
    geo->link=0;
    geo->ndescr=0;
    geo->npart=0;
    geo->nuser_data=0;

    length = strlen(filename);
    if (length<=0) {
        cgi_error("filename undefined for GeometryReference node!");
        return CG_ERROR;
    }
    geo->file = (char *)malloc((length+1)*sizeof(char));
    if (geo->file == NULL) {
        cgi_error("Error allocation geo->file");
        return CG_ERROR;
    }
    strcpy(geo->file, filename);

     /* save data in file */
    if (cgi_new_node(family->id, geo->name, "GeometryReference_t", &geo->id,
        "MT", 0, 0, 0)) return CG_ERROR;
    length = strlen(geo->file);
    if (cgi_new_node(geo->id, "GeometryFile", "GeometryFile_t", &dummy_id,
        "C1", 1, &length, geo->file)) return CG_ERROR;
    length = strlen(geo->format);
    if (cgi_new_node(geo->id, "GeometryFormat", "GeometryFormat_t", &dummy_id,
        "C1", 1, &length, geo->format)) return CG_ERROR;
    return CG_OK;
}

int cg_part_write(int file_number, int B, int F, int G, char const * part_name,
    int *P) {
    int index;
    cgns_geo *geo;
    cgns_part *part = NULL;
    cgns_family *family;

     /* verify input */
    if (cgi_check_strlen(part_name)) return CG_ERROR;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;
    
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for geo */
    family = cgi_get_family(cg, B, F);
    if (family==0) return CG_ERROR;
    if (G > family->ngeos || G <=0) {
        cgi_error("Invalid index for GeometryEntity_t node");
        return CG_ERROR;
    }
    geo = &family->geo[G-1];

     /* Overwrite a GeometryEntity_t Node: */
    for (index=0; index<geo->npart; index++) {
        if (strcmp(part_name, geo->part[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",part_name);
                return CG_ERROR;
            }

             /* overwrite an existing zone */
             /* delete the existing geo from file */
            if (cgi_delete_node(geo->id, geo->part[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            part = &(geo->part[index]);
             /* free memory */
            cgi_free_part(part);
            break;
        }
    }
     /* ... or add a GeometryReference_t Node: */
    if (index==geo->npart) {
        if (geo->npart == 0) {
            geo->part = CGNS_NEW(cgns_part, geo->npart+1);
        } else {
            geo->part = CGNS_RENEW(cgns_part, geo->npart+1, geo->part);
        }
        part = &(geo->part[geo->npart]);
        geo->npart++;
    }
    (*P) = index+1;

    strcpy(part->name, part_name);
    part->id=0;
    part->link=0;

     /* save data in file */
    if (cgi_new_node(geo->id, part->name, "GeometryEntity_t", &part->id,
        "MT", 0, 0, 0)) return CG_ERROR;
    return CG_OK;
}

int cg_discrete_write(int file_number, int B, int Z,  char const * discrete_name, int *D) {
    cgns_zone *zone;
    cgns_discrete *discrete = NULL;
    int index;

     /* verify input */
    if (cgi_check_strlen(discrete_name)) return CG_ERROR;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Overwrite a DiscreteData_t node: */
    for (index=0; index<zone->ndiscrete; index++) {
        if (strcmp(discrete_name, zone->discrete[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",discrete_name);
                return CG_ERROR;
            }

             /* overwrite an existing solution */
             /* delete the existing solution from file */
            if (cgi_delete_node(zone->id, zone->discrete[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            discrete = &(zone->discrete[index]);
             /* free memory */
            cgi_free_discrete(discrete);
            break;
        }
    }
     /* ... or add a FlowSolution_t Node: */
    if (index==zone->ndiscrete) {
        if (zone->ndiscrete == 0) {
            zone->discrete = CGNS_NEW(cgns_discrete, zone->ndiscrete+1);
        } else {
            zone->discrete = CGNS_RENEW(cgns_discrete, zone->ndiscrete+1, zone->discrete);
        }
        discrete = &zone->discrete[zone->ndiscrete];
        zone->ndiscrete++;
    }
    (*D) = index+1;

     /* save data in memory */
    strcpy(discrete->name, discrete_name);

     /* initialize other fields */
    discrete->id = 0;
    discrete->link=0;
    discrete->ndescr=0;
    discrete->location=Vertex;
    discrete->rind_planes=0;
    discrete->narrays=0;
    discrete->data_class=DataClassNull;
    discrete->units=0;
    discrete->nuser_data=0;

     /* save data in file */
    if (cgi_new_node(zone->id, discrete->name, "DiscreteData_t", &discrete->id,
        "MT", 0, 0, 0)) return CG_ERROR;
    return CG_OK;
}


int cg_coord_write(int file_number, int B, int Z, DataType_t type, char const * coordname,
           void const * coord_ptr, int *C) {
    cgns_zone *zone;
    cgns_zcoor *zcoor;
    cgns_array *coord;
    int n, index, index_dim;

     /* verify input */
    if (cgi_check_strlen(coordname)) return CG_ERROR;
    if (type!=RealSingle && type!=RealDouble) {
        cgi_error("Invalid datatype for coord. array:  %d", type);
        return CG_ERROR;
    }
     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for zone */
    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Get memory address for node "GridCoordinates" */
    zcoor = cgi_get_zcoorGC(cg, B, Z);
    if (zcoor==0) return CG_ERROR;

     /* Overwrite a DataArray_t Node of same size, name and data-type: */
    for (index=0; index<zcoor->ncoords; index++) {
        if (strcmp(coordname, zcoor->coord[index].name)==0) {
            coord = &(zcoor->coord[index]);

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",coordname);
                return CG_ERROR;
            }

             /* overwrite an existing coordinate vector */
            if (type==cgi_datatype(coord->data_type)) {
                if (cgio_write_all_data(cg->cgio, coord->id, coord_ptr)) {
                    cg_io_error("cgio_write_all_data");
                    return CG_ERROR;
                }
                (*C) = index+1;
                return CG_OK;
            }
            cgi_error("To overwrite array %s, use data-type '%s'",
                coord->name, DataTypeName[cgi_datatype(coord->data_type)]);
            return CG_ERROR;
        }
    }

     /* ... or add a DataArray_t Node: */
    if (zcoor->ncoords == 0) {
        zcoor->coord = CGNS_NEW(cgns_array, zcoor->ncoords+1);
    } else {
        zcoor->coord = CGNS_RENEW(cgns_array, zcoor->ncoords+1, zcoor->coord);
    }
    coord = &(zcoor->coord[zcoor->ncoords]);
    zcoor->ncoords++;
    (*C) = zcoor->ncoords;

     /* save coord. data in memory */
    strcpy(coord->data_type,cgi_adf_datatype(type));
    strcpy(coord->name,coordname);
    coord->id=0;        /* initialize */
    coord->link=0;
    index_dim = zone->index_dim;
    for (n=0; n<index_dim; n++)
        coord->dim_vals[n] = zone->nijk[n] + zcoor->rind_planes[2*n]
                             + zcoor->rind_planes[2*n+1];
    coord->data_dim=index_dim;
    coord->data=0;
    coord->ndescr=0;
    coord->data_class=DataClassNull;
    coord->units=0;
    coord->exponents=0;
    coord->convert=0;

     /* Create GridCoodinates_t node if not already created */
    if (zcoor->id == 0) {
        if (cgi_new_node(zone->id, "GridCoordinates", "GridCoordinates_t",
            &zcoor->id, "MT", 0, 0, 0)) return CG_ERROR;
    }
     /* Create DataArray_t node on disk */
    if (cgi_new_node(zcoor->id, coord->name, "DataArray_t", &coord->id,
        coord->data_type, index_dim, coord->dim_vals, coord_ptr)) return CG_ERROR;

    return CG_OK;
}

int cg_section_write(int file_number, int B, int Z, char const * SectionName, ElementType_t type,
    int start, int end, int nbndry, int const * elements, int *S) {
    cgns_zone *zone;
    cgns_section *section = NULL;
    int index, num;
    int ElementDataSize=0;

     /* verify input */
    if (cgi_check_strlen(SectionName)) return CG_ERROR;

    if (type < 0 || type >= NofValidElementTypes) {
        cgi_error("Invalid element type defined for section '%s'",SectionName);
        return CG_ERROR;
    }
    if ((end-start)<0) {
        cgi_error("Invalid element range defined for section '%s'",SectionName);
        return CG_ERROR;
    }
    if (nbndry>(end-start+1)) {
        cgi_error("Invalid boundary element number for section '%s'",SectionName);
        return CG_ERROR;
    }

     /* get file and check mode */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Overwrite a Elements_t Node: */
    for (index=0; index<zone->nsections; index++) {
        if (strcmp(SectionName, zone->section[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",SectionName);
                return CG_ERROR;
            }

             /* overwrite an existing section */
             /* delete the existing section from file */
            if (cgi_delete_node(zone->id, zone->section[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            section = &(zone->section[index]);
             /* free memory */
            cgi_free_section(section);
            break;
        }
    }
     /* ... or add a Elements_t Node: */
    if (index==zone->nsections) {
        if (zone->nsections == 0) {
            zone->section = CGNS_NEW(cgns_section, zone->nsections+1);
        } else {
            zone->section = CGNS_RENEW(cgns_section, zone->nsections+1, zone->section);
        }
        section = &(zone->section[zone->nsections]);
        zone->nsections++;
    }
    (*S) = index+1;

     /* save data in memory */
    strcpy(section->name, SectionName);
    section->el_type = type;
    section->range[0] = start;
    section->range[1] = end;
    section->el_bound = nbndry;

     /* Compute ElementDataSize */
    num = end - start +1;
    ElementDataSize = cgi_element_data_size(type, num, elements);
    if (ElementDataSize < 0) return CG_ERROR;

     /* Write element connectivity in internal data structure */
    section->connect = CGNS_NEW(cgns_array, 1);
    section->connect->data = 0;
    strcpy(section->connect->name,"ElementConnectivity");
    strcpy(section->connect->data_type,"I4");
    section->connect->data_dim=1;
    section->connect->dim_vals[0]=ElementDataSize;

    /* initialize ... */
    section->id=0;
    section->link=0;
    section->ndescr=0;
    section->parent=0;
    section->nuser_data=0;
    section->rind_planes=0;

     /* initialize other fields */
    section->connect->id=0;
    section->connect->link=0;
    section->connect->ndescr=0;
    section->connect->data_class=DataClassNull;
    section->connect->units=0;
    section->connect->exponents=0;
    section->connect->convert=0;

    if (cgi_write_section(zone->id, section)) return CG_ERROR;
    if (cgio_write_all_data(cg->cgio, section->connect->id, elements)) {
        cg_io_error("cgio_write_all_data");
        return CG_ERROR;
    }
    return CG_OK;
}

int cg_parent_data_write(int file_number, int B, int Z, int S, int const * parent_data) {
    cgns_section *section;
    cgns_array *parent;
    int num;

     /* get file and check mode */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    section = cgi_get_section(cg, B, Z, S);
    if (section == 0) return CG_ERROR;

    if (section->parent) {
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("ParentData is already defined under Elements_t '%s'",
                   section->name);
            return CG_ERROR;
        }
        if (cgi_delete_node(section->id, section->parent->id))
            return CG_ERROR;
        cgi_free_array(section->parent);
    } else
        section->parent = CGNS_NEW(cgns_array, 1);
    parent = section->parent;

    num = section->range[1]-section->range[0]+1;
    parent->data = 0;
    strcpy(parent->data_type, "I4");
    strcpy(parent->name, "ParentData");
    parent->data_dim =2;
    parent->dim_vals[0]=num;
    parent->dim_vals[1]=4;

     /* initialize other fields */
    parent->id=0;
    parent->link=0;
    parent->ndescr=0;
    parent->data_class=DataClassNull;
    parent->units=0;
    parent->exponents=0;
    parent->convert=0;

    if (cgi_write_array(section->id, section->parent)) return CG_ERROR;
    if (cgio_write_all_data(cg->cgio, section->parent->id, parent_data)) {
        cg_io_error("cgio_write_all_data");
        return CG_ERROR;
    }
    return CG_OK;
}

int cg_grid_write(int file_number, int B, int Z, char const * zcoorname, int *G) {
    cgns_zone *zone;
    cgns_zcoor *zcoor = NULL;
    int index, n, index_dim;

     /* verify input */
    if (cgi_check_strlen(zcoorname)) return CG_ERROR;

     /* get memory address */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Overwrite a GridCoordinates_t Node: */
    for (index=0; index<zone->nzcoor; index++) {
        if (strcmp(zcoorname, zone->zcoor[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",zcoorname);
                return CG_ERROR;
            }

             /* overwrite an existing GridCoordinates_t node */
             /* delete the existing GridCoordinates_t from file */
            if (cgi_delete_node(zone->id, zone->zcoor[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            zcoor = &(zone->zcoor[index]);
             /* free memory */
            cgi_free_zcoor(zcoor);
            break;
        }
    }
     /* ... or add a GridCoordinates_t Node: */
    if (index==zone->nzcoor) {
        if (zone->nzcoor == 0) {
            zone->zcoor = CGNS_NEW(cgns_zcoor, 1);
        } else {
            zone->zcoor = CGNS_RENEW(cgns_zcoor, zone->nzcoor+1, zone->zcoor);
        }
        zcoor = &(zone->zcoor[zone->nzcoor]);
        zone->nzcoor++;
    }
    (*G) = index+1;

     /* save data in memory */
    strcpy(zcoor->name,zcoorname);

     /* initialize */
    zcoor->ncoords = 0;
    zcoor->id=0;
    zcoor->link=0;
    zcoor->ndescr=0;
    zcoor->data_class=DataClassNull;
    zcoor->units=0;
    zcoor->nuser_data=0;

    index_dim = zone->index_dim;
    zcoor->rind_planes = (int *)malloc(index_dim*2*sizeof(int));
    if (zcoor->rind_planes == NULL) {
        cgi_error("Error allocating zcoor->rind_plane.");
        return CG_ERROR;
    }
    for (n=0; n<index_dim; n++)
        zcoor->rind_planes[2*n]=zcoor->rind_planes[2*n+1]=0;

     /* save data in file */
    if (cgi_new_node(zone->id, zcoor->name, "GridCoordinates_t", &zcoor->id,
        "MT", 0, 0, 0)) return CG_ERROR;

    return CG_OK;
}

int cg_sol_write(int file_number, int B, int Z, char const * solname,
         GridLocation_t location, int *S) {
    cgns_zone *zone;
    cgns_sol *sol = NULL;
    int index, n, index_dim;

     /* verify input */
    if (cgi_check_strlen(solname)) return CG_ERROR;
    if (location != Vertex && location != CellCenter &&
        location !=IFaceCenter && location != JFaceCenter &&
        location != KFaceCenter) {
        cgi_error("Given grid location not supported for FlowSolution_t");
        return CG_ERROR;
    }
     /* if (location<0 || location >= NofValidGridLocation) {
        cgi_error("Invalid input:  GridLocation=%d ?",location);
        return CG_ERROR;
    } */

     /* get memory address for FlowSolution node */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;
    if (zone->type != Structured && (location == IFaceCenter ||
        location == JFaceCenter || location == KFaceCenter)) {
        cgi_error ("GridLocation [IJK]FaceCenter only valid for Structured grid");
        return CG_ERROR;
    }

     /* Overwrite a FlowSolution_t Node: */
    for (index=0; index<zone->nsols; index++) {
        if (strcmp(solname, zone->sol[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",solname);
                return CG_ERROR;
            }

             /* overwrite an existing solution */
             /* delete the existing solution from file */
            if (cgi_delete_node(zone->id, zone->sol[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            sol = &(zone->sol[index]);
             /* free memory */
            cgi_free_sol(sol);
            break;
        }
    }
     /* ... or add a FlowSolution_t Node: */
    if (index==zone->nsols) {
        if (zone->nsols == 0) {
            zone->sol = CGNS_NEW(cgns_sol, zone->nsols+1);
        } else {
            zone->sol = CGNS_RENEW(cgns_sol, zone->nsols+1, zone->sol);
        }
        sol = &(zone->sol[zone->nsols]);
        zone->nsols++;
    }
    (*S) = index+1;

     /* save data in memory */
    strcpy(sol->name,solname);
    sol->location = location;

     /* initialize */
    sol->nfields = 0;
    sol->id=0;
    sol->link=0;
    sol->ndescr=0;
    sol->data_class=DataClassNull;
    sol->units=0;
    sol->nuser_data=0;

    index_dim = zone->index_dim;
    sol->rind_planes = (int *)malloc(index_dim*2*sizeof(int));
    if (sol->rind_planes == NULL) {
        cgi_error("Error allocating sol->rind_plane.");
        return CG_ERROR;
    }
    for (n=0; n<index_dim; n++)
        sol->rind_planes[2*n]=sol->rind_planes[2*n+1]=0;

     /* save data in file */
    if (cgi_new_node(zone->id, sol->name, "FlowSolution_t", &sol->id,
        "MT", 0, 0, 0)) return CG_ERROR;
    if (sol->location != Vertex) {
        int length = strlen(GridLocationName[sol->location]);
        double GL_id;
        if (cgi_new_node(sol->id, "GridLocation", "GridLocation_t", &GL_id,
            "C1", 1, &length, (void *)GridLocationName[sol->location])) return CG_ERROR;
    }

    return CG_OK;
}

int cg_field_write(int file_number, int B, int Z, int S, DataType_t type, char const * fieldname,
           void const * field_ptr, int *F) {
    cgns_zone *zone;
    cgns_sol *sol;
    cgns_array *field;
    int index, index_dim;

     /* verify input */
    if (cgi_check_strlen(fieldname)) return CG_ERROR;
    if (type!=RealSingle && type!=RealDouble && type!=Integer) {
        cgi_error("Invalid datatype for solution array %s: %d",fieldname, type);
        return CG_ERROR;
    }
     /* get memory addresses */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    sol = cgi_get_sol(cg, B, Z, S);
    if (sol==0) return CG_ERROR;

    index_dim = zone->index_dim;

     /* Overwrite a DataArray_t Node: */
    for (index=0; index<sol->nfields; index++) {
        if (strcmp(fieldname, sol->field[index].name)==0) {
            field = &(sol->field[index]);

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",fieldname);
                return CG_ERROR;
            }

             /* overwrite an existing solution */
            if (type==cgi_datatype(field->data_type)) {
                if (cgio_write_all_data(cg->cgio, field->id, field_ptr)) {
                    cg_io_error("cgio_write_all_data");
                    return CG_ERROR;
                }
                (*F) = index+1;
                return CG_OK;
            }
            cgi_error("To overwrite array %s, use data-type '%s'",
                field->name, DataTypeName[cgi_datatype(field->data_type)]);
            return CG_ERROR;
        }
    }
     /* ... or add a DataArray_t Node: */
    if (sol->nfields == 0) {
        sol->field = CGNS_NEW(cgns_array, sol->nfields+1);
    } else {
        sol->field = CGNS_RENEW(cgns_array, sol->nfields+1, sol->field);
    }
    field = &(sol->field[sol->nfields]);
    sol->nfields++;
    (*F) = sol->nfields;

     /* save data in memory */
    strcpy(field->data_type, cgi_adf_datatype(type));
    strcpy(field->name,fieldname);
    field->data_dim = zone->index_dim;
    if (cgi_datasize(index_dim, zone->nijk, sol->location,
            sol->rind_planes, field->dim_vals)) return CG_ERROR;

     /* initialize */
    field->id = 0;
    field->link= 0;
    field->data=0;
    field->ndescr= 0;
    field->data_class= DataClassNull;
    field->units= 0;
    field->exponents= 0;
    field->convert= 0;

     /* Save DataArray_t node on disk: */
    if (cgi_new_node(sol->id, field->name, "DataArray_t", &field->id,
        field->data_type, index_dim, field->dim_vals, field_ptr)) return CG_ERROR;

    return CG_OK;
}

int cg_hole_write(int file_number, int B, int Z, char const * holename, GridLocation_t location,
          PointSetType_t ptset_type, int nptsets, int npnts, int const * pnts, int *I) {
    cgns_zone *zone;
    cgns_zconn *zconn;
    cgns_hole *hole = NULL;
    cgns_ptset *ptset;
    char_33 PointSetName;
    int index, set;
    int i, index_dim;

     /* verify input */
    if (cgi_check_strlen(holename)) return CG_ERROR;
    if (location != Vertex && location != CellCenter) {
        cgi_error("cg_hole_write: GridLocation not Vertex or CellCenter");
        return CG_ERROR;
    }
    if (ptset_type!=PointList && ptset_type!=PointRange) {
        cgi_error("Invalid input:  ptset_type=%d ?",ptset_type);
        return CG_ERROR;
    }
    if (!(ptset_type==PointRange && npnts==2*nptsets && nptsets>0) &&
        !(ptset_type==PointList && npnts>=0 && nptsets==1)) {
        cgi_error("Invalid input:  nptsets=%d, npoint=%d, point set type=%s",
               nptsets, npnts, PointSetTypeName[ptset_type]);
        return CG_ERROR;
    }
     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for zone */
    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Allocate ZoneGridConnectivity data struct. if not already created */
    if (zone->zconn == 0) {
        zone->zconn = CGNS_NEW(cgns_zconn, 1);
        zconn = zone->zconn;
        strcpy(zconn->name,"ZoneGridConnectivity");

     /* initialize new ZoneGridConnectivity data structure */
        zconn->id = 0;
        zconn->link= 0;
        zconn->ndescr = 0;
        zconn->n1to1 = 0;
        zconn->nconns = 0;
        zconn->nholes = 0;
        zconn->nuser_data= 0;
    } else zconn = zone->zconn;

    index_dim = zone->index_dim;

     /* Overwrite an OversetHoles_t Node: */
    for (index=0; index<zconn->nholes; index++) {
        if (strcmp(holename, zconn->hole[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",holename);
                return CG_ERROR;
            }

             /* overwrite an existing Overset Hole */
             /* delete the existing Overset hole from file */
            if (cgi_delete_node(zconn->id, zconn->hole[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            hole = &(zconn->hole[index]);
            cgi_free_hole(hole);
            break;
        }
    }
     /* ... or add an OversetHoles_t Node: */
    if (index==zconn->nholes) {
        if (zconn->nholes == 0) {
            zconn->hole = CGNS_NEW(cgns_hole, zconn->nholes+1);
        } else {
            zconn->hole = CGNS_RENEW(cgns_hole, zconn->nholes+1, zconn->hole);
        }
        hole = &(zconn->hole[zconn->nholes]);
        zconn->nholes++;
    }
    (*I) = index+1;

     /* write hole info to internal memory */
    strcpy(hole->name,holename);
    hole->location = location;

    hole->nptsets = nptsets;
    hole->ptset = CGNS_NEW(cgns_ptset, nptsets);
    for (set=0; set<nptsets; set++) {
        ptset = &hole->ptset[set];
        ptset->type = ptset_type;
        strcpy(ptset->data_type,"I4");
        if (ptset_type==PointRange) ptset->npts = 2;
        else            ptset->npts = npnts;
        ptset->id = 0;
        ptset->link = 0;

     /* Record the number of nodes or elements in the point set */
        if (ptset_type==PointList)
            ptset->size_of_patch=npnts;
        else if (ptset_type==PointRange) {
            ptset->size_of_patch = 1;
            for (i=0; i<index_dim; i++)
                ptset->size_of_patch *= (pnts[i+index_dim]-pnts[i]+1);
        }
    }

     /* initialize */
    hole->id=0;
    hole->link=0;
    hole->ndescr=0;
    hole->nuser_data=0;

     /* Create node ZoneGridConnectivity_t node, if not yet created */
    if (zconn->id==0) {
        if (cgi_new_node(zone->id, "ZoneGridConnectivity", "ZoneGridConnectivity_t",
             &zconn->id, "MT", 0, 0, 0)) return CG_ERROR;
    }
    if (cgi_new_node(zconn->id, hole->name, "OversetHoles_t",
        &hole->id, "MT", 0, 0, 0)) return CG_ERROR;

    if (hole->location !=Vertex) {
        double GL_id;
        int length = strlen(GridLocationName[hole->location]);
        if (cgi_new_node(hole->id, "GridLocation", "GridLocation_t", &GL_id,
            "C1", 1, &length, GridLocationName[hole->location])) return CG_ERROR;
    }

    for (set=0; set<nptsets; set++) {
        ptset = &hole->ptset[set];

        if (ptset->npts>0) {
             /* Create Point Set node on disk */
            if (ptset->type==PointRange)
                sprintf(PointSetName, "PointRange%d",set+1);
            else
                sprintf(PointSetName, PointSetTypeName[ptset->type]);
            if (cgi_write_ptset(hole->id, PointSetName, ptset, index_dim,
                (void *)((int *)pnts+2*index_dim*set))) return CG_ERROR;
        }
    }

    return CG_OK;
}

int cg_conn_write(int file_number, int B, int Z,  char const * connectname, GridLocation_t location,
          GridConnectivityType_t type, PointSetType_t ptset_type, int npnts, int const * pnts,
          char const * donorname, ZoneType_t donor_zonetype,  PointSetType_t donor_ptset_type,
          DataType_t donor_datatype, int ndata_donor, void const * donor_data, int *I) {
    cgns_zone *zone;
    cgns_zconn *zconn;
    cgns_conn *conn = NULL;
    cgns_ptset *dptset;
    int i, size_of_zone, length;
    int PointListSize, cell_dim;
    int index, index_dim, index_dim_donor;
    double GL_id, C_id;

     /* verify input */
    if (cgi_check_strlen(connectname)) return CG_ERROR;
    if (cgi_check_strlen(donorname)) return CG_ERROR;
    if (type <0 || type >= NofValidGridConnectivityTypes) {
        cgi_error("Invalid input:  GridConnectivityType=%d ?",type);
        return CG_ERROR;
    }
    if (location != Vertex && location != CellCenter &&
        location != FaceCenter && location != IFaceCenter &&
        location != JFaceCenter && location != KFaceCenter) {
        cgi_error("Invalid input:  GridLocation=%d ?",location);
        return CG_ERROR;
    }
    if (type == Overset && location != Vertex && location != CellCenter) {
        cgi_error("GridLocation must be Vertex or CellCenter for Overset");
        return CG_ERROR;
    }
    if (ptset_type!=PointList && ptset_type!=PointRange) {
        cgi_error("Invalid input:  ptset_type=%d ?",ptset_type);
        return CG_ERROR;
    }
    if (!(ptset_type==PointRange && npnts==2) && !(ptset_type==PointList && npnts>0)) {
        cgi_error("Invalid input:  npoint=%d, point set type=%s",
               npnts, PointSetTypeName[ptset_type]);
        return CG_ERROR;
    }
    if (ndata_donor) {
        if (donor_ptset_type!=CellListDonor && donor_ptset_type!=PointListDonor) {
            cgi_error("Invalid point set type for donor %s",donorname);
            return CG_ERROR;
        }
        if (donor_datatype != Integer) {
            cgi_error("Invalid datatype for donor %s",donorname);
            return CG_ERROR;
        }
/*
        if (donor_zonetype==Unstructured && (donor_ptset_type!=CellListDonor &&
            donor_ptset_type!=PointListDonor)) {
            cgi_error("Invalid point set type for Unstructured donor %s",donorname);
            return CG_ERROR;
        }
        if (donor_zonetype==Unstructured && donor_datatype != Integer) {
            cgi_error("Invalid datatype for Unstructured donor %s",donorname);
            return CG_ERROR;
        }
        if (donor_zonetype==Structured && donor_ptset_type!=PointListDonor) {
            cgi_error("Invalid point set type for Structured donor %s",donorname);
            return CG_ERROR;
        }
        if (donor_datatype!=RealSingle && donor_datatype!=Integer &&
            donor_datatype!=RealDouble) {
            cgi_error("Invalid data type for donor_data: %d",donor_datatype);
            return CG_ERROR;
        }
*/
    } else {
        donor_ptset_type = PointSetTypeNull;
        donor_datatype = Integer;
    }

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address of zone */
    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    if ((location == IFaceCenter || location == JFaceCenter ||
         location == KFaceCenter) && zone->type != Structured) {
        cgi_error("GridLocation [IJK]FaceCenter only valid for Structured grids");
        return CG_ERROR;
    }

     /* Allocate ZoneGridConnectivity data struct. if not already created */
    if (zone->zconn == 0) {
        zone->zconn = CGNS_NEW(cgns_zconn, 1);
        zconn = zone->zconn;
        strcpy(zconn->name,"ZoneGridConnectivity");

     /* initialize new ZoneGridConnectivity data structure */
        zconn->id = 0;
        zconn->link= 0;
        zconn->ndescr = 0;
        zconn->n1to1 = 0;
        zconn->nconns = 0;
        zconn->nholes = 0;
        zconn->nuser_data= 0;
    } else zconn = zone->zconn;

     /* IndexDimension & CellDimension */
    index_dim = zone->index_dim;
    cell_dim=cg->base[B-1].cell_dim;

     /* verify input */
    size_of_zone = 1;
    for (i=0; i<index_dim; i++) size_of_zone*=zone->nijk[i];
    if (npnts<0 || npnts>size_of_zone) {
        cgi_error("Inconsistent number of points in point set");
        return CG_ERROR;
    }
#if 0   /* causes problems when grid is unstructured */
    if (ptset_type==PointRange) {
        if (location == Vertex) {
            for (i=0; i<index_dim; i++) {
                if (pnts[i]<0 || pnts[i+index_dim]>zone->nijk[i]) {
                    cgi_error("Invalid input range:  %d->%d",pnts[i], pnts[i+index_dim]);
                    return CG_ERROR;
                }
            }
        } else if (location == CellCenter) {
            for (i=0; i<index_dim; i++) {
                if (pnts[i]<0 || pnts[i+index_dim]>zone->nijk[i+index_dim]) {
                    cgi_error("Invalid input range:  %d->%d",pnts[i], pnts[i+index_dim]);
                    return CG_ERROR;
                }
            }
        }
    }
#endif

     /* Compute PointListSize */
    if (ptset_type==PointRange) {
        PointListSize = 1;
        for (i=0; i<index_dim; i++) {
            PointListSize *= (pnts[i+index_dim]-pnts[i]+1);
        }
    } else  PointListSize=npnts;

    if (ndata_donor && type == Abutting1to1 && PointListSize != ndata_donor) {
        cgi_error("Invalid input for ndata_donor in cg_conn_write");
        return CG_ERROR;
    }

     /* Overwrite a GridConnectivity_t Node: */
    for (index=0; index<zconn->nconns; index++) {
        if (strcmp(connectname, zconn->conn[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",connectname);
                return CG_ERROR;
            }

             /* overwrite an existing GridConnectivity_t Node */
             /* delete the existing GridConnectivity_t Node from file */
            if (cgi_delete_node(zconn->id, zconn->conn[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            conn = &(zconn->conn[index]);
            cgi_free_conn(conn);
            break;
        }
    }
     /* ... or add a GridConnectivity_t Node: */
    if (index==zconn->nconns) {
        if (zconn->nconns == 0) {
            zconn->conn = CGNS_NEW(cgns_conn, zconn->nconns+1);
        } else {
            zconn->conn = CGNS_RENEW(cgns_conn, zconn->nconns+1, zconn->conn);
        }
        conn = &(zconn->conn[zconn->nconns]);
        zconn->nconns++;
    }
    (*I) = index+1;

     /* write conn info to internal memory */
    strcpy(conn->name,connectname);
    conn->type = type;
    conn->location = location;
    conn->ptset.id = 0;
    conn->ptset.link = 0;
    conn->ptset.type = ptset_type;
    strcpy(conn->ptset.data_type,"I4");
    conn->ptset.npts = npnts;
    conn->ptset.size_of_patch = PointListSize;

     /* ... initialize: */
    conn->id=0;
    conn->link=0;
    conn->ndescr=0;
    conn->ordinal=0;
    conn->nuser_data=0;
    conn->cprop=0;

     /* ... donor: */
    strcpy(conn->donor,donorname);
    conn->interpolants = 0;
    dptset = &conn->dptset;
    dptset->id = 0;
    dptset->link = 0;
    strcpy(dptset->name,PointSetTypeName[donor_ptset_type]);
    dptset->type = donor_ptset_type;
    strcpy(dptset->data_type, cgi_adf_datatype(donor_datatype));
    dptset->npts = ndata_donor;
    dptset->size_of_patch = ndata_donor;

     /* Create node ZoneGridConnectivity_t node, if not yet created */
    if (zconn->id==0) {
        if (cgi_new_node(zone->id, "ZoneGridConnectivity", "ZoneGridConnectivity_t",
            &zconn->id, "MT", 0, 0, 0)) return CG_ERROR;
    }
     /* Create node GridConnectivity_t node */
    length = strlen(conn->donor);
    if (cgi_new_node(zconn->id, conn->name, "GridConnectivity_t", &conn->id,
        "C1", 1, &length, conn->donor)) return CG_ERROR;

     /* Create node GridConnectivityType_t */
    length = strlen(GridConnectivityTypeName[conn->type]);
    if (cgi_new_node(conn->id,"GridConnectivityType","GridConnectivityType_t",
        &C_id, "C1", 1, &length, GridConnectivityTypeName[conn->type])) return CG_ERROR;

     /* write GridLocation */
    if (conn->location != Vertex) {
        length = strlen(GridLocationName[conn->location]);
        if (cgi_new_node(conn->id, "GridLocation", "GridLocation_t", &GL_id,
            "C1", 1, &length, GridLocationName[conn->location])) return CG_ERROR;
    }

     /* Write Point Sets to disk */
    if (npnts>0) {
        char_33 PointSetName;
        strcpy (PointSetName, PointSetTypeName[conn->ptset.type]);
        if (cgi_write_ptset(conn->id, PointSetName, &conn->ptset, index_dim,
            (void *)pnts)) return CG_ERROR;

        /* Write pointset of donor */
        if (ndata_donor) {
            if (donor_zonetype==Structured)
                index_dim_donor = cell_dim;
            else
                index_dim_donor=1;
            strcpy (PointSetName, PointSetTypeName[donor_ptset_type]);
            if (cgi_write_ptset(conn->id, PointSetName, dptset, index_dim_donor,
                (void *)donor_data)) return CG_ERROR;
        }
    }
    return CG_OK;
}

int cg_conn_write_short(int file_number, int B, int Z,  char const * connectname,
          GridLocation_t location, GridConnectivityType_t type, PointSetType_t ptset_type,
          int npnts, int const * pnts, char const * donorname, int *I) {
    return cg_conn_write (file_number, B, Z,  connectname, location,
          type, ptset_type, npnts, pnts, donorname,
          ZoneTypeNull, PointSetTypeNull, DataTypeNull, 0, NULL, I);
}

int cg_conn_average_write(int file_number, int B, int Z, int I,
    AverageInterfaceType_t AverageInterfaceType) {

    cgns_cprop *cprop;
    cgns_caverage *caverage;
    cgns_conn *conn;
    int length;
    double dummy_id;

     /* verify input */
    if (AverageInterfaceType<0 || AverageInterfaceType>=NofValidAverageInterfaceTypes) {
        cgi_error("Invalid AverageInterfaceType:  %d",AverageInterfaceType);
        return CG_ERROR;
    }

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address of GridConnectivity_t node */
    conn = cgi_get_conn(cg, B, Z, I);
    if (conn==0) return CG_ERROR;

     /* Allocate GridConnectivityProperty_t data struct. if not already created */
    if (conn->cprop == 0) {
        conn->cprop = CGNS_NEW(cgns_cprop, 1);
        cprop = conn->cprop;
        strcpy(cprop->name,"GridConnectivityProperty");
        cprop->id=0;
        cprop->link=0;
        cprop->ndescr=0;
        cprop->nuser_data=0;
        cprop->cperio=0;
        cprop->caverage=0;
    } else cprop = conn->cprop;

     /* Overwrite an AverageInterface_t Node: */
    if (cprop->caverage) {
     /* in CG_MODE_WRITE, children names must be unique */
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("AverageInterface_t already defined under GridConnectivityProperty_t");
            return CG_ERROR;
        }

     /* overwrite an existing AverageInterface_t Node */
         /* delete the existing AverageInterface_t Node from file */
        if (cgi_delete_node(cprop->id, cprop->caverage->id))
            return CG_ERROR;
        cgi_free_caverage(cprop->caverage);
    } else
        cprop->caverage = CGNS_NEW(cgns_caverage, 1);
    caverage = cprop->caverage;

     /* write caverage info to internal memory */
    caverage->type = AverageInterfaceType;

     /* initialize other fields */
    strcpy(caverage->name,"AverageInterface");
    caverage->id = 0;
    caverage->link = 0;
    caverage->ndescr = 0;
    caverage->nuser_data = 0;

    /* Create GridConnectivityProperty_t node if it doesn't yet exist */
    if (cprop->id==0) {
        if (cgi_new_node(conn->id, "GridConnectivityProperty",
            "GridConnectivityProperty_t", &cprop->id, "MT", 0, 0, 0)) return CG_ERROR;
    }
    /* Create the AverageInterface_t Node */
    if (cgi_new_node(cprop->id, "AverageInterface", "AverageInterface_t",
        &caverage->id, "MT", 0, 0, 0)) return CG_ERROR;

    /* AverageInterface_t/AverageInterfaceType_t */
    length = strlen(AverageInterfaceTypeName[caverage->type]);
    if (cgi_new_node(caverage->id, "AverageInterfaceType", "AverageInterfaceType_t", &dummy_id,
        "C1", 1, &length, (void *)AverageInterfaceTypeName[caverage->type])) return CG_ERROR;
    return CG_OK;
}

int cg_conn_periodic_write(int file_number, int B, int Z, int I,
    float const *RotationCenter, float const *RotationAngle,
    float const *Translation) {
    cgns_base *base;
    cgns_conn *conn;
    cgns_cprop *cprop;
    cgns_cperio *cperio;
    int n;

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for base */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* get memory address of GridConnectivity_t node */
    conn = cgi_get_conn(cg, B, Z, I);
    if (conn==0) return CG_ERROR;

     /* Allocate GridConnectivityProperty_t data struct. if not already created */
    if (conn->cprop == 0) {
        conn->cprop = CGNS_NEW(cgns_cprop, 1);
        cprop = conn->cprop;
        strcpy(cprop->name,"GridConnectivityProperty");
        cprop->id=0;
        cprop->link=0;
        cprop->ndescr=0;
        cprop->nuser_data=0;
        cprop->cperio=0;
        cprop->caverage=0;
    } else cprop = conn->cprop;

     /* Overwrite a Periodic_t Node: */
    if (cprop->cperio) {
     /* in CG_MODE_WRITE, children names must be unique */
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("Periodic_t already defined under GridConnectivityProperty_t.");
            return CG_ERROR;
        }

     /* overwrite an existing Periodic_t Node */
         /* delete the existing Periodic_t Node from file */
        if (cgi_delete_node(cprop->id, cprop->cperio->id))
            return CG_ERROR;
        cgi_free_cperio(cprop->cperio);
    } else
        cprop->cperio = CGNS_NEW(cgns_cperio, 1);
    cperio = cprop->cperio;

     /* write cperio info to internal memory */
    strcpy(cperio->name,"Periodic");
    cperio->id = 0;
    cperio->link = 0;
    cperio->ndescr = 0;
    cperio->nuser_data = 0;
    cperio->narrays = 3;
    cperio->data_class=DataClassNull;
    cperio->units=0;

     /* Create DataArray_t RotationCenter, RotationAngle, & Translation under Periodic_t */
    cperio->array = CGNS_NEW(cgns_array, 3);

    for (n=0; n<cperio->narrays; n++) {
        strcpy(cperio->array[n].data_type, "R4");
        cperio->array[n].data = (void *)malloc(base->phys_dim*sizeof(float));
        if (cperio->array[n].data == NULL) {
            cgi_error("Error allocating cperio->array[n].data");
            return CG_ERROR;
        }
        cperio->array[n].data_dim=1;
        cperio->array[n].dim_vals[0]=base->phys_dim;
        cperio->array[n].id=0;
        cperio->array[n].link=0;
        cperio->array[n].ndescr=0;
        cperio->array[n].data_class=DataClassNull;
        cperio->array[n].units=0;
        cperio->array[n].exponents=0;
        cperio->array[n].convert=0;
    }
    memcpy(cperio->array[0].data,RotationCenter,base->phys_dim*sizeof(float));
    memcpy(cperio->array[1].data,RotationAngle,base->phys_dim*sizeof(float));
    memcpy(cperio->array[2].data,Translation,base->phys_dim*sizeof(float));
    strcpy(cperio->array[0].name,"RotationCenter");
    strcpy(cperio->array[1].name,"RotationAngle");
    strcpy(cperio->array[2].name,"Translation");

    /* Create GridConnectivityProperty_t node if it doesn't yet exist */
    if (cprop->id==0) {
        if (cgi_new_node(conn->id, "GridConnectivityProperty",
            "GridConnectivityProperty_t", &cprop->id, "MT", 0, 0, 0)) return CG_ERROR;
    }
    /* Create the Periodic_t Node */
    if (cgi_new_node(cprop->id, "Periodic", "Periodic_t",
        &cperio->id, "MT", 0, 0, 0)) return CG_ERROR;

    /* Periodic_t/DataArray_t: RotationCenter, RotationAngle, Translation */
    for (n=0; n<cperio->narrays; n++)
        if (cgi_write_array(cperio->id, &cperio->array[n])) return CG_ERROR;
    return CG_OK;
}

int cg_1to1_write(int file_number, int B, int Z, char const * connectname, char const * donorname,
          int const * range, int const * donor_range, int const * transform, int *I) {
    cgns_zone *zone;
    cgns_zconn *zconn;
    cgns_1to1 *one21 = NULL;
    int index, i, j, index_dim, length;
    double T_id;

     /* verify input */
    if (cgi_check_strlen(connectname)) return CG_ERROR;
    if (cgi_check_strlen(donorname)) return CG_ERROR;

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address of zone */
    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Allocate ZoneGridConnectivity data struct. if not already created */
    if (zone->zconn == 0) {
        zone->zconn = CGNS_NEW(cgns_zconn, 1);
        zconn = zone->zconn;
        strcpy(zconn->name,"ZoneGridConnectivity");

     /* initialize new ZoneGridConnectivity data structure */
        zconn->id = 0;
        zconn->link= 0;
        zconn->ndescr = 0;
        zconn->n1to1 = 0;
        zconn->nconns = 0;
        zconn->nholes = 0;
        zconn->nuser_data= 0;

    } else zconn = zone->zconn;

     /* verify input */
    index_dim = zone->index_dim;
    for (i=0; i<index_dim; i++) {   /* can't check donorrange because it may not yet be written */
        if (range[i]<=0 || range[i+index_dim]>zone->nijk[i]) {
            cgi_error("Invalid input range:  %d->%d",range[i], range[i+index_dim]);
            return CG_ERROR;
        }
        if (abs(transform[i])<0 || abs(transform[i])>index_dim) {
            cgi_error("Invalid transformation index: %d.  The indices must all be between 1 and %d",i, index_dim);
            return CG_ERROR;
        }
        if (abs(transform[i])>0) {
            j = abs(transform[i])-1;
            if (abs(range[i+index_dim]-range[i])-abs(donor_range[j+index_dim]-donor_range[j]) != 0) {
                cgi_error("Invalid input:  range = %d->%d and donor_range = %d->%d",
                range[i], range[i+index_dim], donor_range[j], donor_range[j+index_dim]);
                return CG_ERROR;
            }
        }
    }

     /* Overwrite a GridConnectivity1to1_t Node: */
    for (index=0; index<zconn->n1to1; index++) {
        if (strcmp(connectname, zconn->one21[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",connectname);
                return CG_ERROR;
            }

             /* overwrite an existing GridConnectivity1to1_t Node */
             /* delete the existing GridConnectivity1to1_t Node from file */
            if (cgi_delete_node(zconn->id, zconn->one21[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            one21 = &(zconn->one21[index]);
             /* free memory */
            cgi_free_1to1(one21);
            break;
        }
    }
     /* ... or add a GridConnectivity1to1_t Node: */
    if (index==zconn->n1to1) {
        if (zconn->n1to1 == 0) {
            zconn->one21 = CGNS_NEW(cgns_1to1, zconn->n1to1+1);
        } else {
            zconn->one21 = CGNS_RENEW(cgns_1to1, zconn->n1to1+1, zconn->one21);
        }
        one21 = &(zconn->one21[zconn->n1to1]);
        zconn->n1to1++;
    }
    (*I) = index+1;

     /* allocate memory */
    if ((one21->transform = (int *)malloc(index_dim*sizeof(int)))==NULL) {
        cgi_error("Error allocating memory in cg_1to1_write");
        return CG_ERROR;
    }

     /* write 1to1 info to internal memory */
    strcpy(one21->name,connectname);
    one21->ptset.type = PointRange;
    strcpy(one21->ptset.data_type,"I4");
    one21->ptset.npts = 2;

     /* ... donor: */
    strcpy(one21->donor,donorname);
    one21->dptset.type = PointRangeDonor;
    strcpy(one21->dptset.data_type,"I4");
    one21->dptset.npts = 2;

     /* ... transform: */
    memcpy((void *)one21->transform, (void *)transform, index_dim*sizeof(int));

     /* ... initialize: */
    one21->ptset.id=0;
    one21->ptset.link=0;
    one21->dptset.id=0;
    one21->dptset.link=0;
    one21->id = 0;
    one21->link=0;
    one21->ndescr=0;
    one21->ordinal=0;
    one21->nuser_data=0;
    one21->cprop = 0;

    /* Create node ZoneGridConnectivity_t node, if not yet created */
    if (zconn->id==0) {
        if (cgi_new_node(zone->id, "ZoneGridConnectivity", "ZoneGridConnectivity_t",
            &zconn->id, "MT", 0, 0, 0)) return CG_ERROR;
    }

    /* Create the node */
    length = strlen(one21->donor);
    if (cgi_new_node(zconn->id, one21->name, "GridConnectivity1to1_t",
        &one21->id, "C1", 1, &length, one21->donor)) return CG_ERROR;

   /* Create transform node */
   if (cgi_new_node(one21->id, "Transform", "\"int[IndexDimension]\"", &T_id,
       "I4", 1, &index_dim, (void *)one21->transform)) return CG_ERROR;

   /* Create RECEIVER Point Set node on disk */
    if (cgi_write_ptset(one21->id, "PointRange", &one21->ptset, index_dim,
        (void *)range)) return CG_ERROR;

     /* Create DONOR Point Set node on disk */
    if (cgi_write_ptset(one21->id, "PointRangeDonor", &one21->dptset, index_dim,
        (void *)donor_range)) return CG_ERROR;

    return CG_OK;
}

int cg_1to1_average_write(int file_number, int B, int Z, int I,
    AverageInterfaceType_t AverageInterfaceType)
{
    cgns_cprop *cprop;
    cgns_caverage *caverage;
    cgns_1to1 *one21;
    int length;
    double dummy_id;

     /* verify input */
    if (AverageInterfaceType<0 || AverageInterfaceType>=NofValidAverageInterfaceTypes) {
        cgi_error("Invalid AverageInterfaceType:  %d",AverageInterfaceType);
        return CG_ERROR;
    }

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address of GridConnectivity_t node */
    one21 = cgi_get_1to1(cg, B, Z, I);
    if (one21 == 0) return CG_ERROR;

     /* Allocate GridConnectivityProperty_t data struct. if not already created */
    if (one21->cprop == 0) {
        one21->cprop = CGNS_NEW(cgns_cprop, 1);
        cprop = one21->cprop;
        strcpy(cprop->name,"GridConnectivityProperty");
        cprop->id=0;
        cprop->link=0;
        cprop->ndescr=0;
        cprop->nuser_data=0;
        cprop->cperio=0;
        cprop->caverage=0;
    } else cprop = one21->cprop;

     /* Overwrite an AverageInterface_t Node: */
    if (cprop->caverage) {
     /* in CG_MODE_WRITE, children names must be unique */
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("AverageInterface_t already defined under GridConnectivityProperty_t");
            return CG_ERROR;
        }

     /* overwrite an existing AverageInterface_t Node */
        else if (cg->mode==CG_MODE_MODIFY) {
         /* delete the existing AverageInterface_t Node from file */
            if (cgi_delete_node(cprop->id, cprop->caverage->id))
                return CG_ERROR;
            cgi_free_caverage(cprop->caverage);
        }
    } else cprop->caverage = CGNS_NEW(cgns_caverage, 1);
    caverage = cprop->caverage;

     /* write caverage info to internal memory */
    caverage->type = AverageInterfaceType;

     /* initialize other fields */
    strcpy(caverage->name,"AverageInterface");
    caverage->id = 0;
    caverage->link = 0;
    caverage->ndescr = 0;
    caverage->nuser_data = 0;

    /* Create GridConnectivityProperty_t node if it doesn't yet exist */
    if (cprop->id==0) {
        if (cgi_new_node(one21->id, "GridConnectivityProperty",
            "GridConnectivityProperty_t", &cprop->id, "MT", 0, 0, 0))
            return CG_ERROR;
    }
    /* Create the AverageInterface_t Node */
    if (cgi_new_node(cprop->id, "AverageInterface", "AverageInterface_t",
        &caverage->id, "MT", 0, 0, 0)) return CG_ERROR;

    /* AverageInterface_t/AverageInterfaceType_t */
    length = strlen(AverageInterfaceTypeName[caverage->type]);
    if (cgi_new_node(caverage->id, "AverageInterfaceType", "AverageInterfaceType_t", &dummy_id,
        "C1", 1, &length, (void *)AverageInterfaceTypeName[caverage->type]))
        return CG_ERROR;
    return CG_OK;
}

int cg_1to1_periodic_write(int file_number, int B, int Z, int I,
    float const *RotationCenter, float const *RotationAngle,
    float const *Translation)
{
    cgns_base *base;
    cgns_1to1 *one21;
    cgns_cprop *cprop;
    cgns_cperio *cperio;
    int n;

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for base */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* get memory address of GridConnectivity1to1_t node */
    one21 = cgi_get_1to1(cg, B, Z, I);
    if (one21 == 0) return CG_ERROR;

     /* Allocate GridConnectivityProperty_t data struct. if not already created */
    if (one21->cprop == 0) {
        one21->cprop = CGNS_NEW(cgns_cprop, 1);
        cprop = one21->cprop;
        strcpy(cprop->name,"GridConnectivityProperty");
        cprop->id=0;
        cprop->link=0;
        cprop->ndescr=0;
        cprop->nuser_data=0;
        cprop->cperio=0;
        cprop->caverage=0;
    } else cprop = one21->cprop;

     /* Overwrite a Periodic_t Node: */
    if (cprop->cperio) {
     /* in CG_MODE_WRITE, children names must be unique */
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("Periodic_t already defined under GridConnectivityProperty_t.");
            return CG_ERROR;
        }

     /* overwrite an existing Periodic_t Node */
        else if (cg->mode==CG_MODE_MODIFY) {
         /* delete the existing Periodic_t Node from file */
            if (cgi_delete_node(cprop->id, cprop->cperio->id))
                return CG_ERROR;
            cgi_free_cperio(cprop->cperio);
        }
    } else cprop->cperio = CGNS_NEW(cgns_cperio, 1);
    cperio = cprop->cperio;

     /* write cperio info to internal memory */
    strcpy(cperio->name,"Periodic");
    cperio->id = 0;
    cperio->link = 0;
    cperio->ndescr = 0;
    cperio->nuser_data = 0;
    cperio->narrays = 3;
    cperio->data_class=DataClassNull;
    cperio->units=0;

     /* Create DataArray_t RotationCenter, RotationAngle, & Translation under Periodic_t */
    cperio->array = CGNS_NEW(cgns_array, 3);

    for (n=0; n<cperio->narrays; n++) {
        strcpy(cperio->array[n].data_type, "R4");
        cperio->array[n].data = (void *)malloc(base->phys_dim*sizeof(float));
        if (cperio->array[n].data == NULL) {
            cgi_error("Error allocating cperio->array[n].data");
            return CG_ERROR;
        }
        cperio->array[n].data_dim=1;
        cperio->array[n].dim_vals[0]=base->phys_dim;
        cperio->array[n].id=0;
        cperio->array[n].link=0;
        cperio->array[n].ndescr=0;
        cperio->array[n].data_class=DataClassNull;
        cperio->array[n].units=0;
        cperio->array[n].exponents=0;
        cperio->array[n].convert=0;
    }
    memcpy(cperio->array[0].data,RotationCenter,base->phys_dim*sizeof(float));
    memcpy(cperio->array[1].data,RotationAngle,base->phys_dim*sizeof(float));
    memcpy(cperio->array[2].data,Translation,base->phys_dim*sizeof(float));
    strcpy(cperio->array[0].name,"RotationCenter");
    strcpy(cperio->array[1].name,"RotationAngle");
    strcpy(cperio->array[2].name,"Translation");

    /* Create GridConnectivityProperty_t node if it doesn't yet exist */
    if (cprop->id==0) {
        if (cgi_new_node(one21->id, "GridConnectivityProperty",
            "GridConnectivityProperty_t", &cprop->id, "MT", 0, 0, 0))
            return CG_ERROR;
    }
    /* Create the Periodic_t Node */
    if (cgi_new_node(cprop->id, "Periodic", "Periodic_t",
        &cperio->id, "MT", 0, 0, 0)) return CG_ERROR;

    /* Periodic_t/DataArray_t: RotationCenter, RotationAngle, Translation */
    for (n=0; n<cperio->narrays; n++)
        if (cgi_write_array(cperio->id, &cperio->array[n])) return CG_ERROR;
    return CG_OK;
}

int cg_boco_write(int file_number, int B, int Z, char const * boconame, BCType_t bocotype,
          PointSetType_t ptset_type, int npnts, int const * pnts, int *BC) {
    cgns_zone *zone;
    cgns_zboco *zboco;
    cgns_boco *boco = NULL;
    int index, i, index_dim, length;

     /* verify input */
#if 0  /* re-enable ElementList/ElementRange */
    if (ptset_type!= PointList && ptset_type!=PointRange) {
        cgi_error("Invalid point set type: %d...?",ptset_type);
        return CG_ERROR;
    }
    if (!(ptset_type==PointRange && npnts==2) && !(ptset_type==PointList && npnts>0)) {
        cgi_error("Invalid input:  npoint=%d, point set type=%s",
               npnts, PointSetTypeName[ptset_type]);
        return CG_ERROR;
    }
#else
    if (ptset_type == PointList || ptset_type == ElementList) {
        if (npnts <= 0) {
            cgi_error("Invalid input:  npoint=%d, point set type=%s",
                   npnts, PointSetTypeName[ptset_type]);
            return CG_ERROR;
        }
    } else if (ptset_type == PointRange || ptset_type == ElementRange) {
        if (npnts != 2) {
            cgi_error("Invalid input:  npoint=%d, point set type=%s",
                   npnts, PointSetTypeName[ptset_type]);
            return CG_ERROR;
        }
    } else {
        cgi_error("Invalid point set type: %d...?",ptset_type);
        return CG_ERROR;
    }
#endif
    if (bocotype<0 || bocotype>=NofValidBCTypes) {
        cgi_error("Invalid BCType:  %d",bocotype);
        return CG_ERROR;
    }
    if (cgi_check_strlen(boconame)) return CG_ERROR;

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address of zone */
    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Allocate ZoneBC data struct. if not already created */
    if (zone->zboco == 0) {
        zone->zboco = CGNS_NEW(cgns_zboco, 1);
        zboco = zone->zboco;
        strcpy(zboco->name,"ZoneBC");
        zboco->id=0;
        zboco->link=0;
        zboco->ndescr=0;
        zboco->nbocos=0;
        zboco->state=0;
        zboco->data_class=DataClassNull;
        zboco->units=0;
        zboco->nuser_data=0;
    } else zboco = zone->zboco;

     /* verify input */
    index_dim = zone->index_dim;
#if 0
    if (ptset_type==PointRange) {
        for (i=0; i<index_dim; i++) {
            if (pnts[i]<=0 || pnts[i+index_dim]>zone->nijk[i]) {
                cgi_error("Invalid input range:  %d->%d",pnts[i], pnts[i+index_dim]);
                return CG_ERROR;
            }
        }
    }
#endif

     /* Overwrite a BC_t Node: */
    for (index=0; index<zboco->nbocos; index++) {
        if (strcmp(boconame, zboco->boco[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",boconame);
                return CG_ERROR;
            }

             /* overwrite an existing BC_t Node */
             /* delete the existing BC_t Node from file */
            if (cgi_delete_node(zboco->id, zboco->boco[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            boco = &(zboco->boco[index]);
            cgi_free_boco(boco);
            break;
        }
    }
     /* ... or add a BC_t Node: */
    if (index==zboco->nbocos) {
        if (zboco->nbocos == 0) {
            zboco->boco = CGNS_NEW(cgns_boco, zboco->nbocos+1);
        } else {
            zboco->boco = CGNS_RENEW(cgns_boco, zboco->nbocos+1, zboco->boco);
        }
        boco = &(zboco->boco[zboco->nbocos]);
        zboco->nbocos++;
    }
    (*BC) = index+1;

     /* write boco info to internal memory */
    strcpy(boco->name,boconame);
    boco->type = bocotype;
    boco->ptset = CGNS_NEW(cgns_ptset,1);
    boco->ptset->type = ptset_type;
    strcpy(boco->ptset->data_type,"I4");
    boco->ptset->npts = npnts;

     /* Record the number of nodes or elements in the point set */
#if 0  /* re-enable ElementList/ElementRange */
    if (ptset_type==PointList) boco->ptset->size_of_patch=npnts;
    else if (ptset_type==PointRange) {
        boco->ptset->size_of_patch = 1;
        for (i=0; i<index_dim; i++)
            boco->ptset->size_of_patch = boco->ptset->size_of_patch * (pnts[i+index_dim]-pnts[i]+1);
    }
#else
    if (ptset_type == PointList || ptset_type == ElementList)
        boco->ptset->size_of_patch=npnts;
    else {
        boco->ptset->size_of_patch = 1;
        for (i=0; i<index_dim; i++)
            boco->ptset->size_of_patch = boco->ptset->size_of_patch * (pnts[i+index_dim]-pnts[i]+1);
    }
#endif

     /* ... initialize: */
    boco->normal = 0;
    boco->Nindex = 0;
    boco->id=0;
    boco->link = 0;
    boco->location = Vertex;
    boco->ptset->id = 0;
    boco->ptset->link = 0;
    boco->ndescr = 0;
    boco->ndataset = 0;
    boco->state = 0;
    boco->data_class = DataClassNull;
    boco->units = 0;
    boco->ordinal = 0;
    boco->family_name[0]='\0';
    boco->nuser_data = 0;
    boco->bprop = 0;

    /* Create ZoneBC_t node if it doesn't yet exist */
    if (zboco->id==0) {
        if (cgi_new_node(zone->id, "ZoneBC", "ZoneBC_t",
            &zboco->id, "MT", 0, 0, 0)) return CG_ERROR;
    }
    /* Create the BC_t Node */
    length = strlen(BCTypeName[boco->type]);
    if (cgi_new_node(zboco->id, boco->name, "BC_t", &boco->id, "C1", 1,
        &length, BCTypeName[boco->type])) return CG_ERROR;

     /* Save Point-Set on Disk */
    if (npnts>0) {
        char_33 PointSetName;
        strcpy(PointSetName, PointSetTypeName[boco->ptset->type]);
        if (cgi_write_ptset(boco->id, PointSetName, boco->ptset, index_dim,
            (void *)pnts)) return CG_ERROR;
    }

    return CG_OK;
}

int cg_boco_normal_write(int file_number, int B, int Z, int BC, int const * NormalIndex,
         int NormalListFlag,  DataType_t NormalDataType, void const * NormalList) {
    cgns_boco *boco;
    int npnts, n, index_dim, phys_dim;

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    boco = cgi_get_boco(cg, B, Z, BC);
    if (boco==0) return CG_ERROR;
    npnts = boco->ptset->size_of_patch;

    phys_dim=cg->base[B-1].phys_dim;

    if (NormalListFlag && npnts) {
        cgns_array *normal;

        if (boco->normal) {
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("InwardNormalList is already defined under BC_t '%s'",
                       boco->name);
                return CG_ERROR;
            }
            if (cgi_delete_node(boco->id, boco->normal->id))
                return CG_ERROR;
            cgi_free_array(boco->normal);
        } else
            boco->normal = CGNS_NEW(cgns_array, 1);
        normal = boco->normal;

        strcpy(normal->data_type, cgi_adf_datatype(NormalDataType));
        normal->data = (void *)malloc(npnts*phys_dim*size_of(normal->data_type));
        if (normal->data == NULL) {
            cgi_error("Error allocating normal->data");
            return CG_ERROR;
        }
        memcpy(normal->data, NormalList, npnts*phys_dim*size_of(normal->data_type));
        strcpy(normal->name, "InwardNormalList");
        normal->data_dim =2;
        normal->dim_vals[0]=phys_dim;
        normal->dim_vals[1]=npnts;

     /* initialize other fields */
        normal->id=0;
        normal->link=0;
        normal->ndescr=0;
        normal->data_class=DataClassNull;
        normal->units=0;
        normal->exponents=0;
        normal->convert=0;

        if (cgi_new_node(boco->id, "InwardNormalList", "IndexArray_t",
            &normal->id, normal->data_type, 2, normal->dim_vals,
            (void *)normal->data)) return CG_ERROR;
    }
    if (boco->Nindex) {
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("InwardNormalIndex is already defined under BC_t '%s'",
                   boco->name);
            return CG_ERROR;
        } else {
            if (cgi_delete_node(boco->id, boco->index_id))
                return CG_ERROR;
            free(boco->Nindex);
            boco->Nindex = 0;
        }
    }
    if (NormalIndex && cg->base[B-1].zone[Z-1].type == Structured) {
        index_dim=cg->base[B-1].zone[Z-1].index_dim;
        boco->Nindex = CGNS_NEW(int, index_dim);
        for (n=0; n<index_dim; n++)
            boco->Nindex[n]=NormalIndex[n];

        if (cgi_new_node(boco->id, "InwardNormalIndex", "\"int[IndexDimension]\"",
            &boco->index_id, "I4", 1, &index_dim, (void *)NormalIndex))
            return CG_ERROR;
    }
    return CG_OK;
}

int cg_dataset_write(int file_number, int B, int Z, int BC, char const * name,
    BCType_t BCType, int *Dset) {

    cgns_boco *boco;
    cgns_dataset *dataset = NULL;
    int index, length;

     /* verify input */
    if (BCType<0 || BCType>=NofValidBCTypes) {
        cgi_error("Invalid BCType:  %d",BCType);
        return CG_ERROR;
    }
    if (cgi_check_strlen(name)) return CG_ERROR;

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    boco = cgi_get_boco(cg, B, Z, BC);
    if (boco==0) return CG_ERROR;

     /* Overwrite a BCDataSet_t node : */
    for (index=0; index<boco->ndataset; index++) {
        if (strcmp(name, boco->dataset[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",name);
                return CG_ERROR;
            }

             /* overwrite an existing solution */
             /* delete the existing dataset from file */
            if (cgi_delete_node(boco->id, boco->dataset[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            dataset = &(boco->dataset[index]);
             /* free memory */
            cgi_free_dataset(dataset);
            break;
        }
    }
     /* ... or add a BCDataSet_t Node: */
    if (index==boco->ndataset) {
        if (boco->ndataset == 0)
            boco->dataset = CGNS_NEW(cgns_dataset, boco->ndataset+1);
        else
            boco->dataset= CGNS_RENEW(cgns_dataset, boco->ndataset+1, boco->dataset);
        dataset= &boco->dataset[boco->ndataset];
        boco->ndataset++;
    }
    (*Dset) = index+1;

     /* save data in memory */
    dataset->type = BCType;
    strcpy(dataset->name, name);

     /* initialize other fields */
    dataset->id = 0;
    dataset->link=0;
    dataset->ndescr=0;
    dataset->dirichlet=0;
    dataset->neumann=0;
    dataset->state=0;
    dataset->data_class=DataClassNull;
    dataset->units=0;
    dataset->nuser_data=0;
    /* initialize dataset->ptset and dataset->location with default
     * values
    */
    dataset->location = Vertex;
    dataset->ptset = 0;

     /* save data in file */
    length = strlen(BCTypeName[dataset->type]);
    if (cgi_new_node(boco->id, dataset->name, "BCDataSet_t", &dataset->id,
        "C1", 1, &length, (void *)BCTypeName[dataset->type])) return CG_ERROR;
    return CG_OK;
}

int cg_bcdata_write(int file_number, int B, int Z, int BC, int Dset,
    BCDataType_t BCDataType) {

    cgns_dataset *dataset;
    cgns_bcdata *bcdata;

     /* verify input */
    if (BCDataType<0 || BCDataType>=NofValidBCDataTypes) {
        cgi_error("BCDataType %d not valid",BCDataType);
        return CG_ERROR;
    }

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    dataset = cgi_get_dataset(cg, B, Z, BC, Dset);
    if (dataset==0) return CG_ERROR;

    if (BCDataType==Dirichlet) {
        if (dataset->dirichlet) {
            if (cg->mode == CG_MODE_WRITE) {
                cgi_error("Dirichlet data already defined under BCDataSet_t '%s'",
                       dataset->name);
                return CG_ERROR;
            }
            if (cgi_delete_node(dataset->id, dataset->dirichlet->id))
                return CG_ERROR;
            cgi_free_bcdata(dataset->dirichlet);
        } else
            dataset->dirichlet = CGNS_NEW(cgns_bcdata,1);
        strcpy(dataset->dirichlet->name, "DirichletData");
        bcdata = dataset->dirichlet;
    } else {
        if (dataset->neumann) {
            if (cg->mode == CG_MODE_WRITE) {
                cgi_error("Neumann data already defined under BCDataSet_t '%s'",
                       dataset->name);
                return CG_ERROR;
            }
            if (cgi_delete_node(dataset->id, dataset->neumann->id))
                return CG_ERROR;
            cgi_free_bcdata(dataset->neumann);
        } else
            dataset->neumann = CGNS_NEW(cgns_bcdata,1);
        strcpy(dataset->neumann->name, "NeumannData");
        bcdata = dataset->neumann;
    }

     /* initialize other fields */
    bcdata->id = 0;
    bcdata->link=0;
    bcdata->ndescr=0;
    bcdata->data_class=DataClassNull;
    bcdata->units=0;
    bcdata->narrays=0;
    bcdata->nuser_data=0;

    if (cgi_new_node(dataset->id, bcdata->name, "BCData_t", &bcdata->id,
        "MT", 0, 0, 0)) return CG_ERROR;
    return CG_OK;
}

int cg_bc_wallfunction_write(int file_number, int B, int Z, int BC,
    WallFunctionType_t WallFunctionType) {

    cgns_bprop *bprop;
    cgns_bcwall *bcwall;
    cgns_boco *boco;
    int length;
    double dummy_id;

     /* verify input */
    if (WallFunctionType<0 || WallFunctionType>=NofValidWallFunctionTypes) {
        cgi_error("Invalid WallFunctionType:  %d",WallFunctionType);
        return CG_ERROR;
    }

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address of BC_t node */
    boco = cgi_get_boco(cg, B, Z, BC);
    if (boco==0) return CG_ERROR;

     /* Allocate BCProperty_t data struct. if not already created */
    if (boco->bprop == 0) {
        boco->bprop = CGNS_NEW(cgns_bprop, 1);
        bprop = boco->bprop;
        strcpy(bprop->name,"BCProperty");
        bprop->id=0;
        bprop->link=0;
        bprop->ndescr=0;
        bprop->nuser_data=0;
        bprop->bcarea=0;
        bprop->bcwall=0;
    } else bprop = boco->bprop;

     /* Overwrite a WallFunction_t Node: */
    if (bprop->bcwall) {
     /* in CG_MODE_WRITE, children names must be unique */
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("WallFunction_t already defined under BCProperty_t.");
            return CG_ERROR;
        }

     /* overwrite an existing WallFunction_t Node */
         /* delete the existing WallFunction_t Node from file */
        if (cgi_delete_node(bprop->id, bprop->bcwall->id))
            return CG_ERROR;
        cgi_free_bcwall(bprop->bcwall);
    } else
        bprop->bcwall = CGNS_NEW(cgns_bcwall, 1);
    bcwall = bprop->bcwall;

     /* write bcwall info to internal memory */
    bcwall->type = WallFunctionType;

     /* initialize other fields */
    strcpy(bcwall->name,"WallFunction");
    bcwall->id = 0;
    bcwall->link = 0;
    bcwall->ndescr = 0;
    bcwall->nuser_data = 0;

    /* Create BCProperty_t node if it doesn't yet exist */
    if (bprop->id==0) {
        if (cgi_new_node(boco->id, "BCProperty", "BCProperty_t",
             &bprop->id, "MT", 0, 0, 0)) return CG_ERROR;
    }

    /* Create the WallFunction_t Node */
    if (cgi_new_node(bprop->id, "WallFunction", "WallFunction_t",
        &bcwall->id, "MT", 0, 0, 0)) return CG_ERROR;

    /* WallFunction_t/WallFunctionType_t */
    length = strlen(WallFunctionTypeName[bcwall->type]);
    if (cgi_new_node(bcwall->id, "WallFunctionType", "WallFunctionType_t", &dummy_id,
        "C1", 1, &length, (void *)WallFunctionTypeName[bcwall->type])) return CG_ERROR;

    return CG_OK;
}


int cg_bc_area_write(int file_number, int B, int Z, int BC,
    AreaType_t AreaType, float SurfaceArea, char const *RegionName) {
    cgns_boco *boco;
    cgns_bprop *bprop;
    cgns_bcarea *bcarea;
    int n, len;
    char *RegionName32;
    double dummy_id;

     /* verify input */
    if (AreaType<0 || AreaType>=NofValidAreaTypes) {
        cgi_error("Invalid AreaType:  %d",AreaType);
        return CG_ERROR;
    }

     /* get memory address of file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address of BC_t node */
    boco = cgi_get_boco(cg, B, Z, BC);
    if (boco==0) return CG_ERROR;

     /* Allocate BCProperty_t data struct. if not already created */
    if (boco->bprop == 0) {
        boco->bprop = CGNS_NEW(cgns_bprop, 1);
        bprop = boco->bprop;
        strcpy(bprop->name,"BCProperty");
        bprop->id=0;
        bprop->link=0;
        bprop->ndescr=0;
        bprop->nuser_data=0;
        bprop->bcarea=0;
        bprop->bcwall=0;
    } else bprop = boco->bprop;

     /* Overwrite a Area_t Node: */
    if (bprop->bcarea) {
     /* in CG_MODE_WRITE, children names must be unique */
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("Area_t already defined under BCProperty_t.");
            return CG_ERROR;
        }

     /* overwrite an existing Area_t Node */
         /* delete the existing Area_t Node from file */
        if (cgi_delete_node(bprop->id, bprop->bcarea->id))
            return CG_ERROR;
        cgi_free_bcarea(bprop->bcarea);
    } else
        bprop->bcarea = CGNS_NEW(cgns_bcarea, 1);
    bcarea = bprop->bcarea;

     /* write bcarea info to internal memory */
    bcarea->type = AreaType;
     /* initialize other fields */
    strcpy(bcarea->name,"Area");
    bcarea->id = 0;
    bcarea->link = 0;
    bcarea->ndescr = 0;
    bcarea->nuser_data = 0;
    bcarea->narrays = 2;

     /* Create DataArray_t SurfaceArea & RegionName under Area_t */
    bcarea->array = CGNS_NEW(cgns_array, 2);

    strcpy(bcarea->array[0].data_type, "R4");
    bcarea->array[0].data = (void *)malloc(sizeof(float));
    if (bcarea->array[0].data == NULL) {
        cgi_error("Error allocating bcarea->array[0].data");
        return CG_ERROR;
    }
    memcpy(bcarea->array[0].data, &SurfaceArea, sizeof(float));
    /* *((float *)bcarea->array[0].data) = SurfaceArea; */
    strcpy(bcarea->array[0].name, "SurfaceArea");
    bcarea->array[0].data_dim=1;
    bcarea->array[0].dim_vals[0]=1;

    strcpy(bcarea->array[1].data_type, "C1");
    bcarea->array[1].data = (void *)malloc(32*sizeof(char));
    if (bcarea->array[1].data == NULL) {
        cgi_error("Error allocating bcarea->array[1].data");
        return CG_ERROR;
    }

     /* check length of RegionName and fill in with blanks */
    RegionName32 = (char *)bcarea->array[1].data;
    len = strlen(RegionName);
    for (n=0; n<len; n++) RegionName32[n]=RegionName[n];
    for (n=len; n<32; n++) RegionName32[n]=' ';

    strcpy(bcarea->array[1].name, "RegionName");
    bcarea->array[1].data_dim=1;
    bcarea->array[1].dim_vals[0]=32;

     /* initialize other fields of bcarea->array[n] */
    for (n=0; n<bcarea->narrays; n++) {
        bcarea->array[n].id=0;
        bcarea->array[n].link=0;
        bcarea->array[n].ndescr=0;
        bcarea->array[n].data_class=DataClassNull;
        bcarea->array[n].units=0;
        bcarea->array[n].exponents=0;
        bcarea->array[n].convert=0;
    }

    /* Create BCProperty_t node if it doesn't yet exist */
    if (bprop->id==0) {
        if (cgi_new_node(boco->id, "BCProperty", "BCProperty_t",
             &bprop->id, "MT", 0, 0, 0)) return CG_ERROR;
    }
    /* Create the Area_t Node */
    if (cgi_new_node(bprop->id, "Area", "Area_t",
        &bcarea->id, "MT", 0, 0, 0)) return CG_ERROR;

    /* Area_t/AreaType_t */
    len = strlen(AreaTypeName[bcarea->type]);
    if (cgi_new_node(bcarea->id, "AreaType", "AreaType_t", &dummy_id,
        "C1", 1, &len, (void *)AreaTypeName[bcarea->type])) return CG_ERROR;

    /* Area_t/DataArray_t: SurfaceArea & RegionName */
    for (n=0; n<bcarea->narrays; n++)
        if (cgi_write_array(bcarea->id, &bcarea->array[n])) return CG_ERROR;

    return CG_OK;
}

int cg_rigid_motion_write(int file_number, int B, int Z, char const * rmotionname,
    RigidGridMotionType_t type, int *R) {
    cgns_zone *zone;
    cgns_rmotion *rmotion = NULL;
    int index, length;

     /* verify input */
    if (cgi_check_strlen(rmotionname)) return CG_ERROR;

    if (type<0 || type >= NofValidRigidGridMotionTypes) {
        cgi_error("Invalid input:  RigidGridMotionType=%d ?",type);
        return CG_ERROR;
    }

     /* get memory address for RigidGridMotion_t node */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Overwrite a RigidGridMotion_t Node: */
    for (index=0; index<zone->nrmotions; index++) {
        if (strcmp(rmotionname, zone->rmotion[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",rmotionname);
                return CG_ERROR;
            }

             /* overwrite an existing rmotion */
             /* delete the existing rmotion from file */
            if (cgi_delete_node(zone->id, zone->rmotion[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            rmotion = &(zone->rmotion[index]);
             /* free memory */
            cgi_free_rmotion(rmotion);
            break;
        }
    }
     /* ... or add a new RigidGridMotion_t Node: */
    if (index==zone->nrmotions) {
        if (zone->nrmotions == 0) {
            zone->rmotion = CGNS_NEW(cgns_rmotion, 1);
        } else {
            zone->rmotion = CGNS_RENEW(cgns_rmotion, zone->nrmotions+1, zone->rmotion);
        }
        rmotion = &(zone->rmotion[zone->nrmotions]);
        zone->nrmotions++;
    }
    (*R) = index+1;

     /* save data for cgns_rmotion *rmotion */
    strcpy(rmotion->name,rmotionname);
    rmotion->type = type;

     /* initialize other members of rmotion */
    rmotion->id=0;
    rmotion->link=0;
    rmotion->ndescr=0;
    rmotion->data_class=DataClassNull;
    rmotion->units=0;
    rmotion->narrays=0;
    rmotion->nuser_data=0;

     /* Create node RigidGridMotion_t */
    length = strlen(RigidGridMotionTypeName[rmotion->type]);
    if (cgi_new_node(zone->id, rmotion->name, "RigidGridMotion_t", &rmotion->id,
        "C1", 1, &length, (void *)RigidGridMotionTypeName[rmotion->type])) return CG_ERROR;

    return CG_OK;
}

int cg_arbitrary_motion_write(int file_number, int B, int Z, char const * amotionname,
    ArbitraryGridMotionType_t type, int *A) {
    cgns_zone *zone;
    cgns_amotion *amotion = NULL;
    int index, length;

     /* verify input */
    if (cgi_check_strlen(amotionname)) return CG_ERROR;

    if (type<0 || type >= NofValidArbitraryGridMotionTypes) {
        cgi_error("Invalid input:  ArbitraryGridMotionType=%d ?",type);
        return CG_ERROR;
    }

     /* get memory address for ArbitraryGridMotion_t node */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Overwrite a ArbitraryGridMotion_t Node: */
    for (index=0; index<zone->namotions; index++) {
        if (strcmp(amotionname, zone->amotion[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",amotionname);
                return CG_ERROR;
            }

             /* overwrite an existing amotion */
             /* delete the existing amotion from file */
            if (cgi_delete_node(zone->id, zone->amotion[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            amotion = &(zone->amotion[index]);
             /* free memory */
            cgi_free_amotion(amotion);
            break;
        }
    }
     /* ... or add a new ArbitraryGridMotion_t Node: */
    if (index==zone->namotions) {
        if (zone->namotions == 0) {
            zone->amotion = CGNS_NEW(cgns_amotion, 1);
        } else {
            zone->amotion = CGNS_RENEW(cgns_amotion, zone->namotions+1, zone->amotion);
        }
        amotion = &(zone->amotion[zone->namotions]);
        zone->namotions++;
    }
    (*A) = index+1;

     /* save data for cgns_amotion *amotion */
    strcpy(amotion->name,amotionname);
    amotion->type = type;

     /* initialize other members of amotion */
    amotion->id=0;
    amotion->link=0;
    amotion->ndescr=0;
    amotion->location=Vertex;
    amotion->rind_planes=0;
    amotion->narrays=0;
    amotion->data_class=DataClassNull;
    amotion->units=0;
    amotion->nuser_data=0;

     /* Create node ArbitraryGridMotion_t */
    length = strlen(ArbitraryGridMotionTypeName[amotion->type]);
    if (cgi_new_node(zone->id, amotion->name, "ArbitraryGridMotion_t", &amotion->id,
        "C1", 1, &length, (void *)ArbitraryGridMotionTypeName[amotion->type])) return CG_ERROR;
    return CG_OK;
}

int cg_simulation_type_write(int file_number, int B, SimulationType_t type) {
    cgns_base *base;
    int length;

     /* check input */
    if (type<0 || type >= NofValidSimulationTypes) {
        cgi_error("Invalid input:  SimulationType=%d ?",type);
        return CG_ERROR;
    }

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for CGNSBase_t node */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* write or overwrite SimulationType_t to Base */
    if (base->type) {
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("Simulation type already defined under CGNSBase_t '%s'",
                   base->name);
            return CG_ERROR;
        }
        if (cgi_delete_node(base->id, base->type_id))
            return CG_ERROR;
    }
    base->type = type;
    base->type_id = 0;

     /* save data in file */
    length = strlen(SimulationTypeName[type]);
    if (cgi_new_node(base->id, "SimulationType", "SimulationType_t", &base->type_id,
        "C1", 1, &length, (void *)SimulationTypeName[type])) return CG_ERROR;

    return CG_OK;
}

int cg_biter_write(int file_number, int B,  char const * bitername, int nsteps) {
    cgns_base *base;
    cgns_biter *biter;
    int length=1;

     /* verify input */
    if (nsteps<=0) {
        cgi_error("Invalid input:  The number of steps must be a positive integer!");
        return CG_ERROR;
    }

     /* get memory address for BaseIterativeData_t node */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* If BaseIterativeData_t already exist: */
    if (base->biter) {
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("Error:  BaseIterativeData_t already defined");
            return CG_ERROR;
        }

     /* overwrite an existing rmotion */
         /* delete the existing biter from file */
        if (cgi_delete_node(base->id, base->biter->id))
            return CG_ERROR;
         /* save the old in-memory address to overwrite */
        biter = base->biter;
         /* free memory */
        cgi_free_biter(biter);
     /* ... or add a new BaseIterativeData_t Node: */
    } else {
        base->biter = CGNS_NEW(cgns_biter, 1);
        biter = base->biter;
    }

     /* save data for cgns_biter *biter */
    strcpy(biter->name,bitername);
    biter->nsteps = nsteps;

     /* initialize other members of biter */
    biter->id=0;
    biter->link=0;
    biter->ndescr=0;
    biter->narrays=0;
    biter->data_class=DataClassNull;
    biter->units=0;
    biter->nuser_data=0;

     /* Create node BaseIterativeData_t */
    if (cgi_new_node(base->id, biter->name, "BaseIterativeData_t", &biter->id,
        "I4", 1, &length, (void *)&nsteps)) return CG_ERROR;
    return CG_OK;
}

int cg_ziter_write(int file_number, int B, int Z, char const * zitername) {
    cgns_zone *zone;
    cgns_ziter *ziter;

     /* verify input */
    if (cgi_check_strlen(zitername)) return CG_ERROR;

     /* get memory address for ZoneIterativeData_t node */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Overwrite the ZoneIterativeData_t Node: */
    if (zone->ziter) {
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("Error:  ZoneIterativeData_t already defined");
            return CG_ERROR;
        }
     /* overwrite an existing ZoneIterativeData_t Node */
         /* delete the existing ziter from file */
        if (cgi_delete_node(zone->id, zone->ziter->id))
            return CG_ERROR;
         /* save the old in-memory address to overwrite */
        ziter = zone->ziter;
         /* free memory */
        cgi_free_ziter(ziter);
    } else {
        zone->ziter = CGNS_NEW(cgns_ziter, 1);
        ziter = zone->ziter;
    }

     /* save data for cgns_ziter *ziter */
    strcpy(ziter->name,zitername);

     /* initialize other members of ziter */
    ziter->id=0;
    ziter->link=0;
    ziter->ndescr=0;
    ziter->narrays=0;
    ziter->data_class=DataClassNull;
    ziter->units=0;
    ziter->nuser_data=0;

     /* Create node ZoneIterativeData_t */
    if (cgi_new_node(zone->id, ziter->name, "ZoneIterativeData_t", &ziter->id,
        "MT", 0, 0, 0)) return CG_ERROR;
    return CG_OK;
}

int cg_gravity_write(int file_number, int B, float const *gravity_vector) {
    cgns_base *base;
    cgns_gravity *gravity;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for base */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

    if (base->gravity) {
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("Gravity is already defined under CGNSBase_t '%s'",
                   base->name);
            return CG_ERROR;
        }
        if (cgi_delete_node(base->id, base->gravity->id))
            return CG_ERROR;
        cgi_free_gravity(base->gravity);
    } else
        base->gravity = CGNS_NEW(cgns_gravity, 1);
    gravity = base->gravity;
    gravity->vector = CGNS_NEW(cgns_array, 1);

     /* Create DataArray_t GravityVector under gravity */
    strcpy(gravity->vector->data_type, "R4");
    gravity->vector->data = (void *)malloc(base->phys_dim*sizeof(float));
    if (gravity->vector->data == NULL) {
        cgi_error("Error allocating gravity->vector->data");
        return CG_ERROR;
    }
    memcpy(gravity->vector->data, gravity_vector, base->phys_dim*sizeof(float));
    strcpy(gravity->vector->name, "GravityVector");
    gravity->vector->data_dim=1;
    gravity->vector->dim_vals[0]=base->phys_dim;

     /* initialize other fields of gravity->vector */
    gravity->vector->id=0;
    gravity->vector->link=0;
    gravity->vector->ndescr=0;
    gravity->vector->data_class=DataClassNull;
    gravity->vector->units=0;
    gravity->vector->exponents=0;
    gravity->vector->convert=0;

     /* initialize other fields of gravity */
    strcpy(gravity->name, "Gravity");
    gravity->id=0;
    gravity->link=0;
    gravity->ndescr=0;
    gravity->data_class=DataClassNull;
    gravity->units=0;
    gravity->nuser_data=0;
    gravity->narrays=1;

     /* Write to disk */
    if (cgi_write_gravity(base->id, gravity)) return CG_ERROR;

    return CG_OK;
}

int cg_axisym_write(int file_number, int B, float const *ref_point, float const *axis) {
    int n;
    cgns_base *base;
    cgns_axisym *axisym;

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for base */
    base = cgi_get_base(cg, B);
    if (base==0) return CG_ERROR;

     /* Verify that this is a bidimensional base */
    if (base->phys_dim !=  2) {
        cgi_error("Error: Axisymmetry_t can only be specified for bidimensional bases");
        return CG_ERROR;
    }

    if (base->axisym) {
        if (cg->mode==CG_MODE_WRITE) {
            cgi_error("Axisymmetry is already defined under CGNSBase_t '%s'",
                   base->name);
            return CG_ERROR;
        }
        if (cgi_delete_node(base->id, base->axisym->id))
            return CG_ERROR;
        cgi_free_axisym(base->axisym);
    } else
        base->axisym = CGNS_NEW(cgns_axisym, 1);
    axisym = base->axisym;
    axisym->array = CGNS_NEW(cgns_array, 2);
    axisym->narrays=2;

     /* Create DataArray_t AxisymmetryReferencePoint & AxisymmetryAxisVector under axisym */
    for (n=0; n<axisym->narrays; n++) {
        strcpy(axisym->array[n].data_type, "R4");
        axisym->array[n].data = (void *)malloc(base->phys_dim*sizeof(float));
        if (axisym->array[n].data == NULL) {
            cgi_error("Error allocating axisym->array[n].data");
            return CG_ERROR;
        }
        axisym->array[n].data_dim=1;
        axisym->array[n].dim_vals[0]=base->phys_dim;
    }
    memcpy(axisym->array[0].data, ref_point, base->phys_dim*sizeof(float));
    memcpy(axisym->array[1].data, axis, base->phys_dim*sizeof(float));
    strcpy(axisym->array[0].name, "AxisymmetryReferencePoint");
    strcpy(axisym->array[1].name, "AxisymmetryAxisVector");

     /* initialize other fields of axisym->array[n] */
    for (n=0; n<axisym->narrays; n++) {
        axisym->array[n].id=0;
        axisym->array[n].link=0;
        axisym->array[n].ndescr=0;
        axisym->array[n].data_class=DataClassNull;
        axisym->array[n].units=0;
        axisym->array[n].exponents=0;
        axisym->array[n].convert=0;
    }

     /* initialize other fields of axisym */
    strcpy(axisym->name, "Axisymmetry");
    axisym->id=0;
    axisym->link=0;
    axisym->ndescr=0;
    axisym->data_class=DataClassNull;
    axisym->units=0;
    axisym->nuser_data=0;

     /* Write to disk */
    if (cgi_write_axisym(base->id, axisym)) return CG_ERROR;

    return CG_OK;
}

/*****************************************************************************\
 *           Write Multiple path nodes
\*****************************************************************************/

int cg_famname_write(char const * family_name) {
    char *famname;
    int ier=0, dim_vals;
    double posit_id, dummy_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    if (cgi_check_strlen(family_name)) return CG_ERROR;

    famname = cgi_famname_address(CG_MODE_WRITE, &ier);
    if (famname==0) return ier;

    strcpy(famname, family_name);

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    dim_vals = strlen(family_name);
    if (cgi_new_node(posit_id, "FamilyName", "FamilyName_t", &dummy_id,
        "C1", 1, &dim_vals, (void *)family_name)) return CG_ERROR;

    return CG_OK;
}


int cg_convergence_write(int iterations, char const * NormDefinitions) {
    cgns_converg *converg;
    int ier=0, dim_vals;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    converg = cgi_converg_address(CG_MODE_WRITE, &ier);
    if (converg==0) return ier;

     /* initialize new ConvergenceHistory_t node */
    converg->iterations=0;
    converg->id = 0;
    converg->link=0;
    converg->ndescr=0;
    converg->NormDefinitions = 0;
    converg->narrays=0;
    converg->data_class=DataClassNull;
    converg->units=0;
    converg->nuser_data=0;

     /* save data in memory */
    converg->iterations = iterations;
    if (NormDefinitions && strlen(NormDefinitions)) {
        converg->NormDefinitions=CGNS_NEW(cgns_descr, 1);
        converg->NormDefinitions->id=0;
        converg->NormDefinitions->link=0;
        converg->NormDefinitions->text = CGNS_NEW(char, strlen(NormDefinitions)+1);
        strcpy(converg->NormDefinitions->text, NormDefinitions);
        strcpy(converg->NormDefinitions->name, "NormDefinitions");
    }

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    dim_vals=1;
    if (cgi_new_node(posit_id, converg->name, "ConvergenceHistory_t", &converg->id,
        "I4", 1, &dim_vals, (void *)&converg->iterations)) return CG_ERROR;

     /* write NormDefinitions */
    if (converg->NormDefinitions &&
        cgi_write_descr(converg->id, converg->NormDefinitions)) return CG_ERROR;
    return CG_OK;
}

int cg_state_write(char const * StateDescription) {
    cgns_state *state;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    state = cgi_state_address(CG_MODE_WRITE, &ier);
    if (state==0) return ier;

     /* initialize state */
    strcpy(state->name,"ReferenceState");
    state->id = 0;
    state->link=0;
    state->ndescr=0;
    state->narrays=0;
    state->data_class=DataClassNull;
    state->units=0;
    state->StateDescription=0;
    state->nuser_data=0;

     /* Save data in memory */
    if (StateDescription && strlen(StateDescription)) {
        state->StateDescription=CGNS_NEW(cgns_descr, 1);
        state->StateDescription->id = 0;
        state->StateDescription->link = 0;
        state->StateDescription->text = CGNS_NEW(char, strlen(StateDescription)+1);
        strcpy(state->StateDescription->text, StateDescription);
        strcpy(state->StateDescription->name, "ReferenceStateDescription");
    }

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;

     /* ReferenceState_t */
    if (cgi_new_node(posit_id, state->name, "ReferenceState_t", &state->id,
        "MT", 0, 0, 0)) return CG_ERROR;

     /* ReferenceStateDescription */
    if (state->StateDescription &&
        cgi_write_descr(state->id, state->StateDescription)) return CG_ERROR;
    return CG_OK;
}

int cg_equationset_write(int EquationDimension) {
    cgns_equations *equations;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    equations=cgi_equations_address(CG_MODE_WRITE, &ier);
    if (equations==0) return ier;

     /* Save data */
    equations->equation_dim=EquationDimension;

     /* initialize other fields */
    strcpy(equations->name, "FlowEquationSet");
    equations->id=0;
    equations->link=0;
    equations->ndescr=0;
    equations->governing=0;
    equations->gas=0;
    equations->visc=0;
    equations->conduct=0;
    equations->closure=0;
    equations->turbulence=0;
    equations->relaxation=0;
    equations->chemkin=0;
    equations->data_class=DataClassNull;
    equations->units=0;
    equations->nuser_data=0;
    equations->elecfield = 0;
    equations->magnfield = 0;
    equations->emconduct = 0;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_equations(posit_id, equations)) return CG_ERROR;
    return CG_OK;
}

int cg_governing_write(GoverningEquationsType_t Equationstype) {
    cgns_governing *governing;
    int ier=0, index_dim, dim_vals;
    double posit_id;

     /* verify input */
    if (Equationstype<0 || Equationstype>=NofValidGoverningEquationsTypes) {
        cgi_error("Invalid Governing Equations Type: %d",Equationstype);
        return CG_ERROR;
    }
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    governing = cgi_governing_address(CG_MODE_WRITE, &ier);
    if (governing==0) return ier;

     /* Save data */
    governing->type=Equationstype;

     /* initialize other fields */
    strcpy(governing->name, "GoverningEquations");
    governing->id=0;
    governing->link=0;
    governing->ndescr=0;
    governing->diffusion_model=0;
    governing->nuser_data=0;

    if (posit_base && posit_zone) {
        index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;

     /* If defined under CGNSBase_t node */
    } else if (posit_base) {
        index_dim = cg->base[posit_base-1].cell_dim;

    } else {
        cgi_error("Can't find IndexDimension in cg_governing_write.");
        return CG_NO_INDEX_DIM;
    }
    if (index_dim==1) governing->dim_vals=1;
    else if (index_dim==2) governing->dim_vals=3;
    else if (index_dim==3) governing->dim_vals=6;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    dim_vals = strlen(GoverningEquationsTypeName[governing->type]);
    if (cgi_new_node(posit_id, "GoverningEquations",
        "GoverningEquations_t", &governing->id, "C1", 1, &dim_vals,
        GoverningEquationsTypeName[governing->type])) return CG_ERROR;
    return CG_OK;
}

int cg_diffusion_write(int const * diffusion_model) {
    int *diffusion;
    int n, ndata, ier=0, index_dim;
    double posit_id, dummy_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    diffusion = cgi_diffusion_address(CG_MODE_WRITE, &ier);
    if (diffusion==0) return ier;

     /* Save data */
    if (posit_base && posit_zone) {
        index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;

     /* If defined under CGNSBase_t node */
    } else if (posit_base) {
        index_dim = cg->base[posit_base-1].cell_dim;
    } else {
        cgi_error("Can't find IndexDimension in cg_diffusion_write.");
        return CG_NO_INDEX_DIM;
    }
    if (index_dim==1) ndata=1;
    else if (index_dim==2) ndata=3;
    else if (index_dim==3) ndata=6;
    else {
        cgi_error("invalid value for IndexDimension");
        return CG_ERROR;
    }

    for (n=0; n<ndata; n++) diffusion[n] = diffusion_model[n];

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;

     /* DiffusionModel */
    if (cgi_new_node(posit_id, "DiffusionModel",
        "\"int[1+...+IndexDimension]\"", &dummy_id, "I4", 1,
        &ndata, (void *)diffusion_model)) return CG_ERROR;
    return CG_OK;
}

int cg_model_write(char const * ModelLabel, ModelType_t ModelType) {
    cgns_model *model;
    char ModelName[33];
    int ier=0, index_dim;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;
    if (ModelType<0 || ModelType>=NofValidModelTypes) {
        cgi_error("Invalid %s Type: %d",ModelLabel,ModelType);
        return CG_ERROR;
    }

     /* Validate enums for each model type. */
    if (strcmp(ModelLabel, "GasModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=Ideal && ModelType!=VanderWaals &&
            ModelType!=CaloricallyPerfect && ModelType!=ThermallyPerfect &&
            ModelType!=ConstantDensity && ModelType!=RedlichKwong) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return CG_ERROR;
        }
    } else if (strcmp(ModelLabel, "ViscosityModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=Constant && ModelType!=PowerLaw && ModelType!=SutherlandLaw) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return CG_ERROR;
        }
    } else if (strcmp(ModelLabel, "ThermalConductivityModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=PowerLaw && ModelType!=SutherlandLaw && ModelType!=ConstantPrandtl) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return CG_ERROR;
        }
    } else if (strcmp(ModelLabel, "TurbulenceModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=Algebraic_BaldwinLomax && ModelType!=Algebraic_CebeciSmith &&
            ModelType!=HalfEquation_JohnsonKing && ModelType!=OneEquation_BaldwinBarth &&
            ModelType!=OneEquation_SpalartAllmaras && ModelType!=TwoEquation_JonesLaunder &&
            ModelType!=TwoEquation_MenterSST && ModelType!=TwoEquation_Wilcox) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return CG_ERROR;
        }
    } else if (strcmp(ModelLabel, "TurbulenceClosure_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=EddyViscosity  && ModelType!=ReynoldsStress &&
            ModelType!=ReynoldsStressAlgebraic) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return CG_ERROR;
        }
    } else if (strcmp(ModelLabel, "ThermalRelaxationModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=Frozen && ModelType!=ThermalEquilib &&
            ModelType!=ThermalNonequilib) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return CG_ERROR;
        }
    } else if (strcmp(ModelLabel, "ChemicalKineticsModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=Frozen && ModelType!=ChemicalEquilibCurveFit &&
            ModelType!=ChemicalEquilibMinimization && ModelType!=ChemicalNonequilib) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return CG_ERROR;
        }
    }
    else if (strcmp(ModelLabel, "EMElectricFieldModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=Frozen && ModelType!=Voltage &&
            ModelType!=Interpolated && ModelType!=Constant) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return 1;
        }
    }
    else if (strcmp(ModelLabel, "EMMagneticFieldModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=Frozen && ModelType!=Interpolated &&
	    ModelType!=Constant) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return 1;
        }
    }
    else if (strcmp(ModelLabel, "EMConductivityModel_t")==0) {
        if (ModelType!=ModelTypeNull && ModelType!=ModelTypeUserDefined &&
            ModelType!=Frozen && ModelType!=Constant &&
	    ModelType!=Equilibrium_LinRessler &&
	    ModelType!=Chemistry_LinRessler) {
            cgi_error("Model Type '%s' is not supported for %s",
                ModelTypeName[ModelType],ModelLabel);
            return 1;
        }
    }

    if (strcmp(ModelLabel, "ChemicalKineticsModel_t") &&
        strcmp(ModelLabel, "ThermalRelaxationModel_t") &&
        strcmp(ModelLabel, "TurbulenceClosure_t") &&
        strcmp(ModelLabel, "TurbulenceModel_t") &&
        strcmp(ModelLabel, "ThermalConductivityModel_t") &&
        strcmp(ModelLabel, "ViscosityModel_t") &&
	strcmp(ModelLabel, "EMElectricFieldModel_t") &&
	strcmp(ModelLabel, "EMMagneticFieldModel_t") &&
	strcmp(ModelLabel, "EMConductivityModel_t") &&
        strcmp(ModelLabel, "GasModel_t")) {
        cgi_error("Invalid Model Label: %s",ModelLabel);
        return CG_ERROR;
    }

     /* get address */
    model = cgi_model_address(CG_MODE_WRITE, ModelLabel, &ier);
    if (model==0) return ier;

     /* Save data */
    model->type = ModelType;
    strcpy(ModelName,ModelLabel);
    ModelName[strlen(ModelLabel)-2]='\0';
    strcpy(model->name, ModelName);

     /* initialize other fields */
    model->id=0;
    model->link=0;
    model->ndescr=0;
    model->narrays=0;
    model->data_class=DataClassNull;
    model->units=0;
    model->diffusion_model=0;
    model->dim_vals=0;
    model->nuser_data=0;

    if (strcmp(ModelLabel, "TurbulenceModel_t")==0) {
        if (posit_base && posit_zone) {
            index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;
     /* For TurbulenceModel_t defined under CGNSBase_t */
        } else if (posit_base) {
            index_dim = cg->base[posit_base-1].cell_dim;
        } else {
            cgi_error("Can't find IndexDimension in cg_model_write.");
            return CG_NO_INDEX_DIM;
        }
        if (index_dim==1) model->dim_vals=1;
        else if (index_dim==2) model->dim_vals=3;
        else if (index_dim==3) model->dim_vals=6;
        else {
            cgi_error("invalid value for IndexDimension");
            return CG_ERROR;
        }
    }

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_model(posit_id, model)) return CG_ERROR;
    return CG_OK;
}

int cg_array_write(char const * ArrayName, DataType_t DataType,
    int DataDimension, int const * DimensionVector, void const * Data) {
    cgns_array *array;
    int n, ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_strlen(ArrayName)) return CG_ERROR;
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;
    if (DataType!=RealSingle && DataType!=RealDouble &&
        DataType!=Integer && DataType!=Character) {
        cgi_error("Invalid datatype for data array:  %d", DataType);
        return CG_ERROR;
    }
    if (DataDimension>12) {
        cgi_error("Data arrays are limited to 12 dimensions");
        return CG_ERROR;
    }
    for (n=0; n<DataDimension; n++) {
        if (DimensionVector[n]<=0) {
            cgi_error("Invalid array size: %d",DimensionVector[n]);
            return CG_ERROR;
        }
    }

     /* get address */
    array = cgi_array_address(CG_MODE_WRITE, 0, ArrayName, &ier);
    /* printf("\tcgi_array_address = %x\n",array); */

    if (array==0) return ier;

     /* Save data */
    strcpy(array->name, ArrayName);
    strcpy(array->data_type, cgi_adf_datatype(DataType));
    array->data_dim = DataDimension;
    for (n=0; n<DataDimension; n++) array->dim_vals[n]=DimensionVector[n];

     /* initialize other fields */
    array->link=0;
    array->ndescr=0;
    array->data_class=DataClassNull;
    array->units=0;
    array->exponents=0;
    array->convert=0;
    array->data=0;

     /* write to disk */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_new_node(posit_id, array->name, "DataArray_t", &array->id,
        array->data_type, array->data_dim, array->dim_vals, Data)) return CG_ERROR;

    return CG_OK;
}

int cg_integral_write(char const * IntegralDataName) {
    cgns_integral *integral;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_strlen(IntegralDataName)) return CG_ERROR;
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    integral = cgi_integral_address(CG_MODE_WRITE, 0, IntegralDataName, &ier);
    if (integral==0) return ier;

    strcpy(integral->name, IntegralDataName);

     /* initialize other fields */
    integral->id=0;
    integral->link=0;
    integral->ndescr=0;
    integral->narrays=0;
    integral->data_class=DataClassNull;
    integral->units=0;
    integral->nuser_data=0;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_new_node(posit_id, integral->name, "IntegralData_t",
        &integral->id, "MT", 0, 0, 0)) return CG_ERROR;
    return CG_OK;
}

int cg_rind_write(int const * RindData) {
    int n, ier=0;
    int *rind, index_dim;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    rind = cgi_rind_address(CG_MODE_WRITE, &ier);
    if (rind==0) return ier;

    if (posit_base && posit_zone) {
        index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;
    } else {
        cgi_error("Can't find IndexDimension in cg_rind_write.");
        return CG_NO_INDEX_DIM;
    }

    for (n=0; n<2*index_dim; n++) rind[n]=RindData[n];

     /* save data in file & if different from default (6*0) */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_rind(posit_id, rind, index_dim)) return CG_ERROR;
    return CG_OK;
}

int cg_descriptor_write(char const * descr_name, char const * descr_text) {
    cgns_descr *descr;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_strlen(descr_name)) return CG_ERROR;
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    descr = cgi_descr_address(CG_MODE_WRITE, 0, descr_name, &ier);
    if (descr==0) return ier;

     /* Save Descriptor_t data */
    strcpy(descr->name, descr_name);
    if ((descr->text = (char *)malloc((strlen(descr_text)+1)*sizeof(char)))==NULL) {
        cgi_error("Error allocating memory for Descriptor...");
        return CG_ERROR;
    }
    strcpy(descr->text, descr_text);

     /* initialize other fields */
    descr->id=0;
    descr->link=0;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_descr(posit_id, descr)) return CG_ERROR;
    return CG_OK;
}

int cg_units_write(MassUnits_t mass, LengthUnits_t length, TimeUnits_t time,
    TemperatureUnits_t temperature, AngleUnits_t angle) {
    int ier=0;
    cgns_units *units;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;
    if (mass < 0 || mass >= NofValidMassUnits) {
        cgi_error("Invalid input:  mass unit %d not supported",mass);
        return CG_ERROR;
    }
    if (length < 0 || length >= NofValidLengthUnits) {
        cgi_error("Invalid input:  length unit %d not supported", length);
        return CG_ERROR;
    }
    if (time < 0 || time >= NofValidTimeUnits) {
        cgi_error("Invalid input:  time unit %d not supported", time);
        return CG_ERROR;
    }
    if (temperature < 0 || temperature >= NofValidTemperatureUnits) {
        cgi_error("Invalid input:  temperature unit %d not supported", temperature);
        return CG_ERROR;
    }
    if (angle < 0 || angle >= NofValidAngleUnits) {
        cgi_error("Invalid input:  angle unit %d not supported", angle);
        return CG_ERROR;
    }

     /* get address */
    units = cgi_units_address(CG_MODE_WRITE, &ier);
    if (units==0) return ier;

     /* save data in memory */
    units->mass = mass;
    units->length = length;
    units->time = time;
    units->temperature = temperature;
    units->angle = angle;

     /* initialize other fields */
    strcpy(units->name, "DimensionalUnits");
    units->id = 0;
    units->link=0;
    units->nunits = 5;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_units(posit_id, units)) return CG_ERROR;
    return CG_OK;
}

int cg_unitsfull_write(MassUnits_t mass, LengthUnits_t length,
    TimeUnits_t time, TemperatureUnits_t temperature, AngleUnits_t angle,
    ElectricCurrentUnits_t current, SubstanceAmountUnits_t amount,
    LuminousIntensityUnits_t intensity) {
    int ier=0;
    cgns_units *units;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;
    if (mass < 0 || mass >= NofValidMassUnits) {
        cgi_error("Invalid input:  mass unit %d not supported",mass);
        return CG_ERROR;
    }
    if (length < 0 || length >= NofValidLengthUnits) {
        cgi_error("Invalid input:  length unit %d not supported", length);
        return CG_ERROR;
    }
    if (time < 0 || time >= NofValidTimeUnits) {
        cgi_error("Invalid input:  time unit %d not supported", time);
        return CG_ERROR;
    }
    if (temperature < 0 || temperature >= NofValidTemperatureUnits) {
        cgi_error("Invalid input:  temperature unit %d not supported", temperature);
        return CG_ERROR;
    }
    if (angle < 0 || angle >= NofValidAngleUnits) {
        cgi_error("Invalid input:  angle unit %d not supported", angle);
        return CG_ERROR;
    }
    if (current < 0 || current >= NofValidElectricCurrentUnits) {
        cgi_error("Invalid input:  electric current unit %d not supported", current);
        return CG_ERROR;
    }
    if (amount < 0 || amount >= NofValidSubstanceAmountUnits) {
        cgi_error("Invalid input:  substance amount unit %d not supported", amount);
        return CG_ERROR;
    }
    if (intensity < 0 || intensity >= NofValidLuminousIntensityUnits) {
        cgi_error("Invalid input:  luminous intensity unit %d not supported", intensity);
        return CG_ERROR;
    }

     /* get address */
    units = cgi_units_address(CG_MODE_WRITE, &ier);
    if (units==0) return ier;

     /* save data in memory */
    units->mass = mass;
    units->length = length;
    units->time = time;
    units->temperature = temperature;
    units->angle = angle;
    units->current = current;
    units->amount = amount;
    units->intensity = intensity;

     /* initialize other fields */
    strcpy(units->name, "DimensionalUnits");
    units->id = 0;
    units->link=0;
    units->nunits = 8;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_units(posit_id, units)) return CG_ERROR;
    return CG_OK;
}

int cg_exponents_write(DataType_t DataType, void const * exponents) {
    cgns_exponent *exponent;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    exponent = cgi_exponent_address(CG_MODE_WRITE, &ier);
    if (exponent==0) return ier;

     /* Save Data */
    strcpy(exponent->data_type, cgi_adf_datatype(DataType));
    exponent->data = (void *)malloc(5*size_of(exponent->data_type));
    if (exponent->data == NULL) {
        cgi_error("Error allocating exponent->data");
        return CG_ERROR;
    }

    if (DataType==RealSingle) {
        (*((float *)exponent->data+0)) = (*((float *) exponents+0));
        (*((float *)exponent->data+1)) = (*((float *) exponents+1));
        (*((float *)exponent->data+2)) = (*((float *) exponents+2));
        (*((float *)exponent->data+3)) = (*((float *) exponents+3));
        (*((float *)exponent->data+4)) = (*((float *) exponents+4));

    } else if (DataType==RealDouble) {
        (*((double *)exponent->data+0)) = (*((double *) exponents+0));
        (*((double *)exponent->data+1)) = (*((double *) exponents+1));
        (*((double *)exponent->data+2)) = (*((double *) exponents+2));
        (*((double *)exponent->data+3)) = (*((double *) exponents+3));
        (*((double *)exponent->data+4)) = (*((double *) exponents+4));
    }

     /* initialize other fields */
    strcpy(exponent->name, "DimensionalExponents");
    exponent->id = 0;
    exponent->link = 0;
    exponent->nexps = 5;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_exponents(posit_id, exponent)) return CG_ERROR;
    return CG_OK;
}

int cg_expfull_write(DataType_t DataType, void const * exponents) {
    cgns_exponent *exponent;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    exponent = cgi_exponent_address(CG_MODE_WRITE, &ier);
    if (exponent==0) return ier;

     /* Save Data */
    strcpy(exponent->data_type, cgi_adf_datatype(DataType));
    exponent->data = (void *)malloc(8*size_of(exponent->data_type));
    if (exponent->data == NULL) {
        cgi_error("Error allocating exponent->data");
        return CG_ERROR;
    }

    if (DataType==RealSingle) {
        (*((float *)exponent->data+0)) = (*((float *) exponents+0));
        (*((float *)exponent->data+1)) = (*((float *) exponents+1));
        (*((float *)exponent->data+2)) = (*((float *) exponents+2));
        (*((float *)exponent->data+3)) = (*((float *) exponents+3));
        (*((float *)exponent->data+4)) = (*((float *) exponents+4));
        (*((float *)exponent->data+5)) = (*((float *) exponents+5));
        (*((float *)exponent->data+6)) = (*((float *) exponents+6));
        (*((float *)exponent->data+7)) = (*((float *) exponents+7));

    } else if (DataType==RealDouble) {
        (*((double *)exponent->data+0)) = (*((double *) exponents+0));
        (*((double *)exponent->data+1)) = (*((double *) exponents+1));
        (*((double *)exponent->data+2)) = (*((double *) exponents+2));
        (*((double *)exponent->data+3)) = (*((double *) exponents+3));
        (*((double *)exponent->data+4)) = (*((double *) exponents+4));
        (*((double *)exponent->data+5)) = (*((double *) exponents+5));
        (*((double *)exponent->data+6)) = (*((double *) exponents+6));
        (*((double *)exponent->data+7)) = (*((double *) exponents+7));
    }

     /* initialize other fields */
    strcpy(exponent->name, "DimensionalExponents");
    exponent->id = 0;
    exponent->link = 0;
    exponent->nexps = 8;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_exponents(posit_id, exponent)) return CG_ERROR;
    return CG_OK;
}

int cg_conversion_write(DataType_t DataType, void const * ConversionFactors) {
    cgns_conversion *conversion;
    int ier=0, dim_vals=2;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    conversion = cgi_conversion_address(CG_MODE_WRITE, &ier);
    if (conversion==0) return ier;

     /* Save data */
    strcpy(conversion->data_type, cgi_adf_datatype(DataType));
    conversion->data = (void *)malloc(2*size_of(conversion->data_type));
    if (conversion->data == NULL) {
        cgi_error("Error allocating conversion->data");
        return CG_ERROR;
    }

    if (DataType==RealSingle) {
        *((float *) conversion->data+0) = *((float *)ConversionFactors+0);
        *((float *) conversion->data+1) = *((float *)ConversionFactors+1);

    } else if (DataType==RealDouble) {
        *((double *) conversion->data+0) = *((double *)ConversionFactors+0);
        *((double *) conversion->data+1) = *((double *)ConversionFactors+1);
    }

     /* initialize other fields */
    strcpy(conversion->name, "DataConversion");
    conversion->id=0;
    conversion->link=0;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_new_node(posit_id, "DataConversion", "DataConversion_t",
        &conversion->id, conversion->data_type, 1, &dim_vals,
        conversion->data)) return CG_ERROR;
    return CG_OK;
}

int cg_dataclass_write(DataClass_t dataclass) {
    DataClass_t *DataClass;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    DataClass = cgi_dataclass_address(CG_MODE_WRITE, &ier);
    if (DataClass==0) return ier;

    (*DataClass) = dataclass;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_dataclass(posit_id, dataclass)) return CG_ERROR;
    return CG_OK;
}

int cg_gridlocation_write(GridLocation_t GridLocation) {
    GridLocation_t *location;
    int ier=0, dim_vals;
    double posit_id, dummy_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    location = cgi_location_address(CG_MODE_WRITE, &ier);
    if (location==0) return ier;

    if ((GridLocation == IFaceCenter || GridLocation == JFaceCenter ||
         GridLocation == KFaceCenter) &&
        cg->base[posit_base-1].zone[posit_zone-1].type != Structured) {
        cgi_error("GridLocation [IJK]FaceCenter only valid for Structured Grid");
        return CG_ERROR;
    }

    ier = 0;
    if (strcmp(posit->label,"FlowSolution_t")==0 ||
        strcmp(posit->label,"DiscreteData_t")== 0 ||
        strcmp(posit->label,"ArbitraryGridMotion_t")== 0) {
        if (GridLocation != Vertex && GridLocation != CellCenter &&
            GridLocation != IFaceCenter && GridLocation != JFaceCenter &&
            GridLocation != KFaceCenter) ier = 1;
    }
    else if (strcmp(posit->label,"OversetHoles_t")==0) {
        if (GridLocation != Vertex && GridLocation != CellCenter)
            ier = 1;
    }
    else if (strcmp(posit->label,"GridConnectivity_t")==0) {
        if (GridLocation != Vertex && GridLocation != CellCenter &&
            GridLocation != FaceCenter && GridLocation != IFaceCenter &&
            GridLocation != JFaceCenter && GridLocation != KFaceCenter)
            ier = 1;
    }
    else if (strcmp(posit->label,"BC_t")==0) {
        if (GridLocation != Vertex && GridLocation != FaceCenter &&
            GridLocation != IFaceCenter && GridLocation != JFaceCenter &&
            GridLocation != KFaceCenter)
            ier = 1;
    }
    else {
        if (GridLocation < 0 || GridLocation >= NofValidGridLocation)
            ier = 1;
    }
    if (ier) {
        cgi_error("GridLocation %d not valid for %s", GridLocation, posit->label);
        return CG_ERROR;
    }

    (*location) = GridLocation;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    dim_vals = strlen(GridLocationName[GridLocation]);
    if (cgi_new_node(posit_id, "GridLocation", "GridLocation_t", &dummy_id,
        "C1", 1, &dim_vals, (void *)GridLocationName[GridLocation])) return CG_ERROR;
    return CG_OK;
}

int cg_ordinal_write(int Ordinal) {
    int *ordinal;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    ordinal = cgi_ordinal_address(CG_MODE_WRITE, &ier);
    if (ordinal==0) return ier;

    (*ordinal) = Ordinal;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_ordinal(posit_id, Ordinal)) return CG_ERROR;
    return CG_OK;
}

int cg_link_write(char const * nodename, char const * filename, char const * name_in_file) {
    double posit_id, link_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;
    if (cgi_posit_id(&posit_id)) return CG_ERROR;

    /* check for valid posit */

    if (strcmp(posit->label,"DataArray_t") &&
        strcmp(posit->label,"UserDefinedData_t") &&
        strcmp(posit->label,"IntegralData_t") &&
        strcmp(posit->label,"DiscreteData_t") &&
        strcmp(posit->label,"ConvergenceHistory_t") &&
        strcmp(posit->label,"ReferenceState_t") &&
        strcmp(posit->label,"GasModel_t") &&
        strcmp(posit->label,"ViscosityModel_t") &&
        strcmp(posit->label,"ThermalConductivityModel_t") &&
        strcmp(posit->label,"TurbulenceModel_t") &&
        strcmp(posit->label,"TurbulenceClosure_t") &&
        strcmp(posit->label,"ThermalRelaxationModel_t") &&
        strcmp(posit->label,"ChemicalKineticsModel_t") &&
	strcmp(posit->label,"EMElectricFieldModel_t") &&
	strcmp(posit->label,"EMMagneticFieldModel_t") &&
	strcmp(posit->label,"EMConductivityModel_t") &&
        strcmp(posit->label,"GoverningEquations_t") &&
        strcmp(posit->label,"BCData_t") &&
        strcmp(posit->label,"BCDataSet_t") &&
        strcmp(posit->label,"Elements_t") &&
        strcmp(posit->label,"BC_t") &&
        strcmp(posit->label,"ZoneBC_t") &&
        strcmp(posit->label,"OversetHoles_t") &&
        strcmp(posit->label,"GridConnectivity_t") &&
        strcmp(posit->label,"GridConnectivity1to1_t") &&
        strcmp(posit->label,"ZoneGridConnectivity_t") &&
        strcmp(posit->label,"FlowSolution_t") &&
        strcmp(posit->label,"GridCoordinates_t") &&
        strcmp(posit->label,"RigidGridMotion_t") &&
        strcmp(posit->label,"ArbitraryGridMotion_t") &&
        strcmp(posit->label,"ZoneIterativeData_t") &&
        strcmp(posit->label,"BaseIterativeData_t") &&
        strcmp(posit->label,"Zone_t") &&
        strcmp(posit->label,"GeometryReference_t ") &&
        strcmp(posit->label,"Family_t") &&
        strcmp(posit->label,"CGNSBase_t") &&
        strcmp(posit->label,"Gravity_t") &&
        strcmp(posit->label,"Axisymmetry_t") &&
        strcmp(posit->label,"RotatingCoordinates_t") &&
        strcmp(posit->label,"BCProperty_t") &&
        strcmp(posit->label,"WallFunction_t") &&
        strcmp(posit->label,"Area_t") &&
        strcmp(posit->label,"GridConnectivityProperty_t") &&
        strcmp(posit->label,"Periodic_t") &&
        strcmp(posit->label,"AverageInterface_t")) {
        cgi_error("Links not supported under '%s' type node",posit->label);
        return CG_INCORRECT_PATH;
    }

#if DEBUG_LINKS
        printf("Modify link %s -> \"%s>%s\"\n",
            nodename, filename, name_in_file);
#endif

    /* Create the ADF link. */
    /* If a node already exists by that name, this will fail. */
    /* Need to fix this to allow modifying an existing node */
    /* but that's going to take a bit of work to keep the */
    /* in-core information current */

    if (cgio_create_link(cg->cgio, posit_id, nodename, filename,
            name_in_file, &link_id)) {
        cg_io_error("cgio_create_link");
        return CG_ERROR;
    }
    (cg->added)++;
    return CG_OK;
}


int cg_user_data_write(char const * UserDataName) {
    cgns_user_data *user_data;
    int ier=0;
    double posit_id;

     /* verify input */
    if (cgi_check_strlen(UserDataName)) return CG_ERROR;
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    user_data = cgi_user_data_address(CG_MODE_WRITE, 0, UserDataName, &ier);
    if (user_data==0) return ier;

    strcpy(user_data->name, UserDataName);

     /* initialize other fields */
    user_data->id=0;
    user_data->link=0;
    user_data->ndescr=0;
    user_data->narrays=0;
    user_data->data_class=DataClassNull;
    user_data->units=0;
    user_data->location = Vertex;
    user_data->family_name[0] = '\0';
    user_data->ordinal = 0;
    user_data->ptset = 0;
    user_data->nuser_data = 0;
    user_data->user_data = 0;

     /* save data in file */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_new_node(posit_id, user_data->name, "UserDefinedData_t",
        &user_data->id, "MT", 0, 0, 0)) return CG_ERROR;

    return CG_OK;
}

int cg_rotating_write(float const *rot_rate, float const *rot_center) {
    cgns_rotating *rotating;
    cgns_base *base;
    int ier=0, n;
    double posit_id;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    rotating=cgi_rotating_address(CG_MODE_WRITE, &ier);
    if (rotating==0) return ier;

    if (posit_base) {
        base = &cg->base[posit_base-1];
    } else {
        cgi_error("Can't find the base");
        return CG_ERROR;
    }

    rotating->array = CGNS_NEW(cgns_array, 2);
    rotating->narrays=2;

     /* Create DataArray_t RotationCenter & RotationRateVector under rotating */
    for (n=0; n<rotating->narrays; n++) {
        strcpy(rotating->array[n].data_type, "R4");
        rotating->array[n].data = (void *)malloc(base->phys_dim*sizeof(float));
        if (rotating->array[n].data == NULL) {
            cgi_error("Error allocating rotating->array[n].data");
            return CG_ERROR;
        }
        rotating->array[n].data_dim=1;
        rotating->array[n].dim_vals[0]=base->phys_dim;
    }
    memcpy(rotating->array[0].data, rot_center, base->phys_dim*sizeof(float));
    memcpy(rotating->array[1].data, rot_rate, base->phys_dim*sizeof(float));
    strcpy(rotating->array[0].name, "RotationCenter");
    strcpy(rotating->array[1].name, "RotationRateVector");

     /* initialize other fields of rotating->array[n] */
    for (n=0; n<rotating->narrays; n++) {
        rotating->array[n].id=0;
        rotating->array[n].link=0;
        rotating->array[n].ndescr=0;
        rotating->array[n].data_class=DataClassNull;
        rotating->array[n].units=0;
        rotating->array[n].exponents=0;
        rotating->array[n].convert=0;
    }

     /* initialize other fields of rotating */
    strcpy(rotating->name, "RotatingCoordinates");
    rotating->id=0;
    rotating->link=0;
    rotating->ndescr=0;
    rotating->data_class=DataClassNull;
    rotating->units=0;
    rotating->nuser_data=0;

     /* Write to disk */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;
    if (cgi_write_rotating(posit_id, rotating)) return CG_ERROR;

    return CG_OK;
}

int cg_ptset_write(PointSetType_t ptset_type, int npnts, int const * pnts)
{
    cgns_ptset *ptset = 0;
    int i, index_dim;
    int ier=0;

     /* verify input */
    if(npnts == 0 || pnts == NULL)
    {
	cgi_error("Invalid input:  npoint=%d, point set type=%s",
                   npnts, PointSetTypeName[ptset_type]);
        return CG_ERROR;
    }

    if (ptset_type == PointList)
    {
        if (npnts <= 0)
	{
            cgi_error("Invalid input:  npoint=%d, point set type=%s",
                   npnts, PointSetTypeName[ptset_type]);
            return CG_ERROR;
        }
    } else if (ptset_type == PointRange)
    {
        if (npnts != 2)
	{
            cgi_error("Invalid input:  npoint=%d, point set type=%s",
                   npnts, PointSetTypeName[ptset_type]);
            return CG_ERROR;
        }
    } else
    {
        cgi_error("Invalid point set type: %d...?",ptset_type);
        return CG_ERROR;
    }

    if (posit_base && posit_zone)
        index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;
    else {
        cgi_error("Can not properly resolve IndexDimension unless under a Zone_t node.");
        return CG_NO_INDEX_DIM;
    }

    /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE))
	return CG_ERROR;

    ptset = cgi_ptset_address(CG_MODE_WRITE, &ier);
    if (ptset==0)
	return ier;

    /* set these with the provided argument data */
    ptset->type = ptset_type;
    ptset->npts = npnts;

    if (ptset_type == PointList)
    {
	ptset->size_of_patch = npnts;
    }
    else /* PointRange */
    {
	ptset->size_of_patch = 1;
	for (i=0; i < index_dim; i++)
	    ptset->size_of_patch = ptset->size_of_patch *
		(pnts[i+index_dim] - pnts[i]+1);
    }

    /* initialize the following to default values */
    ptset->id = 0;
    ptset->link = 0;
    strcpy(ptset->name, PointSetTypeName[ptset->type]);
    strcpy(ptset->data_type,"I4");

     /* Save Point-Set on Disk */
    if (ptset->npts > 0)
    {
	double posit_id;

	/* write to disk */
	if (cgi_posit_id(&posit_id))
	    return CG_ERROR;

	if (cgi_write_ptset(posit_id, ptset->name, ptset, index_dim,
			    (void *)pnts))
	    return CG_ERROR;
    }

    return CG_OK;
}

int cg_field_partial_write(int file_number, int B, int Z, int S,
			   DataType_t type, char const *fieldname, int *rmin,
			   int *rmax, void const *field_ptr, int *F) {
    cgns_zone *zone;
    cgns_sol *sol;
    cgns_array *field;
    int n, index, index_dim, dims[CGIO_MAX_DIMENSIONS];

     /* verify input */
    if (cgi_check_strlen(fieldname)) return CG_ERROR;
    if (type!=RealSingle && type!=RealDouble && type!=Integer) {
        cgi_error("Invalid datatype for solution array %s: %d",fieldname, type);
        return CG_ERROR;
    }

    if(rmin == NULL || rmax == NULL) {
	cgi_error("NULL range value.");
	return CG_ERROR;
    }

     /* get memory addresses */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

    sol = cgi_get_sol(cg, B, Z, S);
    if (sol==0) return CG_ERROR;

    /* check for valid ranges */
    index_dim = zone->index_dim;
    if (cgi_datasize(index_dim, zone->nijk, sol->location,
            sol->rind_planes, dims)) return CG_ERROR;
    for (n = 0; n < index_dim; n++) {
        if (rmin[n] > rmax[n] || rmin[n] < 1 || rmax[n] > dims[n]) {
            cgi_error("Invalid index ranges.");
            return CG_ERROR;
        }
    }

     /* Overwrite a DataArray_t Node: */
    for (index=0; index<sol->nfields; index++) {
        if (strcmp(fieldname, sol->field[index].name)==0) {
            int m_start[CGIO_MAX_DIMENSIONS], m_end[CGIO_MAX_DIMENSIONS];
            int m_dim[CGIO_MAX_DIMENSIONS], stride[CGIO_MAX_DIMENSIONS];

            field = &(sol->field[index]);
            /* data type must be the same */
            if (strcmp(field->data_type, cgi_adf_datatype(type))) {
                cgi_error("Mismatch in data types.");
                return CG_ERROR;
            }
            for (n = 0; n < field->data_dim; n++) {
                m_start[n] = 1;
                m_end[n]   = rmax[n] - rmin[n] + 1;
                m_dim[n]   = m_end[n];
                stride[n]  = 1;
            }

            if (cgio_write_data(cg->cgio, field->id, rmin, rmax, stride,
                    field->data_dim, m_dim, m_start, m_end,
                    stride, (void *)field_ptr)) {
                cg_io_error("cgio_write_data");
                return CG_ERROR;
            }
            return CG_OK;
        }
    }

     /* add a DataArray_t Node: */
    if (sol->nfields == 0) {
        sol->field = CGNS_NEW(cgns_array, sol->nfields+1);
    } else {
        sol->field = CGNS_RENEW(cgns_array, sol->nfields+1, sol->field);
    }
    field = &(sol->field[sol->nfields]);
    sol->nfields++;
    (*F) = sol->nfields;

     /* save data in memory */
    strcpy(field->data_type, cgi_adf_datatype(type));
    strcpy(field->name, fieldname);
    field->data_dim = index_dim;
    for (n = 0; n < index_dim; n++)
        field->dim_vals[n] = dims[n];

     /* initialize */
    field->id = 0;
    field->link= 0;
    field->data=0;
    field->ndescr= 0;
    field->data_class= DataClassNull;
    field->units= 0;
    field->exponents= 0;
    field->convert= 0;

     /* Save DataArray_t node on disk: */
    if (cgi_new_node_partial(sol->id, field->name, "DataArray_t", &field->id,
        field->data_type, index_dim, field->dim_vals, rmin, rmax,
	field_ptr))
	return CG_ERROR;

    return CG_OK;
}

int cg_coord_partial_write(int file_number, int B, int Z, DataType_t type,
			   char const *coordname, int *rmin, int *rmax,
			   void const *coord_ptr, int *C) {
    cgns_zone *zone;
    cgns_zcoor *zcoor;
    cgns_array *coord;
    int n, index, index_dim, dims[CGIO_MAX_DIMENSIONS];

     /* verify input */
    if (cgi_check_strlen(coordname)) return CG_ERROR;
    if (type!=RealSingle && type!=RealDouble) {
        cgi_error("Invalid datatype for coord. array:  %d", type);
        return CG_ERROR;
    }

    if(rmin == NULL || rmax == NULL) {
	cgi_error("NULL range value.");
	return CG_ERROR;
    }

     /* get memory address for file */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

     /* get memory address for zone */
    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Get memory address for node "GridCoordinates" */
    zcoor = cgi_get_zcoorGC(cg, B, Z);
    if (zcoor==0) return CG_ERROR;

    /* check for valid ranges */
    index_dim = zone->index_dim;
    for (n = 0; n < index_dim; n++) {
        dims[n] = zone->nijk[n] + zcoor->rind_planes[2*n] +
                                  zcoor->rind_planes[2*n+1];
        if (rmin[n] > rmax[n] || rmin[n] < 1 || rmax[n] > dims[n]) {
            cgi_error("Invalid index ranges.");
            return CG_ERROR;
        }
    }

     /* Overwrite a DataArray_t Node of same size, name and data-type: */
    for (index=0; index<zcoor->ncoords; index++) {
        if (strcmp(coordname, zcoor->coord[index].name)==0) {
            int m_start[CGIO_MAX_DIMENSIONS], m_end[CGIO_MAX_DIMENSIONS];
            int m_dim[CGIO_MAX_DIMENSIONS], stride[CGIO_MAX_DIMENSIONS];

            coord = &(zcoor->coord[index]);
            /* data type must be the same */
            if (strcmp(coord->data_type,cgi_adf_datatype(type))) {
                cgi_error("Mismatch in data types.");
                return CG_ERROR;
            }
            for (n = 0; n < coord->data_dim; n++) {
                m_start[n] = 1;
                m_end[n]   = rmax[n] - rmin[n] + 1;
                m_dim[n]   = m_end[n];
                stride[n]  = 1;
            }

            if (cgio_write_data(cg->cgio, coord->id, rmin, rmax, stride,
                           coord->data_dim, m_dim, m_start, m_end,
                           stride, (void *)coord_ptr)) {
                cg_io_error("cgio_write_data");
                return CG_ERROR;
            }
            return CG_OK;
        }
    }

     /* add a DataArray_t Node: */
    if (zcoor->ncoords == 0) {
        zcoor->coord = CGNS_NEW(cgns_array, zcoor->ncoords+1);
    } else {
        zcoor->coord = CGNS_RENEW(cgns_array, zcoor->ncoords+1, zcoor->coord);
    }
    coord = &(zcoor->coord[zcoor->ncoords]);
    zcoor->ncoords++;
    (*C) = zcoor->ncoords;

     /* save coord. data in memory */
    strcpy(coord->data_type,cgi_adf_datatype(type));
    strcpy(coord->name,coordname);
    coord->id=0;        /* initialize */
    coord->link=0;
    for (n = 0; n < index_dim; n++)
        coord->dim_vals[n] = dims[n];
    coord->data_dim=index_dim;
    coord->data=0;
    coord->ndescr=0;
    coord->data_class=DataClassNull;
    coord->units=0;
    coord->exponents=0;
    coord->convert=0;

     /* Create GridCoodinates_t node if not already created */
    if (zcoor->id == 0) {
        if (cgi_new_node(zone->id, "GridCoordinates", "GridCoordinates_t",
            &zcoor->id, "MT", 0, 0, 0)) return CG_ERROR;
    }

     /* Create DataArray_t node on disk */
    if (cgi_new_node_partial(zcoor->id, coord->name, "DataArray_t", &coord->id,
        coord->data_type, index_dim, coord->dim_vals, rmin, rmax,
	coord_ptr))
	return CG_ERROR;

    return CG_OK;
}

static int read_element_data(cgns_section *section) {
    if (section->connect->data == 0) {
        int ndim, dims[CGIO_MAX_DIMENSIONS];
        void *data;
        if (cgi_read_node(section->connect->id, section->connect->name,
                section->connect->data_type, &ndim, dims,
                &data, READ_DATA)) return 1;
        if (ndim != 1 || dims[0] != section->connect->dim_vals[0]) {
            free(data);
            cgi_error("mismatch in element connectivity size");
            return 1;
        }
        section->connect->data = data;
    }
    return 0;
}

static int read_parent_data(cgns_section *section) {
    if (section->parent->data == 0) {
        int ndim, dims[CGIO_MAX_DIMENSIONS];
        void *data;
        if (cgi_read_node(section->parent->id, section->parent->name,
                section->parent->data_type, &ndim, dims,
                &data, READ_DATA)) return 1;
        if (ndim != 2 || dims[0] != section->parent->dim_vals[0] ||
            dims[1] != section->parent->dim_vals[1]) {
            free(data);
            cgi_error("mismatch in parent data size");
            return 1;
        }
        section->parent->data = data;
    }
    return 0;
}

int cg_section_partial_write(int file_number, int B, int Z,
			     char const * SectionName, ElementType_t type,
			     int start, int end, int nbndry, int *S) {
    cgns_zone *zone;
    cgns_section *section = NULL;
    int index, num, elemsize;
    int ElementDataSize=0;

     /* verify input */
    if (cgi_check_strlen(SectionName)) return CG_ERROR;

    if (type < 0 || type >= NofValidElementTypes) {
        cgi_error("Invalid element type defined for section '%s'",SectionName);
        return CG_ERROR;
    }
    if ((end-start)<0) {
        cgi_error("Invalid element range defined for section '%s'",SectionName);
        return CG_ERROR;
    }
    if (nbndry>(end-start+1)) {
        cgi_error("Invalid boundary element number for section '%s'",SectionName);
        return CG_ERROR;
    }

     /* Compute ElementDataSize */
    num = end - start + 1;
    if (type < MIXED) {
        if (cg_npe(type, &elemsize)) return CG_ERROR;
        if (elemsize <= 0) {
            cgi_error("Invalid element type");
            return CG_ERROR;
        }
    }
    else
        elemsize = 2;
    ElementDataSize = num * elemsize;

     /* get file and check mode */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE))
        return CG_ERROR;

    zone = cgi_get_zone(cg, B, Z);
    if (zone==0) return CG_ERROR;

     /* Overwrite a Elements_t Node: */
    for (index=0; index<zone->nsections; index++) {
        if (strcmp(SectionName, zone->section[index].name)==0) {

             /* in CG_MODE_WRITE, children names must be unique */
            if (cg->mode==CG_MODE_WRITE) {
                cgi_error("Duplicate child name found: %s",SectionName);
                return CG_ERROR;
            }

             /* overwrite an existing section */
             /* delete the existing section from file */
            if (cgi_delete_node(zone->id, zone->section[index].id))
                return CG_ERROR;
             /* save the old in-memory address to overwrite */
            section = &(zone->section[index]);
             /* free memory */
            cgi_free_section(section);
            break;
        }
    }
     /* ... or add a Elements_t Node: */
    if (index==zone->nsections) {
        if (zone->nsections == 0) {
            zone->section = CGNS_NEW(cgns_section, zone->nsections+1);
        } else {
            zone->section = CGNS_RENEW(cgns_section, zone->nsections+1, zone->section);
        }
        section = &(zone->section[zone->nsections]);
        zone->nsections++;
    }
    (*S) = index+1;
        
    /* initialize ... */
    strcpy(section->name, SectionName);
    section->el_type = type;
    section->range[0] = start;
    section->range[1] = end;
    section->el_bound = nbndry;

    section->connect = CGNS_NEW(cgns_array, 1);
    section->connect->data = 0;
    strcpy(section->connect->name,"ElementConnectivity");
    strcpy(section->connect->data_type,"I4");
    section->connect->data_dim=1;
    section->connect->dim_vals[0]=ElementDataSize;

    section->id=0;
    section->link=0;
    section->ndescr=0;
    section->parent=0;
    section->nuser_data=0;
    section->rind_planes=0;

     /* initialize other fields */
    section->connect->id=0;
    section->connect->link=0;
    section->connect->ndescr=0;
    section->connect->data_class=DataClassNull;
    section->connect->units=0;
    section->connect->exponents=0;
    section->connect->convert=0;

    /* if not fixed element size, need to create valid data for sizing */
    
    if (type >= MIXED) {
        int n, nn, *data = CGNS_NEW(int, ElementDataSize);
        for (nn = 0, n = 0; n < num; n++) {
            data[nn++] = (type == MIXED ? (int)NODE : 1);
            data[nn++] = 0;
        }
        section->connect->data = (void *)data;
    }

    if (cgi_write_section(zone->id, section)) return CG_ERROR;

    return CG_OK;
}

int cg_elements_partial_write(int file_number, int B, int Z, int S,
    			      int start, int end, int const *elements) {
    cgns_section *section;
    ElementType_t type;
    int elemsize, oldsize;
    int num, size, offset;
    int n, i, newsize, ElementDataSize;
    int *oldelems, *newelems;
    double id;

     /* get file and check mode */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE))
        return CG_ERROR;

    section = cgi_get_section(cg, B, Z, S);
    if (section == 0) return CG_ERROR;

    /* check input range */

    if (start > end) {
        cgi_error("Invalid element range for section '%s' elements",
            section->name);
        return CG_ERROR;
    }

    type = section->el_type;

    /* get fill-in element type */
    if (type < MIXED) {
        if (cg_npe(type, &elemsize)) return CG_ERROR;
        if (elemsize <= 0) {
            cgi_error("Invalid element type");
            return CG_ERROR;
        }
    }
    else
        elemsize = 2;

    offset  = start < section->range[0] ? section->range[0] - start : 0;
    oldsize = section->range[1] - section->range[0] + 1;
    ElementDataSize = cgi_element_data_size(type, end - start + 1, elements);
    if (ElementDataSize < 0) return CG_ERROR;

    /* can we just use the user's data ? */

    if (start >= section->range[0] && end <= section->range[1] &&
        type < MIXED && section->connect->data == 0) {
        int s_start, s_end, s_stride;
        int m_start, m_end, m_stride, m_dim;

        s_start  = cgi_element_data_size(type, start - section->range[0], 0) + 1;
        s_end    = cgi_element_data_size(type, end - section->range[0] + 1, 0);
        s_stride = 1;
        m_start  = 1;
        m_end    = ElementDataSize;
        m_dim    = ElementDataSize;
        m_stride = 1;

        if (cgio_write_data(cg->cgio, section->connect->id,
                &s_start, &s_end, &s_stride, 1, &m_dim,
                &m_start, &m_end, &m_stride, elements)) {
            cg_io_error("cgio_write_data");
            return CG_ERROR;
        }
    }
    else {

        /* got to do it in memory */

        if (read_element_data(section)) return CG_ERROR;

        oldelems = (int *)section->connect->data;
        oldsize = section->connect->dim_vals[0];
        newsize = ElementDataSize;

        if (end < section->range[0]) {
            newsize += oldsize;
            num = section->range[0] - end - 1;
            if (num > 0) newsize += (elemsize * num);
        } else if (start > section->range[1]) {
            newsize += oldsize;
            num = start - section->range[1] - 1;
            if (num > 0) newsize += (elemsize * num);
        } else {
            /* overlap */
            if (start >= section->range[0]) {
                num = start - section->range[0];
                size = cgi_element_data_size(type, num, oldelems);
                if (size < 0) return CG_ERROR;
                newsize += size;
            }
            if (end <= section->range[1]) {
                num = end - section->range[0] + 1;
                offset = cgi_element_data_size(type, num, oldelems);
                if (offset < 0) return CG_ERROR;
                size = oldsize - offset;
                newsize += size;
            }
        }

        /* create new element connectivity array */

        newelems = (int *) malloc (newsize * sizeof(int));
        if (NULL == newelems) {
            cgi_error("Error allocating new connectivity data");
            return CG_ERROR;
        }
        n = 0;
        if (start <= section->range[0]) {
            memcpy(newelems, elements, ElementDataSize*sizeof(int));
            n += ElementDataSize;
            if (end < section->range[0]) {
                num = section->range[0] - end - 1;
                while (num-- > 0) {
                    if (type >= MIXED) {
                        newelems[n++] = (type == MIXED ? (int)NODE : 1);
                        newelems[n++] = 0;
                    } else {
                        for (i = 0; i < elemsize; i++)
                            newelems[n++] = 0;
                    }
                }
                memcpy(&newelems[n], oldelems, oldsize*sizeof(int));
                n += oldsize;
            } else if (end < section->range[1]) {
                num = end - section->range[0] + 1;
                offset = cgi_element_data_size(type, num, oldelems);
                if (offset < 0) return CG_ERROR;
                size = oldsize - offset;
                memcpy(&newelems[n], &oldelems[offset], size*sizeof(int));
                n += size;
            }
        } else if (start > section->range[1]) {
            memcpy(newelems, oldelems, oldsize*sizeof(int));
            n += oldsize;
            num = start - section->range[1] - 1;
            while (num-- > 0) {
                if (type >= MIXED) {
                    newelems[n++] = (type == MIXED ? (int)NODE : 1);
                    newelems[n++] = 0;
                } else {
                    for (i = 0; i < elemsize; i++)
                        newelems[n++] = 0;
                }
            }
            memcpy(&newelems[n], elements, ElementDataSize*sizeof(int));
            n += ElementDataSize;
        } else {
            num = start - section->range[0];
            size = cgi_element_data_size(type, num, oldelems);
            memcpy(newelems, oldelems, size*sizeof(int));
            n += size;
            memcpy(&newelems[n], elements, ElementDataSize*sizeof(int));
            n += ElementDataSize;
            if (end < section->range[1]) {
                num = end - section->range[0] + 1;
                offset = cgi_element_data_size(type, num, oldelems);
                if (offset < 0) return CG_ERROR;
                size = oldsize - offset;
                memcpy(&newelems[n], &oldelems[offset], size*sizeof(int));
                n += size;
            }
        }
        if (n != newsize) {
            cgi_error("my counting is off !!!\n");
            return CG_ERROR;
        }

        /* save these before updating for parent data */

        offset  = start < section->range[0] ? section->range[0] - start : 0;
        oldsize = section->range[1] - section->range[0] + 1;

        free(section->connect->data);
        section->connect->dim_vals[0] = newsize;
        section->connect->data = newelems;

        /* update ranges */

        if (start < section->range[0]) section->range[0] = start;
        if (end   > section->range[1]) section->range[1] = end;

        /* update ElementRange */

        if (cgio_get_node_id(cg->cgio, section->id, "ElementRange", &id)) {
            cg_io_error("cgio_get_node_id");
            return CG_ERROR;
        }
        if (cgio_write_all_data(cg->cgio, id, section->range)) {
            cg_io_error("cgio_write_all_data");
            return CG_ERROR;
        }

        /* update ElementConnectivity */

        if (cgio_set_dimensions(cg->cgio, section->connect->id,
                section->connect->data_type, 1,
                section->connect->dim_vals)) {
            cg_io_error("cgio_set_dimensions");
            return CG_ERROR;
        }
        if (cgio_write_all_data(cg->cgio, section->connect->id, newelems)) {
            cg_io_error("cgio_write_all_data");
            return CG_ERROR;
        }
    }

    /* update the parent data array if it exists */

    newsize = section->range[1] - section->range[0] + 1;

    if (section->parent && newsize != section->parent->dim_vals[0]) {
        int j;

        if (read_parent_data(section)) return CG_ERROR;

        oldelems = (int *)section->parent->data;
        newelems = (int *)malloc(4 * newsize * sizeof(int));
        if (NULL == newelems) {
            cgi_error("Error alocating new parent data");
            return CG_ERROR;
        }
        for (n = 0; n < 4*newsize; n++)
            newelems[n] = 0;

        /* add previous data */

        for (i = 0, num = 0; num < 4; num++) {
            j = num * newsize + offset;
            for (n = 0; n < oldsize; n++)
                newelems[j++] = oldelems[i++];
        }

        /* overwrite with new range */

        offset = start - section->range[0];
        for (num = 0; num < 4; num++) {
            j = num * newsize + offset;
            for (n = start; n <= end; n++)
                newelems[j++] = 0;
        }

        free(section->parent->data);
        section->parent->data = newelems;
        section->parent->dim_vals[0] = newsize;

        /* update on disk */

        if (cgio_set_dimensions(cg->cgio, section->parent->id,
                section->parent->data_type, 2,
                section->parent->dim_vals)) {
            cg_io_error("cgio_set_dimensions");
            return CG_ERROR;
        }
        if (cgio_write_all_data(cg->cgio, section->parent->id, newelems)) {
            cg_io_error("cgio_write_all_data");
            return CG_ERROR;
        }
    }

    return CG_OK;
}

int cg_parent_data_partial_write(int file_number, int B, int Z, int S,
				 int start, int end,
                                 int const *parent_data) {
    cgns_section *section;
    cgns_array *parent;
    int *data, i, j, n;
    int num, size, offset;

     /* get file and check mode */
    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    section = cgi_get_section(cg, B, Z, S);
    if (section == 0) return CG_ERROR;

    /* check input range */

    if (start < section->range[0] || end > section->range[1] || start > end) {
        cgi_error("Invalid element range for section '%s' parent data",
            section->name);
        return CG_ERROR;
    }

    size   = section->range[1] - section->range[0] + 1;
    offset = start - section->range[0];
    parent = section->parent;

    /* create the parent data if it doesn't exist already */

    if (parent == 0) {
        section->parent = CGNS_NEW(cgns_array, 1);
        parent = section->parent;
        strcpy(parent->data_type, "I4");
        strcpy(parent->name, "ParentData");
        parent->data = 0;
        parent->data_dim =2;
        parent->dim_vals[0]=size;
        parent->dim_vals[1]=4;

         /* initialize other fields */
        parent->id=0;
        parent->link=0;
        parent->ndescr=0;
        parent->data_class=DataClassNull;
        parent->units=0;
        parent->exponents=0;
        parent->convert=0;

        if (cgi_write_array(section->id, section->parent)) return CG_ERROR;
    }

    /* make sure parent data is correct */

    if (size != parent->dim_vals[0]) {
        cgi_error("internal errror - invalid parent data size !!!");
        return CG_ERROR;
    }
        
    if (start >= section->range[0] && end <= section->range[1] &&
        section->parent->data == 0) {
        int s_start[2], s_end[2], s_stride[2];
        int m_start[2], m_end[2], m_stride[2], m_dim[2];

        s_start[0] = start - section->range[0] + 1;
        s_end[0]   = end - section->range[0] + 1;
        s_stride[0]= 1;
        s_start[1] = 1;
        s_end[1]   = 4;
        s_stride[1]= 1;
        m_start[0] = 1;
        m_end[0]   = end - start + 1;
        m_stride[0]= 1;
        m_start[1] = 1;
        m_end[1]   = 4;
        m_stride[1]= 1;
        m_dim[0]   = m_end[0];
        m_dim[1]   = 4;
        
        if (cgio_write_data(cg->cgio, section->parent->id,
                s_start, s_end, s_stride, 2, m_dim,
                m_start, m_end, m_stride, parent_data)) {
            cg_io_error("cgio_write_data");
            return CG_ERROR;
        }
    }
    else {
        if (read_parent_data(section)) return CG_ERROR;
        data = (int *)parent->data;

        for (i = 0, num = 0; num < 4; num++) {
            j = num * size + offset;
            for (n = start; n <= end; n++)
                data[j++] = parent_data[i++];
        }

        if (cgio_write_all_data(cg->cgio, parent->id, data)) {
            cg_io_error("cgio_write_all_data");
            return CG_ERROR;
        }
    }

    return CG_OK;
}

int cg_elements_partial_read(int file_number, int B, int Z, int S,
		 int start, int end, int *elements, int *parent_data) {
    cgns_section *section;
    int offset, size;
    int *data;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    section = cgi_get_section(cg, B, Z, S);
    if (section == 0) return CG_ERROR;

    /* check the requested element range against the stored element range,
    * and the validity of the requested range
    */
    if(start > end || start < section->range[0] || end > section->range[1]) {
	cgi_error("Error in requested element data range.");
        return CG_ERROR;
    }
    
    /* if the elements are fixed size, read directly into user memory */
    if (section->connect->data == 0 && section->el_type < MIXED) {
        int s_start, s_end, s_stride;
        int m_start, m_end, m_stride, m_dim;

        size = cgi_element_data_size(section->el_type, end - start + 1, 0);
        if (size < 0) return CG_ERROR;
        s_start  = cgi_element_data_size(section->el_type,
                       start - section->range[0], 0) + 1;
        s_end    = cgi_element_data_size(section->el_type,
                       end - section->range[0] + 1, 0);
        s_stride = 1;
        m_start  = 1;
        m_end    = size;
        m_dim    = size;
        m_stride = 1;
        
        if (cgio_read_data(cg->cgio, section->connect->id,
                &s_start, &s_end, &s_stride, 1, &m_dim,
                &m_start, &m_end, &m_stride, elements)) {
            cg_io_error("cgio_read_data");
            return CG_ERROR;
        }
    }
    else {
        /* need to get the elements to compute locations */
        if (read_element_data(section)) return CG_ERROR;
        data = (int *)section->connect->data;
        offset = cgi_element_data_size(section->el_type,
                     start - section->range[0], data);
        size = cgi_element_data_size(section->el_type,
                     end - start + 1, &data[offset]);
        memcpy(elements, &data[offset], size*sizeof(int));
    }

    if (section->parent && parent_data) {
        offset = start - section->range[0];
        size = section->range[1] - section->range[0] + 1;
        if (section->connect->data == 0 && section->el_type < MIXED) {
            int s_start[2], s_end[2], s_stride[2];
            int m_start[2], m_end[2], m_stride[2], m_dim[2];

            s_start[0] = start - section->range[0] + 1;
            s_end[0]   = end - section->range[0] + 1;
            s_stride[0]= 1;
            s_start[1] = 1;
            s_end[1]   = 4;
            s_stride[1]= 1;
            m_start[0] = 1;
            m_end[0]   = end - start + 1;
            m_stride[0]= 1;
            m_start[1] = 1;
            m_end[1]   = 4;
            m_stride[1]= 1;
            m_dim[0]   = m_end[0];
            m_dim[1]   = 4;
        
            if (cgio_read_data(cg->cgio, section->parent->id,
                    s_start, s_end, s_stride, 2, m_dim,
                    m_start, m_end, m_stride, parent_data)) {
                cg_io_error("cgio_read_data");
                return CG_ERROR;
            }
        }
        else {
            int i, j, n, nn;
            if (read_parent_data(section)) return CG_ERROR;
            data = (int *)section->parent->data;
            for (n = 0, j = 0; j < 4; j++) {
                nn = j * size + offset;
                for (i = start; i <= end; i++)
                    parent_data[n++] = data[nn++];
            }
        }
    }
    return CG_OK;
}

int cg_ElementPartialSize(int file_number, int B, int Z, int S,
	int start, int end, int *ElementDataSize) {
    cgns_section *section;
    int size, offset, *data;

    cg = cgi_get_file(file_number);
    if (cg == 0) return CG_ERROR;

     /* verify input */
    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    section = cgi_get_section(cg, B, Z, S);
    if (section == 0) return CG_ERROR;

    if (start > end || start < section->range[0] ||
        end > section->range[1]) {
        cgi_error("Invalid range for section '%s'", section->name);
        return CG_ERROR;
    }
    if (start == section->range[0] && end == section->range[1]) {
        *ElementDataSize = section->connect->dim_vals[0];
        return CG_OK;
    }

    if (section->el_type < MIXED) {
        size = cgi_element_data_size(section->el_type, end - start + 1, 0);
        if (size < 0) return CG_ERROR;
        *ElementDataSize = size;
        return CG_OK;
    }

    if (read_element_data(section)) return CG_ERROR;
    data = (int *)section->connect->data;
    offset = cgi_element_data_size(section->el_type,
                 start - section->range[0], data);
    if (offset < 0) return CG_ERROR;
    size = cgi_element_data_size(section->el_type,
               end - start + 1, &data[offset]);
    if (size < 0) return CG_ERROR;
    *ElementDataSize = size;
    return CG_OK;
}

int cg_bcdataset_write(char *name, BCType_t BCType, BCDataType_t BCDataType)
{
    cgns_dataset *dataset = 0;
    cgns_bcdata *bcdata = 0;
    int length;
    int ierr=0;
    double posit_id;

     /* verify input */
    if (BCType<0 || BCType>=NofValidBCTypes) {
        cgi_error("Invalid BCType:  %d",BCType);
        return CG_ERROR;
    }

    if (BCDataType<0 || BCDataType>=NofValidBCDataTypes) {
        cgi_error("BCDataType %d not valid",BCDataType);
        return CG_ERROR;
    }

    if (cgi_check_strlen(name)) return CG_ERROR;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_WRITE)) return CG_ERROR;

    if (strcmp(posit->label,"FamilyBC_t")==0)
    {
	cgns_fambc *fambc = (cgns_fambc *)posit->posit;
	cgns_dataset *dataset_tmp = fambc->dataset;
	int index, ndataset = fambc->ndataset;

	for (index=0; index < ndataset; ++index)
	    if (strcmp(name, dataset_tmp[index].name)==0)
		dataset = &dataset_tmp[index];
    }

    if(dataset)
    {
	/* Overwrite a BCDataSet_t node : */
	if (dataset->dirichlet && BCDataType==Dirichlet)
	{
	    if (cg->mode == CG_MODE_WRITE) {
		cgi_error("Dirichlet data already defined under BCDataSet_t '%s'",
			  dataset->name);
		return CG_ERROR;
	    } else {
		if (cgi_delete_node(dataset->id, dataset->dirichlet->id))
		    return CG_ERROR;
		cgi_free_bcdata(dataset->dirichlet);
	    }
	}
	else if(dataset->neumann && BCDataType==Neumann)
	{
	    if (cg->mode == CG_MODE_WRITE) {
		cgi_error("Neumann data already defined under BCDataSet_t '%s'",
			  dataset->name);
		return CG_ERROR;
	    } else {
		if (cgi_delete_node(dataset->id, dataset->neumann->id))
		    return CG_ERROR;
		cgi_free_bcdata(dataset->neumann);
	    }
	}
    }
    else
    {
	/* get memory address for BCDataSet_t node */
	dataset = cgi_bcdataset_address(CG_MODE_WRITE, 0, name, &ierr);
	if (dataset == 0) return ierr;

	/* save data in memory */
	dataset->type = BCType;
	strcpy(dataset->name, name);

	/* initialize other fields */
	dataset->id = 0;
	dataset->link=0;
	dataset->ndescr=0;
	dataset->dirichlet=0;
	dataset->neumann=0;
	dataset->state=0;
	dataset->data_class=DataClassNull;
	dataset->units=0;
	dataset->nuser_data=0;

	/* write to disk */
	if (cgi_posit_id(&posit_id))
	    return CG_ERROR;

	/* save data in file */
	length = strlen(BCTypeName[dataset->type]);
	if (cgi_new_node(posit_id, dataset->name, "BCDataSet_t", &dataset->id,
			"C1", 1, &length, (void *)BCTypeName[dataset->type]))
	    return CG_ERROR;
    }

    if (BCDataType==Dirichlet) {
        if ( !dataset->dirichlet)
	    dataset->dirichlet = CGNS_NEW(cgns_bcdata,1);

        strcpy(dataset->dirichlet->name, "DirichletData");
        bcdata = dataset->dirichlet;
    } else if(BCDataType==Neumann){
        if ( !dataset->neumann)
	    dataset->neumann = CGNS_NEW(cgns_bcdata,1);

        strcpy(dataset->neumann->name, "NeumannData");
        bcdata = dataset->neumann;
    }
    else {
        cgi_error("BCDataType is not Dirichlet or Neumann");
	return CG_ERROR;
    }

     /* initialize other fields */
    bcdata->id = 0;
    bcdata->link=0;
    bcdata->ndescr=0;
    bcdata->data_class=DataClassNull;
    bcdata->units=0;
    bcdata->narrays=0;
    bcdata->nuser_data=0;

    if (cgi_new_node(dataset->id, bcdata->name, "BCData_t", &bcdata->id,
        "MT", 0, 0, 0)) return CG_ERROR;

    return CG_OK;
}

int cg_bcdataset_info(int *n_dataset)
{
    /* check for valid posit */
    if (posit == 0) {
        cgi_error("No current position set by cg_goto\n");
        (*n_dataset) = 0;
        return CG_ERROR;
    }

    /* Possible parents of BCDataSet_t node:
     * Implemented:
     * 	FamilyBC_t
     * Unimplemented:
     *
     */
    if(strcmp(posit->label,"FamilyBC_t") ==0)
    {
	cgns_fambc *fambc = (cgns_fambc *)posit->posit;
	*n_dataset = fambc->ndataset;
    }
    else{
        cgi_error("FamilyBC_t node not supported under '%s' type node",posit->label);

        return CG_INCORRECT_PATH;
    }

    return CG_OK;
}

int cg_bcdataset_read(int index, char *name, BCType_t *BCType,
		      int *DirichletFlag, int *NeumannFlag)
{
    cgns_dataset *dataset;
    int ier = 0;

    if (cgi_check_mode(cg->filename, cg->mode, CG_MODE_READ)) return CG_ERROR;

    dataset = cgi_bcdataset_address(CG_MODE_READ, index, NULL, &ier);
    if (dataset==0) return CG_ERROR;

    strcpy(name, dataset->name);
    *BCType = dataset->type;
    if (dataset->dirichlet) *DirichletFlag=1;
    else                    *DirichletFlag=0;
    if (dataset->neumann) *NeumannFlag=1;
    else                  *NeumannFlag=0;

    return CG_OK;
}

/****************************************************************************/
/* the index in this list IS the cgnslib.h/ElementType_t index */
int cg_npe(ElementType_t type, int *npe) {
    static int el_size[NofValidElementTypes] = {
        0,  /* ElementTypeNull */
        0,  /* ElementTypeUserDefined */
        NPE_NODE,  /* NODE */
        NPE_BAR_2,  /* BAR_2 */
        NPE_BAR_3,  /* BAR_3 */
        NPE_TRI_3,  /* TRI_3 */
        NPE_TRI_6,  /* TRI_6 */
        NPE_QUAD_4,  /* QUAD_4 */
        NPE_QUAD_8,  /* QUAD_8 */
        NPE_QUAD_9,  /* QUAD_9 */
        NPE_TETRA_4,  /* TETRA_4 */
        NPE_TETRA_10, /* TETRA_10 */
        NPE_PYRA_5,  /* PYRA_5 */
        NPE_PYRA_13, /* PYRA_13 */
        NPE_PYRA_14, /* PYRA_14 */
        NPE_PENTA_6,  /* PENTA_6 */
        NPE_PENTA_15, /* PENTA_15 */
        NPE_PENTA_18, /* PENTA_18 */
        NPE_HEXA_8,  /* HEXA_8 */
        NPE_HEXA_20, /* HEXA_20 */
        NPE_HEXA_27, /* HEXA_27 */
        NPE_MIXED,  /* MIXED */
        NPE_NGON_n,  /* NGON_n */
        NPE_NFACE_n,  /* NFACE_n */ 
};
    if (type == NGON_n)
    {
      *npe = NPE_NGON_n;
    }
    else if (type == NFACE_n)
    {
      *npe = NPE_NFACE_n;
    }
    else if (type < 0 || type >= NofValidElementTypes) 
    {
        *npe = -1;
        cgi_error("Invalid element type");
        return CG_ERROR;

    }
    (*npe) = el_size[type];
    return CG_OK;
}

/*****************************************************************************\
 *            General Delete Function
\*****************************************************************************/

int cg_delete_node(char *node_name) {
    int n, m, index_dim;
    double posit_id, node_id;
    char_33 node_label;

     /* verify input */
    if (cg->mode != CG_MODE_MODIFY) {
        cgi_error("File %s must be opened in mode modify to delete a node", cg->filename);
        return CG_ERROR;
    }
     /* ADF ID of parent = posit_id */
    if (cgi_posit_id(&posit_id)) return CG_ERROR;

     /* ADF ID of node */
    if (cgio_get_node_id(cg->cgio, posit_id, node_name, &node_id)) {
        cg_io_error("cgio_get_node_id");
        return CG_ERROR;
    }
     /* Get label of node to be deleted */
    if (cgio_get_label(cg->cgio, node_id, node_label)) {
        cg_io_error("cgio_get_label");
        return CG_ERROR;
    }

/* Nodes that can't be deleted */
    if (
        (strcmp(posit->label,"Zone_t")==0 &&
         strcmp(node_label,"ZoneType_t")==0 ) ||

        (strcmp(posit->label,"GridConnectivity1to1_t")==0 &&
         (strcmp(node_name,"PointRange")==0 ||
          strcmp(node_name,"PointRangeDonor")==0) ) ||

        (strcmp(posit->label,"OversetHoles_t")==0 &&
         (strcmp(node_label,"IndexRange_t")==0 ||
          strcmp(node_name,"PointList")==0) ) ||

        (strcmp(posit->label,"GridConnectivity_t")==0 &&
         (strcmp(node_name,"PointRange")==0 ||
          strcmp(node_name,"PointList")==0 ||
          strcmp(node_name,"CellListDonor")==0 ||
          strcmp(node_name,"PointListDonor")==0 ||
          strcmp(node_name,"InterpolantsDonor")==0) ) ||

        (strcmp(posit->label,"BC_t")==0 &&
         (strcmp(node_name,"PointList")==0 ||
          strcmp(node_name,"PointRange")==0 ||
          strcmp(node_name,"ElementList")==0 ||
          strcmp(node_name,"ElementRange")==0) ) ||

        (strcmp(posit->label,"GeometryReference_t")==0 &&
         (strcmp(node_name,"GeometryFile")==0 ||
          strcmp(node_name,"GeometryFormat")==0) ) ||

        (strcmp(posit->label,"Elements_t")==0 &&
         (strcmp(node_name,"ElementRange")==0 ||
          strcmp(node_name,"ElementConnectivity")==0) ) ||

        (strcmp(posit->label,"Gravity_t")==0 &&
         strcmp(node_name,"GravityVector")==0) ||

        (strcmp(posit->label,"Axisymmetry_t")==0 &&
         (strcmp(node_name,"AxisymmetryReferencePoint")==0 ||
          strcmp(node_name,"AxisymmetryAxisVector")==0) ) ||

        (strcmp(posit->label,"RotatingCoordinates_t")==0 &&
         (strcmp(node_name,"RotationCenter")==0 ||
          strcmp(node_name,"RotationRateVector")==0) ) ||

        (strcmp(posit->label,"WallFunction_t")==0 &&
         strcmp(node_label,"WallFunctionType_t")==0) ||

        (strcmp(posit->label,"Area_t")==0 &&
         (strcmp(node_label,"AreaType_t")==0 ||
          strcmp(node_label,"DataArray_t")==0) ) ||

        (strcmp(posit->label,"Periodic_t")==0 &&
         strcmp(node_label,"DataArray_t")==0) ||

        (strcmp(posit->label,"AverageInterface_t")==0 &&
         strcmp(node_label,"AverageInterfaceType_t")==0)

    ) {
        cgi_error("Node '%s' under '%s' can not be deleted",node_name,posit->label);
        return CG_ERROR;
    }

     /* Delete node_id under posit_id */
    if (cgi_delete_node(posit_id, node_id)) {
        /*printf("posit->label=%s, node_name=%s\n",posit->label,node_name);*/
        return CG_ERROR;
    }

/* Remove from internal database */
/* Children of CGNSBase_t */
    if (strcmp(posit->label,"CGNSBase_t")==0) {
        cgns_base *parent = (cgns_base *)posit->posit;

     /* Case 1: node_label = can have multiple occurence:  */
        if (strcmp(node_label,"Zone_t")==0)
            CGNS_DELETE_SHIFT(nzones, zone, cgi_free_zone)
        else if (strcmp(node_label,"Family_t")==0)
            CGNS_DELETE_SHIFT(nfamilies, family, cgi_free_family)
        else if (strcmp(node_label,"IntegralData_t")==0)
            CGNS_DELETE_SHIFT(nintegrals, integral, cgi_free_integral)
        else if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)

     /* Case 2: node_label = can only occur once under parent: */
        else if (strcmp(node_name,"SimulationType")==0) {
            parent->type = SimulationTypeNull;
            parent->type_id = 0;
        }
        else if (strcmp(node_label,"BaseIterativeData_t")==0)
            CGNS_DELETE_CHILD(biter, cgi_free_biter)
        else if (strcmp(node_name,"GlobalConvergenceHistory")==0)
            CGNS_DELETE_CHILD(converg, cgi_free_converg)
        else if (strcmp(node_name,"FlowEquationSet")==0)
            CGNS_DELETE_CHILD(equations, cgi_free_equations)
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"ReferenceState")==0)
            CGNS_DELETE_CHILD(state, cgi_free_state)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"Gravity")==0)
            CGNS_DELETE_CHILD(gravity, cgi_free_gravity)
        else if (strcmp(node_name,"Axisymmetry")==0)
            CGNS_DELETE_CHILD(axisym, cgi_free_axisym)
        else if (strcmp(node_name,"RotatingCoordinates")==0)
            CGNS_DELETE_CHILD(rotating, cgi_free_rotating)

/* Children of Zone_t */
    } else if (strcmp(posit->label,"Zone_t")==0) {
        cgns_zone *parent = (cgns_zone *)posit->posit;
        if (strcmp(node_label,"GridCoordinates_t")==0)
            CGNS_DELETE_SHIFT(nzcoor, zcoor, cgi_free_zcoor)
        else if (strcmp(node_label,"DiscreteData_t")==0)
            CGNS_DELETE_SHIFT(ndiscrete, discrete, cgi_free_discrete)
        else if (strcmp(node_label,"Elements_t")==0)
            CGNS_DELETE_SHIFT(nsections, section, cgi_free_section)
        else if (strcmp(node_label,"FlowSolution_t")==0)
            CGNS_DELETE_SHIFT(nsols, sol, cgi_free_sol)
        else if (strcmp(node_label,"RigidGridMotion_t")==0)
            CGNS_DELETE_SHIFT(nrmotions, rmotion, cgi_free_rmotion)
        else if (strcmp(node_label,"ArbitraryGridMotion_t")==0)
            CGNS_DELETE_SHIFT(namotions, amotion, cgi_free_amotion)
        else if (strcmp(node_label,"IntegralData_t")==0)
            CGNS_DELETE_SHIFT(nintegrals, integral, cgi_free_integral)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_name,"ZoneBC")==0)
            CGNS_DELETE_CHILD(zboco, cgi_free_zboco)
        else if (strcmp(node_name,"Ordinal")==0)
            parent->ordinal=0;
        else if (strcmp(node_name,"ZoneGridConnectivity")==0)
            CGNS_DELETE_CHILD(zconn, cgi_free_zconn)
        else if (strcmp(node_label,"ZoneIterativeData_t")==0)
            CGNS_DELETE_CHILD(ziter, cgi_free_ziter)
        else if (strcmp(node_name,"ReferenceState")==0)
            CGNS_DELETE_CHILD(state, cgi_free_state)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"FamilyName")==0)
            parent->family_name[0]='\0';
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"FlowEquationSet")==0)
            CGNS_DELETE_CHILD(equations, cgi_free_equations)
        else if (strcmp(node_name,"ZoneConvergenceHistory")==0)
            CGNS_DELETE_CHILD(converg, cgi_free_converg)
        else if (strcmp(node_name,"RotatingCoordinates")==0)
            CGNS_DELETE_CHILD(rotating, cgi_free_rotating)
     /* ZoneType can not be deleted */

/* Children of GridCoordinates_t */
    } else if (strcmp(posit->label,"GridCoordinates_t")==0) {
        cgns_zcoor *parent = (cgns_zcoor *)posit->posit;
        if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(ncoords, coord, cgi_free_array)
        else if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"Rind")==0) {
            if (posit_base && posit_zone) {
                index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;
            } else {
                cgi_error("Can't find IndexDimension in cg_delete");
                return CG_NO_INDEX_DIM;
            }
            for (n=0; n<2*index_dim; n++) parent->rind_planes[n] = 0;
        }
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of DataArray_t */
    } else if (strcmp(posit->label,"DataArray_t")==0) {
        cgns_array *parent = (cgns_array *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalExponents")==0)
            CGNS_DELETE_CHILD(exponents, cgi_free_exponents)
        else if (strcmp(node_name,"DataConversion")==0)
            CGNS_DELETE_CHILD(convert, cgi_free_convert)
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of FlowSolution_t */
    } else if (strcmp(posit->label,"FlowSolution_t")==0) {
        cgns_sol *parent = (cgns_sol *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(nfields, field, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"GridLocation")==0)
            parent->location=GridLocationNull;
        else if (strcmp(node_name,"Rind")==0) {
            if (posit_base && posit_zone) {
                index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;
            } else {
                cgi_error("Can't find IndexDimension in cg_delete");
                return CG_NO_INDEX_DIM;
            }
            for (n=0; n<2*index_dim; n++) parent->rind_planes[n] = 0;
        }

/* Children of ZoneGridConnectivity_t */
    } else if (strcmp(posit->label,"ZoneGridConnectivity_t")==0) {
        cgns_zconn *parent = (cgns_zconn *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"GridConnectivity1to1_t")==0)
            CGNS_DELETE_SHIFT(n1to1, one21, cgi_free_1to1)
        else if (strcmp(node_label,"GridConnectivity_t")==0)
            CGNS_DELETE_SHIFT(nconns, conn, cgi_free_conn)
        else if (strcmp(node_label,"OversetHoles_t")==0)
            CGNS_DELETE_SHIFT(nholes, hole, cgi_free_hole)

/* Children of OversetHoles_t */
    } else if (strcmp(posit->label,"OversetHoles_t")==0) {
        cgns_hole *parent = (cgns_hole *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"GridLocation")==0)
            parent->location=GridLocationNull;
     /* IndexRange_t & IndexArray_t can't be deleted */

/* Children of GridConnectivity_t */
    } else if (strcmp(posit->label,"GridConnectivity_t")==0) {
        cgns_conn *parent = (cgns_conn *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"InterpolantsDonor")==0) {
            if (parent->dptset.type==CellListDonor) {
                cgi_error("Node '%s' under '%s' can not be deleted",
                    node_name,posit->label);
                return CG_ERROR;
            } else {
                CGNS_DELETE_SHIFT(narrays, interpolants, cgi_free_array)
            }
        }
        else if (strcmp(node_name,"GridLocation")==0)
            parent->location=GridLocationNull;
        else if (strcmp(node_name,"Ordinal")==0)
            parent->ordinal=0;
        else if (strcmp(node_name,"GridConnectivityType")==0)
            parent->type=GridConnectivityTypeNull;
        else if (strcmp(node_name,"GridConnectivityProperty")==0)
            CGNS_DELETE_CHILD(cprop, cgi_free_cprop)
     /* IndexArray_t & IndexRange_t can't be deleted */

/* Children of GridConnectivity1to1_t */
    } else if (strcmp(posit->label,"GridConnectivity1to1_t")==0) {
        cgns_1to1 *parent = (cgns_1to1 *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"Ordinal")==0)
            parent->ordinal=0;
        else if (strcmp(node_name,"GridConnectivityProperty")==0)
            CGNS_DELETE_CHILD(cprop, cgi_free_cprop)
     /* PointRange, PointRangeDonor, Transform can't be deleted */

/* Children of ZoneBC_t */
    } else if (strcmp(posit->label,"ZoneBC_t")==0) {
        cgns_zboco *parent = (cgns_zboco *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"BC_t")==0)
            CGNS_DELETE_SHIFT(nbocos, boco, cgi_free_boco)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"ReferenceState")==0)
            CGNS_DELETE_CHILD(state, cgi_free_state)

/* Children of BC_t */
    } else if (strcmp(posit->label,"BC_t")==0) {
        cgns_boco *parent = (cgns_boco *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"BCDataSet_t")==0) {
 #if 0
            CGNS_DELETE_SHIFT(ndataset, dataset, cgi_free_dataset)
 #else
            for (n = 0; n < parent->ndataset &&
                strcmp(parent->dataset[n].name, node_name); n++);
            if (n == parent->ndataset) {
                cgi_error("Error in cg_delete: Can't find node '%s'",node_name);
                return 1;
            }
            if (parent->dataset[n].ptset == parent->ptset)
                parent->dataset[n].ptset = 0;
            cgi_free_dataset(&parent->dataset[n]);
            for (m = n+1; m < parent->ndataset; m++)
                parent->dataset[m-1] = parent->dataset[m];
            if (--parent->ndataset == 0) {
                free(parent->dataset);
                parent->dataset = 0;
            }
 #endif
        }
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"GridLocation")==0)
            parent->location=GridLocationNull;
        else if (strcmp(node_name,"InwardNormalIndex")==0) {
            if (parent->Nindex) free(parent->Nindex);
            parent->Nindex=0;
        }
        else if (strcmp(node_name,"InwardNormalList")==0)
            CGNS_DELETE_CHILD(normal, cgi_free_array)
        else if (strcmp(node_name,"ReferenceState")==0)
            CGNS_DELETE_CHILD(state, cgi_free_state)
        else if (strcmp(node_name,"FamilyName")==0)
            parent->family_name[0]='\0';
        else if (strcmp(node_name,"Ordinal")==0)
            parent->ordinal=0;
        else if (strcmp(node_name,"BCProperty")==0)
            CGNS_DELETE_CHILD(bprop, cgi_free_bprop)
     /* IndexRange_t PointRange & IndexArray_t PointList can't be deleted */

/* Children of BCDataSet_t */
    } else if (strcmp(posit->label,"BCDataSet_t")==0) {
        cgns_dataset *parent = (cgns_dataset *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"DirichletData")==0)
            CGNS_DELETE_CHILD(dirichlet, cgi_free_bcdata)
        else if (strcmp(node_name,"NeumannData")==0)
            CGNS_DELETE_CHILD(neumann, cgi_free_bcdata)
        else if (strcmp(node_name,"ReferenceState")==0)
            CGNS_DELETE_CHILD(state, cgi_free_state)
	else if (strcmp(node_name,"GridLocation")==0)
	    parent->location=GridLocationNull;
	/* IndexRange_t PointRange & IndexArray_t PointList can't be deleted */

/* Children of BCData_t */
    } else if (strcmp(posit->label,"BCData_t")==0) {
        cgns_bcdata *parent = (cgns_bcdata *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of DiscreteData_t */
    } else if (strcmp(posit->label,"DiscreteData_t")==0) {
        cgns_discrete *parent = (cgns_discrete *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"GridLocation")==0)
            parent->location=GridLocationNull;
        else if (strcmp(node_name,"Rind")==0) {
            if (posit_base && posit_zone) {
                index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;
            } else {
                cgi_error("Can't find IndexDimension in cg_delete");
                return CG_NO_INDEX_DIM;
            }
            for (n=0; n<2*index_dim; n++) parent->rind_planes[n] = 0;
        }

/* Children of FlowEquationSet_t */
    } else if (strcmp(posit->label,"FlowEquationSet_t")==0) {
        cgns_equations *parent = (cgns_equations *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"GoverningEquations")==0)
            CGNS_DELETE_CHILD(governing, cgi_free_governing)
        else if (strcmp(node_name,"GasModel")==0)
            CGNS_DELETE_CHILD(gas, cgi_free_model)
        else if (strcmp(node_name,"ViscosityModel")==0)
            CGNS_DELETE_CHILD(visc, cgi_free_model)
        else if (strcmp(node_name,"ThermalRelaxationModel")==0)
            CGNS_DELETE_CHILD(relaxation, cgi_free_model)
        else if (strcmp(node_name,"ThermalConductivityModel")==0)
            CGNS_DELETE_CHILD(conduct, cgi_free_model)
        else if (strcmp(node_name,"ChemicalKineticsModel")==0)
            CGNS_DELETE_CHILD(chemkin, cgi_free_model)
	else if (strcmp(node_name,"EMElectricFieldModel")==0)
            CGNS_DELETE_CHILD(elecfield, cgi_free_model)
	else if (strcmp(node_name,"EMMagneticFieldModel")==0)
            CGNS_DELETE_CHILD(magnfield, cgi_free_model)
	else if (strcmp(node_name,"EMConductivityModel")==0)
            CGNS_DELETE_CHILD(emconduct, cgi_free_model)
        else if (strcmp(node_name,"TurbulenceModel")==0) {
            if (parent->turbulence) {
                if (parent->turbulence->diffusion_model)
                    free(parent->turbulence->diffusion_model);
                cgi_free_model(parent->turbulence);
                free(parent->turbulence);
            }
            parent->turbulence=0;
        }
        else if (strcmp(node_name,"TurbulenceClosure")==0)
            CGNS_DELETE_CHILD(closure, cgi_free_model)
        else if (strcmp(node_name,"EquationDimension")==0)
            parent->equation_dim=0;

/* Children of GoverningEquations_t */
    } else if (strcmp(posit->label,"GoverningEquations_t")==0) {
        cgns_governing *parent = (cgns_governing *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"DiffusionModel")==0) {
            if (parent->diffusion_model) free(parent->diffusion_model);
            parent->diffusion_model=0;
        }

/* Children of xxxModel_t */
    } else if (strcmp(posit->label,"GasModel_t")==0 ||
           strcmp(posit->label,"ViscosityModel_t")==0 ||
           strcmp(posit->label,"ThermalConductivityModel_t")==0 ||
           strcmp(posit->label,"TurbulenceModel_t")==0 ||
           strcmp(posit->label,"TurbulenceClosure_t")==0 ||
           strcmp(posit->label,"ThermalRelaxationModel_t")==0 ||
           strcmp(posit->label,"ChemicalKineticsModel_t")==0 ||
	   strcmp(posit->label,"EMElectricFieldModel_t")==0 ||
	   strcmp(posit->label,"EMMagneticFieldModel_t")==0 ||
	   strcmp(posit->label,"EMConductivityModel_t")==0) {
        cgns_model *parent = (cgns_model *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(posit->label,"TurbulenceModel_t")==0 &&
                 strcmp(node_name,"DiffusionModel")==0) {
            if (parent->diffusion_model) free(parent->diffusion_model);
            parent->diffusion_model=0;
        }

/* Children of ConvergenceHistory_t */
    } else if (strcmp(posit->label,"ConvergenceHistory_t")==0) {
        cgns_converg *parent = (cgns_converg *)posit->posit;
        if (strcmp(node_name,"NormDefinitions")==0)
            CGNS_DELETE_CHILD(NormDefinitions, cgi_free_descr)
        else if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of IntegralData_t */
    } else if (strcmp(posit->label,"IntegralData_t")==0) {
        cgns_integral *parent = (cgns_integral *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of ReferenceState_t */
    } else if (strcmp(posit->label,"ReferenceState_t")==0) {
        cgns_state *parent = (cgns_state *)posit->posit;
        if (strcmp(node_name,"ReferenceStateDescription")==0)
            CGNS_DELETE_CHILD(StateDescription, cgi_free_descr)
        else if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of Family_t */
    } else if (strcmp(posit->label,"Family_t")==0) {
        cgns_family *parent = (cgns_family *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"GeometryReference_t")==0)
            CGNS_DELETE_SHIFT(ngeos, geo, cgi_free_geo)
        else if (strcmp(node_label,"FamilyBC_t")==0)
            CGNS_DELETE_SHIFT(nfambc, fambc, cgi_free_fambc)
        else if (strcmp(node_name,"Ordinal")==0)
            parent->ordinal=0;
	else if (strcmp(node_name,"RotatingCoordinates")==0)
            CGNS_DELETE_CHILD(rotating, cgi_free_rotating)

/* Children of FamilyBC_t */
    } else if (strcmp(posit->label,"FamilyBC_t")==0) {
        cgns_fambc *parent = (cgns_fambc *)posit->posit;
	if (strcmp(node_label,"BCDataSet_t")==0)
	    CGNS_DELETE_SHIFT(ndataset, dataset, cgi_free_dataset)

/* Children of GeometryReference_t */
    } else if (strcmp(posit->label,"GeometryReference_t")==0) {
        cgns_geo *parent = (cgns_geo *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"GeometryEntity_t")==0)
            CGNS_DELETE_SHIFT(npart, part, cgi_free_part)
     /* GeometryFile and GeometryFormat can not be deleted */

/* Children of Elements_t */
    } else if (strcmp(posit->label,"Elements_t")==0) {
        cgns_section *parent = (cgns_section *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"ParentData")==0)
            CGNS_DELETE_CHILD(parent, cgi_free_array)
     /* ElementRange and ElementConnectivity can not be deleted */

/* Children of RigidGridMotion_t */
    } else if (strcmp(posit->label,"RigidGridMotion_t")==0) {
        cgns_rmotion *parent = (cgns_rmotion *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of ArbitraryGridMotion_t */
    } else if (strcmp(posit->label,"ArbitraryGridMotion_t")==0) {
        cgns_amotion *parent = (cgns_amotion *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_name,"GridLocation")==0)
            parent->location=GridLocationNull;
        else if (strcmp(node_name,"Rind")==0) {
            if (posit_base && posit_zone) {
                index_dim = cg->base[posit_base-1].zone[posit_zone-1].index_dim;
            } else {
                cgi_error("Can't find IndexDimension in cg_delete");
                return CG_NO_INDEX_DIM;
            }
            for (n=0; n<2*index_dim; n++) parent->rind_planes[n] = 0;
        }

/* Children of BaseIterativeData_t */
    } else if (strcmp(posit->label,"BaseIterativeData_t")==0) {
        cgns_biter *parent = (cgns_biter *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of ZoneIterativeData_t */
    } else if (strcmp(posit->label,"ZoneIterativeData_t")==0) {
        cgns_ziter *parent = (cgns_ziter *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of UserDefinedData_t */
    } else if (strcmp(posit->label,"UserDefinedData_t")==0) {
        cgns_user_data *parent = (cgns_user_data *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
	else if (strcmp(node_name,"GridLocation")==0)
	    parent->location=GridLocationNull;
	else if (strcmp(node_name,"FamilyName")==0)
	    parent->family_name[0]='\0';
	else if (strcmp(node_name,"Ordinal")==0)
	    parent->ordinal=0;
	/* IndexRange_t PointRange & IndexArray_t PointList can't be deleted */

/* Children of Gravity_t */
    } else if (strcmp(posit->label,"Gravity_t")==0) {
        cgns_gravity *parent = (cgns_gravity *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* Children of Axisymmetry_t */
    } else if (strcmp(posit->label,"Axisymmetry_t")==0) {
        cgns_axisym *parent = (cgns_axisym *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* RotatingCoordinates_t */
    } else if (strcmp(posit->label,"RotatingCoordinates_t")==0) {
        cgns_rotating *parent = (cgns_rotating *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_label,"DataArray_t")==0)
            CGNS_DELETE_SHIFT(narrays, array, cgi_free_array)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)

/* BCProperty_t */
    } else if (strcmp(posit->label,"BCProperty_t")==0) {
        cgns_bprop *parent = (cgns_bprop *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"WallFunction")==0)
            CGNS_DELETE_CHILD(bcwall, cgi_free_bcwall)
        else if (strcmp(node_name,"Area")==0)
            CGNS_DELETE_CHILD(bcarea, cgi_free_bcarea)

/* WallFunction_t */
    } else if (strcmp(posit->label,"WallFunction_t")==0) {
        cgns_bcwall *parent = (cgns_bcwall *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        /* node WallFunctionType can't be deleted */

/* Area_t */
    } else if (strcmp(posit->label,"Area_t")==0) {
        cgns_bcarea *parent = (cgns_bcarea *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        /* nodes AreaType, SurfaceArea and RegionName can't be deleted */

/* GridConnectivityProperty_t */
    } else if (strcmp(posit->label,"GridConnectivityProperty_t")==0) {
        cgns_cprop *parent = (cgns_cprop *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"Periodic")==0)
            CGNS_DELETE_CHILD(cperio, cgi_free_cperio)
        else if (strcmp(node_name,"AverageInterface")==0)
            CGNS_DELETE_CHILD(caverage, cgi_free_caverage)

/* Periodic_t */
    } else if (strcmp(posit->label,"Periodic_t")==0) {
        cgns_cperio *parent = (cgns_cperio *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        else if (strcmp(node_name,"DataClass")==0)
            parent->data_class = DataClassNull;
        else if (strcmp(node_name,"DimensionalUnits")==0)
            CGNS_DELETE_CHILD(units, cgi_free_units)
        /* RotationCenter, RotationAngle and Translation can't be deleted */

/* AverageInterface_t */
    } else if (strcmp(posit->label,"AverageInterface_t")==0) {
        cgns_caverage *parent = (cgns_caverage *)posit->posit;
        if (strcmp(node_label,"Descriptor_t")==0)
            CGNS_DELETE_SHIFT(ndescr, descr, cgi_free_descr)
        else if (strcmp(node_label,"UserDefinedData_t")==0)
            CGNS_DELETE_SHIFT(nuser_data, user_data, cgi_free_user_data)
        /* AverageInterfaceType can't be deleted */

    } else {
        cgi_error("Unrecognized label: '%s'",posit->label);
        return CG_ERROR;
    }
    return CG_OK;
}

/*****************************************************************************\
 *            Free library malloced memory
\*****************************************************************************/

int cg_free(void *data) {
    if (data != NULL) {
        CGNS_FREE (data);
        return CG_OK;
    }
    return CG_ERROR;
}
