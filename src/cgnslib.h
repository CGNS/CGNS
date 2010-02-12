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

#ifndef CGNSLIB_H
#define CGNSLIB_H

#define CGNS_VERSION 3000
#define CGNS_DOTVERS 3.00

#ifndef CGNSDLL
# ifdef _WIN32
#  if defined(BUILD_DLL)
#    define CGNSDLL _declspec(dllexport)
#  elif defined(USE_DLL)
#    define CGNSDLL _declspec(dllimport)
#  else
#    define CGNSDLL
#  endif
# else
#  define CGNSDLL
# endif
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      modes for cgns file                                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CG_MODE_READ	0
#define CG_MODE_WRITE	1
#define CG_MODE_MODIFY  2
#define CG_MODE_CLOSED  3

/* file types */

#define CG_FILE_NONE 0
#define CG_FILE_ADF  1
#define CG_FILE_HDF5 2
#define CG_FILE_XML  3

/* function return codes */

#define CG_OK		  0
#define CG_ERROR	  1
#define CG_NODE_NOT_FOUND 2
#define CG_INCORRECT_PATH 3
#define CG_NO_INDEX_DIM   4

/* Null and UserDefined enums */

#define CG_Null        0
#define CG_UserDefined 1

/* max goto depth */

#define CG_MAX_GOTO_DEPTH 20

/* configuration options */

#define CG_CONFIG_ERROR     1
#define CG_CONFIG_COMPRESS  2
#define CG_CONFIG_SET_PATH  3
#define CG_CONFIG_ADD_PATH  4
#define CG_CONFIG_FILE_TYPE 5

#define CG_CONFIG_HDF5_COMPRESS   201

#define CG_CONFIG_XML_DELETED     301
#define CG_CONFIG_XML_NAMESPACE   302
#define CG_CONFIG_XML_THRESHOLD   303
#define CG_CONFIG_XML_COMPRESSION 304

/* legacy code support */

#ifdef LEGACY_SUPPORT
#define MODE_READ	CG_MODE_READ
#define MODE_WRITE	CG_MODE_WRITE
#define MODE_MODIFY	CG_MODE_MODIFY
#define Null            CG_Null
#define UserDefined	CG_UserDefined
#define Celcius		Celsius
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *  Enumerations:  if any of this enumerations need to be modified,      *
 *	           the corresponding namelist must also be updated.      *
 *                                                                       *
 *  Any addition to an enum should be done as an addition at end of list *
 *  with an explicit declaration of the corresponding integer.           *
 *  This is required for enums stored as integers in the CGNS file or    *
 *  used in applications.                                                *
 *                                                                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Dimensional Units                                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	MassUnitsNull=0, 
	MassUnitsUserDefined=1,
	Kilogram=2, 
	Gram=3, 
	Slug=4, 
	PoundMass=5
} MassUnits_t;

typedef enum {
	LengthUnitsNull=0,
	LengthUnitsUserDefined=1,
	Meter=2, 
	Centimeter=3, 
	Millimeter=4, 
	Foot=5, 
	Inch=6
} LengthUnits_t;

typedef enum {
	TimeUnitsNull=0, 
	TimeUnitsUserDefined=1, 
	Second=2
} TimeUnits_t;

typedef enum {
	TemperatureUnitsNull=0, 
	TemperatureUnitsUserDefined=1,
	Kelvin=2, 
	Celsius=3, 
	Rankine=4, 
	Fahrenheit=5
} TemperatureUnits_t;

typedef enum {
	AngleUnitsNull=0, 
	AngleUnitsUserDefined=1, 
	Degree=2, 
	Radian=3
} AngleUnits_t;

typedef enum {
	ElectricCurrentUnitsNull=0, 
	ElectricCurrentUnitsUserDefined=1,
	Ampere=2, 
	Abampere=3, 
	Statampere=4, 
	Edison=5, 
	auCurrent=6
} ElectricCurrentUnits_t;

typedef enum {
	SubstanceAmountUnitsNull=0, 
	SubstanceAmountUnitsUserDefined=1,
	Mole=2, 
	Entities=3, 
	StandardCubicFoot=4, 
	StandardCubicMeter=5
} SubstanceAmountUnits_t;

typedef enum {
	LuminousIntensityUnitsNull=0, 
	LuminousIntensityUnitsUserDefined=1,
	Candela=2, 
	Candle=3, 
	Carcel=4, 
	Hefner=5, 
	Violle=6
} LuminousIntensityUnits_t;

#define NofValidMassUnits              6
#define NofValidLengthUnits            7
#define NofValidTimeUnits              3
#define NofValidTemperatureUnits       6
#define NofValidAngleUnits             4
#define NofValidElectricCurrentUnits   7
#define NofValidSubstanceAmountUnits   6
#define NofValidLuminousIntensityUnits 7

extern char const * MassUnitsName[NofValidMassUnits];
extern char const * LengthUnitsName[NofValidLengthUnits];
extern char const * TimeUnitsName[NofValidTimeUnits];
extern char const * TemperatureUnitsName[NofValidTemperatureUnits];
extern char const * AngleUnitsName[NofValidAngleUnits];
extern char const * ElectricCurrentUnitsName[NofValidElectricCurrentUnits];
extern char const * SubstanceAmountUnitsName[NofValidSubstanceAmountUnits];
extern char const * LuminousIntensityUnitsName[NofValidLuminousIntensityUnits];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Data Class                                                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	DataClassNull=0, 
	DataClassUserDefined=1,
	Dimensional=2, 
	NormalizedByDimensional=3,
	NormalizedByUnknownDimensional=4,
	NondimensionalParameter=5, 
	DimensionlessConstant=6
} DataClass_t;

#define NofValidDataClass 7

extern char const * DataClassName[NofValidDataClass];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	Grid Location
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	GridLocationNull=0, 
	GridLocationUserDefined=1,
        Vertex=2, 
	CellCenter=3, 
	FaceCenter=4,
        IFaceCenter=5, 
	JFaceCenter=6, 
	KFaceCenter=7, 
	EdgeCenter=8
} GridLocation_t;

#define NofValidGridLocation 9

extern char const * GridLocationName[NofValidGridLocation];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      BCData Types: Can not add types and stay forward compatible      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	BCDataTypeNull=0, 
	BCDataTypeUserDefined=1,
	Dirichlet=2, 
	Neumann=3
} BCDataType_t;

#define NofValidBCDataTypes 4

extern char const * BCDataTypeName[NofValidBCDataTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	Grid Connectivity Types 					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	GridConnectivityTypeNull=0, 
	GridConnectivityTypeUserDefined=1,
	Overset=2, 
	Abutting=3, 
	Abutting1to1=4
} GridConnectivityType_t;

#define NofValidGridConnectivityTypes 5

extern char const * GridConnectivityTypeName[NofValidGridConnectivityTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	Point Set Types: Can't add types and stay forward compatible
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	PointSetTypeNull=0, 
	PointSetTypeUserDefined=1,
        PointList=2,  
	PointListDonor=3,
        PointRange=4, 
	PointRangeDonor=5,
	ElementRange=6, 
	ElementList=7, 
	CellListDonor=8
} PointSetType_t;

#define NofValidPointSetTypes 9

extern char const * PointSetTypeName[NofValidPointSetTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Governing Equations and Physical Models Types                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	GoverningEquationsNull=0, 
	GoverningEquationsUserDefined=1,
	FullPotential=2, 
	Euler=3, 
	NSLaminar=4, 
	NSTurbulent=5,
	NSLaminarIncompressible=6, 
	NSTurbulentIncompressible=7
} GoverningEquationsType_t;

/* Any model type will accept both ModelTypeNull and ModelTypeUserDefined.
** The following models will accept these values as vaild...
**
** GasModel_t: Ideal, VanderWaals, CaloricallyPerfect, ThermallyPerfect,
**    ConstantDensity, RedlichKwong
**
** ViscosityModel_t: Constant, PowerLaw, SutherlandLaw
**
** ThermalConductivityModel_t: PowerLaw, SutherlandLaw, ConstantPrandtl
**
** TurbulenceModel_t: Algebraic_BaldwinLomax, Algebraic_CebeciSmith,
**    HalfEquation_JohnsonKing, OneEquation_BaldwinBarth,
**    OneEquation_SpalartAllmaras, TwoEquation_JonesLaunder,
**    TwoEquation_MenterSST,TwoEquation_Wilcox
**
** TurbulenceClosure_t: EddyViscosity, ReynoldsStress, ReynoldsStressAlgebraic
**
** ThermalRelaxationModel_t: Frozen, ThermalEquilib, ThermalNonequilib
**
** ChemicalKineticsModel_t: Frozen, ChemicalEquilibCurveFit,
**    ChemicalEquilibMinimization, ChemicalNonequilib
**
** EMElectricFieldModel_t: Voltage, Interpolated, Constant, Frozen
**
** EMMagneticFieldModel_t: Interpolated, Constant, Frozen
**
** EMConductivityModel_t: Constant, Frozen, Equilibrium_LinRessler,
**				Chemistry_LinRessler
*/

typedef enum {
	ModelTypeNull=0, 
	ModelTypeUserDefined=1,
	Ideal=2, 
	VanderWaals=3,
	Constant=4,
	PowerLaw=5, 
	SutherlandLaw=6,
	ConstantPrandtl=7,
	EddyViscosity=8, 
	ReynoldsStress=9, 
	ReynoldsStressAlgebraic=10,
	Algebraic_BaldwinLomax=11, 
	Algebraic_CebeciSmith=12,
	HalfEquation_JohnsonKing=13, 
	OneEquation_BaldwinBarth=14,
	OneEquation_SpalartAllmaras=15, 
	TwoEquation_JonesLaunder=16,
	TwoEquation_MenterSST=17, 
	TwoEquation_Wilcox=18,
	CaloricallyPerfect=19, 
	ThermallyPerfect=20,
	ConstantDensity=21, 
	RedlichKwong=22,
	Frozen=23, 
	ThermalEquilib=24, 
	ThermalNonequilib=25,
	ChemicalEquilibCurveFit=26, 
	ChemicalEquilibMinimization=27,
	ChemicalNonequilib=28,
	EMElectricField=29, 
	EMMagneticField=30, 
	EMConductivity=31,
	Voltage=32, 
	Interpolated=33, 
	Equilibrium_LinRessler=34, 
	Chemistry_LinRessler=35
} ModelType_t;

#define NofValidGoverningEquationsTypes 8
#define NofValidModelTypes 36

extern char const * GoverningEquationsTypeName[NofValidGoverningEquationsTypes];
extern char const * ModelTypeName[NofValidModelTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 * 	Boundary Condition Types					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	BCTypeNull=0, 
	BCTypeUserDefined=1,
	BCAxisymmetricWedge=2, 
	BCDegenerateLine=3, 
	BCDegeneratePoint=4,
	BCDirichlet=5, 
	BCExtrapolate=6, 
	BCFarfield=7, 
	BCGeneral=8, 
	BCInflow=9,
	BCInflowSubsonic=10,  
	BCInflowSupersonic=11, 
	BCNeumann=12, 
	BCOutflow=13,
	BCOutflowSubsonic=14, 
	BCOutflowSupersonic=15, 
	BCSymmetryPlane=16,
	BCSymmetryPolar=17, 
	BCTunnelInflow=18, 
	BCTunnelOutflow=19, 
	BCWall=20,
	BCWallInviscid=21, 
	BCWallViscous=22, 
	BCWallViscousHeatFlux=23,
	BCWallViscousIsothermal=24, 
	FamilySpecified=25
} BCType_t;

#define NofValidBCTypes 26

extern char const * BCTypeName[NofValidBCTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Data types:  Can not add data types and stay forward compatible  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	DataTypeNull=0, 
	DataTypeUserDefined=1, 
	Integer=2, 
	RealSingle=3,
	RealDouble=4, 
	Character=6
} DataType_t;

#define NofValidDataTypes 6

extern char const * DataTypeName[NofValidDataTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Element types                                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* PLEASE ALSO UPDATE the cgnslib.h/el_size static table */

typedef enum {
	ElementTypeNull=0, 
	ElementTypeUserDefined=1,
	NODE=2, 
	BAR_2=3, 
	BAR_3=4,
	TRI_3=5, 
	TRI_6=6,
	QUAD_4=7, 
	QUAD_8=8, 
	QUAD_9=9,
	TETRA_4=10, 
	TETRA_10=11,
	PYRA_5=12, 
	PYRA_14=13,
	PENTA_6=14, 
	PENTA_15=15, 
	PENTA_18=16,
	HEXA_8=17, 
	HEXA_20=18, 
	HEXA_27=19,
	MIXED=20, 
	FAKE_NGON_n=21,
	FAKE_NFACE_n=22,
	PYRA_13=23,
	NGON_n=100,
	NFACE_n=101,
} ElementType_t;

#define NofValidElementTypes 24

extern char const * ElementTypeName[NofValidElementTypes];

#define  NPE_NODE      1
#define  NPE_BAR_2     2
#define  NPE_BAR_3     3
#define  NPE_TRI_3     3
#define  NPE_TRI_6     6
#define  NPE_QUAD_4    4
#define  NPE_QUAD_8    8
#define  NPE_QUAD_9    9
#define  NPE_TETRA_4   4
#define  NPE_TETRA_10 10
#define  NPE_PYRA_5    5
#define  NPE_PYRA_13  13
#define  NPE_PYRA_14  14
#define  NPE_PENTA_6   6
#define  NPE_PENTA_15 15
#define  NPE_PENTA_18 18
#define  NPE_HEXA_8    8
#define  NPE_HEXA_20  20
#define  NPE_HEXA_27  27
#define  NPE_MIXED     0
#define  NPE_NGON_n    0
#define  NPE_NFACE_n   0

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Zone types                                                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	ZoneTypeNull=0, 
	ZoneTypeUserDefined=1,
	Structured=2, 
	Unstructured=3
} ZoneType_t;

#define NofValidZoneTypes 4

extern char const * ZoneTypeName[NofValidZoneTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Rigid Grid Motion types						 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	RigidGridMotionTypeNull=0, 
	RigidGridMotionTypeUserDefined=1,
	ConstantRate=2, 
	VariableRate=3
} RigidGridMotionType_t;

#define NofValidRigidGridMotionTypes 4

extern char const * RigidGridMotionTypeName[NofValidRigidGridMotionTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Arbitrary Grid Motion types                                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
        ArbitraryGridMotionTypeNull=0, 
	ArbitraryGridMotionTypeUserDefined=1,
        NonDeformingGrid=2, 
	DeformingGrid=3
} ArbitraryGridMotionType_t;

#define NofValidArbitraryGridMotionTypes 4

extern char const * ArbitraryGridMotionTypeName[NofValidArbitraryGridMotionTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Simulation types					         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	SimulationTypeNull=0, 
	SimulationTypeUserDefined=1,
	TimeAccurate=2, 
	NonTimeAccurate=3
} SimulationType_t;

#define NofValidSimulationTypes 4

extern char const * SimulationTypeName[NofValidSimulationTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *	BC Property types						 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	WallFunctionTypeNull=0, 
	WallFunctionTypeUserDefined=1,
	Generic=2
} WallFunctionType_t;

typedef enum {
	AreaTypeNull=0, 
	AreaTypeUserDefined=1,
	BleedArea=2, 
	CaptureArea=3
} AreaType_t;

#define NofValidWallFunctionTypes 3
#define NofValidAreaTypes 4

extern char const * WallFunctionTypeName[NofValidWallFunctionTypes];
extern char const * AreaTypeName[NofValidAreaTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Grid Connectivity Property types				 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum {
	AverageInterfaceTypeNull=0, 
	AverageInterfaceTypeUserDefined=1,
	AverageAll=2, 
	AverageCircumferential=3, 
	AverageRadial=4, 
	AverageI=5,
	AverageJ=6, 
	AverageK=7
} AverageInterfaceType_t;

#define NofValidAverageInterfaceTypes 8

extern char const * AverageInterfaceTypeName[NofValidAverageInterfaceTypes];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      LIBRARY FUNCTIONS						 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_is_cgns(const char *filename, int *file_type);

CGNSDLL int cg_open(char const * filename, int mode, int *fn);
CGNSDLL int cg_version(int fn, float *FileVersion);
CGNSDLL int cg_close(int fn);
CGNSDLL int cg_save_as(int fn, const char *filename, int file_type,
	int follow_links);

CGNSDLL int cg_set_file_type(int file_type);
CGNSDLL int cg_get_file_type(int fn, int *file_type);
CGNSDLL int cg_root_id(int fn, double *rootid);

CGNSDLL int cg_configure(int what, void *value);

CGNSDLL int cg_error_handler(void (*)(int, char *));
CGNSDLL int cg_set_compress(int compress);
CGNSDLL int cg_get_compress(int *compress);
CGNSDLL int cg_set_path(const char *path);
CGNSDLL int cg_add_path(const char *path);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      typedef names                   				 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL const char *cg_get_name(int nnames, const char **names, int type);

CGNSDLL const char *cg_MassUnitsName(MassUnits_t type);
CGNSDLL const char *cg_LengthUnitsName(LengthUnits_t type);
CGNSDLL const char *cg_TimeUnitsName(TimeUnits_t type);
CGNSDLL const char *cg_TemperatureUnitsName(TemperatureUnits_t type);
CGNSDLL const char *cg_AngleUnitsName(AngleUnits_t type);
CGNSDLL const char *cg_ElectricCurrentUnitsName(ElectricCurrentUnits_t type);
CGNSDLL const char *cg_SubstanceAmountUnitsName(SubstanceAmountUnits_t type);
CGNSDLL const char *cg_LuminousIntensityUnitsName(LuminousIntensityUnits_t type);
CGNSDLL const char *cg_DataClassName(DataClass_t type);
CGNSDLL const char *cg_GridLocationName(GridLocation_t type);
CGNSDLL const char *cg_BCDataTypeName(BCDataType_t type);
CGNSDLL const char *cg_GridConnectivityTypeName(GridConnectivityType_t type);
CGNSDLL const char *cg_PointSetTypeName(PointSetType_t type);
CGNSDLL const char *cg_GoverningEquationsTypeName(GoverningEquationsType_t type);
CGNSDLL const char *cg_ModelTypeName(ModelType_t type);
CGNSDLL const char *cg_BCTypeName(BCType_t type);
CGNSDLL const char *cg_DataTypeName(DataType_t type);
CGNSDLL const char *cg_ElementTypeName(ElementType_t type);
CGNSDLL const char *cg_ZoneTypeName(ZoneType_t type);
CGNSDLL const char *cg_RigidGridMotionTypeName(RigidGridMotionType_t type);
CGNSDLL const char *cg_ArbitraryGridMotionTypeName(ArbitraryGridMotionType_t type);
CGNSDLL const char *cg_SimulationTypeName(SimulationType_t type);
CGNSDLL const char *cg_WallFunctionTypeName(WallFunctionType_t type);
CGNSDLL const char *cg_AreaTypeName(AreaType_t type);
CGNSDLL const char *cg_AverageInterfaceTypeName(AverageInterfaceType_t type);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write CGNSBase_t Nodes					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nbases(int fn, int *nbases);
CGNSDLL int cg_base_read(int file_number, int B, char *basename,
	int *cell_dim, int *phys_dim);
CGNSDLL int cg_base_id(int fn, int B, double *base_id);
CGNSDLL int cg_base_write(int file_number, char const * basename,
	int cell_dim, int phys_dim, int *B);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Zone_t Nodes    					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nzones(int fn, int B, int *nzones);
CGNSDLL int cg_zone_read(int fn, int B, int Z, char *zonename, int *size);
CGNSDLL int cg_zone_type(int file_number, int B, int Z, ZoneType_t *type);
CGNSDLL int cg_zone_id(int fn, int B, int Z, double *zone_id);
CGNSDLL int cg_zone_write(int fn, int B, char const * zonename,
	int const * size, ZoneType_t type, int *Z);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Family_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nfamilies(int file_number, int B, int *nfamilies);
CGNSDLL int cg_family_read(int file_number, int B, int F, char *family_name,
	int *nboco, int *ngeos);
CGNSDLL int cg_family_write(int file_number, int B, char const * family_name,
	int *F);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FamilyName_t Nodes                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_famname_read(char *family_name);
CGNSDLL int cg_famname_write(char const * family_name);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FamilyBC_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_fambc_read(int file_number, int B, int F, int BC,
	char *fambc_name, BCType_t *bocotype);
CGNSDLL int cg_fambc_write(int file_number, int B, int F,
	char const * fambc_name, BCType_t bocotype, int *BC);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryReference_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_geo_read(int file_number, int B, int F, int G, char *geo_name,
        char **geo_file, char *CAD_name, int *npart);
CGNSDLL int cg_geo_write(int file_number, int B, int F, char const * geo_name,
        char const * filename, char const * CADname, int *G);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryEntity_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_part_read(int file_number, int B, int F, int G, int P,
	char *part_name);
CGNSDLL int cg_part_write(int file_number, int B, int F, int G,
	char const * part_name, int *P);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_ngrids(int file_number, int B, int Z, int *ngrids);
CGNSDLL int cg_grid_read(int file_number, int B, int Z, int G, char *gridname);
CGNSDLL int cg_grid_write(int file_number, int B, int Z,
	char const * zcoorname, int *G);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t/DataArray_t Nodes               *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_ncoords(int fn, int B, int Z, int *ncoords);
CGNSDLL int cg_coord_info(int fn, int B, int Z, int C, DataType_t *type,
	char *coordname);
CGNSDLL int cg_coord_read(int fn, int B, int Z, char const * coordname,
	DataType_t type, int const * rmin, int const * rmax, void *coord);
CGNSDLL int cg_coord_id(int fn, int B, int Z, int C, double *coord_id);
CGNSDLL int cg_coord_write(int fn, int B, int Z, DataType_t type,
	char const * coordname, void const * coord_ptr, int *C);

CGNSDLL int cg_coord_partial_write(int fn, int B, int Z, DataType_t type,
	char const * coordname, int *rmin, int *rmax,
	void const * coord_ptr, int *C);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Elements_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nsections(int file_number, int B, int Z, int *nsections);
CGNSDLL int cg_section_read(int file_number, int B, int Z, int S,
	char *SectionName, ElementType_t *type, int *start, int *end,
        int *nbndry, int *parent_flag);
CGNSDLL int cg_elements_read(int file_number, int B, int Z, int S,
	int *elements, int *parent_data);
CGNSDLL int cg_section_write(int file_number, int B, int Z,
	char const * SectionName, ElementType_t type, int start, int end,
        int nbndry, int const * elements, int *S);
CGNSDLL int cg_parent_data_write(int file_number, int B, int Z, int S,
	int const * parent_data);
CGNSDLL int cg_npe(ElementType_t type, int *npe);
CGNSDLL int cg_ElementDataSize(int file_number, int B, int Z, int S,
	int *ElementDataSize);

CGNSDLL int cg_section_partial_write(int file_number, int B, int Z,
	char const * SectionName, ElementType_t type, int start, int end,
	int nbndry, int *S);

CGNSDLL int cg_elements_partial_write(int fn, int B, int Z, int S,
	int start, int end, int const *elements);

CGNSDLL int cg_parent_data_partial_write(int fn, int B, int Z, int S,
	int start, int end, int const *ParentData);

CGNSDLL int cg_elements_partial_read(int file_number, int B, int Z, int S,
	int start, int end, int *elements, int *parent_data);

CGNSDLL int cg_ElementPartialSize(int file_number, int B, int Z, int S,
	int start, int end, int *ElementDataSize);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FlowSolution_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


CGNSDLL int cg_nsols(int fn, int B, int Z, int *nsols);
CGNSDLL int cg_sol_info(int fn, int B, int Z, int S, char *solname,
	GridLocation_t *location);
CGNSDLL int cg_sol_id(int fn, int B, int Z,int S, double *sol_id);
CGNSDLL int cg_sol_write(int fn, int B, int Z, char const * solname,
	GridLocation_t location, int *S);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write solution DataArray_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nfields(int fn, int B, int Z, int S, int *nfields);
CGNSDLL int cg_field_info(int fn,int B,int Z,int S,int F, DataType_t *type,
	char *fieldname);
CGNSDLL int cg_field_read(int fn, int B, int Z, int S, char *fieldname,
	DataType_t type, int *rmin, int *rmax, void *field_ptr);
CGNSDLL int cg_field_id(int fn, int B, int Z,int S,int F, double *field_id);
CGNSDLL int cg_field_write(int fn,int B,int Z,int S, DataType_t type,
	char const * fieldname, void const * field_ptr, int *F);

CGNSDLL int cg_field_partial_write(int fn, int B, int Z, int S,
	DataType_t type, char const * fieldname, int *rmin, int *rmax,
	void const * field_ptr, int *F);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write OversetHoles_t Nodes  				 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nholes(int fn, int B, int Z, int *nholes);
CGNSDLL int cg_hole_info(int fn, int B, int Z, int I, char *holename,
	GridLocation_t *location, PointSetType_t *ptset_type, int *nptsets,
        int *npnts);
CGNSDLL int cg_hole_read(int fn, int B, int Z, int I, int *pnts);
CGNSDLL int cg_hole_id(int fn, int B, int Z, int I, double *hole_id);
CGNSDLL int cg_hole_write(int fn, int B, int Z, char const * holename,
	GridLocation_t location, PointSetType_t ptset_type, int nptsets,
        int npnts, int const * pnts, int *I);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity_t Nodes                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nconns(int fn, int B, int Z, int *nconns);
CGNSDLL int cg_conn_info(int file_number, int B, int Z, int I,
	char *connectname, GridLocation_t *location,
        GridConnectivityType_t *type, PointSetType_t *ptset_type, int *npnts,
        char *donorname, ZoneType_t *donor_zonetype,
        PointSetType_t *donor_ptset_type, DataType_t *donor_datatype,
        int *ndata_donor);
CGNSDLL int cg_conn_read(int file_number, int B, int Z, int I, int *pnts,
        DataType_t donor_datatype, void *donor_data);
CGNSDLL int cg_conn_id(int fn, int B, int Z, int I, double *conn_id);
CGNSDLL int cg_conn_write(int file_number, int B, int Z,
	char const * connectname, GridLocation_t location,
        GridConnectivityType_t type, PointSetType_t ptset_type, int npnts,
        int const * pnts, char const * donorname, ZoneType_t donor_zonetype,
        PointSetType_t donor_ptset_type, DataType_t donor_datatype,
        int ndata_donor, void const *donor_data, int *I);
CGNSDLL int cg_conn_write_short(int file_number, int B, int Z,
	char const * connectname, GridLocation_t location,
        GridConnectivityType_t type, PointSetType_t ptset_type,
        int npnts, int const * pnts, char const * donorname, int *I);
CGNSDLL int cg_conn_read_short(int file_number, int B, int Z, int I,
	int *pnts);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity1to1_t Nodes in a zone            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_n1to1(int fn, int B, int Z, int *n1to1);
CGNSDLL int cg_1to1_read(int fn, int B, int Z, int I, char *connectname,
	char *donorname, int *range, int *donor_range, int *transform);
CGNSDLL int cg_1to1_id(int fn, int B, int Z, int I, double *one21_id);
CGNSDLL int cg_1to1_write(int fn, int B, int Z, char const * connectname,
	char const * donorname, int const * range, int const * donor_range,
        int const * transform, int *I);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read all GridConnectivity1to1_t Nodes of a base                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_n1to1_global(int fn, int B, int *n1to1_global);
CGNSDLL int cg_1to1_read_global(int fn, int B, char **connectname,
	char **zonename, char **donorname, int **range, int **donor_range,
        int **transform);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BC_t Nodes                                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nbocos(int fn, int B, int Z, int *nbocos);
CGNSDLL int cg_boco_info(int fn, int B, int Z, int BC, char *boconame,
	BCType_t *bocotype, PointSetType_t *ptset_type, int *npnts,
	int *NormalIndex, int *NormalListFlag, DataType_t *NormalDataType,
	int *ndataset);
CGNSDLL int cg_boco_read(int fn, int B, int Z, int BC, int *pnts,
	void *NormalList);
CGNSDLL int cg_boco_id(int fn, int B, int Z, int BC, double *boco_id);
CGNSDLL int cg_boco_write(int file_number, int B, int Z,
	char const * boconame, BCType_t bocotype, PointSetType_t ptset_type,
        int npnts, int const * pnts, int *BC);
CGNSDLL int cg_boco_normal_write(int file_number, int B, int Z, int BC,
	int const * NormalIndex, int NormalListFlag,
        DataType_t NormalDataType, void const * NormalList);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCDataSet_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_dataset_read(int fn, int B, int Z, int BC, int DS, char *name,
	BCType_t *BCType, int *DirichletFlag, int *NeumannFlag);
CGNSDLL int cg_dataset_write(int file_number, int B, int Z, int BC,
	char const * name, BCType_t BCType, int *Dset);
CGNSDLL int cg_bcdataset_write(char *name, BCType_t BCType,
	BCDataType_t BCDataType);
CGNSDLL int cg_bcdataset_info(int *n_dataset);
CGNSDLL int cg_bcdataset_read(int index, char *name, BCType_t *BCType,
	int *DirichletFlag, int *NeumannFlag);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCData_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_bcdata_write(int file_number, int B, int Z, int BC, int Dset,
        BCDataType_t BCDataType);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DiscreteData_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_ndiscrete(int file_number, int B, int Z, int *ndiscrete);
CGNSDLL int cg_discrete_read(int file_number, int B, int Z, int D,
	char *discrete_name);
CGNSDLL int cg_discrete_write(int file_number, int B, int Z,
	char const * discrete_name, int *D);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RigidGridMotion_t Nodes				 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_n_rigid_motions(int file_number, int B, int Z,
	int *n_rigid_motions);
CGNSDLL int cg_rigid_motion_read(int file_number, int B, int Z, int R,
	char *name, RigidGridMotionType_t *type);
CGNSDLL int cg_rigid_motion_write(int file_number, int B, int Z,
	char const * name, RigidGridMotionType_t type, int *R);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ArbitraryGridMotion_t Nodes                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_n_arbitrary_motions(int file_number, int B, int Z,
	int *n_arbitrary_motions);
CGNSDLL int cg_arbitrary_motion_read(int file_number, int B, int Z, int A,
	char *name, ArbitraryGridMotionType_t *type);
CGNSDLL int cg_arbitrary_motion_write(int file_number, int B, int Z,
	char const * amotionname, ArbitraryGridMotionType_t type, int *A);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write SimulationType_t Node                             *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_simulation_type_read(int file_number, int B, SimulationType_t *type);
CGNSDLL int cg_simulation_type_write(int file_number, int B, SimulationType_t type);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BaseIterativeData_t Node                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_biter_read(int file_number, int B, char *bitername, int *nsteps);
CGNSDLL int cg_biter_write(int file_number, int B, char const * bitername, int nsteps);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ZoneIterativeData_t Node                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_ziter_read(int file_number, int B, int Z, char *zitername);
CGNSDLL int cg_ziter_write(int file_number, int B, int Z, char const * zitername);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Gravity_t Nodes                                   *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_gravity_read(int file_number, int B, float *gravity_vector);
CGNSDLL int cg_gravity_write(int file_number, int B, float const *gravity_vector);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Axisymmetry_t Nodes                               *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_axisym_read(int file_number, int B, float *ref_point,
	float *axis);
CGNSDLL int cg_axisym_write(int file_number, int B, float const *ref_point,
  	float const *axis);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RotatingCoordinates_t Nodes                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_rotating_read(float *rot_rate, float *rot_center);
CGNSDLL int cg_rotating_write(float const *rot_rate, float const *rot_center);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/WallFunction_t Nodes   	         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_bc_wallfunction_read(int file_number, int B, int Z, int BC,
	WallFunctionType_t *WallFunctionType);
CGNSDLL int cg_bc_wallfunction_write(int file_number, int B, int Z, int BC,
	WallFunctionType_t WallFunctionType);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/Area_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_bc_area_read(int file_number, int B, int Z, int BC,
	AreaType_t *AreaType, float *SurfaceArea, char *RegionName);
CGNSDLL int cg_bc_area_write(int file_number, int B, int Z, int BC,
	AreaType_t AreaType, float SurfaceArea, char const *RegionName);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivityProperty_t/Periodic_t Nodes       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_conn_periodic_read(int file_number, int B, int Z, int I,
	float *RotationCenter, float *RotationAngle, float *Translation);
CGNSDLL int cg_conn_periodic_write(int file_number, int B, int Z, int I,
	float const *RotationCenter, float const *RotationAngle,
	float const *Translation);
CGNSDLL int cg_1to1_periodic_write(int file_number, int B, int Z, int I,
	float const *RotationCenter, float const *RotationAngle,
	float const *Translation);
CGNSDLL int cg_1to1_periodic_read(int file_number, int B, int Z, int I,
	float *RotationCenter, float *RotationAngle, float *Translation);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *   Read and write GridConnectivityProperty_t/AverageInterface_t Nodes  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_conn_average_read(int file_number, int B, int Z, int I,
	AverageInterfaceType_t *AverageInterfaceType);
CGNSDLL int cg_conn_average_write(int file_number, int B, int Z, int I,
	AverageInterfaceType_t AverageInterfaceType);
CGNSDLL int cg_1to1_average_write(int file_number, int B, int Z, int I,
	AverageInterfaceType_t AverageInterfaceType);
CGNSDLL int cg_1to1_average_read(int file_number, int B, int Z, int I,
	AverageInterfaceType_t *AverageInterfaceType);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Variable Argument List Functions                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_goto(int file_number, int B, ...);
CGNSDLL int cg_gorel(int file_number, ...);
CGNSDLL int cg_gopath(int file_number, const char *path);
CGNSDLL int cg_golist(int file_number, int B, int depth, char **label,
	int *num);
CGNSDLL int cg_where(int *file_number, int *B, int *depth, char **label,
	int *num);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ConvergenceHistory_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_convergence_read(int *iterations, char **NormDefinitions);
CGNSDLL int cg_convergence_write(int iterations, char const * NormDefinitions);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ReferenceState_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_state_read(char **StateDescription);
CGNSDLL int cg_state_write(char const * StateDescription);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FlowEquationSet_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_equationset_read(int *EquationDimension,
        int *GoverningEquationsFlag, int *GasModelFlag,
        int *ViscosityModelFlag,     int *ThermalConductivityModelFlag,
        int *TurbulenceClosureFlag,  int *TurbulenceModelFlag);
CGNSDLL int cg_equationset_chemistry_read(int *ThermalRelaxationFlag,
	int *ChemicalKineticsFlag);
CGNSDLL int cg_equationset_elecmagn_read(int *ElecFldModelFlag,
	int *MagnFldModelFlag, int *ConductivityModelFlag);
CGNSDLL int cg_equationset_write(int EquationDimension);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GoverningEquations_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_governing_read(GoverningEquationsType_t *EquationsType);
CGNSDLL int cg_governing_write(GoverningEquationsType_t Equationstype);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Diffusion Model Nodes                             *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_diffusion_read(int *diffusion_model);
CGNSDLL int cg_diffusion_write(int const * diffusion_model);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GasModel_t, ViscosityModel_t,                     *
 *      ThermalConductivityModel_t, TurbulenceClosure_t,                 *
 *      TurbulenceModel_t, ThermalRelaxationModel_t,                     *
 *      ChemicalKineticsModel_t, EMElectricFieldModel_t,                 *
 *      EMMagneticFieldModel_t Nodes                                     *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_model_read(char const *ModelLabel, ModelType_t *ModelType);
CGNSDLL int cg_model_write(char const * ModelLabel, ModelType_t ModelType);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataArray_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_narrays(int *narrays);
CGNSDLL int cg_array_info(int A, char *ArrayName, DataType_t *DataType,
        int *DataDimension, int *DimensionVector);
CGNSDLL int cg_array_read(int A, void *Data);
CGNSDLL int cg_array_read_as(int A, DataType_t type, void *Data);
CGNSDLL int cg_array_write(char const * ArrayName, DataType_t DataType,
        int DataDimension, int const * DimensionVector, void const * Data);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write UserDefinedData_t Nodes - new in version 2.1      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nuser_data(int *nuser_data);
CGNSDLL int cg_user_data_read(int Index, char *user_data_name);
CGNSDLL int cg_user_data_write(char const * user_data_name);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write IntegralData_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nintegrals(int *nintegrals);
CGNSDLL int cg_integral_read(int IntegralDataIndex, char *IntegralDataName);
CGNSDLL int cg_integral_write(char const * IntegralDataName);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Rind_t Nodes                                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_rind_read(int *RindData);
CGNSDLL int cg_rind_write(int const * RindData);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Descriptor_t Nodes                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_ndescriptors(int *ndescriptors);
CGNSDLL int cg_descriptor_read(int descr_no, char *descr_name, char **descr_text);
CGNSDLL int cg_descriptor_write(char const * descr_name, char const * descr_text);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DimensionalUnits_t Nodes                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_nunits(int *nunits);
CGNSDLL int cg_units_read(MassUnits_t *mass, LengthUnits_t *length, TimeUnits_t *time,
        TemperatureUnits_t *temperature, AngleUnits_t *angle);
CGNSDLL int cg_units_write(MassUnits_t mass, LengthUnits_t length, TimeUnits_t time,
        TemperatureUnits_t temperature, AngleUnits_t angle);
CGNSDLL int cg_unitsfull_read(MassUnits_t *mass, LengthUnits_t *length,
	TimeUnits_t *time, TemperatureUnits_t *temperature, AngleUnits_t *angle,
	ElectricCurrentUnits_t *current, SubstanceAmountUnits_t *amount,
	LuminousIntensityUnits_t *intensity);
CGNSDLL int cg_unitsfull_write(MassUnits_t mass, LengthUnits_t length,
	TimeUnits_t time, TemperatureUnits_t temperature, AngleUnits_t angle,
	ElectricCurrentUnits_t current, SubstanceAmountUnits_t amount,
	LuminousIntensityUnits_t intensity);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DimensionalExponents_t Nodes                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_exponents_info(DataType_t *DataType);
CGNSDLL int cg_nexponents(int *numexp);
CGNSDLL int cg_exponents_read(void *exponents);
CGNSDLL int cg_exponents_write(DataType_t DataType, void const * exponents);
CGNSDLL int cg_expfull_read(void *exponents);
CGNSDLL int cg_expfull_write(DataType_t DataType, void const * exponents);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataConversion_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_conversion_info(DataType_t *DataType);
CGNSDLL int cg_conversion_read(void *ConversionFactors);
CGNSDLL int cg_conversion_write(DataType_t DataType, void const * ConversionFactors);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataClass_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_dataclass_read(DataClass_t *dataclass);
CGNSDLL int cg_dataclass_write(DataClass_t dataclass);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridLocation_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_gridlocation_read(GridLocation_t *GridLocation);
CGNSDLL int cg_gridlocation_write(GridLocation_t GridLocation);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Ordinal_t Nodes                                   *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_ordinal_read(int *Ordinal);
CGNSDLL int cg_ordinal_write(int Ordinal);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write IndexArray/Range_t Nodes  - new in version 2.4    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_ptset_info(PointSetType_t *ptset_type, int *npnts);
CGNSDLL int cg_ptset_write(PointSetType_t ptset_type, int npnts, int const *pnts);
CGNSDLL int cg_ptset_read(int *pnts);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Link Handling Functions - new in version 2.1                     *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_is_link(int *path_length);
CGNSDLL int cg_link_read(char **filename, char **link_path);
CGNSDLL int cg_link_write(char const * nodename, char const * filename,
	char const * name_in_file);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      General Delete Function						 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_delete_node(char *node_name);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Free library malloced memory					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL int cg_free(void *data);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Error Handling Functions                                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

CGNSDLL const char *cg_get_error(void);
CGNSDLL void cg_error_exit(void);
CGNSDLL void cg_error_print(void);

#ifdef __cplusplus
}
#endif
#endif
