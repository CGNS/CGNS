        MODULE cgns
          
        USE ISO_C_BINDING
        IMPLICIT NONE

#include "cgnstypes_f03.h"

#if CG_BUILD_64BIT
#  if HAVE_FORTRAN_2003
        INTEGER, PARAMETER :: CGSIZE_T = C_LONG_LONG
#  else
        INTEGER, PARAMETER :: cgint_kind = SELECTED_INT_KIND(15) ! should map to INTEGER*8 on most modern processors
        INTEGER, PARAMETER :: CGSIZE_T = cgint_kind
#  endif
        LOGICAL, PARAMETER :: CG_BUILD_64BIT_F = .TRUE.
#else
#  ifdef HAVE_FORTRAN_2003
        INTEGER, PARAMETER :: CGSIZE_T = C_INT
#  else
        INTEGER, PARAMETER :: cgint_kind = SELECTED_INT_KIND(5) ! should map to INTEGER*4 on most modern processors
        INTEGER, PARAMETER :: CGSIZE_T = cgint_kind
#  endif
        LOGICAL, PARAMETER :: CG_BUILD_64BIT_F = .FALSE.
#endif   

! Fortran version of cgnslib.h
!
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      modes for cgns file                                            *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        INTEGER(C_INT) CG_MODE_READ, CG_MODE_WRITE, CG_MODE_MODIFY
        PARAMETER (CG_MODE_READ   = 0)
        PARAMETER (CG_MODE_WRITE  = 1)
        PARAMETER (CG_MODE_MODIFY = 2)

!* legacy code support
        INTEGER(C_INT) MODE_READ, MODE_WRITE, MODE_MODIFY
        PARAMETER (MODE_READ   = 0)
        PARAMETER (MODE_WRITE  = 1)
        PARAMETER (MODE_MODIFY = 2)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      file types (found in cgnslib.h)                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

	INTEGER(C_INT), PARAMETER :: CG_FILE_NONE = 0
	INTEGER(C_INT), PARAMETER :: CG_FILE_ADF  = 1
	INTEGER(C_INT), PARAMETER :: CG_FILE_HDF5 = 2
	INTEGER(C_INT), PARAMETER :: CG_FILE_ADF2 = 3


!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      some error code (found in cgnslib.h)                           *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

	INTEGER(C_INT), PARAMETER :: CG_OK              = 0
	INTEGER(C_INT), PARAMETER :: CG_ERROR           = 1
	INTEGER(C_INT), PARAMETER :: CG_NODE_NOT_FOUND  = 2
	INTEGER(C_INT), PARAMETER :: CG_INCORRECT_PATH  = 3
	INTEGER(C_INT), PARAMETER :: CG_CG_NO_INDEX_DIM = 4

!* legacy code support
        INTEGER(C_INT) ALL_OK, ERROR, NODE_NOT_FOUND, INCORRECT_PATH
        PARAMETER (ALL_OK         = 0)
        PARAMETER (ERROR          = 1)
        PARAMETER (NODE_NOT_FOUND = 2)
        PARAMETER (INCORRECT_PATH = 3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Parallel CGNS parameters                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        ENUM, BIND(C)
            ENUMERATOR :: CGP_INDEPENDENT = 0
	    ENUMERATOR :: CGP_COLLECTIVE  = 1
        END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Dimensional Units (found in cgnslib.h)                         *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

	INTEGER(C_INT), PARAMETER :: CG_Null = 0
	INTEGER(C_INT), PARAMETER :: CG_UserDefined = 1	

!* legacy code support
        INTEGER(C_INT) Null, UserDefined
        PARAMETER (Null = 0)
        PARAMETER (UserDefined = 1)

        CHARACTER*32 MassUnitsName(0:5)

        ENUM, BIND(C)
	   ENUMERATOR :: MassUnitsNull = CG_Null
  	   ENUMERATOR :: MassUnitsUserDefined = CG_UserDefined
  	   ENUMERATOR :: Kilogram = 2
           ENUMERATOR :: Gram = 3
           ENUMERATOR :: Slug = 4
           ENUMERATOR :: PoundMass = 5
        END ENUM

        CHARACTER*32 LengthUnitsName(0:6)
	ENUM, BIND(C)
  	   ENUMERATOR :: LengthUnitsNull         = CG_Null
           ENUMERATOR :: LengthUnitsUserDefined  = CG_UserDefined
           ENUMERATOR :: Meter                   = 2
           ENUMERATOR :: Centimeter		 = 3
      	   ENUMERATOR :: Millimeter		 = 4
  	   ENUMERATOR :: Foot			 = 5
  	   ENUMERATOR :: Inch			 = 6
	END ENUM

        CHARACTER*32 TimeUnitsName(0:2)
	ENUM, BIND(C)
  	   ENUMERATOR :: TimeUnitsNull		= CG_Null
  	   ENUMERATOR :: TimeUnitsUserDefined	= CG_UserDefined
  	   ENUMERATOR :: Second			= 2
	END ENUM

        CHARACTER*32 TemperatureUnitsName(0:5)
        ENUM, BIND(C)
  	   ENUMERATOR :: TemperatureUnitsNull		= CG_Null
  	   ENUMERATOR :: TemperatureUnitsUserDefined	= CG_UserDefined
  	   ENUMERATOR :: Kelvin				= 2
  	   ENUMERATOR :: Celsius			= 3
  	   ENUMERATOR :: Rankine			= 4
  	   ENUMERATOR :: Fahrenheit			= 5
	END ENUM

        CHARACTER*32 AngleUnitsName(0:3)
        ENUM, BIND(C)
  	   ENUMERATOR :: AngleUnitsNull 	= CG_Null
  	   ENUMERATOR :: AngleUnitsUserDefined	= CG_UserDefined
  	   ENUMERATOR :: Degree			= 2
  	   ENUMERATOR :: Radian			= 3
	END ENUM

        CHARACTER*32 ElectricCurrentUnitsName(0:6)
        ENUM, BIND(C)
	  ENUMERATOR :: ElectricCurrentUnitsNull 	= CG_Null
  	  ENUMERATOR :: ElectricCurrentUnitsUserDefined = CG_UserDefined
  	  ENUMERATOR :: Ampere				= 2
  	  ENUMERATOR :: Abampere	 		= 3
  	  ENUMERATOR :: Statampere			= 4
  	  ENUMERATOR :: Edison				= 5
  	  ENUMERATOR :: auCurrent			= 6
	END ENUM

        CHARACTER*32 SubstanceAmountUnitsName(0:5)
        ENUM, BIND(C)
  	   ENUMERATOR :: SubstanceAmountUnitsNull	= CG_Null
  	   ENUMERATOR :: SubstanceAmountUnitsUserDefined= CG_UserDefined
  	   ENUMERATOR :: Mole				= 2
  	   ENUMERATOR :: Entities			= 3
  	   ENUMERATOR :: StandardCubicFoot   		= 4
  	   ENUMERATOR :: StandardCubicMeter		= 5
	END ENUM

        CHARACTER*32 LuminousIntensityUnitsName(0:6)
        ENUM, BIND(C)
  	   ENUMERATOR :: LuminousIntensityUnitsNull	  = CG_Null
  	   ENUMERATOR ::LuminousIntensityUnitsUserDefined=CG_UserDefined
  	   ENUMERATOR :: Candela			  = 2
  	   ENUMERATOR :: Candle 			  = 3
  	   ENUMERATOR :: Carcel				  = 4
  	   ENUMERATOR :: Hefner				  = 5
  	   ENUMERATOR :: Violle				  = 6
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Data Class (found in cgnslib.h                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        CHARACTER*32 DataClassName(0:6)
        ENUM, BIND(C)
     	   ENUMERATOR :: DataClassNull			= CG_Null
     	   ENUMERATOR :: DataClassUserDefined		= CG_UserDefined
     	   ENUMERATOR :: Dimensional 			= 2
     	   ENUMERATOR :: NormalizedByDimensional	= 3
     	   ENUMERATOR :: NormalizedByUnknownDimensional	= 4
     	   ENUMERATOR :: NondimensionalParameter	= 5
     	   ENUMERATOR :: DimensionlessConstant		= 6
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Location                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 GridLocationName(0:8)
        ENUM, BIND(C)
     	   ENUMERATOR :: GridLocationNull	= CG_Null
     	   ENUMERATOR :: GridLocationUserDefined= CG_UserDefined
     	   ENUMERATOR :: Vertex 		= 2
     	   ENUMERATOR :: CellCenter		= 3
     	   ENUMERATOR :: FaceCenter		= 4
     	   ENUMERATOR :: IFaceCenter		= 5
     	   ENUMERATOR :: JFaceCenter		= 6
     	   ENUMERATOR :: KFaceCenter		= 7
     	   ENUMERATOR :: EdgeCenter		= 8
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Connectivity Types                                        *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 GridConnectivityTypeName(0:4)
        ENUM, BIND(C)
  	   ENUMERATOR :: GridConnectivityTypeNul	= CG_Null
  	   ENUMERATOR :: GridConnectivityTypeUserDefined= CG_UserDefined
     	   ENUMERATOR :: Overset 			= 2
     	   ENUMERATOR :: Abutting 			= 3
     	   ENUMERATOR :: Abutting1to1 			= 4
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Point Set Types                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 PointSetTypeName(0:8)
        ENUM, BIND(C)
     	   ENUMERATOR :: PointSetTypeNull 	= CG_Null
     	   ENUMERATOR :: PointSetTypeUserDefined= CG_UserDefined
     	   ENUMERATOR :: PointList 		= 2
     	   ENUMERATOR :: PointListDonor 	= 3
     	   ENUMERATOR :: PointRange 		= 4
     	   ENUMERATOR :: PointRangeDonor 	= 5
     	   ENUMERATOR :: ElementRange 		= 6
     	   ENUMERATOR :: ElementList 		= 7
     	   ENUMERATOR :: CellListDonor 		= 8
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Governing Equations and Physical Models Types                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 GoverningEquationsTypeName(0:7)
        ENUM, BIND(C)
     	   ENUMERATOR :: GoverningEquationsNull		= CG_Null
     	   ENUMERATOR :: GoverningEquationsUserDefined 	= CG_UserDefined
     	   ENUMERATOR :: FullPotential 			= 2
     	   ENUMERATOR :: Euler 				= 3
     	   ENUMERATOR :: NSLaminar 			= 4
     	   ENUMERATOR :: NSTurbulent 			= 5
     	   ENUMERATOR :: NSLaminarIncompressible 	= 6
     	   ENUMERATOR :: NSTurbulentIncompressible 	= 7
	END ENUM

!** Any model type will accept both ModelTypeNull and ModelTypeUserDefined.
!** The following models will accept these values as vaild...
!**
!** GasModel_t: Ideal, VanderWaals, CaloricallyPerfect, ThermallyPerfect,
!**    ConstantDensity, RedlichKwong
!**
!** ViscosityModel_t: Constant, PowerLaw, SutherlandLaw
!**
!** ThermalConductivityModel_t: PowerLaw, SutherlandLaw, ConstantPrandtl
!**
!** TurbulenceModel_t: Algebraic_BaldwinLomax, Algebraic_CebeciSmith,
!**    HalfEquation_JohnsonKing, OneEquation_BaldwinBarth,
!**    OneEquation_SpalartAllmaras, TwoEquation_JonesLaunder,
!**    TwoEquation_MenterSST,TwoEquation_Wilcox
!**
!** TurbulenceClosure_t: EddyViscosity, ReynoldsStress,
!**    ReynoldsStressAlgebraic
!**
!** ThermalRelaxationModel_t: Frozen, ThermalEquilib, ThermalNonequilib
!**
!** ChemicalKineticsModel_t: Frozen, ChemicalEquilibCurveFit,
!**    ChemicalEquilibMinimization, ChemicalNonequilib
!**
!** EMElectricFieldModel_t: Voltage, Interpolated, Constant, Frozen
!**
!** EMMagneticFieldModel_t: Interpolated, Constant, Frozen
!**
!** EMConductivityModel_t: Constant, Frozen, Equilibrium_LinRessler,
!**                             Chemistry_LinRessler

        CHARACTER*32 ModelTypeName(0:35)
        ENUM, BIND(C)
  	   ENUMERATOR :: ModelTypeNull 			= CG_Null
  	   ENUMERATOR :: ModelTypeUserDefined 		= CG_UserDefined
  	   ENUMERATOR :: Ideal 				= 2
  	   ENUMERATOR :: VanderWaals 			= 3
   	   ENUMERATOR :: Constant 			= 4
  	   ENUMERATOR :: PowerLaw 			= 5
  	   ENUMERATOR :: SutherlandLaw 			= 6
  	   ENUMERATOR :: ConstantPrandtl 		= 7
  	   ENUMERATOR :: EddyViscosity			= 8
  	   ENUMERATOR :: ReynoldsStress 		= 9
  	   ENUMERATOR :: ReynoldsStressAlgebraic 	= 10
  	   ENUMERATOR :: Algebraic_BaldwinLomax 	= 11
  	   ENUMERATOR :: Algebraic_CebeciSmith 		= 12
   	   ENUMERATOR :: HalfEquation_JohnsonKing 	= 13
  	   ENUMERATOR :: OneEquation_BaldwinBarth 	= 14
  	   ENUMERATOR :: OneEquation_SpalartAllmaras 	= 15
  	   ENUMERATOR :: TwoEquation_JonesLaunder 	= 16
  	   ENUMERATOR :: TwoEquation_MenterSST 		= 17
  	   ENUMERATOR :: TwoEquation_Wilcox 		= 18
  	   ENUMERATOR :: CaloricallyPerfect 		= 19
  	   ENUMERATOR :: ThermallyPerfect 		= 20
  	   ENUMERATOR :: ConstantDensity 		= 21
  	   ENUMERATOR :: RedlichKwong 			= 22
  	   ENUMERATOR :: Frozen 			= 23
  	   ENUMERATOR :: ThermalEquilib 		= 24
  	   ENUMERATOR :: ThermalNonequilib 		= 25
  	   ENUMERATOR :: ChemicalEquilibCurveFit 	= 26
  	   ENUMERATOR :: ChemicalEquilibMinimization 	= 27
  	   ENUMERATOR :: ChemicalNonequilib 		= 28
  	   ENUMERATOR :: EMElectricField 		= 29
  	   ENUMERATOR :: EMMagneticField 		= 30
  	   ENUMERATOR :: EMConductivity 		= 31
  	   ENUMERATOR :: Voltage 			= 32
  	   ENUMERATOR :: Interpolated 			= 33
  	   ENUMERATOR :: Equilibrium_LinRessler 	= 34
  	   ENUMERATOR :: Chemistry_LinRessler 		= 35
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Boundary Condition Types                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 BCTypeName(0:25)
        ENUM, BIND(C)
     	   ENUMERATOR :: BCTypeNull 		= CG_Null
     	   ENUMERATOR :: BCTypeUserDefined 	= CG_UserDefined
     	   ENUMERATOR :: BCAxisymmetricWedge 	= 2
     	   ENUMERATOR :: BCDegenerateLine 	= 3
    	   ENUMERATOR :: BCDegeneratePoint 	= 4
     	   ENUMERATOR :: BCDirichlet 		= 5
     	   ENUMERATOR :: BCExtrapolate 		= 6
     	   ENUMERATOR :: BCFarfield 		= 7
     	   ENUMERATOR :: BCGeneral 		= 8
     	   ENUMERATOR :: BCInflow 		= 9
     	   ENUMERATOR :: BCInflowSubsonic 	= 10
     	   ENUMERATOR :: BCInflowSupersonic 	= 11
     	   ENUMERATOR :: BCNeumann 		= 12
     	   ENUMERATOR :: BCOutflow 		= 13
     	   ENUMERATOR :: BCOutflowSubsonic 	= 14
     	   ENUMERATOR :: BCOutflowSupersonic 	= 15
     	   ENUMERATOR :: BCSymmetryPlane 	= 16
      	   ENUMERATOR :: BCSymmetryPolar 	= 17
     	   ENUMERATOR :: BCTunnelInflow 	= 18
     	   ENUMERATOR :: BCTunnelOutflow 	= 19
     	   ENUMERATOR :: BCWall 		= 20
     	   ENUMERATOR :: BCWallInviscid 	= 21
     	   ENUMERATOR :: BCWallViscous 		= 22
     	   ENUMERATOR :: BCWallViscousHeatFlux 	= 23
     	   ENUMERATOR :: BCWallViscousIsothermal= 24
     	   ENUMERATOR :: FamilySpecified 	= 25
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Data types                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 DataTypeName(0:6)
        ENUM, BIND(C)
	   ENUMERATOR :: DataTypeNull,DataTypeUserDefined
           ENUMERATOR :: Integer,RealSingle
	   ENUMERATOR :: RealDouble,Character,LongInteger
        END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      BCData_t types                                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 BCDataTypeName(0:3)
        ENUM, BIND(C)
  	   ENUMERATOR :: BCDataTypeNull       	= CG_Null
  	   ENUMERATOR :: BCDataTypeUserDefined	= CG_UserDefined
  	   ENUMERATOR :: Dirichlet		= 2
  	   ENUMERATOR :: Neumann		= 3
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Element types                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        CHARACTER*32 ElementTypeName(0:39)
        ENUM, BIND(C)
	   ENUMERATOR :: ElementTypeNull, ElementTypeUserDefined ! 0, 1,
	   ENUMERATOR :: NODE, BAR_2, BAR_3                      ! 2, 3, 4,
	   ENUMERATOR :: TRI_3, TRI_6                            ! 5, 6,
	   ENUMERATOR :: QUAD_4, QUAD_8, QUAD_9                  ! 7, 8, 9,
	   ENUMERATOR :: TETRA_4, TETRA_10                       ! 10, 11,
	   ENUMERATOR :: PYRA_5, PYRA_14                         ! 12, 13,
	   ENUMERATOR :: PENTA_6, PENTA_15, PENTA_18             ! 14, 15, 16,
	   ENUMERATOR :: HEXA_8, HEXA_20, HEXA_27                ! 17, 18, 19,
	   ENUMERATOR :: MIXED, PYRA_13, NGON_n, NFACE_n         ! 20, 21, 22, 23
        END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Zone types                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        
        CHARACTER*32 ZoneTypeName(0:3)
        ENUM, BIND(C)
	   ENUMERATOR :: ZoneTypeNull, ZoneTypeUserDefined
           ENUMERATOR :: Structured, Unstructured
        END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Rigid Grid Motion types                                        *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 RigidGridMotionTypeName(0:3)
        ENUM, BIND(C)
	   ENUMERATOR :: RigidGridMotionTypeNull
	   ENUMERATOR :: RigidGridMotionTypeUserDefined
	   ENUMERATOR :: ConstantRate, VariableRate
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Arbitrary Grid Motion types                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 ArbitraryGridMotionTypeName(0:3)
        ENUM, BIND(C)
	   ENUMERATOR :: ArbitraryGridMotionTypeNull
           ENUMERATOR :: ArbitraryGridMotionTypeUserDefined
           ENUMERATOR :: NonDeformingGrid, DeformingGrid
	END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Simulation type                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        CHARACTER*32 SimulationTypeName(0:3)
        ENUM, BIND(C)
	   ENUMERATOR :: SimulationTypeNull, SimulationTypeUserDefined
	   ENUMERATOR :: TimeAccurate, NonTimeAccurate
        END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      BC Property types                                              *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        CHARACTER*32 WallFunctionTypeName(0:2)
        ENUM, BIND(C)
	   ENUMERATOR:: WallFunctionTypeNull,WallFunctionTypeUserDefined
	   ENUMERATOR:: Generic
        END ENUM

        CHARACTER*32 AreaTypeName(0:3)
        ENUM, BIND(C)
	   ENUMERATOR :: AreaTypeNull, AreaTypeUserDefined
	   ENUMERATOR :: BleedArea, CaptureArea
        END ENUM

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Connectivity Property types                               *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        CHARACTER*32 AverageInterfaceTypeName(0:7)
        ENUM, BIND(C)
	   ENUMERATOR :: AverageInterfaceTypeNull
	   ENUMERATOR :: AverageInterfaceTypeUserDefined
	   ENUMERATOR :: AverageAll, AverageCircumferential
           ENUMERATOR :: AverageRadial, AverageI
	   ENUMERATOR :: AverageJ, AverageK
        END ENUM

! For portability to Linux Absoft, all data statements were moved after the
! variables and parametres declarations

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Dimensional Units                                              *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        data MassUnitsName /'Null','UserDefined','Kilogram','Gram',     &
     &                      'Slug','PoundMass'/
        data LengthUnitsName / 'Null', 'UserDefined',                   &
     &         'Meter','Centimeter','Millimeter','Foot','Inch'/

        data TimeUnitsName /'Null','UserDefined','Second'/

        data TemperatureUnitsName /'Null','UserDefined',                &
     &         'Kelvin','Celsius','Rankine','Fahrenheit'/

        data AngleUnitsName /'Null','UserDefined','Degree','Radian'/

        data ElectricCurrentUnitsName /'Null', 'UserDefined', 'Ampere', &
     &         'Abampere', 'Statampere', 'Edison', 'a.u.'/

        data SubstanceAmountUnitsName /'Null', 'UserDefined', 'Mole',   &
     &         'Entities', 'StandardCubicFoot', 'StandardCubicMeter'/

        data LuminousIntensityUnitsName /'Null', 'UserDefined',         &
     &         'Candela', 'Candle', 'Carcel', 'Hefner', 'Violle'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Data Class                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
        data DataClassName / 'Null','UserDefined',                      &
     &          'Dimensional','NormalizedByDimensional',                &
     &          'NormalizedByUnknownDimensional',                       &
     &          'NondimensionalParameter','DimensionlessConstant'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Location                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data GridLocationName / 'Null','UserDefined',                   &
     &          'Vertex','CellCenter','FaceCenter','IFaceCenter',       &
     &          'JFaceCenter','KFaceCenter','EdgeCenter' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Connectivity Types                                        *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data GridConnectivityTypeName / 'Null','UserDefined',           &
     &          'Overset','Abutting','Abutting1to1'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Point Set Types                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data PointSetTypeName / 'Null','UserDefined',                   &
     &          'PointList','PointListDonor',                           &
     &          'PointRange','PointRangeDonor',                         &
     &          'ElementRange','ElementList','CellListDonor'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Governing Equations and Physical Models Types                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data GoverningEquationsTypeName / 'Null','UserDefined',         &
     &          'FullPotential','Euler', 'NSLaminar', 'NSTurbulent',    &
     &          'NSLaminarIncompressible', 'NSTurbulentIncompressible'/

        data ModelTypeName / 'Null','UserDefined',                      &
     &        'Ideal','VanderWaals', 'Constant','PowerLaw',             &
     &        'SutherlandLaw','ConstantPrandtl','EddyViscosity',        &
     &        'ReynoldsStress','ReynoldsStressAlgebraic',               &
     &        'Algebraic_BaldwinLomax','Algebraic_CebeciSmith',         &
     &        'HalfEquation_JohnsonKing','OneEquation_BaldwinBarth',    &
     &        'OneEquation_SpalartAllmaras','TwoEquation_JonesLaunder', &
     &        'TwoEquation_MenterSST','TwoEquation_Wilcox',             &
     &        'CaloricallyPerfect', 'ThermallyPerfect',                 &
     &        'ConstantDensity', 'RedlichKwong', 'Frozen',              &
     &        'ThermalEquilib', 'ThermalNonequilib',                    &
     &        'ChemicalEquilibCurveFit', 'ChemicalEquilibMinimization', &
     &        'ChemicalNonequilib', 'EMElectricField',                  &
     &        'EMMagneticField', 'EMConductivity', 'Voltage',           &
     &        'Interpolated', 'Equilibrium_LinRessler',                 &
     &        'Chemistry_LinRessler'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Boundary Condition Types                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data BCTypeName / 'Null','UserDefined',                         &
     &          'BCAxisymmetricWedge','BCDegenerateLine',               &
     &          'BCDegeneratePoint','BCDirichlet','BCExtrapolate',      &
     &          'BCFarfield','BCGeneral','BCInflow','BCInflowSubsonic', &
     &          'BCInflowSupersonic','BCNeumann','BCOutflow',           &
     &          'BCOutflowSubsonic','BCOutflowSupersonic',              &
     &          'BCSymmetryPlane','BCSymmetryPolar','BCTunnelInflow',   &
     &          'BCTunnelOutflow','BCWall','BCWallInviscid',            &
     &          'BCWallViscous','BCWallViscousHeatFlux',                &
     &          'BCWallViscousIsothermal','FamilySpecified' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Data types                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data DataTypeName / 'Null','UserDefined',                       &
     &          'Integer','RealSingle','RealDouble','Character',        &
     &          'LongInteger' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      BCData_t types                                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data BCDataTypeName / 'Null','UserDefined',                     &
     &          'Dirichlet', 'Neumann' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Element types                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data ElementTypeName / 'Null','UserDefined',                    &
     &      'NODE', 'BAR_2', 'BAR_3', 'TRI_3', 'TRI_6',                 &
     &      'QUAD_4', 'QUAD_8', 'QUAD_9', 'TETRA_4', 'TETRA_10',        &
     &      'PYRA_5', 'PYRA_14', 'PENTA_6', 'PENTA_15',                 &
     &      'PENTA_18', 'HEXA_8', 'HEXA_20', 'HEXA_27', 'MIXED',        &
     &      'PYRA_13', 'NGON_n', 'NFACE_n',                             &
     &      'BAR_4', 'TRI_9', 'TRI_10',                                 &
     &      'QUAD_12', 'QUAD_16',                                       &
     &      'TETRA_16', 'TETRA_20',                                     &
     &      'PYRA_21', 'PYRA_29', 'PYRA_30',                            &
     &      'PENTA_24', 'PENTA_38', 'PENTA_40',                         &
     &      'HEXA_32', 'HEXA_56', 'HEXA_64' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Zone types                                                     *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data ZoneTypeName / 'Null','UserDefined',                       &
     &      'Structured', 'Unstructured' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Rigid Grid Motion types                                        *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data RigidGridMotionTypeName / 'Null','UserDefined',            &
     &       'ConstantRate', 'VariableRate' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Arbitrary Grid Motion types                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data ArbitraryGridMotionTypeName / 'Null','UserDefined',        &
     &       'NonDeformingGrid', 'DeformingGrid' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Simulation type                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data SimulationTypeName / 'Null','UserDefined',                 &
     &       'TimeAccurate', 'NonTimeAccurate' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      BC Property types                                              *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data WallFunctionTypeName / 'Null','UserDefined',               &
     &       'Generic' /

        data AreaTypeName / 'Null','UserDefined',                       &
     &       'BleedArea', 'CaptureArea' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      Grid Connectivity Property types                               *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        data AverageInterfaceTypeName / 'Null','UserDefined',           &
     &       'AverageAll', 'AverageCircumferential', 'AverageRadial',   &
     &       'AverageI', 'AverageJ', 'AverageK' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      INTERFACES FOR THE FORTRAN FUNCTIONS                           *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
	
	INTERFACE
           SUBROUTINE cg_is_cgns_f(file_type, ier)
             USE ISO_C_BINDING
             INTEGER, INTENT(OUT) :: file_type
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_is_cgns_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_open_f(filename, mode, fn, ier)
             USE ISO_C_BINDING
             CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
             INTEGER, INTENT(IN)  :: mode
             INTEGER, INTENT(OUT) :: fn
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_open_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_version_f (fn,FileVersion, ier)
             INTEGER :: fn
             REAL    :: FileVersion
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_version_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_precision_f (fn, precision, ier)
             INTEGER :: fn
             INTEGER :: PRECISION
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE



!!$	INTERFACE
!!$           SUBROUTINE cg_close_f (cgint_f *fn, cgint_f *ier)
!!$ INTEGER(CGINT_F) :: fn
!!$
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_save_as_f cgint_f *fn,
!!$	STR_PSTR(filename), cgint_f *file_type, cgint_f *follow_links,
!!$	cgint_f *ier STR_PLEN(filename))
!!$ 
!!$ INTEGER(CGINT_F) :: fn
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_set_file_type_f, CG_SET_FILE_TYPE_F) (
!!$	cgint_f *ft, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_get_file_type_f, CG_GET_FILE_TYPE_F) (
!!$	cgint_f *fn, cgint_f *ft, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE

!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_set_compress_f, CG_SET_COMPRESS_F) (
!!$	cgint_f *cmpr, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_get_compress_f, CG_GET_COMPRESS_F) (
!!$	cgint_f *cmpr, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_set_path_f, CG_SET_PATH_F) (STR_PSTR(pathname),
!!$	cgint_f *ier STR_PLEN(pathname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_add_path_f, CG_ADD_PATH_F) (STR_PSTR(pathname),
!!$	cgint_f *ier STR_PLEN(pathname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_get_cgio_f, CG_GET_CGIO_F) (cgint_f *fn,
!!$	cgint_f *cgio_num, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_root_id_f, CG_ROOT_ID_F) (cgint_f *fn,
!!$	double *rootid, cgint_f *ier)
!!$
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!$!      Read and write CGNSBase_t Nodes                                  
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nbases_f, CG_NBASES_F) (cgint_f *fn,
!!$	cgint_f *nbases, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_base_read_f, CG_BASE_READ_F) (cgint_f *fn, cgint_f *B,
!!$	STR_PSTR(basename), cgint_f *cell_dim, cgint_f *phys_dim,
!!$	cgint_f *ier STR_PLEN(basename))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_base_id_f, CG_BASE_ID_F) (cgint_f *fn, cgint_f *B,
!!$	double *base_id, cgint_f *ier)
!!$
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_base_write_f, CG_BASE_WRITE_F) (cgint_f *fn,
!!$	STR_PSTR(basename), cgint_f *cell_dim, cgint_f *phys_dim,
!!$	cgint_f *B, cgint_f *ier STR_PLEN(basename))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_cell_dim_f, CG_CELL_DIM_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_t *dim, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!!$!       Read and write Zone_t Nodes                                      
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nzones_f, CG_NZONES_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *nzones, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_zone_type_f, CG_ZONE_TYPE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, CGNS_ENUMT(ZoneType_t)*type, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_zone_read_f, CG_ZONE_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, STR_PSTR(zonename), cgsize_t *size,
!!$	cgint_f *ier STR_PLEN(zonename))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_zone_id_f, CG_ZONE_ID_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, double *zone_id, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_zone_write_f, CG_ZONE_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	STR_PSTR(zonename), cgsize_t *size, CGNS_ENUMT(ZoneType_t) *type,
!!$	cgint_f *Z, cgint_f *ier STR_PLEN(zonename))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_index_dim_f, CG_INDEX_DIM_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *dim, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!      Read and write Family_t Nodes                                    *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nfamilies_f, CG_NFAMILIES_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *nfamilies, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_family_read_f, CG_FAMILY_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *F, STR_PSTR(family_name), cgint_f *nboco, cgint_f *ngeos,
!!$	cgint_f *ier STR_PLEN(family_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_family_write_f, CG_FAMILY_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	STR_PSTR(family_name), cgint_f *F, cgint_f *ier STR_PLEN(family_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nfamily_names_f, CG_NFAMILY_NAMES_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *F, cgint_f *nnames, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_family_name_read_f, CG_FAMILY_NAME_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *F, cgint_f *N, STR_PSTR(name), STR_PSTR(family),
!!$	cgint_f *ier STR_PLEN(name) STR_PLEN(family))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_family_name_write_f, CG_FAMILY_NAME_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *F, STR_PSTR(name), STR_PSTR(family),
!!$	cgint_f *ier STR_PLEN(name) STR_PLEN(family))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write FamBC_t Nodes                                     *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_fambc_read_f, CG_FAMBC_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *F, cgint_f *BC, STR_PSTR(fambc_name), CGNS_ENUMT(BCType_t) *bocotype,
!!$	cgint_f *ier STR_PLEN(fambc_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_fambc_write_f, CG_FAMBC_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *F, STR_PSTR(fambc_name), CGNS_ENUMT(BCType_t) *bocotype,
!!$	cgint_f *BC, cgint_f *ier STR_PLEN(fambc_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write GeometryReference_t Nodes                         *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_geo_read_f, CG_GEO_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *F, cgint_f *G, STR_PSTR(geo_name), STR_PSTR(geo_file),
!!$	STR_PSTR(CAD_name), cgint_f *npart, cgint_f *ier STR_PLEN(geo_name)
!!$	STR_PLEN(geo_file) STR_PLEN(CAD_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_geo_write_f, CG_GEO_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *F, STR_PSTR(geo_name), STR_PSTR(geo_file), STR_PSTR(CAD_name),
!!$	cgint_f *G, cgint_f *ier STR_PLEN(geo_name) STR_PLEN(geo_file)
!!$	STR_PLEN(CAD_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write GeometryEntity_t Nodes                            *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_part_read_f, CG_PART_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *F,cgint_f *G, cgint_f *P, STR_PSTR(part_name),
!!$	cgint_f *ier STR_PLEN(part_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_part_write_f, CG_PART_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *F, cgint_f *G, STR_PSTR(part_name), cgint_f *P,
!!$	cgint_f *ier STR_PLEN(part_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write DiscreteData_t Nodes                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ndiscrete_f, CG_NDISCRETE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *ndiscrete, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_discrete_read_f, CG_DISCRETE_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *D, STR_PSTR(discrete_name),
!!$	cgint_f *ier STR_PLEN(discrete_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_discrete_write_f, CG_DISCRETE_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, STR_PSTR(discrete_name), cgint_f *D,
!!$	cgint_f *ier STR_PLEN(discrete_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_discrete_size_f, CG_DISCRETE_SIZE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *D, cgint_f *ndim,
!!$	cgsize_t *dims, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_discrete_ptset_info_f, CG_DISCRETE_PTSET_INFO_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	CGNS_ENUMT(PointSetType_t) *ptype, cgsize_t *npnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_discrete_ptset_read_f, CG_DISCRETE_PTSET_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	cgsize_t *pnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_discrete_ptset_write_f, CG_DISCRETE_PTSET_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(name),
!!$	CGNS_ENUMT(GridLocation_t) *location, CGNS_ENUMT(PointSetType_t) *ptype, cgsize_t *npnts,
!!$	cgsize_t *pnts, cgint_f *D, cgint_f *ier STR_PLEN(name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write GridCoordinates_t/DataArray_t Nodes               *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ncoords_f, CG_NCOORDS_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *ncoords, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_coord_info_f, CG_COORD_INFO_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *C, CGNS_ENUMT(DataType_t) *type, STR_PSTR(coordname),
!!$	cgint_f *ier STR_PLEN(coordname))
!!$
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_coord_read_f, CG_COORD_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, STR_PSTR(coordname), CGNS_ENUMT(DataType_t) *type, cgsize_t *rmin,
!!$	cgsize_t *rmax, void *coord, cgint_f *ier STR_PLEN(coordname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_coord_id_f, CG_COORD_ID_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *C, double *coord_id, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_coord_write_f, CG_COORD_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, CGNS_ENUMT(DataType_t) *type, STR_PSTR(coordname), void *coord, cgint_f *C,
!!$	cgint_f *ier STR_PLEN(coordname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_coord_partial_write_f, CG_COORD_PARTIAL_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, CGNS_ENUMT(DataType_t) *type, STR_PSTR(coordname),
!!$	cgsize_t *rmin, cgsize_t *rmax, void *coord, cgint_f *C,
!!$	cgint_f *ier STR_PLEN(coordname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write Elements_t Nodes                                  *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nsections_f, CG_NSECTIONS_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *nsections, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_section_read_f, CG_SECTION_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *E, STR_PSTR(section_name),
!!$	CGNS_ENUMT(ElementType_t) *type, cgsize_t *start, cgsize_t *end, cgint_f *nbndry,
!!$	cgint_f *parent_flag, cgint_f *ier STR_PLEN(section_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_elements_read_f, CG_ELEMENTS_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *E, cgsize_t *elements,
!!$	cgsize_t *parent_data, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_elementdatasize_f, CG_ELEMENTDATASIZE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *E, cgsize_t *ElementDataSize,
!!$	cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_elementpartialsize_f, CG_ELEMENTPARTIALSIZE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *E, cgsize_t *start, cgsize_t *end,
!!$	cgsize_t *ElementDataSize, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_section_write_f, CG_SECTION_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, STR_PSTR(section_name), CGNS_ENUMT(ElementType_t)*type,
!!$	cgsize_t *start, cgsize_t *end, cgint_f *nbndry, cgsize_t *elements,
!!$	cgint_f *S, cgint_f *ier STR_PLEN(section_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_parent_data_write_f, CG_PARENT_DATA_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *parent_data, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_section_partial_write_f, CG_SECTION_PARTIAL_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(section_name),
!!$	cgsize_t *type, cgsize_t *start, cgsize_t *end, cgint_f *nbndry,
!!$	cgint_f *S, cgint_f *ier STR_PLEN(section_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_elements_partial_write_f, CG_ELEMENTS_PARTIAL_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
!!$	cgsize_t *rmax, cgsize_t *elements, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_parent_data_partial_write_f, CG_PARENT_DATA_PARTIAL_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
!!$	cgsize_t *rmax, cgsize_t *parent_data, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_elements_partial_read_f, CG_ELEMENTS_PARTIAL_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *rmin,
!!$	cgsize_t *rmax, cgsize_t *elements, cgsize_t *parent, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write FlowSolution_t Nodes                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nsols_f, CG_NSOLS_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *nsols, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_sol_info_f, CG_SOL_INFO_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *S, STR_PSTR(solname), CGNS_ENUMT(GridLocation_t) *location,
!!$	cgint_f *ier STR_PLEN(solname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_sol_id_f, CG_SOL_ID_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *S, double *sol_id, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_sol_write_f, CG_SOL_WRITE_F)(cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, STR_PSTR(solname), CGNS_ENUMT(GridLocation_t)*location, cgint_f *S,
!!$	cgint_f *ier STR_PLEN(solname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_sol_size_f, CG_SOL_SIZE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *S, cgint_f *ndim,
!!$	cgsize_t *dims, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_sol_ptset_info_f, CG_SOL_PTSET_INFO_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	CGNS_ENUMT(PointSetType_t) *ptype, cgsize_t *npnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_sol_ptset_read_f, CG_SOL_PTSET_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	cgsize_t *pnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_sol_ptset_write_f, CG_SOL_PTSET_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(name),
!!$	CGNS_ENUMT(GridLocation_t) *location, CGNS_ENUMT(PointSetType_t) *ptype, cgsize_t *npnts,
!!$	cgsize_t *pnts, cgint_f *S, cgint_f *ier STR_PLEN(name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write solution DataArray_t Nodes                        *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nfields_f, CG_NFIELDS_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *S, cgint_f *nfields, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_field_info_f, CG_FIELD_INFO_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *S, cgint_f *F, CGNS_ENUMT(DataType_t) *type, STR_PSTR(fieldname),
!!$	cgint_f *ier STR_PLEN(fieldname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_field_read_f, CG_FIELD_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *S, STR_PSTR(fieldname), CGNS_ENUMT(DataType_t) *type, cgsize_t *rmin,
!!$	cgsize_t *rmax, void *field_ptr, cgint_f *ier STR_PLEN(fieldname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_field_id_f, CG_FIELD_ID_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *S, cgint_f *F, double *field_id, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_field_write_f, CG_FIELD_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *S, CGNS_ENUMT(DataType_t) *type, STR_PSTR(fieldname), void *field_ptr,
!!$	cgint_f *F, cgint_f *ier STR_PLEN(fieldname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_field_partial_write_f, CG_FIELD_PARTIAL_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *S, CGNS_ENUMT(DataType_t) *type, STR_PSTR(fieldname),
!!$	cgsize_t *rmin, cgsize_t *rmax, void *field_ptr, cgint_f *F,
!!$	cgint_f *ier STR_PLEN(fieldname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write ZoneSubRegion_t Nodes  			         *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nsubregs_f, CG_NSUBREGS_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *nsubreg, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_subreg_info_f, CG_SUBREG_INFO_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *S, STR_PSTR(regname),
!!$	cgint_f *dimension, CGNS_ENUMT(GridLocation_t) *location,
!!$	CGNS_ENUMT(PointSetType_t) *ptset_type,
!!$	cgsize_t *npnts, cgint_f *bcname_len, cgint_f *gcname_len,
!!$	cgint_f *ier STR_PLEN(regname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_subreg_ptset_read_f, CG_SUBREG_PTSET_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	cgsize_t *pnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_subreg_bcname_read_f, CG_SUBREG_BCNAME_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	STR_PSTR(bcname), cgint_f *ier STR_PLEN(bcname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_subreg_gcname_read_f, CG_SUBREG_GCNAME_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	STR_PSTR(gcname), cgint_f *ier STR_PLEN(gcname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_subreg_ptset_write_f, CG_SUBREG_PTSET_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(regname),
!!$	cgint_f *dimension, CGNS_ENUMT(GridLocation_t) *location, CGNS_ENUMT(PointSetType_t) *ptset_type,
!!$	cgsize_t *npnts, cgsize_t *pnts, cgint_f *S,
!!$	cgint_f *ier STR_PLEN(regname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_subreg_bcname_write_f, CG_SUBREG_BCNAME_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(regname),
!!$	cgint_f *dimension, STR_PSTR(bcname), cgint_f *S,
!!$	cgint_f *ier STR_PLEN(regname) STR_PLEN(bcname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_subreg_gcname_write_f, CG_SUBREG_GCNAME_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(regname),
!!$	cgint_f *dimension, STR_PSTR(gcname), cgint_f *S,
!!$	cgint_f *ier STR_PLEN(regname) STR_PLEN(gcname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write ZoneGridConnectivity_t Nodes  			 *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nzconns_f, CG_NZCONNS_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *nzconns, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_zconn_read_f, CG_ZCONN_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *C, STR_PSTR(name),
!!$	cgint_f *ier STR_PLEN(name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_zconn_write_f, CG_ZCONN_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, STR_PSTR(name), cgint_f *C,
!!$	cgint_f *ier STR_PLEN(name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_zconn_get_f, CG_ZCONN_GET_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *C, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_zconn_set_f, CG_ZCONN_SET_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *C, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write OversetHoles_t Nodes                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nholes_f, CG_NHOLES_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *nholes, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_hole_info_f, CG_HOLE_INFO_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *I, STR_PSTR(holename), CGNS_ENUMT(GridLocation_t) *location,
!!$	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *nptsets, cgsize_t *npnts,
!!$	cgint_f *ier STR_PLEN(holename))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_hole_read_f, CG_HOLE_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *I, cgsize_t *pnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_hole_id_f, CG_HOLE_ID_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *I, double *hole_id, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_hole_write_f, CG_HOLE_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, STR_PSTR(holename), CGNS_ENUMT(GridLocation_t) *location,
!!$	CGNS_ENUMT(PointSetType_t) *ptset_type, cgint_f *nptsets, cgsize_t *npnts,
!!$	cgsize_t *pnts, cgint_f *I, cgint_f *ier STR_PLEN(holename))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write GridConnectivity_t Nodes                          *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nconns_f, CG_NCONNS_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *nconns, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_info_f, CG_CONN_INFO_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *I, STR_PSTR(connectname), CGNS_ENUMT(GridLocation_t) *location,
!!$	CGNS_ENUMT(GridConnectivityType_t) *type, CGNS_ENUMT(PointSetType_t) *ptset_type, 
!!$	cgsize_t *npnts, STR_PSTR(donorname),
!!$	CGNS_ENUMT(ZoneType_t) *donor_zonetype, 
!!$	CGNS_ENUMT(PointSetType_t) *donor_ptset_type, 
!!$	CGNS_ENUMT(DataType_t) *donor_datatype,
!!$	cgsize_t *ndata_donor, cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname)) 
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_read_f, CG_CONN_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *I, cgsize_t *pnts, CGNS_ENUMT(DataType_t) *donor_datatype,
!!$	cgsize_t *donor_data, cgint_f *ier)
!!${
!!$    *ier = (cgint_f)cg_conn_read((int)*fn, (int)*B, (int)*Z, (int)*I, pnts,
!!$               *donor_datatype, donor_data)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_read_short_f, CG_CONN_READ_SHORT_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *I, cgsize_t *pnts, cgint_f *ier)
!!${
!!$    *ier = (cgint_f)cg_conn_read_short((int)*fn, (int)*B, (int)*Z, (int)*I, pnts)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_id_f, CG_CONN_ID_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *I, double *conn_id, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_write_f, CG_CONN_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, STR_PSTR(connectname), 
!!$        CGNS_ENUMT(GridLocation_t) *location, 
!!$        CGNS_ENUMT(GridConnectivityType_t) *type,
!!$	CGNS_ENUMT(PointSetType_t) *ptset_type, 
!!$	cgsize_t *npnts, cgsize_t *pnts,
!!$	STR_PSTR(donorname), 
!!$        CGNS_ENUMT(ZoneType_t)*donor_zonetype, 
!!$        CGNS_ENUMT(PointSetType_t)*donor_ptset_type,
!!$	CGNS_ENUMT(DataType_t)*donor_datatype, cgsize_t *ndata_donor, cgsize_t *donor_data,
!!$	cgint_f *I, cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_write_short_f, CG_CONN_WRITE_SHORT_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, STR_PSTR(connectname), CGNS_ENUMT(GridLocation_t) *location,
!!$	CGNS_ENUMT(GridConnectivityType_t) *type, CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts,
!!$	cgsize_t *pnts, STR_PSTR(donorname), cgint_f *I,
!!$	cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write GridConnectivity1to1_t Nodes in a zone            *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_n1to1_f, CG_N1TO1_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f*n1to1, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_1to1_read_f, CG_1TO1_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *I, STR_PSTR(connectname), STR_PSTR(donorname),
!!$	cgsize_t *range, cgsize_t *donor_range, cgint_f *transform,
!!$	cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_1to1_id_f, CG_1TO1_ID_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *I, double *one21_id, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_1to1_write_f, CG_1TO1_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, STR_PSTR(connectname), STR_PSTR(donorname), cgsize_t *range,
!!$	cgsize_t *donor_range, cgint_f *transform, cgint_f *I,
!!$	cgint_f *ier STR_PLEN(connectname) STR_PLEN(donorname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read all GridConnectivity1to1_t Nodes of a base                  *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_n1to1_global_f, CG_N1TO1_GLOBAL_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *n1to1_global, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_1to1_read_global_f, CG_1TO1_READ_GLOBAL_F) (cgint_f *fn,
!!$	cgint_f *B, STR_PSTR(connectname), STR_PSTR(zonename), STR_PSTR(donorname),
!!$	cgsize_t *range, cgsize_t *donor_range, cgint_f *transform,
!!$	cgint_f *ier STR_PLEN(connectname) STR_PLEN(zonename) STR_PLEN(donorname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write BC_t Nodes                                        *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nbocos_f, CG_NBOCOS_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *nbocos, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_boco_info_f, CG_BOCO_INFO_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *BC, STR_PSTR(boconame), CGNS_ENUMT(BCType_t) *bocotype,
!!$	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts, cgint_f *NormalIndex,
!!$	cgsize_t *NormalListFlag, CGNS_ENUMT(DataType_t) *NormalDataType, cgint_f *ndataset,
!!$	cgint_f *ier STR_PLEN(boconame))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_boco_read_f, CG_BOCO_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *BC, cgsize_t *pnts, void *NormalList, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_boco_id_f, CG_BOCO_ID_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *BC, double *boco_id, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_boco_write_f, CG_BOCO_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, STR_PSTR(boconame), CGNS_ENUMT(BCType_t) *bocotype,
!!$	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts, cgsize_t *pnts,
!!$	cgint_f *BC, cgint_f *ier STR_PLEN(boconame))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_boco_normal_write_f, CG_BOCO_NORMAL_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
!!$	cgsize_t *NormalIndex, cgint_f *NormalListFlag,
!!$	CGNS_ENUMT(DataType_t) *NormalDataType, void *NormalList, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_boco_gridlocation_read_f, CG_BOCO_GRIDLOCATION_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
!!$	CGNS_ENUMT(GridLocation_t) *location, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_boco_gridlocation_write_f, CG_BOCO_GRIDLOCATION_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
!!$	CGNS_ENUMT(GridLocation_t) *location, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write BCProperty_t/WallFunction_t Nodes                 *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_bc_wallfunction_read_f, CG_BC_WALLFUNCTION_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
!!$	CGNS_ENUMT(WallFunctionType_t) *WallFunctionType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_bc_wallfunction_write_f, CG_BC_WALLFUNCTION_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *BC,
!!$	CGNS_ENUMT(WallFunctionType_t) *WallFunctionType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write BCProperty_t/Area_t Nodes                         *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_bc_area_read_f, CG_BC_AREA_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *BC, CGNS_ENUMT(AreaType_t) *AreaType,
!!$	float *SurfaceArea, STR_PSTR(RegionName),
!!$	cgint_f *ier STR_PLEN(RegionName))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_bc_area_write_f, CG_BC_AREA_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *BC, CGNS_ENUMT(AreaType_t) *AreaType,
!!$	float *SurfaceArea, STR_PSTR(RegionName),
!!$	cgint_f *ier STR_PLEN(RegionName))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write GridConnectivityProperty_t/Periodic_t Nodes       *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_periodic_read_f, CG_CONN_PERIODIC_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
!!$	float *RotationCenter, float *RotationAngle, float *Translation,
!!$	cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_periodic_write_f, CG_CONN_PERIODIC_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
!!$	float *RotationCenter, float *RotationAngle, float *Translation,
!!$	cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_1to1_periodic_read_f, CG_1TO1_PERIODIC_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
!!$	float *RotationCenter, float *RotationAngle, float *Translation,
!!$	cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_1to1_periodic_write_f, CG_1TO1_PERIODIC_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
!!$	float *RotationCenter, float *RotationAngle, float *Translation,
!!$	cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *   Read and write GridConnectivityProperty_t/AverageInterface_t Nodes  *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_average_read_f, CG_CONN_AVERAGE_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
!!$	CGNS_ENUMT(AverageInterfaceType_t) *AverageInterfaceType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conn_average_write_f, CG_CONN_AVERAGE_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
!!$	CGNS_ENUMT(AverageInterfaceType_t) *AverageInterfaceType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_1to1_average_read_f, CG_1TO1_AVERAGE_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
!!$	CGNS_ENUMT(AverageInterfaceType_t) *AverageInterfaceType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_1to1_average_write_f, CG_1TO1_AVERAGE_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *I,
!!$	CGNS_ENUMT(AverageInterfaceType_t) *AverageInterfaceType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write BCDataSet_t Nodes                                 *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_dataset_read_f, CG_DATASET_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *BC, cgint_f *DSet,
!!$	STR_PSTR(Dataset_name), CGNS_ENUMT(BCType_t) *BCType, cgint_f *DirichletFlag,
!!$	cgint_f *NeumannFlag, cgint_f *ier STR_PLEN(Dataset_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_dataset_write_f, CG_DATASET_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *BC, STR_PSTR(Dataset_name),
!!$	CGNS_ENUMT(BCType_t) *BCType, cgint_f *Dset, cgint_f *ier STR_PLEN(Dataset_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_bcdataset_write_f, CG_BCDATASET_WRITE_F) (
!!$	STR_PSTR(Dataset_name), CGNS_ENUMT(BCType_t) *BCType, 
!!$	CGNS_ENUMT(BCDataType_t) *BCDataType,
!!$	cgint_f *ier STR_PLEN(Dataset_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_bcdataset_info_f, CG_BCDATASET_INFO_F) (
!!$	cgint_f *ndataset, cgint_f *ier STR_PLEN(Dataset_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_bcdataset_read_f, CG_BCDATASET_READ_F) (
!!$	cgsize_t *index, STR_PSTR(Dataset_name), CGNS_ENUMT(BCType_t) *BCType,
!!$	cgint_f *DirichletFlag, cgint_f *NeumannFlag,
!!$	cgint_f *ier STR_PLEN(Dataset_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write BCData_t Nodes                                    *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_bcdata_write_f, CG_BCDATA_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *BC, cgint_f *Dset,
!!$	CGNS_ENUMT(BCDataType_t) *BCDataType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write RigidGridMotion_t Nodes                           *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_n_rigid_motions_f, CG_N_RIGID_MOTIONS_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *n_rigid_motions, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_rigid_motion_read_f, CG_RIGID_MOTION_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *R, STR_PSTR(rmotion_name),
!!$	CGNS_ENUMT(RigidGridMotionType_t) *type, cgint_f *ier STR_PLEN(rmotion_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_rigid_motion_write_f, CG_RIGID_MOTION_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, STR_PSTR(rmotion_name),
!!$	CGNS_ENUMT(RigidGridMotionType_t) *type, cgint_f *R, cgint_f *ier STR_PLEN(rmotion_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write ArbitraryGridMotion_t Nodes                       *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_n_arbitrary_motions_f, CG_N_ARBITRARY_MOTIONS_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *n_arbitrary_motions,
!!$	cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_arbitrary_motion_read_f, CG_ARBITRARY_MOTION_READ_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *A,
!!$	STR_PSTR(amotion_name), CGNS_ENUMT(ArbitraryGridMotionType_t) *type,
!!$	cgint_f *ier STR_PLEN(amotion_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_arbitrary_motion_write_f, CG_ARBITRARY_MOTION_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(amotion_name),
!!$	CGNS_ENUMT(ArbitraryGridMotionType_t) *type, cgint_f *A, cgint_f *ier STR_PLEN(amotion_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write GridCoordinates_t Nodes                           *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ngrids_f, CG_NGRIDS_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, cgint_f *ngrids, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_grid_read_f, CG_GRID_READ_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *G, STR_PSTR(gridname),
!!$	cgint_f *ier STR_PLEN(gridname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_grid_write_f, CG_GRID_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, STR_PSTR(gridname), cgint_f *G,
!!$	cgint_f *ier STR_PLEN(gridname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write SimulationType_t Node                             *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_simulation_type_read_f, CG_SIMULATION_TYPE_READ_F) (
!!$	cgint_f *fn, cgint_f *B, CGNS_ENUMT(SimulationType_t) *type, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_simulation_type_write_f, CG_SIMULATION_TYPE_WRITE_F) (
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write BaseIterativeData_t Node                          *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_biter_read_f, CG_BITER_READ_F) (cgint_f *fn,
!!$	cgint_f *B, STR_PSTR(bitername), cgint_f *nsteps,
!!$	cgint_f *ier STR_PLEN(bitername))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_biter_write_f, CG_BITER_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, STR_PSTR(bitername), cgint_f *nsteps,
!!$	cgint_f *ier STR_PLEN(bitername))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write ZoneIterativeData_t Nodes                         *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ziter_read_f, CG_ZITER_READ_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, STR_PSTR(zitername), cgint_f *ier STR_PLEN(zitername))
!!$
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ziter_write_f, CG_ZITER_WRITE_F) (cgint_f *fn, cgint_f *B,
!!$	cgint_f *Z, STR_PSTR(zitername), cgint_f *ier STR_PLEN(zitername))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write Gravity_t Node                                    *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_gravity_read_f, CG_GRAVITY_READ_F) (cgint_f *fn,
!!$	cgint_f *B, float *gravity_vector, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_gravity_write_f, CG_GRAVITY_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, float *gravity_vector, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write Axisymmetry_t Node                                *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_axisym_read_f, CG_AXISYM_READ_F) (cgint_f *fn,
!!$	cgint_f *B, float *ref_point, float *axis, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_axisym_write_f, CG_AXISYM_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, float *ref_point, float *axis, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write RotatingCoordinates_t Node                        *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_rotating_read_f, CG_ROTATING_READ_F) (
!!$	float *rot_rate, float *rot_center, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_rotating_write_f, CG_ROTATING_WRITE_F) (
!!$	float *rot_rate, float *rot_center, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Read and write  IndexArray/Range_t Nodes                         *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ptset_info_f, CG_PTSET_INFO_F) (
!!$	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ptset_read_f, CG_PTSET_READ_F) (
!!$	cgsize_t *pnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ptset_write_f, CG_PTSET_WRITE_F) (
!!$	CGNS_ENUMT(PointSetType_t) *ptset_type, cgsize_t *npnts, cgsize_t *pnts, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Go - To Function                                                 *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$#ifdef WIN32_FORTRAN
!!$CGNSDLL void __stdcall cg_goto_f(cgint_f *fn, cgint_f *B, cgint_f *ier, ...)
!!$#else
!!$	INTERFACE
!!$           SUBROUTINE cg_goto_f, CG_GOTO_F)(cgint_f *fn, cgint_f *B, cgint_f *ier, ...)
!!$#endif
!!${
!!$#ifdef _CRAY
!!$    _fcd cray_string;
!!$#endif
!!$    char *f_label[CG_MAX_GOTO_DEPTH], *label[CG_MAX_GOTO_DEPTH];
!!$    int index[CG_MAX_GOTO_DEPTH], n, i, len[CG_MAX_GOTO_DEPTH];
!!$    va_list ap;
!!$    int c_ier;
!!$
!!$     /* initialize ap to the last parameter before the variable argument list */
!!$     /* Note:  On HP, print statements btw va_start and va_end create major problems */
!!$
!!$    va_start(ap, ier);
!!$
!!$     /* read arguments */
!!$    for (n = 0; n < CG_MAX_GOTO_DEPTH; n++)  {
!!$#ifdef _CRAY
!!$        cray_string = va_arg(ap, _fcd);
!!$        f_label[n] = _fcdtocp(cray_string);
!!$        len[n] = _fcdlen(cray_string);
!!$#else
!!$        f_label[n] = va_arg(ap, char *);
!!$# ifdef WIN32_FORTRAN
!!$     /* In Windows, the arguments appear in a different order: char*, len, index,...*/
!!$        len[n] = (int)va_arg(ap, int);
!!$# endif
!!$#endif
!!$        if (f_label[n][0] == ' ' || 0 == strncmp(f_label[n],"end",3) ||
!!$            0 == strncmp(f_label[n],"END",3)) break;
!!$
!!$        index[n] = (int)*(va_arg(ap, cgint_f *));
!!$        if (index[n] < 0) {
!!$            cgi_error("Incorrect input to function cg_goto_f");
!!$            *ier = 1;
!!$            return;
!!$        }
!!$    }
!!$
!!$#if !defined(_CRAY) && !defined(WIN32_FORTRAN)
!!$    for (i=0; i<n; i++) {
!!$      len[i] = va_arg(ap, int);
!!$    }
!!$#endif
!!$    va_end(ap);
!!$
!!$     /* convert strings to C-strings */
!!$    for (i=0; i < n; i++) {
!!$        label[i] = CGNS_NEW(char,len[i]+1);
!!$        string_2_C_string(f_label[i], len[i], label[i], len[i], &c_ier);
!!$    }
!!$
!!$#if DEBUG_GOTO
!!$    printf("\nIn cg_ftoc.c: narguments=%d\n",n);
!!$    for (i=0; i<n; i++) printf("\targ %d: '%s' #%d\n",i,label[i], index[i]);
!!$#endif
!!$
!!$    *ier = (cgint_f)cgi_set_posit((int)*fn, (int)*B, n, index, label);
!!$
!!$    for (i=0; i<n; i++) CGNS_FREE(label[i]);
!!$    return
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$#ifdef WIN32_FORTRAN
!!$CGNSDLL void __stdcall cg_gorel_f(cgint_f *fn, cgint_f *ier, ...)
!!$#else
!!$	INTERFACE
!!$           SUBROUTINE cg_gorel_f, CG_GOREL_F)(cgint_f *fn, cgint_f *ier, ...)
!!$#endif
!!${
!!$#ifdef _CRAY
!!$    _fcd cray_string;
!!$#endif
!!$    char *f_label[CG_MAX_GOTO_DEPTH], *label[CG_MAX_GOTO_DEPTH];
!!$    int index[CG_MAX_GOTO_DEPTH], n, i, len[CG_MAX_GOTO_DEPTH];
!!$    va_list ap;
!!$    int c_ier;
!!$
!!$    if (posit == 0) {
!!$        cgi_error ("position not set with cg_goto");
!!$        *ier = (cgint_f)CG_ERROR;
!!$        return;
!!$    }
!!$    if ((int)*fn != posit_file) {
!!$        cgi_error("current position is in the wrong file");
!!$        *ier = (cgint_f)CG_ERROR;
!!$        return;
!!$    }
!!$
!!$     /* initialize ap to the last parameter before the variable argument list */
!!$     /* Note:  On HP, print statements btw va_start and va_end create major problems */
!!$
!!$    va_start(ap, ier);
!!$
!!$     /* read arguments */
!!$    for (n = 0; n < CG_MAX_GOTO_DEPTH; n++)  {
!!$#ifdef _CRAY
!!$        cray_string = va_arg(ap, _fcd);
!!$        f_label[n] = _fcdtocp(cray_string);
!!$        len[n] = _fcdlen(cray_string);
!!$#else
!!$        f_label[n] = va_arg(ap, char *);
!!$# ifdef WIN32_FORTRAN
!!$     /* In Windows, the arguments appear in a different order: char*, len, index,...*/
!!$        len[n] = va_arg(ap, int);
!!$# endif
!!$#endif
!!$        if (f_label[n][0] == ' ' || 0 == strncmp(f_label[n],"end",3) ||
!!$            0 == strncmp(f_label[n],"END",3)) break;
!!$
!!$        index[n] = (int)*(va_arg(ap, cgint_f *));
!!$        if (index[n] < 0) {
!!$            cgi_error("Incorrect input to function cg_goto_f");
!!$            *ier = 1;
!!$            return;
!!$        }
!!$    }
!!$
!!$#if !defined(_CRAY) && !defined(WIN32_FORTRAN)
!!$    for (i=0; i<n; i++) {
!!$        len[i] = va_arg(ap, int);
!!$    }
!!$#endif
!!$    va_end(ap);
!!$
!!$     /* convert strings to C-strings */
!!$    for (i=0; i < n; i++) {
!!$        label[i] = CGNS_NEW(char,len[i]+1);
!!$        string_2_C_string(f_label[i], len[i], label[i], len[i], &c_ier);
!!$	*ier = (cgint_f)c_ier;
!!$	
!!$    }
!!$
!!$#if DEBUG_GOTO
!!$    printf("\nIn cg_ftoc.c: narguments=%d\n",n);
!!$    for (i=0; i<n; i++) printf("\targ %d: '%s' #%d\n",i,label[i], index[i]);
!!$#endif
!!$
!!$    *ier = (cgint_f)cgi_update_posit(n, index, label);
!!$
!!$    for (i=0; i<n; i++) CGNS_FREE(label[i]);
!!$    return
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_gopath_f, CG_GOPATH_F) (cgint_f *fn,
!!$	STR_PSTR(path), cgint_f *ier STR_PLEN(path))
!!${
!!$    int length;
!!$    char *c_path;
!!$    int c_ier;
!!$
!!$    length = (int) STR_LEN(path);
!!$    c_path = CGNS_NEW(char, length+1);
!!$
!!$    string_2_C_string(STR_PTR(path), STR_LEN(path), c_path, length, &c_ier);
!!$    *ier = (cgint_f)c_ier;
!!$    if (*ier == 0)
!!$        *ier = (cgint_f)cg_gopath((int)*fn, c_path);
!!$    CGNS_FREE(c_path)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *              Read Multiple path nodes                         *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_famname_read_f, CG_FAMNAME_READ_F) (
!!$	STR_PSTR(famname), cgint_f *ier STR_PLEN(famname))
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nmultifam_f, CG_NMULTIFAM_F) (cgint_f *nfam, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_multifam_read_f, CG_MULTIFAM_READ_F) (cgint_f *N,
!!$	STR_PSTR(name), STR_PSTR(family),
!!$	cgint_f *ier STR_PLEN(name) STR_PLEN(family))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_convergence_read_f, CG_CONVERGENCE_READ_F) (
!!$	cgint_f *iterations, STR_PSTR(NormDefinitions),
!!$	cgint_f *ier STR_PLEN(NormDefinitions))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_state_size_f, CG_STATE_SIZE_F) (
!!$	cgint_f *size, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_state_read_f, CG_STATE_READ_F) (
!!$	STR_PSTR(StateDescription), cgint_f *ier STR_PLEN(StateDescription))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_equationset_read_f, CG_EQUATIONSET_READ_F) (
!!$	cgint_f *EquationDimension, cgint_f *GoverningEquationsFlag,
!!$	cgint_f *GasModelFlag, cgint_f *ViscosityModelFlag,
!!$	cgint_f *ThermalConductivityModelFlag,
!!$	cgint_f *TurbulenceClosureFlag, cgint_f *TurbulenceModelFlag,
!!$	cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_equationset_chemistry_read_f, CG_EQUATIONSET_CHEMISTRY_READ_F) (
!!$	cgint_f *ThermalRelaxationFlag, cgint_f *ChemicalKineticsFlag, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_equationset_elecmagn_read_f, CG_EQUATIONSET_ELECMAGN_READ_F) (
!!$	cgint_f *ElecFldModelFlag, cgint_f *MagnFldModelFlag,
!!$	cgint_f *ConductivityModelFlag, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_governing_read_f, CG_GOVERNING_READ_F) (
!!$	CGNS_ENUMT(GoverningEquationsType_t) *EquationsType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_diffusion_read_f, CG_DIFFUSION_READ_F) (
!!$	cgint_f *diffusion_model, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_model_read_f, CG_MODEL_READ_F) (STR_PSTR(ModelLabel),
!!$	CGNS_ENUMT(ModelType_t) *ModelType, cgint_f *ier STR_PLEN(ModelLabel))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_narrays_f, CG_NARRAYS_F) (cgint_f *narrays, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_array_info_f, CG_ARRAY_INFO_F) (cgint_f *A,
!!$	STR_PSTR(ArrayName), CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension,
!!$	cgsize_t *DimensionVector, cgint_f *ier STR_PLEN(ArrayName))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$#ifdef WIN32_FORTRAN
!!$CGNSDLL void __stdcall cg_array_read_f(cgint_f *A, void *Data, ...)
!!${
!!$    va_list ap;
!!$    cgint_f *ier;
!!$    int DataDimension;
!!$    cgsize_t DimensionVector[CGIO_MAX_DIMENSIONS];
!!$    char ArrayName[CGIO_MAX_NAME_LENGTH+1];
!!$    CGNS_ENUMT(DataType_t) DataType;
!!$
!!$    cg_array_info((int)*A, ArrayName, &DataType, &DataDimension, DimensionVector);
!!$
!!$    va_start(ap, Data);
!!$    if (DataType == CGNS_ENUMV(Character)) (void) va_arg(ap, int);
!!$    ier = va_arg(ap, cgsize_t *);
!!$    va_end(ap);
!!$#else
!!$	INTERFACE
!!$           SUBROUTINE cg_array_read_f, CG_ARRAY_READ_F) (cgint_f *A,
!!$	void *Data, cgint_f *ier)
!!${
!!$#endif
!!$    *ier = (cgint_f)cg_array_read((int)*A, Data)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$#ifdef WIN32_FORTRAN
!!$CGNSDLL void __stdcall cg_array_read_as_f(cgint_f *A, CGNS_ENUMT(DataType_t) *type,
!!$	void *Data, ...)
!!${
!!$    va_list ap;
!!$    cgint_f *ier;
!!$    va_start(ap, Data);
!!$    if (*type == CGNS_ENUMV(Character))
!!$        (void) va_arg(ap, int);
!!$    ier = va_arg(ap, cgsize_t *);
!!$    va_end(ap);
!!$#else
!!$    	INTERFACE
!!$           SUBROUTINE cg_array_read_as_f, CG_ARRAY_READ_AS_F) (cgint_f *A,
!!$	CGNS_ENUMT(DataType_t) *type, void *Data, cgint_f *ier)
!!${
!!$#endif
!!$    *ier = (cgint_f)cg_array_read_as((int)*A, *type, Data)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nintegrals_f, CG_NINTEGRALS_F) (
!!$	cgint_f *nintegrals, cgint_f *ier)
!!${
!!$    int i_nintegrals;
!!$
!!$    *ier = (cgint_f)cg_nintegrals(&i_nintegrals);
!!$    *nintegrals = (cgint_f)i_nintegrals
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_integral_read_f, CG_INTEGRAL_READ_F) (
!!$	cgint_f *IntegralDataIndex, STR_PSTR(IntegralDataName),
!!$	cgint_f *ier STR_PLEN(IntegralDataName))
!!${
!!$    char c_name[CGIO_MAX_NAME_LENGTH+1];
!!$    int c_ier;
!!$
!!$    *ier = (cgint_f)cg_integral_read((int)*IntegralDataIndex, c_name);
!!$    if (!*ier) {
!!$      string_2_F_string(c_name, STR_PTR(IntegralDataName),
!!$			STR_LEN(IntegralDataName), &c_ier);
!!$      *ier = (cgint_f)c_ier;
!!$    }
!!$}
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_rind_read_f, CG_RIND_READ_F) (
!!$	cgint_f *RindData, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ndescriptors_f, CG_NDESCRIPTORS_F) (
!!$	cgint_f *ndescriptors, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_descriptor_size_f, CG_DESCRIPTOR_SIZE_F) (
!!$	cgint_f *descr_no, cgint_f *descr_size, cgint_f *ier)
!!$
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_descriptor_read_f, CG_DESCRIPTOR_READ_F) (
!!$	cgint_f *descr_no, STR_PSTR(descr_name), STR_PSTR(descr_text),
!!$	cgint_f *ier STR_PLEN(descr_name)  STR_PLEN(descr_text))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nunits_f, CG_NUNITS_F) (cgint_f *nunits, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_units_read_f, CG_UNITS_READ_F) (
!!$	CGNS_ENUMT(MassUnits_t) *mass, CGNS_ENUMT(LengthUnits_t) *length,  CGNS_ENUMT(TimeUnits_t) *time,
!!$	CGNS_ENUMT(TemperatureUnits_t) *temperature, CGNS_ENUMT(AngleUnits_t) *angle, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_unitsfull_read_f, CG_UNITSFULL_READ_F) (
!!$	CGNS_ENUMT(MassUnits_t) *mass, CGNS_ENUMT(LengthUnits_t) *length, CGNS_ENUMT(TimeUnits_t) *time,
!!$	CGNS_ENUMT(TemperatureUnits_t) *temperature, CGNS_ENUMT(AngleUnits_t) *angle, CGNS_ENUMT(ElectricCurrentUnits_t) *current,
!!$	CGNS_ENUMT(SubstanceAmountUnits_t) *amount, CGNS_ENUMT(LuminousIntensityUnits_t) *intensity, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_exponents_info_f, CG_EXPONENTS_INFO_F) (
!!$	 CGNS_ENUMT(DataType_t) *DataType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nexponents_f, CG_NEXPONENTS_F) (
!!$	cgint_f*nexps, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_exponents_read_f, CG_EXPONENTS_READ_F) (
!!$	void *exponents, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_expfull_read_f, CG_EXPFULL_READ_F) (
!!$	void *exponents, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conversion_info_f, CG_CONVERSION_INFO_F) (
!!$	CGNS_ENUMT(DataType_t) *DataType, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conversion_read_f, CG_CONVERSION_READ_F) (
!!$	void *ConversionFactors, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_dataclass_read_f, CG_DATACLASS_READ_F) (
!!$	CGNS_ENUMT(DataClass_t) *dataclass, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_gridlocation_read_f, CG_GRIDLOCATION_READ_F) (
!!$	CGNS_ENUMT(GridLocation_t) *GridLocation, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ordinal_read_f, CG_ORDINAL_READ_F) (
!!$	cgint_f *Ordinal, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_npe_f, CG_NPE_F) (CGNS_ENUMT(ElementType_t) *type,
!!$	cgint_f *npe, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_is_link_f, CG_IS_LINK_F) (
!!$	cgint_f *path_length, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_link_read_f, CG_LINK_READ_F) (
!!$	STR_PSTR(filename), STR_PSTR(link_path), cgint_f *ier
!!$	STR_PLEN(filename)  STR_PLEN(link_path))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_nuser_data_f, CG_NUSER_DATA_F) (
!!$	cgint_f *nuser_data, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_user_data_read_f, CG_USER_DATA_READ_F) (cgint_f *index,
!!$	STR_PSTR(dataname), cgint_f *ier STR_PLEN(dataname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *                   Write Multiple path nodes                           *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_famname_write_f, CG_FAMNAME_WRITE_F) (
!!$	STR_PSTR(family_name), cgint_f *ier STR_PLEN(family_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_multifam_write_f, CG_MULTIFAM_WRITE_F) (
!!$	STR_PSTR(name), STR_PSTR(family),
!!$	cgint_f *ier STR_PLEN(name) STR_PLEN(family))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_convergence_write_f, CG_CONVERGENCE_WRITE_F) (
!!$	cgint_f *iterations, STR_PSTR(NormDefinitions),
!!$	cgint_f *ier STR_PLEN(NormDefinitions))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_state_write_f, CG_STATE_WRITE_F) (STR_PSTR(StateDescription),
!!$	cgint_f *ier STR_PLEN(StateDescription))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_equationset_write_f, CG_EQUATIONSET_WRITE_F) (
!!$	cgint_f *EquationDimension, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_governing_write_f, CG_GOVERNING_WRITE_F) (
!!$    CGNS_ENUMT(GoverningEquationsType_t) *Equationstype, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_diffusion_write_f, CG_DIFFUSION_WRITE_F) (
!!$	cgint_f *diffusion_model, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_model_write_f, CG_MODEL_WRITE_F) (STR_PSTR(ModelLabel),
!!$	CGNS_ENUMT(ModelType_t) *ModelType, cgint_f *ier STR_PLEN(ModelLabel))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$#ifdef WIN32_FORTRAN
!!$CGNSDLL void __stdcall cg_array_write_f(STR_PSTR(ArrayName),
!!$	CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension,
!!$	cgsize_t *DimensionVector, void *Data, ...)
!!${
!!$    va_list ap;
!!$    cgint_f *ier;
!!$    char c_name[CGIO_MAX_NAME_LENGTH+1];
!!$    int c_ier;
!!$
!!$    va_start(ap, Data);
!!$    if ((CGNS_ENUMT(DataType_t))*DataType == CGNS_ENUMV(Character))
!!$        (void) va_arg(ap, int);
!!$    ier = va_arg(ap, cgsize_t *);
!!$    va_end(ap);
!!$#else
!!$	INTERFACE
!!$           SUBROUTINE cg_array_write_f, CG_ARRAY_WRITE_F) (STR_PSTR(ArrayName),
!!$	CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension, cgsize_t *DimensionVector,
!!$	void *Data, cgint_f *ier STR_PLEN(ArrayName))
!!${
!!$    char c_name[CGIO_MAX_NAME_LENGTH+1];
!!$    int c_ier;
!!$#endif
!!$
!!$     /* convert Fortran-text-string to a C-string */
!!$    string_2_C_string(STR_PTR(ArrayName), STR_LEN(ArrayName),
!!$        c_name, CGIO_MAX_NAME_LENGTH, &c_ier);
!!$    *ier = (cgint_f)c_ier;
!!$    if (*ier == 0)
!!$        *ier = (cgint_f)cg_array_write(c_name, *DataType,
!!$                              (int)*DataDimension, DimensionVector, Data)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_integral_write_f, CG_INTEGRAL_WRITE_F) (
!!$	STR_PSTR(IntegralDataName), cgint_f *ier STR_PLEN(IntegralDataName))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_rind_write_f, CG_RIND_WRITE_F) (
!!$	cgint_f *RindData, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_descriptor_write_f, CG_DESCRIPTOR_WRITE_F) (
!!$	STR_PSTR(descr_name), STR_PSTR(descr_text),
!!$	cgint_f *ier STR_PLEN(descr_name) STR_PLEN(descr_text))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_units_write_f, CG_UNITS_WRITE_F) (
!!$	CGNS_ENUMT(MassUnits_t) *mass, CGNS_ENUMT(LengthUnits_t) *length, CGNS_ENUMT(TimeUnits_t) *time,
!!$	CGNS_ENUMT(TemperatureUnits_t) *temperature, CGNS_ENUMT(AngleUnits_t) *angle, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_unitsfull_write_f, CG_UNITSFULL_WRITE_F) (
!!$	CGNS_ENUMT(MassUnits_t) *mass, CGNS_ENUMT(LengthUnits_t) *length, CGNS_ENUMT(TimeUnits_t) *time,
!!$	CGNS_ENUMT(TemperatureUnits_t) *temperature, CGNS_ENUMT(AngleUnits_t) *angle, CGNS_ENUMT(ElectricCurrentUnits_t) *current,
!!$	CGNS_ENUMT(SubstanceAmountUnits_t) *amount, CGNS_ENUMT(LuminousIntensityUnits_t) *intensity, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_exponents_write_f, CG_EXPONENTS_WRITE_F) (
!!$	CGNS_ENUMT(DataType_t)*DataType, void *exponents, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_expfull_write_f, CG_EXPFULL_WRITE_F) (
!!$	CGNS_ENUMT(DataType_t) *DataType, void *exponents, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_conversion_write_f, CG_CONVERSION_WRITE_F) (
!!$	CGNS_ENUMT(DataType_t) *DataType, void *ConversionFactors, cgint_f *ier)
!!$
!!$          
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_dataclass_write_f, CG_DATACLASS_WRITE_F) (
!!$	CGNS_ENUMT(DataClass_t) *dataclass, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_gridlocation_write_f, CG_GRIDLOCATION_WRITE_F) (
!!$	CGNS_ENUMT(GridLocation_t) *GridLocation, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_ordinal_write_f, CG_ORDINAL_WRITE_F) (
!!$	cgint_f *Ordinal, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_link_write_f, CG_LINK_WRITE_F) (
!!$	STR_PSTR(nodename), STR_PSTR(filename), STR_PSTR(name_in_file), cgint_f *ier
!!$	STR_PLEN(nodename)  STR_PLEN(filename)  STR_PLEN(name_in_file))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_user_data_write_f, CG_USER_DATA_WRITE_F) (
!!$	STR_PSTR(dataname), cgint_f *ier STR_PLEN(dataname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      General Delete Function                      *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_delete_node_f, CG_DELETE_NODE_F) (STR_PSTR(node_name),
!!$	cgint_f *ier STR_PLEN(node_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *      Error Handling Functions                                         *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_get_error_f, CG_GET_ERROR_F) (
!!$	STR_PSTR(errmsg) STR_PLEN(errmsg))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_error_exit_f, CG_ERROR_EXIT_F) ()
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_error_print_f, CG_ERROR_PRINT_F) ()
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_exit_on_error_f, CG_EXIT_ON_ERROR_F) (cgint_f *flag)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$#ifdef BUILD_PARALLEL
!!$
!!$/*======================================================================
!!$ * parallel IO interface
!!$ *======================================================================*/
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_mpi_comm_f, CGP_MPI_COMM_F) (
!!$	cgint_f *mpi_comm_f, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_pio_mode_f, CGP_PIO_MODE_F) (
!!$	CGNS_ENUMT(PIOmode_t) *mode, int *pcg_mpi_info_f, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_queue_set_f, CGP_QUEUE_SET_F) (
!!$	cgint_f *use_queue, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_queue_flush_f, CGP_QUEUE_FLUSH_F) (cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_open_f, CGP_OPEN_F) (STR_PSTR(filename), int *mode,
!!$	cgint_f *fn, cgint_f *ier STR_PLEN(filename))
!!$
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_close_f, CGP_CLOSE_F) (cgint_f *fn, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_write_f, CGP_COORD_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, CGNS_ENUMT(DataType_t) *type, STR_PSTR(coordname),
!!$	cgint_f *C, cgint_f *ier STR_PLEN(coordname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_write_data_f, CGP_COORD_WRITE_DATA_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C,
!!$	cgsize_t *rmin, cgsize_t *rmax, void *data, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_read_data_f, CGP_COORD_READ_DATA_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C,
!!$	cgsize_t *rmin, cgsize_t *rmax, void *data, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_section_write_f, CGP_SECTION_WRITE_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, STR_PSTR(section_name),
!!$	CGNS_ENUMT(ElementType_t)*type, cgsize_t *start, cgsize_t *end, cgint_f *nbndry,
!!$	cgint_f *S, cgint_f *ier STR_PLEN(section_name))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_elements_write_data_f, CGP_ELEMENTS_WRITE_DATA_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *start,
!!$	cgsize_t *end, cgsize_t *elements, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_elements_read_data_f, CGP_ELEMENTS_READ_DATA_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S, cgsize_t *start,
!!$	cgsize_t *end, cgsize_t *elements, cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_field_write_f, CGP_FIELD_WRITE_F) (cgint_f *fn,
!!$	cgint_f *B, cgint_f *Z, cgint_f *S, CGNS_ENUMT(DataType_t) *type,
!!$	STR_PSTR(fieldname), cgint_f *F, cgint_f *ier STR_PLEN(fieldname))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_field_read_data_f, CGP_FIELD_READ_DATA_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	cgint_f *F, cgsize_t *rmin, cgsize_t *rmax, void *field_ptr,
!!$	cgint_f *ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_write_f, CGP_ARRAY_WRITE_F) (STR_PSTR(ArrayName),
!!$	CGNS_ENUMT(DataType_t) *DataType, cgint_f *DataDimension, cgsize_t *DimensionVector,
!!$	cgint_f *A, cgint_f *ier STR_PLEN(ArrayName))
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$#ifdef WIN32_FORTRAN
!!$CGNSDLL void __stdcall cgp_array_write_data_f(cgint_f *A,
!!$	cgsize_t *rmin, cgsize_t *rmax, void *data, ...)
!!${
!!$    va_list ap;
!!$    int ierr;
!!$    cgint_f *ier;
!!$    cgns_array *array;
!!$
!!$    array = cgi_array_address(CG_MODE_READ, (int)*A, "dummy", &ierr);
!!$    if (array == NULL) return;
!!$    va_start(ap, data);
!!$    if (0 == strcmp(array->data_type, "C1"))
!!$        (void) va_arg(ap, int);
!!$    ier = va_arg(ap, cgsize_t *);
!!$    va_end(ap);
!!$#else
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_write_data_f, CGP_ARRAY_WRITE_DATA_F) (
!!$	cgint_f *A, cgsize_t *rmin, cgsize_t *rmax, void *data, 
!!$	cgint_f *ier)
!!${
!!$#endif
!!$    *ier = (cgint_f)cgp_array_write_data((int)*A, rmin, rmax, data)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$#ifdef WIN32_FORTRAN
!!$CGNSDLL void __stdcall cgp_array_read_data_f(cgint_f *A,
!!$	cgsize_t *rmin, cgsize_t *rmax, void *data, ...)
!!${
!!$    va_list ap;
!!$    int ierr;
!!$    cgint_f *ier;
!!$    cgns_array *array;
!!$
!!$    array = cgi_array_address(CG_MODE_READ, (int)*A, "dummy", &ierr);
!!$    if (array == NULL) return;
!!$    va_start(ap, data);
!!$    if (0 == strcmp(array->data_type, "C1"))
!!$        (void) va_arg(ap, int);
!!$    ier = va_arg(ap, cgsize_t *);
!!$    va_end(ap);
!!$#else
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_read_data_f, CGP_ARRAY_READ_DATA_F) (
!!$	cgint_f *A, cgsize_t *rmin, cgsize_t *rmax, void *data, 
!!$	cgint_f *ier)
!!${
!!$#endif
!!$    *ier = (cgint_f)cgp_array_read_data((int)*A, rmin, rmax, data)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_error_exit_f, CGP_ERROR_EXIT_F) ()
!!${
!!$    cgp_error_exit()
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$#ifdef HDF5_HAVE_MULTI_DATASETS
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *       cgp_coord_multi_read_data Function                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$#ifdef WIN32_FORTRAN
!!$ CGNSDLL void __stdcall cgp_coord_multi_read_data_f(cgint_f *fn, cgint_f *B, cgint_t *Z, cgint_t *C, 
!!$	cgsize_t *rmin, cgsize_t *rmax, 
!!$	void *coordsX,  void *coordsY, void *coordsZ, cgint_f *ier)
!!$#else
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_multi_read_data_f, CGP_COORD_MULTI_READ_DATA_F)(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C, 
!!$	cgsize_t *rmin, cgsize_t *rmax, 
!!$	void *coordsX, void *coordsY, void *coordsZ, cgint_f *ier)
!!$#endif
!!${
!!$  *ier = (cgint_f)cgp_coord_multi_read_data((int)*fn, (int)*B, (int)*Z, C, rmin, rmax, coordsX, coordsY, coordsZ);
!!$
!!$}
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *       cgp_coord_multi_write_data Function                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$#ifdef WIN32_FORTRAN
!!$ CGNSDLL void __stdcall cgp_coord_multi_write_data_f(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C, 
!!$						    cgsize_t *rmin, cgsize_t *rmax, 
!!$						    void *coordsX,  void *coordsY, void *coordsZ, cgint_f *ier)
!!$#else
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_multi_write_data_f, CGP_COORD_MULTI_WRITE_DATA_F)(cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *C, 
!!$						    cgsize_t *rmin, cgsize_t *rmax, 
!!$						    void *coordsX, void *coordsY, void *coordsZ, cgint_f *ier)
!!$#endif
!!${
!!$  *ier = (cgint_f)cgp_coord_multi_write_data((int)*fn, (int)*B, (int)*Z, C, rmin, rmax, coordsX, coordsY, coordsZ)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *       cgp_field_multi_write_data Function                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_field_multi_write_data_f, CGP_FIELD_MULTI_WRITE_DATA_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	cgint_f *F, cgsize_t *rmin, cgsize_t *rmax, cgint_f *ier, cgsize_t *nsets, ...)
!!${
!!$  va_list ap; /* argument list passed from the API call */
!!$  int *F_c;
!!$  int n;
!!$
!!$  va_start(ap, nsets);
!!$
!!$  if(sizeof(cgsize_t)!=sizeof(int)) {
!!$    /* type cast F from cgsize_t to an int */
!!$    if ((F_c = (int *)malloc(*nsets*sizeof(int)))==NULL) {
!!$      cgi_error("Error allocating memory...");
!!$      *ier = 1;
!!$      return;
!!$    }
!!$    for (n = 0; n < *nsets; n++) {
!!$      F_c[n] = (int)F[n]; 
!!$    }
!!$    *ier = vcgp_field_multi_write_data((int)*fn, (int)*B, (int)*Z, (int)*S,
!!$				       F_c, rmin, rmax, (int)*nsets, ap);
!!$    free(F_c);
!!$  } else {
!!$    *ier = vcgp_field_multi_write_data((int)*fn, (int)*B, (int)*Z, (int)*S,
!!$				       F, rmin, rmax, (int)*nsets, ap);
!!$  }
!!$  
!!$
!!$}
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *       cgp_field_multi_read_data Function                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_field_multi_read_data_f, CGP_FIELD_MULTI_READ_DATA_F) (
!!$	cgint_f *fn, cgint_f *B, cgint_f *Z, cgint_f *S,
!!$	cgint_f *F, cgsize_t *rmin, cgsize_t *rmax, cgint_f *ier, cgint_f *nsets, ...)
!!${
!!$  va_list ap; /* argument list passed from the API call */
!!$  int *F_c;
!!$  int n;
!!$  
!!$  va_start(ap, nsets);
!!$
!!$  if(sizeof(cgsize_t)!=sizeof(int)) {
!!$    /* type cast F from cgsize_t to an int */
!!$    if ((F_c = (int *)malloc(*nsets*sizeof(int)))==NULL) {
!!$      cgi_error("Error allocating memory...");
!!$      *ier = 1;
!!$      return;
!!$    }
!!$    for (n = 0; n < *nsets; n++) {
!!$      F_c[n] = (int)F[n]; 
!!$    }
!!$    *ier = (cgint_f)vcgp_field_multi_read_data((int)*fn, (int)*B, (int)*Z, (int)*S,
!!$				    F_c, rmin, rmax, (int)*nsets, ap);
!!$  } else {
!!$    *ier = (cgint_f)vcgp_field_multi_read_data((int)*fn, (int)*B, (int)*Z, (int)*S,
!!$				    F, rmin, rmax, (int)*nsets, ap);
!!$  }
!!$    
!!$}
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *        cgp_array_multi_write_data Function                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_multi_write_data_f, CGP_ARRAY_MULTI_WRITE_DATA_F) (
!!$	cgint_f *fn, cgint_f *A, cgsize_t *rmin, cgsize_t *rmax,
!!$	cgint_f *ier, cgint_f *nsets, ...)
!!${  
!!$
!!$  va_list ap; /* argument list passed from the API call */
!!$  int *A_c;
!!$  int n;
!!$  
!!$  va_start(ap, nsets);
!!$  
!!$  if(sizeof(cgsize_t)!=sizeof(int)) {
!!$    /* type cast F from cgsize_t to an int */
!!$    if ((A_c = (int *)malloc(*nsets*sizeof(int)))==NULL) {
!!$      cgi_error("Error allocating memory...");
!!$      *ier = 1;
!!$      return;
!!$    }
!!$    for (n = 0; n < *nsets; n++) {
!!$      A_c[n] = (int)A[n]; 
!!$    }
!!$    *ier = (cgint_f)vcgp_array_multi_write_data((int)*fn, A_c, rmin, rmax, (int)*nsets, ap);
!!$  }else {
!!$    *ier = (cgint_f)vcgp_array_multi_write_data((int)*fn, A, rmin, rmax, (int)*nsets, ap);
!!$  }
!!$}
!!$
!!$/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$ *        cgp_array_multi_read_data Function                              *
!!$\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_multi_read_data_f, CGP_ARRAY_MULTI_READ_DATA_F) (
!!$	cgint_f *fn, cgint_f *A, cgsize_t *rmin, cgsize_t *rmax,
!!$	cgint_f *ier, cgint_f *nsets, ...)
!!${  
!!$
!!$  va_list ap; /* argument list passed from the API call */
!!$  int *A_c;
!!$  int n;
!!$  
!!$  
!!$  va_start(ap, nsets); 
!!$  if(sizeof(cgsize_t)!=sizeof(int)) {
!!$    /* type cast F from cgsize_t to an int */
!!$    if ((A_c = (int *)malloc(*nsets*sizeof(int)))==NULL) {
!!$      cgi_error("Error allocating memory...");
!!$      *ier = 1;
!!$      return;
!!$    }
!!$    for (n = 0; n < *nsets; n++) {
!!$      A_c[n] = (int)A[n]; 
!!$    }
!!$    *ier = (cgint_f)vcgp_array_multi_read_data((int)*fn, A_c, rmin, rmax, (int)*nsets, ap);
!!$  }else {
!!$    *ier = (cgint_f)vcgp_array_multi_read_data((int)*fn, A, rmin, rmax, (int)*nsets, ap);
!!$  }
!!$  
!!$}
!!$#endif /*HDF5_HAVE_MULTI_DATASETS*/
!!$#endif /*BUILD_PARALLEL*/
!!$
!!$


!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      INTERFACES FOR THE C FUNCTIONS                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *



        INTERFACE
           INTEGER(C_INT) FUNCTION cgp_open(filename, mode, fn) BIND(C, name='cgp_open')
             USE ISO_C_BINDING
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: filename
             INTEGER(C_INT), INTENT(IN), VALUE  :: mode
             INTEGER(C_INT), INTENT(OUT) :: fn
           END FUNCTION cgp_open
        END INTERFACE

        INTERFACE
           INTEGER(C_INT) FUNCTION cgp_pio_mode(mode, info) BIND(C, name='cgp_pio_mode')
             USE ISO_C_BINDING
             INTEGER(KIND(CGP_COLLECTIVE)), INTENT(IN), VALUE  :: mode
             INTEGER(C_INT), INTENT(IN), VALUE  :: info
           END FUNCTION cgp_pio_mode
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cg_base_write(fn, basename, cell_dim, phys_dim, B) BIND(C, name='cg_base_write')
             USE ISO_C_BINDING
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: basename
             INTEGER(C_INT)   , INTENT(IN), VALUE  :: cell_dim
             INTEGER(C_INT)   , INTENT(IN), VALUE  :: phys_dim
             INTEGER(C_INT)   , INTENT(OUT)  :: B
           END FUNCTION cg_base_write
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cg_zone_write(fn, B, zonename, nijk, itype, Z) BIND(C, name='cg_zone_write')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t 
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE  :: B
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: zonename
             INTEGER(CGSIZE_T), DIMENSION(*), INTENT(IN)  :: nijk
             INTEGER(KIND(CGP_COLLECTIVE)), INTENT(IN), VALUE  :: itype
             INTEGER(C_INT)   , INTENT(OUT)  :: Z
           END FUNCTION cg_zone_write
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cg_base_read(fn, B, basename, cell_dim, phys_dim) BIND(C, name='cg_base_read')
             USE ISO_C_BINDING
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(OUT)  :: basename
             INTEGER(C_INT)   , INTENT(OUT)  :: cell_dim
             INTEGER(C_INT)   , INTENT(OUT)  :: phys_dim
           END FUNCTION cg_base_read
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cg_zone_read(fn, B, Z, zonename, nijk) BIND(C, name='cg_zone_read')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(OUT)  :: zonename
             INTEGER(CGSIZE_T), DIMENSION(*), INTENT(OUT)  :: nijk
           END FUNCTION cg_zone_read
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_coord_write(fn, B, Z, itype, coordname, C) BIND(C, name='cgp_coord_write')
             USE ISO_C_BINDING
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: itype
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: coordname
             INTEGER(C_INT)   , INTENT(OUT)  :: C
           END FUNCTION cgp_coord_write
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_coord_write_data(fn, B, Z, C, rmin, rmax, coords) BIND(C, name='cgp_coord_write_data')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             INTEGER(C_INT)   , INTENT(IN), VALUE :: C
             INTEGER(CGSIZE_T), INTENT(IN) :: rmin
             INTEGER(CGSIZE_T), INTENT(IN) :: rmax
             TYPE(C_PTR), VALUE :: coords
           END FUNCTION cgp_coord_write_data
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_field_write(fn, B, Z, S, itype, fieldname, F) BIND(C, name='cgp_field_write')
             USE ISO_C_BINDING
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             INTEGER(C_INT)   , INTENT(IN), VALUE :: S
             INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: itype
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: fieldname
             INTEGER(C_INT)   , INTENT(OUT)  :: F
           END FUNCTION cgp_field_write
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_field_write_data(fn, B, Z, S, F, rmin, rmax, data) BIND(C, name='cgp_field_write_data')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             INTEGER(C_INT)   , INTENT(IN), VALUE :: S
             INTEGER(C_INT)   , INTENT(IN), VALUE :: F
             INTEGER(CGSIZE_T), INTENT(IN) :: rmin
             INTEGER(CGSIZE_T), INTENT(IN) :: rmax
             TYPE(C_PTR), VALUE :: data
           END FUNCTION cgp_field_write_data
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_field_read_data(fn, B, Z, S, F, rmin, rmax, data) BIND(C, name='cgp_field_read_data')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             INTEGER(C_INT)   , INTENT(IN), VALUE :: S
             INTEGER(C_INT)   , INTENT(IN), VALUE :: F
             INTEGER(CGSIZE_T), INTENT(IN) :: rmin
             INTEGER(CGSIZE_T), INTENT(IN) :: rmax
             TYPE(C_PTR), VALUE :: data
           END FUNCTION cgp_field_read_data
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_coord_read_data(fn, B, Z, C, rmin, rmax, coords) BIND(C, name='cgp_coord_read_data')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             INTEGER(C_INT)   , INTENT(IN), VALUE :: C
             INTEGER(CGSIZE_T), INTENT(IN) :: rmin
             INTEGER(CGSIZE_T), INTENT(IN) :: rmax
             TYPE(C_PTR), VALUE :: coords
           END FUNCTION cgp_coord_read_data
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_section_write(fn,B,Z,sectionname,itype,start,end,nbndry,S) BIND(C, name='cgp_section_write')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: sectionname
             INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: itype
             INTEGER(CGSIZE_T), INTENT(IN), VALUE :: start
             INTEGER(CGSIZE_T), INTENT(IN), VALUE :: end
             INTEGER(C_INT)   , INTENT(IN), VALUE :: nbndry
             INTEGER(C_INT)   , INTENT(OUT) :: S
           END FUNCTION cgp_section_write
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_array_write(arrayname,itype,DataDimension,DimensionVector,A) BIND(C, name='cgp_array_write')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: arrayname
             INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: itype
             INTEGER(C_INT)   , INTENT(IN), VALUE :: DataDimension
             INTEGER(CGSIZE_T), DIMENSION(1:DataDimension), INTENT(IN) :: DimensionVector
             INTEGER(C_INT)   , INTENT(OUT) :: A
           END FUNCTION cgp_array_write
        END INTERFACE
        
        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_array_write_data(A, rmin, rmax, data) BIND(C, name='cgp_array_write_data')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: A
             INTEGER(CGSIZE_T), INTENT(IN) :: rmin
             INTEGER(CGSIZE_T), INTENT(IN) :: rmax
             TYPE(C_PTR), VALUE :: data
           END FUNCTION cgp_array_write_data
        END INTERFACE
        
        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_elements_write_data(fn,B,Z,S,emin,emax,elements) BIND(C, name='cgp_elements_write_data')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             INTEGER(C_INT)   , INTENT(IN), VALUE :: S
             INTEGER(CGSIZE_T), INTENT(IN), VALUE :: emin
             INTEGER(CGSIZE_T), INTENT(IN), VALUE :: emax
             TYPE(C_PTR), VALUE :: elements
           END FUNCTION cgp_elements_write_data
        END INTERFACE
        
        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_elements_read_data(fn,B,Z,S,start,end,elements) BIND(C, name='cgp_elements_read_data')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             INTEGER(C_INT)   , INTENT(IN), VALUE :: S
             INTEGER(CGSIZE_T), INTENT(IN), VALUE :: start
             INTEGER(CGSIZE_T), INTENT(IN), VALUE :: end
             TYPE(C_PTR), VALUE :: elements
           END FUNCTION cgp_elements_read_data
        END INTERFACE
        
        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_array_read_data(A, rmin, rmax, data) BIND(C, name='cgp_array_read_data')
             USE ISO_C_BINDING
             IMPORT :: cgsize_t
             INTEGER(C_INT)   , INTENT(IN), VALUE :: A
             INTEGER(CGSIZE_T), INTENT(IN) :: rmin
             INTEGER(CGSIZE_T), INTENT(IN) :: rmax
             TYPE(C_PTR), VALUE :: data
           END FUNCTION cgp_array_read_data
        END INTERFACE
        
        INTERFACE     
           INTEGER(C_INT) FUNCTION cg_sol_write(fn,B,Z,solname,location,S) BIND(C, name='cg_sol_write')
             USE ISO_C_BINDING
             INTEGER(C_INT)   , INTENT(IN), VALUE :: fn
             INTEGER(C_INT)   , INTENT(IN), VALUE :: B
             INTEGER(C_INT)   , INTENT(IN), VALUE :: Z
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: solname
             INTEGER(KIND(CGP_INDEPENDENT)), INTENT(IN), VALUE :: location
             INTEGER(C_INT)   , INTENT(OUT) :: S
           END FUNCTION cg_sol_write
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_error_exit() BIND(C, name='cgp_error_exit')
             USE ISO_C_BINDING
           END FUNCTION cgp_error_exit
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_close(fn) BIND(C, name='cgp_close')
             USE ISO_C_BINDING
             INTEGER(C_INT), INTENT(IN), VALUE :: fn
           END FUNCTION cgp_close
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_queue_set(use_queue) BIND(C, name='cgp_queue_set')
             USE ISO_C_BINDING
             INTEGER(C_INT), INTENT(IN), VALUE :: use_queue
           END FUNCTION cgp_queue_set
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cgp_queue_flush() BIND(C, name='cgp_queue_flush')
             USE ISO_C_BINDING
           END FUNCTION cgp_queue_flush
        END INTERFACE

        INTERFACE     
           INTEGER(C_INT) FUNCTION cg_user_data_write(UserDataName) BIND(C, name='cg_user_data_write')
             USE ISO_C_BINDING
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN) :: UserDataName
           END FUNCTION cg_user_data_write
        END INTERFACE

#if HAVE_FORTRAN_2008TS
        ! THE FOLLOWING CODE ONLY WORKS FOR COMPILERS HAVING F2008 STANDARD EXTENSION:
        ! TS 29113 Further Interoperability of FORTRAN with C WG5/N1942

        ! The number of optional parameters should be set to
        ! CG_MAX_GOTO_DEPTH, which is currently set to 20.
        INTERFACE
           INTEGER(C_INT) FUNCTION cg_gorel(fn, &
                UserDataName1, i1, UserDataName2, i2, &
                UserDataName3, i3, UserDataName4, i4, &
                UserDataName5, i5, UserDataName6, i6, &
                UserDataName7, i7, UserDataName8, i8, &
                UserDataName9, i9, UserDataName10, i10, &
                UserDataName11, i11, UserDataName12, i12, &
                UserDataName13, i13, UserDataName14, i14, &
                UserDataName15, i15, UserDataName16, i16, &
                UserDataName17, i17, UserDataName18, i18, &
                UserDataName19, i19, UserDataName20, i20, &
                end) BIND(C, name='cg_gorel_f08')

             USE ISO_C_BINDING
             INTEGER(C_INT) , INTENT(IN), VALUE :: fn
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName1,UserDataName2, &
                  UserDataName3,UserDataName4,UserDataName5,UserDataName6,UserDataName7,UserDataName8, &
                  UserDataName9,UserDataName10,UserDataName11,UserDataName12,UserDataName13,UserDataName14, &
                  UserDataName15,UserDataName16,UserDataName17,UserDataName18,UserDataName19,UserDataName20

             INTEGER(C_INT), INTENT(IN), OPTIONAL :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16, &
                  i17, i18, i19, i20
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: end
           END FUNCTION cg_gorel
        END INTERFACE

        ! The number of optional parameters should be set to
        ! CG_MAX_GOTO_DEPTH, which is currently set to 20.

        INTERFACE
           INTEGER(C_INT) FUNCTION cg_goto(fn, B, &
                UserDataName1, i1, UserDataName2, i2, &
                UserDataName3, i3, UserDataName4, i4, &
                UserDataName5, i5, UserDataName6, i6, &
                UserDataName7, i7, UserDataName8, i8, &
                UserDataName9, i9, UserDataName10, i10, &
                UserDataName11, i11, UserDataName12, i12, &
                UserDataName13, i13, UserDataName14, i14, &
                UserDataName15, i15, UserDataName16, i16, &
                UserDataName17, i17, UserDataName18, i18, &
                UserDataName19, i19, UserDataName20, i20, &
                end) BIND(C, name='cg_goto_f08')

             USE ISO_C_BINDING
             INTEGER(C_INT) , INTENT(IN), VALUE :: fn
             INTEGER(C_INT) , INTENT(IN), VALUE :: B
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: UserDataName1,UserDataName2, &
                  UserDataName3,UserDataName4,UserDataName5,UserDataName6,UserDataName7,UserDataName8, &
                  UserDataName9,UserDataName10,UserDataName11,UserDataName12,UserDataName13,UserDataName14, &
                  UserDataName15,UserDataName16,UserDataName17,UserDataName18,UserDataName19,UserDataName20
             INTEGER(C_INT), INTENT(IN), OPTIONAL :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16, &
                  i17, i18, i19, i20
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN), OPTIONAL :: end
           END FUNCTION cg_goto
        END INTERFACE

#endif

        INTERFACE cg_get_type
           MODULE PROCEDURE cg_get_type_c_int
           MODULE PROCEDURE cg_get_type_c_long_long
           MODULE PROCEDURE cg_get_type_c_float
           MODULE PROCEDURE cg_get_type_c_double
        END INTERFACE cg_get_type
           
      CONTAINS

        FUNCTION cg_get_type_c_int(a)
          USE ISO_C_BINDING
          INTEGER(C_INT) :: a
          INTEGER(KIND(Integer)) :: cg_get_type_c_int
          cg_get_type_c_int = Integer
        END FUNCTION cg_get_type_c_int

        FUNCTION cg_get_type_c_long_long(a)
          USE ISO_C_BINDING
          INTEGER(C_LONG_LONG) :: a
          INTEGER(KIND(Longinteger)) :: cg_get_type_c_long_long 
          cg_get_type_c_long_long = LongInteger
        END FUNCTION cg_get_type_c_long_long

        FUNCTION cg_get_type_c_float(a)
          USE ISO_C_BINDING
          REAL(C_FLOAT) :: a
          INTEGER(KIND(RealSingle)) :: cg_get_type_c_float
          cg_get_type_c_float = RealSingle
        END FUNCTION cg_get_type_c_float

        FUNCTION cg_get_type_c_double(a)
          USE ISO_C_BINDING
          REAL(C_DOUBLE) :: a
          INTEGER(KIND(RealDouble)) :: cg_get_type_c_double
          cg_get_type_c_double = RealDouble
        END FUNCTION cg_get_type_c_double

      END MODULE cgns
