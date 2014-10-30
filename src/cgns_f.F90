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
#  if HAVE_FORTRAN_2003
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

        CHARACTER*32 ElementTypeName(0:56)
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
           ENUMERATOR :: BAR_4, TRI_9, TRI_10
           ENUMERATOR :: QUAD_12, QUAD_16
           ENUMERATOR :: TETRA_16, TETRA_20
           ENUMERATOR :: PYRA_21, PYRA_29, PYRA_30
           ENUMERATOR :: PENTA_24, PENTA_38, PENTA_40
           ENUMERATOR :: HEXA_32, HEXA_56, HEXA_64
           ENUMERATOR :: BAR_5, TRI_12, TRI_15
           ENUMERATOR :: QUAD_P4_16, QUAD_25
           ENUMERATOR :: TETRA_22, TETRA_34, TETRA_35
           ENUMERATOR :: PYRA_P4_29, PYRA_50, PYRA_55
           ENUMERATOR :: PENTA_33, PENTA_66, PENTA_75
           ENUMERATOR :: HEXA_44, HEXA_98, HEXA_125
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
     &      'HEXA_32', 'HEXA_56', 'HEXA_64',                            &
     &      'BAR_5', 'TRI_12', 'TRI_15',                                &
     &      'QUAD_P4_16', 'QUAD_25',                                    &
     &      'TETRA_22', 'TETRA_34', 'TETRA_35',                         &
     &      'PYRA_P4_29', 'PYRA_50', 'PYRA_55',                         &
     &      'PENTA_33', 'PENTA_66', 'PENTA_75',                         &
     &      'HEXA_44', 'HEXA_98', 'HEXA_125' /

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
!*      ENUMTYPE FOR FORTRAN FUNCTIONS                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *

        INTEGER, PARAMETER :: cgenum_t = KIND(CGP_INDEPENDENT)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
!*      INTERFACES FOR THE FORTRAN FUNCTIONS                           *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
	
	INTERFACE
           SUBROUTINE cg_is_cgns_f(file_type, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             INTEGER, INTENT(OUT) :: file_type
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_is_cgns_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_open_f(filename, mode, fn, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
             INTEGER, INTENT(IN)  :: mode
             INTEGER, INTENT(OUT) :: fn
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_open_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_version_f(fn,FileVersion, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             INTEGER :: fn
             REAL(C_FLOAT)    :: FileVersion
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_version_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_precision_f(fn, precision, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: PRECISION
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_close_f(fn, ier)
             IMPLICIT NONE
             INTEGER :: fn
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_close_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_save_as_f(fn, filename, file_type, follow_links, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             INTEGER :: fn
             CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: filename
             INTEGER :: file_type
             INTEGER :: follow_links
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_save_as_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_set_file_type_f(ft, ier)
             IMPLICIT NONE
             INTEGER :: ft
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_set_file_type_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_get_file_type_f (fn, ft, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: ft
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_set_compress_f (cmpr, ier)
             IMPLICIT NONE
             INTEGER :: cmpr
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_get_compress_f (cmpr, ier)
             INTEGER :: cmpr
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_set_path_f (pathname, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: pathname
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_add_path_f (pathname, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: pathname
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_get_cgio_f (fn, cgio_num, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: cgio_num
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_get_cgio_f
        END INTERFACE

	INTERFACE
            SUBROUTINE cg_root_id_f(fn, rootid, ier)
              USE ISO_C_BINDING
             IMPLICIT NONE
              INTEGER :: fn
              REAL(C_DOUBLE) :: rootid
              INTEGER, INTENT(OUT) :: ier
            END SUBROUTINE cg_root_id_f
        END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!      Read and write CGNSBase_t Nodes                                  
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	INTERFACE
           SUBROUTINE cg_nbases_f (fn, nbases, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: nbases
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_nbases_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_base_read_f(fn, B, basename, cell_dim, phys_dim, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: basename
             INTEGER :: cell_dim
             INTEGER :: phys_dim
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_base_read_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_base_id_f (fn, B, base_id, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             REAL(C_DOUBLE) :: base_id
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_base_id_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_base_write_f (fn, basename, cell_dim, phys_dim, B, ier)
             USE ISO_C_BINDING
             IMPLICIT NONE
             INTEGER :: fn
             CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: basename
             INTEGER :: cell_dim
             INTEGER :: phys_dim
             INTEGER :: B
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_base_write_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_cell_dim_f (fn, B, dim, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: dim
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_cell_dim_f
        END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!       Read and write Zone_t Nodes                                      
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	INTERFACE
           SUBROUTINE cg_nzones_f (fn, B, nzones, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: nzones
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_zone_type_f (fn, B, Z, type, ier)
             IMPORT :: cgenum_t, c_char 
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: Z
             INTEGER(cgenum_t) :: type
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_zone_read_f (fn, B, Z, zonename, size, ier)
             IMPORT :: cgsize_t, c_char 
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: Z
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zonename
             INTEGER(cgsize_t), DIMENSION(*) :: size
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_zone_id_f (fn, B, Z, zone_id, ier) 
             IMPORT :: c_double
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: Z
             REAL(C_DOUBLE) :: zone_id
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

!!$	INTERFACE
!!$           SUBROUTINE cg_zone_write_f (fn, B, zonename, size, TYPE, Z, ier) 
!!$             IMPORT :: cgenum_t, c_char, cgsize_t
!!$             IMPLICIT NONE
!!$             INTEGER :: fn
!!$             INTEGER :: B
!!$             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zonename
!!$             INTEGER(CGSIZE_T) :: size
!!$             INTEGER(cgenum_t) :: TYPE
!!$             INTEGER :: Z
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE

	INTERFACE
           SUBROUTINE cg_index_dim_f (fn, B, Z, dim, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: Z
             INTEGER :: dim
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_index_dim_f
        END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write Family_t Nodes                                    *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	INTERFACE
           SUBROUTINE cg_nfamilies_f (fn, B, nfamilies, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: nfamilies
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_nfamilies_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_family_read_f (fn, B, F, family_name, nboco, ngeos, ier)
             IMPORT :: c_char 
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: F
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family_name
             INTEGER :: nboco
             INTEGER :: ngeos
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_family_write_f (fn, B, family_name, F, ier)
             IMPORT :: c_char 
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family_name
             INTEGER :: F
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_nfamily_names_f (fn, B, F, nnames, ier)
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: F
             INTEGER :: nnames
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_family_name_read_f (fn, B, F, N, name, family, ier)
             IMPORT :: c_char 
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: F
             INTEGER :: N
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_family_name_write_f (fn, B, F, name, family, ier)
             IMPORT :: c_char 
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: F
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write FamBC_t Nodes                                     *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	INTERFACE
           SUBROUTINE cg_fambc_read_f (fn, B, F, BC, fambc_name, bocotype, ier)
             IMPORT :: c_char, cgenum_t
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: F
             INTEGER :: BC
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fambc_name
             INTEGER(cgenum_t) :: bocotype
	     INTEGER, INTENT(OUT) :: ier
          END SUBROUTINE cg_fambc_read_f
        END INTERFACE

	INTERFACE
           SUBROUTINE cg_fambc_write_f (fn, B, F, fambc_name, bocotype, BC, ier)
             IMPORT :: c_char, cgenum_t
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: F
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fambc_name
             INTEGER(cgenum_t) :: bocotype
             INTEGER :: BC
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE 
        END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write GeometryReference_t Nodes                         *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	INTERFACE
           SUBROUTINE cg_geo_read_f(fn, B, F, G, geo_name, geo_file, CAD_name, npart, ier)
             IMPORT :: c_char
             IMPLICIT NONE
             INTEGER :: fn
             INTEGER :: B
             INTEGER :: F
             INTEGER :: G
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_name
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_file
             CHARACTER(KIND=C_CHAR), DIMENSION(*) :: CAD_name
             INTEGER :: npart
	     INTEGER, INTENT(OUT) :: ier
           END SUBROUTINE cg_geo_read_f
        END INTERFACE

	INTERFACE
    SUBROUTINE cg_geo_write_f(fn, B, F, geo_name, geo_file, CAD_name, G, ier)
      
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: F
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_name
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: geo_file
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: CAD_name
      INTEGER :: G
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_geo_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write GeometryEntity_t Nodes                            *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
 
 INTERFACE
    SUBROUTINE cg_part_read_f(fn, B, F,G, P, part_name, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: F
      INTEGER :: G
      INTEGER :: P
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: part_name
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_part_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_part_write_f(fn, B, F, G, part_name, P, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: F
      INTEGER :: G
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: part_name
      INTEGER :: P
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_part_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write DiscreteData_t Nodes                              *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_ndiscrete_f(fn, B, Z, ndiscrete, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: ndiscrete
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ndiscrete_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_discrete_read_f(fn, B, Z, D, discrete_name, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: D
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: discrete_name
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_discrete_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_discrete_write_f(fn, B, Z, discrete_name, D, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: discrete_name
      INTEGER :: D
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_discrete_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_discrete_size_f(fn, B, Z, D, ndim, dims, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: D
      INTEGER :: ndim
      INTEGER(CGSIZE_T) :: dims
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_discrete_size_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_discrete_ptset_info_f(fn, B, Z, S, ptype, npnts, ier)
      IMPORT :: cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(cgenum_t) :: ptype
      INTEGER(CGSIZE_T) :: npnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_discrete_ptset_info_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_discrete_ptset_read_f( fn, B, Z, S, pnts, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(CGSIZE_T) :: pnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_discrete_ptset_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_discrete_ptset_write_f( fn, B, Z, name, location, ptype, npnts, pnts, D, ier)
      IMPORT :: cgsize_t, cgenum_t, c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: ptype
      INTEGER(CGSIZE_T) :: npnts
      INTEGER(CGSIZE_T) :: pnts
      INTEGER :: D
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_discrete_ptset_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write GridCoordinates_t/DataArray_t Nodes               *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_ncoords_f(fn, B, Z, ncoords, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: ncoords
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ncoords_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_coord_info_f (fn, B, Z, C, TYPE, coordname, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: C
      INTEGER(cgenum_t) :: TYPE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_coord_info_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_coord_read_f (fn, B, Z, coordname, TYPE, rmin, rmax, coord, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t, c_ptr
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
      INTEGER(cgenum_t) :: TYPE
      INTEGER(CGSIZE_T) :: rmin
      INTEGER(CGSIZE_T) :: rmax
      TYPE(C_PTR) :: coord
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_coord_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_coord_id_f(fn, B, Z, C, coord_id, ier)
      IMPORT :: c_double
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: C
      REAL(C_DOUBLE) :: coord_id
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_coord_id_f
 END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_coord_write_f(fn, B, 	Z, TYPE, coordname, coord, C, ier)
!!$      IMPORT :: c_char, cgenum_t, c_ptr
!!$      IMPLICIT NONE
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER(cgenum_t) :: TYPE
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
!!$      TYPE(C_PTR), VALUE :: coord
!!$      INTEGER :: C
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_coord_write_f
!!$ END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_coord_partial_write_f ( fn, B, Z, TYPE, coordname, rmin, rmax, coord, C, ier)
!!$      IMPORT :: c_char, cgenum_t, cgsize_t, c_ptr
!!$      IMPLICIT NONE
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER(cgenum_t) :: TYPE
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname
!!$      INTEGER(CGSIZE_T) :: rmin
!!$      INTEGER(CGSIZE_T) :: rmax
!!$      TYPE(C_PTR) :: coord
!!$      INTEGER :: C
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_coord_partial_write_f
!!$ END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write Elements_t Nodes                                  *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_nsections_f(fn, B, Z, nsections, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: nsections
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nsections_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_section_read_f(fn, B, Z, E, section_name, TYPE, start, END, nbndry, parent_flag, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: E
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: section_name
      INTEGER(cgenum_t) :: TYPE
      INTEGER(CGSIZE_T) ::start
      INTEGER(CGSIZE_T) ::END
      INTEGER :: nbndry
      INTEGER :: parent_flag
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_section_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_elements_read_f(fn, B, Z, E, elements, parent_data, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: E
      INTEGER(CGSIZE_T) :: elements
      INTEGER(CGSIZE_T) :: parent_data
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_elements_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_elementdatasize_f(fn, B, Z, E, ElementDataSize, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: E
      INTEGER(CGSIZE_T) :: ElementDataSize
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_elementdatasize_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_elementpartialsize_f(fn, B, Z, E, start, END, ElementDataSize, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: E
      INTEGER(CGSIZE_T) :: start
      INTEGER(CGSIZE_T) :: END
      INTEGER(CGSIZE_T) :: ElementDataSize
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_elementpartialsize_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_section_write_f(fn, B, Z, section_name, TYPE, start, END, nbndry, elements, S, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: section_name
      INTEGER(cgenum_t) :: TYPE
      INTEGER(CGSIZE_T) :: start
      INTEGER(CGSIZE_T) :: END
      INTEGER :: nbndry
      INTEGER(CGSIZE_T) ::elements
      INTEGER :: S
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_section_write_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_parent_data_write_f(fn, B, Z, S, parent_data, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(CGSIZE_T) :: parent_data
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_parent_data_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_section_partial_write_f( fn, B, Z, section_name, TYPE, start, END, nbndry, S, ier)
      IMPORT :: c_char, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: section_name
      INTEGER(CGSIZE_T) ::TYPE
      INTEGER(CGSIZE_T) ::start
      INTEGER(CGSIZE_T) ::END
      INTEGER :: nbndry
      INTEGER :: S
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_section_partial_write_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_elements_partial_write_f(fn, B, Z, S, rmin, rmax, elements, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(CGSIZE_T) ::rmin
      INTEGER(CGSIZE_T) ::rmax
      INTEGER(CGSIZE_T) ::elements
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_elements_partial_write_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_parent_data_partial_write_f(fn, B, Z, S, rmin, rmax, parent_data, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(CGSIZE_T) ::rmin
      INTEGER(CGSIZE_T) ::rmax
      INTEGER(CGSIZE_T) ::parent_data
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_parent_data_partial_write_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_elements_partial_read_f(fn, B, Z, S, rmin, rmax, elements, parent, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(CGSIZE_T) ::rmin
      INTEGER(CGSIZE_T) ::rmax
      INTEGER(CGSIZE_T) ::elements
      INTEGER(CGSIZE_T) ::parent
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_elements_partial_read_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write FlowSolution_t Nodes                              *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_nsols_f(fn, B, Z, nsols, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: nsols
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nsols_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_sol_info_f(fn, B, Z, S, solname, location, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: solname
      INTEGER(cgenum_t) :: location
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_sol_info_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_sol_id_f(fn, B, Z, S, sol_id, ier)
      IMPORT :: c_double
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      REAL(C_DOUBLE) :: sol_id
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_sol_id_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_sol_write_f(fn, B, Z, solname, location, S, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: solname
      INTEGER(cgenum_t) :: location
      INTEGER :: S
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_sol_write_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_sol_size_f(fn, B, Z, S, ndim, dims, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER :: ndim
      INTEGER(CGSIZE_T) :: dims
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_sol_size_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_sol_ptset_info_f( fn, B, Z, S, ptype, npnts, ier)
      IMPORT :: cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(cgenum_t) :: ptype
      INTEGER(CGSIZE_T) :: npnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_sol_ptset_info_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_sol_ptset_read_f(fn, B, Z, S, pnts, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(CGSIZE_T) ::pnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_sol_ptset_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_sol_ptset_write_f(fn, B, Z, name, location, ptype, npnts, pnts, S, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: ptype
      INTEGER(CGSIZE_T) :: npnts
      INTEGER(CGSIZE_T) :: pnts
      INTEGER :: S
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_sol_ptset_write_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write solution DataArray_t Nodes                        *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_nfields_f(fn, B, Z, S, nfields, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER :: nfields
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nfields_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_field_info_f(fn, B, Z, S, F, TYPE, fieldname, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER :: F
      INTEGER(cgenum_t) :: TYPE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
      INTEGER, INTENT(OUT) :: ier
   END SUBROUTINE cg_field_info_f
END INTERFACE

!!$INTERFACE
!!$   SUBROUTINE cg_field_read_f(fn, B, Z, S, fieldname), TYPE, rmin, rmax, field_ptr, ier)
!!$     INTEGER :: fn
!!$     INTEGER :: B,
!!$     INTEGER :: Z
!!$     INTEGER :: S, CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname), INTEGER(cgenum_t) :: TYPE
!!$     INTEGER(CGSIZE_T) ::rmin,
!!$     INTEGER(CGSIZE_T) ::rmax
!!$     void *field_ptr,
!!$     INTEGER, INTENT(OUT) :: ier
!!$   END SUBROUTINE cg_field_read_f
!!$END INTERFACE

INTERFACE
   SUBROUTINE cg_field_id_f(fn, B, Z, S, F, field_id, ier)
     IMPORT :: c_double
     IMPLICIT NONE
     INTEGER :: fn
     INTEGER :: B
     INTEGER :: Z
     INTEGER :: S
     INTEGER :: F
     REAL(C_DOUBLE) :: field_id
     INTEGER, INTENT(OUT) :: ier
   END SUBROUTINE cg_field_id_f
END INTERFACE

!!$INTERFACE
!!$   SUBROUTINE cg_field_write_f(fn, B, Z, S, TYPE, fieldname, field_ptr, F, ier)
!!$     INTEGER :: fn
!!$     INTEGER :: B,
!!$     INTEGER :: Z
!!$     INTEGER :: S
!!$     INTEGER(cgenum_t) :: TYPE
!!$     CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
!!$     void *field_ptr,
!!$     INTEGER :: F
!!$     INTEGER, INTENT(OUT) :: ier
!!$   END SUBROUTINE cg_field_write_f
!!$END INTERFACE

!!$INTERFACE
!!$   SUBROUTINE cg_field_partial_write_f) (fn, B, Z, S, TYPE, fieldname, rmin, rmax, void *field_ptr, F, ier)
!!$     INTEGER :: fn,
!!$     INTEGER :: B
!!$     INTEGER :: Z
!!$     INTEGER :: S, INTEGER(cgenum_t) :: TYPE
!!$     CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
!!$     INTEGER(CGSIZE_T) :: rmin
!!$     INTEGER(CGSIZE_T) :: rmax
!!$     void *field_ptr
!!$     INTEGER :: F
!!$     INTEGER, INTENT(OUT) :: ier
!!$   END SUBROUTINE cg_field_partial_write_f
!!$END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write ZoneSubRegion_t Nodes  			         *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_nsubregs_f(fn, B, Z, nsubreg, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: nsubreg
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nsubregs_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_subreg_info_f(fn, B, Z, S, regname, DIMENSION, &
         location, ptset_type, npnts, bcname_len, gcname_len, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: regname
      INTEGER :: DIMENSION
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(CGSIZE_T) ::npnts
      INTEGER :: bcname_len
      INTEGER :: gcname_len
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_subreg_info_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_subreg_ptset_read_f ( fn, B, Z, S, pnts, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_subreg_ptset_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_subreg_bcname_read_f ( fn, B, Z, S, bcname, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: bcname
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_subreg_bcname_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_subreg_gcname_read_f (fn, B, Z, S, gcname, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: S
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: gcname
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_subreg_gcname_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_subreg_ptset_write_f (fn, B, Z, regname, DIMENSION, location, ptset_type, npnts, pnts, S, ier)
      IMPORT :: cgenum_t, c_char, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: regname
      INTEGER :: DIMENSION
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(CGSIZE_T) :: npnts
      INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
      INTEGER :: S
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_subreg_ptset_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_subreg_bcname_write_f ( fn, B, Z, regname, DIMENSION, bcname, S, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: regname
      INTEGER :: DIMENSION
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: bcname
      INTEGER :: S
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_subreg_bcname_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_subreg_gcname_write_f ( fn, B, Z, regname, DIMENSION, gcname, S, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: regname
      INTEGER :: DIMENSION
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: gcname
      INTEGER :: S
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_subreg_gcname_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write ZoneGridConnectivity_t Nodes  			 *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_nzconns_f(fn, B, Z, nzconns, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: nzconns
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nzconns_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_zconn_read_f(fn, B, Z, C, name, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: C
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_zconn_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_zconn_write_f(fn, B, Z, name, C, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
      INTEGER :: C
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_zconn_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_zconn_get_f(fn, B, Z, C, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: C
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_zconn_get_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_zconn_set_f(fn, B, Z, C, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: C
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_zconn_set_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write OversetHoles_t Nodes                              *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_nholes_f(fn, B, Z, nholes, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: nholes
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nholes_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_hole_info_f(fn, B, Z, I, holename, location, ptset_type, nptsets, npnts, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: holename
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(CGSIZE_T) :: nptsets
      INTEGER(CGSIZE_T) :: npnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_hole_info_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_hole_read_f(fn, B, Z, I, pnts, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      INTEGER(CGSIZE_T) :: pnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_hole_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_hole_id_f(fn, B, Z, I, hole_id, ier)
      IMPORT :: c_double
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      REAL(C_DOUBLE) :: hole_id
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_hole_id_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_hole_write_f(fn, B, Z, holename, location, ptset_type, nptsets, npnts, pnts, I, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: holename
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: ptset_type
      INTEGER :: nptsets
      INTEGER(CGSIZE_T) ::npnts
      INTEGER(CGSIZE_T) ::pnts
      INTEGER :: I
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_hole_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write GridConnectivity_t Nodes                          *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_nconns_f(fn, B, Z, nconns, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: nconns
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nconns_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_conn_info_f(fn, B, Z, I, connectname, location, &
         TYPE, ptset_type, npnts, donorname, donor_zonetype,donor_ptset_type, &
         donor_datatype, ndata_donor, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: TYPE
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(CGSIZE_T) ::npnts
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
      INTEGER(cgenum_t) :: donor_zonetype
      INTEGER(cgenum_t) :: donor_ptset_type
      INTEGER(cgenum_t) :: donor_datatype
      INTEGER(CGSIZE_T) ::ndata_donor
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_info_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_conn_read_f(fn, B, Z, I, pnts, donor_datatype, donor_data, ier)
      IMPORT :: cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      INTEGER(CGSIZE_T) ::pnts
      INTEGER(cgenum_t) :: donor_datatype
      INTEGER(CGSIZE_T) ::donor_data
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_conn_read_short_f (fn, B, Z, I, pnts, ier)
      IMPORT :: cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      INTEGER(CGSIZE_T) ::pnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_read_short_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_conn_id_f(fn, B, Z, I, conn_id, ier)
      IMPORT :: c_double
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      REAL(C_DOUBLE) :: conn_id
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_id_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_conn_write_f(fn, B, Z, connectname, location, TYPE, ptset_type, & 
	npnts, pnts, donorname, donor_zonetype, donor_ptset_type, &
	donor_datatype, ndata_donor, donor_data, I, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: TYPE
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(CGSIZE_T) :: npnts
      INTEGER(CGSIZE_T), DIMENSION(*) :: pnts
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
      INTEGER(cgenum_t) :: donor_zonetype
      INTEGER(cgenum_t) :: donor_ptset_type
      INTEGER(cgenum_t) :: donor_datatype
      INTEGER(CGSIZE_T) :: ndata_donor
      INTEGER(CGSIZE_T), DIMENSION(*) :: donor_data
      INTEGER :: I
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_conn_write_short_f(fn, B, Z, connectname, location, &
         TYPE, ptset_type, npnts, pnts, donorname, I, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
      INTEGER(cgenum_t) :: location
      INTEGER(cgenum_t) :: TYPE
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(CGSIZE_T) :: npnts
      INTEGER(CGSIZE_T) :: pnts
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
      INTEGER :: I
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_write_short_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write GridConnectivity1to1_t Nodes in a zone            *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_n1to1_f(fn, B, Z, n1to1, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: n1to1
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_n1to1_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_1to1_read_f(fn, B, Z, I, connectname, donorname, &
	range, donor_range, transform, ier)
      IMPORT :: c_char, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
      INTEGER(CGSIZE_T), DIMENSION(*) :: range
      INTEGER(CGSIZE_T), DIMENSION(*) :: donor_range
      INTEGER, DIMENSION(*) :: transform
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_1to1_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_1to1_id_f(fn, B, Z, I, one21_id, ier)
      IMPORT :: c_double
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      REAL(C_DOUBLE) :: one21_id
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_1to1_id_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_1to1_write_f(fn, B, Z, connectname, donorname, range, &
	donor_range, transform, I, ier)
      IMPORT :: c_char, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
      INTEGER(CGSIZE_T), DIMENSION(*) :: range
      INTEGER(CGSIZE_T), DIMENSION(*) :: donor_range
      INTEGER, DIMENSION(*) :: transform
      INTEGER :: I
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_1to1_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read all GridConnectivity1to1_t Nodes of a base                  *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_n1to1_global_f(fn, B, n1to1_global, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: n1to1_global
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_n1to1_global_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_1to1_read_global_f(fn, B, connectname, zonename, donorname, &
         range, donor_range, transform, ier)
      IMPORT :: c_char, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: connectname
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zonename
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: donorname
      INTEGER(CGSIZE_T), DIMENSION(*) :: range
      INTEGER(CGSIZE_T), DIMENSION(*) :: donor_range
      INTEGER, DIMENSION(*) :: transform
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_1to1_read_global_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write BC_t Nodes                                        *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_nbocos_f(fn, B, Z, nbocos, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: nbocos
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nbocos_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_boco_info_f(fn, B, Z, BC, boconame, bocotype, &
         ptset_type, npnts, NormalIndex, & 
         NormalListFlag, NormalDataType, ndataset, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: boconame
      INTEGER(cgenum_t) :: bocotype
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(CGSIZE_T) :: npnts
      INTEGER :: NormalIndex
      INTEGER(CGSIZE_T) :: NormalListFlag
      INTEGER(cgenum_t) :: NormalDataType
      INTEGER :: ndataset
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_boco_info_f
 END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_boco_read_f(fn, B, Z, BC, pnts, NormalList, ier)
!!$      IMPORT :: cgsize_t
!!$      IMPLICIT NONE
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: BC
!!$      INTEGER(CGSIZE_T) :: pnts
!!$      void *NormalList
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_boco_read_f
!!$ END INTERFACE

 INTERFACE
    SUBROUTINE cg_boco_id_f(fn, B, Z, BC, boco_id, ier)
      IMPORT :: c_double
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      REAL(C_DOUBLE) :: boco_id
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_boco_id_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_boco_write_f(fn, B, Z, boconame, bocotype, ptset_type, npnts, pnts, BC, ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: boconame
      INTEGER(cgenum_t) :: bocotype
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(CGSIZE_T) ::npnts
      INTEGER(CGSIZE_T) ::pnts
      INTEGER :: BC
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_boco_write_f
 END INTERFACE
 
!!$ INTERFACE
!!$    SUBROUTINE cg_boco_normal_write_f( fn, B, Z, BC, NormalIndex, NormalListFlag,
!!$      NormalDataType, NormalList, ier)
!!$
!!$      INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: BC,
!!$      INTEGER(CGSIZE_T) ::NormalIndex
!!$      INTEGER :: NormalListFlag,
!!$      INTEGER(cgenum_t) :: NormalDataType
!!$      ! void *NormalList,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_boco_normal_write_f
!!$ END INTERFACE

 INTERFACE
    SUBROUTINE cg_boco_gridlocation_read_f(fn, B, Z, BC, location, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      INTEGER(cgenum_t) :: location
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_boco_gridlocation_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_boco_gridlocation_write_f( fn, B, Z, BC, location, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      INTEGER(cgenum_t) :: location
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_boco_gridlocation_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write BCProperty_t/WallFunction_t Nodes                 *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_bc_wallfunction_read_f(fn, B, Z, BC, WallFunctionType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      INTEGER(cgenum_t) :: WallFunctionType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_bc_wallfunction_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_bc_wallfunction_write_f(fn, B, Z, BC, WallFunctionType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      INTEGER(cgenum_t) :: WallFunctionType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_bc_wallfunction_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write BCProperty_t/Area_t Nodes                         *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_bc_area_read_f(fn, B, Z, BC, AreaType, SurfaceArea, RegionName, ier)
      IMPORT :: c_char, cgenum_t, c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      INTEGER(cgenum_t) :: AreaType
      REAL(C_FLOAT) :: SurfaceArea
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: RegionName
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_bc_area_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_bc_area_write_f(fn, B, Z, BC, AreaType, SurfaceArea, RegionName, ier)
      IMPORT :: c_char, cgenum_t, c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      INTEGER(cgenum_t) :: AreaType
      REAL(C_FLOAT) :: SurfaceArea
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: RegionName
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_bc_area_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write GridConnectivityProperty_t/Periodic_t Nodes       *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_conn_periodic_read_f(fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      REAL(C_FLOAT) :: RotationCenter
      REAL(C_FLOAT) :: RotationAngle
      REAL(C_FLOAT) :: Translation
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_periodic_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_conn_periodic_write_f( fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      REAL(C_FLOAT) :: RotationCenter
      REAL(C_FLOAT) :: RotationAngle
      REAL(C_FLOAT) :: Translation
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_periodic_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_1to1_periodic_read_f(fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      REAL(C_FLOAT) :: RotationCenter
      REAL(C_FLOAT) :: RotationAngle
      REAL(C_FLOAT) :: Translation
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_1to1_periodic_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_1to1_periodic_write_f(fn, B, Z, I, RotationCenter, RotationAngle, Translation, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      REAL(C_FLOAT) :: RotationCenter
      REAL(C_FLOAT) :: RotationAngle
      REAL(C_FLOAT) :: Translation
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_1to1_periodic_write_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!   Read and write GridConnectivityProperty_t/AverageInterface_t Nodes  *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_conn_average_read_f(fn, B, Z, I, AverageInterfaceType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      INTEGER(cgenum_t) :: AverageInterfaceType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_average_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_conn_average_write_f(fn, B, Z, I, AverageInterfaceType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      INTEGER(cgenum_t) :: AverageInterfaceType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conn_average_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_1to1_average_read_f(fn, B, Z, I, AverageInterfaceType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      INTEGER(cgenum_t) :: AverageInterfaceType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_1to1_average_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_1to1_average_write_f(fn, B, Z, I,AverageInterfaceType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: I
      INTEGER(cgenum_t) :: AverageInterfaceType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_1to1_average_write_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write BCDataSet_t Nodes                                 *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_dataset_read_f(fn, B, Z, BC, DSet, Dataset_name, BCType, DirichletFlag, &
         NeumannFlag, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      INTEGER :: DSet
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: Dataset_name
      INTEGER(cgenum_t) :: BCType
      INTEGER :: DirichletFlag
      INTEGER :: NeumannFlag
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_dataset_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_dataset_write_f(fn, B, Z, BC, Dataset_name,BCType, Dset, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: Dataset_name
      INTEGER(cgenum_t) :: BCType
      INTEGER :: Dset
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_dataset_write_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_bcdataset_write_f(Dataset_name, BCType, BCDataType, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: Dataset_name
      INTEGER(cgenum_t) :: BCType
      INTEGER(cgenum_t) :: BCDataType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_bcdataset_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_bcdataset_info_f(ndataset, ier)
      IMPLICIT NONE
      INTEGER :: ndataset
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_bcdataset_info_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_bcdataset_read_f(index, Dataset_name, BCType, &
	DirichletFlag, NeumannFlag,ier)
      IMPORT :: c_char, cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER(CGSIZE_T) ::index
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: Dataset_name
      INTEGER(cgenum_t) :: BCType
      INTEGER :: DirichletFlag
      INTEGER :: NeumannFlag
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_bcdataset_read_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write BCData_t Nodes                                    *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
 
 INTERFACE
    SUBROUTINE cg_bcdata_write_f(fn, B, Z, BC, Dset, BCDataType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: BC
      INTEGER :: Dset
      INTEGER(cgenum_t) :: BCDataType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_bcdata_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write RigidGridMotion_t Nodes                           *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_n_rigid_motions_f(fn, B, Z, n_rigid_motions, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: n_rigid_motions
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_n_rigid_motions_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_rigid_motion_read_f(fn, B, Z, R, rmotion_name, TYPE, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: R
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: rmotion_name
      INTEGER(cgenum_t) :: TYPE
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_rigid_motion_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_rigid_motion_write_f(fn, B, Z, rmotion_name, TYPE, R, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: rmotion_name
      INTEGER(cgenum_t) :: TYPE
      INTEGER :: R
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_rigid_motion_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write ArbitraryGridMotion_t Nodes                       *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_n_arbitrary_motions_f( fn, B, Z, n_arbitrary_motions, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: n_arbitrary_motions
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_n_arbitrary_motions_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_arbitrary_motion_read_f(fn, B, Z, A, amotion_name, TYPE, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: A
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: amotion_name
      INTEGER(cgenum_t) :: TYPE
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_arbitrary_motion_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_arbitrary_motion_write_f(fn, B, Z, amotion_name, TYPE, A, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: amotion_name
      INTEGER(cgenum_t) :: TYPE
      INTEGER :: A
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_arbitrary_motion_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write GridCoordinates_t Nodes                           *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_ngrids_f(fn, B, Z, ngrids, ier)
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: ngrids
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ngrids_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_grid_read_f(fn, B, Z, G, gridname, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      INTEGER :: G
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: gridname
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_grid_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_grid_write_f(fn, B, Z, gridname, G, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: gridname
      INTEGER :: G
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_grid_write_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write SimulationType_t Node                             *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_simulation_type_read_f(fn, B, TYPE, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER(cgenum_t) :: TYPE
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_simulation_type_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_simulation_type_write_f(fn, B, TYPE, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER(cgenum_t) :: TYPE
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_simulation_type_write_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write BaseIterativeData_t Node                          *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
 
 INTERFACE
    SUBROUTINE cg_biter_read_f(fn, B, bitername, nsteps, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: bitername
      INTEGER :: nsteps
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_biter_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_biter_write_f(fn, B, bitername, nsteps, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: bitername
      INTEGER :: nsteps
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_biter_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write ZoneIterativeData_t Nodes                         *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_ziter_read_f(fn, B, Z, zitername, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zitername
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ziter_read_f
 END INTERFACE
 

 INTERFACE
    SUBROUTINE cg_ziter_write_f(fn, B, Z, zitername, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      INTEGER :: Z
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: zitername
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ziter_write_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write Gravity_t Node                                    *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_gravity_read_f(fn, B, gravity_vector, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      REAL(C_FLOAT) :: gravity_vector
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_gravity_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_gravity_write_f(fn, B, gravity_vector, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      REAL(C_FLOAT) :: gravity_vector
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_gravity_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write Axisymmetry_t Node                                *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_axisym_read_f(fn, B, ref_point, axis, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      REAL(C_FLOAT) :: ref_point
      REAL(C_FLOAT) :: axis
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_axisym_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_axisym_write_f(fn, B, ref_point, axis, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      INTEGER :: fn
      INTEGER :: B
      REAL(C_FLOAT) :: ref_point
      REAL(C_FLOAT) :: axis
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_axisym_write_f
 END INTERFACE


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write RotatingCoordinates_t Node                        *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_rotating_read_f(rot_rate, rot_center, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      REAL(C_FLOAT) :: rot_rate
      REAL(C_FLOAT) :: rot_center
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_rotating_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_rotating_write_f(rot_rate, rot_center, ier)
      IMPORT :: c_float
      IMPLICIT NONE
      REAL(C_FLOAT) :: rot_rate
      REAL(C_FLOAT) :: rot_center
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_rotating_write_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Read and write  IndexArray/Range_t Nodes                         *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_ptset_info_f(ptset_type, npnts, ier)
      IMPORT :: cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: ptset_type 
      INTEGER(cgsize_t) :: npnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ptset_info_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_ptset_read_f(pnts, ier)
      IMPORT :: cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER(cgsize_t) :: pnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ptset_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_ptset_write_f(ptset_type, npnts, pnts, ier)
      IMPORT :: cgenum_t, cgsize_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: ptset_type
      INTEGER(cgsize_t) :: npnts 
      INTEGER(cgsize_t) :: pnts
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ptset_write_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Go - To Function                                                 *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

!!$	INTERFACE
!!$           SUBROUTINE cg_goto_f, CG_GOTO_F)(cgint_f *fn, cgint_f *B, ier, ...)
!!$cgint_f *fn, cgint_f *B, ier, ...)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cg_gorel_f, CG_GOREL_F)(cgint_f *fn, ier, ...)
!!$cgint_f *fn, ier, ...)
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE

 INTERFACE
    SUBROUTINE cg_gopath_f(fn,path, ier)
      IMPORT :: C_CHAR
      IMPLICIT NONE
      INTEGER :: fn
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: path
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_gopath_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!              Read Multiple path nodes                         *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_famname_read_f(famname, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: famname
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_famname_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_nmultifam_f(nfam, ier)
      IMPLICIT NONE
      INTEGER :: nfam
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nmultifam_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_multifam_read_f(N,name, family, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: N
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_multifam_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_convergence_read_f(iterations, NormDefinitions, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: iterations
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: NormDefinitions
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_convergence_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_state_size_f(size, ier)
      IMPLICIT NONE
      INTEGER :: size
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_state_size_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_state_read_f(StateDescription, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: StateDescription
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_state_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_equationset_read_f(EquationDimension, GoverningEquationsFlag, &
         GasModelFlag, ViscosityModelFlag, ThermalConductivityModelFlag, &
         TurbulenceClosureFlag, TurbulenceModelFlag, ier)
      IMPLICIT NONE
      INTEGER :: EquationDimension
      INTEGER :: GoverningEquationsFlag
      INTEGER :: GasModelFlag
      INTEGER :: ViscosityModelFlag
      INTEGER :: ThermalConductivityModelFlag
      INTEGER :: TurbulenceClosureFlag
      INTEGER :: TurbulenceModelFlag
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_equationset_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_equationset_chemistry_read_f(ThermalRelaxationFlag, ChemicalKineticsFlag, ier)
      IMPLICIT NONE
      INTEGER :: ThermalRelaxationFlag
      INTEGER :: ChemicalKineticsFlag
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_equationset_chemistry_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_equationset_elecmagn_read_f(ElecFldModelFlag, MagnFldModelFlag,&
         ConductivityModelFlag, ier)
      IMPLICIT NONE
      INTEGER :: ElecFldModelFlag
      INTEGER :: MagnFldModelFlag
      INTEGER :: ConductivityModelFlag
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_equationset_elecmagn_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_governing_read_f(EquationsType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: EquationsType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_governing_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_diffusion_read_f(diffusion_model, ier)
      IMPLICIT NONE
      INTEGER :: diffusion_model
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_diffusion_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_model_read_f(ModelLabel, ModelType, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ModelLabel 
      INTEGER(cgenum_t) :: ModelType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_model_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_narrays_f(narrays, ier)
      IMPLICIT NONE
      INTEGER :: narrays
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_narrays_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_array_info_f(A, ArrayName, DataType, DataDimension, DimensionVector, ier)
      IMPORT :: c_char, cgsize_t
      IMPLICIT NONE
      INTEGER :: A
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ArrayName, DataType, DataDimension
      INTEGER(cgsize_t) :: DimensionVector
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_array_info_f
 END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_array_read_f(A, DATA, ier)
!!$      INTEGER :: A,
!!$      void *DATA,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_array_read_f
!!$ END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_array_read_as_f(A, TYPE, DATA, ier)
!!$      IMPORT :: cgenum_t
!!$      IMPLICIT NONE
!!$      INTEGER :: A
!!$      INTEGER(cgenum_t) :: TYPE
!!$      void *DATA
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_array_read_as_f
!!$ END INTERFACE

 INTERFACE
    SUBROUTINE cg_nintegrals_f(nintegrals, ier)
      IMPLICIT NONE
      INTEGER :: nintegrals
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nintegrals_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_integral_read_f(IntegralDataIndex, IntegralDataName,ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: IntegralDataIndex
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: IntegralDataName
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_integral_read_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_rind_read_f(RindData, ier)
      IMPLICIT NONE
      INTEGER :: RindData
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_rind_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_ndescriptors_f(ndescriptors, ier)
      IMPLICIT NONE
      INTEGER :: ndescriptors
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ndescriptors_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_descriptor_size_f(descr_no, descr_size, ier)
      IMPLICIT NONE
      INTEGER :: descr_no
      INTEGER :: descr_size
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_descriptor_size_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_descriptor_read_f(descr_no, descr_name, descr_text, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: descr_no
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: descr_name
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: descr_text
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_descriptor_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_nunits_f(nunits, ier)
      IMPLICIT NONE
      INTEGER :: nunits
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nunits_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_units_read_f(mass, length,  time, temperature, angle, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: mass
      INTEGER(cgenum_t) :: length
      INTEGER(cgenum_t) :: time
      INTEGER(cgenum_t) :: temperature
      INTEGER(cgenum_t) :: angle
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_units_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_unitsfull_read_f(mass, length, time, temperature, angle, current, &
         amount, intensity, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: mass
      INTEGER(cgenum_t) :: length
      INTEGER(cgenum_t) :: time
      INTEGER(cgenum_t) :: temperature
      INTEGER(cgenum_t) :: angle
      INTEGER(cgenum_t) :: current
      INTEGER(cgenum_t) :: amount
      INTEGER(cgenum_t) :: intensity
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_unitsfull_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_exponents_info_f(DataType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: DataType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_exponents_info_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_nexponents_f(nexps, ier)
      IMPLICIT NONE
      INTEGER :: nexps
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nexponents_f
 END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_exponents_read_f(void *exponents, ier)
!!$      void *exponents
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_exponents_read_f
!!$ END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_expfull_read_f(exponents, ier)
!!$      void *exponents,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_expfull_read_f
!!$ END INTERFACE

 INTERFACE
    SUBROUTINE cg_conversion_info_f(DataType, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: DataType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_conversion_info_f
 END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_conversion_read_f(ConversionFactors, ier)
!!$      void *ConversionFactors,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_conversion_read_f
!!$ END INTERFACE

 INTERFACE
    SUBROUTINE cg_dataclass_read_f(dataclass, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: dataclass
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_dataclass_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_gridlocation_read_f(GridLocation, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: GridLocation
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_gridlocation_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_ordinal_read_f(Ordinal, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER :: Ordinal
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ordinal_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_npe_f(TYPE,npe, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: TYPE
      INTEGER :: npe
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_npe_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_is_link_f(path_length, ier)
      IMPLICIT NONE
      INTEGER :: path_length
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_is_link_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_link_read_f(filename, link_path, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: filename
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: link_path
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_link_read_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_nuser_data_f(nuser_data, ier)
      IMPLICIT NONE
      INTEGER :: nuser_data
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_nuser_data_f
 END INTERFACE
 
 INTERFACE
    SUBROUTINE cg_user_data_read_f(index,dataname, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: index
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: dataname
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_user_data_read_f
 END INTERFACE
 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!                   Write Multiple path nodes                           *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_famname_write_f(family_name, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family_name
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_famname_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_multifam_write_f(name, family, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: family
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_multifam_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_convergence_write_f(iterations, NormDefinitions, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      INTEGER :: iterations
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: NormDefinitions
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_convergence_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_state_write_f(StateDescription, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: StateDescription
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_state_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_equationset_write_f(EquationDimension, ier)
      IMPLICIT NONE
      INTEGER :: EquationDimension
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_equationset_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_governing_write_f(Equationstype, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: Equationstype
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_governing_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_diffusion_write_f(diffusion_model, ier)
      IMPLICIT NONE
      INTEGER :: diffusion_model
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_diffusion_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_model_write_f(ModelLabel, ModelType, ier)
      IMPORT :: c_char, cgenum_t
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ModelLabel
      INTEGER(cgenum_t) :: ModelType
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_model_write_f
 END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_array_write_f (ArrayName, DataType, DataDimension, DimensionVector, &
!!$	void *Data, ier)
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ArrayName
!!$      INTEGER(cgenum_t) :: DataType
!!$      INTEGER :: DataDimension
!!$      INTEGER(cgsize_t) :: DimensionVector
!!$      void *DATA
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_array_write_f
!!$ END INTERFACE
        
 INTERFACE
    SUBROUTINE cg_integral_write_f(IntegralDataName, ier)
      IMPORT :: c_char
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: IntegralDataName
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_integral_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_rind_write_f(RindData, ier)
      IMPLICIT NONE
      INTEGER :: RindData
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_rind_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_descriptor_write_f(descr_name, descr_text, ier)
      IMPORT :: c_char
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: descr_name
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: descr_text
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_descriptor_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_units_write_f(mass, length, time, temperature, angle, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: mass
      INTEGER(cgenum_t) :: length
      INTEGER(cgenum_t) :: time
      INTEGER(cgenum_t) :: temperature
      INTEGER(cgenum_t) :: angle
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_units_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_unitsfull_write_f(mass, length, time, temperature, angle, current, &
         amount, intensity, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: mass
      INTEGER(cgenum_t) :: length
      INTEGER(cgenum_t) :: time
      INTEGER(cgenum_t) :: temperature
      INTEGER(cgenum_t) :: angle
      INTEGER(cgenum_t) :: current
      INTEGER(cgenum_t) :: amount
      INTEGER(cgenum_t) :: intensity
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_unitsfull_write_f
 END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_exponents_write_f(DataType, void *exponents, ier)
!!$
!!$      INTEGER(cgenum_t) :: DataType_t)*DataType, void *exponents,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_exponents_write_f
!!$ END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_expfull_write_f(DataType, void *exponents, ier)
!!$      INTEGER(cgenum_t) :: DataType
!!$      void *exponents,
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_expfull_write_f
!!$ END INTERFACE

!!$ INTERFACE
!!$    SUBROUTINE cg_conversion_write_f( DataType, ConversionFactors, ier)
!!$      INTEGER(cgenum_t) :: DataType
!!$      void *ConversionFactors
!!$      INTEGER, INTENT(OUT) :: ier
!!$    END SUBROUTINE cg_conversion_write_f
!!$ END INTERFACE

 INTERFACE
    SUBROUTINE cg_dataclass_write_f(dataclass, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: dataclass
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_dataclass_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_gridlocation_write_f(GridLocation, ier)
      IMPORT :: cgenum_t
      IMPLICIT NONE
      INTEGER(cgenum_t) :: GridLocation
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_gridlocation_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_ordinal_write_f(Ordinal, ier)
      IMPLICIT NONE
      INTEGER :: Ordinal
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_ordinal_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_link_write_f(nodename, filename, name_in_file, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: nodename
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: filename
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: name_in_file
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_link_write_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_user_data_write_f(dataname, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: dataname
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_user_data_write_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      General Delete Function                      *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_delete_node_f(node_name, ier)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: node_name
      INTEGER, INTENT(OUT) :: ier
    END SUBROUTINE cg_delete_node_f
 END INTERFACE

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!      Error Handling Functions                                         *
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 INTERFACE
    SUBROUTINE cg_get_error_f(errmsg)
      IMPORT :: c_char
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: errmsg
    END SUBROUTINE cg_get_error_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_error_exit_f()
      IMPLICIT NONE
    END SUBROUTINE cg_error_exit_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_error_print_f()
      IMPLICIT NONE
    END SUBROUTINE cg_error_print_f
 END INTERFACE

 INTERFACE
    SUBROUTINE cg_exit_on_error_f(flag)
      IMPLICIT NONE
      INTEGER :: flag
    END SUBROUTINE cg_exit_on_error_f
 END INTERFACE
!!$
!!$#ifdef BUILD_PARALLEL
!!$
!!$!======================================================================
!!$! parallel IO interface
!!$!======================================================================
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_mpi_comm_f(
!!$	mpi_comm_f, ier)
!!$
!!$	INTEGER :: mpi_comm_f,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_pio_mode_f(
!!$	mode, int *pcg_mpi_info_f, ier)
!!$
!!$	INTEGER(cgenum_t) :: mode, int *pcg_mpi_info_f,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_queue_set_f(
!!$	use_queue, ier)
!!$
!!$	INTEGER :: use_queue,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_queue_flush_f(ier)
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_open_f(filename, int *mode,
!!$	fn, ier)
!!$CHARACTER(KIND=C_CHAR), DIMENSION(*) :: filename, int *mode,
!!$	INTEGER :: fn,
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_close_f(fn, ier)
!!$INTEGER :: fn,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_write_f(fn,
!!$	B, Z, type, coordname,
!!$	INTEGER :: C, ier)
!!$INTEGER :: fn,
!!$	INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER(cgenum_t) :: type
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: coordname,
!!$	INTEGER :: C,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_write_data_f(
!!$	fn, B, Z, C,
!!$	cgsize_t *rmin, cgsize_t *rmax, void *data, ier)
!!$
!!$	INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: C,
!!$	INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, void *data, 
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_read_data_f(
!!$	fn, B, Z, C,
!!$	cgsize_t *rmin, cgsize_t *rmax, void *data, ier)
!!$
!!$	INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: C,
!!$	INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, void *data, 
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_section_write_f(
!!$	fn, B, Z, section_name,
!!$	CGNS_ENUMT(ElementType_t)*type, INTEGER(cgsize_t) :: start, INTEGER(cgsize_t) :: end, nbndry,
!!$	S, ier 
!!$
!!$	INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      CHARACTER(KIND=C_CHAR), DIMENSION(*) :: section_name,
!!$	INTEGER(cgenum_t) :: ElementType_t)*type, cgsize_t *start, INTEGER(cgsize_t) :: end
!!$      INTEGER :: nbndry,
!!$	INTEGER :: S,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_elements_write_data_f(
!!$	fn, B, Z, S, cgsize_t *start,
!!$	cgsize_t *end, cgsize_t *elements, ier)
!!$
!!$	INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S, INTEGER(cgsize_t) :: start,
!!$	INTEGER(cgsize_t) :: end, INTEGER(cgsize_t) :: elements,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_elements_read_data_f(
!!$	fn, B, Z, S, cgsize_t *start,
!!$	cgsize_t *end, cgsize_t *elements, ier)
!!$
!!$	INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S, INTEGER(cgsize_t) :: start,
!!$	INTEGER(cgsize_t) :: end, INTEGER(cgsize_t) :: elements,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_field_write_f(fn,
!!$	B, Z, S, type,
!!$	fieldname, F, ier)
!!$INTEGER :: fn,
!!$	INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S
!!$      INTEGER(cgenum_t) :: type,
!!$	CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
!!$      INTEGER :: F,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_field_read_data_f(
!!$	fn, B, Z, S,
!!$	F, cgsize_t *rmin, cgsize_t *rmax, void *field_ptr,
!!$	ier)
!!$
!!$	INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S,
!!$	INTEGER :: F, INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, void *field_ptr
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_write_f(ArrayName,
!!$	DataType, DataDimension, cgsize_t *DimensionVector,
!!$	A, ier 
!!$CHARACTER(KIND=C_CHAR), DIMENSION(*) :: ArrayName,
!!$	INTEGER(cgenum_t) :: DataType
!!$      INTEGER :: DataDimension, INTEGER(cgsize_t) :: DimensionVector,
!!$	INTEGER :: A,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_write_data_f(
!!$	A, cgsize_t *rmin, cgsize_t *rmax, void *data, 
!!$	ier)
!!$	INTEGER :: A, INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, void *data,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_read_data_f(
!!$	A, cgsize_t *rmin, cgsize_t *rmax, void *data, 
!!$	ier)
!!$	INTEGER :: A, INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, void *data, 
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_error_exit_f()
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_multi_read_data_f, CGP_COORD_MULTI_READ_DATA_F)(fn, B, Z, C, 
!!$	cgsize_t *rmin, cgsize_t *rmax, 
!!$	void *coordsX, void *coordsY, void *coordsZ, ier)
!!$INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: C, 
!!$	INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, 
!!$	void *coordsX, void *coordsY, void *coordsZ,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!       cgp_coord_multi_write_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_coord_multi_write_data_f, CGP_COORD_MULTI_WRITE_DATA_F)(fn, B, Z, C, 
!!$						    cgsize_t *rmin, cgsize_t *rmax, 
!!$						    void *coordsX, void *coordsY, void *coordsZ, ier)
!!$INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: C, 
!!$						    INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, 
!!$						    void *coordsX, void *coordsY, void *coordsZ
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!       cgp_field_multi_write_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_field_multi_write_data_f(
!!$	fn, B, Z, S,
!!$	F, cgsize_t *rmin, cgsize_t *rmax, ier, cgsize_t *nsets, ...)
!!$	fn, B, Z, S,
!!$	F, INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, ier, INTEGER(cgsize_t) :: nsets, ...
!!$
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!       cgp_field_multi_read_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_field_multi_read_data_f(
!!$	INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S,
!!$	F, INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, ier, nsets, ...)
!!$
!!$	INTEGER :: fn
!!$      INTEGER :: B
!!$      INTEGER :: Z
!!$      INTEGER :: S,
!!$	INTEGER :: F, INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax, ier
!!$      INTEGER :: nsets, ...
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!        cgp_array_multi_write_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_multi_write_data_f(
!!$	fn, A, cgsize_t *rmin, cgsize_t *rmax,
!!$	ier, nsets, ...)
!!$	INTEGER :: fn
!!$      INTEGER :: A, INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax,
!!$	ier
!!$      INTEGER :: nsets, ...
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!!$!        cgp_array_multi_read_data Function                              *
!!$! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
!!$
!!$	INTERFACE
!!$           SUBROUTINE cgp_array_multi_read_data_f(
!!$	fn, A, cgsize_t *rmin, cgsize_t *rmax,
!!$	ier, nsets, ...)
!!$
!!$	INTEGER :: fn
!!$      INTEGER :: A, INTEGER(cgsize_t) :: rmin, INTEGER(cgsize_t) :: rmax,
!!$	     INTEGER, INTENT(OUT) :: ier
!!$           END SUBROUTINE 
!!$        END INTERFACE
!!$
!!$#endif HDF5_HAVE_MULTI_DATASETS
!!$#endif BUILD_PARALLEL

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
