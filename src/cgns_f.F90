        MODULE cgns
          
        USE ISO_C_BINDING
        IMPLICIT NONE

#include "cgnstypes_f.h"

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
     	   ENUMERATOR :: NormalizedByDimensiona		= 3
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

        INTERFACE
           INTEGER(C_INT) FUNCTION cgp_open(filename, mode, fn) BIND(C, name='cgp_open')
             USE ISO_C_BINDING
             CHARACTER(C_CHAR), DIMENSION(*), INTENT(IN)  :: filename
             INTEGER(C_INT)  , INTENT(IN), VALUE  :: mode
             INTEGER(C_INT)  , INTENT(OUT) :: fn
           END FUNCTION cgp_open
        END INTERFACE

        INTERFACE
           INTEGER(C_INT) FUNCTION cgp_pio_mode(mode, info) BIND(C, name='cgp_pio_mode')
             USE ISO_C_BINDING
             INTEGER(KIND(CGP_COLLECTIVE)), INTENT(IN), VALUE  :: mode
             INTEGER(C_INT)  , INTENT(IN), VALUE  :: info
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
             INTEGER(C_INT)   , INTENT(IN), VALUE  :: itype
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
             INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
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
             INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
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
             INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
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
             INTEGER(C_INT)   , INTENT(IN), VALUE :: itype
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
             INTEGER(C_INT)   , INTENT(IN), VALUE :: location
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


      END MODULE cgns
