! Fortran version of cgnslib.h

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      modes for cgns file                                          *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
        integer CG_MODE_READ, CG_MODE_WRITE, CG_MODE_MODIFY
        parameter (CG_MODE_READ   = 0)
        parameter (CG_MODE_WRITE  = 1)
        parameter (CG_MODE_MODIFY = 2)
!* legacy code support
        integer MODE_READ, MODE_WRITE, MODE_MODIFY
        parameter (MODE_READ   = 0)
        parameter (MODE_WRITE  = 1)
        parameter (MODE_MODIFY = 2)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      file types                                                   *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
        integer CG_FILE_NONE, CG_FILE_ADF, CG_FILE_HDF5
        integer CG_FILE_XML
        parameter (CG_FILE_NONE = 0)
        parameter (CG_FILE_ADF  = 1)
        parameter (CG_FILE_HDF5 = 2)
        parameter (CG_FILE_XML  = 3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      some error code                                               *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
        integer CG_OK, CG_ERROR, CG_NODE_NOT_FOUND
        integer CG_INCORRECT_PATH, CG_NO_INDEX_DIM
        parameter (CG_OK             = 0)
        parameter (CG_ERROR          = 1)
        parameter (CG_NODE_NOT_FOUND = 2)
        parameter (CG_INCORRECT_PATH = 3)
        parameter (CG_NO_INDEX_DIM   = 4)
!* legacy code support
        integer ALL_OK, ERROR, NODE_NOT_FOUND, INCORRECT_PATH
        parameter (ALL_OK         = 0)
        parameter (ERROR          = 1)
        parameter (NODE_NOT_FOUND = 2)
        parameter (INCORRECT_PATH = 3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Dimensional Units                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
        integer CG_UserDefined, CG_Null
        parameter (CG_Null = 0)
        parameter (CG_UserDefined = 1)
!* legacy code support
        integer Null, UserDefined
        parameter (Null = 0)
        parameter (UserDefined = 1)

        integer Kilogram, Gram, Slug, PoundMass
        character*32 MassUnitsName(0:5)
        parameter (Kilogram  = 2)
        parameter (Gram      = 3)
        parameter (Slug      = 4)
        parameter (PoundMass = 5)

        integer Meter, Centimeter, Millimeter
        integer Foot, Inch
        character*32 LengthUnitsName(0:6)
        parameter (Meter      = 2)
        parameter (Centimeter = 3)
        parameter (Millimeter = 4)
        parameter (Foot       = 5)
        parameter (Inch       = 6)

        integer Second
        character*32 TimeUnitsName(0:2)
        parameter (Second = 2)

        integer Kelvin, Celsius, Rankine, Fahrenheit
        character*32 TemperatureUnitsName(0:5)
        parameter (Kelvin     = 2)
        parameter (Celsius    = 3)
        parameter (Rankine    = 4)
        parameter (Fahrenheit = 5)

        integer Degree, Radian
        character*32 AngleUnitsName(0:3)
        parameter (Degree = 2)
        parameter (Radian = 3)

        integer Ampere, Abampere, Statampere, Edison, auCurrent
        character*32 ElectricCurrentUnitsName(0:6)
        parameter (Ampere     = 2)
        parameter (Abampere   = 3)
        parameter (Statampere = 4)
        parameter (Edison     = 5)
        parameter (auCurrent  = 6)

        integer Mole, Entities, StandardCubicFoot, StandardCubicMeter
        character*32 SubstanceAmountUnitsName(0:5)
        parameter (Mole               = 2)
        parameter (Entities           = 3)
        parameter (StandardCubicFoot  = 4)
        parameter (StandardCubicMeter = 5)

        integer Candela, Candle, Carcel, Hefner, Violle
        character*32 LuminousIntensityUnitsName(0:6)
        parameter (Candela = 2)
        parameter (Candle  = 3)
        parameter (Carcel  = 4)
        parameter (Hefner  = 5)
        parameter (Violle  = 6)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Data Class                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
        integer Dimensional, NormalizedByDimensional
        integer NormalizedByUnknownDimensional
        integer NondimensionalParameter, DimensionlessConstant
        character*32 DataClassName(0:6)
        parameter (Dimensional                    = 2)
        parameter (NormalizedByDimensional        = 3)
        parameter (NormalizedByUnknownDimensional = 4)
        parameter (NondimensionalParameter        = 5)
        parameter (DimensionlessConstant          = 6)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Grid Location                                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer Vertex, CellCenter, FaceCenter
        integer IFaceCenter, JFaceCenter, KFaceCenter, EdgeCenter
        character*32 GridLocationName(0:8)
        parameter (Vertex      = 2)
        parameter (CellCenter  = 3)
        parameter (FaceCenter  = 4)
        parameter (IFaceCenter = 5)
        parameter (JFaceCenter = 6)
        parameter (KFaceCenter = 7)
        parameter (EdgeCenter  = 8)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Grid Connectivity Types                                          *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer Overset, Abutting, Abutting1to1
        character*32 GridConnectivityTypeName(0:4)
        parameter (Overset      = 2)
        parameter (Abutting     = 3)
        parameter (Abutting1to1 = 4)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Point Set Types                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer PointList, PointListDonor, PointRange, PointRangeDonor
        integer ElementRange, ElementList, CellListDonor
        character*32 PointSetTypeName(0:8)
        parameter (PointList       = 2)
        parameter (PointListDonor  = 3)
        parameter (PointRange      = 4)
        parameter (PointRangeDonor = 5)
        parameter (ElementRange    = 6)
        parameter (ElementList     = 7)
        parameter (CellListDonor   = 8)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Governing Equations and Physical Models Types                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer FullPotential, Euler
        integer NSLaminar, NSTurbulent
        integer NSLaminarIncompressible
        integer NSTurbulentIncompressible
        character*32 GoverningEquationsTypeName(0:7)
        parameter (FullPotential             = 2)
        parameter (Euler                     = 3)
        parameter (NSLaminar                 = 4)
        parameter (NSTurbulent               = 5)
        parameter (NSLaminarIncompressible   = 6)
        parameter (NSTurbulentIncompressible = 7)

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
!** TurbulenceClosure_t: EddyViscosity, ReynoldsStress, ReynoldsStressAlgebraic
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
!**				Chemistry_LinRessler

        integer Ideal, VanderWaals
        integer Constant
        integer PowerLaw, SutherlandLaw
        integer ConstantPrandtl
        integer EddyViscosity, ReynoldsStress, ReynoldsStressAlgebraic
        integer Algebraic_BaldwinLomax, Algebraic_CebeciSmith
        integer HalfEquation_JohnsonKing, OneEquation_BaldwinBarth
        integer OneEquation_SpalartAllmaras, TwoEquation_JonesLaunder
        integer TwoEquation_MenterSST, TwoEquation_Wilcox
        integer CaloricallyPerfect, ThermallyPerfect
        integer ConstantDensity, RedlichKwong
        integer Frozen, ThermalEquilib, ThermalNonequilib
        integer ChemicalEquilibCurveFit, ChemicalEquilibMinimization
        integer ChemicalNonequilib
        integer EMElectricField, EMMagneticField, Voltage, Interpolated
        integer EMConductivity, Equilibrium_LinRessler
        integer Chemistry_LinRessler
        character*32 ModelTypeName(0:35)

        parameter (Ideal                       = 2)
        parameter (VanderWaals                 = 3)
        parameter (Constant                    = 4)
        parameter (PowerLaw                    = 5)
        parameter (SutherlandLaw               = 6)
        parameter (ConstantPrandtl             = 7)
        parameter (EddyViscosity               = 8)
        parameter (ReynoldsStress              = 9)
        parameter (ReynoldsStressAlgebraic     = 10)
        parameter (Algebraic_BaldwinLomax      = 11)
        parameter (Algebraic_CebeciSmith       = 12)
        parameter (HalfEquation_JohnsonKing    = 13)
        parameter (OneEquation_BaldwinBarth    = 14)
        parameter (OneEquation_SpalartAllmaras = 15)
        parameter (TwoEquation_JonesLaunder    = 16)
        parameter (TwoEquation_MenterSST       = 17)
        parameter (TwoEquation_Wilcox          = 18)
        parameter (CaloricallyPerfect          = 19)
        parameter (ThermallyPerfect            = 20)
        parameter (ConstantDensity             = 21)
        parameter (RedlichKwong                = 22)
        parameter (Frozen                      = 23)
        parameter (ThermalEquilib              = 24)
        parameter (ThermalNonequilib           = 25)
        parameter (ChemicalEquilibCurveFit     = 26)
        parameter (ChemicalEquilibMinimization = 27)
        parameter (ChemicalNonequilib          = 28)
        parameter (EMElectricField             = 29)
        parameter (EMMagneticField             = 30)
        parameter (EMConductivity              = 31)
        parameter (Voltage                     = 32)
        parameter (Interpolated                = 33)
        parameter (Equilibrium_LinRessler      = 34)
        parameter (Chemistry_LinRessler        = 35)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Boundary Condition Types                                         *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer BCAxisymmetricWedge, BCDegenerateLine, BCDegeneratePoint
        integer BCDirichlet, BCExtrapolate, BCFarfield, BCGeneral
        integer BCInflow, BCInflowSubsonic,  BCInflowSupersonic
        integer BCNeumann
        integer BCOutflow, BCOutflowSubsonic, BCOutflowSupersonic
        integer BCSymmetryPlane, BCSymmetryPolar
        integer BCTunnelInflow, BCTunnelOutflow
        integer BCWall, BCWallInviscid, BCWallViscous
        integer BCWallViscousHeatFlux, BCWallViscousIsothermal
        integer FamilySpecified
        character*32 BCTypeName(0:25)
        parameter (BCAxisymmetricWedge     = 2)
        parameter (BCDegenerateLine        = 3)
        parameter (BCDegeneratePoint       = 4)
        parameter (BCDirichlet             = 5)
        parameter (BCExtrapolate           = 6)
        parameter (BCFarfield              = 7)
        parameter (BCGeneral               = 8)
        parameter (BCInflow                = 9)
        parameter (BCInflowSubsonic        = 10)
        parameter (BCInflowSupersonic      = 11)
        parameter (BCNeumann               = 12)
        parameter (BCOutflow               = 13)
        parameter (BCOutflowSubsonic       = 14)
        parameter (BCOutflowSupersonic     = 15)
        parameter (BCSymmetryPlane         = 16)
        parameter (BCSymmetryPolar         = 17)
        parameter (BCTunnelInflow          = 18)
        parameter (BCTunnelOutflow         = 19)
        parameter (BCWall                  = 20)
        parameter (BCWallInviscid          = 21)
        parameter (BCWallViscous           = 22)
        parameter (BCWallViscousHeatFlux   = 23)
        parameter (BCWallViscousIsothermal = 24)
        parameter (FamilySpecified         = 25)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Data types                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer Integer, RealSingle, RealDouble, Character
        character*32 DataTypeName(0:5)
        parameter (Integer    = 2)
        parameter (RealSingle = 3)
        parameter (RealDouble = 4)
        parameter (Character  = 5)


!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      BCData_t types                                                   *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer Dirichlet, Neumann
        character*32 BCDataTypeName(0:3)
        parameter (Dirichlet = 2)
        parameter (Neumann   = 3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Element types                                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer NODE, BAR_2, BAR_3, TRI_3, TRI_6, QUAD_4, QUAD_8, QUAD_9
        integer TETRA_4, TETRA_10, PYRA_5, PYRA_13, PYRA_14
        integer PENTA_6, PENTA_15, PENTA_18, HEXA_8, HEXA_20, HEXA_27
        integer MIXED, NGON_n, NFACE_n
        character*32 ElementTypeName(0:23)
        parameter (NODE     =  2)
        parameter (BAR_2    =  3)
        parameter (BAR_3    =  4)
        parameter (TRI_3    =  5)
        parameter (TRI_6    =  6)
        parameter (QUAD_4   =  7)
        parameter (QUAD_8   =  8)
        parameter (QUAD_9   =  9)
        parameter (TETRA_4  = 10)
        parameter (TETRA_10 = 11)
        parameter (PYRA_5   = 12)
        parameter (PYRA_13  = 13)
        parameter (PYRA_14  = 14)
        parameter (PENTA_6  = 15)
        parameter (PENTA_15 = 16)
        parameter (PENTA_18 = 17)
        parameter (HEXA_8   = 18)
        parameter (HEXA_20  = 19)
        parameter (HEXA_27  = 20)
        parameter (MIXED    = 21)
        parameter (NGON_n   = 22)
        parameter (NFACE_n  = 23)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Zone types                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer Structured, Unstructured
        character*32 ZoneTypeName(0:3)
        parameter (Structured   =  2)
        parameter (Unstructured =  3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Rigid Grid Motion types						 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer ConstantRate, VariableRate
        character*32 RigidGridMotionTypeName(0:3)
        parameter (ConstantRate = 2)
        parameter (VariableRate = 3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Arbitrary Grid Motion types                                      *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer NonDeformingGrid, DeformingGrid
        character*32 ArbitraryGridMotionTypeName(0:3)
        parameter (NonDeformingGrid = 2)
        parameter (DeformingGrid = 3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Simulation type                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer TimeAccurate, NonTimeAccurate
        character*32 SimulationTypeName(0:3)
        parameter (TimeAccurate = 2)
        parameter (NonTimeAccurate = 3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      BC Property types                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer Generic
        character*32 WallFunctionTypeName(0:2)
        parameter (Generic = 2)

        integer BleedArea, CaptureArea
        character*32 AreaTypeName(0:3)
        parameter (BleedArea = 2)
        parameter (CaptureArea = 3)

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Grid Connectivity Property types                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        integer AverageAll, AverageCircumferential, AverageRadial
        integer AverageI, AverageJ, AverageK
        character*32 AverageInterfaceTypeName(0:7)
        parameter (AverageAll = 2)
        parameter (AverageCircumferential = 3)
        parameter (AverageRadial = 4)
        parameter (AverageI = 5)
        parameter (AverageJ = 6)
        parameter (AverageK = 7)

! For portability to Linux Absoft, all data statements were moved after the
! variables and parametres declarations

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Dimensional Units                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
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

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Data Class                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
        data DataClassName / 'Null','UserDefined',                      &
     &          'Dimensional','NormalizedByDimensional',                &
     &          'NormalizedByUnknownDimensional',                       &
     &          'NondimensionalParameter','DimensionlessConstant'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Grid Location                                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data GridLocationName / 'Null','UserDefined',                   &
     &          'Vertex','CellCenter','FaceCenter','IFaceCenter',       &
     &          'JFaceCenter','KFaceCenter','EdgeCenter' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Grid Connectivity Types                                          *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data GridConnectivityTypeName / 'Null','UserDefined',           &
     &          'Overset','Abutting','Abutting1to1'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Point Set Types                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data PointSetTypeName / 'Null','UserDefined',                   &
     &          'PointList','PointListDonor',                           &
     &          'PointRange','PointRangeDonor',                         &
     &          'ElementRange','ElementList','CellListDonor'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Governing Equations and Physical Models Types                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
     &	      'Chemistry_LinRessler'/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Boundary Condition Types                                         *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Data types                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data DataTypeName / 'Null','UserDefined',                       &
     &          'Integer','RealSingle','RealDouble','Character' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      BCData_t types                                                   *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data BCDataTypeName / 'Null','UserDefined',                     &
     &          'Dirichlet', 'Neumann' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Element types                                                    *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data ElementTypeName / 'Null','UserDefined',                    &
     &      'NODE', 'BAR_2', 'BAR_3', 'TRI_3', 'TRI_6',                 &
     &      'QUAD_4', 'QUAD_8', 'QUAD_9', 'TETRA_4', 'TETRA_10',        &
     &      'PYRA_5', 'PYRA_13', 'PYRA_14', 'PENTA_6', 'PENTA_15',      &
     &      'PENTA_18', 'HEXA_8', 'HEXA_20', 'HEXA_27', 'MIXED',        &
     &      'NGON_n', 'NFACE_n' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Zone types                                                       *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data ZoneTypeName / 'Null','UserDefined',                       &
     &      'Structured', 'Unstructured' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Rigid Grid Motion types						 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data RigidGridMotionTypeName / 'Null','UserDefined',            &
     &       'ConstantRate', 'VariableRate' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Arbitrary Grid Motion types                                      *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data ArbitraryGridMotionTypeName / 'Null','UserDefined',        &
     &       'NonDeformingGrid', 'DeformingGrid' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Simulation type                                                  *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data SimulationTypeName / 'Null','UserDefined',                 &
     &       'TimeAccurate', 'NonTimeAccurate' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      BC Property types                                                *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data WallFunctionTypeName / 'Null','UserDefined',               &
     &       'Generic' /

        data AreaTypeName / 'Null','UserDefined',                       &
     &       'BleedArea', 'CaptureArea' /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
!*      Grid Connectivity Property types                                 *
!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

        data AverageInterfaceTypeName / 'Null','UserDefined',           &
     &       'AverageAll', 'AverageCircumferential', 'AverageRadial',   &
     &       'AverageI', 'AverageJ', 'AverageK' /
