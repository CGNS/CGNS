PROGRAM read_cgns_1
#include "cgnstypes_f03.h"
#ifdef WINNT
  INCLUDE 'cgnswin_f.h'
#endif
  USE ISO_C_BINDING
  USE CGNS
  IMPLICIT NONE

  ! This program reads a 3D mesh, structured or unstructured.

  INTEGER :: Ndim, Nglobal
  PARAMETER (Ndim = 3)
  PARAMETER (Nglobal = 500)
  INTEGER, PARAMETER :: sp = KIND(1.0)
  INTEGER, PARAMETER :: dp = KIND(1.d0)
  
  INTEGER :: i, narrays, iarray
  INTEGER :: nintegrals, integral
  INTEGER :: ndescriptors, idescr
  INTEGER(cgenum_t) :: nzonetype
  INTEGER(cgsize_t) :: nptsets
  INTEGER(cgenum_t) :: ndonor_ptset_type, ndonor_data_type
  INTEGER :: idataset, dirichletflag, neumannflag
  INTEGER IndexDim, CellDim, PhysDim
  INTEGER ier, n
  INTEGER(cgenum_t) :: zonetype
  INTEGER nbases, nzones
  INTEGER(cgsize_t) :: rmin(3), DataSize(Ndim)
  INTEGER(cgsize_t) :: SIZE(Ndim*3)
  INTEGER :: ncoords, nsols, nfields
  INTEGER(cgenum_t) :: location
  INTEGER(cgenum_t) :: TYPE
  INTEGER :: nholes, nconns, n1to1, n1to1_global, nbocos
  INTEGER(cgenum_t) :: ptset_type
  INTEGER(cgsize_t) :: npnts, pnts(100000), donor_pnts(100000)
  INTEGER(cgsize_t) :: npnts_donor
  INTEGER(cgenum_t) :: bocotype, datatype
  CHARACTER(len=32) basename, zonename, solname, fieldname
  CHARACTER(len=32) coordname, holename
#ifndef CG_BASESCOPE
  CHARACTER(len=32) connectname, donorname
#else
  CHARACTER(len=65) connectname, donorname
#endif
  CHARACTER(len=32) boconame
  INTEGER cg, base, zone, coord, sol, field, discr
  INTEGER :: hole, conn, one21, boco
  INTEGER(cgsize_t) :: RANGE(Ndim, 2), donor_range(Ndim, 2)
  INTEGER transform(Ndim)
  INTEGER(cgsize_t) :: G_range(Ndim*2, Nglobal)
  INTEGER(cgsize_t) :: G_donor_range(Ndim*2, Nglobal)
  INTEGER :: G_transform(Ndim, Nglobal)
  CHARACTER(len=32) G_zonename(Nglobal)
#ifndef CG_BASESCOPE
  CHARACTER(len=32) G_connectname(Nglobal), G_donorname(Nglobal)
#else
  CHARACTER(len=65) G_connectname(Nglobal), G_donorname(Nglobal)
#endif
  CHARACTER(len=32) name, filename
  CHARACTER(len=40) text, NormDefinitions, StateDescription
  INTEGER :: equation_dimension, GoverningEquationsFlag
  INTEGER :: GasModelFlag, ViscosityModelFlag
  INTEGER :: ThermalConductivityModelFlag
  INTEGER :: TurbulenceClosureFlag, TurbulenceModelFlag
  INTEGER :: diffusion_model(6)
  INTEGER :: niterations
  INTEGER :: rind(6), ndiscrete, num
  INTEGER :: nndim
  INTEGER(cgsize_t) :: dim_vals(12)
  INTEGER(cgenum_t) :: mass, length, time, temp, deg
  INTEGER :: NormalIndex(3), ndataset
  INTEGER(cgsize_t) :: NormalListSize
  REAL(KIND=sp) data_single(100000)
  REAL(KIND=dp) data_double(100000)
  REAL(KIND=sp) version

  INTEGER one, is_cgns
  PARAMETER (one = 1)

  ! *** open file
  !     write(6,*) 'Input filename'
  !     read(5,600) filename
  WRITE(filename,'(a)')'cgtest.cgns'

  ! *** check if the file is CGNS
  CALL cg_is_cgns_f(filename, is_cgns, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  IF ((is_cgns.NE.CG_FILE_ADF).AND.(is_cgns.NE.CG_FILE_HDF5).AND. &
       (is_cgns.NE.CG_FILE_ADF2)) &
       CALL cg_error_exit_f

  ! *** check if the user passes a file name with the null terminator
  CALL cg_is_cgns_f(TRIM(filename)//C_NULL_CHAR, is_cgns, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  IF ((is_cgns.NE.CG_FILE_ADF).AND.(is_cgns.NE.CG_FILE_HDF5).AND. &
       (is_cgns.NE.CG_FILE_ADF2)) &
       CALL cg_error_exit_f

  CALL cg_open_f(filename, CG_MODE_READ, cg, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  WRITE(6,600)'READING FILE ',filename

  ! *** CGNS Library Version used for file creation:
  CALL cg_version_f(cg, version, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  WRITE(6,102) &
       'Library Version used for file creation: ',version

  ! *** base
  CALL cg_nbases_f(cg, nbases, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f
  WRITE(6,200)'nbases=',nbases

  DO base=1, nbases

     CALL cg_base_read_f(cg, base, basename, CellDim, PhysDim, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f
     WRITE(6,300)'BaseName = "',TRIM(basename),'"', &
          'cell_dimension=',CellDim

     ! *** base attribute:  GOTO base node
     CALL cg_goto_f(cg, base, ier, 'end')
     IF (ier .EQ. ERROR) CALL cg_error_exit_f

     ! ***     base attribute:  Descriptor
     CALL cg_descriptor_read_f(one, name, text, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f
     IF (ier.EQ.ALL_OK) THEN
        WRITE(6,400)'Base Descriptor_t Information:'
        WRITE(6,500)' DescriptorName="',TRIM(name),'"', &
             ' DescriptorText="',TRIM(text),'"'
     ENDIF

     ! ***     base attribute: flow equation set:
     CALL cg_equationset_read_f(equation_dimension, &
          GoverningEquationsFlag,  GasModelFlag, &
          ViscosityModelFlag, ThermalConductivityModelFlag, &
          TurbulenceClosureFlag,  TurbulenceModelFlag, ier)
     IF (ier .EQ. ERROR) THEN
        CALL cg_error_exit_f
     ELSEIF (ier .EQ. NODE_NOT_FOUND) THEN
        WRITE(6,200)&
             'FlowEquationSet_t not defined under CGNSBase_t #',base
     ELSEIF (ier .EQ. INCORRECT_PATH) THEN
        WRITE(6,400)'Incorrect path input to cg_goto_f'
     ELSE
        WRITE(6,400) 'FlowEquationSet_t Information:'
        WRITE(6,100)' equation_dimension=',equation_dimension

        ! ***       flow equation set attributes:  GOTO FlowEquationSet_t node
        CALL cg_goto_f(cg,base,ier,'FlowEquationSet_t',one,'end')
        IF (ier .EQ. ERROR) CALL cg_error_exit_f

        ! ***       flow equation set attribute: Descriptor
        CALL cg_descriptor_read_f(one, name,text,ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        IF (ier .EQ. ALL_OK) WRITE(6,500) &
             ' DescriptorName="',TRIM(name),'"',' DescriptorText="',TRIM(text),'"'

        ! ***       flow equation set attribute: Gas Model Type
        IF (GasModelFlag.EQ.1) THEN
           CALL cg_model_read_f('GasModel_t', TYPE, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier .EQ. ALL_OK) WRITE(6,600) &
                ' GasModelType="',TRIM(ModelTypeName(TYPE)),'"'
        ENDIF

        ! ***       flow equation set attribute: ViscosityModel Type
        IF (ViscosityModelFlag.EQ.1) THEN
           CALL cg_model_read_f('ViscosityModel_t', TYPE, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier .EQ. ALL_OK) WRITE(6,600) &
                ' ViscosityModelType="',TRIM(ModelTypeName(TYPE)),'"'
        ENDIF

        ! ***       flow equation set attribute:  TypmlConductivityModel Type
        IF (ThermalConductivityModelFlag.EQ.1) THEN
           CALL cg_model_read_f('ThermalConductivityModel_t', &
                TYPE, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier .EQ. ALL_OK) WRITE(6,600) &
                ' ThermalConductivityModelType=', &
                TRIM(ModelTypeName(TYPE)),'"'
        ENDIF

        ! ***   flow equation set attribute: TurbulenceClosureType
        IF (TurbulenceClosureFlag.EQ.1) THEN
           CALL cg_model_read_f('TurbulenceClosure_t', TYPE, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier .EQ. ALL_OK) WRITE(6,600) &
                ' TurbulenceClosureType="', TRIM(ModelTypeName(TYPE)),'"'
        ENDIF

        ! ***   flow equation set attribute: TurbulenceModelType
        IF (TurbulenceModelFlag.EQ.1) THEN
           CALL cg_model_read_f('TurbulenceModel_t', TYPE, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier .EQ. ALL_OK) WRITE(6,600) &
                ' TurbulenceModelType="',TRIM(ModelTypeName(TYPE)),'"'
        ENDIF

        ! ***   flow equation set attribute: Governing Equations Type
        IF (GoverningEquationsFlag .EQ. 1) THEN
           CALL cg_governing_read_f(TYPE, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier.EQ.ALL_OK)&
                WRITE(6,600)' GoverningEquationsType="', &
                TRIM(GoverningEquationsTypeName(TYPE)),'"'

           ! *** Governing Equations attribute:  GOTO GoverningEquations_t node
           CALL cg_goto_f(cg,base,ier, 'FlowEquationSet_t', one, &
                'GoverningEquations_t', one ,'end')
           IF (ier .EQ. ERROR) CALL cg_error_exit_f


           ! *** Governing Equations attribute:  Diffusion model
           CALL cg_diffusion_read_f(diffusion_model, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier.EQ.ALL_OK)WRITE(6,103)'   Diffusion model=', &
                (diffusion_model(i), i=1,6)
        ENDIF       ! If Governing Equations are defined
     ENDIF          ! If FlowEquationSet_t exists under CGNSBase_t


     WRITE(6,400)'                              *     *     *'

     CALL cg_nzones_f(cg, base, nzones, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f
     WRITE(6,200)'nzones=',nzones

     ! *** zone
     DO zone=1, nzones
        CALL cg_zone_read_f(cg, base, zone, zonename, size, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        WRITE(6,104)'Name of Zone',zone,' is "',TRIM(zonename),'"'

        CALL cg_zone_type_f(cg, base, zone, zonetype, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        WRITE(6,600)'  Zone type is ', ZoneTypeName(zonetype)


        IF (zonetype.EQ.CGNS_ENUMV(Structured)) THEN
           IndexDim=CellDim
        ELSE
           IndexDim=1
        ENDIF


        WRITE(6,104)'  IndexDimension=',IndexDim

        ! *** zone attribute:  GOTO zone node
        CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, 'end')
        IF (ier .EQ. ERROR) CALL cg_error_exit_f

        ! *** zone attribute:  ordinal
        CALL cg_ordinal_read_f(num, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        IF (ier .EQ. ALL_OK)&
             WRITE(6,200)' Zone ordinal=',num


        ! *** zone attribute:  convergence history
        CALL cg_convergence_read_f(niterations, &
             NormDefinitions, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f

        IF (ier .EQ. ALL_OK) THEN
           WRITE(6,600)'Convergence History of ',zonename
           WRITE(6,104) ' niterations=',niterations, &
                ' NormDefinitions="',TRIM(NormDefinitions),'"'

           ! ** ConvergenceHistory_t attributes:
           CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                'ConvergenceHistory_t', one, 'end')
           IF (ier .EQ. ERROR) CALL cg_error_exit_f

           ! ** ConvergenceHistory_t attributes: DataArray_t
           CALL cg_narrays_f(narrays, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,105) 'ConvergenceHistory_t contains ', &
                narrays,' array(s)'
           DO iarray=1, narrays
              CALL cg_array_info_f(iarray, name, datatype, &
                   nndim, dim_vals, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              WRITE(6,600) ' DataArrayName="',TRIM(name),'"'
              WRITE(6,600) ' DataType="',TRIM(DataTypeName(datatype)),'"'
              WRITE(6,200) ' DataNdim=',nndim
              WRITE(6,200) ' DataDim=',dim_vals(1)

              WRITE(6,105) ' Data:'
              IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                 CALL cg_array_read_f(iarray, data_single, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 WRITE(6,106) (data_single(n),n=1,dim_vals(1))
              ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                 CALL cg_array_read_f(iarray, data_double, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 WRITE(6,106) (data_double(n),n=1,dim_vals(1))
              ENDIF
           ENDDO

           ! ** ConvergenceHistory_t attributes: DataClass_t
           CALL cg_dataclass_read_f(TYPE,ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,600)'DataClassName=',DataClassName(TYPE)

           ! ** ConvergenceHistory_t attributes: DimensionalUnits_t
           CALL cg_units_read_f(mass, length, time, temp, deg, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier .EQ. ALL_OK) THEN
              WRITE(6,100) &
                   'Dimensional Units:', &
                   MassUnitsName(mass), LengthUnitsName(length), &
                   TemperatureUnitsName(temp), TimeUnitsName(time), &
                   AngleUnitsName(deg)
           ENDIF
        ENDIF
        WRITE(6,400)'                             *     *     *'

        ! *** zone attribute:  return to Zone_t node
        CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, 'end')
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        WRITE(6,401)'Integral Data Information of ',zonename

        CALL cg_nintegrals_f(nintegrals, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        WRITE(6,107) nintegrals, ' IntegralData_t node in ', &
             zonename

        ! *** zone attribute:  IntegralData_t
        DO integral=1, nintegrals
           CALL cg_integral_read_f(integral, name, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,104) 'IntegralData_t #',integral, &
                ' is named "', TRIM(name),'"'

           ! *** IntegralData_t attribute:  GOTO IntegralData_t node
           CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                'IntegralData_t', integral, 'end')
           IF (ier .EQ. ERROR) CALL cg_error_exit_f

           CALL cg_narrays_f(narrays, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,108) 'IntegralData_t #',integral, &
                ' contains ', narrays,' data'

           DO iarray=1, narrays

              ! *** IntegralData_t attribute: DataArray_t
              CALL cg_array_info_f(iarray, name, datatype, &
                   nndim, dim_vals, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,600) ' DataArrayName="',TRIM(name),'"'
              WRITE(6,600) ' DataType=',DataTypeName(datatype)
              WRITE(6,108) ' DataNdim=',nndim, &
                   ', DataDim=',dim_vals(1)

              IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                 CALL cg_array_read_f(iarray, data_single, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 WRITE(6,109) ' integraldata=',data_single(1)
              ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                 CALL cg_array_read_f(iarray, data_double, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 WRITE(6,109) 'integraldata=',data_double(1)
              ENDIF

              ! *** DattaArray_t attribute: GOTO DataArray_t
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'IntegralData_t', integral, &
                   'DataArray_t', iarray, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f


              ! *** DattaArray_t attribute: DimensionalExponents_t
              CALL cg_exponents_info_f(datatype, ier)
              IF (ier .EQ. ERROR) THEN
                 CALL cg_error_exit_f
              ELSEIF (ier .EQ. ALL_OK) THEN
                 WRITE(6,600)' Datatype for exponents is ', &
                      DataTypeName(datatype)
                 IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                    CALL cg_exponents_read_f(data_single, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,110)' Exponents:',(data_single(n),n=1,5)
                 ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                    CALL cg_exponents_read_f(data_double, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,110)' Exponents:',(data_double(n),n=1,5)
                 ENDIF
              ENDIF

              ! *** DattaArray_t attribute: DataConversion_t
              CALL cg_conversion_info_f(datatype, ier)
              IF (ier .EQ. ERROR) THEN
                 CALL cg_error_exit_f
              ELSEIF (ier .EQ. ALL_OK) THEN
                 WRITE(6,600)' Datatype for conversion is ', &
                      DataTypeName(datatype)
                 IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                    CALL cg_conversion_read_f(data_single, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,110)' Conversion:',(data_single(n),n=1,2)
                 ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                    CALL cg_conversion_read_f(data_double, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,110)' Conversion:',(data_double(n),n=1,2)
                 ENDIF
              ENDIF

           ENDDO ! loop through DataArray_t
        ENDDO ! loop through IntegralData_t

        WRITE(6,400)'                             *     *     *'

        ! *** zone coordinate attribute:  GOTO GridCoordinates_t node
        CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
             'GridCoordinates_t', one, 'end')
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        IF (ier .EQ. ALL_OK) THEN

           ! *** GridCoordinates_t attribute: dimensional units
           CALL cg_units_read_f(mass, length, time, temp, deg, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier .EQ. ALL_OK) WRITE(6,400) &
                'Dimensional Units for GridCoordinates_t: ', &
                LengthUnitsName(length)

           ! *** GridCoordinates_t attribute:  Rind
           CALL cg_rind_read_f(rind, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,103)'GC Rind Data is ',(rind(i),i=1,6)

           ! *** coordinate array
           CALL cg_narrays_f(narrays, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,105) 'GridCoordinates_t contains ', &
                narrays,' arrays'
           DO iarray=1,narrays

              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'GridCoordinates_t', one, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              ! *** GridCoordinates_t attribute: DataArray_t
              CALL cg_array_info_f(iarray, name, datatype, &
                   nndim, dim_vals, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,600)' DataArrayName="',TRIM(name),'"'
              WRITE(6,600)' DataType=',DataTypeName(datatype)
              WRITE(6,104)' DataNdim=',nndim
              DO i=1,nndim
                 WRITE(6,111)' DataDim(',i,')=',dim_vals(i)
              ENDDO

              ! *** Compute nr of data in data array:
              num = 1
              DO i=1,nndim
                 num = num*dim_vals(i)
              ENDDO

              IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                 CALL cg_array_read_f(iarray, data_single, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 WRITE(6,106) (data_single(i),i=1,2)
                 WRITE(6,106) (data_single(i),i=num-1,num)
              ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                 CALL cg_array_read_f(iarray, data_double, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 WRITE(6,106) (data_double(i),i=1,2)
                 WRITE(6,106) (data_double(i),i=num-1,num)
              ENDIF

              ! *** coordinate attribute:  GOTO coordinate array node
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'GridCoordinates_t', one, 'DataArray_t', iarray, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              CALL cg_ndescriptors_f(ndescriptors, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,105) 'No. of descriptors=',ndescriptors
              DO idescr=1, ndescriptors
                 CALL cg_descriptor_read_f(idescr, name, text, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 WRITE(6,500) ' DescriptorName="',TRIM(name),'"', &
                      ' DescriptorText="',TRIM(text),'"'
              ENDDO

           ENDDO ! loop through data arrays

           ! *** read coordinates using coordinate arrays' specific functions:

           WRITE(6,400)'Specific functions to read coordinates arrays'
           CALL cg_ncoords_f(cg, base, zone, ncoords, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,103)'no. of coordinates=',ncoords

           ! ** Compute the nr of data to be read
           DO i=1,IndexDim
              rmin(i)=1
              DataSize(i)=SIZE(i) + rind(2*i-1) + rind(2*i)
           ENDDO

           DO coord=1, ncoords
              CALL cg_coord_info_f(cg, base, zone, coord, datatype, &
                   coordname, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,112)'coord #',coord, &
                   '   datatype=',DataTypeName(datatype), &
                   '   name="',TRIM(coordname),'"'

              IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                 CALL cg_coord_read_f(cg, base, zone, coordname, &
                      cg_get_type(data_single(1)), rmin, DataSize, &
                      data_single, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f

              ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                 CALL cg_coord_read_f(cg, base, zone, coordname, &
                      cg_get_type(data_double(1)), rmin, DataSize, &
                      data_double, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
              ENDIF
           ENDDO
        ENDIF ! if GridCoordinates_t exists

        WRITE(6,400)'                             *     *     *'

        ! *** solution

        CALL cg_nsols_f(cg, base, zone, nsols, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        WRITE(6,113) nsols,' FlowSolution_t node(s)', &
             'found for ',zonename

        ! *** Read solution with general cg_array_read function
        DO sol=1, nsols
           CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                'FlowSolution_t', sol, 'end')
           IF (ier .EQ. ERROR) CALL cg_error_exit_f

           ! *** FlowSolution_t attribute:  DataArray_t
           CALL cg_narrays_f(narrays, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,108) ' FlowSolution_t #',sol, &
                ' contains ',narrays,' solution arrays'

           ! *** FlowSolution_t attribute:  GridLocation
           CALL cg_gridlocation_read_f(location, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,600)'  The solution data are recorded at the ', &
                GridLocationName(location)

           ! *** FlowSolution_t attribute:  Rind
           CALL cg_rind_read_f(rind, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,103)'  The Rind Data is ',(rind(i),i=1,6)

           DO iarray=1,narrays
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'FlowSolution_t', sol, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              CALL cg_array_info_f(iarray, name, datatype, &
                   nndim, dim_vals, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,114) '  DataArray #',iarray
              WRITE(6,600) '   Name="',TRIM(name),'"'
              WRITE(6,600) '   DataType=',DataTypeName(datatype)
              WRITE(6,103) '   DataNdim=',nndim
              DO i=1,nndim
                 WRITE(6,111)'   DataDim(',i,')=',dim_vals(i)
              ENDDO

              ! *** For dynamic memory allocation, compute the number of data to be read:
              num = 1
              DO i=1,nndim
                 num = num*dim_vals(i)
              ENDDO
              WRITE(6,200) 'Nr of data in solution vector=',num

              IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                 CALL cg_array_read_f(iarray, data_single, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 !write(6,106) (data_single(i),i=1,num)
              ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                 CALL cg_array_read_f(iarray, data_double, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 !write(6,106) (data_double(i),i=1,num)
              ENDIF

              ! *** solution field attribute:  GOTO solution array node
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'FlowSolution_t',sol,'DataArray_t',iarray,'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              ! *** solution field attribute:  DimensionalUnits
              CALL cg_units_read_f(mass, length, time, temp, &
                   deg, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              IF (ier .EQ. ALL_OK) THEN
                 WRITE(6,100)&
                      '   Dimensional Units:', &
                      MassUnitsName(mass), LengthUnitsName(length), &
                      TemperatureUnitsName(temp), TimeUnitsName(time), &
                      AngleUnitsName(deg)
              ENDIF

           ENDDO     ! loop through DataArray_t
           WRITE(6,103)' '

           ! *** Reading solution data with solution specific functions:
           CALL cg_sol_info_f(cg, base, zone, sol, solname, &
                location, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,115)'sol #',sol,':', &
                '   solname="',TRIM(solname),'"', &
                '   location=',GridLocationName(location)

           ! *** Compute the nr of data to be read

           IF (zonetype.EQ.CGNS_ENUMV(Structured)) THEN
              DO i=1,3
                 DataSize(i)=SIZE(i) + rind(2*i-1) + rind(2*i)
                 IF (location.EQ.CGNS_ENUMV(CellCenter)) DataSize(i)=DataSize(i)-1
              ENDDO
           ELSE
              DataSize(1)=SIZE(2)
           ENDIF

           ! *** solution field
           CALL cg_nfields_f(cg, base, zone, sol, nfields, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,105)'  nfields=',nfields

           DO field=1, nfields
              CALL cg_field_info_f(cg, base, zone, sol, field, &
                   TYPE, fieldname, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,115)'  field #',field,':', &
                   '   fieldname="',TRIM(fieldname),'"', &
                   '   datatype=',DataTypeName(TYPE)

              ! *** read entire range of solution data and record in double precision
              CALL cg_field_read_f(cg, base, zone, sol, fieldname, &
                   CGNS_ENUMV(RealDouble), rmin, DataSize, data_double, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
           ENDDO                             ! field loop

        ENDDO     ! loop through FlowSolution_t

        WRITE(6,400)'                             *     *     *'

        ! *** discrete data under zone
        CALL cg_ndiscrete_f(cg, base, zone, ndiscrete, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        IF (ier .EQ. ALL_OK) WRITE(6,113)ndiscrete, &
             ' DiscreteData_t node(s) found under ',zonename

        DO discr=1, ndiscrete
           CALL cg_discrete_read_f(cg, base,zone, discr, name, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,600)' name=',name

           ! *** discrete data attribute:  GOTO DiscreteData_t node
           CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                'DiscreteData_t',  discr, 'end')
           IF (ier .EQ. ERROR) CALL cg_error_exit_f

           ! *** discrete data attribute:  GridLocation_t
           CALL cg_gridlocation_read_f(location, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier .EQ. ALL_OK) WRITE(6,600) &
                ' The location of the DiscreteData vector is ', &
                GridLocationName(location)

           ! *** discrete data arrays:
           CALL cg_narrays_f(narrays, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,116) ' DiscreteData #', discr, &
                ' contains ', narrays,' arrays'
           DO iarray=1, narrays
              CALL cg_array_info_f(iarray, name, datatype, &
                   nndim, dim_vals, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              WRITE(6,116) 'DataArray #',iarray,':'
              WRITE(6,600)'  Name =',name
              WRITE(6,600)'  Datatype=', &
                   DataTypeName(datatype)

              ! *** compute nr of data to be read
              num=1
              DO n=1, nndim
                 num=num*dim_vals(n)
              ENDDO

              IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                 CALL cg_array_read_f(iarray, data_single, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 !write(6,*) (data_single(n),n=1,num)
              ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                 CALL cg_array_read_f(iarray, data_double, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 !write(6,*) (data_double(n),n=1,num)
              ENDIF

              ! *** discrete data arrays attribute: GOTO DataArray node
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'DiscreteData_t', discr, 'DataArray_t', iarray, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              CALL cg_units_read_f(mass, length, time, temp, deg, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              IF (ier .EQ. ALL_OK) THEN
                 WRITE(6,100)&
                      '  Dimensional Units for DiscreteData_t:', &
                      MassUnitsName(mass), LengthUnitsName(length), &
                      TemperatureUnitsName(temp), TimeUnitsName(time), &
                      AngleUnitsName(deg)
              ENDIF
           ENDDO          ! loop through DataArray_t
        ENDDO

        WRITE(6,400)'                             *     *     *'

        ! *** Interblock Connectivity:
        WRITE(6,401)'Interblock Connectivity for ',zonename

        ! *** ZoneGridConnectivity attributes:  GOTO ZoneGridConnectivity_t node
        CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
             'ZoneGridConnectivity_t', one, 'end')
        IF (ier .EQ. ERROR) CALL cg_error_exit_f

        IF (ier.EQ. ALL_OK) THEN
           ! *** ZoneGridConnectivity attributes: Descriptor_t
           CALL cg_ndescriptors_f(ndescriptors, ier)
           IF (ier .NE. 0) CALL cg_error_exit_f
           WRITE(6,117)&
                ndescriptors, ' descriptors for ZoneGridConnectivity_t'
           DO idescr=1, ndescriptors
              CALL cg_descriptor_read_f(idescr, name, text, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,500) '     DescriptorName="',TRIM(name),'"', &
                   '     DescriptorText="',TRIM(text),'"'
           ENDDO


           ! *** overset holes
           CALL cg_nholes_f(cg, base, zone, nholes, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,107) nholes, ' holes found'

           DO hole=1, nholes
              CALL cg_hole_info_f(cg, base, zone, hole, holename, &
                   location, ptset_type, nptsets, npnts, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,118)&
                   '  hole #',hole,':', '   holename="',TRIM(holename),'"', &
                   '   data location=',GridLocationName(location), &
                   '   nptsets = ',nptsets, &
                   ', total no. of points =',npnts

              IF (npnts .LT. 30000) THEN
                 CALL cg_hole_read_f(cg, base, zone, hole, pnts, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
              ENDIF

              ! *** overset holes attributes:  GOTO OversetHoles_t node
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'ZoneGridConnectivity_t', one, &
                   'OversetHoles_t', hole, 'end')
              IF (ier .NE. 0) CALL cg_error_exit_f

              CALL cg_ndescriptors_f(ndescriptors, ier)
              IF (ier .NE. 0) CALL cg_error_exit_f
              WRITE(6,117)&
                   ndescriptors, ' descriptors for ',holename
              DO idescr=1, ndescriptors
                 CALL cg_descriptor_read_f(idescr, name, text, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
                 WRITE(6,500) '     DescriptorName="',TRIM(name),'"', &
                      '     DescriptorText="',TRIM(text),'"'
              ENDDO
           ENDDO     !hole loop



           ! *** general connectivity
           CALL cg_nconns_f(cg, base, zone, nconns, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,107) nconns,' GridConnectivity_t found'
           DO conn=1, nconns
              CALL cg_conn_info_f(cg, base, zone, conn, connectname, &
                   location, TYPE, ptset_type, npnts, donorname, &
                   nzonetype, ndonor_ptset_type, ndonor_data_type, &
                   npnts_donor, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              WRITE(6, 101) &
                   '  GridConnectivity #',conn,':', &
                   '   connect name ='//TRIM(connectname), &
                   '   Grid location='//TRIM(GridLocationName(location)), &
                   '   Connect-type ='//TRIM(GridConnectivityTypeName(TYPE)), &
                   '   ptset type   ="'//TRIM(PointSetTypeName(ptset_type))//'"', &
                   '   npnts=',npnts,'   donorname="'//TRIM(donorname)//'"', &
                   '   donor zonetype='//TRIM(ZoneTypeName(nzonetype)), &
                   '   donor ptset type='//TRIM(PointSetTypeName(ndonor_ptset_type)), &
                   '   npnts_donor=',npnts_donor

              CALL cg_conn_read_f(cg, base, zone, conn, pnts, &
                   cg_get_type(donor_pnts(1)), &
                   donor_pnts, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              WRITE(6,119) '   Current zone:', &
                   '    first point:', pnts(1),pnts(2),pnts(3), &
                   '    last point :', pnts(3*npnts-2), pnts(3*npnts-1), &
                   pnts(3*npnts)
              WRITE(6,119) '   Donor zone:', &
                   '    first point:', donor_pnts(1),donor_pnts(2), &
                   donor_pnts(3), &
                   '    last point :', donor_pnts(3*npnts-2), &
                   donor_pnts(3*npnts-1), &
                   donor_pnts(3*npnts)

              ! *** general connectivity attributes:  GOTO GridConnectivity_t node
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'ZoneGridConnectivity_t', one, &
                   'GridConnectivity_t', conn, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              CALL cg_ordinal_read_f(num, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              IF (ier .EQ. ALL_OK) WRITE(6,200)'  Ordinal=',num
           ENDDO

           ! *** connectivity 1to1
           CALL cg_n1to1_f(cg, base, zone, n1to1, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,107) n1to1,' GridConnectivity1to1_t found'

           DO one21=1, n1to1
              CALL cg_1to1_read_f(cg, base, zone, one21, connectname, &
                   donorname, range, donor_range, transform, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              WRITE(6,105) 'GridConnectivity1to1 #',one21
              WRITE(6,600) 'connectname="',TRIM(connectname),'"'
              WRITE(6,600) 'donorname  ="',TRIM(donorname),'"'

              WRITE(6,120) ' range: ', &
                   '(',RANGE(1,1),',',RANGE(2,1),',',RANGE(3,1), &
                   ') to (',RANGE(1,2),',',RANGE(2,2),',',RANGE(3,2),')'

              WRITE(6,121)' donor_range: ', &
                   '(', donor_range(1,1), ',', donor_range(2,1), ',', &
                   donor_range(3,1), ') to (', &
                   donor_range(1,2), ',', donor_range(2,2), ',', &
                   donor_range(3,2), ')'

              WRITE(6,122) ' Transform: ', '(', &
                   transform(1), ',', &
                   transform(2), ',', transform(3), ')'


              ! *** connectivity 1to1 attributes:  GOTO GridConnectivity1to1_t node
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'ZoneGridConnectivity_t', one, &
                   'GridConnectivity1to1_t', one21, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              IF (ier .EQ. ALL_OK) THEN

                 ! *** connectivity 1to1 attributes:  Descriptor_t
                 CALL cg_ndescriptors_f(ndescriptors, ier)
                 IF (ier .NE. 0) CALL cg_error_exit_f
                 WRITE(6,117)&
                      ndescriptors, ' descriptors for ',connectname
                 DO idescr=1, ndescriptors
                    CALL cg_descriptor_read_f(idescr, name, text, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,500) '   DescriptorName="',TRIM(name),'"', &
                         '   DescriptorText="',TRIM(text),'"'
                 ENDDO
              ENDIF
           ENDDO
        ENDIF ! if ZoneGridConnectivity exists

        WRITE(6,400)'                             *     *     *'

        ! *** bocos
        WRITE(6,600)'Boundary Conditions for ',zonename


        ! *** Zone bound. condition attributes: GOTO ZoneBC_t node
        CALL cg_goto_f(cg, base,ier, 'Zone_t', zone, &
             'ZoneBC_t', one, 'end')
        IF (ier .EQ. ERROR) CALL cg_error_exit_f
        IF (ier .EQ. ALL_OK) THEN

           ! *** Zone bound. condition attributes: ReferenceState_t
           CALL cg_state_read_f(StateDescription, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           IF (ier.EQ.ALL_OK) THEN
              WRITE(6,600)' ReferenceState defined under ZoneBC_t'
              WRITE(6,600)'  StateDescription=',StateDescription

              ! ** ReferenceState_t attributes:  GOTO ReferenceState_t
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'ZoneBC_t', one, 'ReferenceState_t', one, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              CALL cg_narrays_f(narrays, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,105) '  ReferenceState_t contains ', &
                   narrays,' array(s)'

              DO iarray=1, narrays

                 CALL cg_array_info_f(iarray, name, datatype, &
                      nndim, dim_vals, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f

                 WRITE(6,105) '   DataArray #',iarray,':'
                 WRITE(6,600)'    Name =',name
                 WRITE(6,600)'    Datatype=',DataTypeName(datatype)

                 WRITE(6,600)'    Data:'
                 IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                    CALL cg_array_read_f(iarray, data_single, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,124) data_single(1)
                 ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                    CALL cg_array_read_f(iarray, data_double, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,124) data_double(1)
                 ENDIF
              ENDDO


              ! ** ReferenceState_t attributes: DimensionalUnits_t
              CALL cg_units_read_f(mass, length, time, temp, &
                   deg, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              IF (ier .EQ. ALL_OK) THEN
                 WRITE(6,100)&
                      '  Dimensional Units:', &
                      MassUnitsName(mass), LengthUnitsName(length), &
                      TemperatureUnitsName(temp), TimeUnitsName(time), &
                      AngleUnitsName(deg)
              ENDIF
           ENDIF  !if ReferenceState exists under ZoneBC_t

           CALL cg_nbocos_f(cg, base, zone, nbocos, ier)
           IF (ier .EQ. ERROR) CALL cg_error_exit_f
           WRITE(6,113)nbocos,' bound. conditions found for ', &
                zonename

           DO boco=1, nbocos
              CALL cg_boco_info_f(cg, base, zone, boco, boconame, &
                   bocotype, ptset_type, npnts, &
                   NormalIndex, NormalListSize, datatype, &
                   ndataset, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              WRITE(6,105) ' boundary condition #',boco
              WRITE(6,600) '  boconame=',boconame
              WRITE(6,600) '  bocotype=',BCTypeName(bocotype)
              WRITE(6,600) '  ptset_type=', &
                   PointSetTypeName(ptset_type)
              WRITE(6,103) '  NormalIndex=', &
                   NormalIndex(1),NormalIndex(2), NormalIndex(3)
              WRITE(6,104) '  NormalListSize=',NormalListSize
              WRITE(6,600) '  datatype for normals=', &
                   DataTypeName(datatype)

              ! read patch points and InwardNormalList
              IF (datatype.EQ.CGNS_ENUMV(RealSingle)) THEN
                 CALL cg_boco_read_f(cg, base, zone, boco, pnts, &
                      data_single, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
              ELSEIF (datatype.EQ.CGNS_ENUMV(RealDouble)) THEN
                 CALL cg_boco_read_f(cg, base, zone, boco, pnts, &
                      data_double, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f
              ENDIF

              WRITE(6,119) '   Bound. Condition Patch:', &
                   '    first point:', pnts(1),pnts(2),pnts(3), &
                   '    last point :', pnts(3*npnts-2), pnts(3*npnts-1), &
                   pnts(3*npnts)

              IF (NormalListSize .NE. 0) THEN
                 IF (datatype.EQ.CGNS_ENUMV(RealSingle)) &
                      WRITE(6,126) '   Normals:', &
                      '    first point:', data_single(1),data_single(2), &
                      data_single(3), &
                      '    last point :', data_single(3*npnts-2), &
                      data_single(3*npnts-1), &
                      data_single(3*npnts)
                 IF (datatype.EQ.CGNS_ENUMV(RealDouble)) &
                      WRITE(6,126) '   Normals:', &
                      '    first point:', data_double(1),data_double(2), &
                      data_double(3), &
                      '    last point :', data_double(3*npnts-2), &
                      data_double(3*npnts-1), &
                      data_double(3*npnts)
              ENDIF
              ! ***  bound. condition attributes: GOTO BC_t node
              CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                   'ZoneBC_t', one, 'BC_t', boco, 'end')
              IF (ier .EQ. ERROR) CALL cg_error_exit_f

              ! ***  bound. condition attributes: DataClass_t
              CALL cg_dataclass_read_f(TYPE,ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              IF (ier.EQ.ALL_OK)&
                   WRITE(6,600)'  B.C. DataClass=', &
                   DataClassName(TYPE)

              ! ***  boundary condition attributes:  GridLocation_t
              CALL cg_gridlocation_read_f(location, ier)
              IF (ier .EQ. ERROR) CALL cg_error_exit_f
              IF (ier.EQ.ALL_OK)&
                   WRITE(6,600)'    data location=', &
                   GridLocationName(location)

              ! ** boundary condition dataset
              WRITE(6,103) '  ndataset=',ndataset
              DO idataset=1, ndataset
                 CALL cg_dataset_read_f(cg, base, zone, boco,idataset, &
                      name, TYPE, DirichletFlag, NeumannFlag, ier)
                 IF (ier .EQ. ERROR) CALL cg_error_exit_f

                 WRITE(6,103)'   Dataset #',idataset
                 WRITE(6,600)'    Name=',name
                 WRITE(6,600)'    BCType=',BCTypeName(TYPE)

                 ! ** boundary condition data:  GOTO BCData_t node
                 IF (DirichletFlag.EQ.1) THEN
                    CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                         'ZoneBC_t', one, 'BC_t', boco, 'BCDataSet_t', &
                         idataset,'BCData_t',CGNS_ENUMV(Dirichlet),'end')
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f

                    ! ** boundary condition data attributes: DataClass_t
                    WRITE(6,401)'   Dirichlet DataSet:'
                    CALL cg_dataclass_read_f(TYPE,ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,600)'    DataClass=', &
                         DataClassName(TYPE)

                    ! ** boundary condition data attributes: DataArray_t
                    CALL cg_narrays_f(narrays, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,127) '    DirichletData', &
                         ' contains ', narrays,' data arrays'
                    DO iarray=1, narrays
                       CALL cg_array_info_f(iarray, name, datatype, &
                            nndim, dim_vals, ier)
                       IF (ier .EQ. ERROR) CALL cg_error_exit_f

                       WRITE(6,105) '    DataArray #',iarray,':'
                       WRITE(6,600)'     Name =',name
                       WRITE(6,600)'     Datatype=', &
                            DataTypeName(datatype)

                       WRITE(6,105)'    Dirichlet Data:'
                       IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                          CALL cg_array_read_f(iarray, data_single, ier)
                          IF (ier .EQ. ERROR) CALL cg_error_exit_f
                          WRITE(6,106)&
                               (data_single(n),n=1,dim_vals(1))

                       ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                          CALL cg_array_read_f(iarray, data_double, ier)
                          IF (ier .EQ. ERROR) CALL cg_error_exit_f
                          WRITE(6,106)&
                               (data_double(n),n=1,dim_vals(1))
                       ENDIF
                    ENDDO
                 ENDIF

                 IF (NeumannFlag.EQ.1) THEN
                    CALL cg_goto_f(cg, base, ier, 'Zone_t', zone, &
                         'ZoneBC_t', one, 'BC_t', boco, 'BCDataSet_t', &
                         idataset, 'BCData_t', CGNS_ENUMV(Neumann),'end')
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f

                    ! ** boundary condition data attributes: DataClass_t
                    CALL cg_dataclass_read_f(TYPE,ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,600)'    DataClass=', &
                         DataClassName(TYPE)

                    ! ** boundary condition data attributes: DataArray_t
                    CALL cg_narrays_f(narrays, ier)
                    IF (ier .EQ. ERROR) CALL cg_error_exit_f
                    WRITE(6,105) &
                         '    Neumann Data contains ', narrays,' data arrays'
                    DO iarray=1, narrays
                       CALL cg_array_info_f(iarray, name, datatype, &
                            nndim, dim_vals, ier)
                       IF (ier .EQ. ERROR) CALL cg_error_exit_f

                       WRITE(6,105) '    DataArray #',iarray,':'
                       WRITE(6,600)'     Name =',name
                       WRITE(6,600)'     Datatype=', &
                            DataTypeName(datatype)

                       WRITE(6,400)'    Neumann Data:'
                       IF (datatype .EQ. CGNS_ENUMV(RealSingle)) THEN
                          CALL cg_array_read_f(iarray, data_single, ier)
                          IF (ier .EQ. ERROR) CALL cg_error_exit_f
                          WRITE(6,106)&
                               (data_single(n),n=1,dim_vals(1))

                       ELSEIF (datatype .EQ. CGNS_ENUMV(RealDouble)) THEN
                          CALL cg_array_read_f(iarray, data_double, ier)
                          IF (ier .EQ. ERROR) CALL cg_error_exit_f
                          WRITE(6,106)&
                               (data_double(n),n=1,num)
                       ENDIF

                    ENDDO     ! loop through DataArray
                 ENDIF          ! if Neumann
              ENDDO          ! loop through dataset
           ENDDO          ! loop through boco
        ENDIF                  ! if ZoneBC_t exists
     ENDDO               ! zone loop

     WRITE(6,400)'                             *     *     *'

     ! *** connectivity 1to1 - Global
     WRITE(6,600)' Reading 1to1 connectivity for entire Base'
     CALL cg_n1to1_global_f(cg, base, n1to1_global, ier)
     IF (ier .EQ. ERROR) CALL cg_error_exit_f
     WRITE(6,200)'n1to1_global=',n1to1_global

     IF (n1to1_global .GT. 0) THEN
        CALL cg_1to1_read_global_f(cg, base, &
             G_connectname, G_zonename, G_donorname, &
             G_range, G_donor_range, G_transform, ier)
        IF (ier .EQ. ERROR) CALL cg_error_exit_f

        DO i=1, n1to1_global
           WRITE(6,600) ' '
           WRITE(6,130) '*** interface #',i,' ***'
           WRITE(6,600) 'G_connectname="',TRIM(G_connectname(i)),'"'
           WRITE(6,600) 'G_zonename   ="',TRIM(G_zonename(i)),'"'
           WRITE(6,600) 'G_donorname  ="',TRIM(G_donorname(i)),'"'

           WRITE(6,131) 'G_range: ', &
                '(',G_range(1,i),',',G_range(2,i),',',G_range(3,i), &
                ') to (',G_range(4,i),',',G_range(5,i),',',G_range(6,i),')'

           WRITE(6,132) 'G_donor_range: ', &
                '(', G_donor_range(1,i), ',', G_donor_range(2,i), ',', &
                G_donor_range(3,i), ') to (', &
                G_donor_range(4,i), ',', G_donor_range(5,i), ',', &
                G_donor_range(6,i), ')'

           WRITE(6,133) 'Transform: ', '(', &
                G_transform(1,i), ',', &
                G_transform(2,i), ',', G_transform(3,i), ')'
        ENDDO
     ENDIF


  ENDDO        ! loop through bases

  WRITE(6,400)'                             *     *     *'

  CALL cg_close_f(cg, ier)
  IF (ier .EQ. ERROR) CALL cg_error_exit_f

100 FORMAT(a/,'    Mass units: ',a/,'    Length units: ',a/, &
       '    Temperature units: ',a/,'    Time units: ',a/, &
       '    Angle units:',a)
101 FORMAT(A,I1,A,4(/A),/A,i4,A,/A,/A,/A,I4)
102 FORMAT(a,f5.3)
103 FORMAT(a,6i2)
104 FORMAT(a,i5,3a)
105 FORMAT(a,i2,a)
106 FORMAT(6f10.3)
107 FORMAT(i2,2a)
108 FORMAT(a,i2,a,i2,a)
109 FORMAT(a,f5.1)
110 FORMAT(a,5f5.1)
111 FORMAT(a,i1,a,i8)
112 FORMAT(a,i1/2a/3a)
113 FORMAT(i1,3a)
114 FORMAT(/a, i1)
115 FORMAT(a,i1,a/3a/2a)
116 FORMAT(a,i1,a,i1,a)
117 FORMAT(/i4,2a)
118 FORMAT(a,i1,a/3a/2a/a,i1,a,i5)
119 FORMAT(a/a,3i2/a,3i2)
120 FORMAT(a10, 3(a1,i1),a6,3(i1,a1))
121 FORMAT(a16,3(a1,i1),a6,3(i1,a1))
122 FORMAT(a12,3(a1,i2),a1)
124 FORMAT(4x, f7.2)
126 FORMAT(a/a,3f5.2/a,3f5.2)
127 FORMAT(2a,i1,a)
130 FORMAT(a15, i2, a4)
131 FORMAT(a10, 3(a1,i1),a6,3(i1,a1))
132 FORMAT(a16,3(a1,i1),a6,3(i1,a1))
133 FORMAT(a12,3(a1,i2),a1)
200 FORMAT(a,i5)
300 FORMAT(3a/a,i2)
400 FORMAT(/a/)
401 FORMAT(/2a/)
500 FORMAT(3a/3a)
600 FORMAT(3a)

9999 END PROGRAM read_cgns_1
