
	program read_chemistry
	USE CGNS
	implicit none

! Demonstrate Version 2.1 chemistry extensions.
! D. Leich, Intelligent Light 11-Jan-02
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim, Nglobal
	parameter (Ndim = 3)
	parameter (Nglobal = 500)
	integer CellDim, PhysDim
	integer ier, n
	integer nbases
	integer type
        integer datatype
	character*32 basename
	integer cg, base
	character*32 name, filename
	character*40 text
	integer equation_dimension, GoverningEquationsFlag
	integer GasModelFlag, ViscosityModelFlag
	integer ThermalConductivityModelFlag
	integer TurbulenceClosureFlag, TurbulenceModelFlag
	integer ThermalRelaxationFlag, ChemicalKineticsFlag
	integer diffusion_model(6)
	integer nndim
        integer(cgsize_t) dim_vals(12)
	integer mass, length, time, temp, deg, iarray, i,narrays
!234567890!234567890!234567890!234567890!234567890!234567890!23456789012
	real*4 data_single(100000)
	double precision data_double(100000)
	real*4 version
        real*4 exponents(5)

! *** open file
!	write(6,*) 'Input filename'
!	read(5,600) filename
	write(filename,'(a)')'Test_V2'
	call cg_open_f(filename, MODE_READ, cg, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,600)'READING FILE ',filename

! *** CGNS Library Version used for file creation (version must be real*4):
	call cg_version_f(cg, version, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,102) &
          'Library Version used for file creation:',version

! *** base
	call cg_nbases_f(cg, nbases, ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,200)'nbases=',nbases

	do base=1, nbases
	  call cg_base_read_f(cg, base, basename, CellDim, PhysDim, ier)
	  if (ier .eq. ERROR) call cg_error_exit_f
	  write(6,300)'BaseName = "',basename,'"', &
                     'cell_dimension=',CellDim

! *** base attribute:  GOTO base node
	  call cg_goto_f(cg, base, ier, 'end')
          if (ier .eq. ERROR) call cg_error_exit_f

! ***     base attribute:  Descriptor
          call cg_descriptor_read_f(1, name, text, ier)
          if (ier .eq. ERROR) call cg_error_exit_f
          if (ier.eq.ALL_OK) then
            write(6,400)'Base Descriptor_t Information:'
            write(6,500)' DescriptorName="',name,'"',&
                       ' DescriptorText="',text,'"'
          endif

! ***     base attribute: flow equation set:
	  call cg_equationset_read_f(equation_dimension, &
           GoverningEquationsFlag,  GasModelFlag, &
           ViscosityModelFlag, ThermalConductivityModelFlag,&
           TurbulenceClosureFlag,  TurbulenceModelFlag, ier)
	  if (ier .eq. ERROR) then
	    call cg_error_exit_f
	  elseif (ier .eq. NODE_NOT_FOUND) then
	    write(6,200) &
           'FlowEquationSet_t not defined under CGNSBase_t #',base
	  elseif (ier .eq. INCORRECT_PATH) then
  	    write(6,400)'Incorrect path input to cg_goto_f'
	  else
	    write(6,400) 'FlowEquationSet_t Information:'
	    write(6,200)' equation_dimension=',equation_dimension

! *** chemistry attributes: flow equation set:
	    call cg_equationset_chemistry_read_f(ThermalRelaxationFlag,&
             ChemicalKineticsFlag, ier)
	    if (ier .eq. ERROR) then
	      call cg_error_exit_f
	    elseif (ier .eq. NODE_NOT_FOUND) then
	      write(6,200)&
              'FlowEquationSet_t not defined under CGNSBase_t #',base
	    elseif (ier .eq. INCORRECT_PATH) then
  	      write(6,400)'Incorrect path input to cg_goto_f'
	    endif

! ***       flow equation set attributes:  GOTO FlowEquationSet_t node
            call cg_goto_f(cg, base, ier, 'FlowEquationSet_t', 1, 'end')
            if (ier .eq. ERROR) call cg_error_exit_f

! ***       flow equation set attribute: Descriptor
            call cg_descriptor_read_f(1, name,text,ier)
            if (ier .eq. ERROR) call cg_error_exit_f
            if (ier .eq. ALL_OK) write(6,500)&
              ' DescriptorName="',name,'"',' DescriptorText="',text,'"'

! ***       flow equation set attribute: Gas Model Type
	    if (GasModelFlag.eq.1) then
                call cg_model_read_f('GasModel_t', type, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                if (ier .eq. ALL_OK) write(6,600)&
                  ' GasModelType="',ModelTypeName(type),'"'
            endif

! ***       flow equation set attribute: ViscosityModel Type
            if (ViscosityModelFlag.eq.1) then
              call cg_model_read_f('ViscosityModel_t', type, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              if (ier .eq. ALL_OK) write(6,600)&
                ' ViscosityModelType="',ModelTypeName(type),'"'
            endif

! ***       flow equation set attribute:  TypmlConductivityModel Type
            if (ThermalConductivityModelFlag.eq.1) then
              call cg_model_read_f('ThermalConductivityModel_t',&
                        type, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              if (ier .eq. ALL_OK) write(6,600)&
                ' ThermalConductivityModelType=',&
                  ModelTypeName(type),'"'
            endif

! ***   flow equation set attribute: TurbulenceClosureType
            if (TurbulenceClosureFlag.eq.1) then
              call cg_model_read_f('TurbulenceClosure_t', type, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              if (ier .eq. ALL_OK) write(6,600)&
                ' TurbulenceClosureType="', ModelTypeName(type),'"'
            endif

! ***   flow equation set attribute: TurbulenceModelType
            if (TurbulenceModelFlag.eq.1) then
              call cg_model_read_f('TurbulenceModel_t', type, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              if (ier .eq. ALL_OK) write(6,600)&
                ' TurbulenceModelType="',ModelTypeName(type),'"'
            endif

! ***   flow equation set attribute: Governing Equations Type
            if (GoverningEquationsFlag .eq. 1) then
              call cg_governing_read_f(type, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              if (ier.eq.ALL_OK)&
                write(6,600)' GoverningEquationsType="',&
                                GoverningEquationsTypeName(type),'"'

    	! ! *** Governing Equations attribute:  GOTO GoverningEquations_t node
	      call cg_goto_f(cg,base,ier, 'FlowEquationSet_t', 1,&
                  'GoverningEquations_t',1,'end')
	      if (ier .eq. ERROR) call cg_error_exit_f

        ! ! *** Governing Equations attribute:  Diffusion model
	      call cg_diffusion_read_f(diffusion_model, ier)
	      if (ier .eq. ERROR) call cg_error_exit_f
	      if (ier.eq.ALL_OK)write(6,103)'   Diffusion model=',&
                                 (diffusion_model(i), i=1,6)

! ***         flow equation set attributes:  GOTO FlowEquationSet_t node
              call cg_goto_f(cg, base, ier, 'FlowEquationSet_t', 1, &
      	        'end')
              if (ier .eq. ERROR) call cg_error_exit_f

            endif       ! If Governing Equations are defined
	  endif         ! If FlowEquationSet_t exists under CGNSBase_t

! ***   flow equation set attribute: ThermalRelaxationModel
            if (ThermalRelaxationFlag.eq.1) then
              call cg_model_read_f('ThermalRelaxationModel_t', &
      	        type, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              if (ier .eq. ALL_OK) write(6,600)&
                ' ThermalRelaxationModel="',ModelTypeName(type),'"'
            endif

! ***   flow equation set attribute: ChemicalKineticsModel
            if (ChemicalKineticsFlag.eq.1) then
              call cg_model_read_f('ChemicalKineticsModel_t', &
      	        type, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              if (ier .eq. ALL_OK) write(6,600)&
                ' ChemicalKineticsModel="',ModelTypeName(type),'"'

    	! *** GOTO ChemicalKineticsModel_t node
	      call cg_goto_f(cg,base,ier, 'FlowEquationSet_t', 1,&
                  'ChemicalKineticsModel_t',1,'end')
	      if (ier .eq. ERROR) call cg_error_exit_f

           ! ** ChemicalKineticsModel_t attributes: Descriptor
              call cg_descriptor_read_f(1, name, text, ier)
              if (ier .eq. ERROR) call cg_error_exit_f
              if (ier.eq.ALL_OK) then
                write(6,400)'Descriptor_t Information:'
                write(6,500)' DescriptorName="',name,'"',&
                            ' DescriptorText="',text,'"'
              endif

              write(6,400)'                             *     *     *'

         ! ** ChemicalKineticsModel_t attributes: DataArray_t
	      call cg_narrays_f(narrays, ier)
	      if (ier .eq. ERROR) call cg_error_exit_f
	      write(6,105) 'ChemicalKineticsModel_t contains ',&
                            narrays,' array(s)'
	      do iarray=1, narrays

                call cg_array_info_f(iarray, name, datatype,&
                                   nndim, dim_vals, ier)
                if (ier .eq. ERROR) call cg_error_exit_f

	        write(6,600) ' DataArrayName="',name,'"'
	        write(6,600) ' DataType="',DataTypeName(datatype),'"'
	        write(6,200) ' DataNdim=',nndim
	        write(6,200) ' DataDim=',dim_vals(1)

	        write(6,105) ' Data:'
	        if (datatype .eq. CGNS_ENUMV(RealSingle)) then
	          call cg_array_read_f(iarray, data_single, ier)
                  if (ier .eq. ERROR) call cg_error_exit_f
	          write(6,106) (data_single(n),n=1,dim_vals(1))
	        elseif (datatype .eq. CGNS_ENUMV(RealDouble)) then
		  call cg_array_read_f(iarray, data_double, ier)
                  if (ier .eq. ERROR) call cg_error_exit_f
	 	  write(6,106) (data_double(n),n=1,dim_vals(1))
	        endif

    	  ! *** GOTO DataArray_t node
	        call cg_goto_f(cg,base,ier, 'FlowEquationSet_t', 1,&
                    'ChemicalKineticsModel_t',1,&
                    'DataArray_t',iarray,'end')
	        if (ier .eq. ERROR) call cg_error_exit_f

           ! ** DataArray_t attributes: DataClass_t
	        call cg_dataclass_read_f(type,ier)
	        if (ier .eq. ERROR) call cg_error_exit_f
	        write(6,600)'DataClassName=',DataClassName(type)

           ! ** DataArray_t attributes: DimensionalUnits_t
	        call cg_units_read_f(mass, length, time, temp, deg, ier)
	        if (ier .eq. ERROR) call cg_error_exit_f
	        if (ier .eq. ALL_OK) then
	          write(6,100)&
      	 	    'Dimensional Units:',&
                    MassUnitsName(mass), LengthUnitsName(length),&
                    TimeUnitsName(time), TemperatureUnitsName(temp), &
                    AngleUnitsName(deg)
	        endif

           ! ** DataArray_t attributes: DimensionalExponents_t
                call cg_exponents_read_f(exponents, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                if (ier .eq. ALL_OK) then
                  write(6,99)&
                    'Dimensional Exponents:',&
                    exponents(1), exponents(2), exponents(3),&
                    exponents(4), exponents(5)
                endif

           ! ** DataArray_t attributes: Descriptor
                call cg_descriptor_read_f(1, name, text, ier)
                if (ier .eq. ERROR) call cg_error_exit_f
                if (ier.eq.ALL_OK) then
                  write(6,400)'Descriptor_t Information:'
                  write(6,500)' DescriptorName="',name,'"',&
                              ' DescriptorText="',text,'"'
                endif

    	  ! *** GOTO ChemicalKineticsModel_t node
	        call cg_goto_f(cg,base,ier, 'FlowEquationSet_t', 1,&
                    'ChemicalKineticsModel_t',1,'end')
	        if (ier .eq. ERROR) call cg_error_exit_f

              write(6,400)'                             *     *     *'
	      enddo
	    endif
 	enddo    				! loop through bases

        write(6,400)'                             *     *     *'

        call cg_close_f(cg, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

  99    format(a/,'    Mass exponent: ',f5.1/,'    Length exponent: ',&
          f5.1/,'    Time exponent: ',f5.1/,&
          '    Temperature exponent: ',f5.1/,'    Angle exponent:',f5.1)
 100 	format(a/,'    Mass units: ',a/,'    Length units: ',a/,&
          '    Time units: ',a/,'    Temperature units: ',a/,&
          '    Angle units:',a)
 101	format(a,i1,a,/2a,/2a,/2a,/3a,/a,i4,3a,/2a,/2a,/2a,/a,i4)
 102 	format(a,f5.3)
 103	format(a,6i2)
 104	format(a,i5,3a)
 105	format(a,i2,a)
 106    format(6f10.3)
 107	format(i2,2a)
 108    format(a,i2,a,i2,a)
 109	format(a,f5.1)
 110	format(a,5f5.1)
 111	format(a,i1,a,i8)
 112	format(a,i1/2a/3a)
 113	format(i1,3a)
 114	format(/a, i1)
 115	format(a,i1,a/3a/2a)
 116	format(a,i1,a,i1,a)
 117	format(/i4,2a)
 118	format(a,i1,a/3a/2a/a,i1,a,i5)
 119	format(a/a,3i2/a,3i2)
 120	format(a10, 3(a1,i1),a6,3(i1,a1))
 121 	format(a16,3(a1,i1),a6,3(i1,a1))
 122	format(a12,3(a1,i2),a1)
 124	format(4x, f7.2)
 126	format(a/a,3f5.2/a,3f5.2)
 127	format(2a,i1,a)
 130	format(a15, i2, a4)
 131	format(a10, 3(a1,i1),a6,3(i1,a1))
 132	format(a16,3(a1,i1),a6,3(i1,a1))
 133	format(a12,3(a1,i2),a1)
 200    format(a,i5)
 300	format(3a/a,i2)
 400	format(/a/)
 401	format(/2a/)
 500	format(3a/3a)
 600	format(3a)

 9999	end
