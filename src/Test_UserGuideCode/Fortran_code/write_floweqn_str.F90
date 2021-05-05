      program write_floweqn_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file and writes flow eqn info
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_floweqn_str.F90
!   ifort -o write_floweqn_str write_floweqn_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*4 gamma,prandtl
      integer idata(6)
      integer ieq_dim,index_zone,index_base,index_file,ier
      integer(cgsize_t) nuse
!
      write(6,'('' Program write_floweqn_str'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
!
!   data for writing
      gamma=1.4
      prandtl=0.90
!   WRITE FLOW EQUATION SET INFO
!   open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   existing file must be 3D structured (real working code would check!)
!   Create 'FlowEquationSet' node under 'Zone_t'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,'end')
!   equation dimension = 3
      ieq_dim=3
      call cg_equationset_write_f(ieq_dim,ier)
!
!   Create 'GoverningEquations' node under 'FlowEquationSet'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'end')
      call cg_governing_write_f(CGNS_ENUMV(NSTurbulent),ier)
!   Create 'DiffusionModel' node under 'GoverningEquations'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'GoverningEquations_t',1,'end')
      idata(1)=0
      idata(2)=1
      idata(3)=0
      idata(4)=0
      idata(5)=0
      idata(6)=0
      call cg_diffusion_write_f(idata,ier)
!
      nuse=1
!   Create 'GasModel' under 'FlowEquationSet'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'end')
      call cg_model_write_f('GasModel_t',CGNS_ENUMV(Ideal),ier)
!   Create 'SpecificHeatRatio' under GasModel
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'GasModel_t',1,'end')
      call cg_array_write_f('SpecificHeatRatio',CGNS_ENUMV(RealSingle),1,nuse,gamma,ier)
!   Create 'DataClass' under 'SpecificHeatRatio'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'GasModel_t',1,'DataArray_t',1,'end')
      call cg_dataclass_write_f(CGNS_ENUMV(NondimensionalParameter),ier)
!
!   Create 'TurbulenceClosure' under 'FlowEquationSet'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'end')
      call cg_model_write_f('TurbulenceClosure_t',CGNS_ENUMV(EddyViscosity),ier)
!   Create 'PrandtlTurbulent' under 'TurbulenceClosure'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'TurbulenceClosure_t',1,'end')
      call cg_array_write_f('PrandtlTurbulent',CGNS_ENUMV(RealSingle),1,nuse,prandtl,ier)
!   Create 'DataClass' under 'PrandtlTurbulent'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'TurbulenceClosure_t',1,                      &
           'DataArray_t',1,'end')
      call cg_dataclass_write_f(CGNS_ENUMV(NondimensionalParameter),ier)
!
!   Create 'TurbulenceModel' under 'FlowEquationSet'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,            &
           'FlowEquationSet_t',1,'end')
      call cg_model_write_f('TurbulenceModel_t',                               &
           CGNS_ENUMV(OneEquation_SpalartAllmaras),ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote equation set info to file'',             &
       '' grid.cgns'')')
      stop
      end
