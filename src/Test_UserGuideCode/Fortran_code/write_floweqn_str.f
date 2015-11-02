      program write_floweqn_str
      use cgns
c
c   Opens an existing CGNS file and writes flow eqn info
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid_str.f)
c
c   This program uses the fortran convention that all
c   variables beginning with the letters i-n are integers,
c   by default, and all others are real
c
c   Example compilation for this program is (change paths if needed!):
c   Note: when using the cgns module file, you must use the SAME fortran compiler
c   used to compile CGNS (see make.defs file)
c   ...or change, for example, via environment "setenv FC ifort"
c
c   ifort -I ../.. -c write_floweqn_str.f
c   ifort -o write_floweqn_str write_floweqn_str.o -L ../../lib -lcgns
c
c   (../../lib is the location where the compiled
c   library libcgns.a is located)
c
c   The following is no longer supported; now superceded by "use cgns":
c     include 'cgnslib_f.h'
c   Note Windows machines need to include cgnswin_f.h
c
      dimension idata(6)
      integer(cgsize_t) nuse
c
      write(6,'('' Program write_floweqn_str'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
c
c   data for writing
      gamma=1.4
      prandtl=0.90
c   WRITE FLOW EQUATION SET INFO
c   open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   we know there is only one zone (real working code would check!)
      index_zone=1
c   existing file must be 3D structured (real working code would check!)
c   Create 'FlowEquationSet' node under 'Zone_t'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +   'end')
c   equation dimension = 3
      ieq_dim=3
      call cg_equationset_write_f(ieq_dim,ier)
c
c   Create 'GoverningEquations' node under 'FlowEquationSet'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'end')
      call cg_governing_write_f(NSTurbulent,ier)
c   Create 'DiffusionModel' node under 'GoverningEquations'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'GoverningEquations_t',1,'end')
      idata(1)=0
      idata(2)=1
      idata(3)=0
      idata(4)=0
      idata(5)=0
      idata(6)=0
      call cg_diffusion_write_f(idata,ier)
c
      nuse=1
c   Create 'GasModel' under 'FlowEquationSet'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'end')
      call cg_model_write_f('GasModel_t',Ideal,ier)
c   Create 'SpecificHeatRatio' under GasModel
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'GasModel_t',1,'end')
      call cg_array_write_f('SpecificHeatRatio',RealSingle,1,nuse,
     + gamma,ier)
c   Create 'DataClass' under 'SpecificHeatRatio'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'GasModel_t',1,'DataArray_t',
     + 1,'end')
      call cg_dataclass_write_f(NondimensionalParameter,ier)
c
c   Create 'TurbulenceClosure' under 'FlowEquationSet'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'end')
      call cg_model_write_f('TurbulenceClosure_t',
     +    EddyViscosity,ier)
c   Create 'PrandtlTurbulent' under 'TurbulenceClosure'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'TurbulenceClosure_t',1,'end')
      call cg_array_write_f('PrandtlTurbulent',RealSingle,1,nuse,
     + prandtl,ier)
c   Create 'DataClass' under 'PrandtlTurbulent'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'TurbulenceClosure_t',1,
     + 'DataArray_t',1,'end')
      call cg_dataclass_write_f(NondimensionalParameter,ier)
c
c   Create 'TurbulenceModel' under 'FlowEquationSet'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     + 'FlowEquationSet_t',1,'end')
      call cg_model_write_f('TurbulenceModel_t',
     +   OneEquation_SpalartAllmaras,ier)
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote equation set info to file'',
     + '' grid.cgns'')')
      stop
      end
