      program read_floweqn_str
      use cgns
c
c   Opens an existing CGNS file and reads flow equation info
c
c   The CGNS grid file 'grid.cgns' must already exist
c   (created using write_grid_str.f followed by write_floweqn_str.f)
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
c   ifort -I ../.. -c read_floweqn_str.f
c   ifort -o read_floweqn_str read_floweqn_str.o -L ../../lib -lcgns
c
c   (../../lib is the location where the compiled
c   library libcgns.a is located)
c
c   The following is no longer supported; now superceded by "use cgns":
c     include 'cgnslib_f.h'
c   Note Windows machines need to include cgnswin_f.h
c
      dimension idata(6)
c
c   READ FLOW EQUATION SET INFO
c   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
c   we know there is only one base (real working code would check!)
      index_base=1
c   we know there is only one zone (real working code would check!)
      index_zone=1
c   existing file must be 3D structured (real working code would check!)
c   Read info from 'FlowEquationSet' node under 'Zone_t'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +   'end')
      call cg_equationset_read_f(id,ige,igm,ivm,itcm,itc,itm,ier)
      if (ier .gt. 0) then
        write(6,'('' Error!  FlowEquationSet node does not exist.'')')
        stop
      end if
      write(6,'('' Eqn dimension = '',i5)') id
c   Read 'GoverningEquations' node
      if (ige .eq. 1) then
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +    'FlowEquationSet_t',1,'end')
        call cg_governing_read_f(itype,ier)
        write(6,'('' Gov eqn = '',a32)')
     +    GoverningEquationsTypeName(itype)
c   Read 'DiffusionModel' node
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +   'FlowEquationSet_t',1,'GoverningEquations_t',1,'end')
        call cg_diffusion_read_f(idata,ier)
        write(6,'(''    diffusion='',6i5)') idata(1),idata(2),
     +   idata(3),idata(4),idata(5),idata(6)
      end if
c   Read gas model
      if (igm .eq. 1) then
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +    'FlowEquationSet_t',1,'end')
        call cg_model_read_f('GasModel_t',itype,ier)
        write(6,'('' Gas model type = '',a32)')
     +    ModelTypeName(itype)
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +   'FlowEquationSet_t',1,'GasModel_t',1,'end')
        call cg_array_read_as_f(1,RealSingle,gamma,ier)
        write(6,'(''    gamma='',f12.5)') gamma
      end if
c   Read turbulence closure
      if (itc .eq. 1) then
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +    'FlowEquationSet_t',1,'end')
        call cg_model_read_f('TurbulenceClosure_t',itype,ier)
        write(6,'('' Turbulence closure type = '',a32)')
     +    ModelTypeName(itype)
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +   'FlowEquationSet_t',1,'TurbulenceClosure_t',1,'end')
        call cg_array_read_as_f(1,RealSingle,prandtl,ier)
        write(6,'(''    turb prandtl number = '',f12.5)') prandtl
      end if
      if (itm .eq. 1) then
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,
     +    'FlowEquationSet_t',1,'end')
        call cg_model_read_f('TurbulenceModel_t',itype,ier)
        write(6,'('' Turbulence model type = '',a32)')
     +    ModelTypeName(itype)
      end if
c   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read equation set info from file'',
     + '' grid.cgns'')')
      stop
      end
