      program read_floweqn_str
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file and reads flow equation info
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (created using write_grid_str.f followed by write_floweqn_str.f)
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c read_floweqn_str.F90
!   ifort -o read_floweqn_str read_floweqn_str.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*4 prandtl,gamma
      integer idata(6)
      integer itype,itm,itc,itcm,ivm,igm,ige,id
      integer index_zone,index_base,index_file,ier
!
!   READ FLOW EQUATION SET INFO
!   open CGNS file for read-only
      call cg_open_f('grid.cgns',CG_MODE_READ,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   we know there is only one zone (real working code would check!)
      index_zone=1
!   existing file must be 3D structured (real working code would check!)
!   Read info from 'FlowEquationSet' node under 'Zone_t'
      call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,'end')
      call cg_equationset_read_f(id,ige,igm,ivm,itcm,itc,itm,ier)
      if (ier .gt. 0) then
        write(6,'('' Error!  FlowEquationSet node does not exist.'')')
        stop
      end if
      write(6,'('' Eqn dimension = '',i5)') id
!   Read 'GoverningEquations' node
      if (ige .eq. 1) then
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,          &
             'FlowEquationSet_t',1,'end')
        call cg_governing_read_f(itype,ier)
        write(6,'('' Gov eqn = '',a32)') GoverningEquationsTypeName(itype)
!   Read 'DiffusionModel' node
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,          &
             'FlowEquationSet_t',1,'GoverningEquations_t',1,'end')
        call cg_diffusion_read_f(idata,ier)
        write(6,'(''    diffusion='',6i5)') idata(1),idata(2),                 &
         idata(3),idata(4),idata(5),idata(6)
      end if
!   Read gas model
      if (igm .eq. 1) then
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,          &
             'FlowEquationSet_t',1,'end')
        call cg_model_read_f('GasModel_t',itype,ier)
        write(6,'('' Gas model type = '',a32)') ModelTypeName(itype)
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,          &
             'FlowEquationSet_t',1,'GasModel_t',1,'end')
        call cg_array_read_as_f(1,CGNS_ENUMV(RealSingle),gamma,ier)
        write(6,'(''    gamma='',f12.5)') gamma
      end if
!   Read turbulence closure
      if (itc .eq. 1) then
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,          &
          'FlowEquationSet_t',1,'end')
        call cg_model_read_f('TurbulenceClosure_t',itype,ier)
        write(6,'('' Turbulence closure type = '',a32)')                       &
          ModelTypeName(itype)
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,          &
         'FlowEquationSet_t',1,'TurbulenceClosure_t',1,'end')
        call cg_array_read_as_f(1,CGNS_ENUMV(RealSingle),prandtl,ier)
        write(6,'(''    turb prandtl number = '',f12.5)') prandtl
      end if
      if (itm .eq. 1) then
        call cg_goto_f(index_file,index_base,ier,'Zone_t',index_zone,          &
          'FlowEquationSet_t',1,'end')
        call cg_model_read_f('TurbulenceModel_t',itype,ier)
        write(6,'('' Turbulence model type = '',a32)')                         &
          ModelTypeName(itype)
      end if
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully read equation set info from file'',            &
       '' grid.cgns'')')
      stop
      end
