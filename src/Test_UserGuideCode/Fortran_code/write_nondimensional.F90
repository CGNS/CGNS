      program write_nondimensional
      use cgns
      implicit none
#include "cgnstypes_f03.h"
!
!   Opens an existing CGNS file and adds the DataClass and
!   ReferenceState appropriate for a completely
!   NONDIMENSIONAL data set.
!
!   The CGNS grid file 'grid.cgns' must already exist
!   (for example, created using write_grid_str.f or
!   write_grid_unst.f).  In this case, the flow solution does
!   not need to be present.
!
!   Example compilation for this program is (change paths if needed!):
!   Note: when using the cgns module file, you must use the SAME fortran compiler
!   used to compile CGNS (see make.defs file)
!   ...or change, for example, via environment "setenv FC ifort"
!
!   ifort -I ../.. -c write_nondimensional.F90
!   ifort -o write_nondimensional write_nondimensional.o -L ../../lib -lcgns
!
!   (../../lib is the location where the compiled
!   library libcgns.a is located)
!
!   The following is no longer supported; now superseded by "use cgns":
!     include 'cgnslib_f.h'
!   Note Windows machines need to include cgnswin_f.h
!
      real*8 xmach,reue,xmv,xmc,rev,rel,renu,rho0,gamma,p0,c0,vm0
      real*8 xlength0,vx,vy,vz
      integer index_file,ier,index_base,idata
      integer(cgsize_t) nuse
!
      write(6,'('' Program write_nondimensional'')')
      if (CG_BUILD_64BIT) then
        write(6,'('' ...using 64-bit mode for particular integers'')')
      end if
!   define nondimensional parameters
      xmach=4.6d0
      reue=6000000.d0
      xmv=xmach
      xmc=1.d0
      rev=xmach
      rel=1.d0
      renu=xmach/reue
      rho0=1.d0
      gamma=1.4d0
      p0=1.d0/gamma
      c0=1.d0
      vm0=xmach/reue
      xlength0=1.d0
      vx=xmach
      vy=0.d0
      vz=0.d0
      nuse=1
!   WRITE NONDIMENSIONAL INFO
!   open CGNS file for modify
      call cg_open_f('grid.cgns',CG_MODE_MODIFY,index_file,ier)
      if (ier .ne. CG_OK) call cg_error_exit_f
!   we know there is only one base (real working code would check!)
      index_base=1
!   put DataClass under Base
      call cg_goto_f(index_file,index_base,ier,'end')
!   check first if a dataclass has already been written
      call cg_dataclass_read_f(idata,ier)
      if (ier .eq. 0) then
        write(6,'('' Error! DataClass already exists!'')')
        write(6,'(''   Re-make CGNS file without it and related info,'',       &
          '' then try again'')')
        stop
      else
        ier=0
      end if
      call cg_dataclass_write_f(CGNS_ENUMV(NormalizedByUnknownDimensional),ier)
!   put ReferenceState under Base
      call cg_state_write_f('ReferenceQuantities',ier)
!   Go to ReferenceState node, write Mach array and its dataclass
      call cg_goto_f(index_file,index_base,ier,'ReferenceState_t',1,'end')
      call cg_array_write_f('Mach',CGNS_ENUMV(RealDouble),1,nuse,xmach,ier)
      call cg_goto_f(index_file,index_base,ier,'ReferenceState_t',1,           &
           'DataArray_t',1,'end')
      call cg_dataclass_write_f(CGNS_ENUMV(NondimensionalParameter),ier)
!   Go to ReferenceState node, write Reynolds array and its dataclass
      call cg_goto_f(index_file,index_base,ier,'ReferenceState_t',1,'end')
      call cg_array_write_f('Reynolds',CGNS_ENUMV(RealDouble),1,nuse,reue,ier)
      call cg_goto_f(index_file,index_base,ier,'ReferenceState_t',1,           &
           'DataArray_t',2,'end')
      call cg_dataclass_write_f(CGNS_ENUMV(NondimensionalParameter),ier)
!   Go to ReferenceState node to write reference quantities:
      call cg_goto_f(index_file,index_base,ier,'ReferenceState_t',1,'end')
!   First, write reference quantities that make up Mach and Reynolds:
!   Mach_Velocity
      call cg_array_write_f('Mach_Velocity',CGNS_ENUMV(RealDouble),1,nuse,xmv,ier)
!   Mach_VelocitySound
      call cg_array_write_f('Mach_VelocitySound',CGNS_ENUMV(RealDouble),1,nuse,xmc,ier)
!   Reynolds_Velocity
      call cg_array_write_f('Reynolds_Velocity',CGNS_ENUMV(RealDouble),1,nuse,rev,ier)
!   Reynolds_Length
      call cg_array_write_f('Reynolds_Length',CGNS_ENUMV(RealDouble),1,nuse,rel,ier)
!   Reynolds_ViscosityKinematic
      call cg_array_write_f('Reynolds_ViscosityKinematic',CGNS_ENUMV(RealDouble),          &
           1,nuse,renu,ier)
!
!   Next, write flow field reference quantities:
!   Density
      call cg_array_write_f('Density',CGNS_ENUMV(RealDouble),1,nuse,rho0,ier)
!   Pressure
      call cg_array_write_f('Pressure',CGNS_ENUMV(RealDouble),1,nuse,p0,ier)
!   VelocitySound
      call cg_array_write_f('VelocitySound',CGNS_ENUMV(RealDouble),1,nuse,c0,ier)
!   ViscosityMolecular
      call cg_array_write_f('ViscosityMolecular',CGNS_ENUMV(RealDouble),1,nuse,vm0,ier)
!   LengthReference
      call cg_array_write_f('LengthReference',CGNS_ENUMV(RealDouble),1,nuse,xlength0,ier)
!   VelocityX
      call cg_array_write_f('VelocityX',CGNS_ENUMV(RealDouble),1,nuse,vx,ier)
!   VelocityY
      call cg_array_write_f('VelocityY',CGNS_ENUMV(RealDouble),1,nuse,vy,ier)
!   VelocityZ
      call cg_array_write_f('VelocityZ',CGNS_ENUMV(RealDouble),1,nuse,vz,ier)
!   close CGNS file
      call cg_close_f(index_file,ier)
      write(6,'('' Successfully wrote nondimensional info to file'',           &
       '' grid.cgns'')')
      stop
      end
