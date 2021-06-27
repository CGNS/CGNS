	program modify_gravity
	USE CGNS
	implicit none

!       author: Diane Poirier (diane@icemcfd.com)
!       last revised on May 31 2002

!       This example test the Gravity_t data structure and its children
#include "cgnstypes_f03.h"
#ifdef WINNT
	include 'cgnswin_f.h'
#endif

	integer Ndim
	parameter (Ndim = 3)
	integer index_dim, cell_dim, phys_dim
	integer cg, base_no, ier, nbases
	real*4 gravity_vector(Ndim)
	character*100 linkpath
	character*32 basename
	real*4 version

! *** initialize
	ier = 0
	index_dim=Ndim
	cell_dim=Ndim
	phys_dim=Ndim

! *** open CGNS file for modifying
 	call cg_open_f('Test_V2', MODE_MODIFY, cg, ier)
 	if (ier .eq. ERROR) call cg_error_exit_f
	write(6,600)'MODIFYING FILE Test_V2'

! *** CGNS Library Version used for file creation:
        call cg_version_f(cg, version, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,102) &
           'Library Version used for file creation:',version

        call cg_nbases_f(cg, nbases, ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,200)'nbases=',nbases

! *** base
	base_no=1
        call cg_base_read_f(cg,base_no,basename,cell_dim,phys_dim,ier)
        if (ier .eq. ERROR) call cg_error_exit_f
        write(6,300)'BaseName = "',basename,'"', &
                      'cell_dimension=',cell_dim

! *** write a descriptor under the base
        call cg_goto_f(cg, base_no, ier, 'end')
	if (ier .eq. ERROR) call cg_error_exit_f

	call cg_descriptor_write_f('BaseDescriptorName', &
            'BaseDescriptorText', ier)
	if (ier .eq. ERROR) call cg_error_exit_f


! *** write Gravity_t node
	gravity_vector(1)=0
	gravity_vector(2)=0
	gravity_vector(3)=9.8
	call cg_gravity_write_f(cg, base_no, gravity_vector, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! *** write children of Gravity_t
	call cg_goto_f(cg, base_no, ier, 'Gravity_t', 1, 'end')
	if (ier .eq. ERROR) call cg_error_exit_f

! * Descriptor
	call cg_descriptor_write_f('GravityDescriptorName', &
             'GravityDescriptorText', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * DataClass
        call cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! * DimensionalUnits
	call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                              CGNS_ENUMV(Radian), ier)
	if (ier .eq. ERROR) call cg_error_exit_f

! * UserDefinedData
        call cg_user_data_write_f('UserData1', ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * Link to BaseDescriptorName
        write(linkpath,'(a)') '/Basename/BaseDescriptorName'
        call cg_link_write_f('LinkToBaseDescr', ' ',linkpath, ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! * GravityVector/Descriptor
	call cg_goto_f(cg, base_no, ier, 'Gravity_t', 1, 'DataArray_t', &
                       1, 'end')
        if (ier .eq. ERROR) call cg_error_exit_f
	call cg_descriptor_write_f('GravityVectorDescriptorName', &
             'GravityVectorDescriptorText', ier)
	if (ier .eq. ERROR) call cg_error_exit_f
	
! * GravityVector/DimensionalUnits
	call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
                              CGNS_ENUMV(Radian), ier)
        if (ier .eq. ERROR) call cg_error_exit_f

! *** close CGNS file
	call cg_close_f(cg, ier)
	if (ier .eq. ERROR) call cg_error_exit_f

 102    format(a,f5.3)
 200    format(a,i5)
 300    format(3a/a,i2)
 600    format(3a)
	end

