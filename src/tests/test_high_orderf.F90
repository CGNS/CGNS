      program test_high_orderf
!
!     Test Fortran wrappers for high-order interpolation APIs
!
#include "cgnstypes_f03.h"
#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      implicit none

      integer, parameter :: dp = KIND(1.d0)
      integer, parameter :: celldim = 2, physdim = 2
      integer, parameter :: order = 2
      integer, parameter :: ncellI = 6, ncellJ = 4
      integer, parameter :: ni = ncellI*order+1, nj = ncellJ*order+1

      real(kind=dp), allocatable :: x(:), y(:)
      real(kind=dp), allocatable :: pu(:), pv(:), pw(:), puu(:), pvv(:), pww(:)
      real(kind=dp), allocatable :: pt(:), ptt(:)
      real(kind=dp), allocatable :: r(:)
      real(kind=dp), allocatable :: ecoeff(:), ecoeff_read(:)
      real(kind=dp), allocatable :: scoeff(:), scoeff_read(:)
      integer(cgsize_t), allocatable :: ielem(:)
      integer(cgsize_t) :: ecoeff_size, scoeff_size

      integer :: ierr, i, j, ii, jj, iset, ifirstnode, ielem_no
      integer :: cgfile, cgbase, cgzone, cgcoord, cgsection, cgfamily
      integer :: cgeinterp, cgsinterp, cgsol, nfield
      integer :: neinterp, nsinterp, os, ot, ncount
      integer(cgsize_t) :: size(9), nsize, nbsolpts
      integer(cgsize_t) :: nelem_start, nelem_end, nbdyelem

      character(len=32) :: fname
      character(len=33) :: einterpName, sinterpName
      integer :: etyperead, ityperead, einterptype

      fname = 'high_order_f.cgns'

      ! Allocate arrays
      allocate(x(ni*nj), y(ni*nj))
      allocate(pu((order+1)*(order+1)), pv((order+1)*(order+1)))
      allocate(pw((order+1)*(order+1)))  ! dummy array for 3rd coordinate
      allocate(ielem((order+1)*(order+1)*ncellI*ncellJ))

      ! Create 2nd order gridpoints
      iset = 0
      do j = 1, nj
        do i = 1, ni
          iset = iset + 1
          x(iset) = dble(i - 1)
          y(iset) = dble(j - 1)
        enddo
      enddo

      ! Fill (U,V) Lagrange points
      call fillQuadLagrangePoints(order, pu, pv)
      pw = 0.0_dp  ! unused for 2D

      ! Set zone sizes
      size(1) = ni*nj        ! vertex size
      size(2) = ncellI*ncellJ ! cell size
      size(3) = 0            ! boundary vertex size

      ! ========================================
      ! WRITE PHASE
      ! ========================================
      write(*,*) 'Writing cgns file ', trim(fname), ' ...'

      call cg_open_f(fname, CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_base_write_f(cgfile, 'Base', celldim, physdim, cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_zone_write_f(cgfile, cgbase, 'zone', size, &
     &                     CGNS_ENUMV(Unstructured), cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      write(*,*) 'Writing coordinates ...'
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealDouble), &
     &                      'CoordinateX', x, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealDouble), &
     &                      'CoordinateY', y, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      write(*,*) 'Writing element connectivity ...'

      ! Create QUAD_9 elements (NOT following standard SIDS ordering for test)
      ielem_no = 0
      nelem_start = 1
      do j = 0, ncellJ - 1
        do i = 0, ncellI - 1
          ifirstnode = 1 + (i*order) + (j)*order*ni
          do jj = 0, order
            do ii = 0, order
              ielem_no = ielem_no + 1
              ielem(ielem_no) = ifirstnode + ii + jj*ni
            enddo
          enddo
        enddo
      enddo
      nelem_end = ncellI*ncellJ
      nbdyelem = 0

      call cg_section_write_f(cgfile, cgbase, cgzone, 'Domain', &
     &                        CGNS_ENUMV(QUAD_9), nelem_start, nelem_end, &
     &                        nbdyelem, ielem, cgsection, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      write(*,*) 'Writing family ...'
      call cg_family_write_f(cgfile, cgbase, 'family', cgfamily, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      write(*,*) 'Writing ElementInterpolation_t node ...'
      call cg_element_interpolation_write_f(cgfile, cgbase, cgfamily, &
     &                                      'QuadInterpolation', &
     &                                      CGNS_ENUMV(QUAD_9), cgeinterp, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_element_interpolation_points_write_f(cgfile, cgbase, cgfamily, &
     &                                             cgeinterp, pu, pv, &
     &                                             pw, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Test cg_element_interpolation_coefficients_write_f
      ! Use cg_element_monomial_size_f to get correct size for QUAD_9
      write(*,*) 'Writing element interpolation coefficients ...'
      call cg_element_monomial_size_f(CGNS_ENUMV(QUAD_9), ecoeff_size, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      write(*,*) 'Element monomial size for QUAD_9: ', ecoeff_size
      allocate(ecoeff(ecoeff_size))
      do i = 1, ecoeff_size
        ecoeff(i) = dble(i) * 0.01_dp
      enddo
      call cg_element_interpolation_coefficients_write_f(cgfile, cgbase, &
     &                                                   cgfamily, cgeinterp, &
     &                                                   ecoeff, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      write(*,*) 'Element interpolation coefficients written successfully'

      write(*,*) 'Writing SolutionInterpolation_t node ...'

      ! Order 4 solution interpolation
      deallocate(pu, pv, pw)
      allocate(pu(5*5), pv(5*5), pw(5*5), pt(5*5))
      call fillQuadLagrangePoints(4, pu, pv)
      pw = 0.0_dp  ! unused for 2D
      pt = 0.0_dp  ! unused for steady

      call cg_solution_interpolation_write_f(cgfile, cgbase, cgfamily, &
     &                                       '4thOrderQuadSolution', &
     &                                       CGNS_ENUMV(QUAD_4), 4, 0, &
     &                                       CGNS_ENUMV(ParametricLagrange), &
     &                                       cgsinterp, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_solution_interpolation_points_write_f(cgfile, cgbase, cgfamily, &
     &                                              cgsinterp, pu, pv, &
     &                                              pw, pt, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Test cg_solution_interpolation_coefficients_write_f
      ! Use cg_solution_monomial_size_f to get correct size for QUAD_4, order 4, temporal 0
      write(*,*) 'Writing solution interpolation coefficients ...'
      call cg_solution_monomial_size_f(CGNS_ENUMV(QUAD_4), 4, 0, scoeff_size, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      write(*,*) 'Solution monomial size for QUAD_4, order 4: ', scoeff_size
      allocate(scoeff(scoeff_size))
      do i = 1, scoeff_size
        scoeff(i) = dble(i) * 0.001_dp
      enddo
      call cg_solution_interpolation_coefficients_write_f(cgfile, cgbase, &
     &                                                    cgfamily, cgsinterp, &
     &                                                    scoeff, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      write(*,*) 'Solution interpolation coefficients written successfully'

      ! Get Node Count
      call cg_nelement_interpolation_read_f(cgfile, cgbase, cgfamily, &
     &                                      neinterp, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_nsolution_interpolation_read_f(cgfile, cgbase, cgfamily, &
     &                                       nsinterp, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (neinterp .ne. 1 .or. nsinterp .ne. 1) then
        write(*,*) 'ERROR: wrong interpolation node count.'
        write(*,*) '       cg_nelement_interpolation_read_f = ', neinterp, &
     &             ', should be 1.'
        write(*,*) '       cg_nsolution_interpolation_read_f = ', nsinterp, &
     &             ', should be 1.'
        stop 1
      endif

      deallocate(pu, pv, pw, pt)

      ! Test cg_solution_lagrange_interpolation_size_f
      ! Get number of solution points required for a QUAD_9 with order 3
      call cg_solution_lagrange_interpolation_size_f(CGNS_ENUMV(QUAD_9), &
     &                                               3, 0, nbsolpts, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (nbsolpts .ne. 16) then
        write(*,*) 'ERROR: Wrong number of solution points: ', nbsolpts, &
     &             ', expected 16'
        stop 1
      endif
      write(*,*) 'Solution Lagrange interpolation size validated: ', nbsolpts

      ! Test cg_sol_interpolation_order_write_f with InterpolationPoints location
      write(*,*) 'Writing FlowSolution_t with InterpolationPoints location ...'
      call cg_sol_write_f(cgfile, cgbase, cgzone, 'FlowSolution', &
     &                    CGNS_ENUMV(InterpolationPoints), cgsol, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_sol_interpolation_order_write_f(cgfile, cgbase, cgzone, cgsol, &
     &                                        3, 0, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      write(*,*) 'Solution interpolation order written successfully'

      ! Add dummy solution field
      allocate(r(ncellI*ncellJ*nbsolpts))
      r = 0.0_dp
      call cg_field_write_f(cgfile, cgbase, cgzone, cgsol, &
     &                      CGNS_ENUMV(RealDouble), 'Density', r, nfield, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      deallocate(r)

      write(*,*) 'Closing cgns file ...'
      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! ========================================
      ! READ PHASE
      ! ========================================
      write(*,*) 'Reading cgns file ', trim(fname), ' in READ mode ...'
      call cg_open_f(fname, CG_MODE_READ, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      cgbase = 1
      cgzone = 1
      cgfamily = 1
      cgeinterp = 1
      cgsinterp = 1
      cgsol = 1

      ! Test cg_element_lagrange_interpolation_count_f
      write(*,*) 'Testing cg_element_lagrange_interpolation_count_f ...'
      call cg_element_lagrange_interpolation_count_f(cgfile, cgbase, cgfamily, &
     &                                               CGNS_ENUMV(QUAD_9), ncount, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (ncount .ne. 1) then
        write(*,*) 'ERROR: Wrong element interpolation count: ', ncount, &
     &             ', expected 1'
        stop 1
      endif
      write(*,*) 'Element Lagrange interpolation count: ', ncount

      ! Test cg_solution_lagrange_interpolation_count_f
      write(*,*) 'Testing cg_solution_lagrange_interpolation_count_f ...'
      call cg_solution_lagrange_interpolation_count_f(cgfile, cgbase, cgfamily, &
     &                                                CGNS_ENUMV(QUAD_4), 4, 0, &
     &                                                ncount, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (ncount .ne. 1) then
        write(*,*) 'ERROR: Wrong solution interpolation count: ', ncount, &
     &             ', expected 1'
        stop 1
      endif
      write(*,*) 'Solution Lagrange interpolation count: ', ncount

      ! Read Element interpolation Node
      write(*,*) 'Reading ElementInterpolation_t node ...'
      call cg_element_interpolation_read_f(cgfile, cgbase, cgfamily, cgeinterp, &
     &                                     einterpName, etyperead, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (trim(einterpName) .ne. 'QuadInterpolation') then
        write(*,*) 'ERROR: Wrong Element Interpolation Name: ', &
     &             trim(einterpName)
        stop 1
      endif
      write(*,*) 'Element interpolation name: ', trim(einterpName)

      if (etyperead .ne. CGNS_ENUMV(QUAD_9)) then
        write(*,*) 'ERROR: Wrong Element Interpolation Type!'
        stop 1
      endif

      ! Test cg_element_interpolation_type_read_f
      ! Note: Returns CG_NODE_NOT_FOUND if no InterpolationType_t node exists
      write(*,*) 'Testing cg_element_interpolation_type_read_f ...'
      call cg_element_interpolation_type_read_f(cgfile, cgbase, cgfamily, &
     &                                          cgeinterp, einterptype, ierr)
      if (ierr .eq. CG_NODE_NOT_FOUND) then
        write(*,*) 'No InterpolationType_t node (expected for this test)'
      else if (ierr .ne. CG_OK) then
        call cg_error_exit_f
      else
        write(*,*) 'Element interpolation type: ', einterptype
      endif

      ! Get element interpolation size
      call cg_element_lagrange_interpolation_size_f(etyperead, nsize, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (nsize .ne. 9) then
        write(*,*) 'ERROR: Wrong element interpolation size: ', nsize, &
     &             ', expected 9'
        stop 1
      endif
      write(*,*) 'Element interpolation size: ', nsize

      ! Read element interpolation points
      allocate(pu(nsize), pv(nsize), pw(nsize))
      allocate(puu(nsize), pvv(nsize), pww(nsize))
      call fillQuadLagrangePoints(2, pu, pv)
      pw = 0.0_dp

      call cg_element_interpolation_points_read_f(cgfile, cgbase, cgfamily, &
     &                                            cgeinterp, puu, pvv, &
     &                                            pww, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Check UV Points
      do i = 1, nsize
        if (abs(pu(i) - puu(i)) .gt. 1.d-6 .or. &
     &      abs(pv(i) - pvv(i)) .gt. 1.d-6) then
          write(*,*) 'ERROR: Element Interpolation points mismatch at i=', i
          write(*,*) '  expected: (', pu(i), ',', pv(i), ')'
          write(*,*) '  got:      (', puu(i), ',', pvv(i), ')'
          stop 1
        endif
      enddo
      write(*,*) 'All element interpolation points validated successfully'

      deallocate(pu, pv, pw, puu, pvv, pww)

      ! Test cg_element_interpolation_coefficients_read_f
      write(*,*) 'Testing cg_element_interpolation_coefficients_read_f ...'
      allocate(ecoeff_read(ecoeff_size))
      call cg_element_interpolation_coefficients_read_f(cgfile, cgbase, &
     &                                                  cgfamily, cgeinterp, &
     &                                                  ecoeff_read, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Validate coefficients
      do i = 1, ecoeff_size
        if (abs(ecoeff(i) - ecoeff_read(i)) .gt. 1.d-10) then
          write(*,*) 'ERROR: Element coefficient mismatch at i=', i
          write(*,*) '  expected: ', ecoeff(i)
          write(*,*) '  got:      ', ecoeff_read(i)
          stop 1
        endif
      enddo
      write(*,*) 'All element interpolation coefficients validated successfully'
      deallocate(ecoeff_read)

      ! Read Solution interpolation Node
      write(*,*) 'Reading SolutionInterpolation_t node ...'
      call cg_solution_interpolation_read_f(cgfile, cgbase, cgfamily, cgsinterp, &
     &                                      sinterpName, etyperead, os, ot, &
     &                                      ityperead, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (trim(sinterpName) .ne. '4thOrderQuadSolution') then
        write(*,*) 'ERROR: Wrong Solution Interpolation Name: ', &
     &             trim(sinterpName)
        stop 1
      endif
      write(*,*) 'Solution interpolation name: ', trim(sinterpName)

      if (etyperead .ne. CGNS_ENUMV(QUAD_4)) then
        write(*,*) 'ERROR: Wrong Solution Interpolation ElementType!'
        stop 1
      endif

      if (ityperead .ne. CGNS_ENUMV(ParametricLagrange)) then
        write(*,*) 'ERROR: Wrong Solution Interpolation Type!'
        stop 1
      endif

      if (os .ne. 4 .or. ot .ne. 0) then
        write(*,*) 'ERROR: Wrong Solution Interpolation Orders!'
        write(*,*) '  expected: os=4, ot=0'
        write(*,*) '  got:      os=', os, ', ot=', ot
        stop 1
      endif
      write(*,*) 'Solution interpolation orders: spatial=', os, ', temporal=', ot

      ! Get solution interpolation size
      call cg_solution_lagrange_interpolation_size_f(etyperead, os, ot, &
     &                                               nsize, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (nsize .ne. 25) then
        write(*,*) 'ERROR: Wrong solution interpolation size: ', nsize, &
     &             ', expected 25'
        stop 1
      endif
      write(*,*) 'Solution interpolation size: ', nsize

      ! Read solution interpolation points
      allocate(pu(nsize), pv(nsize), pw(nsize), pt(nsize))
      allocate(puu(nsize), pvv(nsize), pww(nsize), ptt(nsize))
      call fillQuadLagrangePoints(4, pu, pv)
      pw = 0.0_dp
      pt = 0.0_dp

      call cg_solution_interpolation_points_read_f(cgfile, cgbase, cgfamily, &
     &                                             cgsinterp, puu, pvv, &
     &                                             pww, ptt, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Check UV Points
      do i = 1, nsize
        if (abs(pu(i) - puu(i)) .gt. 1.d-6 .or. &
     &      abs(pv(i) - pvv(i)) .gt. 1.d-6) then
          write(*,*) 'ERROR: Solution Interpolation points mismatch at i=', i
          write(*,*) '  expected: (', pu(i), ',', pv(i), ')'
          write(*,*) '  got:      (', puu(i), ',', pvv(i), ')'
          stop 1
        endif
      enddo
      write(*,*) 'All solution interpolation points validated successfully'

      deallocate(pu, pv, pw, pt, puu, pvv, pww, ptt)

      ! Test cg_solution_interpolation_coefficients_read_f
      write(*,*) 'Testing cg_solution_interpolation_coefficients_read_f ...'
      allocate(scoeff_read(scoeff_size))
      call cg_solution_interpolation_coefficients_read_f(cgfile, cgbase, &
     &                                                   cgfamily, cgsinterp, &
     &                                                   scoeff_read, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Validate coefficients
      do i = 1, scoeff_size
        if (abs(scoeff(i) - scoeff_read(i)) .gt. 1.d-10) then
          write(*,*) 'ERROR: Solution coefficient mismatch at i=', i
          write(*,*) '  expected: ', scoeff(i)
          write(*,*) '  got:      ', scoeff_read(i)
          stop 1
        endif
      enddo
      write(*,*) 'All solution interpolation coefficients validated successfully'
      deallocate(scoeff_read)

      ! Test cg_sol_interpolation_order_read_f
      write(*,*) 'Testing cg_sol_interpolation_order_read_f ...'
      call cg_sol_interpolation_order_read_f(cgfile, cgbase, cgzone, cgsol, &
     &                                       os, ot, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (os .ne. 3 .or. ot .ne. 0) then
        write(*,*) 'ERROR: Wrong solution interpolation order!'
        write(*,*) '  expected: os=3, ot=0'
        write(*,*) '  got:      os=', os, ', ot=', ot
        stop 1
      endif
      write(*,*) 'Solution interpolation order: spatial=', os, ', temporal=', ot

      write(*,*) 'Closing cgns file ...'
      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! ========================================
      ! TEST UTILITY FUNCTIONS
      ! ========================================
      write(*,*) ''
      write(*,*) 'Testing utility functions ...'

      ! Test cg_npe_ho_f - get number of nodes for high-order element
      write(*,*) 'Testing cg_npe_ho_f ...'
      call cg_npe_ho_f(CGNS_ENUMV(QUAD_4), 2, ncount, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (ncount .ne. 9) then
        write(*,*) 'ERROR: Wrong npe_ho for QUAD_4 order 2: ', ncount, &
     &             ', expected 9'
        stop 1
      endif
      write(*,*) 'cg_npe_ho_f(QUAD_4, order=2) = ', ncount

      call cg_npe_ho_f(CGNS_ENUMV(HEXA_8), 2, ncount, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (ncount .ne. 27) then
        write(*,*) 'ERROR: Wrong npe_ho for HEXA_8 order 2: ', ncount, &
     &             ', expected 27'
        stop 1
      endif
      write(*,*) 'cg_npe_ho_f(HEXA_8, order=2) = ', ncount

      ! Test cg_element_dimension_f
      write(*,*) 'Testing cg_element_dimension_f ...'
      call cg_element_dimension_f(CGNS_ENUMV(QUAD_9), ncount, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (ncount .ne. 2) then
        write(*,*) 'ERROR: Wrong dimension for QUAD_9: ', ncount, &
     &             ', expected 2'
        stop 1
      endif
      write(*,*) 'cg_element_dimension_f(QUAD_9) = ', ncount

      call cg_element_dimension_f(CGNS_ENUMV(HEXA_27), ncount, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (ncount .ne. 3) then
        write(*,*) 'ERROR: Wrong dimension for HEXA_27: ', ncount, &
     &             ', expected 3'
        stop 1
      endif
      write(*,*) 'cg_element_dimension_f(HEXA_27) = ', ncount

      ! Test cg_element_basic_element_type_f
      write(*,*) 'Testing cg_element_basic_element_type_f ...'
      call cg_element_basic_element_type_f(CGNS_ENUMV(QUAD_9), etyperead, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (etyperead .ne. CGNS_ENUMV(QUAD_4)) then
        write(*,*) 'ERROR: Wrong basic type for QUAD_9: ', etyperead, &
     &             ', expected QUAD_4'
        stop 1
      endif
      write(*,*) 'cg_element_basic_element_type_f(QUAD_9) = QUAD_4'

      call cg_element_basic_element_type_f(CGNS_ENUMV(HEXA_125), etyperead, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (etyperead .ne. CGNS_ENUMV(HEXA_8)) then
        write(*,*) 'ERROR: Wrong basic type for HEXA_125: ', etyperead, &
     &             ', expected HEXA_8'
        stop 1
      endif
      write(*,*) 'cg_element_basic_element_type_f(HEXA_125) = HEXA_8'

      ! Test cg_element_isoparametric_write_f
      ! Need to open file in write mode to test this
      write(*,*) 'Testing cg_element_isoparametric_write_f ...'
      call cg_open_f('high_order_iso_f.cgns', CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_base_write_f(cgfile, 'Base', celldim, physdim, cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_family_write_f(cgfile, cgbase, 'IsoFamily', cgfamily, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_element_isoparametric_write_f(cgfile, cgbase, cgfamily, &
     &                                      'IsoQuad9', CGNS_ENUMV(QUAD_9), &
     &                                      cgeinterp, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      write(*,*) 'cg_element_isoparametric_write_f succeeded, index=', cgeinterp

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! ========================================
      ! CLEANUP
      ! ========================================
      deallocate(x, y, ielem)
      deallocate(ecoeff, scoeff)

      write(*,*) ''
      write(*,*) 'All Fortran high-order API tests passed!'

      contains

      subroutine fillQuadLagrangePoints(ord, u, v)
        integer, intent(in) :: ord
        real(kind=dp), intent(out) :: u(*), v(*)
        integer :: ii, jj, idx

        idx = 0
        do jj = 0, ord
          do ii = 0, ord
            idx = idx + 1
            u(idx) = -1.0_dp + dble(ii)*2.0_dp/dble(ord)
            v(idx) = -1.0_dp + dble(jj)*2.0_dp/dble(ord)
          enddo
        enddo
      end subroutine fillQuadLagrangePoints

      end program test_high_orderf
