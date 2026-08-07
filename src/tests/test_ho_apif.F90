!
! test_ho_apif.F90 -- Fortran binding coverage for the CPEX-0045 entries that
! test_high_orderf.F90 does not reach.
!
! Scope: this exercises the *wrappers*, not the C implementation.  Each entry is
! called once and the values are round-tripped, which is what catches the real
! risk in a hand-written BIND(C) layer -- an argument in the wrong position, a
! kind mismatch, a missing INT() conversion, or a symbol that does not link.
! Semantics are the C tests' job; nothing here duplicates their edge cases.
!
! Entries covered (ten, none reached elsewhere from Fortran):
!   cg_element_interpolation_distribution_write_f / _read_f
!   cg_solution_interpolation_distribution_write_f / _read_f
!   cg_solution_interpolation_find_f
!   cg_sol_ptset_write_f / _info_f / _read_f
!   cg_sol_characteristic_length_write_f / _read_f  (including the shape-only
!       query, reachable now that h_e is OPTIONAL)
!
#include "cgnstypes_f03.h"
      program test_ho_apif
      use cgns
      implicit none
      integer, parameter :: dp = kind(1.0d0)

      integer :: cgfile, cgbase, cgzone, cgfam, cgsec, cgcoord
      integer :: cgei, cgsi, cgsm, cgsol, ierr
      integer :: dist, itype, snfound, nscale
      integer :: i, j, k
      integer(cgsize_t) :: isize(3), ielem(16), nelem
      integer(cgsize_t) :: prange(2), pread(2), npnts
      integer :: ptype
      real(dp) :: x(9), y(9)
      real(dp) :: pu(4), pv(4), pw(4), pt(4)
      real(dp) :: coeff(6)
      real(dp) :: hwrite(4), hread(4)
      integer :: failures

      failures = 0

      write(*,*) 'CPEX-0045 Fortran binding coverage'

! ---- build a small unstructured zone -------------------------------------
      k = 1
      do j = 1, 3
        do i = 1, 3
          x(k) = dble(i-1)
          y(k) = dble(j-1)
          k = k + 1
        enddo
      enddo
      k = 1
      do j = 0, 1
        do i = 0, 1
          ielem(k)   = j*3 + i + 1
          ielem(k+1) = j*3 + i + 2
          ielem(k+2) = j*3 + i + 5
          ielem(k+3) = j*3 + i + 4
          k = k + 4
        enddo
      enddo

      call cg_open_f('test_ho_apif.cgns', CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_base_write_f(cgfile, 'Base', 2, 2, cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      isize(1) = 9
      isize(2) = 4
      isize(3) = 0
      call cg_zone_write_f(cgfile, cgbase, 'Zone', isize, CGNS_ENUMV(Unstructured), &
     &                     cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealDouble), &
     &                      'CoordinateX', x, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealDouble), &
     &                      'CoordinateY', y, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      nelem = 4
      call cg_section_write_f(cgfile, cgbase, cgzone, 'Elem', CGNS_ENUMV(QUAD_4), &
     &                        1_cgsize_t, nelem, 0, ielem, cgsec, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_family_write_f(cgfile, cgbase, 'Fam', cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone, 'end')
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_famname_write_f('Fam', ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

! ---- element interpolation + distribution --------------------------------
      pu(1) = -1.0_dp ; pv(1) = -1.0_dp
      pu(2) =  1.0_dp ; pv(2) = -1.0_dp
      pu(3) =  1.0_dp ; pv(3) =  1.0_dp
      pu(4) = -1.0_dp ; pv(4) =  1.0_dp
      pw = 0.0_dp
      pt = 0.0_dp

      call cg_element_interpolation_write_f(cgfile, cgbase, cgfam, &
     &                                      'QuadMesh', CGNS_ENUMV(QUAD_4), cgei, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_element_interpolation_points_write_f(cgfile, cgbase, cgfam, &
     &                                             cgei, pu, pv, pw, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_element_interpolation_distribution_write_f(cgfile, cgbase, &
     &                          cgfam, cgei, CGNS_ENUMV(Equidistant), ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

! ---- solution interpolation (nodal) + distribution -----------------------
      call cg_solution_interpolation_write_f(cgfile, cgbase, cgfam, &
     &                        'QuadSol', CGNS_ENUMV(QUAD_4), 1, 0, CGNS_ENUMV(ParametricLagrange), &
     &                        cgsi, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_solution_interpolation_points_write_f(cgfile, cgbase, cgfam, &
     &                        cgsi, pu, pv, pw, pt, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_solution_interpolation_distribution_write_f(cgfile, cgbase, &
     &                        cgfam, cgsi, CGNS_ENUMV(Equidistant), ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

! ---- solution interpolation (modal), so CharacteristicLength belongs -----
      call cg_solution_interpolation_write_f(cgfile, cgbase, cgfam, &
     &                        'QuadModal', CGNS_ENUMV(QUAD_4), 2, 0, &
     &                        CGNS_ENUMV(CartesianMonomialsPascal), cgsm, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      ! A modal SolutionInterpolation_t stores no array (CPEX-0045).

! ---- FlowSolution point set + degrees + characteristic length ------------
      prange(1) = 1
      prange(2) = 4
      npnts = 2
      call cg_sol_ptset_write_f(cgfile, cgbase, cgzone, 'FS', &
     &                          CGNS_ENUMV(InterpolationPoints), CGNS_ENUMV(PointRange), npnts, &
     &                          prange, cgsol, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_sol_interpolation_degree_write_f(cgfile, cgbase, cgzone, &
     &                                        cgsol, 2, 0, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      do i = 1, 4
        hwrite(i) = 0.25_dp*dble(i)
      enddo
      nelem = 4
      call cg_sol_characteristic_length_write_f(cgfile, cgbase, cgzone, &
     &                        cgsol, 1, nelem, hwrite, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

! ======================= read everything back ============================
      call cg_open_f('test_ho_apif.cgns', CG_MODE_READ, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      dist = -1
      call cg_element_interpolation_distribution_read_f(cgfile, cgbase, &
     &                        cgfam, 1, dist, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (dist .ne. CGNS_ENUMV(Equidistant)) then
        write(*,*) 'ERROR: element distribution round-trip: got ', dist
        failures = failures + 1
      else
        write(*,*) '  element distribution round-trip OK'
      endif

      dist = -1
      call cg_solution_interpolation_distribution_read_f(cgfile, cgbase, &
     &                        cgfam, 1, dist, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (dist .ne. CGNS_ENUMV(Equidistant)) then
        write(*,*) 'ERROR: solution distribution round-trip: got ', dist
        failures = failures + 1
      else
        write(*,*) '  solution distribution round-trip OK'
      endif

      snfound = -1
      itype = -1
      call cg_solution_interpolation_find_f(cgfile, cgbase, cgfam, CGNS_ENUMV(QUAD_4), &
     &                        1, 0, snfound, itype, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (snfound .ne. 1 .or. itype .ne. CGNS_ENUMV(ParametricLagrange)) then
        write(*,*) 'ERROR: interpolation_find: sn=', snfound, ' it=', itype
        failures = failures + 1
      else
        write(*,*) '  interpolation_find OK'
      endif

      ptype = -1
      npnts = -1
      call cg_sol_ptset_info_f(cgfile, cgbase, cgzone, cgsol, ptype, &
     &                         npnts, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (ptype .ne. CGNS_ENUMV(PointRange) .or. npnts .ne. 2) then
        write(*,*) 'ERROR: sol_ptset_info: type=', ptype, ' npnts=', npnts
        failures = failures + 1
      else
        write(*,*) '  sol_ptset_info OK'
      endif

      pread(1) = -1
      pread(2) = -1
      call cg_sol_ptset_read_f(cgfile, cgbase, cgzone, cgsol, pread, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (pread(1) .ne. 1 .or. pread(2) .ne. 4) then
        write(*,*) 'ERROR: sol_ptset_read: ', pread(1), pread(2)
        failures = failures + 1
      else
        write(*,*) '  sol_ptset_read OK'
      endif

!     shape-only query: h_e omitted, which is why it is OPTIONAL
      nscale = -1
      nelem = -1
      call cg_sol_characteristic_length_read_f(cgfile, cgbase, cgzone, &
     &                        cgsol, nscale, nelem, ier=ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nscale .ne. 1 .or. nelem .ne. 4) then
        write(*,*) 'ERROR: characteristic_length shape query: nscale=', &
     &             nscale, ' numElements=', nelem
        failures = failures + 1
      else
        write(*,*) '  characteristic_length shape-only query OK'
      endif

      hread = 0.0_dp
      call cg_sol_characteristic_length_read_f(cgfile, cgbase, cgzone, &
     &                        cgsol, nscale, nelem, hread, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      do i = 1, 4
        if (abs(hread(i) - hwrite(i)) .gt. 1.0d-12) then
          write(*,*) 'ERROR: characteristic_length value ', i, &
     &               ' expected ', hwrite(i), ' got ', hread(i)
          failures = failures + 1
        endif
      enddo
      if (failures .eq. 0) then
        write(*,*) '  characteristic_length values OK'
      endif

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (failures .ne. 0) then
        write(*,*) 'FAILED: ', failures, ' error(s)'
        stop 1
      endif
      write(*,*) 'All CPEX-0045 Fortran bindings exercised successfully'

      end program test_ho_apif
