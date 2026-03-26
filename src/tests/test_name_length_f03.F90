      program test_name_length
!
! Regression test for an off-by-one bug in C_F_string_chars introduced in
! CGNS 4.5.0. The bug caused zone names of exactly CG_MAX_NAME_LENGTH-1 (31)
! characters to be returned with an uninitialized character at position 32
! instead of a space, making len_trim() return 32 instead of 31.
!
! This mismatches the space-padded strings returned by the legacy C wrapper
! (still used by cg_1to1_read_f and others), so any Fortran code that compares
! a zone name read via cg_zone_read_f against a donor name read via
! cg_1to1_read_f would silently fail to find a match.
!
#include "cgnstypes_f03.h"
#ifdef WINNT
      include "cgnswin_f.h"
#endif
      USE CGNS
      IMPLICIT NONE

      INTEGER, PARAMETER :: NAMELEN = 32

      ! Exactly 31 characters (CG_MAX_NAME_LENGTH - 1): the boundary that
      ! triggered the C_F_string_chars off-by-one
      CHARACTER(len=NAMELEN), PARAMETER :: &
          ZONE31 = "1234567890123456789012345678901"

      ! Another zone that holds the connectivity
      CHARACTER(len=NAMELEN), PARAMETER :: ZONE2 = "ZoneShort"

      ! Basic CGNS variables
      INTEGER :: ierr, cgfile, cgbase, cgzone, cgconn, n1to1
      INTEGER(cgsize_t) :: sizes(9), zrange(6), drange(6)
      INTEGER :: transform(3)
      CHARACTER(len=NAMELEN) :: zonename, donorname, connname

      ! Sanity-check: ensure the parameter is indeed 31 characters.
      if (len_trim(ZONE31) .ne. NAMELEN - 1) then
          print *, "Fail: ZONE31 must be 31 chars, got", &
                   len_trim(ZONE31)
          stop 1
      end if

      ! Minimal structured-zone metadata
      sizes(1) = 5; sizes(2) = 5; sizes(3) = 5
      sizes(4) = 4; sizes(5) = 4; sizes(6) = 4
      sizes(7) = 0; sizes(8) = 0; sizes(9) = 0

      zrange(1) = 1; zrange(2) = 1; zrange(3) = 1
      zrange(4) = 5; zrange(5) = 5; zrange(6) = 5

      drange(1) = 1; drange(2) = 1; drange(3) = 1
      drange(4) = 5; drange(5) = 5; drange(6) = 5

      transform(1) = 1; transform(2) = 2; transform(3) = 3

      ! ----------------------------------------------------------------
      ! Write a CGNS file with a zone name of exactly 31 characters and
      ! a 1-to-1 connectivity that references it as the donor zone.
      ! This should store the 31-character zone name correctly.
      ! ----------------------------------------------------------------
      call cg_open_f("test_name_length.cgns", CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_base_write_f(cgfile, "Base", 3, 3, cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Zone 1: 31-character name.
      call cg_zone_write_f(cgfile, cgbase, ZONE31, sizes, &
                           CGNS_ENUMV(Structured), cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Zone 2: short name with a 1-to-1 connectivity whose donor is zone 1
      ! This stores ZONE31 as the donor name via cg_1to1_write_f
      call cg_zone_write_f(cgfile, cgbase, ZONE2, sizes, &
                           CGNS_ENUMV(Structured), cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_1to1_write_f(cgfile, cgbase, cgzone, "1to1", ZONE31, &
                           zrange, drange, transform, cgconn, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! ----------------------------------------------------------------
      ! Read the file back and check
      ! ----------------------------------------------------------------
      call cg_open_f("test_name_length.cgns", CG_MODE_READ, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Read zone 1 name via cg_zone_read_f (uses C_F_string_chars)
      call cg_zone_read_f(cgfile, 1, 1, zonename, sizes, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      ! Check that the zone name has the correct length and content
      if (len_trim(zonename) .ne. NAMELEN - 1) then
          print *, "FAIL: zone length mismatch"
          print *, "  got:", len_trim(zonename)
          print *, "  expected:", NAMELEN - 1
          stop 1
      end if

      if (trim(zonename) .ne. trim(ZONE31)) then
          print *, "FAIL: zone name mismatch"
          print *, "  got:", trim(zonename)
          print *, "  expected:", trim(ZONE31)
          stop 1
      end if

      ! Read the 1-to-1 connectivity donor name via cg_1to1_read_f
      ! (still uses the C wrapper with string_2_F_string).
      call cg_n1to1_f(cgfile, 1, 2, n1to1, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (n1to1 .ne. 1) then
          print *, "FAIL: expected 1 1to1 connectivity, got", n1to1
          stop 1
      end if

      call cg_1to1_read_f(cgfile, 1, 2, 1, connname, donorname, &
                          zrange, drange, transform, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (len_trim(donorname) .ne. NAMELEN - 1) then
          print *, "FAIL: donor name length mismatch"
          print *, "  got:", len_trim(donorname)
          print *, "  expected:", NAMELEN - 1
          stop 1
      end if

      ! Final sanity check. The zonename (from cg_zone_read_f) and donorname
      ! (from cg_1to1_read_f) should be equal.
      if (zonename .ne. donorname) then
          print *, "FAIL: zone name and donor name are not equal"
          print *, "  zone name  len_trim=", len_trim(zonename)
          print *, "  donor name len_trim=", len_trim(donorname)
          stop 1
      end if

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      end program test_name_length
