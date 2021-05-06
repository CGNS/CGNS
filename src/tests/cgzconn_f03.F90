      program cgzconn

#include "cgnstypes_f03.h"
#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      IMPLICIT NONE

      INTEGER, PARAMETER :: sp = KIND(1.0)

      INTEGER ierr, cgfile, cgbase, cgzone, cgcoord, cgconn
      INTEGER dim
      INTEGER nzconn, nconn
      INTEGER n1to1, cgz1, cgz2
      INTEGER(cgsize_t) i, j, k, n, size(9)
      INTEGER transform(3)
      INTEGER(cgsize_t) ptlist(125)
      INTEGER(cgsize_t) ptrange(6)
      INTEGER(cgsize_t) npts
      INTEGER(cgenum_t) loc, type,  ptype, dztype, dptype, ddtype
      INTEGER(cgsize_t) dnpts
      real(kind=sp) x(125), y(125), z(125), exp(5)
      character(len=32) zname, dname, cname

! open and write base
      dim = 3
      call cg_open_f('cgzconn.cgns', CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_base_write_f(cgfile, 'Base', dim, dim, cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

! create some bogus data
      do n=1,3
        size(n)   = 5
        size(n+3) = 4
        size(n+6) = 0
      enddo
      n = 0
      do k=1,5
        do j=1,5
          do i=1,5
            n = n + 1
            x(n) = i
            y(n) = j
            z(n) = k
          enddo
        enddo
      enddo
      do n=1,3
        ptrange(n) = 1
        ptrange(n+3) = 5
        transform(n) = n
      enddo
      ptrange(4) = 1
      n = 0
      do j=1,5
        do i=1,5
          n = n + 1
          ptlist(n) = i
          n = n + 1
          ptlist(n) = j
          n = n + 1
          ptlist(n) = 1
        enddo
      enddo
      do n=1,5
        exp(n) = 0
      enddo
      exp(2) = 1
! loop over zones
      do n=1,2
        if (n .eq. 1) then
          zname = 'Zone1'
          dname = 'Zone2'
        else
          zname = 'Zone2'
          dname = 'Zone1'
        endif
! write zone
        call cg_zone_write_f(cgfile, cgbase, zname, size,               &
     &                       CGNS_ENUMV(Structured), cgzone, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_goto_f(cgfile, cgbase, ierr, 'Zone_t', cgzone, 'end')
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_dataclass_write_f(CGNS_ENUMV(NormalizedByDimensional), ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
! write coordinates
        call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),       &
     &                        'CoordinateX', x, cgcoord, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_gopath_f(cgfile, 'GridCoordinates/CoordinateX', ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_exponents_write_f(CGNS_ENUMV(RealSingle), exp, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f

        call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),       &
     &                        'CoordinateY', y, cgcoord, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_gopath_f(cgfile, '../CoordinateY', ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_exponents_write_f(CGNS_ENUMV(RealSingle), exp, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f

        call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),       &
     &                        'CoordinateZ', z, cgcoord, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_gopath_f(cgfile, '../CoordinateZ', ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_exponents_write_f(CGNS_ENUMV(RealSingle), exp, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
! write first ZoneGridConnectivity - will be active
        call cg_zconn_write_f(cgfile, cgbase, cgzone,                   &
     &                        'ZoneConnectivity1', cgz1, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f

! write second ZoneGridConnectivity - will be active
        call cg_zconn_write_f(cgfile, cgbase, cgzone,                   &
     &                        'ZoneConnectivity2', cgz2, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
! write general connectivity
        npts = 2
        dnpts = 25
        call cg_conn_write_f(cgfile, cgbase, cgzone, 'conn', CGNS_ENUMV(Vertex),    &
     &                       CGNS_ENUMV(Abutting1to1), CGNS_ENUMV(PointRange), npts, ptrange,   &
     &                       dname, CGNS_ENUMV(Structured), CGNS_ENUMV(PointListDonor),         &
     &                       CGNS_ENUMV(Integer), dnpts, ptlist, cgconn, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
! set it back to previous ZoneGridConnectivity and write 1to1
        call cg_zconn_set_f(cgfile, cgbase, cgzone, cgz1, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f

        call cg_1to1_write_f(cgfile, cgbase, cgzone, '1to1', dname,     &
     &                       ptrange, ptrange, transform, cgconn, ierr)

        if (ierr .ne. CG_OK) call cg_error_exit_f
      enddo

! close the file and reopen in read mode
      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_open_f('cgzconn.cgns', CG_MODE_READ, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      cgbase = 1
      cgz1 = 1
      cgz2 = 2
      cgconn = 1
      do cgzone=1,2
        call cg_nzconns_f(cgfile, cgbase, cgzone, nzconn, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        if (nzconn .ne. 2) then
          print *,'nzconn != 2'
          stop
        endif
! read should make ZoneGridConnectivity active
        call cg_zconn_read_f(cgfile, cgbase, cgzone, cgz2, zname, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        if (zname .ne. 'ZoneConnectivity2') then
          print *,'expecting Zoneconnectivity2 - got',zname
          stop
        endif
        call cg_nconns_f(cgfile, cgbase, cgzone, nconn, ierr);
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_n1to1_f(cgfile, cgbase, cgzone, n1to1, ierr);
        if (ierr .ne. CG_OK) call cg_error_exit_f
        if (nconn .ne. 1 .or. n1to1 .ne. 0) then
          print *,'expecting nconn=1,n1to1=0 - got',nconn,n1to1
          stop
        endif
        call cg_conn_info_f(cgfile, cgbase, cgzone, cgconn, cname,      &
     &                      loc, type, ptype, npts, dname, dztype,      &
     &                      dptype, ddtype, dnpts, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        IF (cname .NE. 'conn' .OR. loc .NE. CGNS_ENUMV(Vertex) .OR.     &
     &      TYPE .NE. CGNS_ENUMV(Abutting1to1) .OR.                     &
     &      ptype .NE. CGNS_ENUMV(PointRange) .OR.                      &
     &      npts .NE. 2 .OR. dztype .NE. CGNS_ENUMV(Structured) .OR.    &
     &      dptype .NE. CGNS_ENUMV(PointListDonor) .OR.                 &
     &      dnpts .NE. 25) THEN
          print *,'invalid conn data'
          stop
        endif
! read should make ZoneGridConnectivity active
        call cg_zconn_read_f(cgfile, cgbase, cgzone, cgz1, zname, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        if (zname .ne. 'ZoneConnectivity1') then
          print *,'expecting Zoneconnectivity1 - got',zname
          stop
        endif
        call cg_nconns_f(cgfile, cgbase, cgzone, nconn, ierr);
        if (ierr .ne. CG_OK) call cg_error_exit_f
        call cg_n1to1_f(cgfile, cgbase, cgzone, n1to1, ierr);
        if (ierr .ne. CG_OK) call cg_error_exit_f
        if (nconn .ne. 0 .or. n1to1 .ne. 1) then
          print *,'expecting nconn=0,n1to1=1 - got',nconn,n1to1
          stop
        endif
        call cg_1to1_read_f(cgfile, cgbase, cgzone, cgconn, cname,      &
     &                      dname, ptrange, ptrange, transform, ierr)

        if (ierr .ne. CG_OK) call cg_error_exit_f
        if (cname .ne. '1to1') then
          print *,'invalid 1to1 data'
          stop
        endif
      enddo

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      end
