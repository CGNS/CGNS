      program testfamilytreef

#include "cgnstypes_f03.h"
#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      USE CGNS
      implicit none

      INTEGER, PARAMETER :: sp = KIND(1.0)
      integer, parameter :: NUM_SIDE = 5

      integer(cgsize_t) :: sizes(9)
      integer(cgsize_t) :: ptrange(6)
      integer(cgsize_t) :: npts
      integer :: celldim, physdim

      real(kind=sp), dimension(NUM_SIDE*NUM_SIDE*NUM_SIDE) :: xcoord
      real(kind=sp), dimension(NUM_SIDE*NUM_SIDE*NUM_SIDE) :: ycoord
      real(kind=sp), dimension(NUM_SIDE*NUM_SIDE*NUM_SIDE) :: zcoord

      integer :: ierr
      integer :: i, j, k, n, nfam, nb, ng, nnames
      integer :: cgfile, cgbase, cgtree, cgzone, cgfam, cgcoord
      integer :: cgbc, cgsr

      real(kind=sp), dimension(5) :: exponents
      character(len=32) outfile
      character(len=32) name
      character(len=32) tname
      character(len=20*33) :: family_name
      character(len=20*33) :: tfamily_name

! ----  WRITING TESTS  ----

      outfile = "family_tree_f90.cgns"
      !call unlink( outfile )
      ! write(*, *) 'Create file'
      call cg_open_f(outfile, CG_MODE_WRITE, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      ! write(*, *) 'Create Grid Base'
      CALL cg_base_write_f( cgfile, 'Structured', 3, 3, cgbase, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      CALL cg_gopath_f( cgfile, '/Structured', ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f     
      call cg_dataclass_write_f( CGNS_ENUMV(Dimensional), ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f     
      call cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), &
     &                      CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), &
     &                      CGNS_ENUMV(Radian), ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f     

! -- Create Family Tree Base
      ! write(*, *) 'Create Family Tree Base'
      call cg_base_write_f( cgfile, "FamilyTree", 3, 3, cgtree, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f     

! --  SOME GRID DATA
      do n=1,3
        sizes(n)   = NUM_SIDE;
        sizes(n+3) = NUM_SIDE - 1;
        sizes(n+6) = 0;
      enddo
      n = 1
      do k = 1, NUM_SIDE
        do j = 1, NUM_SIDE
          do i = 1, NUM_SIDE
            xcoord(n) = i
            ycoord(n) = j
            zcoord(n) = k
            n = n + 1
          enddo
        enddo
      enddo

      do n = 1,5
        exponents(n) = 0.0
      enddo
      exponents(2) = 1.0
      call cg_zone_write_f(cgfile, cgbase, "Zone", sizes, &
     &                    CGNS_ENUMV(Structured), cgzone, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle), &
     &       "CoordinateX", xcoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle), &
     &       "CoordinateY", ycoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_coord_write_f(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle), &
     &       "CoordinateZ", zcoord, cgcoord, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_goto_f(cgfile, cgbase, ierr, "Zone_t", cgzone, &
     &       "GridCoordinates_t", 1, &
     &       "CoordinateX", 0, 'end')
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile,                                  &
     &    "/Structured/Zone/GridCoordinates/CoordinateX", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_exponents_write_f(CGNS_ENUMV(RealSingle), exponents, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile, "../CoordinateY", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_exponents_write_f(CGNS_ENUMV(RealSingle), exponents, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile, "../CoordinateZ", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_exponents_write_f(CGNS_ENUMV(RealSingle), exponents, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

! -- Write Family Tree
!    
! FamilyTree
!   +- Family1
!   |  +- Family1.1
!   |  |  +- Family1.1.1
!   |  +- Family1.2
!   |     +- Family1.2.1
!   |        +- Family1.2.1.1      will be deleted (direct deletion)
!   |        +- Family1.2.1.2      will be overwritten
!   +- Family2
!   |  +- Family2.1
!   |     +- Family2.1.1
!   +- Family3
!   +- Family4
!      +- Family4.1
!      +- Family4.2                will be deleted (and sub childs as well)
!      |  +- Family4.2.1
!      |  |  +- Family4.2.1.1
!      |  +- Family4.2.2
!      +- Family4.3
!

! PATH BASED FAMILY NODE CREATION
      call cg_family_write_f(cgfile, cgtree, "Family1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_family_write_f(cgfile, cgtree, &
     &                 "/FamilyTree/Family1/Family1.1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_family_write_f(cgfile, cgtree, &
     &     "/FamilyTree/Family1/Family1.1/Family1.1.1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_family_write_f(cgfile, cgtree, &
     &     "/FamilyTree/Family1/Family1.2/Family1.2.1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_family_write_f(cgfile, cgtree, &
     &                           "/FamilyTree/Family2", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_family_write_f(cgfile, cgtree, "Family3", cgfam, ierr) 
      if (ierr .ne. CG_OK) call cg_error_exit_f

! NODE BASED FAMILY NODE CREATION

      call cg_gopath_f( cgfile, "/FamilyTree/Family2", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_node_family_write_f( "Family2.1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_goto_f( cgfile, cgtree, ierr, "Family2", 0, &
     &               "Family2.1", 0, 'end')
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile, &
     &        "/FamilyTree/Family1/Family1.2/Family1.2.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_node_family_write_f( "Family1.2.1.1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_node_family_write_f( "Family1.2.1.2", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_node_nfamilies_f( nfam , ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      
      call cg_gopath_f( cgfile, "/FamilyTree/Family2/Family2.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_node_family_write_f( "Family2.1.1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f      


      call  cg_family_write_f(cgfile, cgtree, "Family4", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_family_write_f(cgfile, cgtree, &
     &                  "/FamilyTree/Family4/Family4.1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_family_write_f(cgfile, cgtree, &
     & "/FamilyTree/Family4/Family4.2/Family4.2.1/Family4.2.1.1", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_family_write_f(cgfile, cgtree, &
     &      "/FamilyTree/Family4/Family4.2/Family4.2.2", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_family_write_f(cgfile, cgtree, &
     &                  "/FamilyTree/Family4/Family4.3", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      CALL cg_goto_f(cgfile, cgtree, ierr, 'end')
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile, "/FamilyTree", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_node_family_write_f( "FamilyN", cgfam, ierr) 
      if (ierr .ne. CG_OK) call cg_error_exit_f

!  FAMILY (TREE) NAME CREATION

      call cg_goto_f(cgfile, cgbase, ierr, "Zone", 0, 'end')
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile, "/Structured/Zone", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_famname_write_f("/FamilyTree/Family1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      do j=1,3
         write( name,'(a,i1)') 'ZoneFamily', j
         write( family_name,'(a,i1)') &
     &      '/FamilyTree/Family1/Family1.2/Family1.2.1/Family1.2.1.', j
         call cg_multifam_write_f(name, family_name, ierr)
         if (ierr .ne. CG_OK) call cg_error_exit_f
      enddo
      do n=1,3
        ptrange(n) = 1
        ptrange(n+3) = NUM_SIDE
      enddo
      ptrange(6) = 1
      npts = 2
      call cg_boco_write_f(cgfile, cgbase, 1, "Inflow", CGNS_ENUMV(BCInflow), &
     &            CGNS_ENUMV(PointRange), npts, ptrange, cgbc, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_goto_f(cgfile, cgbase, ierr, "Zone", 0, "ZoneBC", 0, &
     &               "Inflow", 0,'end')
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile, "/Structured/Zone/ZoneBC/Inflow", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_famname_write_f("/FamilyTree/Family2", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      do j = 1, 3
         write( name,'(a,i1)') 'BCFamily', j
         write( family_name,'(a,i1)') '/FamilyTree/Family', j
         call cg_multifam_write_f(name, family_name, ierr)
         if (ierr .ne. CG_OK) call cg_error_exit_f
      enddo

      call cg_subreg_bcname_write_f(cgfile, cgbase, cgzone, "SubRegion", &
     &       2, "Inflow", cgsr, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_goto_f(cgfile, cgbase, ierr, "Zone", 0, "SubRegion", 0, 'end')
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile, "/Structured/Zone/SubRegion", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_famname_write_f("/FamilyTree/Family3", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f     
      do j = 1,3
         write( name,'(a,i1)') 'SubRegionFamily', j
         write( family_name,'(a,i1)') '/FamilyTree/Family', j
         call cg_multifam_write_f(name, family_name, ierr)
         if (ierr .ne. CG_OK) call cg_error_exit_f
      enddo

      call cg_gopath_f(cgfile, "/Structured/Zone", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_user_data_write_f("UserData", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_user_data_write_f("UserData2", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_gopath_f(cgfile, "./UserData", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_famname_write_f("/FamilyTree/Family4", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      do j = 1,3
         write( name,'(a,i1)') 'UserDataFamily', j
         write( family_name,'(a,i1)') '/FamilyTree/Family', j
         call cg_multifam_write_f(name, family_name, ierr)
         if (ierr .ne. CG_OK) call cg_error_exit_f
      enddo
!    FAMILY NAMES IN TREE
      call cg_gopath_f( cgfile, "/FamilyTree/Family1/Family1.1", ierr)

      call cg_node_family_name_write_f( "FamilyN1.1-1", &
     &                          "/FamilyTree/Family2/Family2.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_node_family_name_write_f( "FamilyN1.1-2", &
     &                          "/FamilyTree/Family2/Family2.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_node_family_name_write_f( "FamilyN1.1-3", &
     &                          "/FamilyTree/Family2/Family2.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_node_nfamily_names_f( nnames, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (nnames .ne. 3) stop

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

! ---- MODIFYING TESTS ----
      call cg_open_f(outfile, CG_MODE_MODIFY, cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_nbases_f( cgfile, nb, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nb .ne. 2) stop

      cgtree = -1
      cgbase = -1

      do n=1,nb
        call cg_base_read_f( cgfile, n, name, celldim, physdim, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        if( name .eq. "FamilyTree" ) then
            cgtree = n
        else
            cgbase = n
        endif
      enddo

      call cg_nfamilies_f(cgfile, cgtree, nfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nfam .ne. 5) stop

!  FAMILY NODE DELETION

      call cg_gopath_f(cgfile,"/FamilyTree/Family1/Family1.2/Family1.2.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_gopath_f(cgfile, "/FamilyTree/Family1/Family1.2/Family1.2.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_delete_node_f("Family1.2.1.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_node_nfamilies_f(nfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nfam .ne. 1) stop

      call cg_gopath_f(cgfile, "/FamilyTree/Family4", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_delete_node_f("Family4.2", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_node_nfamilies_f(nfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nfam .ne. 2) stop

!  FAMILY NODE OVERWRITING
      call cg_gopath_f( cgfile, "/FamilyTree/Family1/Family1.2/Family1.2.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_node_family_write_f( "Family1.2.1.2", cgfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f 
      call cg_node_nfamilies_f(nfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nfam .ne. 1) stop 

      call cg_gopath_f( cgfile, "/FamilyTree/Family1/Family1.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_node_nfamilies_f( nfam, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nfam .ne. 1) stop 

      call cg_node_family_read_f( nfam, family_name, nb, ng, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      if (family_name .ne. "Family1.1.1") stop
      if (nb .ne. 0) stop
      if (ng .ne. 0) stop

      call cg_node_nfamily_names_f( nnames, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nnames .ne. 3) stop

      do n=1,nnames
        call cg_node_family_name_read_f( n, name, family_name, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        write( tname,'(a,i1)') 'ZoneFamilyN1.1-', n
        write( tfamily_name,'(a)') '/FamilyTree/Family2/Family2.1'
        if (tname .ne. name) stop
        if (tfamily_name .ne. family_name) stop
      enddo

      call cg_delete_node_f( "FamilyN1.1-1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_node_family_name_write_f( "FamilyN1.1-3", "/FamilyTree/Family2/Family3.1", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_node_nfamily_names_f(nnames, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      if (nnames .ne. 2) stop

      do n=1, nnames
        call cg_node_family_name_read_f( n, name, family_name, ierr)
        if (ierr .ne. CG_OK) call cg_error_exit_f
        write( tname,'(a,i1)') 'FamilyN1.1-', n
        write( tfamily_name,'(a, i1, a)') '/FamilyTree/Family2/Family', n, '.1'
        if (tname .ne. name) stop
        if (tfamily_name .ne. family_name) stop
      enddo
 
      call cg_gopath_f( cgfile, "/Structured/Zone/UserData", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_nmultifam_f( nnames, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f
      call cg_famname_read_f( family_name, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_gopath_f(cgfile, "/Structured/Zone/UserData2", ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_nmultifam_f( nnames, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      call cg_famname_read_f( family_name, ierr)
      if( ierr .ne. CG_OK) then
        if( ierr .ne. CG_NODE_NOT_FOUND ) then
          call cg_error_exit_f
        endif
      endif

      call cg_close_f(cgfile, ierr)
      if (ierr .ne. CG_OK) call cg_error_exit_f

      end program
