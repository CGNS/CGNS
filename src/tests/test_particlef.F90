MODULE cgns_particle_test

#include "cgnstypes_f03.h"

  USE iso_c_binding
  USE testing_utils
  USE cgns

  IMPLICIT NONE
  !#define CHECK(L,B) if(!(B)){fprintf(stderr,"mismatch in %s\n",L);exit(1);}

  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(5)  ! This should map to REAL*4 on most modern processors
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(10) ! This should map to REAL*8 on most modern processors


  !void cgi_error(const char *format, ...);

  ! Define constants
  INTEGER, PARAMETER :: MAX_NAME_LENGTH = 32
  INTEGER, PARAMETER :: NUM_XC = 42
  INTEGER, PARAMETER :: npoly = 16
  INTEGER, PARAMETER :: NUM_FACES = 4
  INTEGER, PARAMETER :: NUM_PARTICLE_TYPES = 3

  ! Define global variables
  REAL(c_double), DIMENSION(NUM_XC) :: xc, yc, zc
  INTEGER(cgsize_t), DIMENSION(:), ALLOCATABLE :: poly
  INTEGER(cgsize_t), DIMENSION(:), ALLOCATABLE :: poly_offsets
  INTEGER :: nface
  INTEGER(cgsize_t), DIMENSION(:), ALLOCATABLE :: face
  INTEGER(cgsize_t), DIMENSION(:), ALLOCATABLE :: face_offsets

CONTAINS

  ! Callback to print errors
  SUBROUTINE print_error(error_code, error_msg) BIND(C)

    IMPLICIT NONE

    INTEGER :: i
    INTEGER(C_INT), VALUE :: error_code
    CHARACTER(LEN=1), DIMENSION(*) :: error_msg
    INTEGER :: eol

    eol = 0
    DO i = 1, 80 !CGIO_MAX_ERROR_LENGTH
       IF(error_msg(i)(1:1).EQ.C_NULL_CHAR) EXIT
       eol = eol + 1
    END DO

    SELECT CASE (error_code)
    CASE (0)
       PRINT *, "Warning: ", error_msg(1:eol)
    CASE (1)
       PRINT *, "Error: ", error_msg(1:eol)
    CASE (-1)
       PRINT *, "Fatal Error: ", error_msg(1:eol)
       !  CALL cgio_cleanup_f()
       STOP 1
    CASE default
       PRINT *, "Unknown error code ", error_code, " encountered. Aborting..."
       !   CALL cgio_cleanup_f()
       STOP 1
    END SELECT

  END SUBROUTINE print_error

  ! This tests MLL functions that handle reading and writing of core particle nodes i.e. ParticleZone_t,
  ! ParticleCoordinates_t, ParticleSolution_t, ParticleIterativeData_t and particle fields i.e. DataArray_t

  SUBROUTINE test_particle_io_main()

    IMPLICIT NONE

    INTEGER fnum, bnum, znum, snum, cnum;
    CHARACTER(LEN=19) :: outfile = "test_particlef.cgns"
    INTEGER :: ier

    REAL, DIMENSION(1:5) :: exp
    DOUBLE PRECISION, DIMENSION(1:1) :: output_time = (/0/)
    DOUBLE PRECISION, DIMENSION(1:1) :: iteration = (/1/)
    INTEGER(cgsize_t) :: length = 1
    INTEGER(cgsize_t), DIMENSION(1:3) :: zone_size
    INTEGER(cgsize_t) :: ne
    INTEGER :: pnum, pcnum
    CHARACTER(LEN=32), DIMENSION(1:3) :: particle_names
    INTEGER(CGSIZE_T), DIMENSION(1:3) :: particle_sizes

    DOUBLE PRECISION, DIMENSION(1:2), TARGET :: water_xc, water_yc, water_zc
    INTEGER :: psolnum ! particle solution index
    TYPE(C_FUNPTR) :: f_funptr

    CHARACTER(LEN=32), PARAMETER :: ParticleSolutionName = "ParticleSolution"
    INTEGER :: ii, jj, kk
    REAL, DIMENSION(1:2), TARGET :: radius = (/0.001, 0.005/)
    REAL, DIMENSION(1:2), TARGET :: temperature = (/275., 298./)
    INTEGER :: fieldnum
    INTEGER :: nparticles = 0
    INTEGER :: iparticle
    CHARACTER(LEN=32) :: pname, fname
    INTEGER(cgsize_t) :: size
    DOUBLE PRECISION :: id, coord_id
    INTEGER :: ncoords
    CHARACTER(LEN=32), DIMENSION(1:3) :: coordnames

    INTEGER(cgenum_t), DIMENSION(1:3) :: datatypes

    INTEGER(CGSIZE_T), DIMENSION(1:2) :: dim = (/32,1/)

    INTEGER(CGSIZE_T), DIMENSION(1:3) :: rmin, rmax
    TYPE(C_PTR) :: f_ptr

    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, TARGET :: coord_x, coord_y, coord_z

    INTEGER :: eq_dim, gov_flag, coll, breakup, force, wall_inter, phase_change
    INTEGER(cgenum_t) :: petype, datatype
    INTEGER :: npsols, isol
    DOUBLE PRECISION :: sol_id, field_id
    INTEGER :: nfields, ifield

    REAL, DIMENSION(:), ALLOCATABLE, TARGET :: field


    xc = (/ 0.0d0, 2.0d0, 2.0d0, 0.0d0, 0.0d0, 2.0d0, 2.0d0, 0.0d0, 1.0d0, 1.0d0, 1.0d0, &
         1.0d0, 2.0d0, 1.0d0, 0.0d0, 0.0d0, 2.0d0, 2.0d0, 0.0d0, 1.0d0, 2.0d0, 1.0d0, 0.0d0, &
         1.0d0, 1.0d0, 2.0d0, 1.0d0, 0.0d0, 1.0d0, 1.0d0, &
         1.5d0, 0.5d0, 1.0d0, 1.5d0, 0.5d0, 1.5d0, 0.5d0, &
         0.5d0, 1.5d0, 1.5d0, 0.5d0, &
         1.0d0 /)

    yc = (/ 0.0d0, 0.0d0, 2.0d0, 2.0d0, 0.0d0, 0.0d0, 2.0d0, 2.0d0, 4.0d0, 4.0d0, 2.0d0, &
         0.0d0, 1.0d0, 2.0d0, 1.0d0, 0.0d0, 0.0d0, 2.0d0, 2.0d0, 0.0d0, 1.0d0, 2.0d0, 1.0d0, &
         1.0d0, 0.0d0, 1.0d0, 2.0d0, 1.0d0, 1.0d0, 1.0d0, &
         3.0d0, 3.0d0, 4.0d0, 3.0d0, 3.0d0, 3.0d0, 3.0d0, &
         1.0d0, 1.0d0, 2.0d0, 2.0d0, &
         3.0d0 /)

    zc = (/ 0.0d0, 0.0d0, 0.0d0, 0.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0, 0.0d0, 2.0d0, 4.0d0, &
         0.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0, &
         0.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 2.0d0, 1.0d0, &
         0.0d0, 0.0d0, 1.0d0, 2.0d0, 2.0d0, 1.0d0, 1.0d0, &
         3.0d0, 3.0d0, 3.0d0, 3.0d0, &
         3.0d0 /)

    ALLOCATE(poly(1:56))
    poly(1:56) = (/ 1, 4, 3, 2, 2, 3, 7, 6, 3, 4, 8, 7, 1, 5, 8, 4, 1, 5, 6, 2, 5, 6, 7, 8, 3, 4, 9, &
         8, 7, 10, 3, 9, 10, 7, 4, 8, 10, 9, 5, 6, 11, 6, 7, 11, 7, 8, 11, 5, 11, 8, 7, 10, 11, &
         8, 11, 10 /)

    ALLOCATE(poly_offsets(1:17))
    poly_offsets = (/ 0, 4, 8, 12, 16, 20, 24, 27, 30, 34, 38, 41, 44, 47, 50, 53, 56 /)

    nface = 4
    ALLOCATE(face(1:20))
    face = (/ 1, 2, 3, 4, 5, 6, -3, 7, 8, 9, 10,-6, 11, 12, 13, 14, -8, -13, 15, 16/)

    ALLOCATE(face_offsets(1:5))
    face_offsets = (/0, 6, 11, 16, 20/)

    WRITE(*,'(A)') "creating CGNS file "//outfile

    f_funptr = C_FUNLOC(print_error)

    CALL cg_configure_f(CG_CONFIG_ERROR, f_funptr, ier)

    CALL cg_open_f(outfile, CG_MODE_WRITE, fnum, ier)
    CALL cg_base_write_f(fnum, "Base", 3, 3, bnum, ier)
    CALL cg_goto_f(fnum, bnum, ier, 'end')

    CALL cg_dataclass_write_f(CGNS_ENUMV(NormalizedByDimensional), ier)

    ! Write BaseIterativeData_t so that we can write out ParticleIterativeData_t later on
    CALL cg_goto_f(fnum, bnum, ier, 'end')

    CALL cg_biter_write_f(fnum, bnum, "BaseIterativeData", 1, ier)

    CALL cg_gopath_f(fnum, "./BaseIterativeData", ier)
    CALL cg_array_write_f("TimeValues", CGNS_ENUMV(RealDouble), 1_CGSIZE_T, length, output_time, ier)
    CALL cg_array_write_f("IterationValues", CGNS_ENUMV(INTEGER), 1_CGSIZE_T, length, iteration, ier)

    exp(:) = 0.0
    exp(2) = 1.0

    zone_size(1) = 11
    zone_size(2) = 4
    zone_size(3) = 0

    CALL cg_zone_write_f(fnum, bnum, "Cells", zone_size, CGNS_ENUMV(Unstructured), znum, ier)
    CALL cg_coord_write_f(fnum, bnum, znum, CGNS_ENUMV(RealDouble), "CoordinateX", xc, cnum, ier)
    CALL cg_goto_f(fnum, bnum, ier, "Zone_t", znum, "GridCoordinates", 0, "CoordinateX", 0, "end")
    CALL cg_exponents_write_f(CGNS_ENUMV(RealSingle), exp, ier)
    CALL cg_coord_write_f(fnum, bnum, znum, CGNS_ENUMV(RealDouble), "CoordinateY", yc, cnum, ier)
    CALL cg_gopath_f(fnum, "../CoordinateY",ier)
    CALL cg_exponents_write_f(CGNS_ENUMV(RealSingle), exp, ier)
    CALL cg_coord_write_f(fnum, bnum, znum, CGNS_ENUMV(RealDouble), "CoordinateZ", zc, cnum, ier)
    CALL cg_gopath_f(fnum, "../CoordinateZ", ier)
    CALL cg_exponents_write_f(CGNS_ENUMV(RealSingle), exp, ier)

    ne = 0

    ! NGON_n first so polyhedra face references are correct
    CALL cg_poly_section_write_f(fnum, bnum, znum, "NGON_n", CGNS_ENUMV(NGON_n), &
         ne+1, ne+npoly, 0, poly, poly_offsets, snum, ier)
    ne = ne + npoly

    ! NFACE_n
    CALL cg_poly_section_write_f(fnum, bnum, znum, "NFACE_n", CGNS_ENUMV(NFACE_n), &
         ne+1, ne+nface, 0, face, face_offsets, snum, ier)

    particle_names(1) = "IsoOctane"
    particle_names(2) = "Oil"
    particle_names(3) = "Water"

    particle_sizes(1:3) = (/0, 0, 2/)

    ! Create ParticleZone_t nodes
    ! It is okay to define ParticleZone_t nodes with 0 particles
    ! this implies that for the current solution time, no particles
    ! of said type (IsoOctane and Oil) are in the Base

    CALL cg_particle_write_f(fnum, bnum, "IsoOctane", 0_CGSIZE_T, pnum, ier) ! 0 IsoOctane particles
    CALL cg_particle_write_f(fnum, bnum, "Oil", 0_CGSIZE_T, pnum, ier)       ! 0 Oil particles
    CALL cg_particle_write_f(fnum, bnum, "Water", 2_CGSIZE_T, pnum, ier)     ! 2 Water particles

    ! Write coordinates for the water particle */
    water_xc(1:2) = (/0.7, 0.01/)
    water_yc(1:2) = (/0.8, 0.02/)
    water_zc(1:2) = (/0.9, 0.03/)

    CALL cg_particle_coord_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateX", C_LOC(water_xc(1)), pcnum, ier)
    CALL cg_particle_coord_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateY", C_LOC(water_yc(1)), pcnum, ier)
    CALL cg_particle_coord_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateZ", C_LOC(water_zc(1)), pcnum, ier)

    CALL cg_goto_f(fnum, bnum, ier, "ParticleZone_t", 3, "ParticleCoordinates_t", 1, "end")
    CALL cg_dataclass_write_f(CGNS_ENUMV(Dimensional), ier)
    CALL cg_units_write_f(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), CGNS_ENUMV(Degree), ier)

    DO ii = 1, 3
       CALL cg_goto_f(fnum, bnum, ier, "ParticleZone_t", 3, "ParticleCoordinates_t", 1, "DataArray_t", ii, "end")
       CALL cg_exponents_write_f(CGNS_ENUMV(RealSingle), exp, ier)
    ENDDO

    ! Write particle solution

    CALL cg_particle_sol_write_f(fnum, bnum, pnum, ParticleSolutionName, psolnum, ier)

    CALL cg_particle_field_write_f(fnum, bnum, pnum, psolnum, CGNS_ENUMV(RealSingle), &
         "Radius", C_LOC(radius(1)), fieldnum, ier)
    CALL cg_particle_field_write_f(fnum, bnum, pnum, psolnum, CGNS_ENUMV(RealSingle), &
         "Temperature", C_LOC(temperature(1)), fieldnum, ier)

    ! Write particle iterative data
    CALL cg_piter_write_f(fnum, bnum, pnum, "ParticleIterativeData", ier)
    CALL cg_goto_f(fnum, bnum, ier, "ParticleZone_t", pnum, "end")
    CALL cg_gopath_f(fnum, "./ParticleIterativeData", ier)

    CALL cg_array_write_f("ParticleSolutionPointers",  CGNS_ENUMT(Character), 2_CGSIZE_T, dim, ParticleSolutionName, ier)

    ! Write particle governing equations and models
    CALL cg_goto_f(fnum, bnum, ier, "ParticleZone_t", pnum, "end")
    CALL cg_particle_equationset_write_f(3, ier)
    CALL cg_gopath_f(fnum, "./ParticleEquationSet", ier)
    CALL cg_particle_governing_write_f(CGNS_ENUMV(DEM), ier)

    CALL cg_particle_model_write_f("ParticleCollisionModel_t", CGNS_ENUMV(SoftSphere), ier)
    CALL cg_particle_model_write_f("ParticleBreakupModel_t", CGNS_ENUMV(RayleighTaylor), ier)
    CALL cg_particle_model_write_f("ParticleForceModel_t", CGNS_ENUMV(Gidaspow), ier)
    CALL cg_particle_model_write_f("ParticleWallInteractionModel_t", CGNS_ENUMV(BaiGosman), ier)
    CALL cg_particle_model_write_f("ParticlePhaseChangeModel_t", CGNS_ENUMV(Boil), ier)

    CALL cg_close_f(fnum, ier)

    ! Reopen the file and verify that the particle data matches what we wrote in

    CALL cg_open_f(outfile, CG_MODE_READ, fnum, ier)

    CALL cg_nbases_f(fnum, bnum, ier)

    CALL cg_nparticle_zones_f(fnum, bnum, nparticles, ier)

    IF(nparticles .NE. 3)THEN
       WRITE(*,'(A,I0,A)')"Expected 3 ParticleZone_t nodes but found ", nparticles, " ParticleZone_t nodes instead"
       CALL cg_error_exit_f
    ENDIF

    DO iparticle = 1, nparticles

       CALL cg_particle_read_f(fnum, bnum, iparticle, pname, size, ier)
       CALL cg_particle_id_f(fnum, bnum, iparticle, id, ier)

       IF(.NOT.check_eq(particle_names(iparticle),pname))THEN
          WRITE(*,'(A)') "Expected "//TRIM(particle_names(iparticle))//" as the ParticleZone_t node name but found " &
               //TRIM(pname)//" instead"
          CALL cg_error_exit_f()
       END IF

       IF(.NOT.check_eq(size, particle_sizes(iparticle)))THEN
          WRITE(*,'(A, I0, A, I0, A)') "Expected ", particle_sizes(iparticle)," as the ParticleZone_t node size but found",&
               size, " instead"
          CALL cg_error_exit_f()
       END IF

       ! Check to see if we have coordinates
       CALL cg_particle_ncoords_f(fnum, bnum, iparticle, ncoords, ier)
       IF(ncoords .NE. 3) CYCLE

       ! Since we wrote out coordinates for water alone, verify that they match
       DO jj = 1, ncoords
          CALL cg_particle_coord_id_f(fnum, bnum, iparticle, jj, coord_id, ier)
          CALL cg_particle_coord_info_f(fnum, bnum, iparticle, jj, datatypes(jj), coordnames(jj), ier)
       ENDDO

       rmin = 1
       rmax = size
       ALLOCATE(coord_x(1:size), coord_y(1:size), coord_z(1:size))

       f_ptr = C_LOC(coord_x(1))
       CALL cg_particle_coord_read_f(fnum, bnum, iparticle, coordnames(1), datatypes(1), rmin, rmax, f_ptr, ier)
       f_ptr = C_LOC(coord_y(1))
       CALL cg_particle_coord_read_f(fnum, bnum, iparticle, coordnames(2), datatypes(2), rmin, rmax, f_ptr, ier)
       f_ptr = C_LOC(coord_z(1))
       CALL cg_particle_coord_read_f(fnum, bnum, iparticle, coordnames(3), datatypes(3), rmin, rmax, f_ptr, ier)

       DO kk = 1, size
          IF(.NOT.check_eq(coord_x(kk), water_xc(kk)) .OR. &
               .NOT.check_eq(coord_y(kk), water_yc(kk)) .OR. &
               .NOT.check_eq(coord_z(kk), water_zc(kk)) ) THEN
             WRITE(*,'(A)') "Invalid particle coordinate data - written value doesn't match the read value"
             CALL cg_error_exit_f()
          ENDIF
       ENDDO
       DEALLOCATE(coord_x, coord_y, coord_z)

       CALL cg_goto_f(fnum, bnum, ier, "ParticleZone_t", iparticle, "end")

       CALL cg_particle_equationset_read_f(eq_dim, gov_flag, coll, breakup, force, wall_inter, phase_change, ier)


       IF(eq_dim .NE. 3)THEN
          WRITE(*,'(A,I0,A)') "Expected particle equation dimension of 3 but found ", eq_dim, " instead"
          CALL cg_error_exit_f()
       ENDIF

       IF(gov_flag .NE. 1)THEN
          WRITE(*,'(A)') "Particle governing equations cannot be found"
          CALL cg_error_exit_f()
       ELSE
          CALL cg_gopath_f(fnum, "./ParticleEquationSet", ier)
          CALL cg_particle_governing_read_f(petype, ier)
          IF(petype .NE. CGNS_ENUMV(DEM))THEN
             WRITE(*,'(A)') "Invalid particle governing equation type"
             CALL cg_error_exit_f()
          ENDIF
       ENDIF

       ! Read solution data
       CALL cg_particle_nsols_f(fnum, bnum, iparticle, npsols, ier)
       IF(npsols .NE. psolnum)THEN
          WRITE(*,'(A,I0,A,I0,A)') "For ParticleZone_t ", iparticle, " expected ", &
               psolnum," ParticleSolution_t node(s) but found ",npsols," instead"
          CALL cg_error_exit_f()
       ENDIF

       DO isol = 1, npsols
          CALL cg_particle_sol_id_f(fnum, bnum, iparticle, isol, sol_id, ier)

          CALL cg_particle_nfields_f(fnum, bnum, iparticle, isol, nfields, ier)
          IF(nfields .NE. fieldnum)THEN
             WRITE(*,'(A,I0,A,I0,A)') "For ParticleZone_t ",iparticle," > ParticleSolution_t ", psolnum, &
                  " expected ", fieldnum," DataArray_t node(s) but found ", nfields," instead"
             CALL cg_error_exit_f()
          ENDIF

          DO ifield = 1, nfields

             CALL cg_particle_field_id_f(fnum, bnum, iparticle, isol, ifield, field_id, ier)

             CALL cg_particle_field_info_f(fnum, bnum, iparticle, isol, ifield, datatype, fname, ier)

             rmin = 1
             rmax = size
             ALLOCATE(field(1:size))
             f_ptr = C_LOC(field(1))
             CALL cg_particle_field_read_f(fnum, bnum, iparticle, isol, fname, datatype, rmin, rmax, f_ptr, ier)

             IF(ifield.EQ.1)THEN
                PRINT*,field(1),radius(1)
                IF(field(1).NE.radius(1))THEN
                   CALL cg_error_exit_f()
                ENDIF
             ELSE
                IF(field(1).NE.temperature(1))THEN
                   CALL cg_error_exit_f()
                ENDIF
             ENDIF
             DEALLOCATE(field)
          ENDDO
       ENDDO
    ENDDO

    CALL cg_close_f(fnum, ier)

  END SUBROUTINE test_particle_io_main

  SUBROUTINE test_particle_io_partial()

    IMPLICIT NONE

    INTEGER :: fnum, bnum, pnum, cgcoord, cgsol
    CHARACTER(LEN=27) :: fname = "particle_partial_testf.cgns"
    INTEGER(cgsize_t) :: num_particles = 100
    INTEGER :: n, k, i
    INTEGER :: ier
    REAL, DIMENSION(:), ALLOCATABLE, TARGET :: coord, coord_read
    INTEGER(cgsize_t), DIMENSION(1:3) :: rmin, rmax
    INTEGER :: cgfld
    CHARACTER(LEN=32) :: field_name
    INTEGER(cgenum_t) :: type
    REAL, DIMENSION(:), ALLOCATABLE, TARGET :: field
    TYPE(C_PTR) :: f_ptr

    ! Allocate arrays
    ALLOCATE(coord(1:num_particles))
    DO n = 1, num_particles
       coord(n) = REAL(n)
    END DO

    ! Delete existing file if present
    OPEN(10, FILE=fname, STATUS='OLD', IOSTAT=ier)
    IF(ier == 0) THEN
       CLOSE(10, STATUS='DELETE')
    END IF

    WRITE(*,'(A)') "creating CGNS file "//TRIM(fname)

    ! Configure error handler
    CALL cg_configure_f(CG_CONFIG_ERROR, C_FUNLOC(print_error), ier)

    ! Create file and write base
    CALL cg_open_f(fname, CG_MODE_WRITE, fnum, ier)
    CALL cg_base_write_f(fnum, "Base", 3, 3, bnum, ier)

    WRITE(*,*) "writing particle zone"
    CALL cg_particle_write_f(fnum, bnum, "ParticleZone", num_particles, pnum, ier)

    ! Write coordinates for odd numbered particles
    WRITE(*,*) "coordinates -> 1,3,5,7 ..."
    DO k = 1, num_particles, 2
       rmin = k
       rmax = k
       f_ptr = C_LOC(coord(k))
       CALL cg_particle_coord_partial_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), &
            "CoordinateX", rmin, rmax, f_ptr, cgcoord, ier)
       f_ptr = C_LOC(coord(k))
       CALL cg_particle_coord_partial_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), &
            "CoordinateY", rmin, rmax, f_ptr, cgcoord, ier)
       f_ptr = C_LOC(coord(k))
       CALL cg_particle_coord_partial_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), &
            "CoordinateZ", rmin, rmax, f_ptr, cgcoord, ier)
    END DO

    ! Write solution
    CALL cg_particle_sol_write_f(fnum, bnum, pnum, "Solution", cgsol, ier)
    IF(ier .NE. 0) CALL cg_error_exit_f()

    ! Write field data for odd numbered particles
    WRITE(*,'(A)') "field -> 1,3,5,7 ..."
    DO n = 1, num_particles, 2
       rmin = n
       rmax = n
       f_ptr = C_LOC(coord(n))
       CALL cg_particle_field_partial_write_f(fnum, bnum, pnum, cgsol, CGNS_ENUMV(RealSingle), &
            "Field", rmin, rmax, f_ptr, cgfld, ier)
    END DO

    ! Close and reopen in modify mode
    WRITE(*,*) "closing and reopening in modify mode"
    CALL cg_close_f(fnum, ier)
    CALL cg_open_f(fname, CG_MODE_MODIFY, fnum, ier)
    pnum = 1

    ! Fill in even numbered particles
    WRITE(*,*) "coordinates -> 2,4,6,8 ..."
    DO k = 1, num_particles-1, 2
       rmin = k + 1
       rmax = k + 1
       f_ptr = C_LOC(coord(k+1))
       CALL cg_particle_coord_partial_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), &
            "CoordinateX", rmin, rmax, f_ptr, cgcoord, ier)
       f_ptr = C_LOC(coord(k+1))
       CALL cg_particle_coord_partial_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), &
            "CoordinateY", rmin, rmax, f_ptr, cgcoord, ier)
       f_ptr = C_LOC(coord(k+1))
       CALL cg_particle_coord_partial_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), &
            "CoordinateZ", rmin, rmax, f_ptr, cgcoord, ier)
    END DO

    ! Fill in field data for even numbered particles
    WRITE(*,*) "field -> 2,4,6,8 ..."
    DO n = 1, num_particles-1, 2
       rmin = n + 1
       rmax = n + 1
       f_ptr = C_LOC(coord(n+1))
       CALL cg_particle_field_partial_write_f(fnum, bnum, pnum, cgsol, CGNS_ENUMV(RealSingle), &
            "Field", rmin, rmax, f_ptr, cgfld, ier)
    END DO

    ! Close and reopen in read mode
    WRITE(*,*) "closing and reopening in read mode" 
    CALL cg_close_f(fnum, ier)

    CALL cg_open_f(fname, CG_MODE_READ, fnum, ier)

    ! Verify data
    ALLOCATE(coord_read(num_particles))
    coord_read = -1.0
    rmin = 1
    rmax = num_particles

    ! Read and verify coordinates
    f_ptr = C_LOC(coord_read(1))
    CALL cg_particle_coord_read_f(fnum, bnum, pnum, "CoordinateX", CGNS_ENUMV(RealSingle), &
         rmin, rmax, f_ptr, ier)

    DO i = 1, rmax(1)-1
       IF(.NOT. check_eq(coord(i), coord_read(i))) THEN
          WRITE(*,*) "Particle partial coordinate write (CoordinateX) failed"
          CALL cg_error_exit_f()
       END IF
    END DO

    coord_read = -1.0
    f_ptr = C_LOC(coord_read(1))
    CALL cg_particle_coord_read_f(fnum, bnum, pnum, "CoordinateY", CGNS_ENUMV(RealSingle), &
         rmin, rmax, f_ptr, ier)
    DO i = 1, rmax(1)-1
       IF(.NOT. check_eq(coord(i), coord_read(i))) THEN
          WRITE(*,*) "Particle partial coordinate write (CoordinateY) failed"
          CALL cg_error_exit_f()
       END IF
    END DO

    coord_read = -1.0
    f_ptr = C_LOC(coord_read(1))
    CALL cg_particle_coord_read_f(fnum, bnum, pnum, "CoordinateZ", CGNS_ENUMV(RealSingle), &
         rmin, rmax, f_ptr, ier)
    DO i = 1, rmax(1)-1
       IF(.NOT. check_eq(coord(i), coord_read(i))) THEN
          WRITE(*,*) "Particle partial coordinate write (CoordinateZ) failed"
          CALL cg_error_exit_f()
       END IF
    END DO

    DEALLOCATE(coord_read)

    ! Verify field data
    CALL cg_particle_field_info_f(fnum, bnum, pnum, 1, 1, type, field_name, ier)

    ALLOCATE(field(num_particles))
    f_ptr = C_LOC(field(1))
    CALL cg_particle_field_read_f(fnum, bnum, pnum, 1, field_name, type, rmin, rmax, f_ptr, ier)

    DO i = 1, rmax(1)-1
       IF(.NOT. check_eq(coord(i), field(i))) THEN
          WRITE(*,*) "Particle partial field write failed"
          CALL cg_error_exit_f()
       END IF
    END DO

    DEALLOCATE(field)
    DEALLOCATE(coord)

    CALL cg_close_f(fnum, ier)

  END SUBROUTINE test_particle_io_partial

  SUBROUTINE test_particle_bbox()
    IMPLICIT NONE

    INTEGER :: ier, fnum, bnum, pnum, cgcoord
    CHARACTER(LEN=19) :: fname = "particle_bboxf.cgns"
    DOUBLE PRECISION, DIMENSION(1:2,1:3), TARGET :: bbox
    INTEGER(cgsize_t) :: num_particles = 10
    INTEGER :: n
    REAL, DIMENSION(:), ALLOCATABLE, TARGET :: coord
    REAL, DIMENSION(1:2,1:3), TARGET :: bbox_float
    TYPE(C_PTR) :: f_ptr

    ! Allocate and initialize coord array
    ALLOCATE(coord(1:num_particles))
    DO n = 1, num_particles
       coord(n) = REAL(n-1)
    END DO

    ! Delete existing file if present
    OPEN(10, FILE=fname, STATUS='OLD', IOSTAT=ier)
    IF(ier == 0) THEN
       CLOSE(10, STATUS='DELETE')
    END IF

    WRITE(*,'(A)') "creating CGNS file "//TRIM(fname)

    CALL cg_configure_f(CG_CONFIG_ERROR, C_FUNLOC(print_error), ier)

    CALL cg_open_f(fname, CG_MODE_WRITE, fnum, ier)
    CALL cg_base_write_f(fnum, "Base", 3, 3, bnum, ier)

    WRITE(*,*) "writing particle zone"

    CALL cg_particle_write_f(fnum, bnum, "ParticleZone", num_particles, pnum, ier)
    f_ptr = C_LOC(coord(1))
    CALL cg_particle_coord_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateX", f_ptr, cgcoord, ier)
    f_ptr = C_LOC(coord(1))
    CALL cg_particle_coord_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateY", f_ptr, cgcoord, ier)
    f_ptr = C_LOC(coord(1))
    CALL cg_particle_coord_write_f(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateZ", f_ptr, cgcoord, ier)

    WRITE(*,*) "closing and reopening in read mode"
    CALL cg_close_f(fnum, ier)
    ! Read file
    CALL cg_open_f(fname, CG_MODE_READ, fnum, ier)
    bnum = 1
    pnum = 1

    bbox(1,:) =  1.D0
    bbox(2,:) = -1.D0
    ! Check bounding box is not modified
    f_ptr = C_LOC(bbox(1,1))
    CALL cg_particle_bounding_box_read_f(fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), f_ptr, ier)
    IF (ier .NE. CG_NODE_NOT_FOUND) CALL cg_error_exit_f()

    IF (.NOT. check_eq(bbox(2,1), -1.D0)) THEN
       WRITE(*,*) "Unmodified bounding box data when missing data in ParticleCoordinates node"
       CALL cg_error_exit_f()
    END IF
    IF (.NOT. check_eq(bbox(1,1), 1.D0)) THEN
       WRITE(*,*) "Unmodified bounding box data when missing data in ParticleCoordinates node"
       CALL cg_error_exit_f()
    END IF

    WRITE(*,*) "closing and reopening in modify mode"
    CALL cg_close_f(fnum, ier)

    CALL cg_open_f(fname, CG_MODE_MODIFY, fnum, ier)
    bnum = 1
    pnum = 1

    bbox(1,:) = 0.D0
    bbox(2,:) = REAL(num_particles - 1, KIND(1.D0))

    f_ptr = C_LOC(bbox(1,1))
    CALL cg_particle_bounding_box_write_f(fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), f_ptr, ier)

    WRITE(*,*) "closing and reopening in read mode"
    CALL cg_close_f(fnum, ier)

    CALL cg_open_f(fname, CG_MODE_READ, fnum, ier)
    bnum = 1
    pnum = 1

    bbox(1,:) = 1.D0
    bbox(2,:) = -1.D0

    f_ptr = C_LOC(bbox(1,1))
    CALL cg_particle_bounding_box_read_f(fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), f_ptr, ier)

    IF (.NOT. check_eq(bbox(1,1), 0.D0)) THEN
       WRITE(*,*) "Read bounding box failed"
       CALL cg_error_exit_f()
    END IF
    IF (.NOT. check_eq(bbox(2,1), REAL(num_particles - 1, KIND(1.D0)))) THEN
       WRITE(*,*) "Read bounding box failed"
       CALL cg_error_exit_f()
    END IF
    ! Check datatype conversion
    bbox_float(1,:) = 1.0
    bbox_float(2,:) = -1.0
    f_ptr = C_LOC(bbox_float(1,1))
    CALL cg_particle_bounding_box_read_f(fnum, bnum, pnum, 1, CGNS_ENUMV(RealSingle), f_ptr, ier)

    IF (.NOT. check_eq(bbox_float(1,1), 0.0)) THEN
       WRITE(*,*) "Read bounding box failed"
       CALL cg_error_exit_f()
    END IF
    IF (.NOT. check_eq(bbox_float(2,1), REAL(num_particles - 1))) THEN
       WRITE(*,*) "Read bounding box failed"
       CALL cg_error_exit_f()
    END IF

    WRITE(*,*) "closing and reopening to test limit cases"
    CALL cg_close_f(fnum, ier)

    CALL cg_open_f(fname, CG_MODE_MODIFY, fnum, ier)
    bnum = 1
    pnum = 1

    ! Writing integer type should fail
    f_ptr = C_LOC(bbox(1,1))
    CALL cg_particle_bounding_box_write_f(fnum, bnum, pnum, 1, CGNS_ENUMV(INTEGER), f_ptr, ier)
    IF (ier == 0) THEN
       WRITE(*,*) "writing Integer bounding box data failed to produce error"
       CALL cg_error_exit_f()
    END IF

    ! Reset error state
    ! CALL cgi_error_f("no CGNS error reported")

    DEALLOCATE(coord)

    WRITE(*,*) "closing file"
    CALL cg_close_f(fnum, ier)

  END SUBROUTINE test_particle_bbox

  SUBROUTINE test_particle_coord_io_and_ptset()

    IMPLICIT NONE

    INTEGER :: fnum, bnum, pnum, cgcoord
    CHARACTER(LEN=35) :: fname = "particle_coord_and_ptset_testf.cgns"
    INTEGER(cgsize_t), DIMENSION(1:1) :: num_particles = (/100/)
    INTEGER :: n, i, j
    REAL, DIMENSION(:), ALLOCATABLE, TARGET :: coord, DATA
    INTEGER :: snum, field
    INTEGER(cgsize_t), DIMENSION(2) :: pnts = (/1, 10/)
    INTEGER(cgsize_t), DIMENSION(:), ALLOCATABLE, TARGET :: pnts_list, pnts_read
    INTEGER :: ncoord_nodes
    CHARACTER(LEN=32) :: coordname
    INTEGER(cgenum_t) :: ptset_type
    INTEGER(cgsize_t) :: npnts, sol_size
    INTEGER(cgsize_t), DIMENSION(1:3) :: rmin, rmax
    CHARACTER(LEN=32) :: field_name
    INTEGER :: ier
    TYPE(C_PTR) :: f_ptr

    ! Allocate and initialize coord array
    ALLOCATE(coord(1:num_particles(1)))
    DO n = 1, num_particles(1)
       coord(n) = REAL(n)
    END DO

    ! Delete existing file if present
    OPEN(10, FILE=fname, STATUS='OLD', IOSTAT=i)
    IF(i == 0) THEN
       CLOSE(10, STATUS='DELETE')
    END IF

    WRITE(*,*) "creating CGNS file ", TRIM(fname)

    CALL cg_configure_f(CG_CONFIG_ERROR, C_FUNLOC(print_error), ier)

    CALL cg_open_f(fname, CG_MODE_WRITE, fnum, ier)
    CALL cg_base_write_f(fnum, "Base", 3, 3, bnum, ier)

    WRITE(*,*) "writing particle zone"
    CALL cg_particle_write_f(fnum, bnum, "ParticleZone", num_particles(1), pnum, ier)

    WRITE(*,*) "writing coordinate nodes"

    CALL cg_particle_coord_node_write_f(fnum, bnum, pnum, "ParticleCoordinates", cgcoord, ier)
    CALL cg_goto_f(fnum, bnum, ier, "ParticleZone_t", pnum, "ParticleCoordinates_t", cgcoord, "end")
    CALL cg_array_write_f("CoordinateX", CGNS_ENUMV(RealSingle), 1_CGSIZE_T, num_particles, coord, ier)
    CALL cg_array_write_f("CoordinateY", CGNS_ENUMV(RealSingle), 1_CGSIZE_T, num_particles, coord, ier)
    CALL cg_array_write_f("CoordinateZ", CGNS_ENUMV(RealSingle), 1_CGSIZE_T, num_particles, coord, ier)

    CALL cg_particle_sol_ptset_write_f(fnum, bnum, pnum, "Solution 1", CGNS_ENUMV(PointRange), 2_CGSIZE_T, pnts, snum, ier)
    f_ptr = C_LOC(coord)
    CALL cg_particle_field_write_f(fnum, bnum, pnum, snum, CGNS_ENUMV(RealSingle), "Field", f_ptr, field, ier)
    ALLOCATE(pnts_list(num_particles(1)))
    DO n = 1, num_particles(1)
       pnts_list(n) = n
    END DO

    CALL cg_particle_sol_ptset_write_f(fnum, bnum, pnum, "Solution 2", CGNS_ENUMV(PointList), num_particles(1), pnts_list, snum, ier)
    f_ptr = C_LOC(coord)
    CALL cg_particle_field_write_f(fnum, bnum, pnum, snum, CGNS_ENUMV(RealSingle), "Field", f_ptr, field, ier)

    DEALLOCATE(pnts_list)

    WRITE(*,*) "closing and reopening in read mode"
    CALL cg_close_f(fnum, ier)

    CALL cg_open_f(fname, CG_MODE_READ, fnum, ier)

    ! Verify coordinate count
    CALL cg_particle_ncoord_nodes_f(fnum, bnum, pnum, ncoord_nodes, ier)

    DO i = 1, ncoord_nodes
       CALL cg_particle_coord_node_read_f(fnum, bnum, pnum, i, coordname, ier)

       IF (.NOT.check_eq(coordname,"ParticleCoordinates")) THEN
          WRITE(*,*) "Invalid coordinate name"
          CALL cg_error_exit_f()
       END IF
    END DO

    ! Read and verify PointRange solution
    snum = 1
    CALL cg_particle_sol_ptset_info_f(fnum, bnum, pnum, snum, ptset_type, npnts, ier)

    IF (ptset_type .NE. CGNS_ENUMV(PointRange)) THEN
       WRITE(*,*) "Particle solution point set type mismatch"
       CALL cg_error_exit_f()
    END IF

    IF (npnts .NE. 2) THEN
       WRITE(*,*) "Particle solution point set size mismatch"
       CALL cg_error_exit_f()
    END IF

    ALLOCATE(pnts_read(1:npnts))
    CALL cg_particle_sol_ptset_read_f(fnum, bnum, pnum, snum, pnts_read, ier)

    CALL cg_particle_sol_size_f(fnum, bnum, pnum, snum, sol_size, ier)
    IF (sol_size .NE. pnts_read(2) - pnts_read(1) + 1) THEN
       WRITE(*,*) "Particle sol size mismatch"
       CALL cg_error_exit_f()
    END IF

    ! Read and verify field data for PointRange
    rmin(1) = 1
    rmax(1) = pnts_read(2) - pnts_read(1) + 1
    ALLOCATE(DATA(sol_size))
    f_ptr = C_LOC(data)
    CALL cg_particle_field_read_f(fnum, bnum, pnum, snum, "Field", CGNS_ENUMV(RealSingle), rmin, rmax, f_ptr, ier)

    DO i = pnts_read(1), pnts_read(2)
       j = i - pnts_read(1) + 1
       IF (.NOT. check_eq(coord(i), DATA(j))) THEN
          WRITE(*,*) "Particle field data mismatch"
          CALL cg_error_exit_f()
       END IF
    END DO

    DEALLOCATE(DATA)
    DEALLOCATE(pnts_read)

    ! Read and verify PointList solution
    snum = 2
    CALL cg_particle_sol_ptset_info_f(fnum, bnum, pnum, snum, ptset_type, npnts, ier)

    IF (ptset_type .NE. CGNS_ENUMV(PointList)) THEN
       WRITE(*,*) "Particle solution point set type mismatch"
       CALL cg_error_exit_f()
    END IF

    IF (npnts .NE. num_particles(1)) THEN
       WRITE(*,*) "Particle solution point set size mismatch"
       CALL cg_error_exit_f()
    END IF

    ALLOCATE(pnts_read(npnts))
    CALL cg_particle_sol_ptset_read_f(fnum, bnum, pnum, snum, pnts_read, ier)

    CALL cg_particle_sol_size_f(fnum, bnum, pnum, snum, sol_size, ier)
    IF (sol_size .NE. npnts) THEN
       WRITE(*,*) "Particle sol size mismatch"
       CALL cg_error_exit_f()
    END IF

    ! Read and verify field data for PointList
    rmin(1) = 1
    rmax(1) = npnts
    ALLOCATE(DATA(sol_size))
    f_ptr = C_LOC(data(1))
    CALL cg_particle_field_read_f(fnum, bnum, pnum, snum, "Field", CGNS_ENUMV(RealSingle), rmin, rmax, f_ptr, ier)

    DO i = 1, npnts
       IF (.NOT. check_eq(coord(pnts_read(i)), DATA(i))) THEN
          WRITE(*,*) "Particle field data mismatch"
          CALL cg_error_exit_f()
       END IF
    END DO

    DEALLOCATE(DATA)
    DEALLOCATE(pnts_read)
    DEALLOCATE(coord)

    CALL cg_close_f(fnum, ier)
  END SUBROUTINE test_particle_coord_io_and_ptset

END MODULE cgns_particle_test

PROGRAM main

  USE cgns_particle_test

  ! These tests should cover all serial MLL functions related to particles
  ! that were added as a part of CPEX 0046 (V4.5)

  CALL test_particle_io_main()
  CALL test_particle_io_partial()
  CALL test_particle_bbox()
  CALL test_particle_coord_io_and_ptset()

  WRITE(*,"(A)") "All particle tests completed successfully"

END PROGRAM main
