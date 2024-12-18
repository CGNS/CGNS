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

  INTEGER(CGSIZE_T) :: rmin, rmax
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
  CALL cg_array_write_f("TimeValues", CGNS_ENUMV(RealDouble), 1, length, output_time, ier)
  CALL cg_array_write_f("IterationValues", CGNS_ENUMV(INTEGER), 1, length, iteration, ier)

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


END MODULE cgns_particle_test

#if 0
// Test partial write routines
static void test_particle_io_partial()
{
   int fnum, bnum, pnum, cgcoord, cgsol
   static char *fname = "particle_partial_test.cgns"

   cgsize_t num_particles = 100

   float* coord = (float*)malloc(sizeof(float)*num_particles)
   for (int n = 0 n < num_particles n++)
       coord[n] = (float)n

   unlink (fname)
   printf ("creating CGNS file %s\n", fname)

   cg_configure(CG_CONFIG_ERROR, (void*) print_error)

   cg_open (fname, CG_MODE_WRITE, &fnum)
   cg_base_write (fnum, "Base", 3, 3, &bnum)

   /* write particle zone */

   puts ("writing particle zone")

   cg_particle_write (fnum, bnum, "ParticleZone", num_particles, &pnum)

   /* write the coordinates of every other particle */
   puts("coordinates -> 1,3,5,7 ...")
   for (int k = 0 k < (int)num_particles k += 2) {
      cgsize_t rmin = k + 1
      cgsize_t rmax = k + 1
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateX", &rmin, &rmax, &coord[k], &cgcoord)
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateY", &rmin, &rmax, &coord[k], &cgcoord)
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateZ", &rmin, &rmax, &coord[k], &cgcoord)
   }

   /* write every other solution value */

   if (cg_particle_sol_write(fnum, bnum, pnum, "Solution", &cgsol))
       cg_error_exit()

   puts("field -> 1,3,5,7 ...")
   for (int n = 0 n < (int)num_particles n += 2) {
       cgsize_t rmin = n + 1
       cgsize_t rmax = n + 1
       int cgfld
       cg_particle_field_partial_write(fnum, bnum, pnum, cgsol, CGNS_ENUMV(RealSingle), "Field", &rmin, &rmax, &coord[n], &cgfld)
   }

   puts ("closing and reopening in modify mode")
   cg_close (fnum)

   cg_open (fname, CG_MODE_MODIFY, &fnum)
   pnum = 1

   /* Fill in missing coordinate data */
   puts("coordinates -> 2,4,6,8 ...")
   for (int k = 1 k < (int)num_particles k += 2) {
      cgsize_t rmin = k + 1
      cgsize_t rmax = k + 1
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateX", &rmin, &rmax, &coord[k], &cgcoord)
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateY", &rmin, &rmax, &coord[k], &cgcoord)
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateZ", &rmin, &rmax, &coord[k], &cgcoord)
   }

   /* Fill in missing particle field data */
   puts("field -> 2,4,6,8 ...")
   for (int n = 1 n < num_particles n += 2) {
       cgsize_t rmin = n + 1
       cgsize_t rmax = n + 1
       int cgfld
       cg_particle_field_partial_write(fnum, bnum, pnum, cgsol, CGNS_ENUMV(RealSingle), "Field", &rmin, &rmax, &coord[n], &cgfld)
   }

   puts ("closing and reopening in read mode")
   cg_close (fnum)

   cg_open (fname, CG_MODE_READ, &fnum)

   float* coord_read = (float*)malloc(sizeof(float)*num_particles)
   memset(coord_read, -1, sizeof(float)*num_particles)
   cgsize_t rmin = 1, rmax = num_particles

   /* Read all coordinate arrays and verify that they match the written values */
   cg_particle_coord_read(fnum, bnum, pnum, "CoordinateX", CGNS_ENUMV(RealSingle), &rmin, &rmax, coord_read)
   for(int i = 0 i < rmax - 1 ++i)
   {
      if(!compareValuesFloat(coord[i], coord_read[i]))
      {
         printf("Particle partial coordinate write (CoordinateX) failed\n")
         exit(1)
      }
   }

   memset(coord_read, -1, sizeof(float)*num_particles)
   cg_particle_coord_read(fnum, bnum, pnum, "CoordinateY", CGNS_ENUMV(RealSingle), &rmin, &rmax, coord_read)
   for(int i = 0 i < rmax - 1 ++i)
   {
      if(!compareValuesFloat(coord[i], coord_read[i]))
      {
         printf("Particle partial coordinate write (CoordinateY) failed\n")
         exit(1)
      }
   }

   memset(coord_read, -1, sizeof(float)*num_particles)
   cg_particle_coord_read(fnum, bnum, pnum, "CoordinateZ", CGNS_ENUMV(RealSingle), &rmin, &rmax, coord_read)
   for(int i = 0 i < rmax - 1 ++i)
   {
      if(!compareValuesFloat(coord[i], coord_read[i]))
      {
         printf("Particle partial coordinate write (CoordinateZ) failed\n")
         exit(1)
      }
   }

   free(coord_read)

   /* Verify that the field matches the written value as well */

   CGNS_ENUMT(DataType_t) type
   char field_name[32]
   cg_particle_field_info(fnum, bnum, pnum, 1, 1, &type, field_name)

   float* field = (float*)malloc(sizeof(float)*num_particles)
   cg_particle_field_read(fnum, bnum, pnum, 1, field_name, type, &rmin, &rmax, field)

   for(int i = 0 i < rmax - 1 ++i)
   {
      if(!compareValuesFloat(coord[i], field[i]))
      {
         printf("Particle partial field write failed\n")
         exit(1)
      }
   }

   free(field)
   free(coord)

   cg_close (fnum)
}

static void test_particle_bbox()
{
    int ier
    int fnum, bnum, pnum, cgcoord
    static char *fname = "particle_bbox.cgns"
    double bbox[2][3]

    cgsize_t num_particles = 10

    float* coord = (float*)malloc(sizeof(float)*num_particles)
    for (int n = 0 n < (int)num_particles n++)
        coord[n] = (float)n

    unlink (fname)
    printf ("creating CGNS file %s\n", fname)

    cg_configure(CG_CONFIG_ERROR, (void*) print_error)

    cg_open (fname, CG_MODE_WRITE, &fnum)
    cg_base_write (fnum, "Base", 3, 3, &bnum)

    /* write particle zone */

    puts ("writing particle zone")

    cg_particle_write (fnum, bnum, "ParticleZone", num_particles, &pnum)
    cg_particle_coord_write (fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateX", coord, &cgcoord)
    cg_particle_coord_write (fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateY", coord, &cgcoord)
    cg_particle_coord_write (fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateZ", coord, &cgcoord)

    puts ("closing and reopening in read mode")
    cg_close (fnum)

    /* read file */

    cg_open (fname, CG_MODE_READ, &fnum)
    bnum = pnum = 1

    bbox[0][0] = 1.0
    bbox[1][0] = -1.0
    bbox[0][1] = 1.0
    bbox[1][1] = -1.0
    bbox[0][2] = 1.0
    bbox[1][2] = -1.0
    /* check bounding box is not modified */
    ier = cg_particle_bounding_box_read(fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), bbox)
    if (ier != CG_NODE_NOT_FOUND)
        cg_error_exit()

    CHECK("Unmodified bounding box data when missing data in ParticleCoordinates node", compareValuesDouble(bbox[1][0], -1.0))
    CHECK("Unmodified bounding box data when missing data in ParticleCoordinates node", compareValuesDouble(bbox[0][0], 1.0))
    puts ("closing and reopening in modify mode")
    cg_close (fnum)

    cg_open (fname, CG_MODE_MODIFY, &fnum)
    bnum = pnum = 1

    bbox[0][0] = 0.0
    bbox[1][0] = (double)(num_particles -1)
    bbox[0][1] = 0.0
    bbox[1][1] = (double)(num_particles -1)
    bbox[0][2] = 0.0
    bbox[1][2] = (double)(num_particles -1)

    cg_particle_bounding_box_write (fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), bbox)

    puts ("closing and reopening in read mode")
    cg_close (fnum)

    cg_open (fname, CG_MODE_READ, &fnum)
    bnum = pnum = 1

    bbox[0][0] = 1.0
    bbox[1][0] = -1.0
    bbox[0][1] = 1.0
    bbox[1][1] = -1.0
    bbox[0][2] = 1.0
    bbox[1][2] = -1.0

    /* check that the bounding box is not modified */
    cg_particle_bounding_box_read(fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), bbox)

    CHECK("Read bounding box", compareValuesDouble(bbox[0][0], 0.0))
    CHECK("Read bounding box", compareValuesDouble(bbox[1][0], (double)(num_particles - 1)))

    /* check if the datatype conversion is also handled correctly */
    float bbox_float[2][3]
    bbox_float[0][0] = 1.0
    bbox_float[1][0] = -1.0
    bbox_float[0][1] = 1.0
    bbox_float[1][1] = -1.0
    bbox_float[0][2] = 1.0
    bbox_float[1][2] = -1.0
    cg_particle_bounding_box_read(fnum, bnum, pnum, 1, CGNS_ENUMV(RealSingle), bbox_float)

    CHECK("Read bounding box", compareValuesFloat(bbox_float[0][0], 0.0))
    CHECK("Read bounding box", compareValuesFloat(bbox_float[1][0], (float)(num_particles - 1)))

    puts ("closing and reopening to test limit cases")
    cg_close (fnum)

    cg_open (fname, CG_MODE_MODIFY, &fnum)
    bnum = pnum = 1

    /* writing integer type should fail */
    if (!cg_particle_bounding_box_write(fnum, bnum, pnum, 1, CGNS_ENUMV(Integer), bbox)){
       printf("writing Integer bounding box data failed to produce error\n")
       exit(1)
    }

    cgi_error("no CGNS error reported")  /* reset */

    /* writing empty bbox should do nothing */
    if (cg_particle_bounding_box_write(fnum, bnum, pnum, 1, CGNS_ENUMV(RealSingle), NULL)){
       printf("writing NULL bounding box raised an unexpected error\n")
       exit(1)
    }

    free(coord)

    puts ("closing file")
    cg_close (fnum)
}

static void test_particle_coord_io_and_ptset()
{
   int fnum, bnum, pnum, cgcoord
   static char *fname = "particle_coord_and_ptset_test.cgns"

   cgsize_t num_particles = 100

   float* coord = (float*)malloc(sizeof(float)*num_particles)
   for (int n = 0 n < (int)num_particles n++)
       coord[n] = (float)n

   unlink (fname)
   printf ("creating CGNS file %s\n", fname)

   cg_configure(CG_CONFIG_ERROR, (void*) print_error)

   cg_open (fname, CG_MODE_WRITE, &fnum)
   cg_base_write (fnum, "Base", 3, 3, &bnum)

   /* write particle zone */

   puts ("writing particle zone")
   cg_particle_write (fnum, bnum, "ParticleZone", num_particles, &pnum)

   /* write the coordinate node and use the DataArray_t routine to write out the actual data */
   puts ("writing coordinate nodes")

   cg_particle_coord_node_write(fnum, bnum, pnum, "ParticleCoordinates", &cgcoord)
   cg_goto(fnum, bnum, "ParticleZone_t", pnum, "ParticleCoordinates_t", cgcoord, "end")
   cg_array_write("CoordinateX", CGNS_ENUMV( RealSingle ), 1, &num_particles, coord)
   cg_array_write("CoordinateY", CGNS_ENUMV( RealSingle ), 1, &num_particles, coord)
   cg_array_write("CoordinateZ", CGNS_ENUMV( RealSingle ), 1, &num_particles, coord)

   int snum
   cgsize_t pnts[2] = {1, 10}
   cg_particle_sol_ptset_write(fnum, bnum, pnum, "Solution 1", CGNS_ENUMV(PointRange), 2, pnts, &snum)

   int field
   cg_particle_field_write(fnum, bnum, pnum, snum, CGNS_ENUMV( RealSingle ), "Field", coord, &field)


   cgsize_t* pnts_list = (cgsize_t*)malloc(sizeof(cgsize_t)*num_particles)
   for(cgsize_t n = 0 n < num_particles ++n) {
      pnts_list[n] = n+1
   }
   cg_particle_sol_ptset_write(fnum, bnum, pnum, "Solution 2", CGNS_ENUMV(PointList), num_particles, pnts_list, &snum)
   cg_particle_field_write(fnum, bnum, pnum, snum, CGNS_ENUMV( RealSingle ), "Field", coord, &field)

   free(pnts_list)

   puts ("closing and reopening in read mode")
   cg_close(fnum)

   cg_open(fname, CG_MODE_READ, &fnum)

   /* Verify that the coord count is correct */
   int ncoord_nodes
   cg_particle_ncoord_nodes(fnum, bnum, pnum, &ncoord_nodes)

   for(int i = 1 i <= ncoord_nodes ++i)
   {
      char coordname[32]
      cg_particle_coord_node_read(fnum, bnum, pnum, i, coordname)

      if(strcmp(coordname, "ParticleCoordinates"))
      {
         printf("Invalid coordinate name\n")
         exit(1)
      }
   }

   /* Read the PointRange solution and verify it too */
   snum = 1
   CGNS_ENUMT(PointSetType_t) ptset_type
   cgsize_t npnts
   cg_particle_sol_ptset_info(fnum, bnum, pnum, snum, &ptset_type, &npnts)
   CHECK("Particle solution point set type", ptset_type == CGNS_ENUMT(PointRange))
   CHECK("Particle solution point set size", npnts == 2)

   cgsize_t* pnts_read = (cgsize_t*)malloc(sizeof(cgsize_t)*npnts)
   cg_particle_sol_ptset_read(fnum, bnum, pnum, snum, pnts_read)

   cgsize_t sol_size
   cg_particle_sol_size(fnum, bnum, pnum, snum, &sol_size)
   if(sol_size != pnts_read[1] - pnts_read[0] + 1)
   {
      printf("Particle sol size mismatch\n")
      exit(1)
   }

   /* Read the field for this ptset and verify it */
   cgsize_t rmin = 1, rmax = pnts_read[1] - pnts_read[0] + 1
   float* data = (float*)malloc(sizeof(float)*sol_size)
   cg_particle_field_read(fnum, bnum, pnum, snum, "Field", CGNS_ENUMV(RealSingle), &rmin, &rmax, data)

   for(cgsize_t i = pnts_read[0], j = rmin i <= pnts_read[1] ++i, ++j)
   {
      if(!compareValuesFloat(coord[i-1], data[j-1]))
      {
         printf("Particle field data mismatch\n")
         exit(1)
      }
   }

   free(data)
   free(pnts_read)

   /* Read the PointList solution and verify it too */
   snum = 2
   cg_particle_sol_ptset_info(fnum, bnum, pnum, snum, &ptset_type, &npnts)

   CHECK("Particle solution point set type", ptset_type == CGNS_ENUMT(PointList))
   CHECK("Particle solution point set size", npnts == num_particles)

   pnts_read = (cgsize_t*)malloc(sizeof(cgsize_t)*npnts)
   cg_particle_sol_ptset_read(fnum, bnum, pnum, snum, pnts_read)
   cg_particle_sol_size(fnum, bnum, pnum, snum, &sol_size)
   if(sol_size != npnts)
   {
      printf("Particle sol size mismatch\n")
      exit(1)
   }

   /* Read the field for this ptset and verify it */
   rmin = 1
   rmax = npnts
   data = (float*)malloc(sizeof(float)*sol_size)
   cg_particle_field_read(fnum, bnum, pnum, snum, "Field", CGNS_ENUMV(RealSingle), &rmin, &rmax, data)

   for(cgsize_t i = 0 i < npnts ++i)
   {
      if(!compareValuesFloat(coord[pnts_read[i]-1], data[i]))
      {
         printf("Particle field data mismatch\n")
         exit(1)
      }
   }

   free(data)
   free(pnts_read)
   free(coord)

   cg_close(fnum)
}

#endif

PROGRAM main

  USE cgns_particle_test

  ! These tests should cover all serial MLL functions related to particles
  ! that were added as a part of CPEX 0046 (V4.5)

   CALL test_particle_io_main()
 !  CALL test_particle_io_partial()
 !  CALL test_particle_bbox()
 !  CALL test_particle_coord_io_and_ptset()

   WRITE(*,"(A)") "All particle tests completed successfully"

 END PROGRAM main
