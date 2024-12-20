#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(_WIN32) && !defined(__NUTC__)
# include <io.h>
# define unlink _unlink
#else
# include <unistd.h>
#endif
#include "cgnslib.h"
#include "cgns_io.h"
#include "utils.h"

#define CHECK(L,B) if(!(B)){fprintf(stderr,"mismatch in %s\n",L);exit(1);}

void cgi_error(const char *format, ...);

static double xc[42] = {
    0.0, 2.0, 2.0, 0.0, 0.0, 2.0, 2.0, 0.0, 1.0, 1.0, 1.0,
    1.0, 2.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0, 1.0, 2.0, 1.0, 0.0,
    1.0, 1.0, 2.0, 1.0, 0.0, 1.0, 1.0,
    1.5, 0.5, 1.0, 1.5, 0.5, 1.5, 0.5,
    0.5, 1.5, 1.5, 0.5,
    1.0
};
static double yc[42] = {
    0.0, 0.0, 2.0, 2.0, 0.0, 0.0, 2.0, 2.0, 4.0, 4.0, 2.0,
    0.0, 1.0, 2.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0, 1.0, 2.0, 1.0,
    1.0, 0.0, 1.0, 2.0, 1.0, 1.0, 1.0,
    3.0, 3.0, 4.0, 3.0, 3.0, 3.0, 3.0,
    1.0, 1.0, 2.0, 2.0,
    3.0
};
static double zc[42] = {
    0.0, 0.0, 0.0, 0.0, 2.0, 2.0, 2.0, 2.0, 0.0, 2.0, 4.0,
    0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0,
    0.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0,
    0.0, 0.0, 1.0, 2.0, 2.0, 1.0, 1.0,
    3.0, 3.0, 3.0, 3.0,
    3.0
};

static int npoly = 16;
static cgsize_t poly[] = {1, 4, 3, 2,
                          2, 3, 7, 6,
                          3, 4, 8, 7,
                          1, 5, 8, 4,
                          1, 5, 6, 2,
                          5, 6, 7, 8,
                          3, 4, 9,
                          8, 7, 10,
                          3, 9, 10, 7,
                          4, 8, 10, 9,
                          5, 6, 11,
                          6, 7, 11,
                          7, 8, 11,
                          5, 11, 8,
                          7, 10, 11,
                          8, 11, 10};
static cgsize_t poly_offsets[] = {0, 4, 8, 12, 16,
                                  20, 24, 27,
                                  30, 34, 38, 41,
                                  44, 47, 50, 53, 56};

static int nface = 4;
static cgsize_t face[] = {1, 2, 3, 4, 5, 6,
                          -3, 7, 8, 9, 10,
                          -6, 11, 12, 13, 14,
                          -8, -13, 15, 16};
static cgsize_t face_offsets[] = {0, 6, 11, 16, 20};

static void print_error(int error_code, char *error_msg)
{
   switch(error_code)
   {
   case 0: // Warning
   {
      printf("Warning: %s\n", error_msg);
      break;
   }
   case 1: // Error
   {
      printf("Error: %s\n", error_msg);
      break;
   }
   case -1: // Fatal Error
   {
      printf("Fatal Error: %s\n", error_msg);
      cg_error_exit();
   }
   default: // Shouldn't be possible as -1, 0, and 1 are the only error codes at the moment
   {
      printf("Unknown error code %d encountered. Aborting...\n", error_code);
      cg_error_exit();
   }
   }
}

/* This tests MLL functions that handle reading and writing of core particle nodes i.e. ParticleZone_t,
 * ParticleCoordinates_t, ParticleSolution_t, ParticleIterativeData_t and particle fields i.e. DataArray_t */
static void test_particle_io_main()
{
    int fnum, bnum, znum, snum, cnum;
    char *outfile = "test_particle.cgns";

    unlink (outfile);
    printf ("creating CGNS file %s\n", outfile);

    cg_configure(CG_CONFIG_ERROR, (void*) print_error);

    cg_open (outfile, CG_MODE_WRITE, &fnum);
    cg_base_write (fnum, "Base", 3, 3, &bnum);
    cg_goto(fnum, bnum, NULL);
    cg_dataclass_write(CGNS_ENUMV(NormalizedByDimensional));

    /* Write BaseIterativeData_t so that we can write out ParticleIterativeData_t later on */
    double output_time[1] = {0};
    double iteration[1] = {1};
    cgsize_t length = 1;
    cg_goto(fnum, bnum, "end");
    cg_biter_write(fnum, bnum, "BaseIterativeData", 1);
    cg_gopath(fnum, "./BaseIterativeData");
    cg_array_write("TimeValues", CGNS_ENUMV(RealDouble), 1, &length, output_time);
    cg_array_write("IterationValues", CGNS_ENUMV(Integer), 1, &length, iteration);

    float exp[5];
    for (int n = 0; n < 5; n++)
        exp[n] = (float)0.0;
    exp[1] = (float)1.0;

    cgsize_t zone_size[3];
    zone_size[0] = 11;
    zone_size[1] = 4;
    zone_size[2] = 0;

    cg_zone_write (fnum, bnum, "Cells", zone_size, CGNS_ENUMV(Unstructured), &znum);
    cg_coord_write (fnum, bnum, znum, CGNS_ENUMV(RealDouble), "CoordinateX", xc, &cnum);
    cg_goto(fnum, bnum, "Zone_t", znum, "GridCoordinates", 0, "CoordinateX", 0, NULL);
    cg_exponents_write(CGNS_ENUMV(RealSingle), exp);
    cg_coord_write (fnum, bnum, znum, CGNS_ENUMV(RealDouble), "CoordinateY", yc, &cnum);
    cg_gopath(fnum, "../CoordinateY");
    cg_exponents_write(CGNS_ENUMV(RealSingle), exp);
    cg_coord_write (fnum, bnum, znum, CGNS_ENUMV(RealDouble), "CoordinateZ", zc, &cnum);
    cg_gopath(fnum, "../CoordinateZ");
    cg_exponents_write(CGNS_ENUMV(RealSingle), exp);

    cgsize_t ne = 0;

    /* NGON_n first so polyhedra face references are correct */
    cg_poly_section_write (fnum, bnum, znum, "NGON_n", CGNS_ENUMV(NGON_n), ne+1, ne+npoly, 0, poly, poly_offsets, &snum);
    ne += npoly;

    /* NFACE_n */
    cg_poly_section_write (fnum, bnum, znum, "NFACE_n", CGNS_ENUMV(NFACE_n), ne+1, ne+nface, 0, face, face_offsets, &snum);

    int pnum, pcnum;

    char particle_names[3][32];
    strcpy(particle_names[0], "IsoOctane");
    strcpy(particle_names[1], "Oil");
    strcpy(particle_names[2], "Water");

    cgsize_t particle_sizes[3] = {0, 0, 2};

    /* Create ParticleZone_t nodes */
    /* It is okay to define ParticleZone_t nodes with 0 particles
     * this implies that for the current solution time, no particles
     * of said type (IsoOctane and Oil) are in the Base */
    cg_particle_write(fnum, bnum, "IsoOctane", 0, &pnum); // 0 IsoOctane particles
    cg_particle_write(fnum, bnum, "Oil", 0, &pnum);  // 0 Oil particles
    cg_particle_write(fnum, bnum, "Water", 2, &pnum); // 2 Water particles

    /* Write coordinates for the water particle */
    double water_xc[2] = {0., 0.01};
    double water_yc[2] = {0., 0.02};
    double water_zc[2] = {0., 0.01};

    cg_particle_coord_write(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateX", water_xc, &pcnum);
    cg_particle_coord_write(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateY", water_yc, &pcnum);
    cg_particle_coord_write(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateZ", water_zc, &pcnum);

    cg_goto(fnum, bnum, "ParticleZone_t", 3, "ParticleCoordinates_t", 1, "end");
    cg_dataclass_write(CGNS_ENUMV(Dimensional));
    cg_units_write(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), CGNS_ENUMV(Degree));

    for(int ii = 1; ii <= 3; ++ii)
    {
       cg_goto(fnum, bnum, "ParticleZone_t", 3, "ParticleCoordinates_t", 1, "DataArray_t", ii, "end");
       cg_exponents_write(CGNS_ENUMV(RealSingle), exp);
    }

    /* Write particle solution */
    int psolnum; // particle solution index
    char ParticleSolutionName[32] = "ParticleSolution";

    cg_particle_sol_write(fnum, bnum, pnum, ParticleSolutionName, &psolnum);

    float radius[2] = {0.001, 0.005};
    float temperature[2] = {275, 298};

    int fieldnum;
    cg_particle_field_write(fnum, bnum, pnum, psolnum, CGNS_ENUMV(RealSingle), "Radius", radius, &fieldnum);
    cg_particle_field_write(fnum, bnum, pnum, psolnum, CGNS_ENUMV(RealSingle), "Temperature", temperature, &fieldnum);

    /* Write particle iterative data */
    cg_piter_write(fnum, bnum, pnum, "ParticleIterativeData");
    cg_goto(fnum, bnum, "ParticleZone_t", pnum, "end");
    cg_gopath(fnum, "./ParticleIterativeData");
    cgsize_t dim[2] = {32, 1};
    cg_array_write("ParticleSolutionPointers", CGNS_ENUMT(Character), 2, dim, ParticleSolutionName);

    /* Write particle governing equations and models */
    cg_goto(fnum, bnum, "ParticleZone_t", pnum, "end");
    cg_particle_equationset_write(3);
    cg_gopath(fnum, "./ParticleEquationSet");
    cg_particle_governing_write(CGNS_ENUMV(DEM));
    cg_particle_model_write("ParticleCollisionModel_t", CGNS_ENUMV(SoftSphere));
    cg_particle_model_write("ParticleBreakupModel_t", CGNS_ENUMV(RayleighTaylor));
    cg_particle_model_write("ParticleForceModel_t", CGNS_ENUMV(Gidaspow));
    cg_particle_model_write("ParticleWallInteractionModel_t", CGNS_ENUMV(BaiGosman));
    cg_particle_model_write("ParticlePhaseChangeModel_t", CGNS_ENUMV(Boil));

    cg_close(fnum);

    /* Reopen the file and verify that the particle data matches what we wrote in */

    cg_open (outfile, CG_MODE_READ, &fnum);

    cg_nbases(fnum, &bnum);

    int nparticles = 0;
    cg_nparticle_zones(fnum, bnum, &nparticles);

    if(nparticles != 3)
    {
       printf("Expected 3 ParticleZone_t nodes but found %d ParticleZone_t nodes instead\n", nparticles);
       cg_error_exit();
    }

    for(int iparticle = 1; iparticle <= nparticles; ++iparticle)
    {
       char pname[32];
       cgsize_t size;
       cg_particle_read(fnum, bnum, iparticle, pname, &size);

       double id;
       cg_particle_id(fnum, bnum, iparticle, &id);

       if(strcmp(particle_names[iparticle - 1], pname) != 0)
       {
          printf("Expected %s as the ParticleZone_t node name but found %s instead\n", particle_names[iparticle-1], pname);
          cg_error_exit();
       }

       if(size != particle_sizes[iparticle-1])
       {
          printf("Expected %d as the ParticleZone_t node size but found %d instead\n", (int)particle_sizes[iparticle-1], (int)size);
          cg_error_exit();
       }

       /* Check to see if we have coordinates */
       int ncoords;
       cg_particle_ncoords(fnum, bnum, iparticle, &ncoords);
       if(ncoords != 3)
          continue;

       /* Since we wrote out coordinates for water alone, verify that they match */
       char coordnames[3][32];
       CGNS_ENUMT(DataType_t) datatypes[3];
       for(int jj = 0; jj < ncoords; ++jj)
       {
          double coord_id;
          cg_particle_coord_id(fnum, bnum, iparticle, jj+1, &coord_id);
          cg_particle_coord_info(fnum, bnum, iparticle, jj+1, &datatypes[jj], coordnames[jj]);
       }

       cgsize_t rmin = 1, rmax = size;
       double* coord_x = (double*)malloc(size*sizeof(double));
       double* coord_y = (double*)malloc(size*sizeof(double));
       double* coord_z = (double*)malloc(size*sizeof(double));

       cg_particle_coord_read(fnum, bnum, iparticle, coordnames[0], datatypes[0], &rmin, &rmax, coord_x);
       cg_particle_coord_read(fnum, bnum, iparticle, coordnames[1], datatypes[1], &rmin, &rmax, coord_y);
       cg_particle_coord_read(fnum, bnum, iparticle, coordnames[2], datatypes[2], &rmin, &rmax, coord_z);

       for(int kk = 0; kk < size; ++kk)
       {
          if(compareValuesFloat(coord_x[kk], water_xc[kk]) == 0 ||
                compareValuesFloat(coord_y[kk], water_yc[kk]) == 0 ||
                compareValuesFloat(coord_z[kk], water_zc[kk]) == 0)
          {
             printf("Invalid particle coordinate data - written value doesn't match the read value\n");
             cg_error_exit();
          }
       }

       free(coord_x);
       free(coord_y);
       free(coord_z);

       cg_goto(fnum, bnum, "ParticleZone_t", iparticle, "end");

       int eq_dim, gov_flag, coll, breakup, force, wall_inter, phase_change;
       cg_particle_equationset_read(&eq_dim, &gov_flag, &coll, &breakup, &force, &wall_inter, &phase_change);

       if(eq_dim != 3)
       {
          printf("Expected particle equation dimension of 3 but found %d instead\n", eq_dim);
          exit(1);
       }

       if(gov_flag != 1)
       {
          printf("Particle governing equations cannot be found\n");
          exit(1);
       }
       else
       {
          cg_gopath(fnum, "./ParticleEquationSet");
          CGNS_ENUMT(ParticleGoverningEquationsType_t) petype;
          cg_particle_governing_read(&petype);
          if(petype != (int)CGNS_ENUMV(DEM))
          {
             printf("Invalid particle governing equation type\n");
             exit(1);
          }
       }

       /* Read solution data */
       int npsols;
       cg_particle_nsols(fnum, bnum, iparticle, &npsols);
       if(npsols != psolnum)
       {
          printf("For ParticleZone_t %d, expected %d ParticleSolution_t node(s) but found %d instead\n", iparticle, psolnum, npsols);
          cg_error_exit();
       }

       for(int isol = 1; isol <= npsols; ++isol)
       {
          double sol_id;
          cg_particle_sol_id(fnum, bnum, iparticle, isol, &sol_id);

          int nfields;
          cg_particle_nfields(fnum, bnum, iparticle, isol, &nfields);
          if(nfields != fieldnum)
          {
             printf("For ParticleZone_t %d > ParticleSolution_t %d, expected %d DataArray_t node(s) but found %d instead\n", iparticle, psolnum, fieldnum, nfields);
             cg_error_exit();
          }

          for(int ifield = 1; ifield <= nfields; ++ifield)
          {
             double field_id;
             cg_particle_field_id(fnum, bnum, iparticle, isol, ifield, &field_id);

             CGNS_ENUMT(DataType_t) type;
             char fname[32];
             cg_particle_field_info(fnum, bnum, iparticle, isol, ifield, &type, fname);

             rmin = 1;
             rmax = size;
             float* field = (float*)malloc(size*sizeof(float));
             cg_particle_field_read(fnum, bnum, iparticle, isol, fname, type, &rmin, &rmax, field);
             if(ifield == 1) {
               if( compareValuesFloat(field[0], radius[0]) == 0 ) {
                  printf("Invalid particle field radius data - written value doesn't match the read value\n");
                  cg_error_exit();
               }
             } else {
               if( compareValuesFloat(field[0], temperature[0]) == 0 ) {
                 printf("Invalid particle field temperature data - written value doesn't match the read value\n");
                 cg_error_exit();
               }
             }
             free(field);
          }
       }
    }
    cg_close(fnum);
}

// Test partial write routines
static void test_particle_io_partial()
{
   int fnum, bnum, pnum, cgcoord, cgsol;
   static char *fname = "particle_partial_test.cgns";

   cgsize_t num_particles = 100;

   float* coord = (float*)malloc(sizeof(float)*num_particles);
   for (int n = 0; n < num_particles; n++)
       coord[n] = (float)n;

   unlink (fname);
   printf ("creating CGNS file %s\n", fname);

   cg_configure(CG_CONFIG_ERROR, (void*) print_error);

   cg_open (fname, CG_MODE_WRITE, &fnum);
   cg_base_write (fnum, "Base", 3, 3, &bnum);

   /* write particle zone */

   puts ("writing particle zone");

   cg_particle_write (fnum, bnum, "ParticleZone", num_particles, &pnum);

   /* write the coordinates of every other particle */
   puts("coordinates -> 1,3,5,7 ...");
   for (int k = 0; k < (int)num_particles; k += 2) {
      cgsize_t rmin = k + 1;
      cgsize_t rmax = k + 1;
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateX", &rmin, &rmax, &coord[k], &cgcoord);
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateY", &rmin, &rmax, &coord[k], &cgcoord);
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateZ", &rmin, &rmax, &coord[k], &cgcoord);
   }

   /* write every other solution value */

   if (cg_particle_sol_write(fnum, bnum, pnum, "Solution", &cgsol))
       cg_error_exit();

   puts("field -> 1,3,5,7 ...");
   for (int n = 0; n < (int)num_particles; n += 2) {
       cgsize_t rmin = n + 1;
       cgsize_t rmax = n + 1;
       int cgfld;
       cg_particle_field_partial_write(fnum, bnum, pnum, cgsol, CGNS_ENUMV(RealSingle), "Field", &rmin, &rmax, &coord[n], &cgfld);
   }

   puts ("closing and reopening in modify mode");
   cg_close (fnum);

   cg_open (fname, CG_MODE_MODIFY, &fnum);
   pnum = 1;

   /* Fill in missing coordinate data */
   puts("coordinates -> 2,4,6,8 ...");
   for (int k = 1; k < (int)num_particles; k += 2) {
      cgsize_t rmin = k + 1;
      cgsize_t rmax = k + 1;
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateX", &rmin, &rmax, &coord[k], &cgcoord);
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateY", &rmin, &rmax, &coord[k], &cgcoord);
      cg_particle_coord_partial_write(fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateZ", &rmin, &rmax, &coord[k], &cgcoord);
   }

   /* Fill in missing particle field data */
   puts("field -> 2,4,6,8 ...");
   for (int n = 1; n < num_particles; n += 2) {
       cgsize_t rmin = n + 1;
       cgsize_t rmax = n + 1;
       int cgfld;
       cg_particle_field_partial_write(fnum, bnum, pnum, cgsol, CGNS_ENUMV(RealSingle), "Field", &rmin, &rmax, &coord[n], &cgfld);
   }

   puts ("closing and reopening in read mode");
   cg_close (fnum);

   cg_open (fname, CG_MODE_READ, &fnum);

   float* coord_read = (float*)malloc(sizeof(float)*num_particles);
   memset(coord_read, -1, sizeof(float)*num_particles);
   cgsize_t rmin = 1, rmax = num_particles;

   /* Read all coordinate arrays and verify that they match the written values */
   cg_particle_coord_read(fnum, bnum, pnum, "CoordinateX", CGNS_ENUMV(RealSingle), &rmin, &rmax, coord_read);
   for(int i = 0; i < rmax - 1; ++i)
   {
      if(!compareValuesFloat(coord[i], coord_read[i]))
      {
         printf("Particle partial coordinate write (CoordinateX) failed\n");
         exit(1);
      }
   }

   memset(coord_read, -1, sizeof(float)*num_particles);
   cg_particle_coord_read(fnum, bnum, pnum, "CoordinateY", CGNS_ENUMV(RealSingle), &rmin, &rmax, coord_read);
   for(int i = 0; i < rmax - 1; ++i)
   {
      if(!compareValuesFloat(coord[i], coord_read[i]))
      {
         printf("Particle partial coordinate write (CoordinateY) failed\n");
         exit(1);
      }
   }

   memset(coord_read, -1, sizeof(float)*num_particles);
   cg_particle_coord_read(fnum, bnum, pnum, "CoordinateZ", CGNS_ENUMV(RealSingle), &rmin, &rmax, coord_read);
   for(int i = 0; i < rmax - 1; ++i)
   {
      if(!compareValuesFloat(coord[i], coord_read[i]))
      {
         printf("Particle partial coordinate write (CoordinateZ) failed\n");
         exit(1);
      }
   }

   free(coord_read);

   /* Verify that the field matches the written value as well */

   CGNS_ENUMT(DataType_t) type;
   char field_name[32];
   cg_particle_field_info(fnum, bnum, pnum, 1, 1, &type, field_name);

   float* field = (float*)malloc(sizeof(float)*num_particles);
   cg_particle_field_read(fnum, bnum, pnum, 1, field_name, type, &rmin, &rmax, field);

   for(int i = 0; i < rmax - 1; ++i)
   {
      if(!compareValuesFloat(coord[i], field[i]))
      {
         printf("Particle partial field write failed\n");
         exit(1);
      }
   }

   free(field);
   free(coord);

   cg_close (fnum);
}

static void test_particle_bbox()
{
    int ier;
    int fnum, bnum, pnum, cgcoord;
    static char *fname = "particle_bbox.cgns";
    double bbox[2][3];

    cgsize_t num_particles = 10;

    float* coord = (float*)malloc(sizeof(float)*num_particles);
    for (int n = 0; n < (int)num_particles; n++)
        coord[n] = (float)n;

    unlink (fname);
    printf ("creating CGNS file %s\n", fname);

    cg_configure(CG_CONFIG_ERROR, (void*) print_error);

    cg_open (fname, CG_MODE_WRITE, &fnum);
    cg_base_write (fnum, "Base", 3, 3, &bnum);

    /* write particle zone */

    puts ("writing particle zone");

    cg_particle_write (fnum, bnum, "ParticleZone", num_particles, &pnum);
    cg_particle_coord_write (fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateX", coord, &cgcoord);
    cg_particle_coord_write (fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateY", coord, &cgcoord);
    cg_particle_coord_write (fnum, bnum, pnum, CGNS_ENUMV(RealSingle), "CoordinateZ", coord, &cgcoord);

    puts ("closing and reopening in read mode");
    cg_close (fnum);
    /* read file */

    cg_open (fname, CG_MODE_READ, &fnum);
    bnum = pnum = 1;

    bbox[0][0] = 1.0;
    bbox[1][0] = -1.0;
    bbox[0][1] = 1.0;
    bbox[1][1] = -1.0;
    bbox[0][2] = 1.0;
    bbox[1][2] = -1.0;
    /* check bounding box is not modified */
    ier = cg_particle_bounding_box_read(fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), bbox);
    if (ier != CG_NODE_NOT_FOUND)
        cg_error_exit();

    CHECK("Unmodified bounding box data when missing data in ParticleCoordinates node", compareValuesDouble(bbox[1][0], -1.0));
    CHECK("Unmodified bounding box data when missing data in ParticleCoordinates node", compareValuesDouble(bbox[0][0], 1.0));
    puts ("closing and reopening in modify mode");
    cg_close (fnum);

    cg_open (fname, CG_MODE_MODIFY, &fnum);
    bnum = pnum = 1;

    bbox[0][0] = 0.0;
    bbox[1][0] = (double)(num_particles -1);
    bbox[0][1] = 0.0;
    bbox[1][1] = (double)(num_particles -1);
    bbox[0][2] = 0.0;
    bbox[1][2] = (double)(num_particles -1);

    cg_particle_bounding_box_write (fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), bbox);

    puts ("closing and reopening in read mode");
    cg_close (fnum);

    cg_open (fname, CG_MODE_READ, &fnum);
    bnum = pnum = 1;

    bbox[0][0] = 1.0;
    bbox[1][0] = -1.0;
    bbox[0][1] = 1.0;
    bbox[1][1] = -1.0;
    bbox[0][2] = 1.0;
    bbox[1][2] = -1.0;

    /* check that the bounding box is not modified */
    cg_particle_bounding_box_read(fnum, bnum, pnum, 1, CGNS_ENUMV(RealDouble), bbox);

    CHECK("Read bounding box", compareValuesDouble(bbox[0][0], 0.0));
    CHECK("Read bounding box", compareValuesDouble(bbox[1][0], (double)(num_particles - 1)));

    /* check if the datatype conversion is also handled correctly */
    float bbox_float[2][3];
    bbox_float[0][0] = 1.0;
    bbox_float[1][0] = -1.0;
    bbox_float[0][1] = 1.0;
    bbox_float[1][1] = -1.0;
    bbox_float[0][2] = 1.0;
    bbox_float[1][2] = -1.0;
    cg_particle_bounding_box_read(fnum, bnum, pnum, 1, CGNS_ENUMV(RealSingle), bbox_float);

    CHECK("Read bounding box", compareValuesFloat(bbox_float[0][0], 0.0));
    CHECK("Read bounding box", compareValuesFloat(bbox_float[1][0], (float)(num_particles - 1)));

    puts ("closing and reopening to test limit cases");
    cg_close (fnum);

    cg_open (fname, CG_MODE_MODIFY, &fnum);
    bnum = pnum = 1;

    /* writing integer type should fail */
    if (!cg_particle_bounding_box_write(fnum, bnum, pnum, 1, CGNS_ENUMV(Integer), bbox)){
       printf("writing Integer bounding box data failed to produce error\n");
       exit(1);
    }

    cgi_error("no CGNS error reported");  /* reset */

    /* writing empty bbox should do nothing */
    if (cg_particle_bounding_box_write(fnum, bnum, pnum, 1, CGNS_ENUMV(RealSingle), NULL)){
       printf("writing NULL bounding box raised an unexpected error\n");
       exit(1);
    }

    free(coord);

    puts ("closing file");
    cg_close (fnum);
}

static void test_particle_coord_io_and_ptset()
{
   int fnum, bnum, pnum, cgcoord;
   static char *fname = "particle_coord_and_ptset_test.cgns";

   cgsize_t num_particles = 100;

   float* coord = (float*)malloc(sizeof(float)*num_particles);
   for (int n = 0; n < (int)num_particles; n++)
       coord[n] = (float)n;

   unlink (fname);
   printf ("creating CGNS file %s\n", fname);

   cg_configure(CG_CONFIG_ERROR, (void*) print_error);

   cg_open (fname, CG_MODE_WRITE, &fnum);
   cg_base_write (fnum, "Base", 3, 3, &bnum);

   /* write particle zone */

   puts ("writing particle zone");
   cg_particle_write (fnum, bnum, "ParticleZone", num_particles, &pnum);

   /* write the coordinate node and use the DataArray_t routine to write out the actual data */
   puts ("writing coordinate nodes");

   cg_particle_coord_node_write(fnum, bnum, pnum, "ParticleCoordinates", &cgcoord);
   cg_goto(fnum, bnum, "ParticleZone_t", pnum, "ParticleCoordinates_t", cgcoord, "end");
   cg_array_write("CoordinateX", CGNS_ENUMV( RealSingle ), 1, &num_particles, coord);
   cg_array_write("CoordinateY", CGNS_ENUMV( RealSingle ), 1, &num_particles, coord);
   cg_array_write("CoordinateZ", CGNS_ENUMV( RealSingle ), 1, &num_particles, coord);

   int snum;
   cgsize_t pnts[2] = {1, 10};
   cg_particle_sol_ptset_write(fnum, bnum, pnum, "Solution 1", CGNS_ENUMV(PointRange), 2, pnts, &snum);

   int field;
   cg_particle_field_write(fnum, bnum, pnum, snum, CGNS_ENUMV( RealSingle ), "Field", coord, &field);


   cgsize_t* pnts_list = (cgsize_t*)malloc(sizeof(cgsize_t)*num_particles);
   for(cgsize_t n = 0; n < num_particles; ++n) {
      pnts_list[n] = n+1;
   }
   cg_particle_sol_ptset_write(fnum, bnum, pnum, "Solution 2", CGNS_ENUMV(PointList), num_particles, pnts_list, &snum);
   cg_particle_field_write(fnum, bnum, pnum, snum, CGNS_ENUMV( RealSingle ), "Field", coord, &field);

   free(pnts_list);

   puts ("closing and reopening in read mode");
   cg_close(fnum);

   cg_open(fname, CG_MODE_READ, &fnum);

   /* Verify that the coord count is correct */
   int ncoord_nodes;
   cg_particle_ncoord_nodes(fnum, bnum, pnum, &ncoord_nodes);

   for(int i = 1; i <= ncoord_nodes; ++i)
   {
      char coordname[32];
      cg_particle_coord_node_read(fnum, bnum, pnum, i, coordname);

      if(strcmp(coordname, "ParticleCoordinates"))
      {
         printf("Invalid coordinate name\n");
         exit(1);
      }
   }

   /* Read the PointRange solution and verify it too */
   snum = 1;
   CGNS_ENUMT(PointSetType_t) ptset_type;
   cgsize_t npnts;
   cg_particle_sol_ptset_info(fnum, bnum, pnum, snum, &ptset_type, &npnts);
   CHECK("Particle solution point set type", ptset_type == CGNS_ENUMT(PointRange));
   CHECK("Particle solution point set size", npnts == 2);

   cgsize_t* pnts_read = (cgsize_t*)malloc(sizeof(cgsize_t)*npnts);
   cg_particle_sol_ptset_read(fnum, bnum, pnum, snum, pnts_read);

   cgsize_t sol_size;
   cg_particle_sol_size(fnum, bnum, pnum, snum, &sol_size);
   if(sol_size != pnts_read[1] - pnts_read[0] + 1)
   {
      printf("Particle sol size mismatch\n");
      exit(1);
   }

   /* Read the field for this ptset and verify it */
   cgsize_t rmin = 1, rmax = pnts_read[1] - pnts_read[0] + 1;
   float* data = (float*)malloc(sizeof(float)*sol_size);
   cg_particle_field_read(fnum, bnum, pnum, snum, "Field", CGNS_ENUMV(RealSingle), &rmin, &rmax, data);

   for(cgsize_t i = pnts_read[0], j = rmin; i <= pnts_read[1]; ++i, ++j)
   {
      if(!compareValuesFloat(coord[i-1], data[j-1]))
      {
         printf("Particle field data mismatch\n");
         exit(1);
      }
   }

   free(data);
   free(pnts_read);

   /* Read the PointList solution and verify it too */
   snum = 2;
   cg_particle_sol_ptset_info(fnum, bnum, pnum, snum, &ptset_type, &npnts);

   CHECK("Particle solution point set type", ptset_type == CGNS_ENUMT(PointList));
   CHECK("Particle solution point set size", npnts == num_particles);

   pnts_read = (cgsize_t*)malloc(sizeof(cgsize_t)*npnts);
   cg_particle_sol_ptset_read(fnum, bnum, pnum, snum, pnts_read);
   cg_particle_sol_size(fnum, bnum, pnum, snum, &sol_size);
   if(sol_size != npnts)
   {
      printf("Particle sol size mismatch\n");
      exit(1);
   }

   /* Read the field for this ptset and verify it */
   rmin = 1;
   rmax = npnts;
   data = (float*)malloc(sizeof(float)*sol_size);
   cg_particle_field_read(fnum, bnum, pnum, snum, "Field", CGNS_ENUMV(RealSingle), &rmin, &rmax, data);

   for(cgsize_t i = 0; i < npnts; ++i)
   {
      if(!compareValuesFloat(coord[pnts_read[i]-1], data[i]))
      {
         printf("Particle field data mismatch\n");
         exit(1);
      }
   }

   free(data);
   free(pnts_read);
   free(coord);

   cg_close(fnum);
}

int main()
{
   /* These tests should cover all serial MLL functions related to particles
    * that were added as a part of CPEX 0046 (V4.5) */
   test_particle_io_main();
   test_particle_io_partial();
   test_particle_bbox();
   test_particle_coord_io_and_ptset();

   printf("All particle tests completed successfully\n");
   return(0);
}
