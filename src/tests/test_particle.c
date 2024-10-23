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
      cgio_cleanup();
      exit(1);
      break;
   }
   default: // Shouldn't be possible as -1, 0, and 1 are the only error codes at the moment
   {
      printf("Unkown error code %d encountered. Aborting...\n", error_code);
      cgio_cleanup();
      exit(1);
   }
   }
}

int main ()
{
    int fnum, bnum, znum, snum, cnum;
    char *outfile = "test_particle.cgns";

    unlink (outfile);

    cg_configure(CG_CONFIG_ERROR, (void*) print_error);

    cg_open (outfile, CG_MODE_WRITE, &fnum);
    cg_base_write (fnum, "Base", 3, 3, &bnum);
    cg_goto(fnum, bnum, NULL);
    cg_dataclass_write(CGNS_ENUMV(NormalizedByDimensional));

    float exp[5];
    for (int n = 0; n < 5; n++)
        exp[n] = (float)0.0;
    exp[1] = (float)1.0;

    cgsize_t size[3];
    size[0] = 11;
    size[1] = 4;
    size[2] = 0;

    cg_zone_write (fnum, bnum, "Cells", size, Unstructured, &znum);
    cg_coord_write (fnum, bnum, znum, RealDouble, "CoordinateX", xc, &cnum);
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

    /* Write co-ordinates for the water particle */
    double water_xc[2] = {0., 0.01};
    double water_yc[2] = {0., 0.02};
    double water_zc[2] = {0., 0.01};

    cg_particle_coord_write(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateX", water_xc, &pcnum);
    cg_particle_coord_write(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateY", water_yc, &pcnum);
    cg_particle_coord_write(fnum, bnum, pnum, CGNS_ENUMV(RealDouble), "CoordinateZ", water_zc, &pcnum);

    cg_goto(fnum, bnum, "ParticleZone_t", 3, "ParticleCoordinates_t", 1, "end");
    cg_dataclass_write(CGNS_ENUMV(Dimensional));
    cg_units_write(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter), CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), CGNS_ENUMV(Degree));

    float exponents[5] = {0,1,0,0,0};
    for(int ii = 1; ii <= 3; ++ii)
    {
       cg_goto(fnum, bnum, "ParticleZone_t", 3, "ParticleCoordinates_t", 1, "DataArray_t", ii, "end");
       cg_exponents_write(CGNS_ENUMV(RealSingle), exponents);
    }

    /* Write particle solution */
    int psolnum; // particle solution index
    cg_particle_sol_write(fnum, bnum, pnum, "ParticleSolution", &psolnum);

    float radius[2] = {0.001, 0.005};
    float temperature[2] = {275, 298};

    int fieldnum;
    cg_particle_field_write(fnum, bnum, pnum, psolnum, CGNS_ENUMV(RealSingle), "Radius", radius, &fieldnum);
    cg_particle_field_write(fnum, bnum, pnum, psolnum, CGNS_ENUMV(RealSingle), "Temperature", temperature, &fieldnum);

    cg_close(fnum);

    /* Reopen file and verify that the particle data matches what we wrote in */

    cg_open (outfile, CG_MODE_READ, &fnum);

    cg_nbases(fnum, &bnum);

    int nparticles = 0;
    cg_nparticle_zones(fnum, bnum, &nparticles);

    if(nparticles != 3)
    {
       printf("Expected 3 ParticleZone_t nodes but found %d ParticleZone_t nodes instead\n", nparticles);
       cgio_cleanup();
       exit(1);
    }

    for(int iparticle = 1; iparticle <= nparticles; ++iparticle)
    {
       char name[32];
       cgsize_t size;
       cg_particle_read(fnum, bnum, iparticle, name, &size);

       if(strcmp(particle_names[iparticle - 1], name) != 0)
       {
          printf("Expected %s as the ParticleZone_t node name but found %s instead\n", particle_names[iparticle-1], name);
          cgio_cleanup();
          exit(1);
       }

       if(size != particle_sizes[iparticle-1])
       {
          printf("Expected %ld as the ParticleZone_t node size but found %ld instead\n", particle_sizes[iparticle-1], size);
          cgio_cleanup();
          exit(1);
       }


       int ncoords;
       cg_particle_ncoords(fnum, bnum, iparticle, &ncoords);
       if(ncoords != 3)
          continue;

       /* Since we wrote out co-ordinates for water alone, verify that they match */
       for(int jj = 0; jj <= ncoords; ++jj)
       {
          cgsize_t rmin = 1, rmax = size;
          double coord_x[size], coord_y[size], coord_z[size];
          cg_particle_coord_read(fnum, bnum, iparticle, "CoordinateX", RealDouble, &rmin, &rmax, coord_x);
          cg_particle_coord_read(fnum, bnum, iparticle, "CoordinateY", RealDouble, &rmin, &rmax, coord_y);
          cg_particle_coord_read(fnum, bnum, iparticle, "CoordinateZ", RealDouble, &rmin, &rmax, coord_z);

          for(int kk = 0; kk < size; ++kk)
          {
             if(coord_x[kk] != water_xc[kk] || coord_y[kk] != water_yc[kk] || coord_z[kk] != water_zc[kk])
             {
                printf("Invalid particle coordinate data - written value doesn't match with the read value\n");
                cgio_cleanup();
                exit(1);
             }
          }
       }

       /* Read solution data */
       int npsols;
       cg_particle_nsols(fnum, bnum, iparticle, &npsols);
       if(npsols != psolnum)
       {
          printf("For ParticleZone_t %d, expected %d ParticleSolution_t node(s) but found %d instead\n", iparticle, psolnum, npsols);
          cgio_cleanup();
          exit(1);
       }

       for(int isol = 1; isol <= npsols; ++isol)
       {
          int nfields;
          cg_particle_nfields(fnum, bnum, iparticle, isol, &nfields);
          if(nfields != fieldnum)
          {
             printf("For ParticleZone_t %d > ParticleSolution_t %d, expected %d DataArray_t node(s) but found %d instead\n", iparticle, psolnum, fieldnum, nfields);
             cgio_cleanup();
             exit(1);
          }

          for(int ifield = 1; ifield <= nfields; ++ifield)
          {
             CGNS_ENUMT(DataType_t) type;
             char name[32];
             cg_particle_field_info(fnum, bnum, iparticle, isol, ifield, &type, name);
             cgsize_t rmin = 1, rmax = size;
             int field;
             cg_particle_field_read(fnum, bnum, iparticle, isol, name, type, &rmin, &rmax, &field);
          }
       }
    }

    cg_close(fnum);

    printf("Particle test completed successfully\n");
}
