#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#if defined(_WIN32) && !defined(__NUTC__)
# include <io.h>
# define unlink _unlink
#else
# include <unistd.h>
#endif
#include "cgnslib.h"

#define NUM_SIDE 5

#define CHECK(L,B) if(!(B)){fprintf(stderr,"mismatch in %s\n",L);exit(1);}

cgsize_t sizes[9], range[6];

float xcoord[NUM_SIDE*NUM_SIDE*NUM_SIDE];
float ycoord[NUM_SIDE*NUM_SIDE*NUM_SIDE];
float zcoord[NUM_SIDE*NUM_SIDE*NUM_SIDE];

void error_exit (char *where)
{
    fprintf (stderr, "ERROR:%s:%s\n", where, cg_get_error());
    exit (1);
}

int main ()
{
    int i, j, k, n, nfam, nnames;
    int cgfile, cgbase, cgzone, cgfam, cgcoord, cgbc, cgsr;
    float exp[5];
    char name[33], outfile[33];
    char tname[33];
#ifdef CG_BUILD_BASESCOPE
    char tfamily[66],family[66];
#else
    char tfamily[33],family[33];
#endif

    strcpy (outfile, "multifam.cgns");

    for (n = 0; n < 3; n++) {
        sizes[n]   = NUM_SIDE;
        sizes[n+3] = NUM_SIDE - 1;
        sizes[n+6] = 0;
    }
    for (n = 0, k = 0; k < NUM_SIDE; k++) {
        for (j = 0; j < NUM_SIDE; j++) {
            for (i = 0; i < NUM_SIDE; i++, n++) {
                xcoord[n] = (float)i;
                ycoord[n] = (float)j;
                zcoord[n] = (float)k;
            }
        }
    }

    unlink(outfile);
    if (cg_open(outfile, CG_MODE_WRITE, &cgfile))
        error_exit("cg_open");

    if (cg_base_write(cgfile, "Structured", 3, 3, &cgbase) ||
        cg_gopath(cgfile, "/Structured") ||
        cg_dataclass_write(CGNS_ENUMV(Dimensional)) ||
        cg_units_write(CGNS_ENUMV(Kilogram), CGNS_ENUMV(Meter),
            CGNS_ENUMV(Second), CGNS_ENUMV(Kelvin), CGNS_ENUMV(Radian)))
        error_exit("base write");

    for (i = 1; i <= 4; i++) {
        sprintf(family, "TopFamily%d", i);
        if (cg_family_write(cgfile, cgbase, family, &cgfam))
            error_exit("family write");
        for (j = 1; j <= 4; j++) {
            sprintf(name, "SubFamily%d", j);
            sprintf(family, "Family%d", j);
            if (cg_family_name_write(cgfile, cgbase, cgfam,
                    name, family))
                error_exit("family name write");
        }
    }

    for (n = 0; n < 5; n++)
        exp[n] = (float)0;
    exp[1] = (float)1;
    if (cg_zone_write(cgfile, cgbase, "Zone", sizes,
            CGNS_ENUMV(Structured), &cgzone))
        error_exit("zone write");
    if (cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
            "CoordinateX", xcoord, &cgcoord) ||
        cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
            "CoordinateY", ycoord, &cgcoord) ||
        cg_coord_write(cgfile, cgbase, cgzone, CGNS_ENUMV(RealSingle),
            "CoordinateZ", zcoord, &cgcoord) ||
        cg_goto(cgfile, cgbase, "Zone_t", cgzone, "GridCoordinates", 0,
            "CoordinateX", 0, NULL) ||
        cg_exponents_write(CGNS_ENUMV(RealSingle), exp) ||
        cg_gopath(cgfile, "../CoordinateY") ||
        cg_exponents_write(CGNS_ENUMV(RealSingle), exp) ||
        cg_gopath(cgfile, "../CoordinateZ") ||
        cg_exponents_write(CGNS_ENUMV(RealSingle), exp))
        error_exit("coordinate write");

    if (cg_goto(cgfile, cgbase, "Zone", 0, NULL))
        error_exit("go to zone");
    if (cg_famname_write("TopFamily1"))
        error_exit("zone family write");
    for (j = 1; j <= 3; j++) {
        sprintf(name, "ZoneFamily%d", j);
        sprintf(family, "Family%d", j);
        if (cg_multifam_write(name, family))
            error_exit("zone multifam write");
    }

    for (n = 0; n < 3; n++) {
        range[n] = 1;
        range[n+3] = NUM_SIDE;
    }
    range[5] = 1;
    if (cg_boco_write(cgfile, cgbase, 1, "Inflow", CGNS_ENUMV(BCInflow),
            CGNS_ENUMV(PointRange), 2, range, &cgbc))
        error_exit("inflow boco");
    if (cg_goto(cgfile, cgbase, "Zone", 0, "ZoneBC", 0, "Inflow", 0, NULL))
        error_exit("go to BC");
    if (cg_famname_write("TopFamily2"))
        error_exit("zone family write");
#ifdef CG_BUILD_BASESCOPE
    if (cg_multifam_write("F1","BASE/TopFamily2"))
      cg_error_print();
    if (cg_multifam_write("F2","BaseWithAQuiteLongNameToTestSize/TopFamily2"))
      cg_error_print();
    if (cg_multifam_write("F3","B/FamilyNameHasExactly32Characters"))
      cg_error_print();
    if (cg_multifam_write("F4","BaseWithAQuiteLongNameToTestSize/FamilyNameHasExactly32Characters"))
      cg_error_print();
#endif
    for (j = 1; j <= 3; j++) {
        sprintf(name, "BCFamily%d", j);
        sprintf(family, "Family%d", j);
        if (cg_multifam_write(name, family))
            error_exit("BC multifam write");
    }

    if (cg_subreg_bcname_write(cgfile, cgbase, cgzone, "SubRegion",
            2, "Inflow", &cgsr))
        error_exit("subregion write");
    if (cg_goto(cgfile, cgbase, "Zone", 0, "SubRegion", 0, NULL))
        error_exit("go to subregion");
    if (cg_famname_write("TopFamily3"))
        error_exit("subregion family write");
    for (j = 1; j <= 3; j++) {
        sprintf(name, "SubRegionFamily%d", j);
        sprintf(family, "Family%d", j);
        if (cg_multifam_write(name, family))
            error_exit("subregion multifam write");
    }

    if (cg_goto(cgfile, cgbase, "Zone", 0, NULL))
        error_exit("go to zone");
    if (cg_user_data_write("UserData"))
        error_exit("user data write");
    if (cg_goto(cgfile, cgbase, "Zone", 0, "UserData", 0, NULL))
        error_exit("go to user data");
    if (cg_famname_write("TopFamily4"))
        error_exit("user data family write");
    for (j = 1; j <= 3; j++) {
        sprintf(name, "UserDataFamily%d", j);
        sprintf(family, "Family%d", j);
        if (cg_multifam_write(name, family))
            error_exit("user data multifam write");
    }

    if (cg_close(cgfile)) error_exit("cg_close");
    if (cg_open(outfile, CG_MODE_MODIFY, &cgfile)) error_exit("cg_open");

    cgbase = cgzone = 1;
    if (cg_nfamilies(cgfile, cgbase, &nfam))
        error_exit("number of families");
    CHECK("cg_nfamilies", nfam == 4);

    for (i = 1; i <= 4; i++) {
        sprintf(tfamily, "TopFamily%d", i);
        if (cg_family_read(cgfile, cgbase, i, family, &j, &k))
            error_exit("family read");
        CHECK("family", 0 == strcmp(family, tfamily));
        if (cg_nfamily_names(cgfile, cgbase, i, &nnames))
            error_exit("number of family names");
        CHECK("number of family names", nnames == 4);
        for (j = 1; j <= 4; j++) {
            sprintf(tname, "SubFamily%d", j);
            sprintf(tfamily, "Family%d", j);
            if (cg_family_name_read(cgfile, cgbase, i, j,
                    name, family))
                error_exit("family name read");
            CHECK("node name", 0 == strcmp(name, tname));
            CHECK("family name", 0 == strcmp(family, tfamily));
        }
        if (cg_goto(cgfile, cgbase, "Family_t", i, NULL))
            error_exit("goto topfamily");
        sprintf(tname, "SubFamily%d", i);
        if (cg_delete_node(tname)) error_exit("delete SubFamily");
        if (cg_nfamily_names(cgfile, cgbase, i, &nnames))
            error_exit("number of family names after delete");
        CHECK("number of family names after delete", nnames == 3);
    }

    if (cg_goto(cgfile, cgbase, "Zone", 0, NULL))
        error_exit("go to zone");
    if (cg_nmultifam(&nnames)) error_exit("zone nmultifam");
    CHECK("zone nmultifam", nnames == 3);
    for (j = 1; j <= 3; j++) {
        sprintf(tname, "ZoneFamily%d", j);
        sprintf(tfamily, "Family%d", j);
        if (cg_multifam_read(j, name, family))
            error_exit("zone multifam read");
        CHECK("node name", 0 == strcmp(name, tname));
        CHECK("family name", 0 == strcmp(family, tfamily));
    }
    if (cg_delete_node("ZoneFamily1"))
        error_exit("delete ZoneFamily1");
    if (cg_nmultifam(&nnames)) error_exit("zone nmultifam after delete");
    CHECK("zone nmultifam after delete", nnames == 2);

    if (cg_goto(cgfile, cgbase, "Zone", 0, "ZoneBC", 0, "Inflow", 0, NULL))
        error_exit("go to BC");
    if (cg_nmultifam(&nnames)) error_exit("BC nmultifam");
#ifdef CG_BUILD_BASESCOPE
    CHECK("BC nmultifam", nnames == 7);
#else
    CHECK("BC nmultifam", nnames == 3);
#endif
    for (j = 1; j <= 3; j++) {
        sprintf(tname, "BCFamily%d", j);
        sprintf(tfamily, "Family%d", j);
        if (cg_multifam_read(j, name, family))
            error_exit("BC multifam read");
#ifndef CG_BUILD_BASESCOPE
        CHECK("node name", 0 == strcmp(name, tname));
        CHECK("family name", 0 == strcmp(family, tfamily));
#endif
    }
    if (cg_delete_node("BCFamily1"))
        error_exit("delete BCFamily1");
    if (cg_nmultifam(&nnames)) error_exit("BC nmultifam after delete");
#ifdef CG_BUILD_BASESCOPE
    CHECK("BC nmultifam after delete", nnames == 6);
#else
    CHECK("BC nmultifam after delete", nnames == 2);
#endif

    if (cg_goto(cgfile, cgbase, "Zone", 0, "SubRegion", 0, NULL))
        error_exit("go to subregion");
    if (cg_nmultifam(&nnames)) error_exit("subreg nmultifam");
    CHECK("subreg nmultifam", nnames == 3);
    for (j = 1; j <= 3; j++) {
        sprintf(tname, "SubRegionFamily%d", j);
        sprintf(tfamily, "Family%d", j);
        if (cg_multifam_read(j, name, family))
            error_exit("subreg multifam read");
        CHECK("node name", 0 == strcmp(name, tname));
        CHECK("family name", 0 == strcmp(family, tfamily));
    }
    if (cg_delete_node("SubRegionFamily1"))
        error_exit("delete SubRegionFamily1");
    if (cg_nmultifam(&nnames)) error_exit("subreg nmultifam after delete");
    CHECK("subreg nmultifam after delete", nnames == 2);

    if (cg_goto(cgfile, cgbase, "Zone", 0, "UserData", 0, NULL))
        error_exit("go to user data");
    if (cg_nmultifam(&nnames)) error_exit("user data nmultifam");
    CHECK("user data nmultifam", nnames == 3);
    for (j = 1; j <= 3; j++) {
        sprintf(tname, "UserDataFamily%d", j);
        sprintf(tfamily, "Family%d", j);
        if (cg_multifam_read(j, name, family))
            error_exit("user data multifam read");
        CHECK("node name", 0 == strcmp(name, tname));
        CHECK("family name", 0 == strcmp(family, tfamily));
    }
    if (cg_delete_node("UserDataFamily1"))
        error_exit("delete UserDataFamily1");
    if (cg_nmultifam(&nnames)) error_exit("user data nmultifam after delete");
    CHECK("user data nmultifam after delete", nnames == 2);

    if (cg_close(cgfile)) error_exit("cg_close");
    return 0;
}

