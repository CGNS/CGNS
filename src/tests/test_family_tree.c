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

/* from cgns_internal: so we can reset expected error messages */
void cgi_error(const char *format, ...);

#define NUM_SIDE 5

#define TRACE(x)   printf("** TRACE [%d] %s\n", __LINE__, x )
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
    int i, j, k, n, nfam, nb, ng, nnames, ier;

    int cgfile, cgbase, cgtree, cgzone, cgfam, cgcoord, cgbc, cgsr;
    float exp[5];
    char outfile[33], name[33], tname[33];
    char family_name[CG_MAX_GOTO_DEPTH*33+1];
    char tfamily_name[CG_MAX_GOTO_DEPTH*33+1];
    /*  big Family name of size 33*CG_MAX_GOTO_DEPTH+1 that should fail  */
    char big_family_name[33*CG_MAX_GOTO_DEPTH+2];

    /* ================================================================ WRITING TESTS */

    strcpy( outfile, "family_tree.cgns" );
    unlink( outfile );

    TRACE( "Create file" );
    if( cg_open( outfile, CG_MODE_WRITE, &cgfile ) )
        error_exit( "cg_open" );

    TRACE( "Create Grid Base" );
    if( cg_base_write( cgfile, "Structured", 3, 3, &cgbase ) ||
        cg_gopath( cgfile, "/Structured" )                   ||
        cg_dataclass_write( CGNS_ENUMV(Dimensional))         ||
        cg_units_write( CGNS_ENUMV(Kilogram),
                        CGNS_ENUMV(Meter),
                        CGNS_ENUMV(Second),
                        CGNS_ENUMV(Kelvin),
                        CGNS_ENUMV(Radian) ) )
        error_exit( "grid base write" );

    TRACE( "Create Family Tree Base" );
    if( cg_base_write( cgfile, "FamilyTree", 3, 3, &cgtree ) )
        error_exit( "family tree base");

    /* SOME GRID DATA */
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

    /* Write Family Tree */
    /*
     * FamilyTree
     *   +- Family1
     *   |  +- Family1.1
     *   |  |  +- Family1.1.1
     *   |  +- Family1.2
     *   |     +- Family1.2.1
     *   |        +- Family1.2.1.1      will be deleted (direct deletion)
     *   |        +- Family1.2.1.2      will be overwritten
     *   +- Family2
     *   |  +- Family2.1
     *   |     +- Family2.1.1
     *   +- Family3
     *   +- Family4
     *      +- Family4.1
     *      +- Family4.2                will be deleted (and sub childs as well)
     *      |  +- Family4.2.1
     *      |  |  +- Family4.2.1.1
     *      |  +- Family4.2.2
     *      +- Family4.3
     */

    /* PATH BASED FAMILY NODE CREATION */

    if( cg_family_write(cgfile, cgtree, "Family1", &cgfam ) )
        error_exit( "write Family1" );
    printf( "Conventional creation of Family_t \"Family1\" node [%d]\n", cgfam );

    if( cg_family_write(cgfile, cgtree, "/FamilyTree/Family1/Family1.1", &cgfam ) )
        error_exit( "write FamilyTree/Family1/Family1.1" );
    printf( "Creation of Family_t \"/FamilyTree/Family1/Family1.1\" node using path with existing intermediate Family_t node [%d]\n", cgfam );

    if( cg_family_write(cgfile, cgtree, "/FamilyTree/Family1/Family1.1/Family1.1.1", &cgfam ) )
        error_exit( "write /FamilyTree/Family1/Family1.1/Family1.1.1" );
    printf( "Creation of Family_t \"/FamilyTree/Family1/Family1.1/Family1.1.1\" node using path with existing intermediate Family_t node [%d]\n", cgfam );

    if( cg_family_write(cgfile, cgtree, "/FamilyTree/Family1/Family1.2/Family1.2.1", &cgfam ) )
        error_exit( "write /FamilyTree/Family1/Family1.2/Family1.2.1" );
    printf( "Creation of Family_t \"/FamilyTree/Family1/Family1.2/Family1.2.1\" node  using path with missing intermediate Family_t node [%d]\n", cgfam );

    if( cg_family_write(cgfile, cgtree, "/FamilyTree/Family2", &cgfam ) )
        error_exit( "write /FamilyTree/Family2" );
    printf( "Creation of Family_t node \"/FamilyTree/Family2\" at conventional level using its path [%d]\n", cgfam );


    if( cg_family_write(cgfile, cgtree, "Family3", &cgfam ) )
        error_exit( "write /FamilyTree/Family2" );
    printf( "Conventional creation of Family_t node \"Family3\" after non conventional creation [%d]\n", cgfam );

    /* NODE BASED FAMILY NODE CREATION */

    if( cg_goto( cgfile, cgtree, "Family2",0,NULL))
        error_exit( "goto Family2" );
    if( cg_node_family_write( "Family2.1", &cgfam ) )
        error_exit( "write /FamilyTree/Family2/Family2.1 (follow cg_goto) ");
    printf("Creation of Family_t \"/FamilyTree/Family2/Family2.1\" using node based creation (follow cg_goto) [%d]\n", cgfam );


    if( cg_gopath(cgfile, "/FamilyTree/Family1/Family1.2/Family1.2.1" ) )
        error_exit( "gopath \"/FamilyTree/Family1/Family1.2/Family1.2.1\"/");

    if( cg_node_family_write( "Family1.2.1.1", &cgfam ) )
        error_exit( "write /FamilyTree/Family1/Family1.2/Family1.2.1/Family1.2.1.1 (follow cg_gopath) ");
    printf("Creation of Family_t \"/FamilyTree/Family1/Family1.2/Family1.2.1/Family1.2.1.1\" using node based creation (follow cg_gopath) [%d]\n", cgfam );

    if( cg_node_family_write( "Family1.2.1.2", &cgfam ) )
        error_exit( "write /FamilyTree/Family1/Family1.2/Family1.2.1/Family1.2.1.2 (follow cg_gopath) ");
    printf("Creation of Family_t \"/FamilyTree/Family1/Family1.2/Family1.2.1/Family1.2.1.2\" using node based creation (follow cg_gopath) [%d]\n", cgfam );

    if( cg_node_nfamilies( &nfam ) )
        error_exit( "cg_node_nfamilies ");
    printf( "At current level, %d families (expect 2)\n", nfam );

    if( cg_goto( cgfile, cgtree, "Family2", 0, "Family2.1", 0, NULL ) )
        error_exit( "goto Family2, Family2.1" );
    if( cg_node_family_write( "Family2.1.1", &cgfam ) )
        error_exit( "write /FamilyTree/Family2/Family2.1/Family2.1.1 (follow cg_goto) ");
    printf("Creation of Family_t \"/FamilyTree/Family2/Family2.1/Family2.1.1\" using node based creation (follow cg_goto) [%d]\n", cgfam );


    if( cg_family_write(cgfile, cgtree, "Family4", &cgfam ) )
        error_exit( "write /FamilyTree/Family4" );
    printf( "Conventional creation of Family_t node \"Family4\" after non conventional creation [%d]\n", cgfam );

    if( cg_family_write(cgfile, cgtree, "/FamilyTree/Family4/Family4.1", &cgfam  ) )
        error_exit( "write Family4.1" );
    if( cg_family_write(cgfile, cgtree, "/FamilyTree/Family4/Family4.2/Family4.2.1/Family4.2.1.1", &cgfam ))
        error_exit( "write Family4.2.1.1");
    if( cg_family_write(cgfile, cgtree, "/FamilyTree/Family4/Family4.2/Family4.2.2", &cgfam ))
        error_exit( "write Family4.2.2");
    if( cg_family_write(cgfile, cgtree, "/FamilyTree/Family4/Family4.3", &cgfam ))
        error_exit( "write Family4.3");

    /* Add failing Family */
    if ( cg_family_write(cgfile, cgtree, "", &cgfam ) ==  CG_OK )
	    error_exit( "writing empty Family did not failed");
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */
    if ( cg_family_write(cgfile, cgtree, "///", &cgfam ) ==  CG_OK )
        error_exit( "writing Family of / only did not failed");
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */
    /* Family of size 33*CG_MAX_GOTO_DEPTH+1 should fail  */
    strcpy(big_family_name, "/FamilyTree/");
    for (n=12; n<33*CG_MAX_GOTO_DEPTH+2; n++){
        big_family_name[n] = 'a';
        if (n % 33 == 0)
            big_family_name[n] = '/';
    }
    big_family_name[33*CG_MAX_GOTO_DEPTH+1] = '\0';
    if ( cg_family_write(cgfile, cgtree, big_family_name , &cgfam ) ==  CG_OK )
        error_exit( "writing too long Family did not failed");
    cg_error_print();
    cgi_error("no CGNS error reported");  /* reset */
    big_family_name[33*CG_MAX_GOTO_DEPTH] = '\0';
    if ( cg_family_write(cgfile, cgtree, big_family_name , &cgfam ))
        error_exit( "writing long Family failed");

    if (cg_goto(cgfile, cgtree,  NULL))
        error_exit("go to family tree base");
    if( cg_node_family_write( "FamilyN", &cgfam ) )
        error_exit( "write /FamilyTree/FamilyN (node based writem follow cg_gopath) ");
    printf("Creation of Family_t write /FamilyTree/FamilyN (node based writem follow cg_gopath) [%d]\n", cgfam );

    /* FAMILY (TREE) NAME CREATION */

    if (cg_goto(cgfile, cgbase, "Zone", 0, NULL))
        error_exit("go to zone");
    if (cg_famname_write("/FamilyTree/Family1"))
        error_exit("zone family write");
    for (j = 1; j <= 3; j++) {
        sprintf(name, "ZoneFamily%d", j);
        sprintf(family_name, "/FamilyTree/Family1/Family1.2/Family1.2.1/Family1.2.1.%d", j);
        if (cg_multifam_write(name, family_name))
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
    if (cg_famname_write("/FamilyTree/Family2"))
        error_exit("zone family write");

#ifdef CG_BUILD_BASESCOPE
    if (cg_multifam_write("F1","/FamilyTree/TopFamily2"))
      cg_error_print();
    if (cg_multifam_write("F2","BaseWithAQuiteLongNameToTestSize/TopFamily2"))
      cg_error_print();
    if (cg_multifam_write("F3","B/FamilyNameHasExactly32Characters"))
      cg_error_print();
    if (cg_multifam_write("F4","BaseWithAQuiteLongNameToTestSize/FamilyNameHasExactly32Characters"))
      cg_error_print();
    if (cg_multifam_write("F5","BaseWithAQuiteLongNameToTestSize/FamilyNameHasExactly32Characters/ButMoreThan66For/The/Full/Path"))
      cg_error_print();
#endif

    for (j = 1; j <= 3; j++) {
        sprintf(name, "BCFamily%d", j);
        sprintf(family_name, "/FamilyTree/Family%d", j);
        if (cg_multifam_write(name, family_name))
            error_exit("BC multifam write");
    }

    if (cg_subreg_bcname_write(cgfile, cgbase, cgzone, "SubRegion",
            2, "Inflow", &cgsr))
        error_exit("subregion write");
    if (cg_goto(cgfile, cgbase, "Zone", 0, "SubRegion", 0, NULL))
        error_exit("go to subregion");
    if (cg_famname_write("/FamilyTree/Family3"))
        error_exit("subregion family write");
    for (j = 1; j <= 3; j++) {
        sprintf(name, "SubRegionFamily%d", j);
        sprintf(family_name, "/FamilyTree/Family%d", j);
        if (cg_multifam_write(name, family_name))
            error_exit("subregion multifam write");
    }

    if (cg_goto(cgfile, cgbase, "Zone", 0, NULL))
        error_exit("go to zone");
    if (cg_user_data_write("UserData"))
        error_exit("user data write");
    if (cg_user_data_write("UserData2"))
        error_exit("user data write2");
    if (cg_goto(cgfile, cgbase, "Zone", 0, "UserData", 0, NULL))
        error_exit("go to user data");
    if (cg_famname_write("/FamilyTree/Family4"))
        error_exit("user data family write");
    for (j = 1; j <= 3; j++) {
        sprintf(name, "UserDataFamily%d", j);
        sprintf(family_name, "/FamilyTree/Family%d", j);
        if (cg_multifam_write(name, family_name))
            error_exit("user data multifam write");
    }

    /* FAMILY NAMES IN TREE */
    if( cg_gopath( cgfile, "/FamilyTree/Family1/Family1.1") )
        error_exit( "gopath /FamilyTree/Family1/Family1.1 ");

    if( cg_node_family_name_write( "FamilyN1.1-1", "/FamilyTree/Family2/Family2.1") )
        error_exit( "write FamilyN1.1-2" );
    if( cg_node_family_name_write( "FamilyN1.1-2", "/FamilyTree/Family2/Family2.1") )
        error_exit( "write FamilyN1.1-2" );
    if( cg_node_family_name_write( "FamilyN1.1-3", "/FamilyTree/Family2/Family2.1") )
        error_exit( "write FamilyN1.1-3" );

    if( cg_node_nfamily_names( &nnames ) )
        error_exit( "node_nfamily_names" );
    CHECK( "num of names ", nnames == 3 );


    TRACE( "Closing file" );
    if (cg_close(cgfile)) error_exit("cg_close");

    /* ============================================================== MODIFYING TESTS */
    TRACE( "Modify file" );

    if (cg_open(outfile, CG_MODE_MODIFY, &cgfile)) error_exit("cg_open");

    if( cg_nbases( cgfile, &nb ) )
        error_exit("Get nbases" );
    CHECK( "num of bases", nb == 2 );

    cgtree = cgbase = -1;

    for( n=0; n < nb; n++ ) {
        int cell_dim, phys_dim;
        if( cg_base_read( cgfile, n+1, name, &cell_dim,&phys_dim ))
            error_exit( "Read base" );
        if( strcmp( name, "FamilyTree" ) ==0 )
            cgtree = n+1;
        else
            cgbase = n+1;
    }

    if( cg_gopath( cgfile, "/FamilyTree") )
        error_exit( "gopath /FamilyTree ");
    if (cg_delete_node("aaaaaaaaaaaaaaaaaaaaa") )
        error_exit("delete long tree");

    printf( "Structured %d, FamilyTree %d\n", cgbase, cgtree );

    if (cg_nfamilies(cgfile, cgtree, &nfam))
        error_exit("number of families");

    CHECK("cg_nfamilies", nfam == 5);

    /* FAMILY NODE DELETION */

    if (cg_goto(cgfile, cgtree,  "Family1", 0, "Family1.2",0, "Family1.2.1",0, NULL))
            error_exit("goto topfamily");

    if (cg_gopath(cgfile, "/FamilyTree/Family1/Family1.2/Family1.2.1"))
        error_exit("goto path /FamilyTree/Family1/Family1.2/Family1.2.1");

    if (cg_delete_node("Family1.2.1.1") )
        error_exit("delete SubFamily Family1.2.1.1");

    if (cg_node_nfamilies(&nfam))
        error_exit("number of family after delete");
    CHECK("number of family after delete", nfam == 1);


    if (cg_gopath(cgfile, "/FamilyTree/Family4"))
        error_exit("goto path /FamilyTree/Family4");
    if (cg_delete_node("Family4.2") )
        error_exit("delete Sub tree from Family Family4.2");

    if (cg_node_nfamilies(&nfam))
        error_exit("number of family after delete");
    CHECK("number of family after delete", nfam == 2);


    /* FAMILY NODE OVERWRITING */
    if( cg_gopath( cgfile, "/FamilyTree/Family1/Family1.2/Family1.2.1"))
        error_exit("goto path /FamilyTree/Family1/Family1.2/Family1.2.1");

    if( cg_node_family_write( "Family1.2.1.2", &cgfam ) )
        error_exit( "Rewrite 1.2.1.2");
    if (cg_node_nfamilies(&nfam))
        error_exit("number of family after overwritten");
    CHECK("number of family after overwritten (unchanged)", nfam == 1);


    if( cg_gopath( cgfile, "/FamilyTree/Family1/Family1.1" ) )
        error_exit( "gopath /FamilyTree/Family1/Family1.1" );

    if( cg_node_nfamilies( &nfam ) )
        error_exit( "nfamilies Family1.1");
    CHECK( "number of Family_t under Family1.1", nfam == 1 );

    if( cg_node_family_read( nfam, family_name, &nb, &ng ) )
        error_exit( "node family read");

    CHECK( "Family name", strcmp( family_name, "Family1.1.1") == 0 );
    CHECK( "nFamBC", nb == 0 );
    CHECK( "nGeo", ng == 0 );

    if( cg_node_nfamily_names( &nnames ) )
        error_exit( "node nfamily names");

    CHECK( "nnames == 3", nnames == 3 );

    for( n=1; n <= nnames; n++ ) {
        if( cg_node_family_name_read( n, name, family_name ) )
            error_exit( "node family_name_read");
        sprintf( tname, "FamilyN1.1-%d", n );
        sprintf( tfamily_name, "/FamilyTree/Family2/Family2.1");
        CHECK( "FamilyName node name", strcmp( tname, name ) == 0 );
        CHECK( "FamilyName data",      strcmp( tfamily_name, family_name ) == 0 );
    }

    if( cg_delete_node( "FamilyN1.1-1" ) )
        error_exit( "Delete FamilyN1.1-1");

    if( cg_node_family_name_write( "FamilyN1.1-3", "/FamilyTree/Family2/Family3.1") )
        error_exit( "overwrite FamilyN1.1-3" );

    if( cg_node_nfamily_names( &nnames ) )
        error_exit( "node nfamily names");

    CHECK( "nnames == 2", nnames == 2 );

    for( n=1; n <= nnames; n++ ) {
        if( cg_node_family_name_read( n, name, family_name ) )
            error_exit( "node family_name_read");
        sprintf( tname, "FamilyN1.1-%d", n+1 );
        sprintf( tfamily_name, "/FamilyTree/Family2/Family%d.1", n+1 );
        CHECK( "FamilyName node name", strcmp( tname, name ) == 0 );
        CHECK( "FamilyName data",      strcmp( tfamily_name, family_name ) == 0 );
    }


    if( cg_gopath( cgfile, "/Structured/Zone/UserData") )
        error_exit( "gopath /Structured/Zone/UserData" );

    if( cg_nmultifam( &nnames ) )
        error_exit( "nfamily names in /Structured/Zone/UserData" );
    printf( "nfamily names in /Structured/Zone/UserData : %d\n", nnames );

    if( cg_famname_read( family_name ) )
        error_exit( "family_name read ud\n" );

    printf( "family_name %s\n", family_name);

    if( cg_gopath( cgfile, "/Structured/Zone/UserData2") )
        error_exit( "gopath /Structured/Zone/UserData2" );

    if( cg_nmultifam( &nnames ) )
        error_exit( "nfamily names in /Structured/Zone/UserData2" );
    printf( "nfamily names in /Structured/Zone/UserData2 : %d\n", nnames );

    ier = cg_famname_read( family_name );
    if( ier )
    {
        if( ier == CG_NODE_NOT_FOUND ) {
            printf( "No associated FamilyName\n" );
        }
        else
            error_exit( "family_name read ud2\n" );
    }
    else
        printf( "family_name %s\n", family_name);

    TRACE( "Closing file" );
    if (cg_close(cgfile)) error_exit("cg_close");
    return 0;
}

