#include <stdio.h>
#include <stdlib.h>
#if defined(_WIN32) && !defined(__NUTC__)
# include <io.h>
#else
# include <unistd.h>
#endif
#include "cgnslib.h"

static float xc[27], yc[27], zc[27];

static int node[1] = {1};
static int bar[3] = {1, 3, 2};
static int tri[6] = {1, 3, 7, 2, 5, 4};
static int quad[9] = {1, 3, 9, 7, 2, 6, 8, 4, 5};
static int tetra[10] = {1, 3, 7, 19, 2, 5, 4, 10, 11, 13};
static int pyra[14] = {1, 3, 9, 7, 19, 2, 6, 8, 4, 10, 11, 14, 13, 5};
static int penta[18] = {1, 3, 7, 19, 21, 25, 2, 5, 4, 10, 12, 16, 20, 23, 22, 11, 14, 13};
static int hexa[27] = {1, 3, 9, 7, 19, 21, 27, 25, 2, 6, 8, 4, 10, 12, 18, 16,
                       20, 24, 26, 22, 5, 11, 15, 17, 13, 23, 14};

static int npoly = 16;
static int poly[80] = {6, 1, 2, 12, 21, 20, 10,
                       3, 2, 3, 12,
                       3, 10, 20, 19,
                       6, 4, 13, 23, 24, 15, 5,
                       3, 5, 15, 6,
                       3, 13, 23, 12,
                       4, 1, 4, 5, 2,
                       4, 2, 5, 15, 12,
                       4, 12, 15, 24, 21,
                       4, 21, 24, 23, 20,
                       4, 20, 23, 13, 10,
                       4, 10, 13, 4, 1,
                       4, 2, 5, 6, 3,
                       4, 3, 6, 15, 12,
                       4, 13, 10, 19, 22,
                       4, 22, 19, 20, 24};
static int nface = 3;
static int face[21] = {8, 1, 4, 7, 8, 9, 10, 11, 12,
                       5, 2, 5, 8, 13, 14,
                       5, 3, 6, 11, 15, 16};

static int elems[256];

int main (int argc, char *argv[])
{
    int i, j, k, n, ne;
    int fnum, bnum, znum, snum, cnum;
    int size[3];
    CGNS_ENUMT( ElementType_t )  type;
    char *outfile = "elemtest.cgns";

    for (i = 0, n = 0; n < NofValidElementTypes; n++) {
      type = (CGNS_ENUMT( ElementType_t ) )n;
      if (type >= CGNS_ENUMV( TETRA_4 ) && type <= CGNS_ENUMV( HEXA_27 ))
            i++;
    }
    size[0] = 27;
    size[1] = i << 1;
    size[2] = 0;
#if CGNS_VERSION >= 3000
    size[1] += nface; /* include polyhedra */
#endif        
 
    for (n = 0, k = 0; k < 3; k++) {
        for (j = 0; j < 3; j++) {
            for (i = 0; i < 3; i++) {
                xc[n] = (float)i;        
                yc[n] = (float)j;        
                zc[n] = (float)k;
                n++;
            }
        }
    }        
 
    unlink (outfile); 

    if (cg_open (outfile, CG_MODE_WRITE, &fnum) ||
        cg_base_write (fnum, "Base", 3, 3, &bnum) ||
        cg_zone_write (fnum, bnum, "Zone", size, CGNS_ENUMV( Unstructured ), &znum) ||
        cg_coord_write (fnum, bnum, znum, CGNS_ENUMV( RealSingle ), "CoordinateX", xc, &cnum) ||
        cg_coord_write (fnum, bnum, znum, CGNS_ENUMV( RealSingle ), "CoordinateY", yc, &cnum) ||
        cg_coord_write (fnum, bnum, znum, CGNS_ENUMV( RealSingle ), "CoordinateZ", zc, &cnum))
        cg_error_exit ();
    
    ne = j = 0;

    /* NODE */

    if (cg_section_write (fnum, bnum, znum, "NODE", CGNS_ENUMV( NODE ),
            ne+1, ne+1, 0, node, &snum))
        cg_error_exit ();
    ne++;

    elems[j++] = (int)CGNS_ENUMV( NODE );
    elems[j++] = node[0];
    
    /* BAR */

    if (cg_section_write (fnum, bnum, znum, "BAR_2", CGNS_ENUMV( BAR_2 ),
            ne+1, ne+1, 0, bar, &snum) ||
        cg_section_write (fnum, bnum, znum, "BAR_3", CGNS_ENUMV( BAR_3 ),
            ne+2, ne+2, 0, bar, &snum))
        cg_error_exit ();
    ne += 2;

    elems[j++] = (int)CGNS_ENUMV( BAR_2 );
    for (n = 0; n < 2; n++)
        elems[j++] = bar[n];
    elems[j++] = (int)CGNS_ENUMV( BAR_3 );
    for (n = 0; n < 3; n++)
        elems[j++] = bar[n];
    
    /* TRI */

    if (cg_section_write (fnum, bnum, znum, "TRI_3", CGNS_ENUMV( TRI_3 ),
            ne+1, ne+1, 0, tri, &snum) ||
        cg_section_write (fnum, bnum, znum, "TRI_6", CGNS_ENUMV( TRI_6 ),
            ne+2, ne+2, 0, tri, &snum))
        cg_error_exit ();
    ne += 2;

    elems[j++] = (int)CGNS_ENUMV( TRI_3 );
    for (n = 0; n < 3; n++)
        elems[j++] = tri[n];
    elems[j++] = (int)CGNS_ENUMV( TRI_6 );
    for (n = 0; n < 6; n++)
        elems[j++] = tri[n];
    
    /* QUAD */

    if (cg_section_write (fnum, bnum, znum, "QUAD_4", CGNS_ENUMV( QUAD_4 ),
            ne+1, ne+1, 0, quad, &snum) ||
        cg_section_write (fnum, bnum, znum, "QUAD_8", CGNS_ENUMV( QUAD_8 ),
            ne+2, ne+2, 0, quad, &snum) ||
        cg_section_write (fnum, bnum, znum, "QUAD_9", CGNS_ENUMV( QUAD_9 ),
            ne+3, ne+3, 0, quad, &snum))
        cg_error_exit ();
    ne += 3;

    elems[j++] = (int)CGNS_ENUMV( QUAD_4 );
    for (n = 0; n < 4; n++)
        elems[j++] = quad[n];
    elems[j++] = (int)CGNS_ENUMV( QUAD_8 );
    for (n = 0; n < 8; n++)
        elems[j++] = quad[n];
    elems[j++] = (int)CGNS_ENUMV( QUAD_9 );
    for (n = 0; n < 9; n++)
        elems[j++] = quad[n];
    
    /* TETRA */
    
    if (cg_section_write (fnum, bnum, znum, "TETRA_4", CGNS_ENUMV( TETRA_4 ),
            ne+1, ne+1, 0, tetra, &snum) ||
        cg_section_write (fnum, bnum, znum, "TETRA_10", CGNS_ENUMV( TETRA_10 ),
            ne+2, ne+2, 0, tetra, &snum))
        cg_error_exit ();
    ne += 2;

    elems[j++] = (int)CGNS_ENUMV( TETRA_4 );
    for (n = 0; n < 4; n++)
        elems[j++] = tetra[n];
    elems[j++] = (int)CGNS_ENUMV( TETRA_10 );
    for (n = 0; n < 10; n++)
        elems[j++] = tetra[n];
    
    /* PYRA */

    if (cg_section_write (fnum, bnum, znum, "PYRA_5", CGNS_ENUMV( PYRA_5 ),
            ne+1, ne+1, 0, pyra, &snum) ||
        cg_section_write (fnum, bnum, znum, "PYRA_14", CGNS_ENUMV( PYRA_14 ),
            ne+2, ne+2, 0, pyra, &snum))
        cg_error_exit ();
    ne += 2;

    elems[j++] = (int)CGNS_ENUMV( PYRA_5 );
    for (n = 0; n < 5; n++)
        elems[j++] = pyra[n];
    elems[j++] = (int)CGNS_ENUMV( PYRA_14 );
    for (n = 0; n < 14; n++)
        elems[j++] = pyra[n];
        
#if CGNS_VERSION >= 3000            
    if (cg_section_write (fnum, bnum, znum, "PYRA_13", CGNS_ENUMV( PYRA_13 ),
            ne+1, ne+1, 0, pyra, &snum))
        cg_error_exit ();
    ne++;

    elems[j++] = (int)CGNS_ENUMV( PYRA_13 );
    for (n = 0; n < 13; n++)
        elems[j++] = pyra[n];
#endif            
 
    /* PENTA */

    if (cg_section_write (fnum, bnum, znum, "PENTA_6", CGNS_ENUMV( PENTA_6 ),
            ne+1, ne+1, 0, penta, &snum) ||
        cg_section_write (fnum, bnum, znum, "PENTA_15", CGNS_ENUMV( PENTA_15 ),
            ne+2, ne+2, 0, penta, &snum) ||
        cg_section_write (fnum, bnum, znum, "PENTA_18", CGNS_ENUMV( PENTA_18 ),
            ne+3, ne+3, 0, penta, &snum))
        cg_error_exit ();
    ne += 3;

    elems[j++] = (int)CGNS_ENUMV( PENTA_6 );
    for (n = 0; n < 6; n++)
        elems[j++] = penta[n];
    elems[j++] = (int)CGNS_ENUMV( PENTA_15 );
    for (n = 0; n < 15; n++)
        elems[j++] = penta[n];
    elems[j++] = (int)CGNS_ENUMV( PENTA_18 );
    for (n = 0; n < 18; n++)
        elems[j++] = penta[n];
   
    /* HEXA */

    if (cg_section_write (fnum, bnum, znum, "HEXA_8", CGNS_ENUMV( HEXA_8 ),
            ne+1, ne+1, 0, hexa, &snum) ||
        cg_section_write (fnum, bnum, znum, "HEXA_20", CGNS_ENUMV( HEXA_20 ),
            ne+2, ne+2, 0, hexa, &snum) ||
        cg_section_write (fnum, bnum, znum, "HEXA_27", CGNS_ENUMV( HEXA_27 ),
            ne+3, ne+3, 0, hexa, &snum))
        cg_error_exit ();
    ne += 3;

    elems[j++] = (int)CGNS_ENUMV( HEXA_8 );
    for (n = 0; n < 8; n++)
        elems[j++] = hexa[n];
    elems[j++] = (int)CGNS_ENUMV( HEXA_20 );
    for (n = 0; n < 20; n++)
        elems[j++] = hexa[n];
    elems[j++] = (int)CGNS_ENUMV( HEXA_27 );
    for (n = 0; n < 27; n++)
        elems[j++] = hexa[n];

    /* MIXED */
 
    i = ne;
    elems[j++] = (int)(CGNS_ENUMV( NGON_n ) + 3);
    for (n = 0; n < 3; n++)
        elems[j++] = tri[n];
    elems[j++] = (int)(CGNS_ENUMV( NGON_n ) + 4);
    for (n = 0; n < 4; n++)
        elems[j++] = quad[n];
    i += 2;
        
    if (cg_section_write (fnum, bnum, znum, "MIXED", CGNS_ENUMV( MIXED ),
            ne+1, ne+i, 0, elems, &snum))
        cg_error_exit ();
    ne += i;

#if CGNS_VERSION >= 3000
    for (k = 0, i = 0; i < nface; i++) {
        n = face[k++];
        for (j = 0; j < n; j++) {
            face[k++] += ne;
        }
    }
 
    if (cg_section_write (fnum, bnum, znum, "NGON_n", CGNS_ENUMV( NGON_n ),
            ne+1, ne+npoly, 0, poly, &snum) ||
        cg_section_write (fnum, bnum, znum, "NFACE_n", CGNS_ENUMV( NFACE_n ),
            ne+npoly+1, ne+npoly+nface, 0, face, &snum))
        cg_error_exit ();
#endif   

    if (cg_close (fnum)) cg_error_exit ();
    return 0;
}
