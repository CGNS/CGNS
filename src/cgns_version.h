/* ------------------------------------------------------------------------- *
 * CGNS - CFD General Notation System (http://www.cgns.org)                  *
 * CGNS/MLL - Mid-Level Library header file                                  *
 * ------------------------------------------------------------------------- */

/* Single source of truth for CGNS version numbers.
 * Included by cgnslib.h, cgnsKeywords.h, and cgns_f.F90.
 * Valid for both C and Fortran preprocessing.
 *
 * To bump the version, edit only CGNS_VERSION_MAJOR and CGNS_VERSION_MINOR.
 * All derived macros update automatically.  CGNS_COMPAT* are fixed
 * historical constants and must be updated manually when compatibility
 * requirements change.
 *
 * The public macros (CGNS_VERSION, etc.) are aliases used by C code.
 * Fortran #undefs them and creates typed PARAMETERs from the _VALUE
 * macros so both languages share a single value.   */

#ifndef CGNS_VERSION_H
#define CGNS_VERSION_H

#define CGNS_VERSION_MAJOR       5
#define CGNS_VERSION_MINOR       0

#define CGNS_VERSION_VALUE       (CGNS_VERSION_MAJOR * 1000 + CGNS_VERSION_MINOR * 10)
#define CGNS_DOTVERS_VALUE       (CGNS_VERSION_MAJOR + CGNS_VERSION_MINOR * 0.01)
#define CGNS_COMPATVERSION_VALUE 2540
#define CGNS_COMPATDOTVERS_VALUE 2.54

#define CGNS_VERSION       CGNS_VERSION_VALUE
#define CGNS_DOTVERS       CGNS_DOTVERS_VALUE
#define CGNS_COMPATVERSION CGNS_COMPATVERSION_VALUE
#define CGNS_COMPATDOTVERS CGNS_COMPATDOTVERS_VALUE

/* Version comparison macros — valid in both C and preprocessed Fortran (.F90).
 * ABI changes only occur at minor-version boundaries, so major.minor is
 * sufficient for all compatibility guards.
 *
 * Example (Fortran):
 *   #include "cgns_version.h"
 *   #if CGNS_VERSION_GE(5,0)
 *     call cgp_coord_write_data_f(fn,B,Z,C, C_LOC(rmin(1)),C_LOC(rmax(1)),C_LOC(buf(1)),ier)
 *   #else
 *     call cgp_coord_write_data_f(fn,B,Z,C, rmin,rmax,buf,ier)
 *   #endif
 */
#define CGNS_VERSION_GE(maj, min) \
    ((CGNS_VERSION_MAJOR > (maj)) || \
     (CGNS_VERSION_MAJOR == (maj) && CGNS_VERSION_MINOR >= (min)))

#define CGNS_VERSION_LE(maj, min) \
    ((CGNS_VERSION_MAJOR < (maj)) || \
     (CGNS_VERSION_MAJOR == (maj) && CGNS_VERSION_MINOR <= (min)))

#endif
