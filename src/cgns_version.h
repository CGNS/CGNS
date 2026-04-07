/* ------------------------------------------------------------------------- *
 * CGNS - CFD General Notation System (http://www.cgns.org)                  *
 * CGNS/MLL - Mid-Level Library header file                                  *
 * ------------------------------------------------------------------------- */

/* Single source of truth for CGNS version numbers.
 * Included by cgnslib.h, cgnsKeywords.h, and cgns_f.F90.
 * Uses only #define with numeric literals so it is valid
 * for both C and Fortran preprocessing.
 *
 * The _VALUE macros hold the raw numbers.  The public macros
 * (CGNS_VERSION, etc.) are aliases used by C code.  Fortran
 * #undefs the public macros and creates typed PARAMETERs from
 * the _VALUE macros so both languages share a single value.   */

#ifndef CGNS_VERSION_H
#define CGNS_VERSION_H

#define CGNS_VERSION_VALUE       5000
#define CGNS_DOTVERS_VALUE       5.00
#define CGNS_COMPATVERSION_VALUE 2540
#define CGNS_COMPATDOTVERS_VALUE 2.54

#define CGNS_VERSION       CGNS_VERSION_VALUE
#define CGNS_DOTVERS       CGNS_DOTVERS_VALUE
#define CGNS_COMPATVERSION CGNS_COMPATVERSION_VALUE
#define CGNS_COMPATDOTVERS CGNS_COMPATDOTVERS_VALUE

#endif
