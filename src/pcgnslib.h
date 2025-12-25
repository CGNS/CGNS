/* ------------------------------------------------------------------------- *
 * CGNS - CFD General Notation System (http://www.cgns.org)                  *
 * CGNS/MLL - Mid-Level Library header file                                  *
 * Please see cgnsconfig.h file for this local installation configuration    *
 * ------------------------------------------------------------------------- */

/* ------------------------------------------------------------------------- *

  This software is provided 'as-is', without any express or implied warranty.
  In no event will the authors be held liable for any damages arising from
  the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must not
     be misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.

 * ------------------------------------------------------------------------- */

#ifndef PCGNSLIB_H_
#define PCGNSLIB_H_

#include "cgnslib.h"
#include "mpi.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    CGP_INDEPENDENT=0,
    CGP_COLLECTIVE=1,
} CGNS_ENUMT( PIOmode_t );

/*===== MPI communicator =====*/

CGNSDLL int cgp_mpi_comm(MPI_Comm mpicomm);

/*===== MPI info =====*/

CGNSDLL int cgp_mpi_info(MPI_Info info);

/*===== parallel IO mode =====*/

CGNSDLL int cgp_pio_mode(CGNS_ENUMT(PIOmode_t) mode);

/*===== File IO Prototypes =====*/

/**
 * \ingroup ParallelFile
 * \brief Open a CGNS file for parallel MPI access (legacy 3-argument version)
 *
 * Opens a CGNS file for parallel access using global configuration state.
 * This is the traditional API maintained for backward compatibility.
 *
 * \param[in] filename Name of the CGNS file
 * \param[in] mode Access mode (CG_MODE_READ, CG_MODE_WRITE, CG_MODE_MODIFY)
 * \param[out] fn File index number
 * \return CG_OK on success, CG_ERROR on failure
 *
 * \par Example:
 * \code
 * MPI_Init(&argc, &argv);
 * cgp_mpi_comm(MPI_COMM_WORLD);
 *
 * int fn;
 * cgp_open("file.cgns", CG_MODE_WRITE, &fn);
 * // ... parallel I/O ...
 * cgp_close(fn);
 * MPI_Finalize();
 * \endcode
 *
 * \par Thread Safety:
 * This function uses global state and is NOT thread-safe.
 * For thread-safe operation, use cgp_open_with_params().
 *
 * \par Progressive Enhancement (C11+):
 * On C11+ compilers, cgp_open() becomes a polymorphic macro that can also
 * accept 4 arguments (with cg_parameters_t). This provides automatic type
 * dispatch without changing function names.
 *
 * \note Parallel I/O requires HDF5 with parallel support enabled
 * \sa cgp_open_with_params, cgp_close, cgp_mpi_comm, cg_open
 */
CGNSDLL int cgp_open(const char *filename, int mode, int *fn);

/**
 * \ingroup ParallelFile
 * \brief Open a CGNS file for parallel MPI access with explicit parameters
 *
 * Opens a CGNS file for parallel access using an explicit parameter object
 * for thread-safe, configurable file access.
 *
 * \param[in] filename Name of the CGNS file
 * \param[in] mode Access mode (CG_MODE_READ, CG_MODE_WRITE, CG_MODE_MODIFY)
 * \param[in] params Parameter object created with cg_params_create()
 * \param[out] fn File index number
 * \return CG_OK on success, CG_ERROR on failure
 *
 * \par Example:
 * \code
 * MPI_Init(&argc, &argv);
 * cgp_mpi_comm(MPI_COMM_WORLD);
 *
 * cg_parameters_t params;
 * cg_params_create(&params);
 * cg_params_set_int(params, CG_PARAM_FILE_TYPE, CG_FILE_HDF5);
 * cg_params_set_int(params, CG_PARAM_MIN_VERSION, CG_LIBVER_V40);
 *
 * int fn;
 * cgp_open_with_params("parallel.cgns", CG_MODE_WRITE, params, &fn);
 * // ... parallel I/O ...
 * cgp_close(fn);
 * cg_params_destroy(params);
 * MPI_Finalize();
 * \endcode
 *
 * \par Thread Safety:
 * This function is fully thread-safe. Each thread can have its own parameter
 * object and call this function concurrently.
 *
 * \par Progressive Enhancement (C11+):
 * On C11+ compilers, you can call cgp_open() with 4 arguments and the compiler
 * will automatically dispatch to this function based on the parameter type.
 * On C99 compilers, you must explicitly call cgp_open_with_params().
 *
 * \note Parallel I/O requires HDF5 with parallel support enabled.
 *       The parameter object must have file_type set to CG_FILE_HDF5.
 * \sa cgp_open, cg_params_create, cg_params_set_int, cgp_mpi_comm
 */
CGNSDLL int cgp_open_with_params(const char *filename, int mode,
                                   cg_parameters_t params, int *fn);

#ifndef BUILDING_CGNS
/* Progressive Enhancement: Polymorphic cgp_open() via argument counting
 * Uses variadic macros (C99) to support both 3 and 4 argument forms.
 * Only enabled when not building the library itself (to avoid macro conflicts).
 *
 * This allows calling cgp_open() with either 3 or 4 arguments:
 *   cgp_open(file, mode, &fn)           // 3-arg: calls cgp_open_with_params(file, mode, CG_PARAMS_DEFAULT, &fn)
 *   cgp_open(file, mode, params, &fn)   // 4-arg: calls cgp_open_with_params(file, mode, params, &fn)
 */

/* Helper macros for argument counting */
#define CGP_OPEN_3(file, mode, fn) \
    cgp_open_with_params(file, mode, CG_PARAMS_DEFAULT, fn)
#define CGP_OPEN_4(file, mode, params, fn) \
    cgp_open_with_params(file, mode, params, fn)
#define CGP_OPEN_CHOOSER(_1, _2, _3, _4, NAME, ...) NAME
#define CGP_OPEN_EXPAND(x) x  /* MSVC workaround: force __VA_ARGS__ expansion */

/* Redefine cgp_open to dispatch based on argument count */
#undef cgp_open
#define cgp_open(...) \
    CGP_OPEN_EXPAND(CGP_OPEN_CHOOSER(__VA_ARGS__, CGP_OPEN_4, CGP_OPEN_3)(__VA_ARGS__))

#endif /* !BUILDING_CGNS */

CGNSDLL int cgp_close(int fn);

/*===== Grid IO Prototypes =====*/

CGNSDLL int cgp_coord_write(int fn, int B, int Z,
    CGNS_ENUMT(DataType_t) type, const char *coordname, int *C);
CGNSDLL int cgp_coord_write_data(int fn, int B, int Z, int C,
    const cgsize_t *rmin, const cgsize_t *rmax, const void *coord_array);
CGNSDLL int cgp_coord_general_write_data(int fn, int B, int Z, int C,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, const void *coords);
CGNSDLL int cgp_coord_read_data(int fn, int B, int Z, int C,
    const cgsize_t *rmin, const cgsize_t *rmax, void *coord_array);
CGNSDLL int cgp_coord_general_read_data(int fn, int B, int Z, int C,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, void *coords);

CGNSDLL int cgp_coord_multi_read_data(int fn, int B, int Z, int *C, const cgsize_t *rmin, const cgsize_t *rmax,
                                      int nsets, void **buf);

CGNSDLL int cgp_coord_multi_write_data(int fn, int B, int Z, int *C, const cgsize_t *rmin, const cgsize_t *rmax,
                                       int nsets, const void **buf);

/*===== Unstructured Grid Prototypes =====*/

CGNSDLL int cgp_section_write(int fn, int B, int Z,
    const char *sectionname, CGNS_ENUMT(ElementType_t) type,
    cgsize_t start, cgsize_t end, int nbndry, int *S);
CGNSDLL int cgp_elements_write_data(int fn, int B, int Z, int S,
    cgsize_t start, cgsize_t end, const cgsize_t *elements);
CGNSDLL int cgp_elements_read_data(int fn, int B, int Z, int S,
    cgsize_t start, cgsize_t end, cgsize_t *elements);

CGNSDLL int cgp_poly_section_write(int fn, int B, int Z,
    const char *sectionname, CGNS_ENUMT(ElementType_t) type,
    cgsize_t start, cgsize_t end, cgsize_t maxoffset,
    int nbndry, int *S);
CGNSDLL int cgp_poly_elements_write_data(int fn, int B, int Z, int S,
    cgsize_t start, cgsize_t end, const cgsize_t *elements, const cgsize_t *offsets);
CGNSDLL int cgp_poly_elements_read_data_offsets(int fn, int B, int Z, int S,
    cgsize_t start, cgsize_t end, cgsize_t *offsets);
CGNSDLL int cgp_poly_elements_read_data_elements(int fn, int B, int Z, int S,
    cgsize_t start, cgsize_t end, const cgsize_t *offsets, cgsize_t *elements);
CGNSDLL int cgp_parent_data_write(int fn, int B, int Z, int S,
				  cgsize_t start, cgsize_t end,
				  const cgsize_t *parent_data);
CGNSDLL int cgp_parentelements_read_data(int fn, int B, int Z, int S, cgsize_t start,
    cgsize_t end, cgsize_t *parentelements);
CGNSDLL int cgp_parentelements_write_data(int fn, int B, int Z, int S, cgsize_t start,
    cgsize_t end, cgsize_t *parentelements);

/*===== Solution IO Prototypes =====*/

CGNSDLL int cgp_field_write(int fn, int B, int Z, int S,
    CGNS_ENUMT(DataType_t) type, const char *fieldname, int *F);
CGNSDLL int cgp_field_write_data(int fn, int B, int Z, int S, int F,
    const cgsize_t *rmin, const cgsize_t *rmax, const void *data);
CGNSDLL int cgp_field_general_write_data(int fn, int B, int Z, int S, int F,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, const void *data);
CGNSDLL int cgp_field_read_data(int fn, int B, int Z, int S, int F,
    const cgsize_t *rmin, const cgsize_t *rmax, void *data);
CGNSDLL int cgp_field_general_read_data(int fn, int B, int Z, int S, int F,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, void *data);


CGNSDLL int cgp_field_multi_read_data(int fn, int B, int Z, int S, int *F,
                                       const cgsize_t *rmin, const cgsize_t *rmax, int nsets, void **buf);

CGNSDLL int cgp_field_multi_write_data(int fn, int B, int Z, int S, int *F,
                                       const cgsize_t *rmin, const cgsize_t *rmax, int nsets, const void **buf);

/*===== Particles IO Prototypes =====*/
CGNSDLL int cgp_particle_coord_write(int fn, int B, int P,
    CGNS_ENUMT(DataType_t) type, const char *coordname, int *C);
CGNSDLL int cgp_particle_coord_write_data(int fn, int B, int P, int C,
    const cgsize_t *rmin, const cgsize_t *rmax, const void *coord_array);
CGNSDLL int cgp_particle_coord_general_write_data(int fn, int B, int P, int C,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, const void *coords);
CGNSDLL int cgp_particle_coord_read_data(int fn, int B, int P, int C,
    const cgsize_t *rmin, const cgsize_t *rmax, void *coord_array);
CGNSDLL int cgp_particle_coord_general_read_data(int fn, int B, int P, int C,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, void *coords);

CGNSDLL int cgp_particle_coord_multi_read_data(int fn, int B, int P, int *C, const cgsize_t *rmin, const cgsize_t *rmax,
                                               int nsets, void **buf);

CGNSDLL int cgp_particle_coord_multi_write_data(int fn, int B, int P, int *C, const cgsize_t *rmin, const cgsize_t *rmax,
                                                int nsets, const void **buf);

/*===== Particles Solution IO Prototypes =====*/

CGNSDLL int cgp_particle_field_write(int fn, int B, int P, int S,
    CGNS_ENUMT(DataType_t) type, const char *fieldname, int *F);
CGNSDLL int cgp_particle_field_write_data(int fn, int B, int P, int S, int F,
    const cgsize_t *rmin, const cgsize_t *rmax, const void *data);
CGNSDLL int cgp_particle_field_general_write_data(int fn, int B, int P, int S, int F,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, const void *data);
CGNSDLL int cgp_particle_field_read_data(int fn, int B, int P, int S, int F,
    const cgsize_t *rmin, const cgsize_t *rmax, void *data);
CGNSDLL int cgp_particle_field_general_read_data(int fn, int B, int P, int S, int F,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, void *data);


CGNSDLL int cgp_particle_field_multi_read_data(int fn, int B, int P, int S, int *F,
                                       const cgsize_t *rmin, const cgsize_t *rmax, int nsets, void **buf);

CGNSDLL int cgp_particle_field_multi_write_data(int fn, int B, int P, int S, int *F,
                                       const cgsize_t *rmin, const cgsize_t *rmax, int nsets, const void **buf);
/*===== Array IO Prototypes =====*/

CGNSDLL int cgp_array_write(const char *arrayname,
    CGNS_ENUMT(DataType_t) type, int DataDimension,
    const cgsize_t *DimensionVector, int *A);
CGNSDLL int cgp_array_write_data(int A, const cgsize_t *rmin,
    const cgsize_t *rmax, const void *data);
CGNSDLL int cgp_array_general_write_data(int A,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, const void *data);
CGNSDLL int cgp_array_read_data(int A, const cgsize_t *rmin,
    const cgsize_t *rmax, void *data);
CGNSDLL int cgp_array_general_read_data(int A,
    const cgsize_t *rmin, const cgsize_t *rmax,
    CGNS_ENUMT(DataType_t) m_type,
    int m_numdim, const cgsize_t *m_arg_dimvals,
    const cgsize_t *m_rmin, const cgsize_t *m_rmax, void *data);

CGNSDLL int cgp_array_multi_write_data(int fn, int *A, const cgsize_t *rmin,
                                       const cgsize_t *rmax, int nsets, const void **buf);

CGNSDLL int cgp_array_multi_read_data(int fn, int *A, const cgsize_t *rmin,
                                      const cgsize_t *rmax, int nsets, void **buf);


/*===== PointList Prototypes =====*/
CGNSDLL int cgp_ptlist_write_data(int file_number, cgsize_t start,
    cgsize_t end, const cgsize_t *points);
CGNSDLL int cgp_ptlist_read_data(int file_number, cgsize_t start, cgsize_t end, cgsize_t *points);

/*===== exit with error and call MPI_Abort =====*/

CGNSDLL void cgp_error_exit(void);

#ifdef __cplusplus
}
#endif
#endif
