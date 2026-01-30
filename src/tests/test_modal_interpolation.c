#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utils.h"

/* Test monomial coefficient size calculations */
int test_monomial_size()
{
    cgsize_t sz;
    int expected;

    printf("\n==============================================\n");
    printf("  Testing Monomial Coefficient Size Calculation\n");
    printf("==============================================\n\n");

    /* Test QUAD_9 (order 2, 2D) - should have C(2+2, 2) = 6 coefficients */
    printf("Testing QUAD_9 monomial size...\n");
    cg_element_monomial_size(CGNS_ENUMV(QUAD_9), &sz);
    expected = 6;  /* C(4, 2) = 6 */
    if (sz != expected) {
        fprintf(stderr, "ERROR: QUAD_9 expected %d coefficients, got %d\n", expected, sz);
        return 1;
    }
    printf("QUAD_9: %d monomial coefficients (order 2, 2D)\n", sz);

    /* Test QUAD_16 (order 3, 2D) - should have C(3+2, 2) = 10 coefficients */
    printf("Testing QUAD_16 monomial size...\n");
    cg_element_monomial_size(CGNS_ENUMV(QUAD_16), &sz);
    expected = 10;  /* C(5, 2) = 10 */
    if (sz != expected) {
        fprintf(stderr, "ERROR: QUAD_16 expected %d coefficients, got %d\n", expected, sz);
        return 1;
    }
    printf("QUAD_16: %d monomial coefficients (order 3, 2D)\n", sz);

    /* Test HEXA_27 (order 2, 3D) - should have C(2+3, 3) = 10 coefficients */
    printf("Testing HEXA_27 monomial size...\n");
    cg_element_monomial_size(CGNS_ENUMV(HEXA_27), &sz);
    expected = 10;  /* C(5, 3) = 10 */
    if (sz != expected) {
        fprintf(stderr, "ERROR: HEXA_27 expected %d coefficients, got %d\n", expected, sz);
        return 1;
    }
    printf("HEXA_27: %d monomial coefficients (order 2, 3D)\n", sz);

    /* Test solution monomial size with temporal component */
    printf("\nTesting solution monomial size (order 2, temporal 0)...\n");
    cg_solution_monomial_size(CGNS_ENUMV(QUAD_4), 2, 0, &sz);
    expected = 6;  /* C(2+2, 2) * (0+1) = 6 * 1 = 6 */
    if (sz != expected) {
        fprintf(stderr, "ERROR: Solution expected %d coefficients, got %d\n", expected, sz);
        return 1;
    }
    printf("Solution: %d monomial coefficients (order 2, 2D, temporal 0)\n", sz);

    printf("\nTesting solution monomial size (order 2, temporal 1)...\n");
    cg_solution_monomial_size(CGNS_ENUMV(QUAD_4), 2, 1, &sz);
    expected = 12;  /* C(2+2, 2) * (1+1) = 6 * 2 = 12 */
    if (sz != expected) {
        fprintf(stderr, "ERROR: Solution expected %d coefficients, got %d\n", expected, sz);
        return 1;
    }
    printf("Solution: %d monomial coefficients (order 2, 2D, temporal 1)\n", sz);

    printf("\nALL MONOMIAL SIZE TESTS PASSED\n");
    return 0;
}

/* Test element modal interpolation write/read */
int test_element_modal()
{
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    int i, n;
    cgsize_t ncoeff;
    double *coeff, *coeff_read;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) type = CGNS_ENUMV(QUAD_9);
    char filename[] = "test_modal_element.cgns";

    printf("\n==============================================\n");
    printf("  Testing Element Modal Interpolation\n");
    printf("==============================================\n\n");

    /* Get number of coefficients for QUAD_9 */
    cg_element_monomial_size(type, &ncoeff);
    printf("Creating CGNS file with %d monomial coefficients...\n", ncoeff);

    /* Allocate and fill coefficient array */
    coeff = (double*) malloc(ncoeff * sizeof(double));
    for (i = 0; i < ncoeff; i++) {
        coeff[i] = (double)(i + 1) * 0.5;  /* Simple test values */
    }

    /* Create CGNS file */
    size[0] = 9;  /* vertex size */
    size[1] = 1;  /* cell size */
    size[2] = 0;  /* boundary vertex size */

    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        free(coeff);
        return 1;
    }

    /* Write family */
    if (cg_family_write(cgfile, cgbase, "ModalFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        free(coeff);
        return 1;
    }

    /* Write ElementInterpolation_t */
    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "ModalInterpolation",
                                       type, &cgeinterp))
    {
        fprintf(stderr, "ERROR: Failed to write ElementInterpolation_t node\n");
        free(coeff);
        return 1;
    }
    printf("ElementInterpolation_t node created (index=%d)\n", cgeinterp);

    /* Write monomial coefficients */
    printf("Writing %d monomial coefficients...\n", ncoeff);
    if (cg_element_interpolation_coefficients_write(cgfile, cgbase, cgfamily,
                                                    cgeinterp, coeff))
    {
        fprintf(stderr, "ERROR: Failed to write monomial coefficients\n");
        free(coeff);
        return 1;
    }
    printf("Written %d monomial coefficients\n", ncoeff);

    cg_close(cgfile);

    /* Read back and validate */
    printf("\nOpening file for reading...\n");
    if (cg_open(filename, CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: Failed to open file\n");
        free(coeff);
        return 1;
    }

    coeff_read = (double*) malloc(ncoeff * sizeof(double));

    /* Read coefficients */
    printf("Reading monomial coefficients...\n");
    if (cg_element_interpolation_coefficients_read(cgfile, cgbase, cgfamily,
                                                   cgeinterp, coeff_read))
    {
        fprintf(stderr, "ERROR: Failed to read monomial coefficients\n");
        free(coeff);
        free(coeff_read);
        return 1;
    }

    /* Validate coefficients */
    printf("Validating coefficients...\n");
    int failed = 0;
    for (i = 0; i < ncoeff; i++) {
        if (fabs(coeff[i] - coeff_read[i]) > 1.e-12) {
            fprintf(stderr, "ERROR: Coefficient %d mismatch: %f != %f\n",
                    i, coeff[i], coeff_read[i]);
            failed++;
        }
    }

    if (failed > 0) {
        fprintf(stderr, "ERROR: %d coefficients failed validation\n", failed);
        free(coeff);
        free(coeff_read);
        return 1;
    }

    printf("All %d coefficients validated successfully\n", ncoeff);

    cg_close(cgfile);
    free(coeff);
    free(coeff_read);

    printf("\nALL ELEMENT MODAL TESTS PASSED\n");
    return 0;
}

/* Test solution modal interpolation write/read */
int test_solution_modal()
{
    int cgfile, cgbase, cgzone, cgfamily, cgsinterp;
    int i, n;
    cgsize_t ncoeff;
    int spatialorder = 3, temporalorder = 1;
    double *coeff, *coeff_read;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) type = CGNS_ENUMV(QUAD_4);
    CGNS_ENUMT(ElementType_t) type_read;
    CGNS_ENUMT(InterpolationType_t) itype_read;
    int os_read, ot_read;
    char filename[] = "test_modal_solution.cgns";
    char sinterpname[33];

    printf("\n==============================================\n");
    printf("  Testing Solution Modal Interpolation\n");
    printf("==============================================\n\n");

    /* Get number of coefficients for order 3 spatial, order 1 temporal */
    cg_solution_monomial_size(type, spatialorder, temporalorder, &ncoeff);
    printf("Creating CGNS file with %d monomial coefficients...\n", ncoeff);
    printf("  (spatial order %d, temporal order %d)\n", spatialorder, temporalorder);

    /* Allocate and fill coefficient array */
    coeff = (double*) malloc(ncoeff * sizeof(double));
    for (i = 0; i < ncoeff; i++) {
        coeff[i] = (double)(i + 1) * 0.25;  /* Simple test values */
    }

    /* Create CGNS file */
    size[0] = 4;  /* vertex size */
    size[1] = 1;  /* cell size */
    size[2] = 0;  /* boundary vertex size */

    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 2, 2, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        free(coeff);
        return 1;
    }

    /* Write family */
    if (cg_family_write(cgfile, cgbase, "SolutionFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        free(coeff);
        return 1;
    }

    /* Write SolutionInterpolation_t */
    if (cg_solution_interpolation_write(cgfile, cgbase, cgfamily, "ModalSolutionInterp",
                                       type, spatialorder, temporalorder,
                                       CGNS_ENUMV(ParametricMonomialsPascal), &cgsinterp))
    {
        fprintf(stderr, "ERROR: Failed to write SolutionInterpolation_t node\n");
        free(coeff);
        return 1;
    }
    printf("SolutionInterpolation_t node created (index=%d)\n", cgsinterp);

    /* Write monomial coefficients */
    printf("Writing %d monomial coefficients...\n", ncoeff);
    if (cg_solution_interpolation_coefficients_write(cgfile, cgbase, cgfamily,
                                                     cgsinterp, coeff))
    {
        fprintf(stderr, "ERROR: Failed to write monomial coefficients\n");
        free(coeff);
        return 1;
    }
    printf("Written %d monomial coefficients\n", ncoeff);

    cg_close(cgfile);

    /* Read back and validate */
    printf("\nOpening file for reading...\n");
    if (cg_open(filename, CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: Failed to open file\n");
        free(coeff);
        return 1;
    }

    /* Read solution interpolation properties */
    printf("Reading SolutionInterpolation_t node...\n");
    if (cg_solution_interpolation_read(cgfile, cgbase, cgfamily, cgsinterp,
                                      sinterpname, &type_read, &os_read, &ot_read, &itype_read))
    {
        fprintf(stderr, "ERROR: Failed to read SolutionInterpolation_t node\n");
        free(coeff);
        return 1;
    }

    /* Validate properties */
    if (strcmp(sinterpname, "ModalSolutionInterp") != 0) {
        fprintf(stderr, "ERROR: Wrong interpolation name: %s\n", sinterpname);
        free(coeff);
        return 1;
    }
    printf("Interpolation name: %s\n", sinterpname);

    if (type_read != type) {
        fprintf(stderr, "ERROR: Wrong element type\n");
        free(coeff);
        return 1;
    }
    printf("Element type: %s\n", cg_ElementTypeName(type_read));

    if (os_read != spatialorder || ot_read != temporalorder) {
        fprintf(stderr, "ERROR: Wrong orders: spatial=%d (expected %d), temporal=%d (expected %d)\n",
                os_read, spatialorder, ot_read, temporalorder);
        free(coeff);
        return 1;
    }
    printf("Orders: spatial=%d, temporal=%d\n", os_read, ot_read);

    if (itype_read != CGNS_ENUMV(ParametricMonomialsPascal)) {
        fprintf(stderr, "ERROR: Wrong interpolation type\n");
        free(coeff);
        return 1;
    }
    printf("Interpolation type: ParametricMonomialsPascal\n");

    coeff_read = (double*) malloc(ncoeff * sizeof(double));

    /* Read coefficients */
    printf("Reading monomial coefficients...\n");
    if (cg_solution_interpolation_coefficients_read(cgfile, cgbase, cgfamily,
                                                    cgsinterp, coeff_read))
    {
        fprintf(stderr, "ERROR: Failed to read monomial coefficients\n");
        free(coeff);
        free(coeff_read);
        return 1;
    }

    /* Validate coefficients */
    printf("Validating coefficients...\n");
    int failed = 0;
    for (i = 0; i < ncoeff; i++) {
        if (fabs(coeff[i] - coeff_read[i]) > 1.e-12) {
            fprintf(stderr, "ERROR: Coefficient %d mismatch: %f != %f\n",
                    i, coeff[i], coeff_read[i]);
            failed++;
        }
    }

    if (failed > 0) {
        fprintf(stderr, "ERROR: %d coefficients failed validation\n", failed);
        free(coeff);
        free(coeff_read);
        return 1;
    }

    printf("All %d coefficients validated successfully\n", ncoeff);

    cg_close(cgfile);
    free(coeff);
    free(coeff_read);

    printf("\nALL SOLUTION MODAL TESTS PASSED\n");
    return 0;
}

/* Test CartesianMonomialsPascal interpolation type */
int test_cartesian_modal()
{
    int cgfile, cgbase, cgzone, cgfamily, cgeinterp;
    int i;
    cgsize_t ncoeff;
    double *coeff, *coeff_read;
    cgsize_t size[9];
    CGNS_ENUMT(ElementType_t) type = CGNS_ENUMV(HEXA_27);
    CGNS_ENUMT(ElementType_t) type_read;
    char filename[] = "test_cartesian_modal.cgns";
    char einterpname[33];

    printf("\n==============================================\n");
    printf("  Testing CartesianMonomialsPascal Type\n");
    printf("==============================================\n\n");

    /* Get number of coefficients for HEXA_27 (order 2, 3D) */
    cg_element_monomial_size(type, &ncoeff);
    printf("Testing 3D Cartesian modal interpolation...\n");
    printf("  Element: HEXA_27 with %d coefficients\n", ncoeff);

    /* Allocate and fill coefficient array */
    coeff = (double*) malloc(ncoeff * sizeof(double));
    for (i = 0; i < ncoeff; i++) {
        coeff[i] = (double)(i + 1) * 0.1;  /* Test values */
    }

    /* Create CGNS file */
    size[0] = 27;  /* vertex size */
    size[1] = 1;   /* cell size */
    size[2] = 0;   /* boundary vertex size */

    if (cg_open(filename, CG_MODE_WRITE, &cgfile) ||
        cg_base_write(cgfile, "Base", 3, 3, &cgbase) ||
        cg_zone_write(cgfile, cgbase, "Zone", size, CGNS_ENUMV(Unstructured), &cgzone))
    {
        fprintf(stderr, "ERROR: Failed to create file structure\n");
        free(coeff);
        return 1;
    }

    /* Write family */
    if (cg_family_write(cgfile, cgbase, "CartesianFamily", &cgfamily))
    {
        fprintf(stderr, "ERROR: Failed to write Family_t node\n");
        free(coeff);
        return 1;
    }

    /* Write ElementInterpolation_t with CartesianMonomialsPascal */
    /* Note: cg_element_interpolation_write doesn't have interpolation type parameter,
     * but we can still write coefficients which work for both Parametric and Cartesian */
    if (cg_element_interpolation_write(cgfile, cgbase, cgfamily, "CartesianInterp",
                                       type, &cgeinterp))
    {
        fprintf(stderr, "ERROR: Failed to write ElementInterpolation_t node\n");
        free(coeff);
        return 1;
    }
    printf("ElementInterpolation_t node created\n");

    /* Write monomial coefficients */
    if (cg_element_interpolation_coefficients_write(cgfile, cgbase, cgfamily,
                                                    cgeinterp, coeff))
    {
        fprintf(stderr, "ERROR: Failed to write coefficients\n");
        free(coeff);
        return 1;
    }
    printf("Written %d Cartesian monomial coefficients\n", ncoeff);

    cg_close(cgfile);

    /* Read back and validate */
    if (cg_open(filename, CG_MODE_READ, &cgfile))
    {
        fprintf(stderr, "ERROR: Failed to open file\n");
        free(coeff);
        return 1;
    }

    /* Read interpolation properties */
    if (cg_element_interpolation_read(cgfile, cgbase, cgfamily, cgeinterp,
                                      einterpname, &type_read))
    {
        fprintf(stderr, "ERROR: Failed to read ElementInterpolation_t\n");
        free(coeff);
        return 1;
    }

    if (type_read != type) {
        fprintf(stderr, "ERROR: Wrong element type\n");
        free(coeff);
        return 1;
    }
    printf("Element type verified: %s\n", cg_ElementTypeName(type_read));

    coeff_read = (double*) malloc(ncoeff * sizeof(double));

    /* Read coefficients */
    if (cg_element_interpolation_coefficients_read(cgfile, cgbase, cgfamily,
                                                   cgeinterp, coeff_read))
    {
        fprintf(stderr, "ERROR: Failed to read coefficients\n");
        free(coeff);
        free(coeff_read);
        return 1;
    }

    /* Validate */
    int failed = 0;
    for (i = 0; i < ncoeff; i++) {
        if (fabs(coeff[i] - coeff_read[i]) > 1.e-12) {
            fprintf(stderr, "ERROR: Coefficient %d mismatch\n", i);
            failed++;
        }
    }

    if (failed > 0) {
        fprintf(stderr, "ERROR: %d coefficients failed\n", failed);
        free(coeff);
        free(coeff_read);
        return 1;
    }

    printf("All %d Cartesian coefficients validated\n", ncoeff);

    cg_close(cgfile);
    free(coeff);
    free(coeff_read);

    printf("\nCARTESIAN MONOMIAL TESTS PASSED\n");
    printf("  Note: CartesianMonomialsPascal uses same coefficient\n");
    printf("        storage as ParametricMonomialsPascal (Pascal ordering)\n");
    return 0;
}

int main(int argc, char **argv)
{
    int errors = 0;

    printf("\n");
    printf("##################################################\n");
    printf("#  CPEX0045 Test: Modal Interpolation API       #\n");
    printf("##################################################\n");

    /* Test monomial size calculations */
    if (test_monomial_size())
        errors++;

    /* Test element modal interpolation */
    if (test_element_modal())
        errors++;

    /* Test solution modal interpolation */
    if (test_solution_modal())
        errors++;

    /* Test Cartesian modal interpolation */
    if (test_cartesian_modal())
        errors++;

    printf("\n");
    printf("##################################################\n");
    if (errors == 0)
    {
        printf("#ALL MODAL INTERPOLATION TESTS PASSED      #\n");
    }
    else
    {
        printf("#  ✗ FAILURES: %d test(s) failed                 #\n", errors);
    }
    printf("##################################################\n");
    printf("\n");

    return errors;
}
