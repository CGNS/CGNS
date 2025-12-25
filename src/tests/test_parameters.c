/*
 * Test program for cg_parameters_t API
 *
 * Tests the new parameter object architecture for thread-safe CGNS configuration.
 * This test verifies:
 *  1. Parameter object lifecycle (create/destroy)
 *  2. Generic parameter setter (cg_params_set)
 *  3. Version bounds configuration
 *  4. File type and compression settings
 *  5. cg_open() polymorphic API with explicit parameters (C11 _Generic)
 *  6. Backward compatibility with cg_open() legacy 3-argument usage
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cgnslib.h"

#define TEST_FILE_1 "test_params_default.cgns"
#define TEST_FILE_2 "test_params_custom.cgns"
#define TEST_FILE_3 "test_params_copy.cgns"

static int test_count = 0;
static int test_pass = 0;
static int test_fail = 0;

#define TEST_START(name) \
    do { \
        test_count++; \
        printf("\n========================================\n"); \
        printf("Test %d: %s\n", test_count, name); \
        printf("========================================\n"); \
    } while(0)

#define TEST_CHECK(condition, message) \
    do { \
        if (condition) { \
            printf("  PASSED: %s\n", message); \
            test_pass++; \
        } else { \
            printf("  FAILED: %s\n", message); \
            printf("    Error: %s\n", cg_get_error()); \
            test_fail++; \
        } \
    } while(0)

#define TEST_SUMMARY() \
    do { \
        printf("\n========================================\n"); \
        printf("Test Summary:\n"); \
        printf("  Total:  %d\n", test_count); \
        printf("  Passed: %d\n", test_pass); \
        printf("  Failed: %d\n", test_fail); \
        printf("========================================\n"); \
        if (test_fail == 0) { \
            printf("All tests passed!\n"); \
        } else { \
            printf("%d test(s) failed!\n", test_fail); \
        } \
    } while(0)

/* Test 1: Parameter lifecycle (create/destroy) */
void test_parameter_lifecycle(void)
{
    cg_parameters_t params = NULL;
    int result;

    TEST_START("Parameter Lifecycle (create/destroy)");

    /* Create parameter object */
    result = cg_params_create(&params);
    TEST_CHECK(result == CG_OK && params != NULL, "cg_params_create() succeeds");

    /* Destroy parameter object */
    result = cg_params_destroy(params);
    TEST_CHECK(result == CG_OK, "cg_params_destroy() succeeds");

    /* Test NULL pointer handling */
    result = cg_params_create(NULL);
    TEST_CHECK(result == CG_ERROR, "cg_params_create(NULL) returns error");

    /* Test destroying NULL */
    result = cg_params_destroy(NULL);
    TEST_CHECK(result == CG_ERROR, "cg_params_destroy(NULL) returns error");
}

/* Test 2: Generic setter configuration */
void test_generic_setter(void)
{
    cg_parameters_t params = NULL;
    int result;

    TEST_START("Generic Setter (cg_params_set_int)");

    /* Create parameter object */
    cg_params_create(&params);

    /* Set version bounds */
    result = cg_params_set(params, CG_PARAM_MIN_VERSION, (void *)CG_LIBVER_V30);
    TEST_CHECK(result == CG_OK, "Set MIN_VERSION succeeds");

    result = cg_params_set(params, CG_PARAM_MAX_VERSION, (void *)CG_LIBVER_V40);
    TEST_CHECK(result == CG_OK, "Set MAX_VERSION succeeds");

    /* Test invalid bounds (min > max) */
    result = cg_params_set(params, CG_PARAM_MIN_VERSION, (void *)(CG_LIBVER_V40 + 100));
    TEST_CHECK(result == CG_ERROR, "Invalid MIN_VERSION (> MAX) returns error");

    /* Test NULL parameter handling */
    result = cg_params_set(NULL, CG_PARAM_FILE_TYPE, (void *)CG_FILE_HDF5);
    TEST_CHECK(result == CG_ERROR, "Set with NULL params returns error");

    /* Test unknown key */
    result = cg_params_set(params, 9999, (void *)0);
    TEST_CHECK(result == CG_ERROR, "Unknown parameter key returns error");

    cg_params_destroy(params);
}

/* Test 3: Write version configuration */
void test_write_version(void)
{
    cg_parameters_t params = NULL;
    int result;

    TEST_START("Write Version Configuration");

    /* Create parameter object */
    cg_params_create(&params);

    /* Set write version to AUTO */
    result = cg_params_set(params, CG_PARAM_WRITE_VERSION, (void *)CG_LIBVER_AUTO);
    TEST_CHECK(result == CG_OK, "Set write version to AUTO");

    /* Set explicit write version */
    result = cg_params_set(params, CG_PARAM_WRITE_VERSION, (void *)CG_LIBVER_V40);
    TEST_CHECK(result == CG_OK, "Set write version to v4.0");

    /* Test NULL parameter handling */
    result = cg_params_set(NULL, CG_PARAM_WRITE_VERSION, (void *)CG_LIBVER_V40);
    TEST_CHECK(result == CG_ERROR, "Set write version with NULL params returns error");

    cg_params_destroy(params);
}

/* Test 4: File type and compression configuration */
void test_file_options(void)
{
    cg_parameters_t params = NULL;
    int result;

    TEST_START("File Type and Compression Configuration");

    /* Create parameter object */
    cg_params_create(&params);

    /* Set file type */
#if CG_BUILD_HDF5
    result = cg_params_set(params, CG_PARAM_FILE_TYPE, (void *)CG_FILE_HDF5);
    TEST_CHECK(result == CG_OK, "Set file type to HDF5");
#else
    result = cg_params_set(params, CG_PARAM_FILE_TYPE, (void *)CG_FILE_ADF);
    TEST_CHECK(result == CG_OK, "Set file type to ADF");
#endif

    /* Set compression level */
    result = cg_params_set(params, CG_PARAM_COMPRESS, (void *)6);
    TEST_CHECK(result == CG_OK, "Set compression level to 6");

    /* Test NULL parameter handling */
    result = cg_params_set(NULL, CG_PARAM_FILE_TYPE, (void *)CG_FILE_HDF5);
    TEST_CHECK(result == CG_ERROR, "Set file type with NULL params returns error");

    result = cg_params_set(NULL, CG_PARAM_COMPRESS, (void *)6);
    TEST_CHECK(result == CG_ERROR, "Set compression with NULL params returns error");

    cg_params_destroy(params);
}

/* Test 5: cg_open() polymorphic - default parameters (C11) */
void test_open_ex_default(void)
{
    int fn, B, result;
    int cell_dim = 3, phys_dim = 3;

    TEST_START("cg_open() polymorphic with CG_PARAMS_DEFAULT");

    /* Open file with default parameters */
    result = cg_open_with_params(TEST_FILE_1, CG_MODE_WRITE, CG_PARAMS_DEFAULT, &fn);
    TEST_CHECK(result == CG_OK, "cg_open_with_params() with default params succeeds");

    /* Create a base to verify file is usable */
    result = cg_base_write(fn, "Base", cell_dim, phys_dim, &B);
    TEST_CHECK(result == CG_OK, "Create base in file opened with default params");

    cg_close(fn);
    printf("  Created file: %s\n", TEST_FILE_1);
}

/* Test 6: cg_open() polymorphic - custom parameters (C11) */
void test_open_ex_custom(void)
{
    cg_parameters_t params = NULL;
    int fn, B, result;
    int cell_dim = 3, phys_dim = 3;
    float version;

    TEST_START("cg_open() polymorphic with Custom Parameters");

    /* Create custom parameter object */
    cg_params_create(&params);
    cg_params_set(params, CG_PARAM_MIN_VERSION, (void *)CG_LIBVER_V40);
    cg_params_set(params, CG_PARAM_MAX_VERSION, (void *)CG_LIBVER_LATEST);
    cg_params_set(params, CG_PARAM_WRITE_VERSION, (void *)CG_LIBVER_V40);

    /* Open file with custom parameters using polymorphic cg_open()
     * On C11+, this uses _Generic dispatch and cg_open() automatically calls cg_open_with_params().
     * On C99, we explicitly call cg_open_with_params(). */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    result = cg_open(TEST_FILE_2, CG_MODE_WRITE, params, &fn);  // C11: polymorphic dispatch
#else
    result = cg_open_with_params(TEST_FILE_2, CG_MODE_WRITE, params, &fn);  // C99: explicit call
#endif
    TEST_CHECK(result == CG_OK, "cg_open() with custom params succeeds");

    /* Create a base */
    result = cg_base_write(fn, "Base", cell_dim, phys_dim, &B);
    TEST_CHECK(result == CG_OK, "Create base in file with custom params");

    /* Verify file version */
    cg_version(fn, &version);
    printf("  File version: %.2f\n", version);
    TEST_CHECK(version >= 4.0f, "File version is >= 4.0");

    cg_close(fn);
    cg_params_destroy(params);
    printf("  Created file: %s\n", TEST_FILE_2);
}

/* Test 7: Backward compatibility with cg_open() */
void test_backward_compatibility(void)
{
    int fn, B, result;
    int cell_dim = 3, phys_dim = 3;

    TEST_START("Backward Compatibility with cg_open()");

    /* Traditional cg_open() should still work */
    result = cg_open("test_legacy.cgns", CG_MODE_WRITE, &fn);
    TEST_CHECK(result == CG_OK, "Traditional cg_open() still works");

    /* Create a base */
    result = cg_base_write(fn, "Base", cell_dim, phys_dim, &B);
    TEST_CHECK(result == CG_OK, "Create base in legacy file");

    cg_close(fn);
    printf("  Created file: test_legacy.cgns\n");
}

int main(void)
{
    printf("========================================\n");
    printf("CGNS Parameter API Test Suite\n");
    printf("========================================\n");

    /* Run all tests */
    test_parameter_lifecycle();
    test_generic_setter();
    test_write_version();
    test_file_options();
    test_open_ex_default();
    test_open_ex_custom();
    test_backward_compatibility();

    /* Print summary */
    TEST_SUMMARY();

    /* Cleanup test files */
    printf("\nCleaning up test files...\n");
    unlink(TEST_FILE_1);
    unlink(TEST_FILE_2);
    unlink(TEST_FILE_3);
    unlink("test_legacy.cgns");

    return (test_fail == 0) ? 0 : 1;
}
