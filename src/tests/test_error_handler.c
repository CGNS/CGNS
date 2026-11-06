/* Direct error-handler registration and restoration regression. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cgnslib.h"

static int calls, replacement_calls, callback_code;
static char callback_message[256];
static const char *filename = "error_handler_test.cgns";
static int file_number;
static int file_open;
static int file_created;

static void callback(int code, char *message)
{
    ++calls;
    callback_code = code;
    snprintf(callback_message, sizeof(callback_message), "%s", message);
}

static void replacement(int code, char *message)
{
    ++replacement_calls;
    callback_code = code;
    snprintf(callback_message, sizeof(callback_message), "%s", message);
}

static void require(int condition, const char *description)
{
    if (!condition) {
        fprintf(stderr, "%s\n", description);
        cg_error_handler(NULL);
        if (file_open) cg_close(file_number);
        if (file_created) remove(filename);
        exit(1);
    }
}

static void check_message(void)
{
    require(callback_code == 1, "Ordinary public errors must have callback code1");
    require(callback_message[0] != '\0', "Error callback message is empty");
    require(strcmp(callback_message, cg_get_error()) == 0,
            "Callback message differs from cg_get_error");
}

int main(int argc, char **argv)
{
    int bases, base, cell_dim, phys_dim, actual_backend;
    char name[33];
    FILE *existing;
    int backend = 0;

    if (argc == 2 && strcmp(argv[1], "--adf") == 0) {
        backend = CG_FILE_ADF;
        filename = "error_handler_adf_test.cgns";
    } else if (argc == 2 && strcmp(argv[1], "--hdf5") == 0) {
        backend = CG_FILE_HDF5;
        filename = "error_handler_hdf5_test.cgns";
    } else if (argc != 1) {
        fputs("usage: test_error_handler [--adf|--hdf5]\n", stderr);
        return 2;
    }
    if (backend && cg_set_file_type(backend) != CG_OK) {
        fputs(cg_get_error(), stderr);
        return 2;
    }

    existing = fopen(filename, "rb");
    if (existing != NULL) {
        fclose(existing);
        fputs("Refusing pre-existing error-handler test file\n", stderr);
        return 1;
    }

    require(cg_error_handler(callback) == CG_OK, "Register direct callback");
    require(calls == 0, "Registration invoked the callback");
    require(cg_open(filename, CG_MODE_WRITE, &file_number) == CG_OK, "Create private test file");
    file_open = 1;
    file_created = 1;
    require(cg_base_write(file_number, "Base", 3, 3, &base) == CG_OK && base == 1,
            "Write a valid base");
    require(cg_close(file_number) == CG_OK, "Close newly written file");
    file_open = 0;
    require(cg_open(filename, CG_MODE_READ, &file_number) == CG_OK, "Reopen valid test file");
    file_open = 1;
    require(cg_get_file_type(file_number, &actual_backend) == CG_OK,
            "Read actual file backend");
    require(!backend || actual_backend == backend, "Requested backend differs from actual file");
    require(cg_nbases(file_number, &bases) == CG_OK && bases == 1,
            "Read valid base count");
    require(cg_base_read(file_number, 1, name, &cell_dim, &phys_dim) == CG_OK &&
            strcmp(name, "Base") == 0 && cell_dim == 3 && phys_dim == 3,
            "Read valid base values");
    require(calls == 0, "A successful public call invoked the callback");

    require(cg_nbases(-1, &bases) == CG_ERROR, "Invalid file did not fail");
    require(calls == 1, "Invalid file did not invoke exactly one callback");
    check_message();
    require(cg_base_read(file_number, 2, name, &cell_dim, &phys_dim) == CG_ERROR,
            "Missing base did not fail");
    require(calls == 2, "Missing base did not invoke exactly one callback");
    check_message();

    require(cg_error_handler(NULL) == CG_OK, "Restore default handler");
    require(calls == 2, "Restoration invoked the callback");
    require(cg_nbases(-1, &bases) == CG_ERROR, "Default invalid-file call did not return");
    require(calls == 2, "Default restoration retained the previous callback");
    require(cg_get_error()[0] != '\0', "Default handler lost error text");

    require(cg_error_handler(replacement) == CG_OK, "Register replacement callback");
    require(replacement_calls == 0, "Replacement registration invoked callback");
    require(cg_nbases(-1, &bases) == CG_ERROR, "Replacement invalid-file call did not fail");
    require(calls == 2 && replacement_calls == 1,
            "Replacing the callback did not isolate the counters");
    check_message();
    require(cg_error_handler(NULL) == CG_OK, "Final default restoration");
    require(cg_close(file_number) == CG_OK, "Close valid private test file");
    file_open = 0;
    require(calls == 2 && replacement_calls == 1, "Successful close invoked a callback");
    require(remove(filename) == 0, "Remove owned private test file");
    file_created = 0;
    printf("Direct error-handler registration and restoration passed (backend=%d)\n", actual_backend);
    return 0;
}
