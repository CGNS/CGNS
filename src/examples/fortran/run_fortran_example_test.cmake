# CMake script to run Fortran example tests (cross-platform compatible)
#
# Usage: cmake -DTEST_DIR=<dir> -DWRITE_EXE=<exe> -DREAD_EXE=<exe>
#              -DREF_OUTPUT=<file> -DWORK_DIR=<dir> -P run_fortran_example_test.cmake
#
# This script:
# 1. Executes the write program (cgwrite)
# 2. Executes the read program (cgread) and captures output
# 3. Compares output against reference file (filtering certain lines)
# 4. Returns 0 on success, non-zero on failure

# Check required parameters
if(NOT DEFINED WRITE_EXE)
  message(FATAL_ERROR "WRITE_EXE must be defined")
endif()

if(NOT DEFINED READ_EXE)
  message(FATAL_ERROR "READ_EXE must be defined")
endif()

if(NOT DEFINED REF_OUTPUT)
  message(FATAL_ERROR "REF_OUTPUT must be defined")
endif()

if(NOT DEFINED WORK_DIR)
  message(FATAL_ERROR "WORK_DIR must be defined")
endif()

# Set working directory
file(MAKE_DIRECTORY "${WORK_DIR}")

# Execute write program
message(STATUS "Running ${WRITE_EXE}...")
execute_process(
  COMMAND "${WRITE_EXE}"
  WORKING_DIRECTORY "${WORK_DIR}"
  RESULT_VARIABLE WRITE_RESULT
  OUTPUT_QUIET
  ERROR_QUIET
)

if(NOT WRITE_RESULT EQUAL 0)
  message(FATAL_ERROR "Write program failed with exit code ${WRITE_RESULT}")
endif()

# Execute read program and capture output
message(STATUS "Running ${READ_EXE}...")
execute_process(
  COMMAND "${READ_EXE}"
  WORKING_DIRECTORY "${WORK_DIR}"
  RESULT_VARIABLE READ_RESULT
  OUTPUT_VARIABLE READ_OUTPUT
  ERROR_QUIET
)

if(NOT READ_RESULT EQUAL 0)
  message(FATAL_ERROR "Read program failed with exit code ${READ_RESULT}")
endif()

# Read reference output file
if(NOT EXISTS "${REF_OUTPUT}")
  message(FATAL_ERROR "Reference output file not found: ${REF_OUTPUT}")
endif()

file(READ "${REF_OUTPUT}" REF_CONTENT)

# Function to filter output lines (remove Library, DonorDatatype, datatype= lines)
function(filter_output INPUT_STR OUTPUT_VAR)
  # Split into lines
  string(REGEX REPLACE "\r?\n" ";" LINES "${INPUT_STR}")

  set(FILTERED_LINES "")
  foreach(LINE ${LINES})
    # Skip lines containing "Library", "DonorDatatype", or "datatype="
    if(NOT LINE MATCHES "Library" AND
       NOT LINE MATCHES "DonorDatatype" AND
       NOT LINE MATCHES "datatype=")
      list(APPEND FILTERED_LINES "${LINE}")
    endif()
  endforeach()

  # Join back into string
  string(REPLACE ";" "\n" FILTERED "${FILTERED_LINES}")
  set(${OUTPUT_VAR} "${FILTERED}" PARENT_SCOPE)
endfunction()

# Filter both outputs
filter_output("${READ_OUTPUT}" FILTERED_READ)
filter_output("${REF_CONTENT}" FILTERED_REF)

# Compare filtered outputs
if(NOT "${FILTERED_READ}" STREQUAL "${FILTERED_REF}")
  # Write outputs to files for debugging
  file(WRITE "${WORK_DIR}/output_actual.txt" "${FILTERED_READ}")
  file(WRITE "${WORK_DIR}/output_expected.txt" "${FILTERED_REF}")

  message(FATAL_ERROR "Output mismatch! Actual output saved to ${WORK_DIR}/output_actual.txt, expected output saved to ${WORK_DIR}/output_expected.txt")
endif()

message(STATUS "Test passed!")
