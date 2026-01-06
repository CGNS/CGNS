# CMake script to run Test_UserGuideCode tests with output verification
#
# Usage: cmake -DTEST_PROGRAM=<exe> -DREF_OUTPUT=<file> -DWORK_DIR=<dir>
#              -P run_test_with_output_check.cmake
#
# This script:
# 1. Executes the test program and captures output
# 2. Compares output against reference file (filtering Library lines)
# 3. Returns 0 on success, non-zero on failure

# Check required parameters
if(NOT DEFINED TEST_PROGRAM)
  message(FATAL_ERROR "TEST_PROGRAM must be defined")
endif()

if(NOT DEFINED REF_OUTPUT)
  message(FATAL_ERROR "REF_OUTPUT must be defined")
endif()

if(NOT DEFINED WORK_DIR)
  message(FATAL_ERROR "WORK_DIR must be defined")
endif()

# Set working directory
file(MAKE_DIRECTORY "${WORK_DIR}")

# Execute test program and capture output
execute_process(
  COMMAND "${TEST_PROGRAM}"
  WORKING_DIRECTORY "${WORK_DIR}"
  RESULT_VARIABLE TEST_RESULT
  OUTPUT_VARIABLE TEST_OUTPUT
  ERROR_QUIET
)

if(NOT TEST_RESULT EQUAL 0)
  message(FATAL_ERROR "Test program failed with exit code ${TEST_RESULT}")
endif()

# Read reference output file
if(NOT EXISTS "${REF_OUTPUT}")
  message(FATAL_ERROR "Reference output file not found: ${REF_OUTPUT}")
endif()

file(READ "${REF_OUTPUT}" REF_CONTENT)

# Function to filter output lines (remove Library lines)
function(filter_output INPUT_STR OUTPUT_VAR)
  # Split into lines
  string(REGEX REPLACE "\r?\n" ";" LINES "${INPUT_STR}")

  set(FILTERED_LINES "")
  foreach(LINE ${LINES})
    # Skip lines containing "Library"
    if(NOT LINE MATCHES "Library")
      list(APPEND FILTERED_LINES "${LINE}")
    endif()
  endforeach()

  # Join back into string
  string(REPLACE ";" "\n" FILTERED "${FILTERED_LINES}")
  set(${OUTPUT_VAR} "${FILTERED}" PARENT_SCOPE)
endfunction()

# Filter both outputs
filter_output("${TEST_OUTPUT}" FILTERED_TEST)
filter_output("${REF_CONTENT}" FILTERED_REF)

# Compare filtered outputs
if(NOT "${FILTERED_TEST}" STREQUAL "${FILTERED_REF}")
  # Write outputs to files for debugging
  file(WRITE "${WORK_DIR}/output_actual.txt" "${FILTERED_TEST}")
  file(WRITE "${WORK_DIR}/output_expected.txt" "${FILTERED_REF}")

  message(FATAL_ERROR "Output mismatch! Actual output saved to ${WORK_DIR}/output_actual.txt, expected output saved to ${WORK_DIR}/output_expected.txt")
endif()

message(STATUS "Test passed!")
