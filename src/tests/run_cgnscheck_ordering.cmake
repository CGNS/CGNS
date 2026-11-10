# Run cgnscheck on a fixture and assert only that it does NOT report the
# CPEX-0045 S3.2.2 principal-vertex ordering violation.
#
# The fixtures this guards are metadata-only: they legitimately report
# "no grid coordinates defined"/"no element sets found", so cgnscheck exits
# non-zero and ctest would mark the test failed on the exit code alone.  That
# says nothing about the rule under test, so the exit status is deliberately
# ignored here and only the diagnostic text is asserted on.
#
# Invoked as:
#   cmake -DCGNSCHECK=<exe> -DFIXTURE=<file.cgns> -P run_cgnscheck_ordering.cmake

if(NOT CGNSCHECK OR NOT FIXTURE)
  message(FATAL_ERROR "CGNSCHECK and FIXTURE must both be defined")
endif()

execute_process(
  COMMAND "${CGNSCHECK}" -s "${FIXTURE}"
  OUTPUT_VARIABLE _out
  ERROR_VARIABLE  _err
  RESULT_VARIABLE _res)

set(_all "${_out}${_err}")
message(STATUS "${_all}")

if(_all MATCHES "leading control points")
  message(FATAL_ERROR
    "${FIXTURE}: cgnscheck reports a principal-vertex ordering violation "
    "(CPEX-0045 S3.2.2). The generator for this fixture must emit the linear "
    "element's corner nodes first, in Figure 1 order.")
endif()

message(STATUS "${FIXTURE}: no ordering violation (cgnscheck exit ${_res})")
