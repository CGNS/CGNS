# Compare the C and Fortran ElementTypeName tables element-by-element.
#
# cgnslib.c's ElementTypeName[] and cgns_f.F90's ElementTypeName DATA
# statement are two hand-maintained, independently-ordered lists of the same
# NofValidElementTypes/CGNS_ElementType_MAX_val strings. Nothing enforces
# they stay in sync: a mismatched increment (adding a type to one but not
# the other, or reordering one) leaves the Fortran array silently reading
# out-of-bounds data at the tail, since there is no bounds check on either
# side once C indexes an ElementType_t value into a table sized differently
# than it expects.
#
# Invoked as:
#   cmake -DCGNSLIB_C=<path/to/cgnslib.c> -DCGNS_F90=<path/to/cgns_f.F90> \
#         -P compare_element_type_names.cmake

if(NOT CGNSLIB_C OR NOT CGNS_F90)
  message(FATAL_ERROR "CGNSLIB_C and CGNS_F90 must both be defined")
endif()

file(READ "${CGNSLIB_C}" _c_src)
file(READ "${CGNS_F90}" _f_src)

# --- C side: the initializer between "ElementTypeName[NofValidElementTypes] ="
# and the closing "};". ---
string(REGEX MATCH "ElementTypeName\\[NofValidElementTypes\\][ \t\n]*=[ \t\n]*\\{([^}]*)\\}"
       _c_match "${_c_src}")
if(NOT _c_match)
  message(FATAL_ERROR "Could not find ElementTypeName[] initializer in ${CGNSLIB_C}")
endif()
set(_c_block "${CMAKE_MATCH_1}")
string(REGEX MATCHALL "\"[A-Za-z0-9_]+\"" _c_names_quoted "${_c_block}")

# --- Fortran side: the DATA statement between "DATA ElementTypeName /" and
# the matching closing "/". ---
string(REGEX MATCH "DATA ElementTypeName /([^/]*)/[ \t]*\n" _f_match "${_f_src}")
if(NOT _f_match)
  message(FATAL_ERROR "Could not find DATA ElementTypeName statement in ${CGNS_F90}")
endif()
set(_f_block "${CMAKE_MATCH_1}")
string(REGEX MATCHALL "'[A-Za-z0-9_]+'" _f_names_quoted "${_f_block}")

list(LENGTH _c_names_quoted _c_count)
list(LENGTH _f_names_quoted _f_count)

set(_mismatches 0)
if(NOT _c_count EQUAL _f_count)
  message(STATUS "C table has ${_c_count} entries, Fortran table has ${_f_count}")
  math(EXPR _mismatches "${_mismatches}+1")
endif()

math(EXPR _last "${_c_count}-1")
if(_f_count LESS _c_count)
  set(_last "${_f_count}-1")
  math(EXPR _last "${_last}")
endif()

foreach(_i RANGE 0 ${_last})
  list(GET _c_names_quoted ${_i} _c_name)
  list(GET _f_names_quoted ${_i} _f_name)
  string(REPLACE "\"" "" _c_name "${_c_name}")
  string(REPLACE "'" "" _f_name "${_f_name}")
  if(NOT _c_name STREQUAL _f_name)
    message(STATUS "index ${_i}: C='${_c_name}' Fortran='${_f_name}'")
    math(EXPR _mismatches "${_mismatches}+1")
  endif()
endforeach()

if(_mismatches GREATER 0)
  message(FATAL_ERROR
    "${_mismatches} mismatch(es) between cgnslib.c's ElementTypeName[] "
    "(${_c_count} entries) and cgns_f.F90's ElementTypeName DATA statement "
    "(${_f_count} entries). See messages above.")
endif()

message(STATUS "ElementTypeName tables match: ${_c_count} entries, C and Fortran agree")
