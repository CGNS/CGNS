#######################
# Fortran Examples    #
#######################

# Link all the executables to cgns and hdf5
if(CGNS_BUILD_SHARED)
  set(FORTRAN_EXAMPLES_LK_LIBS cgns_shared)
else()
  set(FORTRAN_EXAMPLES_LK_LIBS cgns_static)
endif()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND FORTRAN_EXAMPLES_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND FORTRAN_EXAMPLES_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND FORTRAN_EXAMPLES_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND FORTRAN_EXAMPLES_LK_LIBS ${MPI_LIBS})
  endif()
endif ()

if (NOT WIN32)
  list(APPEND FORTRAN_EXAMPLES_LK_LIBS m)
endif ()

# Prefix to avoid naming conflicts with other executables
set(EXAMPLE_PREFIX "fortran_ex_")

# Define test directories and their programs
# Format: "directory:program1,program2,program3,...|write_prog|read_prog"
# Last two fields are the write and read programs for testing
# Note: Test_ADF is excluded as it's not in autotools DIRS list
set(EXAMPLE_PROGRAMS
  "Test1:cgwrite,cgread,cgmodify|cgwrite|cgread"
  "Test_1to1ConnProp:cgwrite,cgread|cgwrite|cgread"
  "Test_Axisym:cgwrite,cgread,cgmodify,cgdelete|cgwrite|cgread"
  "Test_BCProperty:cgwrite,cgread,cgmodify,cgdelete|cgwrite|cgread"
  "Test_ConnProperty:cgwrite,cgread,cgmodify,cgdelete|cgwrite|cgread"
  "Test_EM:cgwrite,cgread|cgwrite|cgread"
  "Test_Family:cgwrite,cgread|cgwrite|cgread"
  "Test_Gravity:cgwrite,cgread,cgmodify,cgdelete|cgwrite|cgread"
  "Test_Links:cgwrite,cgread|cgwrite|cgread"
  "Test_PartRdWr:cgwrite,cgread|cgwrite|cgread"
  "Test_RotatingCoordinates:cgwrite,cgread|cgwrite|cgread"
  "Test_UD_BCData:cgwrite,cgread|cgwrite|cgread"
  "Test_UserDefinedData:cgwrite,cgread|cgwrite|cgread"
  "Test_cgio:cgiotest||cgiotest"
  "Test_chemistry:cgwrite,cgread|cgwrite|cgread"
  "Test_mixed_elements:cgwrite,cgread,cgmodify|cgwrite|cgread"
  "Test_mixed_grid:cgwrite,cgread,cgmodify|cgwrite|cgread"
  "Test_motion:cgwrite,cgread,cgmodify|cgwrite|cgread"
)

# Build all example programs
foreach(ENTRY ${EXAMPLE_PROGRAMS})
  # Parse "directory:program1,program2,...|write_prog|read_prog"
  string(REPLACE "|" ";" ENTRY_PARTS ${ENTRY})
  list(GET ENTRY_PARTS 0 DIR_AND_PROGRAMS)
  list(GET ENTRY_PARTS 1 WRITE_PROG)
  list(GET ENTRY_PARTS 2 READ_PROG)

  # Split directory and programs
  string(REPLACE ":" ";" DIR_PARTS ${DIR_AND_PROGRAMS})
  list(GET DIR_PARTS 0 DIR)
  list(GET DIR_PARTS 1 PROGRAMS)

  # Split programs by comma
  string(REPLACE "," ";" PROGRAM_LIST ${PROGRAMS})

  foreach(PROGRAM ${PROGRAM_LIST})
    # Construct source file path
    set(SOURCE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${DIR}/${PROGRAM}.F90")

    # Create unique target name: fortran_ex_Test1_cgwrite
    set(TARGET_NAME "${EXAMPLE_PREFIX}${DIR}_${PROGRAM}")

    # Build executable
    add_executable(${TARGET_NAME} ${SOURCE_FILE})
    target_link_libraries(${TARGET_NAME} PRIVATE ${FORTRAN_EXAMPLES_LK_LIBS})

    # Set output name to match original (without prefix)
    # This allows examples to be run in their directories if needed
    set_target_properties(${TARGET_NAME} PROPERTIES OUTPUT_NAME ${PROGRAM})

    # Set runtime output directory to separate by test directory
    set_target_properties(${TARGET_NAME} PROPERTIES
      RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${DIR}")
  endforeach()
endforeach()

# Add CTest integration for regression testing
if (CGNS_ENABLE_TESTS)
  # Copy OUTPUT reference files to build directory
  foreach(ENTRY ${EXAMPLE_PROGRAMS})
    # Parse directory name
    string(REPLACE "|" ";" ENTRY_PARTS ${ENTRY})
    list(GET ENTRY_PARTS 0 DIR_AND_PROGRAMS)
    string(REPLACE ":" ";" DIR_PARTS ${DIR_AND_PROGRAMS})
    list(GET DIR_PARTS 0 DIR)

    # Copy OUTPUT reference file if it exists
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${DIR}/OUTPUT")
      file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/${DIR}/OUTPUT"
           DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/${DIR}")
    endif()
  endforeach()

  # Add individual test for each example directory
  foreach(ENTRY ${EXAMPLE_PROGRAMS})
    # Parse "directory:program1,program2,...|write_prog|read_prog"
    string(REPLACE "|" ";" ENTRY_PARTS ${ENTRY})
    list(GET ENTRY_PARTS 0 DIR_AND_PROGRAMS)
    list(GET ENTRY_PARTS 1 WRITE_PROG)
    list(GET ENTRY_PARTS 2 READ_PROG)

    # Split directory and programs
    string(REPLACE ":" ";" DIR_PARTS ${DIR_AND_PROGRAMS})
    list(GET DIR_PARTS 0 DIR)

    # Skip if no write/read programs defined (empty test)
    if(NOT WRITE_PROG AND NOT READ_PROG)
      continue()
    endif()

    # Determine test executable paths
    if(WRITE_PROG)
      set(WRITE_EXE "$<TARGET_FILE:${EXAMPLE_PREFIX}${DIR}_${WRITE_PROG}>")
    else()
      set(WRITE_EXE "")
    endif()

    if(READ_PROG)
      set(READ_EXE "$<TARGET_FILE:${EXAMPLE_PREFIX}${DIR}_${READ_PROG}>")
    else()
      set(READ_EXE "")
    endif()

    # Reference OUTPUT file path
    set(REF_OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${DIR}/OUTPUT")

    # Work directory for test execution
    set(WORK_DIR "${CMAKE_CURRENT_BINARY_DIR}/${DIR}")

    # Add test using the CMake test script
    # Note: Test_ADF and Test_cgio only run a single program, so they need special handling
    if(DIR STREQUAL "Test_ADF" OR DIR STREQUAL "Test_cgio")
      # For single-program tests, just run the program and compare output
      add_test(NAME fortran_ex_${DIR}
               COMMAND ${CMAKE_COMMAND}
                 -DWRITE_EXE=${READ_EXE}
                 -DREAD_EXE=${READ_EXE}
                 -DREF_OUTPUT=${REF_OUTPUT}
                 -DWORK_DIR=${WORK_DIR}
                 -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fortran_example_test.cmake)
    else()
      # For write/read tests, run write then read
      add_test(NAME fortran_ex_${DIR}
               COMMAND ${CMAKE_COMMAND}
                 -DWRITE_EXE=${WRITE_EXE}
                 -DREAD_EXE=${READ_EXE}
                 -DREF_OUTPUT=${REF_OUTPUT}
                 -DWORK_DIR=${WORK_DIR}
                 -P ${CMAKE_CURRENT_SOURCE_DIR}/run_fortran_example_test.cmake)
    endif()

    # Set test properties for Windows DLL paths if using shared libraries
    if(CGNS_BUILD_SHARED AND WIN32)
      # Collect all DLL directories from write and read executables
      # Prefer WRITE_PROG if available, otherwise use READ_PROG
      if(WRITE_PROG)
        set_tests_properties(fortran_ex_${DIR} PROPERTIES ENVIRONMENT_MODIFICATION
          "PATH=path_list_prepend:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${EXAMPLE_PREFIX}${DIR}_${WRITE_PROG}>,\;>")
      elseif(READ_PROG)
        set_tests_properties(fortran_ex_${DIR} PROPERTIES ENVIRONMENT_MODIFICATION
          "PATH=path_list_prepend:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${EXAMPLE_PREFIX}${DIR}_${READ_PROG}>,\;>")
      endif()
    endif()
  endforeach()
endif (CGNS_ENABLE_TESTS)
