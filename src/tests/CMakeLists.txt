#########
# Tests #
#########

# Link all the executables to cgns and hdf5
if(CGNS_BUILD_SHARED)
  set(TESTS_LK_LIBS cgns_shared)
else()
  set(TESTS_LK_LIBS cgns_static)
endif()

if (CGNS_ENABLE_HDF5 AND HDF5_LIBRARY)
  list(APPEND TESTS_LK_LIBS ${HDF5_LIBRARY})
  if(HDF5_NEED_ZLIB AND ZLIB_LIBRARY)
    list(APPEND TESTS_LK_LIBS ${ZLIB_LIBRARY})
  endif()
  if(HDF5_NEED_SZIP AND SZIP_LIBRARY)
    list(APPEND TESTS_LK_LIBS ${SZIP_LIBRARY})
  endif()
  if(HDF5_NEED_MPI AND MPI_LIBS)
    list(APPEND TESTS_LK_LIBS ${MPI_LIBS})
  endif()
endif ()

if (NOT WIN32)
  list(APPEND TESTS_LK_LIBS m)
endif ()

# Set the files needed by each test
set(cgioc_FILES cgioc.c)
set(dbtest_FILES dbtest.c utils.c)
set(elemtest_FILES elemtest.c)
set(open_cgns_FILES open_cgns.c utils.c)
set(ser_benchmark_hdf5_FILES ser_benchmark_hdf5.c)
set(test_core_vfd_FILES test_core_vfd.c)
set(test_exts_FILES test_exts.c)
set(test_goto_FILES test_goto.c)
set(test_partial_FILES test_partial.c)
set(test_ver31_FILES test_ver31.c)
set(test_multifam_FILES test_multifam.c)
set(test_general_rind_FILES test_general_rind.c)
set(test_general_reshape_FILES test_general_reshape.c)
set(test_bbox_FILES test_bbox.c)
set(write_array_FILES write_array.c utils.c)
set(write_bcdata_FILES write_bcdata.c utils.c)
set(write_links_FILES write_links.c utils.c)
set(write_rind_FILES write_rind.c)
set(write_test_FILES write_test.c)
set(write_zones_FILES write_zones.c utils.c)
set(HO_TESTS
    test_high_order test_hexa_order2 test_3d_elements
    test_high_order_quad test_high_order_hexa test_modal_interpolation
    test_spacetime test_serendipity test_isoparametric test_mixed_ho
)
set(test_family_tree_FILES test_family_tree.c)
set(test_complex_FILES test_complex.c)
set(test_convert_elem_FILES test_convert_elem.c)
set(test_back_comp_FILES test_back_comp.c)
set(test_particle_FILES test_particle.c)
set(test_error_handling_FILES test_error_handling.c)
set(test_variable_order_FILES test_variable_order.c)
set(test_characteristic_length_FILES test_characteristic_length.c)
set(test_degree_zero_FILES test_degree_zero.c)
set(test_distribution_FILES test_distribution.c)
set(test_unicode_FILES test_unicode.c)
#set(cgiof_FILES cgiof.F90)
#set(cgwrite_FILES cgwrite.F90)
#set(cgread_FILES cgread.F90)
#set(cgzconn_FILES cgzconn.F90)
#set(cgsubreg_FILES cgsubreg.F90)
set(cgiof_f03_FILES cgiof_f03.F90)
set(cgwrite_f03_FILES cgwrite_f03.F90)
set(cgread_f03_FILES cgread_f03.F90)
set(cgzconn_f03_FILES cgzconn_f03.F90)
set(cgsubreg_f03_FILES cgsubreg_f03.F90)
set(test_general_rindf_FILES test_general_rindf.F90)
set(test_family_treef_FILES test_family_treef.F90)
set(test_bboxf_FILES test_bboxf.F90)
set(test_complexf_FILES test_complexf.F90)
set(test_particlef_FILES test_particlef.F90 utilsf.F90)
set(test_high_orderf_FILES test_high_orderf.F90)
set(test_cg_npe_f_FILES test_cg_npe_f.F90)
set(test_ho_apif_FILES test_ho_apif.F90)

# HDF5-specific tests
set(cgioc_hdf_FILES cgioc_hdf.c)

# 64-bit tests
set(test64c_FILES test64c.c utils.c)
# Note: test64f.F90 and test64f_f03.F90 use fixed-form syntax despite .F90 extension
# In autotools, these are in a separate FALL64 variable and may not be regularly built
set(test64f_FILES test64f.F90)
set(test64f_f03_FILES test64f_f03.F90)

# Build all C tests whose sources are listed in ${name}_FILES variables above.
set(C_TESTS_LIST
    cgioc dbtest elemtest open_cgns ser_benchmark_hdf5
    test_core_vfd test_exts test_goto test_partial test_ver31
    test_multifam test_general_rind test_general_reshape
    write_array write_bcdata write_links write_rind write_test write_zones
    test_family_tree test_bbox test_complex test_convert_elem
    test_back_comp test_particle test_error_handling test_variable_order
    test_characteristic_length test_degree_zero test_distribution
)
foreach(t IN LISTS C_TESTS_LIST)
    add_executable(${t} ${${t}_FILES})
endforeach()

# High-order tests share utils.c and do not use the _FILES variable pattern.
foreach(HO_TEST IN LISTS HO_TESTS)
    add_executable(${HO_TEST} ${HO_TEST}.c utils.c)
endforeach()
list(APPEND C_TESTS_LIST ${HO_TESTS})

# Conditionally add HDF5-specific tests
if (CGNS_ENABLE_HDF5)
  add_executable( cgioc_hdf ${cgioc_hdf_FILES} )
  list(APPEND C_TESTS_LIST cgioc_hdf)
  # Unicode filepath test requires HDF5 >= 2.0.0
  if (HDF5_VERSION VERSION_GREATER_EQUAL "2.0.0")
    add_executable( test_unicode ${test_unicode_FILES} )
    list(APPEND C_TESTS_LIST test_unicode)
  endif()
endif()

# Conditionally add 64-bit tests
if (CGNS_ENABLE_64BIT)
  add_executable( test64c ${test64c_FILES} )
  list(APPEND C_TESTS_LIST test64c)
endif()

set(SKIP_TESTS
        dbtest)
# Modern Linking
foreach(TGTNAME ${C_TESTS_LIST})
  target_link_libraries(${TGTNAME} PRIVATE ${TESTS_LK_LIBS})
endforeach()

# Conditionally build the fortran tests
if (CGNS_ENABLE_FORTRAN)
	#add_executable( f2c_test           ${f2c_test_FILES}           )
	add_executable( cgiof_f03          ${cgiof_f03_FILES}          )
	add_executable( cgwrite_f03        ${cgwrite_f03_FILES}        )
	add_executable( cgread_f03         ${cgread_f03_FILES}         )
	add_executable( cgzconn_f03        ${cgzconn_f03_FILES}        )
	add_executable( cgsubreg_f03       ${cgsubreg_f03_FILES}       )
	add_executable( test_general_rindf ${test_general_rindf_FILES} )
	add_executable( test_family_treef  ${test_family_treef_FILES}  )
	add_executable( test_bboxf         ${test_bboxf_FILES}         )
        add_executable( test_complexf      ${test_complexf_FILES}      )
        add_executable( test_particlef     ${test_particlef_FILES}     )
        add_executable( test_high_orderf   ${test_high_orderf_FILES}   )
        add_executable( test_cg_npe_f      ${test_cg_npe_f_FILES}      )
        add_executable( test_ho_apif       ${test_ho_apif_FILES}       )

	# 64-bit Fortran tests (conditional on 64-bit support)
	# Both test64f.F90 and test64f_f03.F90 use fixed-form syntax despite .F90 extension
	if (CGNS_ENABLE_64BIT)
	  add_executable( test64f ${test64f_FILES} )
	  set_source_files_properties(${test64f_FILES} PROPERTIES Fortran_FORMAT FIXED)

	  add_executable( test64f_f03 ${test64f_f03_FILES} )
	  set_source_files_properties(${test64f_f03_FILES} PROPERTIES Fortran_FORMAT FIXED)
	endif()
#	if (NOT CGNS_ENABLE_64BIT)
# Don't build the F77 source because they will fail if ENABLE_64BIT is used
#	  add_executable( cgiof       ${cgiof_FILES}  )
#	  add_executable( cgwrite      ${cgwrite_FILES}  )
#	  add_executable( cgread       ${cgread_FILES}   )
#	  add_executable( cgzconn      ${cgzconn_FILES}  )
#	  add_executable( cgsubreg     ${cgsubreg_FILES}  )
#	endif (NOT CGNS_ENABLE_64BIT)
	set(FORTRAN_TESTS_LIST
	cgiof_f03          
	cgwrite_f03        
	cgread_f03         
	cgzconn_f03        
	cgsubreg_f03       
	test_general_rindf 
	test_family_treef  
	test_bboxf
        test_complexf
        test_particlef
        test_high_orderf
        test_cg_npe_f
        test_ho_apif
        )

	# Conditionally add 64-bit Fortran tests
	if (CGNS_ENABLE_64BIT)
	  list(APPEND FORTRAN_TESTS_LIST test64f test64f_f03)
	endif()

	# Modern Linking
	foreach(TGTNAME ${FORTRAN_TESTS_LIST})
	  target_link_libraries(${TGTNAME} PRIVATE ${TESTS_LK_LIBS})
	endforeach()
endif ()

# Add the tests so that ctest can find them
if (CGNS_ENABLE_TESTS)

  file(COPY data DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

  foreach(TARGETNAME ${C_TESTS_LIST})
    if ( ${TARGETNAME} IN_LIST SKIP_TESTS )
      continue()
    endif()
    if (${TARGETNAME} STREQUAL "test64c")
      # Use 2.2 GB to exercise the >2 GB single-array write/read path without
      # requiring excessive disk space on CI runners. -e 0 skips the element
      # connectivity array (which would double file size), -t enables timing output.
      add_test(NAME ${TARGETNAME} COMMAND ${TARGETNAME} -m 2.2g -e 0 -t)
    else()
      add_test(NAME ${TARGETNAME} COMMAND ${TARGETNAME})
    endif()
    set_property (TEST ${TARGETNAME} PROPERTY SKIP_RETURN_CODE 125)
    # On windows help ctest to find the required dlls
    if(CGNS_BUILD_SHARED)
      set_tests_properties(${TARGETNAME} PROPERTIES ENVIRONMENT_MODIFICATION
        "PATH=path_list_prepend:$<$<BOOL:${WIN32}>:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${TARGETNAME}>,\;>>")
    endif()

  endforeach()

  # ------------------------------------------------------------------------
  # Validate the high-order files with cgnscheck, in default and strict mode.
  #
  # This is the test suite's only automated coverage of CPEX-0045 validation
  # defects and of "cgnscheck -s" strict mode.  cgnscheck always exits 0, so
  # failure is detected from its output instead of its return code.
  #
  # Only files that carry a complete grid (coordinates + Elements_t) are listed
  # here; several high-order tests write family-level interpolation nodes only,
  # and cgnscheck rightly reports those as structurally incomplete.
  # ------------------------------------------------------------------------
  set(CGNSCHECK_HO_FILES
      high_order.cgns                  # test_high_order
      test_mixed_ho_simple.cgns        # test_mixed_ho (MIXED: 2 TRI_3 + 1 QUAD_4)
      test_degree_zero_modal.cgns      # test_degree_zero: (p,q)=(0,0) modal
      test_degree_zero_lagrange.cgns   # test_degree_zero: (p,q)=(0,0) Lagrange
      test_degree_zero_spacetime.cgns  # test_degree_zero: (p,q)=(0,2)
      test_dist_match.cgns             # test_distribution: coords match the name
      test_dist_reordered.cgns         # test_distribution: same set, reordered
      test_dist_equi.cgns              # test_distribution: equidistant, named so
      test_dist_wb_p3.cgns             # test_distribution: WarpAndBlend p=3
      test_dist_wb_p3_reord.cgns       # test_distribution: same W&B set, reordered
      test_dist_wb_p4.cgns             # test_distribution: WarpAndBlend p=4
      test_dist_wbtet_p2.cgns          # test_distribution: TETRA W&B p=2
      test_dist_wbtet_p3.cgns          # test_distribution: TETRA W&B p=3
  )
  # A fixture, not DEPENDS: ctest honours FIXTURES_REQUIRED even when the run is
  # filtered with -R, so the producers cannot be skipped, leaving a cgnscheck
  # test to run against a file that was never written.
  set_tests_properties(test_high_order test_mixed_ho test_degree_zero
                       test_distribution
                       PROPERTIES FIXTURES_SETUP HO_TEST_FILES)

  foreach(HOFILE IN LISTS CGNSCHECK_HO_FILES)
    foreach(MODE default strict)
      if (MODE STREQUAL "strict")
        set(_args -s)
        set(_suffix strict)
      else()
        set(_args)
        set(_suffix default)
      endif()
      add_test(NAME cgnscheck_${HOFILE}_${_suffix}
               COMMAND cgnscheck ${_args} ${HOFILE})
      # cgnscheck exits 1 when it finds errors, which ctest treats as a
      # failure; the regex is a second guard in case a diagnostic is ever
      # printed without being counted.  Warnings are tolerated.
      set_tests_properties(cgnscheck_${HOFILE}_${_suffix} PROPERTIES
        FAIL_REGULAR_EXPRESSION "ERROR"
        FIXTURES_REQUIRED HO_TEST_FILES
        TIMEOUT 120)
      if(CGNS_BUILD_SHARED)
        set_tests_properties(cgnscheck_${HOFILE}_${_suffix} PROPERTIES
          ENVIRONMENT_MODIFICATION
          "PATH=path_list_prepend:$<$<BOOL:${WIN32}>:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:cgnscheck>,\;>>")
      endif()
    endforeach()
  endforeach()

  # The coordinate-vs-distribution comparison must FLAG a file whose stored
  # points disagree with the named family.  A check that only ever passes proves
  # nothing, so this asserts the failure directly: strict mode must exit
  # non-zero, while default mode must still succeed -- the coordinates are
  # authoritative, so such a file is conformant and only -s rejects it.
  add_test(NAME cgnscheck_dist_mismatch_strict_fails
           COMMAND cgnscheck -s test_dist_mismatch.cgns)
  set_tests_properties(cgnscheck_dist_mismatch_strict_fails PROPERTIES
    PASS_REGULAR_EXPRESSION "do not match the named distribution"
    FIXTURES_REQUIRED HO_TEST_FILES
    TIMEOUT 120)

  add_test(NAME cgnscheck_dist_wb_bad_strict_fails
           COMMAND cgnscheck -s test_dist_wb_p3_bad.cgns)
  set_tests_properties(cgnscheck_dist_wb_bad_strict_fails PROPERTIES
    PASS_REGULAR_EXPRESSION "do not match the named distribution"
    FIXTURES_REQUIRED HO_TEST_FILES
    TIMEOUT 120)

  add_test(NAME cgnscheck_dist_wbtet_bad_strict_fails
           COMMAND cgnscheck -s test_dist_wbtet_p3_bad.cgns)
  set_tests_properties(cgnscheck_dist_wbtet_bad_strict_fails PROPERTIES
    PASS_REGULAR_EXPRESSION "do not match the named distribution"
    FIXTURES_REQUIRED HO_TEST_FILES
    TIMEOUT 120)

  add_test(NAME cgnscheck_dist_mismatch_default_passes
           COMMAND cgnscheck test_dist_mismatch.cgns)
  set_tests_properties(cgnscheck_dist_mismatch_default_passes PROPERTIES
    PASS_REGULAR_EXPRESSION "do not match the named distribution"
    FIXTURES_REQUIRED HO_TEST_FILES
    TIMEOUT 120)

  # CPEX-0045: Rind_t under GridLocation=InterpolationPoints must be reported.
  # The node's presence is what is non-conformant, whatever its extents, so one
  # file exercises both modes.
  add_test(NAME cgnscheck_ho_rind_flagged
           COMMAND cgnscheck test_ho_rind.cgns)
  set_tests_properties(cgnscheck_ho_rind_flagged PROPERTIES
    PASS_REGULAR_EXPRESSION "Rind_t has no meaning with GridLocation"
    FIXTURES_REQUIRED HO_TEST_FILES
    TIMEOUT 120)

  add_test(NAME cgnscheck_ho_rind_flagged_strict
           COMMAND cgnscheck -s test_ho_rind.cgns)
  set_tests_properties(cgnscheck_ho_rind_flagged_strict PROPERTIES
    PASS_REGULAR_EXPRESSION "Rind_t has no meaning with GridLocation"
    FIXTURES_REQUIRED HO_TEST_FILES
    TIMEOUT 120)

  # CPEX-0045 S3.2.2: the leading LagrangeControlPoints of an
  # ElementInterpolation_t must be the principal vertices of the linear element
  # in Figure 1 order.  These fixtures all violated it until the generators were
  # corrected, so guard against a silent regression.  They cannot be added to
  # CGNSCHECK_HO_FILES above: they are metadata-only files that legitimately
  # report "no element sets found"/"no grid coordinates defined", which would
  # trip that group's blanket FAIL_REGULAR_EXPRESSION "ERROR".  Asserting on the
  # ordering diagnostic specifically is what this rule needs anyway.
  set(CGNSCHECK_ORDERING_FILES
      test_QUAD_16.cgns        # test_high_order_quad
      test_QUAD_25.cgns        # test_high_order_quad
      test_HEXA_64.cgns        # test_high_order_hexa
      test_HEXA_125.cgns       # test_high_order_hexa
      test_hexa27.cgns         # test_hexa_order2
      test_TETRA_20.cgns       # test_3d_elements (bi-unit simplex)
      test_PENTA_18.cgns       # test_3d_elements (bi-unit simplex)
      test_PYRA_14.cgns        # test_3d_elements (bi-unit pyramid)
      test_error_dimensions.cgns  # test_error_handling
      test_error_rewrite.cgns     # test_error_handling
      test_isoparam_ambig.cgns    # test_error_handling
  )
  set_tests_properties(test_high_order_quad test_high_order_hexa
                       test_hexa_order2 test_3d_elements test_error_handling
                       PROPERTIES FIXTURES_SETUP HO_ORDERING_FILES)

  foreach(ORDFILE IN LISTS CGNSCHECK_ORDERING_FILES)
    # Driven through a script wrapper rather than invoking cgnscheck directly:
    # these files exit non-zero for unrelated reasons (see above), which ctest
    # would report as a failure regardless of the rule under test.  The wrapper
    # ignores the exit status and fails only on the ordering diagnostic.
    add_test(NAME cgnscheck_ordering_${ORDFILE}
             COMMAND ${CMAKE_COMMAND}
                     -DCGNSCHECK=$<TARGET_FILE:cgnscheck>
                     -DFIXTURE=${ORDFILE}
                     -P ${CMAKE_CURRENT_SOURCE_DIR}/run_cgnscheck_ordering.cmake)
    set_tests_properties(cgnscheck_ordering_${ORDFILE} PROPERTIES
      FIXTURES_REQUIRED HO_ORDERING_FILES
      TIMEOUT 120)
    if(CGNS_BUILD_SHARED)
      set_tests_properties(cgnscheck_ordering_${ORDFILE} PROPERTIES
        ENVIRONMENT_MODIFICATION
        "PATH=path_list_prepend:$<$<BOOL:${WIN32}>:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:cgnscheck>,\;>>")
    endif()
  endforeach()

  # Every cgnscheck test needs the DLL directory on PATH for a Windows shared
  # build.  Omitting a test here does not fail it cleanly: the loader cannot
  # find cgns.dll, Windows raises a modal error dialog, and the test blocks
  # until ctest kills it at its timeout with no output at all.
  if(CGNS_BUILD_SHARED)
    set_tests_properties(cgnscheck_dist_mismatch_strict_fails
                         cgnscheck_dist_wb_bad_strict_fails
                         cgnscheck_dist_wbtet_bad_strict_fails
                         cgnscheck_dist_mismatch_default_passes
                         cgnscheck_ho_rind_flagged
                         cgnscheck_ho_rind_flagged_strict
      PROPERTIES ENVIRONMENT_MODIFICATION
      "PATH=path_list_prepend:$<$<BOOL:${WIN32}>:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:cgnscheck>,\;>>")
  endif()

  # Conditionally add the fortran tests
  if (CGNS_ENABLE_FORTRAN)
    foreach(TARGETNAME ${FORTRAN_TESTS_LIST})
      add_test(NAME ${TARGETNAME} COMMAND ${TARGETNAME})
      # On windows help ctest to find the required dlls
      if(CGNS_BUILD_SHARED)
        set_tests_properties(${TARGETNAME} PROPERTIES ENVIRONMENT_MODIFICATION
          "PATH=path_list_prepend:$<$<BOOL:${WIN32}>:$<JOIN:$<TARGET_RUNTIME_DLL_DIRS:${TARGETNAME}>,\;>>")
      endif()
    endforeach()

  endif (CGNS_ENABLE_FORTRAN)
endif (CGNS_ENABLE_TESTS)
