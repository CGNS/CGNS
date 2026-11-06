#-----------------------------------------------------------------------------
# Resolve the compression and crypto libraries an HDF5 target links but whose
# own hdf5-config.cmake does not find.
#
# Used when building CGNS (CMakeLists.txt) and when a project finds an
# installed CGNS (cgns-config.cmake), so the rules live in one place.
#
#   cgns_resolve_hdf5_dependencies(<hdf5 target>)
#
# Each block applies only to the HDF5 versions whose package configuration
# misses that dependency, and only when HDF5 names a target that does not
# exist. When the minimum supported HDF5 passes a block's version, delete
# that block.
#-----------------------------------------------------------------------------

macro (cgns_resolve_hdf5_dependencies _cgns_hdf5_tgt)

  # zlib and libaec: HDF5 2.1 and later find these in hdf5-config.cmake.
  # Older versions (or an unknown version) leave them to the consumer.
  # Delete this block once the minimum supported HDF5 is 2.1.
  if (TARGET ${_cgns_hdf5_tgt} AND NOT HDF5_VERSION VERSION_GREATER_EQUAL "2.1.0")
    get_target_property(_hdf5_link_libs ${_cgns_hdf5_tgt} INTERFACE_LINK_LIBRARIES)
    if (_hdf5_link_libs)
      # Normalize with leading/trailing semicolons so delimited patterns always match
      set(_hdf5_link_str ";${_hdf5_link_libs};")

      # Detect ZLIB::ZLIB in link interface — matches both plain references
      # and generator expressions like $<LINK_ONLY:ZLIB::ZLIB>.
      # Check for the target explicitly rather than ZLIB_FOUND — a parent project
      # may have called find_package(ZLIB) in legacy mode (variables only) without
      # creating the ZLIB::ZLIB imported target that HDF5's interface requires.
      if (_hdf5_link_str MATCHES "ZLIB::ZLIB" AND NOT TARGET ZLIB::ZLIB)
        message(STATUS "${_cgns_hdf5_tgt} requires ZLIB::ZLIB, finding ZLIB package")
        # Try module mode first — CMake's FindZLIB always creates ZLIB::ZLIB.
        # Config-mode (ZLIBConfig.cmake from a CMake install of zlib) may export
        # different target names (ZLIB::zlib, ZLIB::zlibstatic) without ZLIB::ZLIB.
        find_package(ZLIB MODULE QUIET)
        if (NOT ZLIB_FOUND)
          # Fallback to config-mode in case module mode failed
          find_package(ZLIB CONFIG QUIET)
        endif ()
        if (NOT TARGET ZLIB::ZLIB AND ZLIB_FOUND)
          # ZLIB was found (config-mode) but without ZLIB::ZLIB — alias a
          # known config-mode target so its properties are inherited correctly.
          if (TARGET ZLIB::zlib)
            add_library(ZLIB::ZLIB ALIAS ZLIB::zlib)
          elseif (TARGET ZLIB::zlibstatic)
            add_library(ZLIB::ZLIB ALIAS ZLIB::zlibstatic)
          endif ()
        endif ()
        if (NOT TARGET ZLIB::ZLIB)
          message(FATAL_ERROR "${_cgns_hdf5_tgt} requires ZLIB::ZLIB but ZLIB was not found")
        endif ()
      endif ()

      # Detect SZIP/libaec in link interface — target names like szip-static or
      # libaec::sz-shared, or bare sz/szip from older HDF5 installations. A
      # static HDF5 target wraps them in $<LINK_ONLY:...>, so unwrap that first.
      # CMake regex does not support \b, so we use ;-delimited alternation on
      # the semicolon-normalized string.
      string(REGEX REPLACE "\\$<LINK_ONLY:([^>]*)>" "\\1" _hdf5_link_bare "${_hdf5_link_str}")
      # libaec 1.1 exports libaec::sz-shared and libaec::sz-static, with
      # libaec::sz only an alias, so check for the exact names HDF5 recorded,
      # and check for the targets rather than libaec_FOUND, as for ZLIB above.
      string(REGEX MATCHALL "libaec::[A-Za-z0-9_-]+" _hdf5_aec_targets "${_hdf5_link_bare}")
      set(_hdf5_aec_missing "")
      foreach (_hdf5_aec_target IN LISTS _hdf5_aec_targets)
        if (NOT TARGET ${_hdf5_aec_target})
          list(APPEND _hdf5_aec_missing ${_hdf5_aec_target})
        endif ()
      endforeach ()
      if (_hdf5_aec_missing OR
          (_hdf5_link_bare MATCHES ";szip-|;szip;|;sz;|;sz-" AND NOT TARGET libaec::sz))
        message(STATUS "${_cgns_hdf5_tgt} requires SZIP, finding libaec package")
        if (_hdf5_aec_missing MATCHES "-static")
          set(libaec_USE_STATIC_LIBS ON)
        endif ()
        find_package(libaec QUIET)
        if (NOT libaec_FOUND)
          find_library(SZIP_LIBRARY NAMES szip sz)
        endif ()
        foreach (_hdf5_aec_target IN LISTS _hdf5_aec_missing)
          if (NOT TARGET ${_hdf5_aec_target})
            message(FATAL_ERROR "${_cgns_hdf5_tgt} requires ${_hdf5_aec_target} but the "
                    "libaec package providing it was not found; set libaec_DIR to the "
                    "directory containing libaec-config.cmake")
          endif ()
        endforeach ()
      endif ()
      unset(_hdf5_link_bare)
      unset(_hdf5_aec_targets)
      unset(_hdf5_aec_missing)
      unset(_hdf5_aec_target)
    endif ()
    unset(_hdf5_link_libs)
    unset(_hdf5_link_str)
  endif ()

  # OpenSSL: HDF5 2.2 can require signed plugins (HDF5_REQUIRE_SIGNED_PLUGINS);
  # its static library then links OpenSSL::Crypto, but its hdf5-config.cmake
  # does not find OpenSSL (HDFGroup/hdf5#6617 fixes that in 2.3.0). Earlier
  # HDF5 has no plugin signing, so this applies to 2.2.x only (or an unknown
  # version). Delete this block once the minimum supported HDF5 is 2.3.
  if (TARGET ${_cgns_hdf5_tgt} AND NOT (HDF5_VERSION AND (HDF5_VERSION VERSION_LESS "2.2.0" OR
                                                         HDF5_VERSION VERSION_GREATER_EQUAL "2.3.0")))
    get_target_property(_hdf5_link_libs ${_cgns_hdf5_tgt} INTERFACE_LINK_LIBRARIES)
    string(REGEX MATCHALL "OpenSSL::[A-Za-z0-9_]+" _hdf5_ssl_targets "${_hdf5_link_libs}")
    foreach (_hdf5_ssl_target IN LISTS _hdf5_ssl_targets)
      if (NOT TARGET ${_hdf5_ssl_target})
        message(STATUS "${_cgns_hdf5_tgt} requires ${_hdf5_ssl_target}, finding OpenSSL package")
        find_package(OpenSSL REQUIRED)
        break ()
      endif ()
    endforeach ()
    unset(_hdf5_link_libs)
    unset(_hdf5_ssl_targets)
    unset(_hdf5_ssl_target)
  endif ()

endmacro ()
