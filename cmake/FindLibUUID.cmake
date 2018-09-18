find_package(PkgConfig QUIET)
pkg_check_modules(PC_LIBUUID QUIET ossp-uuid)

find_path(LIBUUID_INCLUDE_DIR
	  NAMES uuid.h
	  PATH_SUFFIXES ossp
	  HINTS ${PC_LIBUUID_INCLUDEDIR}
	        ${PC_LIBUUID_INCLUDE_DIRS})
find_library(UUID_LIBRARY
	     NAMES ossp-uuid uuid
	     HINTS ${PC_LIBXML_LIBDIR}
	           ${PC_LIBXML_LIBRARY_DIRS})

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    LibUUID
    REQUIRED_VARS LIBUUID_INCLUDE_DIR UUID_LIBRARY)

mark_as_advanced(LIBUUID_INCLUDE_DIR
		 UUID_LIBRARY)

