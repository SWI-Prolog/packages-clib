find_package(PkgConfig QUIET)
pkg_check_modules(PC_LIBUUID QUIET ossp-uuid)

find_path(LIBUUID_INCLUDE_DIR
	  NAMES uuid.h
	  PATH_SUFFIXES ossp
	  HINTS ${PC_LIBUUID_INCLUDEDIR}
	        ${PC_LIBUUID_INCLUDE_DIRS})

# MinGW version seems very aggressive finding uuid.h from our version,
# but libuuid.a from the system directory.  As ossp-uuid is not part
# of most defaults, we'll try ignoring this.

string(REPLACE "/include" "/lib" i_libuuid_libdir ${LIBUUID_INCLUDE_DIR})
find_library(UUID_LIBRARY
	     NAMES ossp-uuid uuid
	     HINTS ${i_libuuid_libdir}
	           ${PC_LIBUUID_LIBDIR}
	           ${PC_LIBUUID_LIBRARY_DIRS}
	     NO_CMAKE_SYSTEM_PATH)
if(NOT UUID_LIBRARY)
  find_library(UUID_LIBRARY
	       NAMES ossp-uuid uuid)
endif()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    LibUUID
    REQUIRED_VARS LIBUUID_INCLUDE_DIR UUID_LIBRARY)

mark_as_advanced(LIBUUID_INCLUDE_DIR
		 UUID_LIBRARY)

