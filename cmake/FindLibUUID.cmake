if(NOT UUID_LIBRARY)

if(EXISTS ${LibUUID_ROOT}/bin/uuid-config)
  set(UUID_CONFIG ${LibUUID_ROOT}/bin/uuid-config CACHE FILEPATH
      "Path name for ossp-uuid library config tool")
else()
  find_program(
      UUID_CONFIG
      uuid-config
      PATH_SUFFIXES ""
      DOC "OSSP uuid config tool")
endif()
mark_as_advanced(UUID_CONFIG)

if(UUID_CONFIG)
  message("-- Using ${UUID_CONFIG} to find ossp-uuid")
  execute_process(COMMAND ${UUID_CONFIG} --version OUTPUT_VARIABLE UUID_VERSION)
  if(UUID_VERSION MATCHES "OSSP")
    execute_process(COMMAND ${UUID_CONFIG} --includedir
		    OUTPUT_VARIABLE LIBUUID_INCLUDE_DIR)
    execute_process(COMMAND ${UUID_CONFIG} --libdir
		    OUTPUT_VARIABLE LIBUUID_LIBRARY_DIR)
    execute_process(COMMAND ${UUID_CONFIG} --libs
		    OUTPUT_VARIABLE LIBUUID_LIBFLAG)

    string(REPLACE "-l" "" LIBUUID_LIB ${LIBUUID_LIBFLAG})
    find_library(UUID_LIBRARY
		 NAMES ${LIBUUID_LIB}
		 PATHS ${LIBUUID_LIBRARY_DIR}
		 NO_DEFAULT_PATH)
    if(NOT UUID_LIBRARY AND LIBUUID_LIB MATCHES "ossp")
      find_library(UUID_LIBRARY
		   NAMES ${LIBUUID_LIB}
		   PATHS ${LIBUUID_LIBRARY_DIR})
    endif()

    set(LIBUUID_INCLUDE_DIR ${LIBUUID_INCLUDE_DIR} CACHE INTERNAL
	"Directory holding OSSP UUID <uuid.h>")
  else()
    set(UUID_CONFIG "")
  endif()
endif()

if(NOT UUID_CONFIG)

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

endif(NOT UUID_CONFIG)

mark_as_advanced(LIBUUID_INCLUDE_DIR
		 UUID_LIBRARY)

endif(NOT UUID_LIBRARY)

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    LibUUID
    REQUIRED_VARS LIBUUID_INCLUDE_DIR UUID_LIBRARY)
