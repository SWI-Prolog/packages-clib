cmake_minimum_required(VERSION 2.8.12)
project(swipl-clib)

include("../cmake/PrologPackage.cmake")
include(CheckTypeSize)

check_library_exists(socket    socket      "" HAVE_LIBSOCKET)
check_library_exists(nsl       gethostent  "" HAVE_LIBNSL)

if(NOT DEFINED LIB_UUID)
  message("-- Looking for UUID library and headers")
  check_library_exists(ossp-uuid uuid_create "" HAVE_LIBOSSP_UUID)
  if(HAVE_LIBOSSP_UUID)
    set(LIB_UUID -lossp-uuid)
  else()
    check_library_exists(uuid    uuid_create "" HAVE_LIBUUID)
    if(HAVE_LIBUUID)
      set(LIB_UUID -luuid)
    endif()
  endif()
  set(LIB_UUID ${LIB_UUID} CACHE INTERNAL "Name of UUID library")
endif(NOT DEFINED LIB_UUID)

if(NOT DEFINED UUID_H)
  check_include_file(ossp/uuid.h HAVE_OSSP_UUID_H)
  if(HAVE_OSSP_UUID_H)
    set(UUID_H "<ossp/uuid.h>")
  else()
    check_include_file(uuid.h HAVE_UUID_H)
    if(HAVE_UUID_H)
      set(UUID_H "<uuid.h>")
    endif()
    set(UUID_H ${UUID_H} CACHE INTERNAL "UUID header")
  endif()
endif(NOT DEFINED UUID_H)

check_library_exists(crypt crypt  "" HAVE_LIBCRYPT)

check_function_exists(syslog HAVE_SYSLOG)

if(HAVE_LIBSOCKET)
  set(HAVE_SOCKET 1)
else()
  check_function_exists(socket HAVE_SOCKET)
endif()

if(HAVE_SOCKET)
AC_CHECK_HEADERS(sys/socket.h)
if(HAVE_SYS_SOCKET_H)
  set(CMAKE_EXTRA_INCLUDE_FILES ${CMAKE_EXTRA_INCLUDE_FILES} sys/socket.h)
endif()

check_type_size(socklen_t SIZEOF_SOCKLEN_T)
if(NOT SIZEOF_SOCKLEN_T STREQUAL "")
  set(HAVE_SOCKLEN_T 1)
endif()

check_c_source_compiles(
    "#include <sys/types.h>
     #include <sys/socket.h>
     #include <netdb.h>
     #include <netinet/in.h>

     int main() { int x = h_errno; return 0;}"
    HAVE_H_ERRNO)
endif(HAVE_SOCKET)

check_c_source_compiles(
    "#define _XOPEN_SOURCE
     #include <unistd.h>
     int main() { return 0;}"
    DEFINE_XOPEN_SOURCE)

AC_CHECK_HEADERS(malloc.h alloca.h unistd.h sys/time.h fcntl.h
		 utime.h execinfo.h sys/resource.h crypt.h syslog.h
		 sys/types.h sys/wait.h sys/stat.h sys/prctl.h
		 netinet/tcp.h crt_externs.h poll.h)

check_type_size("long" SIZEOF_LONG)
check_type_size("long long" SIZEOF_LONG_LONG)

AC_CHECK_FUNCS(setsid strerror utime getrlimit strcasestr vfork _NSGetEnviron
	       pipe2 prctl sysconf poll initgroups setgroups chmod
	       mallinfo malloc_info open_memstream)

configure_file(config.h.cmake config.h)

################
# Setup the targets

if(HAVE_SOCKET)
if(HAVE_LIBSOCKET)
  set(SOCKET_LIBRARIES ${SOCKET_LIBRARIES} -lsocket)
endif()
if(HAVE_LIBNSL)
  set(SOCKET_LIBRARIES ${SOCKET_LIBRARIES} -lnsl)
endif()

add_library(libsocket MODULE error.c socket.c nonblockio.c)
set_target_properties(libsocket PROPERTIES OUTPUT_NAME "socket" PREFIX "")
target_link_libraries(libsocket ${SOCKET_LIBRARIES} ${SWIPL_LIBRARIES})

install(TARGETS libsocket
	LIBRARY DESTINATION ${SWIPL_INSTALL_MODULES})
install(FILES socket.pl
	DESTINATION ${SWIPL_INSTALL_LIBRARY})
endif(HAVE_SOCKET)
