cmake_minimum_required(VERSION 3.5)
project(swipl-clib)

include("../cmake/PrologPackage.cmake")
include(Sockets)
find_package(LibUUID)

check_function_exists(crypt HAVE_CRYPT)
if(NOT HAVE_CRYPT)
  check_library_exists(crypt crypt  "" HAVE_LIBCRYPT)
  if(HAVE_LIBCRYPT)
    set(CRYPT_LIBS -lcrypt)
  else()
    set(CRYPT_SOURCE bsd-crypt.c)
  endif()
endif()

check_function_exists(syslog HAVE_SYSLOG)

AC_CHECK_HEADERS(malloc.h alloca.h unistd.h sys/time.h fcntl.h
		 utime.h execinfo.h sys/resource.h crypt.h syslog.h
		 sys/types.h sys/wait.h sys/stat.h sys/prctl.h
		 netinet/tcp.h crt_externs.h poll.h)

check_type_size("long" SIZEOF_LONG)
check_type_size("long long" SIZEOF_LONG_LONG)

AC_CHECK_FUNCS(setsid strerror utime getrlimit strcasestr vfork _NSGetEnviron
	       pipe2 prctl sysconf poll initgroups setgroups chmod
	       mallinfo malloc_info open_memstream posix_spawn gai_strerror
	       hstrerror)

configure_file(config.h.cmake config.h)

################
# Setup the targets

set(plugins)
set(pllibs)
function(clib_plugin name)
  cmake_parse_arguments(my "" "" "C_SOURCES;PL_LIBS;C_LIBS;C_INCLUDE_DIR" ${ARGN})
  set(plugins ${plugins} ${name}       PARENT_SCOPE)
  set(pllibs  ${pllibs}  ${my_PL_LIBS} PARENT_SCOPE)
  swipl_plugin(${name} ${ARGN})
endfunction()

clib_plugin(cgi        C_SOURCES error.c form.c cgi.c PL_LIBS cgi.pl)
clib_plugin(memfile    C_SOURCES error.c memfile.c    PL_LIBS memfile.pl THREADED)
clib_plugin(time       C_SOURCES error.c time.c       PL_LIBS time.pl THREADED)
clib_plugin(files      C_SOURCES error.c files.c      PL_LIBS filesex.pl)
clib_plugin(process    C_SOURCES error.c process.c    PL_LIBS process.pl)
clib_plugin(streaminfo C_SOURCES error.c streaminfo.c PL_LIBS streaminfo.pl)
if(HAVE_MALLINFO)
clib_plugin(mallocinfo C_SOURCES error.c mallocinfo.c PL_LIBS mallocinfo.pl)
endif()

clib_plugin(uri           C_SOURCES uri.c           PL_LIBS uri.pl THREADED)
clib_plugin(readutil      C_SOURCES readutil.c	    PL_LIBS)
clib_plugin(prolog_stream C_SOURCES prolog_stream.c PL_LIBS prolog_stream.pl)
clib_plugin(md54pl        C_SOURCES md54pl.c md5.c  PL_LIBS md5.pl)

if(UNIX)
clib_plugin(unix   C_SOURCES error.c unix.c   PL_LIBS unix.pl)
clib_plugin(uid    C_SOURCES error.c uid.c    PL_LIBS uid.pl)
clib_plugin(rlimit C_SOURCES error.c rlimit.c PL_LIBS rlimit.pl)
clib_plugin(syslog C_SOURCES syslog.c         PL_LIBS syslog.pl)
endif(UNIX)

if(LIBUUID_FOUND)
  clib_plugin(uuid
	      C_SOURCES uuid.c
	      C_LIBS ${UUID_LIBRARY}
	      C_INCLUDE_DIR ${LIBUUID_INCLUDE_DIR}
	      PL_LIBS uuid.pl)
endif()

clib_plugin(crypt
	     C_SOURCES error.c crypt.c md5.c md5passwd.c ${CRYPT_SOURCE}
	     THREADED C_LIBS ${CRYPT_LIBS}
	     PL_LIBS crypt.pl)
clib_plugin(hashstream
	     C_SOURCES hash_stream.c md5.c sha1/sha1.c sha1/sha2.c
	     PL_LIBS hash_stream.pl)
clib_plugin(sha4pl
	     C_SOURCES error.c sha4pl.c sha1/sha1.c sha1/sha2.c
	               sha1/hmac_sha1.c sha1/hmac_sha256.c
	     PL_LIBS sha.pl)

if(HAVE_SOCKET)
clib_plugin(
    socket
    C_SOURCES error.c socket.c nonblockio.c
    C_LIBS ${SOCKET_LIBRARIES}
    PL_LIBS socket.pl streampool.pl prolog_server.pl udp_broadcast.pl)
endif(HAVE_SOCKET)

################
# Tests

test_libs(cgi crypt memfile process readutil socket stream time uri)

################
# Documentation

# Files that have PlDoc comments whose TeX file is included in clib.tex
set(pldoc_files process.pl uri.pl filesex.pl uid.pl udp_broadcast.pl
    uuid.pl unix.pl syslog.pl socket.pl prolog_stream.pl md5.pl
    hash_stream.pl)

# Filter the ones we will build
set(pldoc_ok)
foreach(f ${pldoc_files})
  list(FIND pllibs ${f} idx)		# IN_LIST is cmake >= 3.3
  if(NOT idx EQUAL -1)
    set(pldoc_ok ${pldoc_ok} ${f})
  endif()
endforeach()

add_custom_target(clib)
add_dependencies(clib ${plugins})

pkg_doc(clib
	SOURCES ${pldoc_ok}
	DEPENDS ${plugins})
