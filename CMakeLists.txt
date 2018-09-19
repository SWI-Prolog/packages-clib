cmake_minimum_required(VERSION 2.8.12)
project(swipl-clib)

include("../cmake/PrologPackage.cmake")
include(Sockets)
find_package(LibUUID)

check_library_exists(crypt crypt  "" HAVE_LIBCRYPT)
if(HAVE_LIBCRYPT)
  set(CRYPT_LIBS -lcrypt)
else()
  set(CRYPT_SOURCE bsd-crypt.c)
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
	       mallinfo malloc_info open_memstream)

configure_file(config.h.cmake config.h)

################
# Setup the targets

swipl_plugin(cgi        C_SOURCES error.c form.c cgi.c PL_LIBS cgi.pl)
swipl_plugin(memfile    C_SOURCES error.c memfile.c    PL_LIBS memfile.pl)
swipl_plugin(time       C_SOURCES error.c time.c       PL_LIBS time.pl)
swipl_plugin(files      C_SOURCES error.c files.c      PL_LIBS filesex.pl)
swipl_plugin(process    C_SOURCES error.c process.c    PL_LIBS process.pl)
swipl_plugin(streaminfo C_SOURCES error.c streaminfo.c PL_LIBS streaminfo.pl)
if(HAVE_MALLINFO)
swipl_plugin(mallocinfo C_SOURCES error.c mallocinfo.c PL_LIBS mallocinfo.pl)
endif()

swipl_plugin(uri           C_SOURCES uri.c           PL_LIBS uri.pl)
swipl_plugin(readutil      C_SOURCES readutil.c	     PL_LIBS)
swipl_plugin(prolog_stream C_SOURCES prolog_stream.c PL_LIBS prolog_stream.pl)
swipl_plugin(md54pl        C_SOURCES md54pl.c md5.c  PL_LIBS md5.pl)

if(UNIX)
swipl_plugin(unix   C_SOURCES error.c unix.c   PL_LIBS unix.pl)
swipl_plugin(uid    C_SOURCES error.c uid.c    PL_LIBS uid.pl)
swipl_plugin(rlimit C_SOURCES error.c rlimit.c PL_LIBS rlimit.pl)
swipl_plugin(syslog C_SOURCES syslog.c         PL_LIBS syslog.pl)
endif(UNIX)

if(LIBUUID_FOUND)
swipl_plugin(uuid
	     C_SOURCES uuid.c
	     C_LIBS ${UUID_LIBRARY}
	     PL_LIBS uuid.pl)
target_include_directories(plugin_uuid BEFORE PRIVATE
			   ${LIBUUID_INCLUDE_DIR})
endif()

swipl_plugin(crypt
	     C_SOURCES error.c crypt.c md5.c md5passwd.c ${CRYPT_SOURCE}
	     C_LIBS ${CRYPT_LIBS}
	     PL_LIBS crypt.pl)
swipl_plugin(hashstream
	     C_SOURCES hash_stream.c md5.c sha1/sha1.c sha1/sha2.c
	     PL_LIBS hash_stream.pl)
swipl_plugin(sha4pl
	     C_SOURCES error.c sha4pl.c sha1/sha1.c sha1/sha2.c
	               sha1/hmac_sha1.c sha1/hmac_sha256.c
	     PL_LIBS sha.pl)

if(HAVE_SOCKET)
swipl_plugin(
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

pkg_doc(clib
	SOURCES process.pl uri.pl filesex.pl uid.pl udp_broadcast.pl
	        uuid.pl unix.pl syslog.pl socket.pl prolog_stream.pl md5.pl
	        hash_stream.pl)
