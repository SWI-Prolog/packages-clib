dnl Process this file with autoconf to produce a configure script.

AC_INIT(install-sh)
AC_PREREQ([2.50])
AC_CONFIG_HEADER(config.h)

AC_SUBST(TARGETS)
AC_SUBST(PLTARGETS)
AC_SUBST(NETLIBS)
AC_SUBST(CRYPTLIBS)
AC_SUBST(CRYPTOBJ)
AC_SUBST(LIBUUID)
AC_SUBST(LIBUUID_CFLAGS)
AC_SUBST(LIBUUID_LDFLAGS)

m4_include([../ac_swi_c.m4])

TARGETS="socket.$SO cgi.$SO memfile.$SO files.$SO crypt.$SO time.$SO"

case "$PLARCH" in
    *-win32|*-win64)
        AC_CHECK_LIB(ws2_32, main,
                     [NETLIBS="$NETLIBS -lws2_32"])
        AC_CHECK_LIB(gdi32, main,
                     [NETLIBS="$NETLIBS -lgdi32"])
        ;;
    *)
        TARGETS="$TARGETS uid.$SO unix.$SO"
        ;;
esac

AC_CHECK_FUNC(socket, [], [
             AC_CHECK_LIB(socket, socket,
                      [NETLIBS="$NETLIBS -lsocket"; LIBS="$LIBS -lsocket"]
                      AC_DEFINE(HAVE_LIBSOCKET, 1,
                      "Define if you have the socket library (-lsocket)."))])
AC_CHECK_FUNC(gethostent, [], [
             AC_CHECK_LIB(nsl, gethostent,
                      [NETLIBS="$NETLIBS -lnsl"]
                      AC_DEFINE(HAVE_LIBNSL, 1,
                      "Define if you have the nsl library (-lnsl)."))])

dnl Configure UUID library.  We use the OSSP UUID library, but it seems
dnl to be installed in various places.

AC_MSG_CHECKING(ossp-uuid)
AC_PATH_PROG(UUID_CONFIG, uuid-config, :)
if test "$UUID_CONFIG" != ":"; then
  LIBUUID="$($UUID_CONFIG --libs)"
  LIBUUID_CFLAGS="$($UUID_CONFIG --cflags)"
  LIBUUID_LDFLAGS="$($UUID_CONFIG --ldflags)"

  oldlibs="$LIBS"
  uuidlib=$(echo $LIBUUID | sed 's/-l//')
  AC_CHECK_LIB($uuidlib, uuid_create,
	       [ TARGETS="$TARGETS uuid.$SO"
		 AC_MSG_RESULT(OSSP-UUID at $LIBUUID_CFLAGS $LIBUUID_LDFLAGS $LIBUUID)
	       ],
	       [ AC_MSG_RESULT(no)
	       ])
  LIBS=$oldlibs
else
  AC_MSG_RESULT(no)
fi

oldlibs="$LIBS"
AC_CHECK_LIB(crypt, crypt)
CRYPTLIBS="$LIBS"
AC_CHECK_FUNC(crypt,
	      [],
	      [CRYPTOBJ=bsd-crypt.o])
AC_DEFINE(HAVE_CRYPT, 1,
	  "Define if you have the crypt function.")
LIBS="$oldlibs"

AC_CHECK_FUNC(syslog,
	      [ TARGETS="$TARGETS syslog.$SO" ],
	      AC_MSG_WARN([Cannot find syslog"()" -- dropping syslog.pl]))

AC_MSG_CHECKING(h_errno)
AC_TRY_COMPILE(
[
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
],
[ int x = h_errno;
], AC_DEFINE(HAVE_H_ERRNO, 1,
	     [Define of h_errno is provided])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

AC_CHECK_HEADERS(malloc.h alloca.h unistd.h sys/time.h fcntl.h utime.h)
AC_CHECK_HEADERS(execinfo.h sys/resource.h crypt.h syslog.h)
AC_CHECK_HEADERS(sys/types.h sys/wait.h sys/stat.h sys/prctl.h)
AC_CHECK_HEADERS(netinet/tcp.h crt_externs.h poll.h)

AC_CHECK_SIZEOF(long, 4)
AC_CHECK_SIZEOF(long long, 8)

AC_CHECK_FUNCS(setsid strerror utime getrlimit strcasestr vfork _NSGetEnviron)
AC_CHECK_FUNCS(pipe2 prctl sysconf poll initgroups setgroups chmod)
AC_CHECK_FUNCS(mallinfo malloc_info open_memstream)

AC_CHECK_TYPES(socklen_t, [], [],
[
#include <sys/types.h>
#include <sys/socket.h>
])
AC_CHECK_TYPES(ssize_t, [], [],
[
#include <sys/types.h>
#include <sys/socket.h>
])

AC_MSG_CHECKING(_XOPEN_SOURCE)
AC_TRY_COMPILE(
[
#define _XOPEN_SOURCE
#include <unistd.h>
],
[
], AC_DEFINE(DEFINE_XOPEN_SOURCE, 1,
	     "Define if _XOPEN_SOURCE is needed")
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

with_time=yes
AC_ARG_WITH(time, [  --without-time		  Exclude alarm library],
	    [case "$withval" in
		yes)	with_time=yes ;;
		no)     with_time=no ;;
	     esac])

if test "$ac_cv_func_getrlimit" = "yes"; then
  TARGETS="$TARGETS rlimit.$SO"
else
  AC_MSG_WARN([Cannot find getrlimit"()" -- dropping rlimit.pl])
fi

if test "$ac_cv_func_mallinfo" = "yes" -o \
        \( "$ac_cv_func_malloc_info" = "yes" -a  \
           "$ac_cv_func_open_memstream" = "yes" \) ; then
  TARGETS="$TARGETS mallocinfo.$SO"
fi

PLTARGETS=`echo $TARGETS | sed -e "s/\.$SO/.pl/g" -e "s/files.pl/filesex.pl/"`
if echo "$PLTARGETS" | grep socket.pl 2>&1 >/dev/null; then
  PLTARGETS="$PLTARGETS streampool.pl"
fi


dnl setpgid support
AC_CHECK_FUNC([setpgid],
   AC_DEFINE(HAVE_SETPGID,1,[Define to 1 if you have the `setpgid' function.]))

dnl setpgrp support
AC_CHECK_FUNC([setpgrp],
   AC_DEFINE(HAVE_SETPGRP,1,[Define to 1 if you have the `setpgrp' function.]))

dnl AC_FUNC_SETPGRP does not work if cross compiling
dnl Instead, assume we will have a prototype for setpgrp if cross compiling.
if test "$cross_compiling" = no; then
  AC_FUNC_SETPGRP
else
  AC_CACHE_CHECK([whether setpgrp takes no argument], ac_cv_func_setpgrp_void,
    [AC_TRY_COMPILE([
#include <unistd.h>
], [
  if (setpgrp(1,1) == -1)
    exit (0);
  else
    exit (1);
], ac_cv_func_setpgrp_void=no, ac_cv_func_setpgrp_void=yes)])
if test $ac_cv_func_setpgrp_void = yes; then
  AC_DEFINE(SETPGRP_VOID, 1)
fi
fi

AC_OUTPUT(Makefile)



