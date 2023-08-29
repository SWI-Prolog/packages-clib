/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2012, University of Amsterdam
                              Vu University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <config.h>

#include <SWI-Prolog.h>
#include "clib.h"
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#ifdef HAVE_UTIME_H
#include <utime.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#ifdef __WINDOWS__
#ifndef WINVER
#define WINVER 0x0501
#endif

#define MAKE_FUNCTORS 1
#include <windows.h>
#include "win_error.c"
#define statstruct struct _stati64
#define statfunc _wstati64
#ifdef HAVE_UTIME
#include <sys/utime.h>
#define utimestruct struct _utimbuf
#define utimefunc _wutime
#endif
#define FCHAR wchar_t
#define PL_get_file_name PL_get_file_nameW
#include <winbase.h>
#include <io.h>

#else /*__WINDOWS__*/

#define statstruct struct stat
#define statfunc stat
#define utimestruct struct utimbuf
#define utimefunc utime
#define FCHAR char

#endif /*__WINDOWS__*/

static functor_t FUNCTOR_access1;
static functor_t FUNCTOR_modified1;
static functor_t FUNCTOR_changed1;
static atom_t    ATOM_now;
static atom_t    ATOM_hard;
static atom_t    ATOM_symbolic;


		 /*******************************
		 *	       TIME		*
		 *******************************/

static int
add_time_option(term_t list, functor_t f, time_t time)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  while(PL_get_list(tail, head, tail))
  { if ( PL_unify_functor(head, f) )
    { term_t a = PL_new_term_ref();

      return (PL_get_arg(1, head, a) &&
	      PL_unify_float(a, (double)time));
    }
  }

  if ( PL_unify_list(tail, head, tail) )
    return PL_unify_term(head, PL_FUNCTOR, f, PL_FLOAT, (double)time);

  return FALSE;
}


static int
close_list(term_t list)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  while(PL_get_list(tail, head, tail))
    ;

  return PL_unify_nil(tail);
}


static int
get_time_option(term_t list, functor_t f, time_t def, time_t *tme)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  while(PL_get_list(tail, head, tail))
  { if ( PL_is_functor(head, f) )
    { term_t a = PL_new_term_ref();
      double f;

      _PL_get_arg(1, head, a);
      if ( !PL_get_float(a, &f) )
      { atom_t now;

	if ( PL_get_atom(a, &now) && now == ATOM_now )
	{ time(tme);
	  return TRUE;
	} else
	  return pl_error(NULL, 0, NULL, ERR_TYPE, a, "time");
      }
      *tme = (long)f;
      return TRUE;
    }
  }

  *tme = def;
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
set_file_time(+Spec, -Old, +New)
    Query/set file-times.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_set_time_file(term_t spec, term_t old, term_t new)
{ FCHAR *name;
  statstruct sbuf;

  if ( !PL_get_file_name(spec, &name, 0) )
    return FALSE;

  if ( statfunc(name, &sbuf) )
    return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "stat", "file", spec);

  add_time_option(old, FUNCTOR_access1,   sbuf.st_atime);
  add_time_option(old, FUNCTOR_modified1, sbuf.st_mtime);
  add_time_option(old, FUNCTOR_changed1,  sbuf.st_ctime);
  close_list(old);

  if ( !PL_get_nil(new) )
#ifdef HAVE_UTIME
  { utimestruct tbuf;

    if ( !get_time_option(new, FUNCTOR_access1,
			  sbuf.st_atime, &tbuf.actime) )
      return FALSE;
    if ( !get_time_option(new, FUNCTOR_modified1,
			  sbuf.st_mtime, &tbuf.modtime) )
      return FALSE;

    if ( utimefunc(name, &tbuf) != 0 )
      return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "set_time", "file", spec);
  }
#elif defined (__WINDOWS__)
  { FILETIME ctime;
    FILETIME atime;
    FILETIME mtime;
    time_t tmp;
    BOOL rc;
    LONGLONG ll;
    HANDLE hFile;

    if ( !get_time_option(new, FUNCTOR_access1,
                          sbuf.st_atime, &tmp) )
       return FALSE;
    ll = Int32x32To64(tmp, 10000000) + 116444736000000000LL;
    atime.dwLowDateTime = (DWORD)ll;
    atime.dwHighDateTime = ll >> 32;
    if ( !get_time_option(new, FUNCTOR_modified1,
                          sbuf.st_mtime, &tmp) )
       return FALSE;
    ll = Int32x32To64(tmp, 10000000) + 116444736000000000;
    mtime.dwLowDateTime = (DWORD)ll;
    mtime.dwHighDateTime = ll >> 32;
    hFile = CreateFileW((LPCWSTR)name,
                        GENERIC_WRITE,
                        FILE_SHARE_READ | FILE_SHARE_WRITE,
                        NULL,
                        OPEN_EXISTING,
                        FILE_ATTRIBUTE_NORMAL ,
                        0);
    if ( hFile == INVALID_HANDLE_VALUE )
       return pl_error(NULL, 0, NULL, ERR_ERRNO, GetLastError(), "stat", "file", spec);
    rc = SetFileTime(hFile,
                     &ctime,
                     &atime,
                     &mtime);
    CloseHandle(hFile);
    if (rc == FALSE)
       return FALSE;
  }
#else
    return pl_error(NULL, 0, NULL, ERR_NOTIMPLEMENTED, "set_time", name);
#endif

  return TRUE;
}


static foreign_t
pl_file_mode(term_t spec, term_t mode)
{ FCHAR *name;
  statstruct sbuf;

  if ( !PL_get_file_name(spec, &name, 0) )
    return FALSE;

  if ( statfunc(name, &sbuf) )
    return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "stat", "file", spec);

  return PL_unify_integer(mode, sbuf.st_mode);
}


#ifdef HAVE_CHMOD
static foreign_t
pl_chmod(term_t spec, term_t mode)
{ FCHAR *name;
  int imode;

  if ( !PL_get_file_name(spec, &name, 0) ||
       !PL_get_integer_ex(mode, &imode) )
    return FALSE;

#ifdef __WINDOWS__
  if ( _wchmod(name, imode) != 0 )
#else
  if ( chmod(name, imode) != 0 )
#endif
    return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "chmod", "file", mode);

  return TRUE;
}
#endif

		 /*******************************
		 *	       LINK		*
		 *******************************/

static foreign_t
pl_link_file(term_t from, term_t to, term_t how)
{ FCHAR *fname, *tname;
  atom_t hname;

  if ( !PL_get_file_name(from, &fname, PL_FILE_OSPATH) ||
       !PL_get_file_name(to,   &tname, PL_FILE_OSPATH) )
    return FALSE;

  if ( !PL_get_atom(how, &hname) )
    return pl_error(NULL, 0, NULL, ERR_TYPE, how, "atom");

#ifdef __WINDOWS__

  if ( hname == ATOM_hard )
  { if ( !CreateHardLinkW(tname, fname, NULL) )
      return win_error("CreateHardLink");
  } else if ( hname == ATOM_symbolic )
  { static int (*symlink)(wchar_t *new, wchar_t *existing, DWORD flags);
    static int fetched = FALSE;

    if ( !fetched )
    { HMODULE hmod = GetModuleHandle("kernel32.dll");
      void *addr = GetProcAddress(hmod, "CreateSymbolicLink");

      symlink = addr;
      fetched = TRUE;
    }

    if ( !symlink )
      return pl_error(NULL, 0, NULL, ERR_DOMAIN, how, "link_type");

    if ( !(*symlink)(tname, fname, 0) )
      return win_error("CreateSymbolicLink");
  } else
    return pl_error(NULL, 0, NULL, ERR_DOMAIN, how, "link_type");

#else /*__WINDOWS__*/

  if ( hname == ATOM_hard )
  { if ( link(fname, tname) != 0 )
      return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "link", "file", to);
  } else if ( hname == ATOM_symbolic )
  { if ( symlink(fname, tname) != 0 )
      return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "link", "file", to);
  } else
    return pl_error(NULL, 0, NULL, ERR_DOMAIN, how, "link_type");

#endif /*__WINDOWS__*/

  return TRUE;
}


install_t
install_files()
{ FUNCTOR_access1   = PL_new_functor(PL_new_atom("access"), 1);
  FUNCTOR_modified1 = PL_new_functor(PL_new_atom("modified"), 1);
  FUNCTOR_changed1  = PL_new_functor(PL_new_atom("changed"), 1);
  ATOM_now          = PL_new_atom("now");
  ATOM_hard         = PL_new_atom("hard");
  ATOM_symbolic     = PL_new_atom("symbolic");

#ifdef __WINDOWS__
  win_init_errors();
#endif

  PL_register_foreign("set_time_file", 3, pl_set_time_file, 0);
  PL_register_foreign("link_file",     3, pl_link_file,     0);
  PL_register_foreign("file_mode_",    2, pl_file_mode,     0);
#ifdef HAVE_CHMOD
  PL_register_foreign("chmod_",        2, pl_chmod,         0);
#endif
}
