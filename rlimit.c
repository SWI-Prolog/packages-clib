/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2014, University of Amsterdam
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

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <SWI-Prolog.h>
#include "clib.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide	an interface to the Unix system resources (getrlimit()/setrlimit()).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(RLIMIT_NOFILE) && defined(RLIMIT_OFILE)
#define RLIMIT_NOFILE RLIMIT_OFILE
#endif

foreign_t
pl_rlimit(term_t what, term_t old, term_t new)
{ char *s;
  int resource;
  struct rlimit rlim;

  if ( PL_get_atom_chars(what, &s) )
  { if      ( strcmp(s, "cpu") == 0 )
      resource = RLIMIT_CPU;
    else if ( strcmp(s, "fsize") == 0 )
      resource = RLIMIT_FSIZE;
    else if ( strcmp(s, "data") == 0 )
      resource = RLIMIT_DATA;
    else if ( strcmp(s, "stack") == 0 )
      resource = RLIMIT_STACK;
    else if ( strcmp(s, "core") == 0 )
      resource = RLIMIT_CORE;
#ifdef RLIMIT_RSS
    else if ( strcmp(s, "rss") == 0 )
      resource = RLIMIT_RSS;
#endif
#ifdef RLIMIT_AS
    else if ( strcmp(s, "as") == 0 )
      resource = RLIMIT_AS;
#endif
#ifdef RLIMIT_MEMLOCK
    else if ( strcmp(s, "memlock") == 0 )
      resource = RLIMIT_MEMLOCK;
#endif
#ifdef RLIMIT_NPROC
    else if ( strcmp(s, "nproc") == 0 )
      resource = RLIMIT_NPROC;
#endif
#ifdef RLIMIT_NOFILE
    else if ( strcmp(s, "nofile") == 0 )
      resource = RLIMIT_NOFILE;
#endif
    else
      return pl_error("rlimit", 3, NULL, ERR_DOMAIN,
		      what, "resource");
  } else
    return pl_error("rlimit", 3, NULL, ERR_TYPE,
		    what, "atom");

  if ( getrlimit(resource, &rlim) == 0 )
  { int rval;

    if ( rlim.rlim_cur == RLIM_INFINITY )
      rval = PL_unify_atom_chars(old, "unlimited");
    else
      rval = PL_unify_int64(old, rlim.rlim_cur);

    if ( rval )
    { int64_t n;

      if ( PL_get_int64(new, &n) )
      {
      set:
	if ( rlim.rlim_cur != (rlim_t) n )
	{ rlim.rlim_cur = n;
	  if ( setrlimit(resource, &rlim) != 0 )
	    return pl_error("rlimit", 3, NULL, ERR_ERRNO, errno, "set", "resource_limit", what);
	}
	return TRUE;
      } else if ( PL_get_atom_chars(new, &s) && strcmp(s, "unlimited") == 0 )
      { n = RLIM_INFINITY;
	goto set;
      } else
	return pl_error("rlimit", 3, NULL, ERR_TYPE,
			new, "integer_or_unlimited");
    } else
      return FALSE;
  } else
    return pl_error("rlimit", 3, NULL, ERR_ERRNO, errno, "get",  "resource_limit", what);
}


install_t
install_rlimit()
{ PL_register_foreign("rlimit", 3, pl_rlimit, 0);
}
