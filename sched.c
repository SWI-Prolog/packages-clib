/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

#include <SWI-Prolog.h>
#include <config.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#include <errno.h>

static atom_t ATOM_process;
static atom_t ATOM_pgrp;
static atom_t ATOM_user;

static int
get_which(term_t Which, int *whichp, const char **namep)
{ atom_t awhich;

  if ( PL_get_atom_ex(Which, &awhich) )
  { if ( awhich == ATOM_process )
    { *whichp = PRIO_PROCESS; *namep = "process";
    } else if ( awhich == ATOM_pgrp )
    { *whichp = PRIO_PGRP; *namep = "pgrp";
    } else if ( awhich == ATOM_user )
    { *whichp = PRIO_USER; *namep = "user";
    } else
      return PL_domain_error("priority_which", Which),FALSE;

    return TRUE;
  }

  return FALSE;
}

#ifdef HAVE_SETPRIORITY
static foreign_t
pl_setpriority(term_t Which, term_t Who, term_t Prio)
{ int which;
  const char *which_str;
  int who;
  int prio;

  if ( !get_which(Which, &which, &which_str) ||
       !PL_get_integer_ex(Who, &who) ||
       !PL_get_integer_ex(Prio, &prio) )
    return FALSE;

  if ( setpriority(which, who, prio) == 0 )
    return TRUE;

  switch(errno)
  { case ESRCH:
      return PL_existence_error(which_str, Who);
    case EACCES:
    case EPERM:
      return PL_permission_error("setpriority", which_str, Who);
    default:
      return PL_warning("setpriority/3: unknown error %d", errno);
  }
}


static foreign_t
pl_getpriority(term_t Which, term_t Who, term_t Prio)
{ int which;
  const char *which_str;
  int who;
  int prio;

  if ( !get_which(Which, &which, &which_str) ||
       !PL_get_integer_ex(Who, &who) )
    return FALSE;

  errno = 0;
  prio = getpriority(which, who);

  switch(errno)
  { case 0:
      return PL_unify_integer(Prio, prio);
    case ESRCH:
      return PL_existence_error(which_str, Who);
    default:
      return PL_warning("setpriority/3: unknown error %d", errno);
  }
}
#endif


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n)

install_t
install_sched(void)
{ MKATOM(process);
  MKATOM(pgrp);
  MKATOM(user);

#ifdef HAVE_SETPRIORITY
  PL_register_foreign("setpriority", 3, pl_setpriority, 0);
  PL_register_foreign("getpriority", 3, pl_getpriority, 0);
#endif
}
