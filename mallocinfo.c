/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, University of Amsterdam
                         VU University Amsterdam
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
#include "error.h"
#include <malloc.h>
#include <errno.h>

static functor_t FUNCTOR_equals2;

static int
addNameInteger(term_t list, const char *name, intptr_t val)
{ term_t head = PL_new_term_ref();

  if ( !PL_unify_list(list, head, list) )
    return FALSE;
  if ( !PL_unify_term(head, PL_FUNCTOR, FUNCTOR_equals2,
		      PL_CHARS, name, PL_INTPTR, val) )
    return FALSE;

  PL_reset_term_refs(head);

  return TRUE;
}


/** memory_statistics(-Stats) is det.

Provide  statistics  on  memory  allocation    if  the  system  provides
mallinfo(), the values of this structure  are   added  to  the list. See
"info mallinfo" for a the defined names and their meaning. Unused values
are not included.
*/

#ifdef HAVE_MALLINFO
static foreign_t
pl_malinfo(term_t stats)
{ term_t tail = PL_copy_term_ref(stats);
  struct mallinfo info = mallinfo();

  addNameInteger(tail, "arena",    (unsigned)info.arena);
  addNameInteger(tail, "ordblks",  (unsigned)info.ordblks);
  addNameInteger(tail, "hblks",    (unsigned)info.hblks);
  addNameInteger(tail, "hblkhd",   (unsigned)info.hblkhd);
  addNameInteger(tail, "uordblks", (unsigned)info.uordblks);
  addNameInteger(tail, "fordblks", (unsigned)info.fordblks);
  addNameInteger(tail, "keepcost", (unsigned)info.keepcost);

  return PL_unify_nil(tail);
}
#endif /*HAVE_MALLINFO*/

#if defined(HAVE_OPEN_MEMSTREAM) && defined(HAVE_MALLOC_INFO)

static foreign_t
pl_malloc_info(term_t info)
{ char *data = NULL;
  size_t len = 0;
  FILE *fp;

  if ( (fp=open_memstream(&data, &len)) )
  { return ( malloc_info(0, fp) == 0 &&
	     fclose(fp) == 0 &&
	     PL_unify_chars(info, PL_STRING, len, data) );
  }

  return pl_error("malloc_info", 1, NULL,
		  ERR_ERRNO, errno, "open", "memstream", 0);
}
#else
#undef HAVE_MALLOC_INFO

#endif /*defined(HAVE_OPEN_MEMSTREAM) && defined(HAVE_MALLOC_INFO)*/

install_t
install_mallocinfo(void)
{ FUNCTOR_equals2 = PL_new_functor(PL_new_atom("="), 2);

#ifdef HAVE_MALLINFO
  PL_register_foreign("$mallinfo", 1, pl_malinfo, 0);
#endif
#ifdef HAVE_MALLOC_INFO
  PL_register_foreign("$malloc_info", 1, pl_malloc_info, 0);
#endif
}

