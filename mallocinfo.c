/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2015, University of Amsterdam
			      VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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

