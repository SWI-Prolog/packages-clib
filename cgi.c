/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2022, University of Amsterdam
                              VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "clib.h"
#include "form.h"

/* [+-]?\sd*[.\sd*][eE]\sd+
*/

#define isdigit(c) (c >= '0' && c <= '9')

static int
isnumber(const char *s, size_t len)
{ int digits = 0;
  const char *e = &s[len];

  if ( s < e && (*s == '+' || *s == '-') )
    s++;
  while(s < e && isdigit(*s)) digits++, s++;
  if ( s < e && *s == '.' )
  { s++;
    while(s < e && isdigit(*s)) digits++, s++;
  }
  if ( s < e+1 && (*s == 'e' || *s == 'E') && isdigit(s[1]) )
    while(isdigit(*s)) s++;

  return digits > 0 && s == e;
}


static int
unify_number(term_t t, const char *s, size_t len)
{ char buf[100];
  char *a, *o;
  const char *i;
  int rc;

  if ( len+1 > sizeof(buf) )
  { if ( !(a = malloc(len+1)) )
      return PL_resource_error("memory");
  } else
  { a = buf;
  }

  for(i=s,o=a; len-- > 0; )
    *o++ = (char)*i++;
  *o = '\0';

  rc = PL_chars_to_term(a, t);
  if ( a != buf )
    free(a);

  return rc;
}


static int
add_to_form(const char *name, size_t nlen,
	    const char *value, size_t len,
	    void *closure)
{ term_t head = PL_new_term_ref();
  term_t tail = (term_t) closure;
  term_t val  = PL_new_term_ref();
  int rc;
  atom_t aname = 0;

  if ( isnumber(value, len) )
  { rc = unify_number(val, value, len);
  } else
  { rc = PL_unify_chars(val, PL_ATOM|REP_UTF8, len, value);
  }

  rc = ( rc &&
	 PL_unify_list(tail, head, tail) &&
	 (aname = PL_new_atom_nchars(nlen, name)) &&
	 PL_unify_term(head,
		       PL_FUNCTOR, PL_new_functor(aname, 1),
		       PL_TERM, val) );

  if ( aname )
    PL_unregister_atom(aname);

  return rc;
}


static int
mp_add_to_form(const char *name, size_t nlen,
	       const char *value, size_t len,
	       const char *file, void *closure)
{ term_t head = PL_new_term_ref();
  term_t tail = (term_t) closure;
  term_t val  = PL_new_term_ref();
  int rc;
  atom_t aname = 0;

  if ( isnumber(value, len) )
    rc = unify_number(val, value, len);
  else
    rc = PL_unify_chars(val, PL_ATOM|REP_UTF8, len, value);

  rc = ( rc &&
	 PL_unify_list(tail, head, tail) &&
	 (aname = PL_new_atom_nchars(nlen, name)) &&
	 PL_unify_term(head,
			PL_FUNCTOR, PL_new_functor(aname, 1),
			PL_TERM, val) );

  if ( aname )
    PL_unregister_atom(aname);

  return rc;
}


static foreign_t
pl_cgi_get_form(term_t form)
{ size_t len = 0;
  char *data;
  int must_free = FALSE;
  term_t list = PL_copy_term_ref(form);
  char *ct, *boundary;

  if ( !get_raw_form_data(&data, &len, &must_free) )
    return FALSE;

  if ( (ct = getenv("CONTENT_TYPE")) &&
       (boundary = strstr(ct, "boundary=")) )
  { boundary = strchr(boundary, '=')+1;

    switch( break_multipart(data, len, boundary,
			    mp_add_to_form, (void *)list) )
    { case FALSE:
	return FALSE;
      case TRUE:
	break;
      default:
	assert(0);
        return FALSE;
    }
  } else
  { switch( break_form_argument(data, add_to_form, (void *)list) )
    { case FALSE:
	return FALSE;
      case TRUE:
	break;
      case ERROR_NOMEM:
	return pl_error("cgi_get_form", 1, NULL,
			ERR_RESOURCE, "memory");
      case ERROR_SYNTAX_ERROR:
	return pl_error("cgi_get_form", 1, NULL,
			ERR_SYNTAX, "cgi_value");
      default:
	assert(0);
        return FALSE;
    }
  }

  if ( must_free )
    free(data);

  return PL_unify_nil(list);
}


install_t
install_cgi()
{ PL_register_foreign("cgi_get_form", 1, pl_cgi_get_form, 0);
}
