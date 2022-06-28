/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2022, University of Amsterdam
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
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "utf8.h"

#define BUFSIZE 1024
#define UCFLAGS (REP_UTF8|PL_CODE_LIST)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library is dynamically picked up  by library(readline), which falls
back to a pure Prolog implementation if this library cannot be found.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static atom_t ATOM_end_of_file;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read_line_to_codes(+Stream, -Codes, ?Tail)

Read a line, upto the  next  '\n'   from  Stream.  Normally  the line is
returned as a difference list  Codes-Tail.   If  EOF is encountered, the
Codes list is closed and Tail is unified with [].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
read_line_to_codes3(term_t stream, term_t codes, term_t tail)
{ char buf[BUFSIZE];
  char *o = buf, *e = &buf[BUFSIZE];
  IOSTREAM *s;
  term_t diff = PL_new_term_refs(2);
  term_t cl = diff;
  int rc = FALSE;

  if ( !PL_unify(codes, cl) )
    return FALSE;
  if ( !PL_get_stream(stream, &s, SIO_INPUT) )
    return FALSE;

  for(;;)
  { int	c = Sgetcode(s);

    if ( c == EOF )
    { if ( Sferror(s) || PL_exception(0) )
	goto out;				/* error */

      if ( tail == 0 && o == buf )
      { rc = PL_unify_atom(codes, ATOM_end_of_file);
	goto out;
      }
      if ( PL_unify_chars(cl, UCFLAGS, o-buf, buf) &&
           (tail == 0 || PL_unify_nil(tail)) )
	rc = TRUE;

      goto out;
    }

    if ( o+8 >= e && o[-1] != '\r' ) /* UTF-8 <= 6 bytes, may discard \r */
    { if ( !PL_unify_chars(diff, PL_DIFF_LIST|UCFLAGS, o-buf, buf) ||
	   !PL_put_term(cl, diff+1) )
	goto out;
      o = buf;
    }

    o = utf8_put_char(o, c);
    if ( c == '\n' )
    { if ( tail )
      { rc = ( PL_unify_chars(diff, PL_DIFF_LIST|UCFLAGS, o-buf, buf) &&
	       PL_unify(diff+1, tail) );
      } else
      { o--;
	if ( o>buf && o[-1] == '\r' )
	  o--;
	rc = PL_unify_chars(cl, REP_UTF8|PL_CODE_LIST, o-buf, buf);
      }

      goto out;
    }
  }

out:
  PL_release_stream(s);
  return rc;
}


static foreign_t
read_line_to_codes2(term_t stream, term_t codes)
{ return read_line_to_codes3(stream, codes, 0);
}


static foreign_t
read_stream_to_codes3(term_t stream, term_t codes, term_t tail)
{ char buf[BUFSIZE];
  char *o = buf, *e = &buf[BUFSIZE];
  IOSTREAM *s;
  term_t diff = PL_new_term_refs(2);
  term_t cl = diff;

  if ( !PL_unify(codes, cl) )
    return FALSE;
  if ( !PL_get_stream(stream, &s, SIO_INPUT) )
    return FALSE;

  for(;;)
  { int	c = Sgetcode(s);

    if ( c == EOF )
    { if ( !PL_release_stream(s) )
	return FALSE;			/* error */

      if ( PL_exception(0) )
	return FALSE;

      if ( tail )
      { return ( PL_unify_chars(diff, PL_DIFF_LIST|UCFLAGS, o-buf, buf) &&
		 PL_unify(diff+1, tail) );
      }	else
      { return PL_unify_chars(cl, REP_UTF8|PL_CODE_LIST, o-buf, buf);
      }
    }

    if ( o+7 >= e )
    { if ( !PL_unify_chars(diff, PL_DIFF_LIST|UCFLAGS, o-buf, buf) ||
	   !PL_put_term(cl, diff+1) )
      { PL_release_stream(s);
	return FALSE;
      }
      o = buf;
    }

    o = utf8_put_char(o, c);
  }
}


static foreign_t
read_stream_to_codes2(term_t stream, term_t codes)
{ return read_stream_to_codes3(stream, codes, 0);
}


install_t
install_readutil()
{ ATOM_end_of_file = PL_new_atom("end_of_file");

  PL_register_foreign("read_line_to_codes", 3, read_line_to_codes3, 0);
  PL_register_foreign("read_line_to_codes", 2, read_line_to_codes2, 0);
  PL_register_foreign("read_stream_to_codes", 3, read_stream_to_codes3, 0);
  PL_register_foreign("read_stream_to_codes", 2, read_stream_to_codes2, 0);
}
