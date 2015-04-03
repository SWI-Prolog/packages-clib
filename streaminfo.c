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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "error.h"
#include <ctype.h>

static int
print_byte(IOSTREAM *out, int value)
{ if ( isgraph(value) || isspace(value) )
  { Sfprintf(out, "%c", value);
  } else
  { Sfprintf(out, "\\\\u%04x", value);
  }

  return 0;
}

static int
print_buffer(IOSTREAM *out, const char *data, size_t len)
{ size_t i;

  Sfprintf(out, "----------------\n");
  for(i=0; i<len; i++)
  { if ( data[i] == 0 )
    { size_t zeros;

      for(zeros = 0; i+zeros < len && data[i+zeros] == 0; zeros++)
	;
      if ( zeros > 10 )
      { Sfprintf(out, "<%d 0-bytes>", zeros);
      }
      i += zeros;
    } else
    { print_byte(out, data[i]&0xff);
    }
  }
  if ( data[len-1] != '\n' )
    Sfprintf(out, "\n");
  Sfprintf(out, "----------------\n");

  return 0;
}


static foreign_t
stream_info(term_t to, term_t stream)
{ IOSTREAM *s = NULL, *out = NULL;
  int rc;

  if ( !(rc=PL_get_stream_handle(to, &out)) )
  { pl_error("stream_info", 2, NULL, ERR_ARGTYPE, 1,
	     to, "stream");
    goto finish;
  }
  if ( !(rc=PL_get_stream_handle(stream, &s)) )
  { pl_error("stream_info", 2, NULL, ERR_ARGTYPE, 2,
	     stream, "stream");
    goto finish;
  }

  if ( (s->flags & SIO_INPUT) )
  { if ( s->buffer )
    { if ( s->bufp > s->buffer )
      { Sfprintf(out, "Processed input:\n");
	print_buffer(out, s->buffer, s->bufp-s->buffer);
      }

      if ( s->bufp < s->limitp )
      { Sfprintf(out, "Unprocessed input:\n");
	print_buffer(out, s->bufp, s->limitp-s->bufp);
      }
    }
  } else if ( (s->flags & SIO_OUTPUT) )
  { if ( s->buffer )
    { if ( s->bufp > s->buffer )
      { Sfprintf(out, "Buffered output:\n");
	print_buffer(out, s->buffer, s->bufp-s->buffer);
      }

      if ( s->bufp < s->limitp )
      { Sfprintf(out, "Possibly sent output (or junk):\n");
	print_buffer(out, s->bufp, s->limitp-s->bufp);
      }
    }
  }

finish:
  if ( out ) rc = (rc && PL_release_stream(out));
  if ( s   ) rc = (rc && PL_release_stream(s));

  return rc;
}

install_t
install_streaminfo(void)
{ PL_register_foreign("$stream_info", 2, stream_info, 0);
}
