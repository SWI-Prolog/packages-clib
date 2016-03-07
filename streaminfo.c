/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2015, University of Amsterdam
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
