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

#ifndef UTF8_INCLUDED
#define UTF8_INCLUDED

#define ISUTF8_MB(c) ((unsigned)(c) >= 0xc0 && (unsigned)(c) <= 0xfd)

#define utf8_put_char(out, chr) \
	((chr) < 0x80 ? out[0]=(char)(chr), out+1 \
		      : _utf8_put_char(out, (chr)))

#include <assert.h>

static char *
_utf8_put_char(char *out, int chr)
{ if ( chr < 0x80 )
  { *out++ = (char) chr; /* dubious cast, because chr can be negative */
  } else if ( chr < 0x800 )
  { *out++ = (char) (0xc0|((chr>>6)&0x1f));
    *out++ = (char) (0x80|(chr&0x3f));
  } else if ( chr < 0x10000 )
  { *out++ = (char) (0xe0|((chr>>12)&0x0f));
    *out++ = (char) (0x80|((chr>>6)&0x3f));
    *out++ = (char) (0x80|(chr&0x3f));
  } else if ( chr < 0x200000 )
  { *out++ = (char) (0xf0|((chr>>18)&0x07));
    *out++ = (char) (0x80|((chr>>12)&0x3f));
    *out++ = (char) (0x80|((chr>>6)&0x3f));
    *out++ = (char) (0x80|(chr&0x3f));
  } else if ( chr < 0x4000000 )
  { *out++ = (char) (0xf8|((chr>>24)&0x03));
    *out++ = (char) (0x80|((chr>>18)&0x3f));
    *out++ = (char) (0x80|((chr>>12)&0x3f));
    *out++ = (char) (0x80|((chr>>6)&0x3f));
    *out++ = (char) (0x80|(chr&0x3f));
  } else if ( (unsigned)chr < 0x80000000 )
  { *out++ = (char) (0xfc|((chr>>30)&0x01));
    *out++ = (char) (0x80|((chr>>24)&0x3f));
    *out++ = (char) (0x80|((chr>>18)&0x3f));
    *out++ = (char) (0x80|((chr>>12)&0x3f));
    *out++ = (char) (0x80|((chr>>6)&0x3f));
    *out++ = (char) (0x80|(chr&0x3f));
  }

  return out;
}

#endif /*UTF8_INCLUDED*/
