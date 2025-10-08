/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2025, VU University Amsterdam
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
#ifdef __WINDOWS__
#define _CRT_SECURE_NO_WARNINGS 1
#define inline __inline
#endif

#include <SWI-Prolog.h>
#include <string.h>
#include <stdio.h>
#include <wchar.h>
#include <wctype.h>
#include <assert.h>
#include "utf8.h"

static size_t removed_dot_segments(size_t len, const pl_wchar_t *in,
				   pl_wchar_t *out);
static pl_wchar_t *remove_last_segment(const pl_wchar_t *base,
				       const pl_wchar_t *o);


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static atom_t ATOM_query_value;
static atom_t ATOM_fragment;
static atom_t ATOM_path;
static atom_t ATOM_segment;

static functor_t FUNCTOR_equal2;	/* =/2 */
static functor_t FUNCTOR_pair2;		/* -/2 */
static functor_t FUNCTOR_uri_components5;
static functor_t FUNCTOR_urn_components3;
static functor_t FUNCTOR_uri_authority4;


		 /*******************************
		 *	      ESCAPING		*
		 *******************************/

#define	ESC_PATH       (CH_UNRESERVED|CH_SUBDELIM|CH_EX_PATH)
#define	ESC_SEGMENT    (CH_UNRESERVED|CH_SUBDELIM|CH_EX_SEGMENT)
#define	ESC_QUERY      (CH_UNRESERVED|CH_PSUBDELIM|CH_EX_QF)
#define	ESC_QVALUE     (CH_UNRESERVED|CH_QSUBDELIM|CH_EX_QF)
#define	ESC_QNAME      (CH_PCHAR)
#define	ESC_FRAGMENT   (CH_PCHAR|CH_EX_QF)
#define	ESC_AUTH       (CH_PCHAR)
#define	ESC_PASSWD     (CH_PCHAR)
#define ESC_USER       (CH_PCHAR)
#define	ESC_SCHEME     (CH_SCHEME)
#define ESC_PORT       (CH_DIGIT)
#define ESC_HOST       (CH_UNRESERVED|CH_SUBDELIM)

#define CH_ALPHA      0x0001
#define CH_DIGIT      0x0002
#define CH_EX_UNRES   0x0004
#define CH_GENDELIM   0x0008
#define CH_SUBDELIM   0x0010
#define CH_URL	      0x0020
#define CH_EX_PCHAR   0x0040
#define CH_EX_QF      0x0080		/* Extra query and fragment chars */
#define CH_EX_SCHEME  0x0100
#define CH_QSUBDELIM  0x0200
#define CH_PSUBDELIM  0x0400
#define CH_EX_PATH    0x0800
#define CH_EX_SEGMENT 0x1000

#define CH_SCHEME	(CH_ALPHA|CH_DIGIT|CH_EX_SCHEME)
#define CH_UNRESERVED	(CH_ALPHA|CH_DIGIT|CH_EX_UNRES)
#define CH_PCHAR	(CH_UNRESERVED|CH_SUBDELIM|CH_EX_PCHAR)

static int  charflags[128] = {0};
static bool flags_done = 0;

static void
set_flags(const char *from, int flag)
{ for(; *from; from++)
    charflags[from[0]&0xff] |= flag;
}

static void
fill_flags(void)
{ if ( !flags_done )
  { int c;

    for(c='a'; c<='z'; c++)
      charflags[c] |= CH_ALPHA;
    for(c='A'; c<='Z'; c++)
      charflags[c] |= CH_ALPHA;
    for(c='0'; c<='9'; c++)
      charflags[c] |= CH_DIGIT;

    set_flags("-._~",        CH_EX_UNRES);
    set_flags(":/?#[]@",     CH_GENDELIM);
    set_flags("!$&'()+*,;=", CH_SUBDELIM);
    set_flags("!$&'()*,;=",  CH_PSUBDELIM); /* = CH_SUBDELIM - "+"  */
    set_flags("!$'()*,",     CH_QSUBDELIM); /* = CH_SUBDELIM - "&=+" */
    set_flags(":@",          CH_EX_PCHAR);
    set_flags("/@",          CH_EX_PATH);
    set_flags("@",           CH_EX_SEGMENT);
    set_flags("/?@",         CH_EX_QF);
    set_flags("+-.",	     CH_EX_SCHEME);

    set_flags("/:?#&=", CH_URL);

    flags_done = true;
  }
}

#define no_escape(c, f) \
	(((c) < 128) && (charflags[(int)c] & (f)))
#define iri_no_escape(c, f) \
	(((c) >= 128) || (c) == '%' || (charflags[(int)c] & (f)))


/* hex(const pl_wchar_t *in, int digits, int *value)

   Get <digits> characters from in and interpret them as a hexadecimal
   integer.  Returns pointer to the end on success or NULL if error.
*/

static const pl_wchar_t *
hex(const pl_wchar_t *in, int digits, int *value)
{ int v = 0;

  while(digits-- > 0)
  { int c = *in++;

    if ( c >= '0' && c <= '9' )
      v = (v<<4) + c - '0';
    else if ( c >= 'A' && c <= 'F' )
      v = (v<<4) + c + 10 - 'A';
    else if ( c >= 'a' && c <= 'f' )
      v = (v<<4) + c + 10 - 'a';
    else
      return NULL;
  }

  *value = v;
  return in;
}


static const pl_wchar_t *
get_encoded_utf8_cont_1(const pl_wchar_t *in, int *val)
{ int c;

  if ( in[0] == '%' && hex(in+1, 2, &c) )
  { if ( (c&0xc0) == 0x80 )
    { *val = (c&0x3f);
      return in+3;
    }
  }

  return NULL;
}


static const pl_wchar_t *
get_encoded_utf8_cont(const pl_wchar_t *in, int cnt, int *val)
{ int shift = cnt*6;

  *val <<= shift;
  shift -= 6;

  while(cnt-->0)
  { int v0;

    if ( (in = get_encoded_utf8_cont_1(in, &v0)) )
    { *val |= (v0<<shift);
      shift -= 6;
    } else
      return NULL;
  }

  return in;
}


static const pl_wchar_t *
get_encoded_utf8(const pl_wchar_t *in, int *chr)
{ int c1;

  if ( in[0] == '%' && hex(in+1, 2, &c1) )
  { in += 3;

    if ( ISUTF8_MB(c1) )
    { if ( (c1&0xe0) == 0xc0 )		/* 2-byte */
      { *chr = (c1&0x1f);
	return get_encoded_utf8_cont(in, 1, chr);
      } else if ( (c1&0xf0) == 0xe0 )	/* 3-byte */
      { *chr = (c1&0xf);
	return get_encoded_utf8_cont(in, 2, chr);
      } else if ( (c1&0xf8) == 0xf0 )	/* 4-byte */
      { *chr = (c1&0x7);
	return get_encoded_utf8_cont(in, 3, chr);
      } else if ( (c1&0xfc) == 0xf8 )	/* 5-byte */
      { *chr = (c1&0x3);
	return get_encoded_utf8_cont(in, 4, chr);
      } else if ( (c1&0xfe) == 0xfc )	/* 6-byte */
      { *chr = (c1&0x1);
	return get_encoded_utf8_cont(in, 5, chr);
      } else
	return NULL;
    } else
    { *chr = c1;
      return in;			/* Encoded ASCII character */
    }
  }

  return NULL;
}


		 /*******************************
		 *	      RANGES		*
		 *******************************/

typedef struct range
{ const pl_wchar_t *start;
  const pl_wchar_t *end;
} range;


		 /*******************************
		 *	 CHARACTER BUFFER	*
		 *******************************/

typedef struct charbuf
{ pl_wchar_t *base;
  pl_wchar_t *here;
  pl_wchar_t *end;
  pl_wchar_t tmp[256];
} charbuf;


static void
init_charbuf(charbuf *cb)
{ cb->base = cb->here = cb->tmp;
  cb->end = &cb->tmp[sizeof(cb->tmp)/sizeof(pl_wchar_t)];
}


static bool
init_charbuf_at_size(charbuf *cb, size_t size)
{ size++;

  if ( size < sizeof(cb->tmp)/sizeof(pl_wchar_t) )
    cb->base = cb->here = cb->tmp;
  else
    cb->base = cb->here = PL_malloc(size*sizeof(pl_wchar_t));

  return true;
}


static bool
add_charbuf(charbuf *cb, int c)
{ if ( cb->here < cb->end )
  { *cb->here++ = c;
  } else
  { size_t len = (cb->end-cb->base);

    if ( cb->base == cb->tmp )
    { pl_wchar_t *n = PL_malloc(len*2*sizeof(pl_wchar_t));
      memcpy(n, cb->base, sizeof(cb->tmp));
      cb->base = n;
    } else
    { cb->base = PL_realloc(cb->base, len*2*sizeof(pl_wchar_t));
    }
    cb->here = &cb->base[len];
    cb->end = &cb->base[len*2];
    *cb->here++ = c;
  }

  return true;
}


static inline int
hexdigit(int val)
{ if ( val < 10 )
    return '0'+val;
  return 'A'-10+val;
}


static bool
add_encoded_charbuf(charbuf *cb, int c, int flags)
{ if ( no_escape(c, flags) )
  { add_charbuf(cb, c);
  } else
  { char tmp[6];
    const char *end = utf8_put_char(tmp, c);
    const char *s;

    for(s=tmp; s<end; s++)
    { int b = s[0]&0xff;

      add_charbuf(cb, '%');
      add_charbuf(cb, hexdigit(b>>4));
      add_charbuf(cb, hexdigit(b&0xf));
    }
  }

  return true;
}


static bool
iri_add_encoded_charbuf(charbuf *cb, int c, int flags)
{ if ( iri_no_escape(c, flags) )
  { add_charbuf(cb, c);
  } else
  { assert(c < 128);
    add_charbuf(cb, '%');
    add_charbuf(cb, hexdigit(c>>4));
    add_charbuf(cb, hexdigit(c&0xf));
  }

  return true;
}



static bool
add_nchars_charbuf(charbuf *cb, size_t len, const pl_wchar_t *s)
{ if ( cb->here+len <= cb->end )
  { wcsncpy(cb->here, s, len);
    cb->here += len;
  } else
  { size_t n;

    for(n=0; n<len; n++)
      add_charbuf(cb, s[n]);
  }

  return true;
}


static bool
range_has_escape(const range *r, int flags)
{ const pl_wchar_t *s = r->start;

  for(; s<r->end; s++)
  { if ( s[0] == '%' || (s[0] == '+' && flags == ESC_QVALUE) )
      return true;
  }

  return false;
}


static bool
range_is_unreserved(const range *r, int iri, int flags)
{ const pl_wchar_t *s = r->start;

  if ( iri )
  { for(; s<r->end; s++)
    { if ( !iri_no_escape(s[0], flags) )
	return false;
    }
  } else
  { for(; s<r->end; s++)
    { if ( !no_escape(s[0], flags) )
	return false;
    }
  }

  return true;
}


static bool
add_verb_range_charbuf(charbuf *cb, const range *r)
{ return add_nchars_charbuf(cb, r->end-r->start, r->start);
}


static bool
add_decoded_range_charbuf(charbuf *cb, const range *r, int flags)
{ const pl_wchar_t *s = r->start;

  while(s<r->end)
  { int c;

    if ( *s == '%' )
    { const pl_wchar_t *e;

      if ( (e=get_encoded_utf8(s, &c)) )
      { s = e;
      } else if (hex(s+1, 2, &c) )
      { s += 3;
      } else
      { c = *s++;
      }
    } else if ( *s == '+' && flags == ESC_QVALUE )
    { s++;
      c = ' ';
    } else
    { c = *s++;
    }

    add_charbuf(cb, c);
  }

  return true;
}


static bool
add_normalized_range_charbuf(charbuf *cb, const range *r, bool iri, int flags)
{ const pl_wchar_t *s = r->start;

  while(s<r->end)
  { int c;

    if ( *s == '%' )
    { const pl_wchar_t *e;

      if ( (e=get_encoded_utf8(s, &c)) )
      { if ( flags == ESC_QUERY &&
	     (c == '=' || c == '&' || c == ';') )
	{ while( s<e )
	    add_charbuf(cb, *s++);
	  continue;
	}
	s = e;
      } else if (hex(s+1, 2, &c) )
      { s += 3;
      } else
      { c = *s++;
      }
    } else if ( *s == '+' && flags == ESC_QVALUE )
    { s++;
      c = ' ';
    } else
    { c = *s++;
    }

    if ( iri )
    { iri_add_encoded_charbuf(cb, c, flags);
    } else
    { add_encoded_charbuf(cb, c, flags);
    }
  }

  return true;
}


/* add_range_charbuf(charbuf *cb, const range *r, int iri, int flags)

   Add a range of characters while normalizing %-encoding.  This
   implies not to use encoding if it is not needed and upcase
   %xx to %XX otherwise.

   If iri == true, values >= 128 are not escaped.  Otherwise they
   use %-encoded UTF-8
*/

static bool
add_range_charbuf(charbuf *cb, const range *r, bool unesc, bool iri, int flags)
{ if ( unesc && range_has_escape(r, flags) )
  { return add_normalized_range_charbuf(cb, r, iri, flags);
  } else if ( range_is_unreserved(r, iri, flags) )
  { add_nchars_charbuf(cb, r->end-r->start, r->start);
  } else
  { const pl_wchar_t *s = r->start;

    if ( iri )
    { while(s<r->end)
	iri_add_encoded_charbuf(cb, *s++, flags);
    } else
    { while(s<r->end)
	add_encoded_charbuf(cb, *s++, flags);
    }
  }

  return true;
}


/* add_lwr_range_charbuf(charbuf *cb, const range *r,
			 bool unesc, bool iri, int flags)

   Add a range of characters while normalizing %-encoding and
   mapping all characters to lowercase.

   FIXME: encoding and decoding compatible to add_range_charbuf();
*/


static bool
add_lwr_range_charbuf(charbuf *cb, const range *r,
		      bool unesc, bool iri, int flags)
{ const pl_wchar_t *s = r->start;

  while(s<r->end)
  { int c;

    if ( unesc && *s == '%' )
    { const pl_wchar_t *e;

      if ( (e=get_encoded_utf8(s, &c)) )
      { s = e;
      } else if (hex(s+1, 2, &c) )
      { s += 3;
      } else
      { c = *s++;
      }
    } else
    { c = *s++;
    }

    if ( iri )
      iri_add_encoded_charbuf(cb, towlower((wint_t)c), flags);
    else
      add_encoded_charbuf(cb, towlower((wint_t)c), flags);
  }

  return true;
}


static void
free_charbuf(charbuf *cb)
{ if ( cb->base != cb->tmp )
    PL_free(cb->base);
}


#define TXT_EX_TEXT (CVT_ATOM|CVT_STRING|CVT_EXCEPTION)

static int
get_text_arg(term_t term, int pos, size_t *len, pl_wchar_t **s, int flags)
{ term_t tmp = PL_new_term_ref();

  _PL_get_arg(pos, term, tmp);
  if ( PL_is_variable(tmp) )
    return false;
  if ( !PL_get_wchars(tmp, len, s, flags) )
    return -1;

  return true;
}


/** uri_components(+URI, -Components)

Based on RFC-3986 regular expression:

    ==
    ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
     12            3  4          5       6  7        8 9
    ==
*/

typedef struct uri_component_ranges
{ bool  is_urn;
  range scheme;
  range authority;
  range path;
  range query;
  range fragment;
  range nid;			/* URN Namespace Identifier */
  range nss;			/* URN Namespace Specific String */
} uri_component_ranges;


static const pl_wchar_t *
skip_not(const pl_wchar_t *in, const pl_wchar_t *end, const pl_wchar_t *chars)
{ if ( !chars[1] )
  { for(; in < end; in++)
    { if ( chars[0] == in[0] )
	return in;
    }
  } else
  { for(; in < end; in++)
    { if ( wcschr(chars, in[0]) )
	return in;
    }
  }
  return in;
}


static bool
unify_range(term_t t, const range *r)
{ if ( r->start )
    return PL_unify_wchars(t, PL_ATOM, r->end - r->start, r->start);

  return true;
}


static bool
parse_uri(uri_component_ranges *ranges, size_t len, const pl_wchar_t *s)
{ const pl_wchar_t *end = &s[len];
  const pl_wchar_t *here = s;
  const pl_wchar_t *e;

  memset(ranges, 0, sizeof(*ranges));

  e = skip_not(here, end, L":/?#");
  if ( e > s && e[0] == ':' )			/* 1&2 */
  { ranges->scheme.start = s;
    ranges->scheme.end = e;
    here = e+1;
    if ( e-s == 3 && wcsncmp(s, L"urn", 3) == 0 )
      ranges->is_urn = true;
  }

  if ( ranges->is_urn )
  { e = skip_not(here, end, L":/?#");
    if ( e > here && e[0] == ':' )			/* NID */
    { ranges->nid.start = here;
      ranges->nid.end = e;
      here = e+1;
    }
    ranges->nss.start = here;
    ranges->nss.end   = end;
  } else
  { if ( here[0] == '/' && here[1] == '/' )	/* 3 */
    { here += 2;				/* 4 */
      e = skip_not(here, end, L"/?#");
      ranges->authority.start = here;
      ranges->authority.end   = e;
      here = e;					/* 5 */
    }

    e = skip_not(here, end, L"?#");
    ranges->path.start = here;
    ranges->path.end   = e;
    here = e;					/* 6 */

    if ( here[0] == '?' )
    { here++;					/* 7 */
      e = skip_not(here, end, L"#");
      ranges->query.start = here;
      ranges->query.end = e;
      here = e;					/* 8 */
    }

    if ( here[0] == '#' )
    { here++;					/* 9 */
      ranges->fragment.start = here;
      ranges->fragment.end   = end;
    }
  }

  return true;
}


static foreign_t
uri_components(term_t URI, term_t components)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(URI, &len, &s, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { uri_component_ranges ranges;
    term_t rt = PL_new_term_refs(6);
    term_t av = rt+1;

    parse_uri(&ranges, len, s);

    if ( ranges.is_urn )
    { unify_range(av+0, &ranges.scheme);
      unify_range(av+1, &ranges.nid);
      unify_range(av+2, &ranges.nss);
      return (PL_cons_functor_v(rt, FUNCTOR_urn_components3, av) &&
	      PL_unify(components, rt));
    } else
    { unify_range(av+0, &ranges.scheme);
      unify_range(av+1, &ranges.authority);
      unify_range(av+2, &ranges.path);
      unify_range(av+3, &ranges.query);
      unify_range(av+4, &ranges.fragment);

      return (PL_cons_functor_v(rt, FUNCTOR_uri_components5, av) &&
	      PL_unify(components, rt));
    }
  } else if ( PL_is_functor(components, FUNCTOR_uri_components5) )
  { charbuf b;
    int rc;

    init_charbuf(&b);
					/* schema */
    if ( (rc=get_text_arg(components, 1, &len, &s, TXT_EX_TEXT)) == true )
    { add_nchars_charbuf(&b, len, s);
      add_charbuf(&b, ':');
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }
					/* authority */
    if ( (rc=get_text_arg(components, 2, &len, &s, TXT_EX_TEXT)) == true )
    { add_charbuf(&b, '/');
      add_charbuf(&b, '/');
      add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }
					/* path */
    if ( (rc=get_text_arg(components, 3, &len, &s, TXT_EX_TEXT)) == true )
    { add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }
					/* query */
    if ( (rc=get_text_arg(components, 4, &len, &s, TXT_EX_TEXT)) == true )
    { if ( len > 0 )
      { add_charbuf(&b, '?');
	add_nchars_charbuf(&b, len, s);
      }
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }
					/* fragment */
    if ( (rc=get_text_arg(components, 5, &len, &s, TXT_EX_TEXT)) == true )
    { add_charbuf(&b, '#');
      add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }

    rc = PL_unify_wchars(URI, PL_ATOM, b.here-b.base, b.base);
    free_charbuf(&b);

    return rc;
  } else if ( PL_is_functor(components, FUNCTOR_urn_components3) )
  { charbuf b;
    int rc;

    init_charbuf(&b);

    if ( (rc=get_text_arg(components, 1, &len, &s, TXT_EX_TEXT)) == true )
    { add_nchars_charbuf(&b, len, s);
      add_charbuf(&b, ':');
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }
					/* NID */
    if ( (rc=get_text_arg(components, 2, &len, &s, TXT_EX_TEXT)) == true )
    { add_nchars_charbuf(&b, len, s);
      add_charbuf(&b, ':');
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }

    if ( (rc=get_text_arg(components, 3, &len, &s, TXT_EX_TEXT)) == true )
    { add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }

    rc = PL_unify_wchars(URI, PL_ATOM, b.here-b.base, b.base);
    free_charbuf(&b);

    return rc;
  } else				/* generate an error */
  { return PL_get_wchars(URI, &len, &s,
			 CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION);
  }
}


/** uri_is_global(+URI) is semidet.
*/

static foreign_t
uri_is_global(term_t URI)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(URI, &len, &s,
		     CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { const pl_wchar_t *e;
    const pl_wchar_t *end = &s[len];
    range r;

    fill_flags();

    e = skip_not(s, end, L":/?#");
    if ( e > s+1 && e[0] == ':' )
    { r.start = s;
      r.end = e;
      if ( range_is_unreserved(&r, false, CH_SCHEME) )
	return true;
    }
  }

  return false;
}


		 /*******************************
		 *	   QUERY-STRING		*
		 *******************************/

static bool
unify_decoded_atom(term_t t, range *r, int flags)
{ if ( range_has_escape(r, flags) )
  { charbuf b;
    bool rc;

    init_charbuf(&b);
    add_decoded_range_charbuf(&b, r, flags);
    rc = PL_unify_wchars(t, PL_ATOM, b.here - b.base, b.base);
    free_charbuf(&b);
    return rc;
  } else
  { return unify_range(t, r);
  }
}


static bool
unify_query_string_components(term_t list, size_t len, const pl_wchar_t *qs)
{ if ( len == 0 )
  { return PL_unify_nil(list);
  } else
  { term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();
    term_t eq   = PL_new_term_refs(3);
    term_t nv   = eq+1;
    const pl_wchar_t *end = &qs[len];

    while(qs < end)
    { range name, value;

      name.start = qs;
      name.end   = skip_not(qs, end, L"=");
      if ( name.end < end )
      { value.start = name.end+1;
	value.end   = skip_not(value.start, end, L"&;");

	qs = value.end+1;
      } else
      { return PL_syntax_error("illegal_uri_query", NULL);
      }

      PL_STRINGS_MARK();
      PL_put_variable(nv+0);
      PL_put_variable(nv+1);
      unify_decoded_atom(nv+0, &name, ESC_QNAME);
      unify_decoded_atom(nv+1, &value, ESC_QVALUE);
      PL_STRINGS_RELEASE();

      if ( !PL_cons_functor_v(eq, FUNCTOR_equal2, nv) ||
	   !PL_unify_list(tail, head, tail) ||
	   !PL_unify(head, eq) )
	return false;
    }

    return PL_unify_nil(tail);
  }
}


static bool
add_encoded_term_charbuf(charbuf *cb, term_t value, bool iri, int flags)
{ pl_wchar_t *s;
  range r;
  size_t len;

  if ( !PL_get_wchars(value, &len, &s, CVT_ATOMIC|CVT_EXCEPTION) )
    return false;

  r.start = s;
  r.end = r.start+len;
  if ( range_is_unreserved(&r, iri, flags) )
  { add_nchars_charbuf(cb, r.end-r.start, r.start);
  } else
  { const pl_wchar_t *s = r.start;

    while(s<r.end)
      add_encoded_charbuf(cb, *s++, flags);
  }

  return true;
}


/** uri_query_components(+QueryString, -ValueList) is det.
*/

static foreign_t
uri_query_components(term_t string, term_t list)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(string, &len, &s, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { return unify_query_string_components(list, len, s);
  } else if ( PL_is_list(list) )
  { term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();
    term_t nv   = PL_new_term_refs(2);
    charbuf out;
    int rc;

    fill_flags();
    init_charbuf(&out);
    while( PL_get_list(tail, head, tail) )
    { atom_t fname;
      size_t arity;

      if ( PL_is_functor(head, FUNCTOR_equal2) ||
	   PL_is_functor(head, FUNCTOR_pair2) )
      {	_PL_get_arg(1, head, nv+0);
	_PL_get_arg(2, head, nv+1);
      } else if ( PL_get_name_arity(head, &fname, &arity) && arity == 1 )
      { PL_put_atom(nv+0, fname);
	_PL_get_arg(1, head, nv+1);
      } else
      { free_charbuf(&out);
	return PL_type_error("name_value", head);
      }

      if ( out.here != out.base )
	add_charbuf(&out, '&');
      if ( !add_encoded_term_charbuf(&out, nv+0, false, ESC_QNAME) )
      { free_charbuf(&out);
	return false;
      }
      add_charbuf(&out, '=');
      if ( !add_encoded_term_charbuf(&out, nv+1, false, ESC_QVALUE) )
      { free_charbuf(&out);
	return false;
      }
    }

    rc = PL_unify_wchars(string, PL_ATOM, out.here-out.base, out.base);
    free_charbuf(&out);
    return rc;
  } else
  { return PL_get_wchars(string, &len, &s,
			 CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION);
  }

  return false;
}


/** uri_encoded(+What, +String, -Encoded)
*/

static foreign_t
uri_encoded(term_t what, term_t qv, term_t enc)
{ pl_wchar_t *s;
  size_t len;
  atom_t w;
  int flags;

  if ( !PL_get_atom(what, &w) )
    return PL_type_error("atom", what);
  if ( w == ATOM_query_value )
    flags = ESC_QVALUE;
  else if ( w == ATOM_fragment )
    flags = ESC_FRAGMENT;
  else if ( w == ATOM_path )
    flags = ESC_PATH;
  else if ( w == ATOM_segment )
    flags = ESC_SEGMENT;
  else
    return PL_domain_error("uri_component", what);

  fill_flags();

  if ( !PL_is_variable(qv) )
  { charbuf out;
    int rc;

    init_charbuf(&out);
    if ( !add_encoded_term_charbuf(&out, qv, false, flags) )
    { free_charbuf(&out);
      return false;
    }
    rc = PL_unify_wchars(enc, PL_ATOM, out.here-out.base, out.base);
    free_charbuf(&out);
    return rc;
  } else if ( PL_get_wchars(enc, &len, &s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
  { range r;

    r.start = s;
    r.end = s+len;

    return unify_decoded_atom(qv, &r, flags);
  } else
  { return false;
  }
}


		 /*******************************
		 *	      AUTHORITY		*
		 *******************************/

static bool
unify_uri_authority_components(term_t components,
			       size_t len, const pl_wchar_t *s)
{ const pl_wchar_t *end = &s[len];
  const pl_wchar_t *e;
  range user   = {0};
  range passwd = {0};
  range host   = {0};
  range port   = {0};
  term_t t = PL_new_term_refs(5);
  term_t av = t+1;

  if ( (e=skip_not(s, end, L"@")) && e<end )
  { user.start = s;
    user.end = e;
    s = e+1;
    if ( (e=skip_not(user.start, user.end, L":")) && e<user.end )
    { passwd.start = e+1;
      passwd.end   = user.end;
      user.end     = e;
    }
  }
  host.start = s;
  if ( *s == '[' )
  { host.end = skip_not(s+1, end, L"]");
    if ( host.end == end )	/* No "]" */
      goto no_ipv6;
    if ( host.end == end-1 )	/* ends with "]" */
    { host.start++;
      goto done;
    }
    if ( host.end+1 < end && host.end[1] == ':' )
    { host.start++;
      port.start = host.end+2;
      port.end = end;
      goto done;
    }
    s = host.end;
  }

no_ipv6:
  host.end = skip_not(s, end, L":");
  if ( host.end < end )
  { port.start = host.end+1;
    port.end = end;
  }

done:
  if ( user.start )
    unify_decoded_atom(av+0, &user, ESC_USER);
  if ( passwd.start )
    unify_decoded_atom(av+1, &passwd, ESC_PASSWD);
  unify_decoded_atom(av+2, &host, ESC_HOST);
  if ( port.start )
  { wchar_t *ep;
    long pn = wcstol(port.start, &ep, 10);

    if ( ep == port.end )
    { if ( !PL_put_integer(av+3, pn) )
	return false;
    } else
    { unify_decoded_atom(av+3, &port, ESC_PORT);
    }
  }

  return (PL_cons_functor_v(t, FUNCTOR_uri_authority4, av) &&
	  PL_unify(components, t));
}


/** uri_authority_components(+Authority, -Components) is det.
    uri_authority_components(-Authority, +Components) is det.
*/

static foreign_t
uri_authority_components(term_t Authority, term_t components)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(Authority, &len, &s, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { return  unify_uri_authority_components(components, len, s);
  } else if ( PL_is_functor(components, FUNCTOR_uri_authority4) )
  { charbuf b;
    int rc;

    init_charbuf(&b);
    /* user[:password] */
    if ( (rc=get_text_arg(components, 1, &len, &s, TXT_EX_TEXT)) == true )
    { add_nchars_charbuf(&b, len, s);
      if ( (rc=get_text_arg(components, 2, &len, &s, TXT_EX_TEXT)) == true )
      { add_charbuf(&b, ':');
	add_nchars_charbuf(&b, len, s);
      } else if ( rc == -1 )
      { free_charbuf(&b);
	return false;
      }
      add_charbuf(&b, '@');
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }
    /* host */
    if ( (rc=get_text_arg(components, 3, &len, &s, TXT_EX_TEXT)) == true )
    { if ( wcschr(s, ':') )
      { add_charbuf(&b, '[');
	add_nchars_charbuf(&b, len, s);
	add_charbuf(&b, ']');
      } else
      { add_nchars_charbuf(&b, len, s);
      }
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }
    /* port */
    if ( (rc=get_text_arg(components, 4, &len, &s,
			  TXT_EX_TEXT|CVT_INTEGER)) == true )
    { add_charbuf(&b, ':');
      add_nchars_charbuf(&b, len, s);
    } else if ( rc == -1 )
    { free_charbuf(&b);
      return false;
    }

    rc = PL_unify_wchars(Authority, PL_ATOM, b.here-b.base, b.base);
    free_charbuf(&b);

    return rc;
  } else
  { return PL_get_wchars(Authority, &len, &s,
			 CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION);
  }
}



		 /*******************************
		 *	  NORMALIZATION		*
		 *******************************/

static bool
normalize_in_charbuf(charbuf *cb, uri_component_ranges *ranges,
		     bool unesc, bool iri)
{ fill_flags();

  if ( ranges->scheme.start )
  { add_lwr_range_charbuf(cb, &ranges->scheme, unesc, iri, ESC_SCHEME);
    add_charbuf(cb, ':');
  }
  if ( ranges->is_urn )
  { if ( ranges->nid.start )
    { add_lwr_range_charbuf(cb, &ranges->nid, unesc, iri, ESC_SCHEME);
      add_charbuf(cb, ':');
    }
    if ( ranges->nss.start )
    { add_lwr_range_charbuf(cb, &ranges->nss, unesc, iri, ESC_SEGMENT);
    }
  } else
  { if ( ranges->authority.start )
    { add_charbuf(cb, '/');
      add_charbuf(cb, '/');
      add_lwr_range_charbuf(cb, &ranges->authority, unesc, iri, ESC_AUTH);
    }
    if ( ranges->path.end > ranges->path.start )
    { charbuf pb;
      charbuf path;
      size_t len;

      init_charbuf(&pb);
      add_range_charbuf(&pb, &ranges->path, unesc, iri, ESC_PATH);
      init_charbuf_at_size(&path, pb.here-pb.base);
      len = removed_dot_segments(pb.here-pb.base, pb.base, path.base);
      add_nchars_charbuf(cb, len, path.base);
      free_charbuf(&path);
      free_charbuf(&pb);
    }
    if ( ranges->query.start )
    { add_charbuf(cb, '?');
      add_range_charbuf(cb, &ranges->query, unesc, iri, ESC_QUERY);
    }
    if ( ranges->fragment.start )
    { add_charbuf(cb, '#');
      add_range_charbuf(cb, &ranges->fragment, unesc, iri, ESC_QVALUE);
    }
  }

  return true;
}


static foreign_t
normalized(term_t URI, term_t CannonicalURI, bool unesc, bool iri)
{ pl_wchar_t *s;
  size_t len;

  if ( PL_get_wchars(URI, &len, &s,
		     CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { uri_component_ranges ranges;
    charbuf b;
    bool rc;

    parse_uri(&ranges, len, s);
    init_charbuf(&b);
    normalize_in_charbuf(&b, &ranges, unesc, iri);

    rc = PL_unify_wchars(CannonicalURI, PL_ATOM, b.here-b.base, b.base);
    free_charbuf(&b);

    return rc;
  }

  return false;
}


/** uri_normalized(+URI, -CannonicalURI)
*/

static foreign_t
uri_normalized(term_t URI, term_t CannonicalURI)
{ return normalized(URI, CannonicalURI, true, false);
}


/** iri_normalized(+IRI, -CannonicalIRI)
*/

static foreign_t
iri_normalized(term_t IRI, term_t CannonicalIRI)
{ return normalized(IRI, CannonicalIRI, false, true);
}


/** uri_normalized_iri(+URI, -CannonicalIRI)
*/

static foreign_t
uri_normalized_iri(term_t URI, term_t CannonicalIRI)
{ return normalized(URI, CannonicalIRI, true, true);
}


static bool
ranges_in_charbuf(charbuf *cb, uri_component_ranges *ranges)
{ if ( ranges->scheme.start )
  { add_verb_range_charbuf(cb, &ranges->scheme);
    add_charbuf(cb, ':');
  }
  if ( ranges->authority.start )
  { add_charbuf(cb, '/');
    add_charbuf(cb, '/');
    add_verb_range_charbuf(cb, &ranges->authority);
  }
  add_verb_range_charbuf(cb, &ranges->path);
  if ( ranges->query.start )
  { add_charbuf(cb, '?');
    add_verb_range_charbuf(cb, &ranges->query);
  }
  if ( ranges->fragment.start )
  { add_charbuf(cb, '#');
    add_verb_range_charbuf(cb, &ranges->fragment);
  }

  return true;
}


typedef struct
{ pl_wchar_t	      *text;
  uri_component_ranges ranges;
} base_cache;

#ifdef _REENTRANT
#include <pthread.h>
static pthread_key_t base_key;

static void
free_base_cache(void *cache)
{ base_cache *base = cache;

  if ( PL_query(PL_QUERY_HALTING) )
    return;

  if ( base->text )
    PL_free(base->text);

  PL_free(base);
}

static base_cache *
myBase(void)
{ base_cache *base;

  if ( (base=pthread_getspecific(base_key)) )
    return base;
  base = PL_malloc_uncollectable(sizeof(*base)); /* Due to bug in Boehm-GC */
  memset(base, 0, sizeof(*base));

  pthread_setspecific(base_key, base);
  return base;
}

#else
static base_cache base_store;
#define myBase() &base_store;
#endif


static const uri_component_ranges *
base_ranges(term_t t)
{ size_t len;
  pl_wchar_t *s;

  if ( PL_get_wchars(t, &len, &s, CVT_ATOM|CVT_STRING|BUF_MALLOC|CVT_EXCEPTION) )
  { base_cache *base = myBase();

    if ( !base->text || wcscmp(base->text, s) != 0 )
    { if ( base->text )
      { PL_free(base->text);
	base->text = NULL;
      }
      base->text = s;
      parse_uri(&base->ranges, len, s);
    }

    return &base->ranges;
  } else
    return false;
}


static bool
resolve_guarded(term_t Rel, term_t Base, term_t URI,
		bool unesc, bool normalize, bool iri)
{ pl_wchar_t *s;
  size_t slen;
  uri_component_ranges s_ranges, t_ranges;
  bool rc;
  size_t len;
  charbuf out, pb, path;

  init_charbuf(&pb);			/* path-buffer */

  if ( PL_get_wchars(Rel, &slen, &s,
		     CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { parse_uri(&s_ranges, slen, s);
    if ( s_ranges.scheme.start )
    { t_ranges = s_ranges;
    } else
    { const uri_component_ranges *b_ranges;

      if ( !(b_ranges = base_ranges(Base)) )
	return false;

      memset(&t_ranges, 0, sizeof(t_ranges));
      if ( s_ranges.authority.start )
      { t_ranges.authority = s_ranges.authority;
	t_ranges.path      = s_ranges.path;
	t_ranges.query     = s_ranges.query;
      } else
      { if ( s_ranges.path.start == s_ranges.path.end )
	{ t_ranges.path = b_ranges->path;
	  if ( s_ranges.query.start )
	    t_ranges.query = s_ranges.query;
	  else
	    t_ranges.query = b_ranges->query;
	} else
	{ if ( s_ranges.path.start[0] == '/' )
	  { t_ranges.path = s_ranges.path;
	  } else
	  { if ( b_ranges->authority.start &&
		 b_ranges->path.start == b_ranges->path.end )
	    { add_charbuf(&pb, '/');
	      add_verb_range_charbuf(&pb, &s_ranges.path);
	    } else
	    { range path = b_ranges->path;

	      path.end = remove_last_segment(path.start, path.end);
	      add_verb_range_charbuf(&pb, &path);
	      add_verb_range_charbuf(&pb, &s_ranges.path);
	      t_ranges.path.start = pb.base;
	      t_ranges.path.end = pb.here;
	    }
	  }
	  t_ranges.query = s_ranges.query;
	}
	t_ranges.authority = b_ranges->authority;
      }
      t_ranges.scheme = b_ranges->scheme;
      t_ranges.fragment = s_ranges.fragment;
    }
  } else
    return false;

  init_charbuf(&out);			/* output buffer */

  if ( normalize )
  { normalize_in_charbuf(&out, &t_ranges, unesc, iri);
  } else
  { init_charbuf_at_size(&path, t_ranges.path.end - t_ranges.path.start);
    len = removed_dot_segments(t_ranges.path.end - t_ranges.path.start,
			       t_ranges.path.start,
			       path.base);
    t_ranges.path.start = path.base;
    t_ranges.path.end   = path.base+len;
    free_charbuf(&pb);

    ranges_in_charbuf(&out, &t_ranges);
  }

  rc = PL_unify_wchars(URI, PL_ATOM, out.here-out.base, out.base);
  free_charbuf(&out);

  return rc;
}

static bool
resolve(term_t Rel, term_t Base, term_t URI, int unesc, int normalize, int iri)
{ bool rc;

  PL_STRINGS_MARK();
  rc = resolve_guarded(Rel, Base, URI, unesc, normalize, iri);
  PL_STRINGS_RELEASE();

  return rc;
}


/** uri_resolve(+Relative, +Base, -Absolute) is det.
*/

static foreign_t
uri_resolve(term_t Rel, term_t Base, term_t URI)
{ return resolve(Rel, Base, URI, true, false, false);
}


/** uri_normalized(+Relative, +Base, -Absolute) is det.
*/

static foreign_t
uri_normalized3(term_t Rel, term_t Base, term_t URI)
{ return resolve(Rel, Base, URI, true, true, false);
}

/** iri_normalized(+Relative, +Base, -Absolute) is det.
*/

static foreign_t
iri_normalized3(term_t Rel, term_t Base, term_t IRI)
{ return resolve(Rel, Base, IRI, false, true, true);
}


/** uri_normalized_iri(+Relative, +Base, -Absolute) is det.
*/

static foreign_t
uri_normalized_iri3(term_t Rel, term_t Base, term_t IRI)
{ return resolve(Rel, Base, IRI, true, true, true);
}


		 /*******************************
		 *	    PATH LOGIC		*
		 *******************************/

/* http://labs.apache.org/webarch/uri/rfc/rfc3986.html#relative-dot-segments
*/

static pl_wchar_t *
remove_last_segment(const pl_wchar_t *base, const pl_wchar_t *o)
{ while(o>base && o[-1] != '/' )
    o--;

  return (pl_wchar_t*) o;
}


static inline int
fetch(const pl_wchar_t *in, const pl_wchar_t *end, int at)
{ if ( in+at>=end )
    return 0;
  return in[at];
}

static size_t
removed_dot_segments(size_t len, const pl_wchar_t *in, pl_wchar_t *out)
{ const pl_wchar_t *end = &in[len];
  pl_wchar_t *o = out;

  while(in<end)
  { if ( in[0] == '.' )
    { if ( fetch(in, end, 1) == '/' ||
	   (fetch(in, end, 1) == '.' && fetch(in, end, 2) == '/') )
      { in += 2;			/* 2A */
	continue;
      }
    }
    if ( in[0] == '/' && fetch(in, end, 1) == '.' )
    { if ( fetch(in, end, 2) == '/' )
      { in += 2;			/* 2B "/./" --> "/" */
	continue;
      }
      if ( !fetch(in, end, 2) )
      { *o++ = '/';			/* 2B "/." --> "/" (and close) */
	in += 2;
	continue;
      }
      if ( fetch(in, end, 2) == '.' )
      { if ( fetch(in, end, 3) == '/' )
	{ in += 3;			/* 2C "/../" --> "/" */
	  o = remove_last_segment(out, o);
	  if ( o>out ) o--;		/* delete / */
	  continue;
	}
	if ( !fetch(in, end, 3) )
	{ o = remove_last_segment(out, o);
	  if ( o>out ) o--;		/* delete / */
	  *o++ = '/';
	  in += 3;
	  continue;
	}
      }
    }
    if ( in[0] == '.' )
    { if ( !fetch(in, end, 1) )
      { in++;				/* 3D */
	continue;
      }
      if ( fetch(in, end, 1) == '.' && !fetch(in, end, 2) )
      { in += 2;			/* 3D */
	continue;
      }
    }
    if ( in[0] == '/' )
      *o++ = *in++;
    while( in < end && in[0] != '/' )
      *o++ = *in++;
  }

  return o-out;
}


		 /*******************************
		 *	    IRI HANDLING	*
		 *******************************/

/** uri_iri(+URI, -IRI) is det.
    uri_iri(-URI, +IRI) is det.

Perform %- and UTF-8 encoding/decoding to translate between a URI and
IRI
*/

static foreign_t
uri_iri(term_t URI, term_t IRI)
{ if ( !PL_is_variable(URI) )
    return uri_normalized_iri(URI, IRI);
  else
    return uri_normalized(IRI, URI);
}


		 /*******************************
		 *	   REGISTRATION		*
		 *******************************/

#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_uri()
{ MKATOM(query_value);
  MKATOM(fragment);
  MKATOM(path);
  MKATOM(segment);

  MKFUNCTOR(uri_components, 5);
  MKFUNCTOR(urn_components, 3);
  MKFUNCTOR(uri_authority, 4);
  FUNCTOR_equal2 = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_pair2 = PL_new_functor(PL_new_atom("-"), 2);

#ifdef _REENTRANT
  pthread_key_create(&base_key, free_base_cache);
#endif

  PL_register_foreign("uri_components",	      2, uri_components,       0);
  PL_register_foreign("uri_is_global",	      1, uri_is_global,	       0);
  PL_register_foreign("uri_normalized",	      2, uri_normalized,       0);
  PL_register_foreign("iri_normalized",	      2, iri_normalized,       0);
  PL_register_foreign("uri_normalized_iri",   2, uri_normalized_iri,   0);
  PL_register_foreign("uri_resolve",	      3, uri_resolve,	       0);
  PL_register_foreign("uri_normalized",	      3, uri_normalized3,      0);
  PL_register_foreign("iri_normalized",	      3, iri_normalized3,      0);
  PL_register_foreign("uri_normalized_iri",   3, uri_normalized_iri3,  0);
  PL_register_foreign("uri_query_components", 2, uri_query_components, 0);
  PL_register_foreign("uri_authority_components",
					      2, uri_authority_components, 0);
  PL_register_foreign("uri_encoded",	      3, uri_encoded,	       0);
  PL_register_foreign("uri_iri",	      2, uri_iri,	       0);
}


install_t
uninstall_uri()
{
#ifdef _REENTRANT
  pthread_key_delete(base_key);
#endif
}
