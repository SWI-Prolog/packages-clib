/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2001-2015, University of Amsterdam
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

#define _GNU_SOURCE 1
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __WINDOWS__
#define HAVE_MALLOC_H 1
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <rfc2045.h>
#include "error.h"
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <errno.h>

#undef max				/* be sure we have ours */
#define max(x, y) ((x)>(y) ? (x) : (y))


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines an interface to   the rfc2045 (MIME) parsing library
by Double Precision, Inc, part of the maildrop system.

Parsing MIME messages is accomplished  using   a  single predicate. This
predicate parses the input  and  returns   a  complex  term  holding the
various MIME message  parts.  The  mime   message  is  encoded  into the
following structure:

	mime(Attributes, Data, SubMimeList)

Where Data is the (decoded) field data   returned as an atom, Attributes
is a property-list and SubMimeList is a  list of mime/3 terms reflecting
the sub-parts. Attributes contains the following members:

	# id(Atom)
	# description(Atom)
	# language(Atom)
	# md5(Atom)
	# type(Atom)
	# character_set(Atom)
	# transfer_encoding(Atom)
	# disposition(Atom)
	# filename(Atom)
	# name(Atom)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static atom_t	 ATOM_;
static atom_t	 ATOM_stream;
static functor_t FUNCTOR_type1;
static functor_t FUNCTOR_transfer_encoding1;
static functor_t FUNCTOR_character_set1;
static functor_t FUNCTOR_mime3;
static functor_t FUNCTOR_id1;
static functor_t FUNCTOR_description1;
static functor_t FUNCTOR_language1;
static functor_t FUNCTOR_md51;
static functor_t FUNCTOR_disposition1;
static functor_t FUNCTOR_name1;
static functor_t FUNCTOR_filename1;

struct dbuf
{ char *buf;
  size_t size;
  size_t allocated;
};

static int
add_data(const char *ndata, size_t len, void *closure)
{ struct dbuf *dbuf = closure;

  if ( dbuf->size + (int)len > dbuf->allocated )
  { dbuf->allocated = max(dbuf->allocated, max(1024, dbuf->size + (int)len));
    if ( dbuf->buf )
      dbuf->buf = realloc(dbuf->buf, dbuf->allocated);
    else
      dbuf->buf = malloc(dbuf->allocated);

    if ( !dbuf->buf )
    { pl_error("mime_parse", 3, NULL, ERR_ERRNO, errno, "add_data", "mime", 0);
      return -1;
    }
  }

  memcpy(dbuf->buf+dbuf->size, ndata, len);
  dbuf->size += len;

  return 0;
}


#ifndef HAVE_STRCASESTR

#ifdef __WINDOWS__
#define strncasecmp(h,n,c) _strnicmp(h,n,c)
#endif

static char *
my_strcasestr(const char *haystack, const char *needle)
{ size_t nlen = strlen(needle);
  const char *end = haystack + strlen(haystack) - nlen;

  for(; haystack < end; haystack++)
  { if ( strncasecmp(haystack, needle, nlen) == 0 )
      return (char*)haystack;
  }

  return NULL;
}

#define strcasestr(haystack, needle) my_strcasestr(haystack, needle)

#endif


static const char *utf8_aliases[] =
{ "UTF-8",
  "FSS_UTF",
  "TF-8",
  "u8",
  "UTF-2",
  "UTF-FSS",
  NULL
};

static const char *lat1_aliases[] =
{ "ISO-8859-1",
  "819/CR-LF",
  "CP819/CR-LF",
  "csISOLatin1",
  "IBM819/CR-LF",
  "ISO8859-1",
  "iso-ir-100",
  "ISO_8859-1",
  "ISO_8859-1:1987",
  "l1",
  "lat1",
  "latin1",
  "Latin-1",

  "ANSI_X3.4-1968",
  "367/CR-LF",
  "ANSI_X3.4-1986",
  "ASCII CP367/CR-LF",
  "csASCII",
  "IBM367/CR-LF",
  "ISO646-US",
  "ISO646.1991-IRV",
  "iso-ir-6",
  "ISO_646.irv:1991",
  "us",
  "US-ASCII",
  "ASCII-BS",
  "BS",

  NULL
};

static int
is_alias(const char **alias, const char *cset)
{ for( ; *alias; alias++)
  { if ( strcasestr(cset, *alias) )
      return TRUE;
  }

  return FALSE;
}


static int
mime_unify_data(term_t data, struct rfc2045 *rfc, const char *buffer,
	        const char *cset, const char *type)
{ off_t start_pos, end_pos, start_body, nlines, nbodylines;
  struct dbuf dbuf;
  int rval;

  dbuf.buf       = NULL;
  dbuf.size      = 0;
  dbuf.allocated = 0;

  rfc2045_mimepos(rfc,
		  &start_pos, &end_pos, &start_body, &nlines, &nbodylines);
  rfc2045_cdecode_start(rfc, add_data, &dbuf);
  if ( rfc2045_cdecode(rfc, buffer+start_body, end_pos-start_body) == 0 &&
       rfc2045_cdecode_end(rfc) == 0 )
  { int rep = REP_ISO_LATIN_1;

    if ( strncmp(type, "text/", strlen("text/")) == 0 )
    { if ( is_alias(utf8_aliases, cset) )
	rep = REP_UTF8;
      else if ( is_alias(lat1_aliases, cset) )
	rep = REP_ISO_LATIN_1;
      else
	rep = REP_MB;
    }

    rval = PL_unify_chars(data, PL_ATOM|rep, dbuf.size, dbuf.buf);
  } else
    rval = FALSE;

  if ( dbuf.buf )
    free(dbuf.buf);

  return rval;
}


/* add_attribute() adds a name(value) term to the list if value is provided
   (i.e. not NULL and non "")
*/

static int
add_attribute(term_t list, const char *value, functor_t functor)
{ if ( value && value[0] )
  { term_t h = PL_new_term_ref();
    int rval;

    rval = PL_unify_list(list, h, list) &&
	   PL_unify_term(h, PL_FUNCTOR, functor, PL_CHARS, value);

    PL_reset_term_refs(h);
    return rval;
  }

  return TRUE;
}


static int
mime_unify(term_t result, struct rfc2045 *rfc, const char *buffer)
{ term_t data = PL_new_term_ref();
  term_t subs = PL_new_term_ref();
  term_t atts = PL_new_term_ref();
  const char *type, *cset;

  if ( !PL_unify_term(result,
		      PL_FUNCTOR, FUNCTOR_mime3,
		        PL_TERM, atts,
		        PL_TERM, data,
		        PL_TERM, subs) )
    return FALSE;

  if ( rfc->isdummy )
  { if ( !PL_unify_nil(data) ||
	 !PL_unify_nil(atts) )
      return FALSE;
  } else
  { term_t at = PL_copy_term_ref(atts);
    const char *enc;
    const char *disp, *name, *fnam;

    const char *id   = rfc2045_content_id(rfc);
    const char *desc = rfc2045_content_description(rfc);
    const char *lang = rfc2045_content_language(rfc);
    const char *md5  = rfc2045_content_md5(rfc);

    rfc2045_mimeinfo(rfc, &type, &enc, &cset);
    rfc2045_dispositioninfo(rfc, &disp, &name, &fnam);

    if ( !add_attribute(at, type, FUNCTOR_type1) )              return FALSE;
    if ( !add_attribute(at, enc,  FUNCTOR_transfer_encoding1) ) return FALSE;
    if ( !add_attribute(at, cset, FUNCTOR_character_set1) )     return FALSE;
    if ( !add_attribute(at, id,   FUNCTOR_id1) )                return FALSE;
    if ( !add_attribute(at, desc, FUNCTOR_description1) )       return FALSE;
    if ( !add_attribute(at, lang, FUNCTOR_language1) )          return FALSE;
    if ( !add_attribute(at, disp, FUNCTOR_disposition1) )       return FALSE;
    if ( !add_attribute(at, name, FUNCTOR_name1) )              return FALSE;
    if ( !add_attribute(at, fnam, FUNCTOR_filename1) )          return FALSE;
    if ( !add_attribute(at, md5,  FUNCTOR_md51) )               return FALSE;

    if ( !PL_unify_nil(at) )
      return FALSE;
  }

  if ( rfc->firstpart )
  { term_t st = PL_copy_term_ref(subs);
    term_t s  = PL_new_term_ref();
    struct rfc2045 *sub;

    if ( !PL_unify_atom(data, ATOM_) )
      return FALSE;

    for(sub=rfc->firstpart; sub; sub = sub->next)
    { if ( sub->isdummy )
	continue;

      if ( !PL_unify_list(st, s, st) ||
	   !mime_unify(s, sub, buffer) )
	return FALSE;
    }
    return PL_unify_nil(st);
  } else
  { if ( !PL_unify_nil(subs) ||
	 !mime_unify_data(data, rfc, buffer, cset, type) )
      return FALSE;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_character_data()
    Get a buffer of data from a specification.  Currently the following
    specs are acceptable:

	stream(Stream)		All data from this stream
	stream(Stream, N)	At most N characters from stream
	Atom, String, CodeList	Data from native Prolog character data
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_character_data(term_t from, char **data, size_t *len, int *malloced)
{ atom_t name;
  size_t arity;
  char *buf;
  size_t size;

  if ( PL_get_name_arity(from, &name, &arity) && arity > 0 )
  { if ( name == ATOM_stream )
    { IOSTREAM *stream;
      term_t arg = PL_new_term_ref();

      _PL_get_arg(1, from, arg);
      if ( !PL_get_stream_handle(arg, &stream) )
	return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, from, "stream");

      if ( arity == 1 )			/* stream(Stream) */
      { int c;
	size_t done, allocated = 1024;

	if ( !(buf = malloc(allocated)) )
	  return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "allocate", "memory", 0);

	for( done=0; (c=Sgetcode(stream)) != EOF; )
	{ if ( done >= allocated )
	  { allocated *= 2;

	    if ( !(buf = realloc(buf, allocated)) )
	      return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "allocate", "memory", 0);
	  }

	  buf[done++] = c;
	}

	*len = done;
	*data = buf;
	*malloced = TRUE;

        return TRUE;
      }	else if ( arity == 2 )		/* stream(Stream, Length) */
      { long size;
	long done;
	int c;

	_PL_get_arg(2, from, arg);
	if ( !PL_get_long(arg, &size) || size < 0 )
	  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, arg, "natural");

	if ( !(buf = malloc(size)) )
	  return pl_error(NULL, 0, NULL, ERR_ERRNO, errno, "allocate", "memory", 0);

	for( done=0; (c=Sgetcode(stream)) != EOF && done < size; )
	  buf[done++] = c;

	*len = done;
	*data = buf;
	*malloced = TRUE;

        return TRUE;
      }
    }
  } else if ( PL_get_nchars(from, &size, data, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { *len = size;
    *malloced = FALSE;

    return TRUE;
  }

  return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1, from, "data");
}



static foreign_t
mime_parse(term_t handle, term_t result)
{ char *buf;
  size_t len = 0;
  int malloced = FALSE;
  struct rfc2045 *rfc;
  int rval;

  if ( !get_character_data(handle, &buf, &len, &malloced) )
    return FALSE;

  rfc = rfc2045_alloc();
  rfc2045_parse(rfc, buf, len);
  rval = mime_unify(result, rfc, buf);

  if ( malloced )
    free(buf);
  rfc2045_free(rfc);

  return rval;
}


static foreign_t
mime_default_charset(term_t old, term_t new)
{ if ( PL_unify_atom_chars(old, rfc2045_getdefaultcharset()) )
  { if ( PL_compare(old, new) != 0 )
    { char *cs;

      if ( PL_get_chars(new, &cs, CVT_ATOM|CVT_EXCEPTION) )
      { rfc2045_setdefaultcharset(cs);
	return TRUE;
      }
      return FALSE;
    }
    return TRUE;
  }
  return FALSE;
}



		 /*******************************
		 *	       ERRORS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Not typically elegant, but the documentation  whishes us to call exit(),
which is even worse.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
rfc2045_error(const char *errmsg)
{ term_t e = PL_new_term_ref();

  if ( (e=PL_new_term_ref()) &&
       PL_unify_term(e,
		     PL_FUNCTOR_CHARS, "error", 2,
		       PL_FUNCTOR_CHARS, "mime", 1,
		         PL_CHARS, errmsg,
		       PL_VARIABLE) )
    PL_throw(e);

  PL_fatal_error("Could not recover from rfc2045 error");
}

		 /*******************************
		 *	      INSTALL		*
		 *******************************/

#define mkfunctor(n, a) PL_new_functor(PL_new_atom(n), a)


install_t
install_mime()
{ ATOM_			     = PL_new_atom("");
  ATOM_stream		     = PL_new_atom("stream");

  FUNCTOR_type1		     = mkfunctor("type", 1);
  FUNCTOR_transfer_encoding1 = mkfunctor("transfer_encoding", 1);
  FUNCTOR_character_set1     = mkfunctor("character_set", 1);
  FUNCTOR_mime3	             = mkfunctor("mime", 3);
  FUNCTOR_id1                = mkfunctor("id", 1);
  FUNCTOR_description1       = mkfunctor("description", 1);
  FUNCTOR_language1          = mkfunctor("language", 1);
  FUNCTOR_md51               = mkfunctor("md5", 1);
  FUNCTOR_disposition1       = mkfunctor("disposition", 1);
  FUNCTOR_name1		     = mkfunctor("name", 1);
  FUNCTOR_filename1	     = mkfunctor("filename", 1);

  PL_register_foreign("mime_parse",           2, mime_parse,           0);
  PL_register_foreign("mime_default_charset", 2, mime_default_charset, 0);
}
