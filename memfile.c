/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2014, University of Amsterdam
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
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#ifdef O_PLMT
#include <pthread.h>
#endif
#include "error.h"

#ifdef O_PLMT
#define LOCK(mf)   pthread_mutex_lock(&(mf)->mutex)
#define UNLOCK(mf) pthread_mutex_unlock(&(mf)->mutex)
#else
#define LOCK(mf)
#define UNLOCK(mf)
#endif

static atom_t ATOM_encoding;
static atom_t ATOM_unknown;
static atom_t ATOM_octet;
static atom_t ATOM_ascii;
static atom_t ATOM_iso_latin_1;
static atom_t ATOM_text;
static atom_t ATOM_utf8;
static atom_t ATOM_unicode_be;
static atom_t ATOM_unicode_le;
static atom_t ATOM_wchar_t;
static atom_t ATOM_read;
static atom_t ATOM_write;
static atom_t ATOM_append;
static atom_t ATOM_update;
static atom_t ATOM_insert;
static atom_t ATOM_free_on_close;

#define MEMFILE_MAGIC	0x5624a6b3L
#define NOSIZE ((size_t)-1)

#define	V_CHARCOUNT	0x01
#define	V_LINENO	0x02
#define	V_LINEPOS	0x04
#define V_ALL		0x07

typedef struct
{ size_t	byte_count;		/* Byte position in MF */
  size_t	char_count;		/* Corresponding logical char */
  size_t	line_no;		/* Line */
  size_t	line_pos;		/* Line position */
  unsigned int	valid;			/* Valid mask */
} pos_cache;


typedef struct
{ char	       *data;			/* data of the file */
  size_t	end;			/* End of buffer */
  size_t	gap_start;		/* Insertion point */
  size_t	gap_size;		/* Insertion hole */
  size_t	char_count;		/* size in characters */
  pos_cache	pcache;			/* Cached position */
  size_t	here;			/* read pointer */
  IOSTREAM     *stream;			/* Stream hanging onto it */
  atom_t	symbol;			/* <memory_file>(%p) */
  atom_t	atom;			/* Created from atom */
  atom_t	mode;			/* current open mode */
#ifdef O_PLMT
  pthread_mutex_t mutex;		/* Our lock */
#endif
  int		magic;			/* MEMFILE_MAGIC */
  int		free_on_close;		/* free if it is closed */
  IOENC		encoding;		/* encoding of the data */
} memfile;

static int	destroy_memory_file(memfile *m);


		 /*******************************
		 *	      SYMBOL		*
		 *******************************/

static void
acquire_memfile_symbol(atom_t symbol)
{ memfile *mf = PL_blob_data(symbol, NULL, NULL);
  mf->symbol = symbol;
}

static int
release_memfile_symbol(atom_t symbol)
{ memfile *mf = PL_blob_data(symbol, NULL, NULL);

  destroy_memory_file(mf);
  return TRUE;
}

static int
compare_memfile_symbols(atom_t a, atom_t b)
{ memfile *mfa = PL_blob_data(a, NULL, NULL);
  memfile *mfb = PL_blob_data(b, NULL, NULL);

  return ( mfa > mfb ?  1 :
	   mfa < mfb ? -1 : 0
	 );
}


static int
write_memfile_symbol(IOSTREAM *s, atom_t symbol, int flags)
{ memfile *mf = PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<memory_file>(%p)", mf);
  return TRUE;
}


static PL_blob_t memfile_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "memory_file",
  release_memfile_symbol,
  compare_memfile_symbols,
  write_memfile_symbol,
  acquire_memfile_symbol
};


static int
unify_memfile(term_t handle, memfile *mf)
{ if ( PL_unify_blob(handle, mf, sizeof(*mf), &memfile_blob) )
    return TRUE;

  if ( !PL_is_variable(handle) )
    return PL_uninstantiation_error(handle);

  return FALSE;					/* (resource) error */
}


static int
get_memfile(term_t handle, memfile **mfp)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(handle, &data, NULL, &type) && type == &memfile_blob)
  { memfile *mf = data;

    assert(mf->magic == MEMFILE_MAGIC);
    LOCK(mf);

    if ( mf->symbol )
    { *mfp = mf;

      return TRUE;
    }

    PL_permission_error("access", "freed_memory_file", handle);
    return FALSE;
  }

  return PL_type_error("memory_file", handle);
}


static void
release_memfile(memfile *mf)
{ UNLOCK(mf);
}


static void
empty_memory_file(memfile *m)
{ if ( m->data )
    free(m->data);

  m->encoding     = ENC_UTF8;
  m->data         = NULL;
  m->end          = 0;
  m->gap_start    = 0;
  m->gap_size     = 0;
  m->char_count   = NOSIZE;
  m->pcache.valid = 0;
  m->here         = 0;
}


static foreign_t
new_memory_file(term_t handle)
{ memfile *m = calloc(1, sizeof(*m));

  if ( !m )
    return PL_resource_error("memory");

  m->magic    = MEMFILE_MAGIC;
  m->encoding = ENC_UTF8;
#ifdef O_PLMT
  pthread_mutex_init(&m->mutex, NULL);
#endif

  if ( unify_memfile(handle, m) )
    return TRUE;

  destroy_memory_file(m);
  return FALSE;
}


static void
clean_memory_file(memfile *m)
{ if ( m->stream )
  { Sclose(m->stream);
    m->stream = NULL;
  }
  if ( m->atom )
  { PL_unregister_atom(m->atom);
    m->atom = 0;
    m->data = NULL;
  } else if ( m->data )
  { free(m->data);
    m->data = NULL;
  }
}


static int
destroy_memory_file(memfile *m)
{ clean_memory_file(m);
#ifdef O_PLMT
  pthread_mutex_destroy(&m->mutex);
#endif
  free(m);

  return TRUE;
}


static foreign_t
free_memory_file(term_t handle)
{ memfile *m;

  if ( get_memfile(handle, &m) )
  { m->symbol = 0;
    clean_memory_file(m);
    release_memfile(m);
    return TRUE;
  }

  return FALSE;
}

		 /*******************************
		 *	     CHECKING		*
		 *******************************/

#define ISUTF8_CB(c)  (((c)&0xc0) == 0x80) /* Is continuation byte */

#ifdef O_SECURE

#define CONT(in,i)   (assert(ISUTF8_CB(in[i])),1)
#define IS_UTF8_2BYTE(in) \
	((in[0]&0xe0) == 0xc0 && CONT(in,1))
#define IS_UTF8_3BYTE(in) \
	((in[0]&0xf0) == 0xe0 && CONT(in,1)&&CONT(in,2))
#define IS_UTF8_4BYTE(in) \
	((in[0]&0xf8) == 0xf0 && CONT(in,1)&&CONT(in,2)&&CONT(in,3))
#define IS_UTF8_5BYTE(in) \
	((in[0]&0xfc) == 0xf8 && CONT(in,1)&&CONT(in,2)&&CONT(in,3)&&CONT(in,4))
#define IS_UTF8_6BYTE(in) \
	((in[0]&0xfe) == 0xfc && CONT(in,1)&&CONT(in,2)&&CONT(in,3)&&CONT(in,4)&&CONT(in,5))

static size_t
check_utf8_seq(char *s, size_t len)
{ size_t count = 0;
  size_t skip;

  while(len > 0)
  { if ( (*s&0x80) )
    {      if ( IS_UTF8_2BYTE(s) ) skip = 2;
      else if ( IS_UTF8_3BYTE(s) ) skip = 3;
      else if ( IS_UTF8_4BYTE(s) ) skip = 4;
      else if ( IS_UTF8_5BYTE(s) ) skip = 5;
      else if ( IS_UTF8_6BYTE(s) ) skip = 6;
      else assert(0);
    } else
      skip = 1;

    assert(len >= skip);
    len -= skip;
    s   += skip;

    count++;
  }

  return count;
}


static void
check_memfile(memfile *mf)
{ size_t count = 0;

  count += check_utf8_seq(&mf->data[0],                          mf->gap_start);
  count += check_utf8_seq(&mf->data[mf->gap_start+mf->gap_size],
			  mf->end-(mf->gap_size+mf->gap_start));

  assert(mf->char_count == NOSIZE || mf->char_count == count);
}

#else /*O_SECURE*/

#define check_memfile(mf) (void)0

#endif /*O_SECURE*/

		 /*******************************
		 *	 STREAM FUNCTIONS	*
		 *******************************/

#define CHECK_MEMFILE(m) \
	if ( m->magic != MEMFILE_MAGIC ) \
	{ errno = EINVAL; \
	  return -1; \
	}

static ssize_t
read_memfile(void *handle, char *buf, size_t size)
{ memfile *m = handle;
  size_t done = 0;

  CHECK_MEMFILE(m);
  if ( m->here < m->gap_start )
  { size_t before_gap = m->gap_start - m->here;
    if ( size <= before_gap )
    { memcpy(buf, &m->data[m->here], size);
      m->here += size;
      return size;
    } else
    { memcpy(buf, &m->data[m->here], before_gap);
      m->here += before_gap;
      done = before_gap;
    }
  }

  /* we are now after the gap */
  { size_t left = size - done;
    size_t start = m->here + m->gap_size;
    size_t avail = m->end - start;

    if ( avail < left )
    { left = avail;
      size = done + avail;
    }
    m->here += left;
    memcpy(&buf[done], &m->data[start], left);
    return size;
  }
}


static size_t
memfile_nextsize(size_t needed)
{ size_t size = 512;

  while ( size < needed )
    size *= 2;

  return size;
}


static int
ensure_gap_size(memfile *m, size_t size)
{ if ( m->gap_size < size )
  { size_t nextsize = memfile_nextsize(m->end+(size-m->gap_size));
    void *ptr;

    if ( m->data )
      ptr = realloc(m->data, nextsize);
    else
      ptr = malloc(nextsize);

    if ( ptr != NULL )
    { size_t after_gap = m->end - (m->gap_start + m->gap_size);

      m->data = ptr;
      memmove(&m->data[nextsize-after_gap], &m->data[m->end-after_gap], after_gap);
      m->gap_size += nextsize - m->end;
      m->end = nextsize;
    } else
    { return -1;
    }
  }

  return 0;
}


static void
move_gap_to(memfile *m, size_t to)
{ assert(to <= m->end - m->gap_size);

  if ( to != m->gap_start )
  { if ( to > m->gap_start )		/* move forwards */
    { memmove(&m->data[m->gap_start],
	      &m->data[m->gap_start+m->gap_size],
	      to - m->gap_start);
      m->gap_start = to;
    } else				/* move backwards */
    { memmove(&m->data[to+m->gap_size],
	      &m->data[to],
	      m->gap_start - to);
      m->gap_start = to;
    }
  }
}


static ssize_t
write_memfile(void *handle, char *buf, size_t size)
{ memfile *m = handle;
  int rc;

  CHECK_MEMFILE(m);
  if ( size > 0 )
  { m->char_count = NOSIZE;		/* TBD: Dynamically update? */
    if ( m->gap_start < m->pcache.byte_count )
      m->pcache.valid = 0;

    if ( m->mode == ATOM_update )
    { size_t start = m->gap_start + m->gap_size;
      size_t after = m->end - start;

      if ( size > after )
      { if ( (rc=ensure_gap_size(m, size-after)) != 0 )
	  return rc;
	m->gap_size -= size-after;
      }
      memmove(&m->data[m->gap_start], buf, size);
      m->gap_start += size;
    } else
    { if ( (rc=ensure_gap_size(m, size)) != 0 )
	return rc;
      memcpy(&m->data[m->gap_start], buf, size);
      m->gap_start += size;
      m->gap_size  -= size;
    }
  }

  return size;
}

static int64_t
seek64_memfile(void *handle, int64_t offset, int whence)
{ memfile *m = handle;

  CHECK_MEMFILE(m);
  switch(whence)
  { case SIO_SEEK_SET:
      break;
    case SIO_SEEK_CUR:
      offset += m->here;
      break;
    case SIO_SEEK_END:
      offset = (m->end-m->gap_size) - offset;
      break;
    default:
      errno = EINVAL;
      return -1;
  }
  if ( offset < 0 || offset > (m->end - m->gap_size) )
  { errno = EINVAL;
    return -1;
  }

  if ( (m->stream->flags & SIO_INPUT) )	/* reading */
  { m->here = offset;
  } else
  { move_gap_to(m, offset);
  }

  return offset;
}

static long
seek_memfile(void *handle, long offset, int whence)
{ return (long)seek64_memfile(handle, (int64_t)offset, whence);
}

static int
close_memfile(void *handle)
{ memfile *m = handle;

  CHECK_MEMFILE(m);
  m->stream = NULL;
  m->mode = 0;
  if ( m->free_on_close )
    clean_memory_file(m);
  PL_unregister_atom(m->symbol);

  return 0;
}


IOFUNCTIONS memfile_functions =
{ read_memfile,
  write_memfile,
  seek_memfile,
  close_memfile,
  NULL,					/* control */
  seek64_memfile
};


static foreign_t
alreadyOpen(term_t handle, const char *op)
{ return pl_error(NULL, 0, "already open",
		  ERR_PERMISSION, handle, op, "memory_file");
}


static struct encname
{ IOENC  code;
  atom_t *name;
} encoding_names[] =
{ { ENC_UNKNOWN,     &ATOM_unknown },
  { ENC_OCTET,       &ATOM_octet },
  { ENC_ASCII,       &ATOM_ascii },
  { ENC_ISO_LATIN_1, &ATOM_iso_latin_1 },
  { ENC_ANSI,	     &ATOM_text },
  { ENC_UTF8,        &ATOM_utf8 },
  { ENC_UNICODE_BE,  &ATOM_unicode_be },
  { ENC_UNICODE_LE,  &ATOM_unicode_le },
  { ENC_WCHAR,	     &ATOM_wchar_t },
  { ENC_UNKNOWN,     NULL },
};


static IOENC
atom_to_encoding(atom_t a)
{ struct encname *en;

  for(en=encoding_names; en->name; en++)
  { if ( *en->name == a )
      return en->code;
  }

  return ENC_UNKNOWN;
}


static int
get_encoding(term_t t, IOENC *enc)
{ atom_t en;

  if ( PL_get_atom(t, &en) )
  { IOENC encoding;

    if ( (encoding = atom_to_encoding(en)) == ENC_UNKNOWN )
      return pl_error(NULL, 0, NULL, ERR_DOMAIN, t, "encoding");

    *enc = encoding;
    return TRUE;
  }

  return pl_error(NULL, 0, NULL, ERR_TYPE, t, "encoding");
}


static foreign_t
open_memory_file4(term_t handle, term_t mode, term_t stream, term_t options)
{ memfile *m;
  int rc;

  if ( get_memfile(handle, &m) )
  { int flags = SIO_FBUF|SIO_RECORDPOS|SIO_NOMUTEX;
    atom_t iom;
    IOSTREAM *fd;
    IOENC encoding;
    int free_on_close = FALSE;

    if ( m->stream )
    { rc = alreadyOpen(handle, "open");
      goto out;
    }
    if ( !PL_get_atom(mode, &iom) )
    { rc = pl_error("open_memory_file", 3, NULL, ERR_ARGTYPE, 2,
		    mode, "io_mode");
      goto out;
    }

    encoding = m->encoding;

    if ( options )
    { term_t tail = PL_copy_term_ref(options);
      term_t head = PL_new_term_ref();

      while(PL_get_list(tail, head, tail))
      { int arity;
	atom_t name;

	if ( PL_get_name_arity(head, &name, &arity) && arity == 1 )
	{ term_t arg = PL_new_term_ref();

	  _PL_get_arg(1, head, arg);
	  if ( name == ATOM_encoding )
	  { if ( !get_encoding(arg, &encoding) )
	    { rc = FALSE;
	      goto out;
	    }
	  } else if ( name == ATOM_free_on_close )
	  { if ( !PL_get_bool(arg, &free_on_close) )
	    { rc = pl_error("open_memory_file", 4, NULL, ERR_TYPE,
			    arg, "boolean");
	      goto out;
	    }
	  }
	} else
	{ rc = pl_error("open_memory_file", 4, NULL, ERR_TYPE, head, "option");
	  goto out;
	}
      }
      if ( !PL_get_nil(tail) )
      { rc = pl_error("open_memory_file", 4, NULL, ERR_TYPE, tail, "list");
	goto out;
      }
    }

    if ( iom == ATOM_write  || iom == ATOM_append ||
	 iom == ATOM_update || iom == ATOM_insert )
    { flags |= SIO_OUTPUT;
      if ( m->atom )
      { rc = pl_error("open_memory_file", 3, "read only",
		      ERR_PERMISSION, handle, "modify", "memory_file");
	goto out;
      }

      if ( iom == ATOM_write )
      { empty_memory_file(m);
	m->encoding   = encoding;
      } else
      { if ( m->encoding != encoding )
	{ rc = pl_error("open_memory_file", 3, "inconsistent encoding",
			ERR_PERMISSION, handle, PL_atom_chars(iom), "memory_file");
	  goto out;
	}
	if ( iom == ATOM_append )
	{ move_gap_to(m, m->end - m->gap_size);
	} else
	{ move_gap_to(m, 0);
	}
      }

    } else if ( iom == ATOM_read )
    { flags |= SIO_INPUT;
      m->free_on_close = free_on_close;
      m->here = 0;
    } else
    { rc = pl_error("open_memory_file", 3, NULL, ERR_DOMAIN,
		    mode, "io_mode");
      goto out;
    }

    if ( encoding != ENC_OCTET )
      flags |= SIO_TEXT;

    if ( !(fd = Snew(m, flags, &memfile_functions)) )
    { rc = pl_error("open_memory_file", 3, NULL, ERR_ERRNO, errno,
		    "create", "memory_file", handle);
      goto out;
    }

    if ( (rc=PL_unify_stream(stream, fd)) )
    { fd->encoding = encoding;
      m->stream = fd;
      m->mode = iom;
      PL_register_atom(m->symbol);
    } else
    { Sclose(fd);
    }

  out:
    release_memfile(m);
  } else
    rc = FALSE;

  return rc;
}


static foreign_t
open_memory_file(term_t handle, term_t mode, term_t stream)
{ return open_memory_file4(handle, mode, stream, 0);
}


static int
get_size_mf(memfile *m, IOENC encoding, size_t *sizep)
{ size_t size;

  if ( m->char_count != NOSIZE && encoding == m->encoding )
  { size = m->char_count;
  } else
  { size = m->end - m->gap_size;

    switch( encoding )
    { case ENC_ISO_LATIN_1:
      case ENC_OCTET:
      case ENC_ASCII:
	break;
      case ENC_UNICODE_BE:
      case ENC_UNICODE_LE:
	size /= 2;
        break;
      case ENC_WCHAR:
	size /= sizeof(wchar_t);
        break;
      case ENC_UTF8:
      { size_t gap_end = m->gap_start+m->gap_size;
	size = ( PL_utf8_strlen(m->data, m->gap_start) +
		 PL_utf8_strlen(&m->data[gap_end], m->end-gap_end)
	       );  /* assumes UTF-8 sequences are not broken over the gap */
	break;
      }
      default:
	assert(0);
        return FALSE;
    }

    if ( encoding == m->encoding )
      m->char_count = size;
  }

  *sizep = size;
  return TRUE;
}



static foreign_t
size_memory_file(term_t handle, term_t sizeh, term_t encoding)
{ memfile *m;
  int rc;

  if ( get_memfile(handle, &m) )
  { size_t size;
    IOENC size_enc;

    if ( m->stream && !m->atom )
    { rc = alreadyOpen(handle, "size");
      goto out;
    }

    if ( encoding )
    { if ( !get_encoding(encoding, &size_enc) )
      { rc = FALSE;
	goto out;
      }
    } else
      size_enc = m->encoding;

    rc = ( get_size_mf(m, size_enc, &size) &&
	   PL_unify_int64(sizeh, size)
	 );
  out:
    release_memfile(m);
  } else
    rc = FALSE;

  return rc;
}


static foreign_t
size_memory_file2(term_t handle, term_t size)
{ return size_memory_file(handle, size, 0);
}


static foreign_t
size_memory_file3(term_t handle, term_t size, term_t encoding)
{ return size_memory_file(handle, size, encoding);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
utf8_position_memory_file(+MF, -Here, -Size)

Given  MF  is  a  UTF-8  encoded  memory   file,  unify  here  with  the
byte-position of the read-pointer and Size with   the  total size of the
memory file in bytes. This is a bit hacky predicate, but the information
is easily available at low cost, while it is very valuable for producing
answers  in  content-length  computation  of    the   HTTP  server.  See
http_wrapper.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
utf8_position(term_t handle, term_t here, term_t size)
{ memfile *m;
  int rc;

  if ( get_memfile(handle, &m) )
  { if ( m->encoding != ENC_UTF8 )
    { rc = pl_error(NULL, 0, "no UTF-8 encoding",
		    ERR_PERMISSION, handle, "utf8_position", "memory_file");
      goto out;
    }
    if ( !PL_unify_integer(size, m->end - m->gap_size) )
    { rc = FALSE;
      goto out;
    }

    if ( m->stream )
    { IOPOS *op = m->stream->position;
      long p;

      m->stream->position = NULL;
      p = Stell(m->stream);
      m->stream->position = op;

      rc = PL_unify_integer(here, p);
    } else
      rc = PL_unify_integer(here, 0);

  out:
    release_memfile(m);
  } else
    rc = FALSE;

  return rc;
}


static foreign_t
atom_to_memory_file(term_t atom, term_t handle)
{ atom_t a;

  if ( PL_get_atom(atom, &a) )
  { memfile *m = calloc(1, sizeof(*m));

    if ( !m )
      return pl_error(NULL, 0, NULL, ERR_ERRNO, errno,
		      "create", "memory_file", handle);

    m->atom = a;
    PL_register_atom(m->atom);
    m->magic = MEMFILE_MAGIC;

    if ( (m->data = (char *)PL_atom_nchars(a, &m->char_count)) )
    { m->encoding  = ENC_ISO_LATIN_1;
      m->end       = m->char_count;
      m->gap_start = m->end;
    } else if ( (m->data = (char *)PL_atom_wchars(a, &m->char_count)) )
    { m->encoding = ENC_WCHAR;
      m->end = m->char_count * sizeof(wchar_t);
      m->gap_start = m->end;
    } else if ( PL_blob_data(a, &m->char_count, NULL) )
    { m->data = PL_blob_data(a, &m->end, NULL);
      m->encoding = ENC_OCTET;
      m->char_count = m->end;
      m->gap_start  = m->end;
    }

#ifdef O_PLMT
    pthread_mutex_init(&m->mutex, NULL);
#endif

    if ( unify_memfile(handle, m) )
    { return TRUE;
    } else
    { destroy_memory_file(m);
      return FALSE;
    }
  } else
  { return pl_error(NULL, 0, NULL, ERR_ARGTYPE, 1,
		    atom, "atom");
  }
}

		 /*******************************
		 *	  DIRECT EXCHANGE	*
		 *******************************/

static int
can_modify_memory_file(term_t handle, memfile *mf)
{ if ( mf->atom )
    return pl_error(NULL, 0, "read only",
		    ERR_PERMISSION, handle, "modify", "memory_file");
  if ( mf->stream )
    return alreadyOpen(handle, "modify");

  return TRUE;
}


/* Get the byte offset for a character
*/

static char *
utf8_skip_char(const char *in, const char *e)
{ if ( !(in[0]&0x80) )
  { return (char*)in+1;
  } else
  { in++;
    while ( in < e && ISUTF8_CB(in[0]) )
      in++;
    return (char*)in;
  }
}


/* Skip chars forward from a byte position, returning the new
   byte position in the memory file or NOSIZE if we would skip
   outside the limits of the memory file.  Returns:

     OUTOFRANGE if memfile is too small
     FALSE      if the encoding is not supported
     TRUE       if ok
*/

#define OUTOFRANGE -1

static int
mf_skip(memfile *mf, IOENC encoding, size_t from, size_t chars, size_t *end)
{ size_t to;

  switch(encoding)
  { case ENC_OCTET:
    case ENC_ASCII:
    case ENC_ISO_LATIN_1:
      to = from+chars;
      break;
    case ENC_UTF8:
    { const char *start, *s, *e;
      const size_t chars0 = (from == 0 ? chars : NOSIZE);

      if ( from == 0 &&
	   (mf->pcache.valid & V_CHARCOUNT) &&
	   chars >= mf->pcache.char_count )
      { from   = mf->pcache.byte_count;
	chars -= mf->pcache.char_count;
      }

      if ( from < mf->gap_start )
      { start = s = &mf->data[from];
	e = &mf->data[mf->gap_start];

	while(chars>0 && s<e)
	{ chars--;
	  s = utf8_skip_char(s, e);
	}
	from += s - start;
	if ( chars == 0 )
	{ utf8_out:
	  if ( chars0 != NOSIZE )
	  { mf->pcache.char_count = chars0;
	    mf->pcache.byte_count = from;
	    mf->pcache.valid |= V_CHARCOUNT;
	  }
	  *end = from;
	  return TRUE;
	}
	assert(s == e);
      }

      start = s = &mf->data[mf->gap_size+from];
      e = &mf->data[mf->end];
      while(chars>0 && s<e)
      { chars--;
	s = utf8_skip_char(s, e);
      }
      from += s - start;
      if ( chars == 0 )
	goto utf8_out;
      goto outofrange;
    }
    case ENC_UNICODE_BE:
    case ENC_UNICODE_LE:
      to = from + 2*chars;
      break;
    case ENC_WCHAR:
      to = from + sizeof(wchar_t) * chars;
      break;
    default:
      return PL_representation_error("encoding");
  }

  if ( to > mf->end - mf->gap_size )
  { outofrange:
    *end = mf->end - mf->gap_size;
    return OUTOFRANGE;
  }

  *end = to;
  return TRUE;
}


static int
get_offset(term_t where, memfile *mf, IOENC encoding, size_t *pos)
{ size_t p;

  if ( PL_get_size_ex(where, &p) )
  { int rc = mf_skip(mf, encoding, 0, p, pos);

    if ( rc != OUTOFRANGE )
      return rc;

    return PL_domain_error("offset", where);
  }

  return FALSE;
}


static foreign_t
insert_memory_file(term_t handle, term_t where, term_t data)
{ memfile *m;
  int rc;

  if ( get_memfile(handle, &m) )
  { size_t pos;
    int flags = CVT_ALL|CVT_WRITEQ|CVT_EXCEPTION;

    if ( can_modify_memory_file(handle, m) &&
	 get_offset(where, m, m->encoding, &pos) )
    { move_gap_to(m, pos);
      switch(m->encoding)
      { case ENC_OCTET:
	case ENC_ASCII:
	case ENC_ISO_LATIN_1:
	case ENC_ANSI:
	case ENC_UTF8:
	{ size_t len;
	  char *buf;
	  int rep = ( m->encoding == ENC_UTF8 ? REP_UTF8 :
		      m->encoding == ENC_ANSI ? REP_MB :
						REP_ISO_LATIN_1
		    );

	  if ( (rc=PL_get_nchars(data, &len, &buf, flags|rep)) )
	  { if ( write_memfile(m, buf, len) < 0 )
	      rc = PL_resource_error("memory");
	    check_memfile(m);
	  }
	  break;
	}
	case ENC_WCHAR:
	{ size_t len;
	  wchar_t *buf;

	  if ( (rc=PL_get_wchars(data, &len, &buf, flags)) )
	  { if ( write_memfile(m, (void*)buf, len*sizeof(wchar_t)) < 0 )
	      rc = PL_resource_error("memory");
	  }
	  break;
	}
	default:
	  rc = PL_representation_error("encoding");
      }
    } else
      rc = FALSE;

    release_memfile(m);
  } else
    rc = FALSE;

  return rc;
}


static foreign_t
delete_memory_file(term_t handle, term_t where, term_t len)
{ memfile *m;
  int rc;

  if ( get_memfile(handle, &m) )
  { size_t pos, end;
    size_t l;

    if ( can_modify_memory_file(handle, m) &&
	 get_offset(where, m, m->encoding, &pos) &&
	 PL_get_size_ex(len, &l) &&
	 mf_skip(m, m->encoding, pos, l, &end) != FALSE )
    { if ( end > pos )
      { if ( pos < m->pcache.byte_count )
	  m->pcache.valid = 0;

	move_gap_to(m, pos);
	m->gap_size += end-pos;
	m->char_count = NOSIZE;
      }
      rc = TRUE;
    } else
      rc = FALSE;

    release_memfile(m);
  } else
    rc = FALSE;

  return rc;
}


static foreign_t
mf_to_text(term_t handle, memfile *m, size_t from, size_t len,
	   term_t atom, term_t encoding, int flags)
{ IOENC enc;
  size_t start, end;

  if ( m->stream && (m->stream->flags & SIO_OUTPUT))
    return alreadyOpen(handle, "to_atom");

  if ( encoding )
  { if ( !get_encoding(encoding, &enc) )
      return FALSE;
  } else
    enc = m->encoding;

  if ( from == NOSIZE )
  { start = 0;
  } else
  { if ( mf_skip(m, enc, 0, from, &start) != TRUE )
      return FALSE;
  }

  if ( len == NOSIZE )
  { end = m->end - m->gap_size;
  } else
  { if ( mf_skip(m, enc, start, len, &end) != TRUE )
      return FALSE;
  }

  if ( m->data )
  { size_t len = end-start;
    const char *data;

    if ( start <= m->gap_start && end <= m->gap_start )
    { data = &m->data[start];
    } else if ( start >= m->gap_start+m->gap_size )
    { data = &m->data[m->gap_size + (start-m->gap_start)];
    } else
    { move_gap_to(m, end);
      data = &m->data[start];
    }

    switch(enc)
    { case ENC_ISO_LATIN_1:
      case ENC_OCTET:
	return PL_unify_chars(atom, flags, len, data);
      case ENC_WCHAR:
	return PL_unify_wchars(atom, flags,
			       len/sizeof(wchar_t),
			       (pl_wchar_t*)data);
      case ENC_UTF8:
	return PL_unify_chars(atom, flags|REP_UTF8, len, data);
      default:
	assert(0);
    }
  } else
    return PL_unify_chars(atom, flags, 0, "");

  return FALSE;
}


static foreign_t
memory_file_to_text(term_t handle, term_t text, term_t encoding, int flags)
{ memfile *mf;
  int rc;

  if ( get_memfile(handle, &mf) )
  { rc = mf_to_text(handle, mf, NOSIZE, NOSIZE, text, encoding, flags);
    release_memfile(mf);
  } else
    rc = FALSE;

  return rc;
}


static foreign_t
memory_file_to_atom2(term_t handle, term_t atom)
{ return memory_file_to_text(handle, atom, 0, PL_ATOM);
}


static foreign_t
memory_file_to_atom3(term_t handle, term_t atom, term_t encoding)
{ return memory_file_to_text(handle, atom, encoding, PL_ATOM);
}


static foreign_t
memory_file_to_codes2(term_t handle, term_t atom)
{ return memory_file_to_text(handle, atom, 0, PL_CODE_LIST);
}


static foreign_t
memory_file_to_codes3(term_t handle, term_t atom, term_t encoding)
{ return memory_file_to_text(handle, atom, encoding, PL_CODE_LIST);
}


static foreign_t
memory_file_to_string2(term_t handle, term_t atom)
{ return memory_file_to_text(handle, atom, 0, PL_STRING);
}


static foreign_t
memory_file_to_string3(term_t handle, term_t atom, term_t encoding)
{ return memory_file_to_text(handle, atom, encoding, PL_STRING);
}


static int
get_size_or_var(term_t t, size_t *sp)
{ if ( PL_is_variable(t) )
  { *sp = NOSIZE;
    return TRUE;
  }

  return PL_get_size_ex(t, sp);
}


static foreign_t
memory_file_substring(term_t handle,
		      term_t before, term_t len, term_t after,
		      term_t string)
{ memfile *mf;
  int rc;

  if ( (rc=get_memfile(handle, &mf)) )
  { size_t b, l, a, size;

    if ( get_size_or_var(before, &b) &&
	 get_size_or_var(len, &l) &&
	 get_size_or_var(after, &a) &&
	 get_size_mf(mf, mf->encoding, &size) )
    { if ( b != NOSIZE && l != NOSIZE )
      { rc = ( mf_to_text(handle, mf, b, l, string, 0, PL_STRING) &&
	       PL_unify_int64(after, size-(b+l)) );
      } else if ( b != NOSIZE && a != NOSIZE )
      {	rc = ( mf_to_text(handle, mf, b, size-(b+a), string, 0, PL_STRING) &&
	       PL_unify_int64(len, size-(b+a)) );
      } else if ( l != NOSIZE && a != NOSIZE )
      { rc = ( mf_to_text(handle, mf, size-(l+a), l, string, 0, PL_STRING) &&
	       PL_unify_int64(before, size-(l+a)) );
      } else
      { rc = PL_instantiation_error(b != NOSIZE ? before : len);
      }
    } else
      rc = FALSE;

    release_memfile(mf);
  }

  return rc;
}

/* skip lines from a given byte-offset, returning the byte-offset
   for the beginning of the next line and the logical character
   count that belongs to that.
*/

static int
skip_lines(memfile *mf, size_t from, size_t lines,
	   size_t *startp, size_t *chcountp)
{ const char *start, *s, *e;
  size_t chcount = 0;

  if ( lines == 0 )
  { *startp = from;
    *chcountp = 0;
    return TRUE;
  }

  if ( from < mf->gap_start )
  { start = s = mf->data+from;
    e = &mf->data[mf->gap_start];
  } else
  { after_gap:
    start = s = &mf->data[mf->gap_size+from];
    e = &mf->data[mf->end];
  }

  switch(mf->encoding)
  { case ENC_OCTET:
    case ENC_ASCII:
    case ENC_ISO_LATIN_1:
      while( s < e )
      { if ( *s++ == '\n' )
	{ if ( --lines == 0 )
	  { *startp   = from + (s-start) + 1;
	    *chcountp = chcount + (s-start) + 1;
	    return TRUE;
	  }
	}
      }
      chcount += e-s;
      break;
    case ENC_UTF8:
      while( s < e )
      { chcount++;
	if ( *s == '\n' )
	{ if ( --lines == 0 )
	  { *startp   = from + (s-start) + 1;
	    *chcountp = chcount;
	    return TRUE;
	  }
	}
	s = utf8_skip_char(s, e);
      }
      break;
    case ENC_WCHAR:
    { const wchar_t *ws = (const wchar_t*)s;
      const wchar_t *we = (const wchar_t*)e;
      const wchar_t *wstart = ws;

      while( ws < we )
      { if ( *ws++ == '\n' )
	{ if ( --lines == 0 )
	  { *startp   = from + (ws-wstart) + 1;
	    *chcountp = chcount + (ws-wstart) + 1;
	    return TRUE;
	  }
	}
      }
      chcount += we-ws;
      break;
    }
    default:
      return PL_representation_error("encoding");
  }

  if ( from < mf->gap_start )
  { from = mf->gap_start;
    goto after_gap;
  }

  *startp   = mf->end;
  *chcountp = chcount;

  return OUTOFRANGE;
}


/** memory_file_line_position(+MF, +Line, +LinePos, -Offset) is det.
    memory_file_line_position(+MF, -Line, -LinePos, +Offset) is det.

True when Offset is the character offset for Line:LinePos.
*/

static foreign_t
memory_file_line_position(term_t handle,
			  term_t line, term_t linepos, term_t offset)
{ memfile *mf;
  int rc;

  if ( get_memfile(handle, &mf) )
  { size_t l, lp, o;

    if ( get_size_or_var(line, &l) &&
	 get_size_or_var(linepos, &lp) &&
	 get_size_or_var(offset, &o) )
    { if ( l != NOSIZE && lp != NOSIZE )
      { size_t lstart, nstart;
	size_t chcount;
	size_t linelen;

	if ( l == 0 )
	{ rc = PL_domain_error("not_less_than_one", line);
	  goto out;
	}
	l--;

	if (     skip_lines(mf, 0,      l, &lstart, &chcount)  == TRUE &&
	     (rc=skip_lines(mf, lstart, 1, &nstart, &linelen)) != FALSE &&
	     (lp < linelen || (lp == linelen && rc == OUTOFRANGE)) )
	  rc = PL_unify_int64(offset, chcount+lp);
	else
	  rc = FALSE;
      } else if ( o != NOSIZE )
      { size_t chcount = 0;
	size_t lstart = 0;
	size_t line_count = 1;

	do
	{ size_t linelen;

	  if ( (rc=skip_lines(mf, lstart, 1, &lstart, &linelen)) != FALSE )
	  { if (  chcount + linelen > o ||
		 (chcount + linelen == o && rc == OUTOFRANGE)
	       )
	    { rc = ( PL_unify_int64(line, line_count) &&
		     PL_unify_int64(linepos, o-chcount) );
	      goto out;
	    }
	  }
	  line_count++;
	  chcount += linelen;
	} while(chcount < o && rc == TRUE);

	rc = FALSE;
      } else
      { rc = PL_instantiation_error(offset);
      }
    } else
      rc = FALSE;

  out:
    release_memfile(mf);
  } else
    rc = FALSE;

  return rc;
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install_memfile()
{ MKATOM(encoding);
  MKATOM(unknown);
  MKATOM(octet);
  MKATOM(ascii);
  MKATOM(iso_latin_1);
  MKATOM(text);
  MKATOM(utf8);
  MKATOM(unicode_be);
  MKATOM(unicode_le);
  MKATOM(wchar_t);
  MKATOM(read);
  MKATOM(write);
  MKATOM(append);
  MKATOM(update);
  MKATOM(insert);
  MKATOM(free_on_close);

  PL_register_foreign("new_memory_file",	   1, new_memory_file,	      0);
  PL_register_foreign("free_memory_file",	   1, free_memory_file,	      0);
  PL_register_foreign("size_memory_file",	   2, size_memory_file2,      0);
  PL_register_foreign("size_memory_file",	   3, size_memory_file3,      0);
  PL_register_foreign("open_memory_file",	   3, open_memory_file,	      0);
  PL_register_foreign("open_memory_file",	   4, open_memory_file4,      0);
  PL_register_foreign("atom_to_memory_file",	   2, atom_to_memory_file,    0);
  PL_register_foreign("memory_file_to_atom",	   2, memory_file_to_atom2,   0);
  PL_register_foreign("memory_file_to_codes",	   2, memory_file_to_codes2,  0);
  PL_register_foreign("memory_file_to_string",	   2, memory_file_to_string2, 0);
  PL_register_foreign("memory_file_to_atom",	   3, memory_file_to_atom3,   0);
  PL_register_foreign("memory_file_to_codes",	   3, memory_file_to_codes3,  0);
  PL_register_foreign("memory_file_to_string",	   3, memory_file_to_string3, 0);
  PL_register_foreign("utf8_position_memory_file", 3, utf8_position,	      0);
  PL_register_foreign("insert_memory_file",	   3, insert_memory_file,     0);
  PL_register_foreign("delete_memory_file",	   3, delete_memory_file,     0);
  PL_register_foreign("memory_file_substring",     5, memory_file_substring,  0);
  PL_register_foreign("memory_file_line_position", 4, memory_file_line_position, 0);
}
