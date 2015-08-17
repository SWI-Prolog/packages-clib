/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, University of Amsterdam
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
#include <string.h>

static atom_t		ATOM_read;
static atom_t		ATOM_write;
static functor_t	FUNCTOR_stream_write2;
static functor_t	FUNCTOR_stream_read2;
static functor_t	FUNCTOR_stream_close1;

typedef struct stream_context
{ IOSTREAM	   *stream;		/* Stream I'm handle of */
  module_t	    module;		/* Associated module */
  predicate_t	    pred_write;		/* write handler */
  predicate_t	    pred_read;		/* read handler */
  char		   *buffered;		/* buffered read chars */
  size_t	    buflen;		/* length of buffered */
  size_t	    sent;		/* sent buffered chars */
} stream_context;

static stream_context*
alloc_stream_context(void)
{ stream_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));

  return ctx;
}

static void
free_stream_context(stream_context *ctx)
{ if ( ctx->buffered )
    PL_free(ctx->buffered);

  PL_free(ctx);
}

static ssize_t
stream_read(void *handle, char *buf, size_t size)
{ stream_context *ctx = handle;
  ssize_t rc = -1;

  if ( !ctx->pred_read )
    ctx->pred_read = PL_pred(FUNCTOR_stream_read2, ctx->module);

  if ( !ctx->buffered )
  { fid_t fid = 0;
    term_t av;
    wchar_t *ws;
    size_t len;

    if ( (fid = PL_open_foreign_frame()) &&
	 (av = PL_new_term_refs(2)) &&
	 PL_unify_stream(av+0, ctx->stream) &&
	 PL_call_predicate(ctx->module, PL_Q_NORMAL, ctx->pred_read, av) &&
	 PL_get_wchars(av+1, &len, &ws,
		       CVT_ALL|CVT_WRITEQ|CVT_EXCEPTION|BUF_MALLOC) )
    { if ( len == 0 )
	rc = 0;			/* EOF */
      ctx->buffered = (char*)ws;
      ctx->buflen = len*sizeof(wchar_t);
    } else
    { term_t ex;

      if ( (ex = PL_exception(0)) )
      { Sset_exception(ctx->stream, ex);
      } else
      { Sseterr(ctx->stream, SIO_FERR, "Prolog read handler failed");
      }
    }

    if ( fid )
      PL_close_foreign_frame(fid);
  }

  if ( ctx->buffered )
  { size_t left = ctx->buflen - ctx->sent;

    if ( left < size )
    { memcpy(buf, &ctx->buffered[ctx->sent], left);
      PL_free(ctx->buffered);
      ctx->buffered = NULL;
      rc = left;
    } else
    { memcpy(buf, &ctx->buffered[ctx->sent], size);
      ctx->sent += size;
      rc = size;
    }
  }

  return rc;
}


static ssize_t
stream_write(void *handle, char *buf, size_t size)
{ stream_context *ctx = handle;
  fid_t fid = 0;
  term_t av;
  int rc;

  if ( !ctx->pred_write )
    ctx->pred_write = PL_pred(FUNCTOR_stream_write2, ctx->module);

  if ( (fid = PL_open_foreign_frame()) &&
       (av = PL_new_term_refs(2)) &&
       PL_unify_stream(av+0, ctx->stream) &&
       PL_unify_wchars(av+1, PL_STRING, size/sizeof(wchar_t), (wchar_t*)buf) &&
       PL_call_predicate(ctx->module, PL_Q_NORMAL, ctx->pred_write, av) )
  { rc = size;
  } else
  { term_t ex;

    if ( (ex = PL_exception(0)) )
    { Sset_exception(ctx->stream, ex);
    } else
    { Sseterr(ctx->stream, SIO_FERR, "Prolog write handler failed");
    }

    rc = -1;
  }

  if ( fid )
    PL_close_foreign_frame(fid);

  return rc;
}

static int64_t
stream_seek64(void *handle, int64_t pos, int whence)
{ stream_context *ctx = handle;

  (void)ctx;

  return -1;
}

static int
stream_control(void *handle, int op, void *data)
{ stream_context *ctx = handle;

  (void)ctx;
  switch(op)
  { case SIO_FLUSHOUTPUT:
      return 0;
  }

  return -1;
}

static int
stream_close(void *handle)
{ stream_context *ctx = handle;
  fid_t fid = 0;
  term_t av;
  int rc;
  predicate_t pred_close = PL_pred(FUNCTOR_stream_close1, ctx->module);

  if ( (fid = PL_open_foreign_frame()) &&
       (av = PL_new_term_refs(1)) &&
       PL_unify_stream(av+0, ctx->stream) &&
       PL_call_predicate(ctx->module, PL_Q_NORMAL, pred_close, av) )
  { rc = 0;
  } else
  { term_t ex;

    if ( (ex = PL_exception(0)) )
    { Sset_exception(ctx->stream, ex);
    } else
    { Sseterr(ctx->stream, SIO_FERR, "Prolog write handler failed");
    }

    rc = -1;
  }

  if ( fid )
    PL_close_foreign_frame(fid);

  free_stream_context(ctx);
  return rc;
}

static IOFUNCTIONS stream_functions =
{ stream_read,
  stream_write,
  NULL,
  stream_close,
  stream_control,
  stream_seek64
};

static foreign_t
open_prolog_stream(term_t module, term_t mode, term_t stream, term_t options)
{ IOSTREAM *s;
  stream_context *ctx;
  int flags = SIO_TEXT|SIO_RECORDPOS|SIO_FBUF;
  atom_t a;
  module_t m;

  if ( !PL_get_atom_ex(mode, &a) )
    return FALSE;
  if ( a == ATOM_read )
    flags |= SIO_INPUT;
  else if ( a == ATOM_write )
    flags |= SIO_OUTPUT;
  else
    return PL_domain_error("io_mode", mode);

  if ( !PL_get_atom_ex(module, &a) )
    return FALSE;
  m = PL_new_module(a);

  ctx = alloc_stream_context();
  s = Snew(ctx, flags, &stream_functions);
  ctx->stream = s;
  ctx->module = m;
  s->encoding = ENC_WCHAR;
  s->newline  = SIO_NL_POSIX;

  if ( PL_unify_stream(stream, s) )
  { return TRUE;
  } else
  { Sclose(s);
    return PL_uninstantiation_error(stream);
  }
}


install_t
install_prolog_stream(void)
{ ATOM_read  = PL_new_atom("read");
  ATOM_write = PL_new_atom("write");
  FUNCTOR_stream_write2 = PL_new_functor(PL_new_atom("stream_write"), 2);
  FUNCTOR_stream_read2  = PL_new_functor(PL_new_atom("stream_read"),  2);
  FUNCTOR_stream_close1 = PL_new_functor(PL_new_atom("stream_close"), 1);

  PL_register_foreign("open_prolog_stream", 4, open_prolog_stream, 0);
}
