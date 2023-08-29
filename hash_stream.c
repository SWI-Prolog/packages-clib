/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2015, University of Amsterdam,
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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <errno.h>
#include "md5.h"
#include "sha1/sha1.h"
#include "sha1/sha2.h"

static atom_t ATOM_algorithm;
static atom_t ATOM_close_parent;
static atom_t ATOM_md5;
static atom_t ATOM_sha1;
static atom_t ATOM_sha224;
static atom_t ATOM_sha256;
static atom_t ATOM_sha384;
static atom_t ATOM_sha512;

typedef enum hash_algorithm
{ ALGORITHM_MD5,
  ALGORITHM_SHA1,
  ALGORITHM_SHA224,
  ALGORITHM_SHA256,
  ALGORITHM_SHA384,
  ALGORITHM_SHA512
} hash_algorithm;


		 /*******************************
		 *		MD5		*
		 *******************************/

static int
unify_digest(term_t t, unsigned char *digest, size_t len)
{ if ( len*2 <= 256 )
  { char hex_output[256];
    int di;
    char *pi;
    static char hexd[] = "0123456789abcdef";

    for(pi=hex_output, di = 0; di < len; ++di)
    { *pi++ = hexd[(digest[di] >> 4) & 0x0f];
      *pi++ = hexd[digest[di] & 0x0f];
    }

    return PL_unify_atom_nchars(t, len*2, hex_output);
  } else
  { return PL_representation_error("digest_length");
  }
}


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef struct hash_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *hash_stream;	/* Stream I'm handle of */
  IOENC		    parent_encoding;	/* original encoding */
  int		    close_parent;	/* close parent stream */
  hash_algorithm    algorithm;
  unsigned long	    digest_size;
  union
  { md5_state_t md5;
    sha1_ctx	sha1;
    sha2_ctx    sha2;
  } state;
} hash_context;


static hash_context*
alloc_hash_context(IOSTREAM *s)
{ hash_context *ctx = PL_malloc(sizeof(*ctx));

  memset(ctx, 0, sizeof(*ctx));
  ctx->stream = s;

  return ctx;
}


static void
free_hash_context(hash_context *ctx)
{ if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);

  PL_free(ctx);
}


		 /*******************************
		 *	RANGE LIMITED INPUT	*
		 *******************************/

static void
hash_append(hash_context *ctx, void *data, size_t size)
{ switch ( ctx->algorithm )
  { case ALGORITHM_MD5:
      md5_append(&ctx->state.md5, data, size);
      break;
    case ALGORITHM_SHA1:
      sha1_hash(data, (unsigned long)size, &ctx->state.sha1);
      break;
    default:
      sha2_hash(data, (unsigned long)size, &ctx->state.sha2);
      break;
  }
}


static ssize_t				/* range-limited read */
hash_read(void *handle, char *buf, size_t size)
{ hash_context *ctx = handle;
  ssize_t rd;

  if ( (rd = Sfread(buf, sizeof(char), size, ctx->stream)) >= 0 )
  { hash_append(ctx, buf, rd);

    return rd;
  }

  return rd;
}


static ssize_t
hash_write(void *handle, char *buf, size_t size)
{ hash_context *ctx = handle;
  size_t written = 0;

  hash_append(ctx, buf, size);

  while ( written < size )
  { ssize_t wr = Sfwrite(buf+written, sizeof(char), size, ctx->stream);

    if ( wr >= 0 )
    { written += wr;
    } else
      return wr;
  }

  return size;
}


static int
hash_control(void *handle, int op, void *data)
{ hash_context *ctx = handle;

  switch(op)
  { case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
      if ( ctx->stream->functions->control )
	return (*ctx->stream->functions->control)(ctx->stream->handle, op, data);
      return -1;
  }
}


static int
hash_close(void *handle)
{ int rc = 0;
  hash_context *ctx = handle;

  ctx->stream->encoding = ctx->parent_encoding;
  if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);

  if ( ctx->close_parent )
    rc = Sclose(ctx->stream);

  free_hash_context(ctx);

  return rc;
}


static IOFUNCTIONS hash_functions =
{ hash_read,
  hash_write,
  NULL,					/* seek */
  hash_close,
  hash_control,			/* control */
  NULL,					/* seek64 */
};

		 /*******************************
		 *	 PROLOG CONNECTION	*
		 *******************************/

static int
get_hash_algorithm(term_t t, hash_algorithm *ap)
{ atom_t a;

  if ( PL_get_atom_ex(t, &a) )
  { hash_algorithm alg;

    if ( a  == ATOM_md5 )
      alg = ALGORITHM_MD5;
    else if ( a  == ATOM_sha1 )
      alg = ALGORITHM_SHA1;
    else if ( a == ATOM_sha224 )
      alg = ALGORITHM_SHA224;
    else if ( a == ATOM_sha256 )
      alg = ALGORITHM_SHA256;
    else if ( a == ATOM_sha384 )
      alg = ALGORITHM_SHA384;
    else if ( a == ATOM_sha512 )
      alg = ALGORITHM_SHA512;
    else
      return PL_domain_error("algorithm", t);

    *ap = alg;
    return TRUE;
  }

  return FALSE;
}


static unsigned long
digest_size(hash_algorithm algorithm)
{ switch(algorithm)
  { case ALGORITHM_MD5:	   return 16;
    case ALGORITHM_SHA1:   return SHA1_DIGEST_SIZE;
    case ALGORITHM_SHA224: return SHA224_DIGEST_SIZE;
    case ALGORITHM_SHA256: return SHA256_DIGEST_SIZE;
    case ALGORITHM_SHA384: return SHA384_DIGEST_SIZE;
    case ALGORITHM_SHA512: return SHA512_DIGEST_SIZE;
    default:               assert(0); return(0);
  }
}



#define COPY_FLAGS (SIO_INPUT|SIO_OUTPUT| \
		    SIO_TEXT| \
		    SIO_REPXML|SIO_REPPL|\
		    SIO_RECORDPOS)

static foreign_t
pl_stream_hash_open(term_t org, term_t new, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  hash_context *ctx;
  IOSTREAM *s, *s2;
  hash_algorithm algorithm = ALGORITHM_SHA1;
  int close_parent = TRUE;

  while(PL_get_list_ex(tail, head, tail))
  { atom_t name;
    size_t arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return PL_type_error("option", head);
    _PL_get_arg(1, head, arg);

    if ( name == ATOM_algorithm )
    { if ( !get_hash_algorithm(arg, &algorithm) )
	return FALSE;
    } else if ( name == ATOM_close_parent )
    { if ( !PL_get_bool_ex(arg, &close_parent) )
	return FALSE;
    }
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */

  ctx = alloc_hash_context(s);
  ctx->close_parent = close_parent;
  ctx->algorithm    = algorithm;
  ctx->digest_size  = digest_size(algorithm);

  switch ( algorithm )
  { case ALGORITHM_MD5:
      md5_init(&ctx->state.md5);
      break;
    case ALGORITHM_SHA1:
      sha1_begin(&ctx->state.sha1);
      break;
    default:
      sha2_begin(ctx->digest_size, &ctx->state.sha2);
      break;
  }

  if ( !(s2 = Snew(ctx,
		   (s->flags&COPY_FLAGS)|SIO_FBUF,
		   &hash_functions))	)
  { PL_release_stream(s);
    free_hash_context(ctx);			/* no memory */

    return FALSE;
  }

  s2->encoding = s->encoding;
  ctx->parent_encoding = s->encoding;
  s->encoding = ENC_OCTET;
  ctx->hash_stream = s2;
  if ( PL_unify_stream(new, s2) )
  { Sset_filter(s, s2);
    PL_release_stream(s);

    return TRUE;
  } else
  { PL_release_stream(s);
    return FALSE;
  }
}


static foreign_t
pl_stream_hash(term_t stream, term_t hash)
{ IOSTREAM *s;

  if ( PL_get_stream_handle(stream, &s) )
  { unsigned char hval[SHA2_MAX_DIGEST_SIZE];
    hash_context *ctx = s->handle;
    int rc;

    if ( Sferror(s) || ((s->flags & SIO_OUTPUT) && Sflush(s) < 0) )
      return PL_release_stream(s);

    if ( s->functions != &hash_functions )
    { PL_release_stream(s);
      return PL_domain_error("hash_stream", stream);
    }

    switch(ctx->algorithm)
    { case ALGORITHM_MD5:
	md5_finish(&ctx->state.md5, (md5_byte_t*)hval);
	break;
      case ALGORITHM_SHA1:
	sha1_end(hval, &ctx->state.sha1);
	break;
      default:
	sha2_end(hval, &ctx->state.sha2);
        break;
    }
    rc = unify_digest(hash, hval, ctx->digest_size);

    PL_release_stream(s);
    return rc;
  }

  return FALSE;
}


		 /*******************************
		 *	       INSTALL		*
		 *******************************/

#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install_hashstream(void)
{ MKATOM(md5);
  MKATOM(sha1);				/* =160 */
  MKATOM(sha224);
  MKATOM(sha256);
  MKATOM(sha384);
  MKATOM(sha512);
  MKATOM(algorithm);
  MKATOM(close_parent);

  PL_register_foreign("open_hash_stream", 3, pl_stream_hash_open, 0);
  PL_register_foreign("stream_hash",	  2, pl_stream_hash,	  0);
}
