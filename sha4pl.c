/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2015, University of Amsterdam
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define _ISOC99_SOURCE
#define USE_SHA256 1

#include <SWI-Prolog.h>
#include "error.h"
#include "sha1/sha1.h"
#include "sha1/sha2.h"
#include "sha1/hmac.h"
#include <assert.h>

static atom_t ATOM_sha1;
static atom_t ATOM_sha224;
static atom_t ATOM_sha256;
static atom_t ATOM_sha384;
static atom_t ATOM_sha512;
static atom_t ATOM_algorithm;
static atom_t ATOM_utf8;
static atom_t ATOM_octet;
static atom_t ATOM_encoding;

typedef enum
{ ALGORITHM_SHA1,
  ALGORITHM_SHA224,
  ALGORITHM_SHA256,
  ALGORITHM_SHA384,
  ALGORITHM_SHA512
} sha_algorithm;


typedef struct
{ sha_algorithm algorithm;
  size_t	digest_size;
  term_t	algorithm_term;
  unsigned int  encoding;
} optval;

#define CONTEXT_MAGIC (~ 0x53484163L)

struct context
{ int		magic;
  optval	opts;
  union {
    sha1_ctx	sha1;
    sha2_ctx    sha2;
  } context;
};

static int
sha_options(term_t options, optval *result)
{ term_t opts = PL_copy_term_ref(options);
  term_t opt = PL_new_term_ref();

					/* defaults */
  memset(result, 0, sizeof(*result));
  result->algorithm   = ALGORITHM_SHA1;
  result->digest_size = SHA1_DIGEST_SIZE;
  result->encoding    = REP_UTF8;

  while(PL_get_list(opts, opt, opts))
  { atom_t aname;
    size_t arity;

    if ( PL_get_name_arity(opt, &aname, &arity) && arity == 1 )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, opt, a);

      if ( aname == ATOM_algorithm )
      { atom_t a_algorithm;

	result->algorithm_term = a;
	if ( !PL_get_atom_ex(a, &a_algorithm) )
	  return FALSE;
	if ( a_algorithm == ATOM_sha1 )
	{ result->algorithm   = ALGORITHM_SHA1;
	  result->digest_size = SHA1_DIGEST_SIZE;
	} else if ( a_algorithm == ATOM_sha224 )
	{ result->algorithm = ALGORITHM_SHA224;
	  result->digest_size = SHA224_DIGEST_SIZE;
	} else if ( a_algorithm == ATOM_sha256 )
	{ result->algorithm = ALGORITHM_SHA256;
	  result->digest_size = SHA256_DIGEST_SIZE;
	} else if ( a_algorithm == ATOM_sha384 )
	{ result->algorithm = ALGORITHM_SHA384;
	  result->digest_size = SHA384_DIGEST_SIZE;
	} else if ( a_algorithm == ATOM_sha512 )
	{ result->algorithm = ALGORITHM_SHA512;
	  result->digest_size = SHA512_DIGEST_SIZE;
	} else
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a, "algorithm");
      } else if ( aname == ATOM_encoding )
      { atom_t a_enc;

	if ( !PL_get_atom_ex(a, &a_enc) )
	  return FALSE;
	if ( a_enc == ATOM_utf8 )
	  result->encoding = REP_UTF8;
	else if ( a_enc == ATOM_octet )
	  result->encoding = REP_ISO_LATIN_1;
	else
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a, "encoding");
      }
    } else
    { return pl_error(NULL, 0, NULL, ERR_TYPE, opt, "option");
    }
  }

  if ( !PL_get_nil(opts) )
    return pl_error("sha_hash", 1, NULL, ERR_TYPE, opts, "list");

  return TRUE;
}




static foreign_t
pl_sha_hash(term_t from, term_t hash, term_t options)
{ char *data;
  size_t datalen;
  optval opts;
  unsigned char hval[SHA2_MAX_DIGEST_SIZE];

  if ( !sha_options(options, &opts) )
    return FALSE;

  if ( !PL_get_nchars(from, &datalen, &data,
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|opts.encoding) )
    return FALSE;

  if ( opts.algorithm == ALGORITHM_SHA1 )
  { sha1((unsigned char*)hval,
	 (unsigned char*)data, (unsigned long)datalen);
  } else
  { sha2((unsigned char*)hval, (unsigned long) opts.digest_size,
	 (unsigned char*)data, (unsigned long)datalen);
  }

  return PL_unify_list_ncodes(hash, opts.digest_size, (char*)hval);
}


static foreign_t
pl_sha_new_ctx(term_t ctx, term_t options)
{ struct context c;
  optval *op = &(c.opts);

  if ( !sha_options(options, op) )
    return FALSE;

  c.magic = CONTEXT_MAGIC;

  if ( op->algorithm == ALGORITHM_SHA1 )
  { sha1_begin(&(c.context.sha1));
  } else
  { sha2_begin((unsigned long) op->digest_size, &(c.context.sha2));
  }

  /* NB: the context size depends on the digest size */
  /* (e. g., sha512_ctx is twice as long as sha256_ctx) */
  /* so there're extra data.  It will do no harm, though. */
  /* . */
  return PL_unify_string_nchars(ctx, sizeof(c), (char*)&c);
}


static foreign_t
pl_sha_hash_ctx(term_t old_ctx, term_t from, term_t new_ctx, term_t hash)
{ char *data;
  size_t datalen;
  struct context c;
  size_t clen;
  unsigned char hval[SHA2_MAX_DIGEST_SIZE];

  { struct context *cp;

    if ( !PL_get_string_chars(old_ctx, (char **)&cp, &clen) )
      return FALSE;

    if ( clen != sizeof (*cp) )
    { bad_context:
	return pl_error(NULL, 0, "Invalid OldContext passed",
			ERR_DOMAIN, old_ctx, "algorithm");
    }

    memcpy(&c, cp, sizeof(c));
  }

  if ( c.magic != CONTEXT_MAGIC )
    goto bad_context;

  if ( !PL_get_nchars(
	    from, &datalen, &data,
	    CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|c.opts.encoding) )
    return FALSE;

  if ( c.opts.algorithm == ALGORITHM_SHA1 )
  { sha1_ctx *c1p = &c.context.sha1;
    sha1_hash((unsigned char*)data, (unsigned long)datalen, c1p);
    if ( !PL_unify_string_nchars(new_ctx, sizeof(c), (char*)&c) )
      return FALSE;
    sha1_end((unsigned char *)hval, c1p);
  } else
  { sha2_ctx *c1p = &c.context.sha2;
    sha2_hash((unsigned char*)data, (unsigned long)datalen, c1p);
    if ( !PL_unify_string_nchars(new_ctx, sizeof(c), (char*)&c) )
      return FALSE;
    sha2_end((unsigned char *)hval, c1p);
  }

  /* . */
  return PL_unify_list_ncodes(hash, c.opts.digest_size, (char*)hval);
}


static foreign_t
pl_hmac_sha(term_t key, term_t data, term_t mac, term_t options)
{ char *sdata, *skey;
  size_t datalen, keylen;
  optval opts;
  unsigned char digest[SHA2_MAX_DIGEST_SIZE];

  if ( !PL_get_nchars(key, &keylen, &skey,
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    return FALSE;
  if ( !PL_get_nchars(data, &datalen, &sdata,
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    return FALSE;

  if ( !sha_options(options, &opts) )
    return FALSE;

  switch(opts.algorithm)
  { case ALGORITHM_SHA1:
      hmac_sha1((unsigned char*)skey, (unsigned long)keylen,
		(unsigned char*)sdata, (unsigned long)datalen,
		digest, (unsigned long)opts.digest_size);
      break;
    case ALGORITHM_SHA256:
      hmac_sha256((unsigned char*)skey, (unsigned long)keylen,
		  (unsigned char*)sdata, (unsigned long)datalen,
		  digest, (unsigned long)opts.digest_size);
      break;
    default:
      return pl_error(NULL, 0, "HMAC-SHA only for SHA-1 and SHA-256",
		      ERR_DOMAIN, opts.algorithm_term, "algorithm");
  }

  return PL_unify_list_ncodes(mac, opts.digest_size, (char*)digest);
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install_sha4pl()
{ MKATOM(sha1);				/* =160 */
  MKATOM(sha224);
  MKATOM(sha256);
  MKATOM(sha384);
  MKATOM(sha512);
  MKATOM(algorithm);
  MKATOM(utf8);
  MKATOM(octet);
  MKATOM(encoding);

  PL_register_foreign("sha_hash", 3, pl_sha_hash, 0);
  PL_register_foreign("sha_new_ctx", 2, pl_sha_new_ctx, 0);
  PL_register_foreign("sha_hash_ctx", 4, pl_sha_hash_ctx, 0);
  PL_register_foreign("hmac_sha", 4, pl_hmac_sha, 0);
}
