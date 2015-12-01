/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Prolog.h>
#include "md5.h"
#include <string.h>

static atom_t ATOM_encoding;
static atom_t ATOM_utf8;
static atom_t ATOM_octet;

typedef struct
{ unsigned int  encoding;
} optval;


static int
md5_options(term_t options, optval *result)
{ term_t opts = PL_copy_term_ref(options);
  term_t opt = PL_new_term_ref();

					/* defaults */
  memset(result, 0, sizeof(*result));
  result->encoding = REP_UTF8;

  while(PL_get_list(opts, opt, opts))
  { atom_t aname;
    size_t arity;

    if ( PL_get_name_arity(opt, &aname, &arity) && arity == 1 )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, opt, a);

      if ( aname == ATOM_encoding )
      { atom_t a_enc;

	if ( !PL_get_atom_ex(a, &a_enc) )
	  return FALSE;
	if ( a_enc == ATOM_utf8 )
	  result->encoding = REP_UTF8;
	else if ( a_enc == ATOM_octet )
	  result->encoding = REP_ISO_LATIN_1;
	else
	  return PL_domain_error("encoding", a);
      }
    } else
    { return PL_type_error("option", opt);
    }
  }

  if ( !PL_get_nil(opts) )
    return PL_type_error("list", opts);

  return TRUE;
}


static int
md5_unify_digest(term_t t, md5_byte_t digest[16])
{ char hex_output[16*2];
  int di;
  char *pi;
  static char hexd[] = "0123456789abcdef";

  for(pi=hex_output, di = 0; di < 16; ++di)
  { *pi++ = hexd[(digest[di] >> 4) & 0x0f];
    *pi++ = hexd[digest[di] & 0x0f];
  }

  return PL_unify_atom_nchars(t, 16*2, hex_output);
}


static foreign_t
md5_hash(term_t from, term_t md5, term_t options)
{ char *data;
  size_t datalen;
  optval opts;
  md5_byte_t digest[16];
  md5_state_t state;

  if ( !md5_options(options, &opts) )
    return FALSE;

  if ( !PL_get_nchars(from, &datalen, &data,
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|opts.encoding) )
    return FALSE;

  md5_init(&state);
  md5_append(&state, (const md5_byte_t *)data, datalen);
  md5_finish(&state, digest);

  return md5_unify_digest(md5, digest);
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install_md54pl(void)
{ MKATOM(utf8);
  MKATOM(octet);
  MKATOM(encoding);

  PL_register_foreign("md5_hash", 3, md5_hash, 0);
}
