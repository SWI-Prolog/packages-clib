/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2021, VU University Amsterdam
			      CWI, Amsterdam
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
#ifdef HAVE_OSSP_UUID_H
#include <ossp/uuid.h>
#else
#include <uuid.h>
#endif

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <assert.h>
#if defined(HAVE_OSSP_UUID_H)
#include <ossp/uuid.h>
#else
#include <uuid.h>
#endif

/* Seems to be defined in some MinGW installations.  The
 * ossp-uuid header defines the types using typedef, so
 * we can safely kill these macros
 */
#undef UUID
#undef uuid_t

static atom_t ATOM_version;
static atom_t ATOM_format;
static atom_t ATOM_atom;
static atom_t ATOM_integer;
static atom_t ATOM_url;
static atom_t ATOM_dns;
static atom_t ATOM_oid;
static atom_t ATOM_x500;

static foreign_t
pl_uuid(term_t UUID, term_t options)
{ unsigned int mode = UUID_MAKE_V1;
  atom_t format = ATOM_atom;
  uuid_t *uuid;
  char *ns = NULL;
  char *str = NULL;
  int rc;
  uuid_rc_t urc;

  if ( !PL_get_nil(options) )
  { term_t tail = PL_copy_term_ref(options);
    term_t head = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();

    while( PL_get_list(tail, head, tail) )
    { atom_t name;
      size_t arity;

      if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
	return PL_type_error("option", head);
      _PL_get_arg(1, head, arg);

      if ( name == ATOM_version )
      { int v;

	if ( !PL_get_integer_ex(arg, &v) )
	  return FALSE;
	switch(v)
	{ case 1: mode = UUID_MAKE_V1; break;
	  case 2: mode = UUID_MAKE_MC; break;
	  case 3: mode = UUID_MAKE_V3; break;
	  case 4: mode = UUID_MAKE_V4; break;
	  case 5: mode = UUID_MAKE_V5; break;
          default: return PL_domain_error("uuid_version", arg);
	}
      } else if ( name == ATOM_format )
      { if ( !PL_get_atom_ex(arg, &format) )
	  return FALSE;
	if ( format != ATOM_atom && format != ATOM_integer )
	  return PL_domain_error("uuid_format", arg);
      } else
      { char *newns = NULL;

	if ( name == ATOM_dns )
	{ newns = "ns:DNS";
	} else if ( name == ATOM_url )
	{ newns = "ns:URL";
	} else if ( name == ATOM_oid )
	{ newns = "ns:OID";
	} else if ( name == ATOM_x500 )
	{ newns = "ns:X500";
	}

	if ( newns )
	{ ns = newns;
	  if ( !PL_get_chars(arg, &str, CVT_ATOM|CVT_EXCEPTION) )
	    return FALSE;
	  if ( mode == UUID_MAKE_V1 )
	    mode = UUID_MAKE_V3;
	}
      }
    }
    if ( !PL_get_nil_ex(tail) )
      return FALSE;
  }

  switch(mode)
  { case UUID_MAKE_V1:
    case UUID_MAKE_MC:
    case UUID_MAKE_V4:
      uuid_create(&uuid);
      if ( (urc=uuid_make(uuid, mode)) != UUID_RC_OK )
	return PL_warning("UUID: make: %s\n", uuid_error(urc));
      break;
    case UUID_MAKE_V3:
    case UUID_MAKE_V5:
    { uuid_t *uuid_ns;

      if ( !ns )
	return PL_existence_error("uuid_context", options);

      uuid_create(&uuid);
      uuid_create(&uuid_ns);
      uuid_load(uuid_ns, ns);
      if ( (urc=uuid_make(uuid, mode, uuid_ns, str)) != UUID_RC_OK )
	return PL_warning("UUID: make: %s\n", uuid_error(urc));
      uuid_destroy(uuid_ns);
      break;
    }
    default:
      assert(0);
      return FALSE;
  }

  if ( format == ATOM_atom )
  { char buf[UUID_LEN_STR+1];
    void *ptr = buf;
    size_t datalen = sizeof(buf);

    if ( (urc=uuid_export(uuid, UUID_FMT_STR, &ptr, &datalen)) != UUID_RC_OK )
      return PL_warning("UUID: export: %s\n", uuid_error(urc));
    rc = PL_unify_chars(UUID, PL_ATOM|REP_ISO_LATIN_1, (size_t)-1, buf);
  } else if ( format == ATOM_integer )
  { char buf[UUID_LEN_SIV+1];
    void *ptr = buf;
    size_t datalen = sizeof(buf);
    term_t tmp = PL_new_term_ref();

    if ( (urc=uuid_export(uuid, UUID_FMT_SIV, &ptr, &datalen)) != UUID_RC_OK )
      return PL_warning("UUID: export: %s\n", uuid_error(urc));
    rc = ( PL_chars_to_term(buf, tmp) &&
	   PL_unify(UUID, tmp)
	 );
  } else
  { assert(0);
    return FALSE;
  }

  uuid_destroy(uuid);

  return rc;
}


install_t
install_uuid(void)
{ ATOM_version = PL_new_atom("version");
  ATOM_format  = PL_new_atom("format");
  ATOM_atom    = PL_new_atom("atom");
  ATOM_integer = PL_new_atom("integer");
  ATOM_dns     = PL_new_atom("dns");
  ATOM_url     = PL_new_atom("url");
  ATOM_oid     = PL_new_atom("oid");
  ATOM_x500    = PL_new_atom("x500");

  PL_register_foreign("ossp_uuid", 2, pl_uuid, 0);
}

