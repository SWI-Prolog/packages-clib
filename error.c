/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2022, University of Amsterdam
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

#include <errno.h>
#include <stdlib.h>
#include "clib.h"
#include <assert.h>
#include <string.h>

int
pl_error(const char *pred, int arity, const char *msg, int id, ...)
{ fid_t fid;
  term_t except, formal, swi;
  int rc;
  int msg_locale = FALSE;
  va_list args;

  if ( !(fid=PL_open_foreign_frame()) )
    return FALSE;

  except = PL_new_term_ref();
  formal = PL_new_term_ref();
  swi    = PL_new_term_ref();

  va_start(args, id);
  switch(id)
  { case ERR_ERRNO:
    { int err = va_arg(args, int);
      const char *action = va_arg(args, const char *);
      const char *type   = va_arg(args, const char *);
      term_t	  object = va_arg(args, term_t);

      if ( !object )
	object = PL_new_term_ref();

      msg = strerror(err);
      msg_locale = TRUE;

      switch(err)
      { case ENOMEM:
	case EAGAIN:			/* fork(); might be other resource */
	  rc = PL_unify_term(formal,
			     CompoundArg("resource_error", 1),
			       AtomArg("no_memory"));
	  break;
	case EACCES:
	case EPERM:
	{ rc = PL_unify_term(formal,
			     CompoundArg("permission_error", 3),
			       AtomArg(action),
			       AtomArg(type),
			     PL_TERM, object);
	  break;
	}
	case ENOENT:
	case ESRCH:
	{ rc = PL_unify_term(formal,
			     CompoundArg("existence_error", 2),
			     AtomArg(type),
			     PL_TERM, object);
	  break;
	}
	default:
	  rc = PL_unify_atom_chars(formal, "system_error");
	  break;
      }
      break;
    }
    case ERR_ARGTYPE:
    { int argn        = va_arg(args, int);	/* argument position (unused) */
      term_t actual   = va_arg(args, term_t);
      atom_t expected = PL_new_atom(va_arg(args, const char*));

      (void)argn;				/* avoid unused warning */

      if ( PL_is_variable(actual) && expected != PL_new_atom("variable") )
	rc = PL_unify_atom_chars(formal, "instantiation_error");
      else
	rc = PL_unify_term(formal,
			   CompoundArg("type_error", 2),
			   PL_ATOM, expected,
			   PL_TERM, actual);
      break;
    }
    case ERR_TYPE:
    { term_t actual   = va_arg(args, term_t);
      atom_t expected = PL_new_atom(va_arg(args, const char*));

      if ( PL_is_variable(actual) && expected != PL_new_atom("variable") )
	rc = PL_unify_atom_chars(formal, "instantiation_error");
      else
	rc = PL_unify_term(formal,
			   CompoundArg("type_error", 2),
			   PL_ATOM, expected,
			   PL_TERM, actual);
      break;
    }
    case ERR_DOMAIN:
    { term_t actual   = va_arg(args, term_t);
      atom_t expected = PL_new_atom(va_arg(args, const char*));

      rc = PL_unify_term(formal,
			 CompoundArg("domain_error", 2),
			 PL_ATOM, expected,
			 PL_TERM, actual);
      break;
    }
    case ERR_EXISTENCE:
    { const char *type = va_arg(args, const char *);
      term_t obj  = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 CompoundArg("existence_error", 2),
			 PL_CHARS, type,
			 PL_TERM, obj);

      break;
    }
    case ERR_PERMISSION:
    { term_t obj  = va_arg(args, term_t);
      const char *op = va_arg(args, const char *);
      const char *objtype = va_arg(args, const char *);

      rc = PL_unify_term(formal,
			 CompoundArg("permission_error", 3),
			 AtomArg(op),
			 AtomArg(objtype),
			 PL_TERM, obj);
      break;
    }
    case ERR_NOTIMPLEMENTED:
    { const char *op = va_arg(args, const char *);
      term_t obj  = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 CompoundArg("not_implemented", 2),
			 AtomArg(op),
			 PL_TERM, obj);
      break;
    }
    case ERR_RESOURCE:
    { const char *res = va_arg(args, const char *);

      rc = PL_unify_term(formal,
			 CompoundArg("resource_error", 1),
			 AtomArg(res));
      break;
    }
    case ERR_SYNTAX:
    { const char *culprit = va_arg(args, const char *);

      rc = PL_unify_term(formal,
			 CompoundArg("syntax_error", 1),
			 AtomArg(culprit));
      break;
    }
    default:
      assert(0);
      rc = FALSE;
  }
  va_end(args);

  if ( rc && (pred || msg) )
  { term_t predterm = PL_new_term_ref();
    term_t msgterm  = PL_new_term_ref();

    if ( pred )
    { rc = PL_unify_term(predterm,
		    CompoundArg("/", 2),
		      AtomArg(pred),
		      IntArg(arity));
    }
    if ( msg )
    { if ( msg_locale )
	rc = PL_unify_term(msgterm, PL_MBCHARS, msg);
      else
	rc = PL_put_atom_chars(msgterm, msg);
    }

    if ( rc )
      rc = PL_unify_term(swi,
			 CompoundArg("context", 2),
			 PL_TERM, predterm,
			 PL_TERM, msgterm);
  }

  if ( rc )
    rc = PL_unify_term(except,
		       CompoundArg("error", 2),
		       PL_TERM, formal,
		       PL_TERM, swi);

  if ( rc )
    rc = PL_raise_exception(except);

  PL_close_foreign_frame(fid);

  return rc;
}

