/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2014, University of Amsterdam, VU Amsterdam
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

#define _CRT_SECURE_NO_WARNINGS 1
#ifdef DEFINE_XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_CRYPT_H
#include <crypt.h>
#else
extern char *crypt(const char *key, const char *setting);
#endif
#include <stdlib.h>
#include <string.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "clib.h"

/* md5passwd.c */
extern char *md5_crypt(const char *pw, const char *salt);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple interface to the Unix   password  encryption routine. Implemented
for providing authorization in  the   multi-threaded  Prolog  based HTTP
deamon and therefore providing a thread-safe interface.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef _REENTRANT
#include <pthread.h>

static pthread_mutex_t crypt_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&crypt_mutex)
#define UNLOCK() pthread_mutex_unlock(&crypt_mutex)
#else
#define LOCK()
#define UNLOCK()
#endif


static foreign_t
pl_crypt(term_t passwd, term_t encrypted)
{ char *pw, *e;
  char salt[64];

  if ( !PL_get_chars(passwd, &pw, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return pl_error("crypt", 2, NULL, ERR_ARGTYPE,
		    1, passwd, "text");

  if ( PL_get_chars(encrypted, &e, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
  { char *s2;

    if ( strncmp(e, "$1$", 3) == 0 )	/* MD5 Hash */
    { char *p = strchr(e+3, '$');
      size_t slen;

      if ( p && (slen=(size_t)(p-e-3)) < sizeof(salt) )
      { strncpy(salt, e+3, slen);
	salt[slen] = 0;
	s2 = md5_crypt(pw, salt);
	return (strcmp(s2, e) == 0) ? TRUE : FALSE;
      } else
      { Sdprintf("No salt???\n");
	return FALSE;
      }
    } else
    { int rval;

      salt[0] = e[0];
      salt[1] = e[1];
      salt[2] = '\0';

      LOCK();
      rval = ( (s2 = crypt(pw, salt)) &&
	       strcmp(s2, e) == 0
	     );
      UNLOCK();

      return rval;
    }
  } else
  { term_t tail = PL_copy_term_ref(encrypted);
    term_t head = PL_new_term_ref();
    int slen = 2;
    int n;
    int flags = PL_CODE_LIST;
    char *s2;
    int rval;

    for(n=0; n<sizeof(salt)-1; n++)
    { if ( PL_get_list(tail, head, tail) )
      { int i;
	char *t;

	if ( PL_get_integer(head, &i) && i>=0 && i<=255 )
	{ salt[n] = (char) i; /* safe cast */
	} else if ( PL_get_atom_chars(head, &t) && t[1] == '\0' )
	{ salt[n] = t[0];
	  flags = PL_CHAR_LIST;
	} else
	{ return pl_error("crypt", 2, NULL, ERR_ARGTYPE,
			  2, head, "character");
	}
      } else
	break;
    }

    if ( n >= 3 && strncmp(salt, "$1$", 3) == 0 )
      slen = 3+8;

    for( ; n < slen; n++ )
    { int c = 'a'+(int)(26.0*rand()/(RAND_MAX+1.0));

      if ( rand() & 0x1 )
	c += 'A' - 'a';

      salt[n] = (char) c; /* safe cast */
    }
    salt[n] = 0;
    LOCK();
    if ( slen > 2 && strncmp(salt, "$1$", 3) == 0 )
    { s2 = md5_crypt(pw, salt);
    } else
    { s2 = crypt(pw, salt);
    }
    if ( s2 )
      rval = PL_unify_chars(encrypted, flags|REP_ISO_LATIN_1, (size_t)-1, s2);
    else
      rval = PL_domain_error("salt", encrypted);
    UNLOCK();

    return rval;
  }
}


install_t
install_crypt(void)
{ PL_register_foreign("crypt", 2, pl_crypt, 0);
}
