/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2011, University of Amsterdam
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

#ifndef UTIL_H_INCLUDED
#define UTIL_H_INCLUDED

#define HAVE_STRERROR 1
#define HAVE_TZNAME

#if defined(sun) && !defined(__svr4__)	/* Old SunOS 4.x */
#undef HAVE_STRERROR
#undef HAVE_TZNAME
#endif

#include <stdarg.h>
#include <time.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

typedef struct
{ char *name;
  char *value;
} var, *Var;

extern int	use_nph;		/* 1: nph-cgi-script */

/* util.c */
#ifndef HAVE_STRERROR
const char *	strerror(int err);
#endif
char *		save_string(const char *s);
int             cat(const char *file);
int             catfd(const char *file, FILE *out);
void            error(const char *msg, ...);
void		printenv(void);
int		copyPage(FILE *in, FILE *out, Var vars);
int		echoPage(const char *page, ...);
int		echoPagev(const char *page, va_list args);
int		mailFileFromVars(const char *email,
				 const char *from,
				 const char *file,
				 Var vars);
int		mailFile(const char *email,
			 const char *from,
			 const char *file,
			 ...);
long		fileSize(const char *file);
char *		rfc_date(time_t t);
char *		rfc_modified(const char *file);

#endif /*UTIL_H_INCLUDED*/
