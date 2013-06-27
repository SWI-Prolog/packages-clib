/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

#include <SWI-Prolog.h>
#include <syslog.h>
#include <string.h>

#define streq(s,q) (strcmp(s,q) == 0)

static int
get_option(term_t t, int *opt)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  char *s;
  int option = 0;

  while( PL_get_list_ex(tail, head, tail) )
  { if ( PL_get_chars(head, &s, CVT_ATOM|CVT_EXCEPTION) )
    { if      ( streq(s, "cons"  ) ) option |= LOG_CONS;
      else if ( streq(s, "ndelay") ) option |= LOG_NDELAY;
      else if ( streq(s, "nowait") ) option |= LOG_NOWAIT;
      else if ( streq(s, "odelay") ) option |= LOG_ODELAY;
#ifdef LOG_PERROR
      else if ( streq(s, "perror") ) option |= LOG_PERROR;
#endif
      else if ( streq(s, "pid") )    option |= LOG_PID;
      else return PL_domain_error("syslog_option", head);
    } else
      return FALSE;
  }

  if ( PL_get_nil_ex(tail) )
  { *opt = option;
    return TRUE;
  }

  return FALSE;
}

static int
get_facility(term_t t, int *fac)
{ char *s;
  int facility;

  if ( PL_get_chars(t, &s, CVT_ATOM|CVT_EXCEPTION) )
  { if      ( streq(s, "auth"  ) )   facility = LOG_AUTH;
#ifdef LOG_AUTHPRIV
    else if ( streq(s, "authpriv") ) facility = LOG_AUTHPRIV;
#endif
    else if ( streq(s, "cron") )     facility = LOG_CRON;
    else if ( streq(s, "daemon") )   facility = LOG_DAEMON;
#ifdef LOG_FTP
    else if ( streq(s, "ftp") )      facility = LOG_FTP;
#endif
    else if ( streq(s, "kern") )     facility = LOG_KERN;
    else if ( streq(s, "local0") )   facility = LOG_LOCAL0;
    else if ( streq(s, "local1") )   facility = LOG_LOCAL1;
    else if ( streq(s, "local2") )   facility = LOG_LOCAL2;
    else if ( streq(s, "local3") )   facility = LOG_LOCAL3;
    else if ( streq(s, "local4") )   facility = LOG_LOCAL4;
    else if ( streq(s, "local5") )   facility = LOG_LOCAL5;
    else if ( streq(s, "local6") )   facility = LOG_LOCAL6;
    else if ( streq(s, "local7") )   facility = LOG_LOCAL7;
    else if ( streq(s, "lpr") )      facility = LOG_LPR;
    else if ( streq(s, "mail") )     facility = LOG_MAIL;
    else if ( streq(s, "news") )     facility = LOG_NEWS;
    else if ( streq(s, "syslog") )   facility = LOG_SYSLOG;
    else if ( streq(s, "user") )     facility = LOG_USER;
    else if ( streq(s, "uucp") )     facility = LOG_UUCP;
    else return PL_domain_error("syslog_facility", t);
  } else
    return FALSE;

  *fac = facility;
  return TRUE;
}

static int
get_priority(term_t t, int *pri)
{ char *s;
  int priority;

  if ( PL_get_chars(t, &s, CVT_ATOM|CVT_EXCEPTION) )
  { if      ( streq(s, "emerg"  ) ) priority =	LOG_EMERG;
    else if ( streq(s, "alert") )   priority = LOG_ALERT;
    else if ( streq(s, "crit") )    priority = LOG_CRIT;
    else if ( streq(s, "err") )     priority = LOG_ERR;
    else if ( streq(s, "warning") ) priority = LOG_WARNING;
    else if ( streq(s, "notice") )  priority = LOG_NOTICE;
    else if ( streq(s, "info") )    priority = LOG_INFO;
    else if ( streq(s, "debug") )   priority = LOG_DEBUG;
    else return PL_domain_error("syslog_priority", t);
  } else
    return FALSE;

  *pri = priority;
  return TRUE;
}



static foreign_t
pl_openlog(term_t Ident, term_t Option, term_t Facility)
{ char *ident;
  int option = 0;
  int facility = 0;

  if ( PL_get_chars(Ident, &ident, CVT_ATOM|REP_MB|CVT_EXCEPTION) &&
       get_option(Option, &option) &&
       get_facility(Facility, &facility) )
  { openlog(strdup(ident), option, facility);
    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_syslog(term_t Priority, term_t Message)
{ int p;
  char *msg;

  if ( get_priority(Priority, &p) &&
       PL_get_chars(Message, &msg,
		    CVT_ALL|CVT_VARIABLE|CVT_WRITE|REP_MB|CVT_EXCEPTION) )
  { syslog(p, "%s", msg);

    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_closelog(void)
{ closelog();

  return TRUE;
}


install_t
install_syslog(void)
{ PL_register_foreign("$openlog",  3, pl_openlog,  0);
  PL_register_foreign("syslog",    2, pl_syslog,   0);
  PL_register_foreign("$closelog", 0, pl_closelog, 0);
}
