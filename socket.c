/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2023, University of Amsterdam
                              VU University Amsterdam
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

#define O_DEBUG 1

#include <SWI-Prolog.h>
#include <config.h>

#if defined(__MINGW32__)
#define WINVER 0x0501
#include <ws2tcpip.h>
#endif

#include "nonblockio.h"

#include <SWI-Stream.h>
#include "clib.h"
#include "error.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <assert.h>
#include <string.h>
#ifdef __WINDOWS__
#include <malloc.h>
#endif

#ifdef __WINDOWS__
#define GET_ERRNO WSAGetLastError()
#define GET_H_ERRNO WSAGetLastError()
#else
#define GET_ERRNO errno
#define GET_H_ERRNO h_errno
#endif

#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#else
/* Windows does not have the header, but does have AF_UNIX? */
#undef AF_UNIX
#endif

#if !defined(HAVE_IP_MREQN) && defined(__APPLE__)
/* 10.6 doesn't provide this */
#define ip_mreqn ip_mreq
#endif

static atom_t ATOM_address;
static atom_t ATOM_af_unix;
static atom_t ATOM_as;
static atom_t ATOM_atom;
static atom_t ATOM_bindtodevice;
static atom_t ATOM_broadcast;
static atom_t ATOM_codes;
static atom_t ATOM_dgram;
static atom_t ATOM_dispatch;
static atom_t ATOM_domain;
static atom_t ATOM_encoding;
static atom_t ATOM_file_no;
static atom_t ATOM_host;
static atom_t ATOM_inet6;
static atom_t ATOM_inet;
static atom_t ATOM_infinite;
static atom_t ATOM_ip_add_membership;
static atom_t ATOM_ip_drop_membership;
static atom_t ATOM_local;
static atom_t ATOM_max_message_size;
static atom_t ATOM_nodelay;
static atom_t ATOM_nonblock;
static atom_t ATOM_reuseaddr;
static atom_t ATOM_sndbuf;
static atom_t ATOM_sockaddr;
static atom_t ATOM_stream;
static atom_t ATOM_string;
static atom_t ATOM_term;
static atom_t ATOM_type;
static atom_t ATOM_unix;

static int get_socket_from_stream(term_t t, IOSTREAM **s, nbio_sock_t *sp);


		 /*******************************
		 *	      SYMBOL		*
		 *******************************/

static void
acquire_socket_symbol(atom_t symbol)
{ nbio_sock_t s = *(nbio_sock_t*)PL_blob_data(symbol, NULL, NULL);

  nbio_set_symbol(s, symbol);
}

static int
release_socket_symbol(atom_t symbol)
{ nbio_sock_t s = *(nbio_sock_t*)PL_blob_data(symbol, NULL, NULL);

  nbio_closesocket(s);			/* no-op if already done */
  freeSocket(s);

  return TRUE;
}

static int
compare_socket_symbols(atom_t a, atom_t b)
{ nbio_sock_t sa = *(nbio_sock_t*)PL_blob_data(a, NULL, NULL);
  nbio_sock_t sb = *(nbio_sock_t*)PL_blob_data(b, NULL, NULL);

  return ( sa > sb ?  1 :
	   sa < sb ? -1 : 0
	 );
}


static int
write_socket_symbol(IOSTREAM *s, atom_t symbol, int flags)
{ nbio_sock_t sock = *(nbio_sock_t*)PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<socket>(%p)", sock);
  return TRUE;
}


static PL_blob_t socket_blob =
{ PL_BLOB_MAGIC,
  0,
  "socket",
  release_socket_symbol,
  compare_socket_symbols,
  write_socket_symbol,
  acquire_socket_symbol
};


static int
tcp_unify_socket(term_t handle, nbio_sock_t socket)
{ if ( PL_unify_blob(handle, &socket, sizeof(socket), &socket_blob) )
    return TRUE;

  if ( !PL_is_variable(handle) )
    return PL_uninstantiation_error(handle);

  return FALSE;					/* (resource) error */
}


static int
tcp_get_socket(term_t handle, nbio_sock_t *sp)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(handle, &data, NULL, &type) && type == &socket_blob)
  { nbio_sock_t s = *(nbio_sock_t*)data;

    if ( !is_nbio_socket(s) )
      return PL_existence_error("socket", handle);

    *sp = s;

    return TRUE;
  }

  if ( get_socket_from_stream(handle, NULL, sp) )
    return TRUE;

  return PL_type_error("socket", handle);
}


		 /*******************************
		 *	     CONVERSION		*
		 *******************************/

static PL_option_t host_address_options[] =
{ PL_OPTION("domain",    OPT_ATOM),
  PL_OPTION("type",	 OPT_ATOM),
  PL_OPTION("canonname", OPT_BOOL),
  PL_OPTIONS_END
};


static int
put_socket_domain(term_t t, int domain)
{ switch(domain)
  { case AF_INET:  return PL_put_atom(t, ATOM_inet);
    case AF_INET6: return PL_put_atom(t, ATOM_inet6);
    default:
      return FALSE;
  }
}

static int
put_socket_type(term_t t, int type)
{ switch(type)
  { case SOCK_STREAM: return PL_put_atom(t, ATOM_stream);
    case SOCK_DGRAM:  return PL_put_atom(t, ATOM_dgram);
    default:
      return FALSE;
  }
}



static foreign_t
pl_host_address(term_t Host, term_t Ip, term_t options)
{ struct sockaddr_storage addr;
  char *host_name;
  atom_t a_domain = 0;
  atom_t a_type   = 0;
  int    cname    = FALSE;

  if ( !PL_scan_options(options, 0, "socket_options", host_address_options,
                        &a_domain, &a_type, &cname) )
    return FALSE;

  if ( PL_get_chars(Host, &host_name, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { struct addrinfo hints;
    struct addrinfo *res;
    int rc;

    memset(&hints, 0, sizeof(hints));
    if ( a_domain )
    { if ( a_domain == ATOM_inet )
	hints.ai_family = AF_INET;
      else if ( a_domain == ATOM_inet6 )
	hints.ai_family = AF_INET6;
      else
	return PL_domain_error("socket_domain", a_domain);
    } else
      hints.ai_family = AF_UNSPEC;

    if ( a_type )
    { if ( a_type == ATOM_stream )
	hints.ai_socktype = SOCK_STREAM;
      else if ( a_type == ATOM_dgram )
	hints.ai_socktype = SOCK_DGRAM;
      else
	return PL_domain_error("socket_type", a_type);
    }

    if ( cname )
      hints.ai_flags |= AI_CANONNAME;

    if ( (rc=getaddrinfo(host_name, NULL, &hints, &res)) == 0 )
    { term_t tail = PL_copy_term_ref(Ip);
      term_t head = PL_new_term_ref();
      term_t dict = PL_new_term_ref();
      struct addrinfo *addri = res;
      atom_t keys[4];
      keys[0] = ATOM_host;
      keys[1] = ATOM_domain;
      keys[2] = ATOM_type;
      keys[3] = ATOM_address;
      term_t values = PL_new_term_refs(4);

      for( ; addri; addri = addri->ai_next )
      {
#if 0
	Sdprintf("flags = %d; family = %d; type = %d, protocol = %d\n",
		 addri->ai_flags,
		 addri->ai_family,
		 addri->ai_socktype,
		 addri->ai_protocol);
#endif
	if ( !put_socket_domain(values+1, addri->ai_family) ||
	     !put_socket_type(values+2, addri->ai_socktype) ||
	     !PL_put_variable(values+3) ||
	     !nbio_unify_addr(values+3, addri->ai_addr) ||
	     !PL_unify_list(tail, head, tail) )
	{ if ( !PL_exception(0) )
	    continue;
	out_fail:
	  freeaddrinfo(res);
	  return FALSE;
	}
	if ( res->ai_canonname )
	{ if ( !PL_put_chars(values+0, PL_ATOM|REP_MB,
			     (size_t)-1, res->ai_canonname) ||
	       !PL_put_dict(dict, ATOM_sockaddr, 4, keys, values) )
	    goto out_fail;
	} else
	{ if ( !PL_put_dict(dict, ATOM_sockaddr, 3, keys+1, values+1) )
	    goto out_fail;
	}
	if ( !PL_unify(head, dict) )
	  goto out_fail;
      }
      freeaddrinfo(res);

      return PL_unify_nil(tail);
    } else
    { return nbio_error(rc, TCP_GAI_ERRNO);
    }
  } else if ( nbio_get_ip(AF_INET, Ip, &addr) )
  { struct hostent *host;
    struct sockaddr_in *sin = (struct sockaddr_in*)&addr;
    struct addr_in *in = (struct addr_in *)&sin->sin_addr;

    if ( (host = gethostbyaddr((char *)&in, sizeof(in), AF_INET)) )
      return PL_unify_atom_chars(Host, host->h_name);
    else
      return nbio_error(GET_H_ERRNO, TCP_HERRNO);
  }

  return FALSE;
}


#ifndef HAVE_IP_MREQN
#define ip_mreqn ip_mreq
#define imr_address imr_interface
#endif


static foreign_t
pl_setopt(term_t Socket, term_t opt)
{ nbio_sock_t socket;
  atom_t a;
  size_t arity;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;

  if ( PL_get_name_arity(opt, &a, &arity) )
  { if ( a == ATOM_reuseaddr && arity == 0 )
    { if ( nbio_setopt(socket, TCP_REUSEADDR, TRUE) == 0 )
	return TRUE;

      return FALSE;
    } else if ( a == ATOM_bindtodevice && arity == 1)
    { term_t a = PL_new_term_ref();
      char *dev;
      int rc;

      _PL_get_arg(1, opt, a);
      if ( !PL_get_chars(a, &dev, CVT_ATOM|CVT_EXCEPTION) )
	return FALSE;

      if ( (rc=nbio_setopt(socket, SCK_BINDTODEVICE, dev)) == 0 )
	return TRUE;
      if ( rc == -2 )
	goto not_implemented;

      return FALSE;
    } else if ( a == ATOM_nodelay && arity <= 1 )
    { int enable, rc;

      if ( arity == 0 )
      { enable = TRUE;
      } else /*if ( arity == 1 )*/
      { term_t a = PL_new_term_ref();

	_PL_get_arg(1, opt, a);
	if ( !PL_get_bool(a, &enable) )
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a, "boolean");
      }

      if ( (rc=nbio_setopt(socket, TCP_NO_DELAY, enable) == 0) )
	return TRUE;
      if ( rc == -2 )
	goto not_implemented;

      return FALSE;
    } else if ( a == ATOM_broadcast && arity == 0 )
    { if ( nbio_setopt(socket, UDP_BROADCAST, TRUE) == 0 )
	return TRUE;

      return FALSE;
    } else if ( a == ATOM_dispatch && arity == 1 )
    { int val;
      term_t a1 = PL_new_term_ref();

      if ( PL_get_arg(1, opt, a1) && PL_get_bool(a1, &val) )
      { if ( nbio_setopt(socket, TCP_DISPATCH, val) == 0 )
	  return TRUE;
	return FALSE;
      }
    } else if ( a == ATOM_nonblock && arity == 0 )
    { if ( nbio_setopt(socket, TCP_NONBLOCK) == 0 )
	return TRUE;
      return FALSE;
#ifdef IP_ADD_MEMBERSHIP
    } else if ( (a == ATOM_ip_add_membership || a == ATOM_ip_drop_membership)
		&& arity >= 1 )
    { struct ip_mreqn mreq;
      term_t arg = PL_new_term_ref();
      int opname = (a == ATOM_ip_add_membership) ? IP_ADD_MEMBERSHIP
						 : IP_DROP_MEMBERSHIP;

      if ( nbio_domain(socket) != AF_INET )
	return PL_permission_error(a == ATOM_ip_add_membership ? "ip_add_membership"
							       : "ip_drop_membership",
				   "socket",
				   Socket);

      _PL_get_arg(1, opt, arg);
      memset(&mreq, 0, sizeof(mreq));
      if ( !nbio_get_ip4(arg, &mreq.imr_multiaddr, TRUE) )
	return PL_domain_error("ip", arg);
      if ( arity >= 2 )
      { _PL_get_arg(2, opt, arg);
	if ( !nbio_get_ip4(arg, &mreq.imr_address, TRUE) )
	  return PL_domain_error("ip", arg);
      } else
	mreq.imr_address.s_addr = htonl(INADDR_ANY);
#ifdef HAVE_IP_MREQN
      if ( arity == 3 )
      { _PL_get_arg(3, opt, arg);
	if ( !PL_get_integer(arg, &mreq.imr_ifindex) )
	  return FALSE;
      }
#endif
      if ( arity > 3 )
	goto not_implemented;

      if ( setsockopt(nbio_fd(socket), IPPROTO_IP, opname,
		      (void*)&mreq, sizeof(mreq)) < 0 )
	return nbio_error(GET_ERRNO, TCP_ERRNO);
      else
	return TRUE;
#endif
    } else if ( a == ATOM_sndbuf && arity == 1 )
    { int bufsize;
      int rc;
      term_t a = PL_new_term_ref();
      _PL_get_arg(1, opt, a);
      if ( !PL_get_integer(a, &bufsize) )
	return pl_error(NULL, 0, NULL, ERR_DOMAIN, a, "integer");
      if ( (rc=nbio_setopt(socket, TCP_SNDBUF, bufsize) == 0) )
	return TRUE;
      if ( rc == -2 )
	goto not_implemented;

      return FALSE;
    }
  }

not_implemented:
  return pl_error(NULL, 0, NULL, ERR_DOMAIN, opt, "socket_option");
}


static foreign_t
pl_getopt(term_t Socket, term_t opt)
{ nbio_sock_t socket;
  atom_t a;
  size_t arity;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;

  if ( PL_get_name_arity(opt, &a, &arity) && arity >= 1 )
  { term_t a1 = PL_new_term_ref();

    _PL_get_arg(1, opt, a1);
    if ( a == ATOM_file_no && arity == 1 )
    { SOCKET s = nbio_fd(socket);

      if ( s != -1 )
	return PL_unify_integer(a1, s);
      return FALSE;
    }
  }

  return pl_error(NULL, 0, NULL, ERR_DOMAIN, opt, "socket_option");
}

#include "sockcommon.c"

static socklen_t
sizeof_sockaddr(struct sockaddr_storage *sockaddr)
{ switch(sockaddr->ss_family)
  { case AF_INET:
      return sizeof(struct sockaddr_in);
    case AF_INET6:
      return sizeof(struct sockaddr_in6);
    case AF_UNIX:
    { struct sockaddr_un *a = (struct sockaddr_un*)sockaddr;
      return offsetof(struct sockaddr_un, sun_path) + strlen(a->sun_path) + 1;
    }
    default:
      assert(0);
      return 0;
  }
}

		 /*******************************
		 *	    UDP SOCKETS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
udp_receive(+Socket, -String, -From, +Options)
udp_send(+String, +String, +To, +Options)

From/To are of the format <Host>:<Port>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define UDP_MAXDATA         65535
#define UDP_DEFAULT_BUFSIZE  4096

static int
unify_address(term_t t, struct sockaddr_storage *addr)
{ term_t av = PL_new_term_refs(2);

  switch ( addr->ss_family )
  { case AF_INET:
    { struct sockaddr_in *addr4 = (struct sockaddr_in*)addr;
      if ( !nbio_unify_ip4(av+0, ntohl(addr4->sin_addr.s_addr)) ||
	   !PL_unify_integer(av+1, ntohs(addr4->sin_port)) )
	return FALSE;
      break;
    }
    default:
      assert(0);
  }

  return PL_unify_term(t, PL_FUNCTOR_CHARS, ":", 2,
		       PL_TERM, av+0,
		       PL_TERM, av+1);
}

static int
get_representation(term_t arg, int *rp)
{ atom_t a;
  IOENC enc;
  int rep;

  if ( !PL_get_atom_ex(arg, &a) )
    return FALSE;

  switch((enc=PL_atom_to_encoding(a)))
  { case ENC_OCTET:
    case ENC_ISO_LATIN_1: rep = REP_ISO_LATIN_1; break;
    case ENC_UTF8:        rep = REP_UTF8;        break;
    case ENC_ANSI:        rep = REP_MB;          break;
    default:
      return PL_domain_error("encoding", arg);
  }

  *rp = rep;
  return TRUE;
}

static int
get_as(term_t arg, int *asp)
{ atom_t a;
  int as;

  if ( !PL_get_atom_ex(arg, &a) )
    return FALSE;
  if ( a == ATOM_atom )
    as = PL_ATOM;
  else if ( a == ATOM_codes )
    as = PL_CODE_LIST;
  else if ( a == ATOM_string )
    as = PL_STRING;
  else if ( a == ATOM_term )
    as = PL_TERM;
  else
    return PL_domain_error("as", arg);

  *asp = as;
  return TRUE;
}


#ifdef __WINDOWS__
#define socklen_t int
#endif

static foreign_t
udp_receive(term_t Socket, term_t Data, term_t From, term_t options)
{ struct sockaddr_storage sockaddr;
  socklen_t alen;
  nbio_sock_t socket;
  int flags = 0;
  char smallbuf[UDP_DEFAULT_BUFSIZE];
  char *buf = smallbuf;
  int bufsize = UDP_DEFAULT_BUFSIZE;
  term_t varport = 0;
  ssize_t n;
  int as = PL_STRING;
  int rc;
  int rep = REP_ISO_LATIN_1;

  if ( !PL_get_nil(options) )
  { term_t tail = PL_copy_term_ref(options);
    term_t head = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();

    while(PL_get_list(tail, head, tail))
    { atom_t name;
      size_t arity;

      if ( PL_get_name_arity(head, &name, &arity) && arity == 1 )
      { _PL_get_arg(1, head, arg);

	if ( name == ATOM_as )
	{ if ( !get_as(arg, &as) )
	    return FALSE;
	} else if ( name == ATOM_max_message_size )
	{ if ( !PL_get_integer(arg, &bufsize) )
	    return pl_error(NULL, 0, NULL, ERR_TYPE, arg, "integer");
	  if ( bufsize < 0 || bufsize > UDP_MAXDATA )
	    return pl_error(NULL, 0, NULL, ERR_DOMAIN, arg, "0 - 65535");
	} else if ( name == ATOM_encoding )
	{ if ( !get_representation(arg, &rep) )
	    return FALSE;
	}
      } else
	return PL_type_error("option", head);
    }
    if ( !PL_get_nil_ex(tail) )
      return FALSE;
  }

  if ( !tcp_get_socket(Socket, &socket) ||
       !nbio_get_sockaddr(socket, From, &sockaddr, &varport) )
    return FALSE;

  if ( bufsize > UDP_DEFAULT_BUFSIZE )
  { if ( !(buf = malloc(bufsize)) )
      return pl_error(NULL, 0, NULL, ERR_RESOURCE, "memory");
  }

  if ( (n=nbio_recvfrom(socket, buf, bufsize, flags,
			(struct sockaddr*)&sockaddr, &alen)) == -1 )
  { rc = nbio_error(GET_ERRNO, TCP_ERRNO);;
    goto out;
  }

  if ( as == PL_TERM )
  { term_t tmp;

    rc = ( (tmp=PL_new_term_ref()) &&
	   PL_put_term_from_chars(tmp, rep|CVT_EXCEPTION, n, buf) &&
	   PL_unify(tmp, Data) );
  } else
  { rc = PL_unify_chars(Data, as|rep, n, buf);
  }

  rc = rc && unify_address(From, &sockaddr);

out:
  if ( buf != smallbuf )
    free(buf);

  return rc;
}


static foreign_t
udp_send(term_t Socket, term_t Data, term_t To, term_t options)
{ struct sockaddr_storage sockaddr;
  nbio_sock_t socket;
  int flags = 0L;
  char *data;
  size_t dlen;
  ssize_t n;
  int rep = REP_ISO_LATIN_1;
  int as = PL_VARIABLE;			/* any */
  int cvt;

  if ( !PL_get_nil(options) )
  { term_t tail = PL_copy_term_ref(options);
    term_t head = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();

    while(PL_get_list(tail, head, tail))
    { atom_t name;
      size_t arity;

      if ( PL_get_name_arity(head, &name, &arity) && arity == 1 )
      {	_PL_get_arg(1, head, arg);

	if ( name == ATOM_as )
	{ if ( !get_as(arg, &as) )
	    return FALSE;
	} else if ( name == ATOM_encoding )
	{ if ( !get_representation(arg, &rep) )
	    return FALSE;
	}
      } else
	return PL_type_error("option", head);
    }
    if ( !PL_get_nil_ex(tail) )
      return FALSE;
  }

  switch(as)
  { case PL_VARIABLE:  cvt = CVT_ALL;		  break;
    case PL_ATOM:      cvt = CVT_ATOM;            break;
    case PL_STRING:
    case PL_CODE_LIST: cvt = CVT_STRING|CVT_LIST; break;
    case PL_TERM:      cvt = CVT_WRITE_CANONICAL; break;
    default:	       assert(0);                 return FALSE;
  }

  if ( !PL_get_nchars(Data, &dlen, &data, cvt|CVT_EXCEPTION|rep) )
    return FALSE;

  if ( !tcp_get_socket(Socket, &socket) ||
       !nbio_get_sockaddr(socket, To, &sockaddr, NULL) )
    return FALSE;

  if ( (n=nbio_sendto(socket, data,
		      (int)dlen,
		      flags,
		      (struct sockaddr*)&sockaddr,
		      sizeof_sockaddr(&sockaddr))) == -1 )
    return nbio_error(GET_ERRNO, TCP_ERRNO);;

  return TRUE;
}

		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

static int
atom_domain_error(const char *domain, atom_t a)
{ term_t t;

  return ( (t=PL_new_term_ref()) &&
	   PL_put_atom(t, a) &&
	   PL_domain_error(domain, t) );
}


static PL_option_t socket_options[] =
{ PL_OPTION("domain",   OPT_ATOM),
  PL_OPTION("type",	OPT_ATOM),
  PL_OPTIONS_END
};


static foreign_t
socket_create(term_t socket, term_t options)
{ atom_t a_domain = ATOM_inet;
  atom_t a_type   = ATOM_stream;
  int domain;
  int type;
  nbio_sock_t sock;

  if ( !PL_scan_options(options, 0, "socket_options", socket_options,
                        &a_domain, &a_type) )
    return FALSE;

  if ( a_domain == ATOM_inet )
    domain = AF_INET;
  else if ( a_domain == ATOM_inet6 )
    domain = AF_INET6;
#ifdef AF_UNIX
  else if ( a_domain == ATOM_unix || a_domain == ATOM_local )
    domain = AF_UNIX;
#endif
  else
    return atom_domain_error("socket_domain", a_domain);

  if ( a_type == ATOM_stream )
    type = SOCK_STREAM;
  else if ( a_type == ATOM_dgram )
    type = SOCK_DGRAM;
  else
    return atom_domain_error("socket_type", a_type);

  if ( !(sock = nbio_socket(domain, type, 0)) )
    return FALSE;

  return tcp_unify_socket(socket, sock);
}

static foreign_t
create_socket(int domain, int type, term_t socket)
{ nbio_sock_t sock;

  if ( !(sock = nbio_socket(domain, type, 0)) )
    return FALSE;

  return tcp_unify_socket(socket, sock);
}


static foreign_t
tcp_socket(term_t socket)
{ return create_socket(AF_INET, SOCK_STREAM, socket);
}


static foreign_t
udp_socket(term_t socket)
{ return create_socket(AF_INET, SOCK_DGRAM, socket);
}


#ifdef AF_UNIX
static foreign_t
unix_domain_socket(term_t socket)
{ return create_socket(AF_UNIX, SOCK_STREAM, socket);
}

static int
af_unix_address(term_t Address,
		struct sockaddr_un *sockaddr, int *addrlen,
		int flags)
{ char *file_name_chars;
  int nmlen;

  if ( !PL_get_file_name(Address, &file_name_chars,
			 PL_FILE_OSPATH|flags) )
    return FALSE;
  nmlen = strlen(file_name_chars);
  if ( nmlen >= sizeof(sockaddr->sun_path) )
  { PL_representation_error("af_unix_name");
    return FALSE;
  }

  memset(sockaddr, 0, sizeof(*sockaddr));
  sockaddr->sun_family = AF_UNIX;
  memcpy(sockaddr->sun_path, file_name_chars, nmlen);
  *addrlen = offsetof(struct sockaddr_un, sun_path) + nmlen + 1;

  return TRUE;
}

#endif /*AF_UNIX*/

static int
af_unix_connect(nbio_sock_t sock, term_t Address)
{
#ifdef AF_UNIX
  if ( nbio_domain(sock) == AF_UNIX )
  { struct sockaddr_un sockaddr;
    int addrlen;

    return ( af_unix_address(Address, &sockaddr, &addrlen, PL_FILE_READ) &&
	     nbio_connect(sock, (struct sockaddr *)&sockaddr, addrlen) == 0 );
  } else
#endif
  { return -1;
  }
}

static int
af_unix_bind(nbio_sock_t sock, term_t Address)
{
#ifdef AF_UNIX
  if ( nbio_domain(sock) == AF_UNIX )
  { struct sockaddr_un sockaddr;
    int addrlen;

    return ( af_unix_address(Address, &sockaddr, &addrlen, 0) &&
	     nbio_bind(sock, (struct sockaddr *)&sockaddr, addrlen) == 0 );
  } else
#endif
  { return -1;
  }
}

static foreign_t
pl_connect(term_t Socket, term_t Address)
{ nbio_sock_t sock;
  struct sockaddr_storage sockaddr;
  int rc;

  if ( !tcp_get_socket(Socket, &sock) )
    return FALSE;

  if ( (rc=af_unix_connect(sock, Address)) != -1 )
    return rc;

  if ( !nbio_get_sockaddr(sock, Address, &sockaddr, NULL) )
    return FALSE;

  if ( nbio_connect(sock, (struct sockaddr*)&sockaddr,
		    sizeof_sockaddr(&sockaddr)) == 0 )
    return TRUE;

  return FALSE;
}


static foreign_t
pl_bind(term_t Socket, term_t Address)
{ nbio_sock_t socket;
  int rc;

  if ( !tcp_get_socket(Socket, &socket) )
    return FALSE;

  if ( (rc=af_unix_bind(socket, Address)) != -1 )
  { return rc;
  } else
  { struct sockaddr_storage sockaddr;
    term_t varport = 0;

    memset(&sockaddr, 0, sizeof(sockaddr));
    if ( !nbio_get_sockaddr(socket, Address, &sockaddr, &varport) )
      return FALSE;

    if ( nbio_bind(socket, (struct sockaddr*)&sockaddr,
		   sizeof_sockaddr(&sockaddr)) < 0 )
      return FALSE;

    if ( varport )
    { SOCKET fd = nbio_fd(socket);
      struct sockaddr_in addr;
#ifdef __WINDOWS__
      int len = sizeof(addr);
#else
      socklen_t len = sizeof(addr);
#endif

      if ( getsockname(fd, (struct sockaddr *) &addr, &len) )
	return nbio_error(GET_ERRNO, TCP_ERRNO);
      return PL_unify_integer(varport, ntohs(addr.sin_port));
    }

    return TRUE;
  }
}


static foreign_t
pl_accept(term_t Master, term_t Slave, term_t Peer)
{ nbio_sock_t master, slave;

  if ( !tcp_get_socket(Master, &master) )
    return FALSE;

#ifdef AF_UNIX
  if ( nbio_domain(master) == AF_UNIX )
  { struct sockaddr_un addr;
    socklen_t addrlen = sizeof(addr);

    if ( !PL_unify_atom(Peer, ATOM_af_unix) )
      return FALSE;
    if ( !(slave = nbio_accept(master, (struct sockaddr*)&addr, &addrlen)) )
      return FALSE;
  } else
#endif
  { struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);

    if ( !(slave = nbio_accept(master, (struct sockaddr*)&addr, &addrlen)) )
      return FALSE;
    if ( !nbio_unify_ip4(Peer, ntohl(addr.sin_addr.s_addr)) )
      goto failure;
  }

  if ( tcp_unify_socket(Slave, slave) )
    return TRUE;

failure:
  nbio_closesocket(slave);
  return FALSE;
}


static foreign_t
pl_gethostname(term_t name)
{ static atom_t hname;

  if ( !hname )
  { char buf[256];

    if ( gethostname(buf, sizeof(buf)) == 0 )
    { struct addrinfo *res;
      struct addrinfo hints;

      memset(&hints, 0, sizeof(hints));
      hints.ai_flags = AI_CANONNAME;

      if ( getaddrinfo(buf, NULL, &hints, &res) == 0 )
      { hname = PL_new_atom(res->ai_canonname);
        freeaddrinfo(res);
      }
      else
	hname = PL_new_atom(buf);

    } else
    { return nbio_error(GET_ERRNO, TCP_ERRNO);
    }
  }

  return PL_unify_atom(name, hname);
}


#ifdef O_DEBUG
static foreign_t
pl_debug(term_t val)
{ int dbg;

  if ( PL_get_integer(val, &dbg) )
  { nbio_debug(dbg);
    return TRUE;
  }

  return FALSE;
}
#endif

#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install_socket(void)
{ nbio_init("socket");

  MKATOM(address);
  MKATOM(af_unix);
  MKATOM(as);
  MKATOM(atom);
  MKATOM(bindtodevice);
  MKATOM(broadcast);
  MKATOM(codes);
  MKATOM(dgram);
  MKATOM(dispatch);
  MKATOM(domain);
  MKATOM(encoding);
  MKATOM(file_no);
  MKATOM(host);
  MKATOM(inet);
  MKATOM(inet6);
  MKATOM(infinite);
  MKATOM(ip_add_membership);
  MKATOM(ip_drop_membership);
  MKATOM(local);
  MKATOM(max_message_size);
  MKATOM(nodelay);
  MKATOM(nonblock);
  MKATOM(reuseaddr);
  MKATOM(sndbuf);
  MKATOM(sockaddr);
  MKATOM(stream);
  MKATOM(string);
  MKATOM(term);
  MKATOM(type);
  MKATOM(unix);

  PL_register_foreign("tcp_accept",           3, pl_accept,           0);
  PL_register_foreign("tcp_bind",             2, pl_bind,             0);
  PL_register_foreign("tcp_connect",          2, pl_connect,	      0);
  PL_register_foreign("tcp_listen",           2, pl_listen,           0);
  PL_register_foreign("tcp_open_socket",      3, pl_open_socket,      0);
  PL_register_foreign("tcp_socket",           1, tcp_socket,          0);
  PL_register_foreign("tcp_close_socket",     1, pl_close_socket,     0);
  PL_register_foreign("tcp_setopt",           2, pl_setopt,           0);
  PL_register_foreign("tcp_getopt",           2, pl_getopt,           0);
  PL_register_foreign("$host_address",        3, pl_host_address,     0);
  PL_register_foreign("gethostname",          1, pl_gethostname,      0);

  PL_register_foreign("socket_create",        2, socket_create,       0);
  PL_register_foreign("udp_socket",           1, udp_socket,          0);
  PL_register_foreign("udp_receive",	      4, udp_receive,	      0);
  PL_register_foreign("udp_send",	      4, udp_send,	      0);

#ifndef __WINDOWS__
  PL_register_foreign("unix_domain_socket",   1, unix_domain_socket,  0);
#endif

#ifdef O_DEBUG
  PL_register_foreign("tcp_debug",	      1, pl_debug,	      0);
#endif
}


install_t
uninstall_socket(void)
{ nbio_cleanup();
}
