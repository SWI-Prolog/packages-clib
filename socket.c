/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2018, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
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

#include <config.h>

#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# endif
#endif

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

#if defined(__linux__) || defined(__APPLE__)
#define HAVE_IP_MREQN 1
#endif

static atom_t ATOM_reuseaddr;		/* "reuseaddr" */
static atom_t ATOM_bindtodevice;	/* "bindtodevice" */
static atom_t ATOM_broadcast;		/* "broadcast" */
static atom_t ATOM_nodelay;		/* "nodelay" */
static atom_t ATOM_dispatch;		/* "dispatch" */
static atom_t ATOM_nonblock;		/* "nonblock" */
static atom_t ATOM_infinite;		/* "infinite" */
static atom_t ATOM_as;			/* "as" */
static atom_t ATOM_atom;		/* "atom" */
static atom_t ATOM_string;		/* "string" */
static atom_t ATOM_codes;		/* "codes" */
static atom_t ATOM_max_message_size;    /* "message_size" */
static atom_t ATOM_file_no;		/* "file_no" */
static atom_t ATOM_ip_add_membership;	/* "ip_add_membership" */
static atom_t ATOM_ip_drop_membership;	/* "ip_drop_membership" */
static functor_t FUNCTOR_socket1;	/* $socket(Id) */

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

static foreign_t
pl_host_to_address(term_t Host, term_t Ip)
{ struct in_addr ip;
  char *host_name;

  if ( PL_get_atom_chars(Host, &host_name) )
  { struct addrinfo hints;
    struct addrinfo *res;
    int rc;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;

    if ( (rc=getaddrinfo(host_name, NULL, &hints, &res)) == 0 )
    { int rc;

      switch( res->ai_family )
      { case AF_INET:
	{ struct sockaddr_in *addr = (struct sockaddr_in*)res->ai_addr;

	  rc = nbio_unify_ip4(Ip, ntohl(addr->sin_addr.s_addr));
	  break;
	}
	case AF_INET6:
	{ rc = PL_warning("tcp_host_to_address/2: IPv6 address not supported");
	  break;
	}
	default:
	  assert(0);
	  rc = FALSE;
      }

      freeaddrinfo(res);

      return rc;
    } else
    { return nbio_error(rc, TCP_GAI_ERRNO);
    }
  } else if ( nbio_get_ip(Ip, &ip) )
  { struct hostent *host;

    if ( (host = gethostbyaddr((char *)&ip, sizeof(ip), AF_INET)) )
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

      _PL_get_arg(1, opt, arg);
      memset(&mreq, 0, sizeof(mreq));
      if ( !nbio_get_ip(arg, &mreq.imr_multiaddr) )
	return PL_domain_error("ip", arg);
      if ( arity >= 2 )
      { _PL_get_arg(2, opt, arg);
	if ( !nbio_get_ip(arg, &mreq.imr_address) )
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
unify_address(term_t t, struct sockaddr_in *addr)
{ term_t av = PL_new_term_refs(2);

  if ( !nbio_unify_ip4(av+0, ntohl(addr->sin_addr.s_addr)) ||
       !PL_unify_integer(av+1, ntohs(addr->sin_port)) )
    return FALSE;

  return PL_unify_term(t, PL_FUNCTOR_CHARS, ":", 2,
		       PL_TERM, av+0,
		       PL_TERM, av+1);
}



static foreign_t
udp_receive(term_t Socket, term_t Data, term_t From, term_t options)
{ struct sockaddr_in sockaddr;
#ifdef __WINDOWS__
  int alen = sizeof(sockaddr);
#else
  socklen_t alen = sizeof(sockaddr);
#endif
  nbio_sock_t socket;
  int flags = 0;
  char smallbuf[UDP_DEFAULT_BUFSIZE];
  char *buf = smallbuf;
  int bufsize = UDP_DEFAULT_BUFSIZE;
  term_t varport = 0;
  ssize_t n;
  int as = PL_STRING;
  int rc;

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
	{ atom_t a;

	  if ( !PL_get_atom(arg, &a) )
	    return pl_error(NULL, 0, NULL, ERR_TYPE, head, "atom");
	  if ( a == ATOM_atom )
	    as = PL_ATOM;
	  else if ( a == ATOM_codes )
	    as = PL_CODE_LIST;
	  else if ( a == ATOM_string )
	    as = PL_STRING;
	  else
	    return pl_error(NULL, 0, NULL, ERR_DOMAIN, arg, "as_option");

	} else if ( name == ATOM_max_message_size )
	{ if ( !PL_get_integer(arg, &bufsize) )
	    return pl_error(NULL, 0, NULL, ERR_TYPE, arg, "integer");
	  if ( bufsize < 0 || bufsize > UDP_MAXDATA )
	    return pl_error(NULL, 0, NULL, ERR_DOMAIN, arg, "0 - 65535");
	}
      } else
	return pl_error(NULL, 0, NULL, ERR_TYPE, head, "option");
    }
    if ( !PL_get_nil(tail) )
      return pl_error(NULL, 0, NULL, ERR_TYPE, tail, "list");
  }

  if ( !tcp_get_socket(Socket, &socket) ||
       !nbio_get_sockaddr(From, &sockaddr, &varport) )
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

  rc = ( PL_unify_chars(Data, as, n, buf) &&
	 unify_address(From, &sockaddr)
       );

out:
  if ( buf != smallbuf )
    free(buf);

  return rc;
}


static foreign_t
udp_send(term_t Socket, term_t Data, term_t To, term_t Options)
{ struct sockaddr_in sockaddr;
#ifdef __WINDOWS__
  int alen = sizeof(sockaddr);
#else
  int alen = sizeof(sockaddr);
#endif
  nbio_sock_t socket;
  int flags = 0L;
  char *data;
  size_t dlen;
  ssize_t n;

  if ( !PL_get_nchars(Data, &dlen, &data, CVT_ALL|CVT_EXCEPTION) )
    return FALSE;

  if ( !tcp_get_socket(Socket, &socket) ||
       !nbio_get_sockaddr(To, &sockaddr, NULL) )
    return FALSE;

  if ( (n=nbio_sendto(socket, data,
		      (int)dlen,
		      flags,
		      (struct sockaddr*)&sockaddr, alen)) == -1 )
    return nbio_error(GET_ERRNO, TCP_ERRNO);;

  return TRUE;
}

		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/


static foreign_t
create_socket(term_t socket, int type)
{ nbio_sock_t sock;

  if ( !(sock = nbio_socket(AF_INET, type, 0)) )
    return FALSE;

  return tcp_unify_socket(socket, sock);
}


static foreign_t
tcp_socket(term_t socket)
{ return create_socket(socket, SOCK_STREAM);
}


static foreign_t
udp_socket(term_t socket)
{ return create_socket(socket, SOCK_DGRAM);
}


static foreign_t
pl_connect(term_t Socket, term_t Address)
{ nbio_sock_t sock;
  struct sockaddr_in sockaddr;

  if ( !tcp_get_socket(Socket, &sock) ||
       !nbio_get_sockaddr(Address, &sockaddr, NULL) )
    return FALSE;

  if ( nbio_connect(sock, (struct sockaddr*)&sockaddr, sizeof(sockaddr)) == 0 )
    return TRUE;

  return FALSE;
}


static foreign_t
pl_bind(term_t Socket, term_t Address)
{ struct sockaddr_in sockaddr;
  nbio_sock_t socket;
  term_t varport = 0;

  memset(&sockaddr, 0, sizeof(sockaddr));

  if ( !tcp_get_socket(Socket, &socket) ||
       !nbio_get_sockaddr(Address, &sockaddr, &varport) )
    return FALSE;

  if ( nbio_bind(socket, (struct sockaddr*)&sockaddr, sizeof(sockaddr)) < 0 )
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


static foreign_t
pl_accept(term_t Master, term_t Slave, term_t Peer)
{ nbio_sock_t master, slave;
  struct sockaddr_in addr;
  socklen_t addrlen = sizeof(addr);

  if ( !tcp_get_socket(Master, &master) )
    return FALSE;

  if ( !(slave = nbio_accept(master, (struct sockaddr*)&addr, &addrlen)) )
    return FALSE;
					/* TBD: close on failure */
  if ( nbio_unify_ip4(Peer, ntohl(addr.sin_addr.s_addr)) &&
       tcp_unify_socket(Slave, slave) )
    return TRUE;

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


#ifdef __WINDOWS__

		 /*******************************
		 *	       SELECT		*
		 *******************************/

typedef struct fdentry
{ int fd;
  term_t stream;
  struct fdentry *next;
} fdentry;


static term_t
findmap(fdentry *map, int fd)
{ for( ; map; map = map->next )
  { if ( map->fd == fd )
      return map->stream;
  }
  assert(0);
  return 0;
}


#ifndef SIO_GETPENDING
static size_t
Spending(IOSTREAM *s)
{ if ( s->bufp < s->limitp )
    return s->limitp - s->bufp;
  return 0;
}
#endif

static foreign_t
tcp_select(term_t Streams, term_t Available, term_t timeout)
{ fd_set fds;
  struct timeval t, *to;
  double time;
  int max = 0, ret, min = 1000000;
  fdentry *map     = NULL;
  term_t head      = PL_new_term_ref();
  term_t streams   = PL_copy_term_ref(Streams);
  term_t available = PL_copy_term_ref(Available);
  term_t ahead     = PL_new_term_ref();
  int from_buffer  = 0;
  atom_t a;
#ifdef __WINDOWS__
  int count = 0;			/* on Windows the setsize is limited */
#endif					/* to FD_SETSIZE */

  FD_ZERO(&fds);
  while( PL_get_list_ex(streams, head, streams) )
  { IOSTREAM *s;
    nbio_sock_t socket;
    fdentry *e;
    size_t pending;
    int fd;

    if ( !get_socket_from_stream(head, &s, &socket) )
      return PL_type_error("socket_stream", head);

    fd = nbio_fd(socket);
    pending = Spending(s);
    PL_release_stream(s);
					/* check for input in buffer */
    if ( pending > 0 )
    { if ( !PL_unify_list(available, ahead, available) ||
	   !PL_unify(ahead, head) )
	return FALSE;
      from_buffer++;
    }

    e         = alloca(sizeof(*e));
    e->fd     = fd;
    e->stream = PL_copy_term_ref(head);
    e->next   = map;
    map       = e;

#ifdef __WINDOWS__
    if ( ++count > FD_SETSIZE )
#else
    if ( fd >= FD_SETSIZE )
#endif
      return PL_representation_error("FD_SETSIZE");
    FD_SET((SOCKET)fd, &fds);

    if ( fd > max )
      max = fd;
    if( fd < min )
      min = fd;
  }
  if ( !PL_get_nil_ex(streams) )
    return FALSE;

  if ( from_buffer > 0 )
    return PL_unify_nil(available);

  if ( PL_get_atom(timeout, &a) && a == ATOM_infinite )
  { to = NULL;
  } else
  { if ( !PL_get_float(timeout, &time) )
      return pl_error("tcp_select", 3, NULL,
		      ERR_TYPE, timeout, "number");

    if ( time >= 0.0 )
    { t.tv_sec  = (int)time;
      t.tv_usec = ((int)(time * 1000000) % 1000000);
    } else
    { t.tv_sec  = 0;
      t.tv_usec = 0;
    }
    to = &t;
  }

  while( (ret=nbio_select(max+1, &fds, NULL, NULL, to)) == -1 &&
	 errno == EINTR )
  { fdentry *e;

    if ( PL_handle_signals() < 0 )
      return FALSE;			/* exception */

    FD_ZERO(&fds);			/* EINTR may leave fds undefined */
    for(e=map; e; e=e->next)		/* so we rebuild it to be safe */
    { FD_SET((SOCKET)e->fd, &fds);
    }
  }

  switch(ret)
  { case -1:
      return pl_error("tcp_select", 3, NULL, ERR_ERRNO, GET_ERRNO,
		      "select", "streams", Streams);

    case 0: /* Timeout */
      break;

    default: /* Something happened -> check fds */
    { int n;

      for(n=min; n <= max; n++)
      { if ( FD_ISSET(n, &fds) )
	{ if ( !PL_unify_list(available, ahead, available) ||
	       !PL_unify(ahead, findmap(map, n)) )
	    return FALSE;
	}
      }
      break;
    }
  }

  return PL_unify_nil(available);
}
#endif /*__WINDOWS__*/


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

install_t
install_socket(void)
{ nbio_init("socket");

  ATOM_reuseaddr          = PL_new_atom("reuseaddr");
  ATOM_bindtodevice       = PL_new_atom("bindtodevice");
  ATOM_broadcast          = PL_new_atom("broadcast");
  ATOM_nodelay	          = PL_new_atom("nodelay");
  ATOM_dispatch	          = PL_new_atom("dispatch");
  ATOM_nonblock	          = PL_new_atom("nonblock");
  ATOM_infinite	          = PL_new_atom("infinite");
  ATOM_as	          = PL_new_atom("as");
  ATOM_atom	          = PL_new_atom("atom");
  ATOM_string	          = PL_new_atom("string");
  ATOM_codes	          = PL_new_atom("codes");
  ATOM_max_message_size   = PL_new_atom("max_message_size");
  ATOM_file_no		  = PL_new_atom("file_no");
  ATOM_ip_add_membership  = PL_new_atom("ip_add_membership");
  ATOM_ip_drop_membership = PL_new_atom("ip_drop_membership");

  FUNCTOR_socket1 = PL_new_functor(PL_new_atom("$socket"), 1);

  PL_register_foreign("tcp_accept",           3, pl_accept,           0);
  PL_register_foreign("tcp_bind",             2, pl_bind,             0);
  PL_register_foreign("tcp_connect",          2, pl_connect,	      0);
  PL_register_foreign("tcp_listen",           2, pl_listen,           0);
  PL_register_foreign("tcp_open_socket",      3, pl_open_socket,      0);
  PL_register_foreign("tcp_socket",           1, tcp_socket,          0);
  PL_register_foreign("tcp_close_socket",     1, pl_close_socket,     0);
  PL_register_foreign("tcp_setopt",           2, pl_setopt,           0);
  PL_register_foreign("tcp_getopt",           2, pl_getopt,           0);
  PL_register_foreign("tcp_host_to_address",  2, pl_host_to_address,  0);
  PL_register_foreign("gethostname",          1, pl_gethostname,      0);
#ifdef __WINDOWS__
  PL_register_foreign("tcp_select",           3, tcp_select,          0);
#endif

  PL_register_foreign("udp_socket",           1, udp_socket,          0);
  PL_register_foreign("udp_receive",	      4, udp_receive,	      0);
  PL_register_foreign("udp_send",	      4, udp_send,	      0);

#ifdef O_DEBUG
  PL_register_foreign("tcp_debug",	      1, pl_debug,	      0);
#endif
}


install_t
uninstall_socket(void)
{ nbio_cleanup();
}
